(library (chez-lang loader)
  (export
    install-loader!
    compile-lang-library
    compile-lang-program)
  (import (chezscheme) (only (chez-lang reader) open-source-port) (chez-lang language))

  (define loader-installed? (make-thread-parameter #f))

  (define install-loader!
    (case-lambda
      [() (install-loader! '())]
      [(extensions)
       (define who 'install-loader!)
       (unless (and (list? extensions) (for-all string? extensions))
         (assertion-violationf who "~s is not a list of extension strings" extensions))
       (when (loader-installed?)
         (assertion-violation who "the loader is already installed in this thread"))
       (let ([compile-other-library (compile-library-handler)]
             [compile-other-program (compile-program-handler)]
             [search-other (library-search-handler)])
         (compile-library-handler
           (lambda (src obj)
             (call-with-library-lock
               (lambda ()
                 (if (lang-file? src)
                     (compile-lang-library src obj)
                     (compile-other-library src obj))))))
         (compile-program-handler
           (lambda (src obj)
             (call-with-library-lock
               (lambda ()
                 (if (lang-file? src)
                     (compile-lang-program src obj)
                     (compile-other-program src obj))))))
         (library-search-handler (part-search-handler search-other)))
       (library-extensions
         (append (library-extensions)
                 (map (lambda (extension) (cons extension ".so"))
                      (remp (lambda (extension) (assoc extension (library-extensions)))
                            extensions))))
       ;; Otherwise an import of a stale language-file library loads the
       ;; source with Chez's reader, which rejects the language line.
       (compile-imported-libraries #t)
       (loader-installed? #t)]))

  (define (check-paths who src obj)
    (unless (string? src) (assertion-violationf who "~s is not a string" src))
    (unless (string? obj) (assertion-violationf who "~s is not a string" obj)))

  (define (string-prefix? prefix s)
    (let ([n (string-length prefix)])
      (and (fx<= n (string-length s)) (string=? prefix (substring s 0 n)))))

  (define (proper-suffix? suffix s)
    (let ([n (string-length suffix)] [m (string-length s)])
      (and (fx< n m) (string=? suffix (substring s (fx- m n) m)))))

  (define (longest strings)
    (fold-left (lambda (best s) (if (or (not best) (fx> (string-length s) (string-length best))) s best))
               #f strings))

  ;; Chez omits the directory from a source path under ".", so a path under
  ;; lib/ is also under "."; the longest matching directory names the library.
  (define (library-relative-path src)
    (let ([directory
           (longest
             (filter (lambda (prefix) (string-prefix? prefix src))
                     (map (lambda (dir) (if (string=? dir ".") "" (path-build dir "")))
                          (map car (library-directories)))))])
      (and directory
           (not (and (string=? directory "") (path-absolute? src)))
           (substring src (string-length directory) (string-length src)))))

  (define (relative-library-name relative)
    (let ([pair (extension-pair relative)])
      (and pair (path->library-name (path-without-suffix relative (car pair))))))

  (define (source-library-name who src)
    (let ([relative (library-relative-path src)])
      (unless relative
        (errorf who "source file ~a is not in a library directory" src))
      (or (relative-library-name relative)
          (errorf who "source file ~a has no extension in library-extensions" src))))

  (define (path-without-suffix path suffix)
    (substring path 0 (fx- (string-length path) (string-length suffix))))

  (define (extension-pair path)
    (fold-left
      (lambda (best pair)
        (if (and (proper-suffix? (car pair) path)
                 (or (not best) (fx> (string-length (car pair)) (string-length (car best)))))
            pair
            best))
      #f
      (library-extensions)))

  (define (path->library-name path)
    (let ([first (path-first path)])
      (if (string=? first "")
          (list (string->symbol path))
          (cons (string->symbol first) (path->library-name (path-rest path))))))

  ;; A part's object is where Chez's default search handler looks for the
  ;; part's library name, lib/pages/post/metas.so for (pages post metas), so
  ;; a process without the loader imports it.
  (define (part-object-layout page-src page-obj)
    (let ([pair (extension-pair (path-last page-src))])
      (and pair
           (proper-suffix? (cdr pair) page-obj)
           (cons (path-build (path-without-suffix page-obj (cdr pair)) "") (cdr pair)))))

  (define (part-object-path layout part)
    (string-append (car layout) (symbol->string part) (cdr layout)))

  ;; The handler never reads a page's language: that loads the language
  ;; library, which may be a sibling of the page whose import Chez is in the
  ;; middle of, and loading it again fails with "attempting to re-install
  ;; compile-time part". So a real library that takes a part's name is
  ;; reported when the page compiles, and a name whose only object is at the
  ;; part's path beside a language-file page is the page's part; an object in
  ;; another library directory is that library's own. A part has its own object,
  ;; because two libraries in one object file make a warm import loop
  ;; forever, and the handler only names it, because a search handler that
  ;; compiles and returns an object fails the same way.
  (define (part-search-handler search-other)
    (lambda (who path directories extensions)
      (let-values ([(src obj found?) (search-other who path directories extensions)])
        (if (or src (null? (cdr path)))
            (values src obj found?)
            (let-values ([(page-src page-obj page-found?)
                          (search-other who (list-head path (fx- (length path) 1)) directories extensions)])
              (let ([layout (and page-src page-obj (lang-file? page-src)
                                 (part-object-layout page-src page-obj))])
                (if layout
                    (let ([part-obj (part-object-path layout (car (last-pair path)))])
                      (if (and found? (not (string=? obj part-obj)))
                          (values src obj found?)
                          (values page-src part-obj (file-exists? part-obj))))
                    (values src obj found?))))))))

  (define (check-part-conflicts who src language)
    (let* ([relative (library-relative-path src)]
           [name (and relative (relative-library-name relative))])
      (when name
        (for-each
          (lambda (entry)
            (let ([path (append name (list (car entry)))])
              (let-values ([(part-src part-obj found?)
                            ((library-search-handler) who path (library-directories) (library-extensions))])
                (when (and part-src (not (string=? part-src src)))
                  (raise-language-error who language "library ~s is both a part of ~a and the library ~a"
                    path src part-src)))))
          (language-parts language)))))

  ;; A test of the object path's shape alone would take lib/pages/pages.so,
  ;; the object of (pages pages) itself, for a part.
  (define (object-part who src obj)
    (let* ([relative (library-relative-path src)]
           [name (and relative (relative-library-name relative))])
      (and name
           (let-values ([(page-src page-obj page-found?)
                         ((library-search-handler) who name (library-directories) (library-extensions))])
             (let ([layout (and page-obj (part-object-layout src page-obj))])
               (and layout
                    (let ([directory (car layout)] [extension (cdr layout)])
                      (and (fx> (string-length obj) (fx+ (string-length directory) (string-length extension)))
                           (string-prefix? directory obj)
                           (proper-suffix? extension obj)
                           (let ([part (substring obj (string-length directory)
                                         (fx- (string-length obj) (string-length extension)))])
                             (and (string=? (path-last part) part)
                                  (string->symbol part)))))))))))

  ;; The page does not otherwise import the language library, the reader or
  ;; this generator; an empty import records each one's uid, so Chez
  ;; recompiles the page when one of them is recompiled.
  (define (tracking-imports language)
    `(,@(map (lambda (name) `(only (library ,name))) (language-libraries language))
      ,@(if (language-readtable language) '((only (chez-lang reader))) '())
      (only (chez-lang loader))))

  (define (raise-language-error who language message . irritants)
    (raise
      (condition
        (make-language-error (language-name language))
        (make-who-condition who)
        (make-message-condition message)
        (make-irritants-condition irritants)
        (make-format-condition))))

  (define (check-body-macro who language)
    (let ([body-macro (language-body-macro language)] [imports (language-imports language)])
      (unless (memq body-macro (environment-symbols (apply environment imports)))
        (raise-language-error who language "body macro ~a is not exported by ~s" body-macro imports))))

  ;; Gensyms keep the wrapper's names apart from the language's exports and
  ;; the page's definitions, so a page may define a name its language
  ;; exports. The import inside export is not renamed: Chez matches it by
  ;; name.
  (define (generate-library who name language forms)
    (check-body-macro who language)
    (let ([module-id (gensym "module")] [export-id (gensym "export")] [body-id (gensym "body")])
      `(library ,name
         (export)
         (import (rename (only (chezscheme) module export) (module ,module-id) (export ,export-id))
                 ,@(language-imports language)
                 ,@(tracking-imports language))
         (,module-id ,body-id () (,(language-body-macro language) ,@forms))
         (,export-id (import ,body-id)))))

  (define (generate-program who language forms)
    (check-body-macro who language)
    (let ([module-id (gensym "module")] [body-id (gensym "body")])
      `(top-level-program
         (import (rename (only (chezscheme) module) (module ,module-id))
                 ,@(language-imports language)
                 ,@(tracking-imports language))
         (,module-id ,body-id () (,(language-body-macro language) ,@forms)))))

  (define (unwrap x)
    (if (annotation? x) (annotation-expression x) x))

  (define (headed? x head)
    (let ([x (unwrap x)])
      (and (pair? x) (eq? (unwrap (car x)) head))))

  (define (import-specs clause)
    (let ([clause (unwrap clause)])
      (and (list? clause) (cdr clause))))

  (define (add-tracking-imports who form language)
    (let ([elements (unwrap form)])
      (unless (and (list? elements) (fx>= (length elements) 4) (headed? (cadddr elements) 'import)
                   (import-specs (cadddr elements)))
        (syntax-violation who "expected (library name (export ...) (import ...) body ...)" form))
      `(library ,(cadr elements) ,(caddr elements)
         (import ,@(import-specs (cadddr elements)) ,@(tracking-imports language))
         ,@(cddddr elements))))

  (define (plain-library who src language forms)
    (when (null? forms)
      (syntax-violation who "expected one library form" src))
    (let ([form (car forms)])
      (unless (headed? form 'library)
        (syntax-violation who "expected one library form" form))
      (unless (null? (cdr forms))
        (syntax-violation who "unexpected form after the library form" (cadr forms)))
      (add-tracking-imports who form language)))

  (define (part-library who name part language forms)
    (let ([entry (assq part (language-parts language))])
      (unless entry
        (raise-language-error who language "language ~a has no part ~a" (language-name language) part))
      (add-tracking-imports who ((cdr entry) name forms) language)))

  (define (plain-program who src language forms)
    (unless (and (pair? forms) (headed? (car forms) 'import) (import-specs (car forms)))
      (syntax-violation who "expected a program starting with an import form"
        (if (null? forms) src (car forms))))
    `(top-level-program
       (import ,@(import-specs (car forms)) ,@(tracking-imports language))
       ,@(cdr forms)))

  ;; A page that its own language needs, through its module
  ;; language or a library that imports it, would otherwise compile again
  ;; and again until memory ran out. Keyed by object, because a page may
  ;; import its own part, which compiles from the same source. A page
  ;; compiled again while its language is still being read is left to
  ;; read-language, which reports the language library that needs itself.
  (define objects-in-progress (make-thread-parameter '()))

  (define (compile-lang-file who src obj generate)
    (cond
      [(assoc obj (objects-in-progress))
       => (lambda (entry)
            (raise-language-error who (cdr entry) "language ~a depends on the page ~a being compiled"
              (language-name (cdr entry)) src))])
    (when (compile-file-message)
      (printf "compiling ~a with output to ~a\n" src obj))
    (let-values ([(port sfd) (open-source-port src)])
      (let ([language (read-language port sfd)])
        (parameterize ([objects-in-progress (cons (cons obj language) (objects-in-progress))])
          (let ([forms (read-language-forms language port sfd)])
            (compile-to-file (list (generate language forms)) obj sfd))))))

  (define (compile-lang-library src obj)
    (define who 'compile-lang-library)
    (check-paths who src obj)
    (call-with-library-lock
      (lambda ()
        (let ([part (object-part who src obj)])
          (compile-lang-file who src obj
            (lambda (language forms)
              (cond
                [part
                 (part-library who (append (source-library-name who src) (list part))
                   part language forms)]
                [else
                 (check-part-conflicts who src language)
                 (if (language-body-macro language)
                     (generate-library who (source-library-name who src) language forms)
                     (plain-library who src language forms))])))))))

  (define (compile-lang-program src obj)
    (define who 'compile-lang-program)
    (check-paths who src obj)
    (call-with-library-lock
      (lambda ()
        (compile-lang-file who src obj
          (lambda (language forms)
            (if (language-body-macro language)
                (generate-program who language forms)
                (plain-program who src language forms))))))))
