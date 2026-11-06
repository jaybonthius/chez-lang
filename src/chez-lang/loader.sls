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
             [compile-other-program (compile-program-handler)])
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
                     (compile-other-program src obj)))))))
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
  (define (source-library-name who src)
    (let ([directory
           (longest
             (filter (lambda (prefix) (string-prefix? prefix src))
                     (map (lambda (dir) (if (string=? dir ".") "" (path-build dir "")))
                          (map car (library-directories)))))])
      (unless (and directory (not (and (string=? directory "") (path-absolute? src))))
        (errorf who "source file ~a is not in a library directory" src))
      (let* ([relative (substring src (string-length directory) (string-length src))]
             [extension (longest
                          (filter (lambda (extension) (proper-suffix? extension relative))
                                  (map car (library-extensions))))])
        (unless extension
          (errorf who "source file ~a has no extension in library-extensions" src))
        (path->library-name
          (substring relative 0 (fx- (string-length relative) (string-length extension)))))))

  (define (path->library-name path)
    (let ([first (path-first path)])
      (if (string=? first "")
          (list (string->symbol path))
          (cons (string->symbol first) (path->library-name (path-rest path))))))

  ;; The page does not otherwise import the language library, the reader or
  ;; this generator; an empty import records each one's uid, so Chez
  ;; recompiles the page when one of them is recompiled.
  (define (tracking-imports language)
    `(,@(map (lambda (name) `(only (library ,name))) (language-libraries language))
      ,@(if (language-readtable language) '((only (chez-lang reader))) '())
      (only (chez-lang loader))))

  (define (check-body-macro who language)
    (let ([body-macro (language-body-macro language)] [imports (language-imports language)])
      (unless (memq body-macro (environment-symbols (apply environment imports)))
        (raise
          (condition
            (make-language-error (language-name language))
            (make-who-condition who)
            (make-message-condition "body macro ~a is not exported by ~s")
            (make-irritants-condition (list body-macro imports))
            (make-format-condition))))))

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

  (define (plain-library who src language forms)
    (when (null? forms)
      (syntax-violation who "expected one library form" src))
    (let ([form (car forms)])
      (unless (headed? form 'library)
        (syntax-violation who "expected one library form" form))
      (unless (null? (cdr forms))
        (syntax-violation who "unexpected form after the library form" (cadr forms)))
      (let ([parts (unwrap form)])
        (unless (and (list? parts) (fx>= (length parts) 4) (headed? (cadddr parts) 'import)
                     (import-specs (cadddr parts)))
          (syntax-violation who "expected (library name (export ...) (import ...) body ...)" form))
        `(library ,(cadr parts) ,(caddr parts)
           (import ,@(import-specs (cadddr parts)) ,@(tracking-imports language))
           ,@(cddddr parts)))))

  (define (plain-program who src language forms)
    (unless (and (pair? forms) (headed? (car forms) 'import) (import-specs (car forms)))
      (syntax-violation who "expected a program starting with an import form"
        (if (null? forms) src (car forms))))
    `(top-level-program
       (import ,@(import-specs (car forms)) ,@(tracking-imports language))
       ,@(cdr forms)))

  (define (compile-lang-file src obj generate)
    (when (compile-file-message)
      (printf "compiling ~a with output to ~a\n" src obj))
    (let-values ([(port sfd) (open-source-port src)])
      (let* ([language (read-language port sfd)]
             [forms (read-language-forms language port sfd)])
        (compile-to-file (list (generate language forms)) obj sfd))))

  (define (compile-lang-library src obj)
    (define who 'compile-lang-library)
    (check-paths who src obj)
    (call-with-library-lock
      (lambda ()
        (compile-lang-file src obj
          (lambda (language forms)
            (if (language-body-macro language)
                (generate-library who (source-library-name who src) language forms)
                (plain-library who src language forms)))))))

  (define (compile-lang-program src obj)
    (define who 'compile-lang-program)
    (check-paths who src obj)
    (call-with-library-lock
      (lambda ()
        (compile-lang-file src obj
          (lambda (language forms)
            (if (language-body-macro language)
                (generate-program who language forms)
                (plain-program who src language forms))))))))
