(library (lang language)
  (export
    make-s-exp-language
    make-parser-language
    make-plain-language
    make-meta-language
    language-with-part
    language-with-lexer
    language?
    meta-language?
    language-name
    language-libraries
    language-readtable
    language-mode
    language-imports
    language-body-macro
    language-parts
    language-lexer

    lang-file?
    read-language
    read-language-forms
    read-lang-file
    language-library-name

    call-with-library-lock

    &language-error
    make-language-error
    language-error?
    language-error-name)
  (import (chezscheme) (lang reader))

  (define-record-type language
    (nongenerative)
    (sealed #t)
    (fields name libraries readtable mode parser imports body-macro parts lexer))

  (define-record-type meta-language
    (nongenerative)
    (sealed #t)
    (fields extend)
    (protocol
      (lambda (new)
        (lambda (extend)
          (unless (procedure? extend)
            (assertion-violationf 'make-meta-language "~s is not a procedure" extend))
          (new extend)))))

  (define-condition-type &language-error &error
    make-language-error language-error?
    (name language-error-name))

  (define (check-readtable who readtable)
    (unless (readtable? readtable)
      (assertion-violationf who "~s is not a readtable" readtable)))

  (define (check-mode who mode)
    (unless (read-mode? mode)
      (assertion-violationf who "~s is not a read mode" mode)))

  (define (check-imports who imports)
    (unless (list? imports)
      (assertion-violationf who "~s is not a list of import specs" imports)))

  (define (check-body-macro who body-macro)
    (unless (symbol? body-macro)
      (assertion-violationf who "~s is not a symbol" body-macro)))

  (define (check-language who language)
    (unless (language? language)
      (assertion-violationf who "~s is not a language" language)))

  (define (check-port who port)
    (unless (and (input-port? port) (textual-port? port) (port-has-port-position? port))
      (assertion-violationf who "~s is not a textual input port with positions" port)))

  (define (check-sfd who sfd)
    (unless (or (not sfd) (source-file-descriptor? sfd))
      (assertion-violationf who "~s is not a source-file descriptor or #f" sfd)))

  (define (make-s-exp-language readtable mode imports body-macro)
    (define who 'make-s-exp-language)
    (check-readtable who readtable)
    (check-mode who mode)
    (check-imports who imports)
    (check-body-macro who body-macro)
    (make-language #f '() readtable mode #f imports body-macro '() #f))

  (define (make-parser-language read imports body-macro)
    (define who 'make-parser-language)
    (unless (procedure? read) (assertion-violationf who "~s is not a procedure" read))
    (check-imports who imports)
    (check-body-macro who body-macro)
    (make-language #f '() #f #f read imports body-macro '() #f))

  (define (make-plain-language readtable mode)
    (define who 'make-plain-language)
    (check-readtable who readtable)
    (check-mode who mode)
    (make-language #f '() readtable mode #f '() #f '() #f))

  (define (copy-language language name libraries readtable parts lexer)
    (make-language name libraries readtable
      (language-mode language)
      (language-parser language)
      (language-imports language)
      (language-body-macro language)
      parts
      lexer))

  (define (language-with-part language part generate)
    (define who 'language-with-part)
    (check-language who language)
    (unless (symbol? part) (assertion-violationf who "~s is not a symbol" part))
    (when (assq part (language-parts language))
      (assertion-violationf who "the language already has a part ~s" part))
    (unless (procedure? generate) (assertion-violationf who "~s is not a procedure" generate))
    (copy-language language (language-name language) (language-libraries language)
      (language-readtable language)
      (append (language-parts language) (list (cons part generate)))
      (language-lexer language)))

  (define (language-with-lexer language lexer)
    (check-language 'language-with-lexer language)
    (copy-language language (language-name language) (language-libraries language)
      (language-readtable language) (language-parts language) lexer))

  (define (name-char? c)
    (and (char? c)
         (or (char<=? #\a c #\z)
             (char<=? #\A c #\Z)
             (char<=? #\0 c #\9)
             (and (memv c '(#\+ #\- #\_ #\/)) #t))))

  ;; #! followed by / is a script header.
  (define (name-start? c)
    (and (name-char? c) (not (char=? c #\/))))

  (define (name-end? c)
    (or (eof-object? c)
        (and (memv c '(#\space #\tab #\newline #\return #\x85 #\x2028)) #t)))

  (define chez-directives
    '("r6rs" "chezscheme" "fold-case" "no-fold-case" "eof" "bwp" "base-rtd"))

  (define (string-prefix? prefix s)
    (let ([n (string-length prefix)])
      (and (fx<= n (string-length s)) (string=? prefix (substring s 0 n)))))

  ;; Chez does not need a delimiter after a directive: it reads #!r6rsfoo as
  ;; #!r6rs followed by foo, so no language name may start with one.
  (define (directive-prefix name)
    (find (lambda (directive) (string-prefix? directive name)) chez-directives))

  (define (name-segments name)
    (let loop ([i 0] [start 0] [segments '()])
      (cond
        [(fx= i (string-length name))
         (reverse (cons (substring name start i) segments))]
        [(char=? (string-ref name i) #\/)
         (loop (fx+ i 1) (fx+ i 1) (cons (substring name start i) segments))]
        [else (loop (fx+ i 1) start segments)])))

  (define (name->library name)
    (append (map string->symbol (name-segments name)) '(lang)))

  (define (language-library-name name)
    (unless (and (string? name)
                 (for-all name-char? (string->list name))
                 (not (directive-prefix name))
                 (not (member "" (name-segments name))))
      (assertion-violationf 'language-library-name "~s is not a language name" name))
    (name->library name))

  (define (read-name port)
    (call-with-string-output-port
      (lambda (out)
        (let loop ()
          (when (name-char? (peek-char port))
            (write-char (read-char port) out)
            (loop))))))

  (define (lang-file? path)
    (unless (string? path) (assertion-violationf 'lang-file? "~s is not a string" path))
    (call-with-port (open-file-input-port path (file-options) (buffer-mode block) (current-transcoder))
      (lambda (port)
        (and (eqv? (read-char port) #\#)
             (eqv? (read-char port) #\!)
             (name-start? (peek-char port))
             (not (member (read-name port) chez-directives))))))

  (define (read-language-opening who port sfd)
    (let* ([start (port-position port)]
           [opening (get-string-n port 2)]
           [name-start (port-position port)])
      (unless (equal? opening "#!")
        (raise-read-error who sfd start name-start (if (eof-object? opening) "" opening)
          "missing language line"))
      (unless (name-start? (peek-char port))
        (raise-read-error who sfd start name-start opening "missing language name after #!"))))

  (define (read-language-name who port sfd)
    (define (fail bfp efp excerpt message . irritants)
      (apply raise-read-error who sfd bfp efp excerpt message irritants))
    (let* ([name-start (port-position port)] [name (read-name port)] [name-end (port-position port)])
      (cond
        [(directive-prefix name)
         => (lambda (directive)
              (if (string=? directive name)
                  (fail name-start name-end name "#!~a is a Chez Scheme directive, not a language" name)
                  (fail name-start name-end name "language name ~a starts with the Chez Scheme directive #!~a" name directive)))])
      (let ([c (peek-char port)])
        (unless (name-end? c)
          (read-char port)
          (fail name-end (port-position port) (string c) "invalid character ~s in language name" c)))
      (when (member "" (name-segments name))
        (fail name-start name-end name "empty segment in language name ~a" name))
      (values name name-start name-end)))

  (define (skip-to-base-name who port sfd meta-name)
    (let skip-blanks ()
      (when (memv (peek-char port) '(#\space #\tab))
        (read-char port)
        (skip-blanks)))
    (unless (name-start? (peek-char port))
      (let ([position (port-position port)])
        (raise-read-error who sfd position position ""
          "missing language name after meta-language ~a" meta-name))))

  (define (raise-language-error who name sfd start end message . irritants)
    (raise
      (condition
        (make-language-error name)
        (make-who-condition who)
        (make-message-condition message)
        (make-irritants-condition irritants)
        (make-format-condition)
        (if sfd
            (make-source-condition
              (make-annotation name ((current-make-source-object) sfd start end) name))
            (condition)))))

  ;; A language library that needs, while it loads or extends a readtable,
  ;; a file in its own language would compile that file again and again
  ;; until memory ran out.
  (define libraries-loading (make-thread-parameter '()))

  (define (load-language who name library sfd start end)
    (when (member library (libraries-loading))
      (raise-language-error who name sfd start end
        "language ~a depends on itself through library ~s" name library))
    (call-with-library-lock
      (lambda ()
        (let-values ([(src obj found?)
                      ((library-search-handler) who library (library-directories) (library-extensions))])
          (unless (or src found? (member library (library-list)))
            (raise-language-error who name sfd start end
              "unknown language ~a: library ~s not found" name library)))
        ;; Local imports, because Chez never frees an environment.
        ;; library-exports answers only for a loaded library, so an empty
        ;; import loads it first.
        (let ([language
               (parameterize ([libraries-loading (cons library (libraries-loading))])
                 (eval `(let () (import (only (library ,library))) #f) (scheme-environment))
                 (and (memq 'language (library-exports library))
                      (eval `(let () (import (only (library ,library) language)) language)
                            (scheme-environment))))])
          (unless (or (language? language) (meta-language? language))
            (raise-language-error who name sfd start end
              "library ~s does not export a language" library))
          language))))

  (define (read-language-chain who port sfd extending-meta-name)
    (let-values ([(name start end) (read-language-name who port sfd)])
      (let* ([library (name->library name)]
             [language (load-language who name library sfd start end)])
        (cond
          [(meta-language? language)
           (skip-to-base-name who port sfd name)
           (let* ([base (read-language-chain who port sfd name)]
                  [readtable (parameterize ([libraries-loading (cons library (libraries-loading))])
                               ((meta-language-extend language) (language-readtable base)))])
             (unless (readtable? readtable)
               (assertion-violationf who "the meta-language ~a returned ~s, not a readtable"
                 name readtable))
             (copy-language base
               (string-append name " " (language-name base))
               (cons library (language-libraries base))
               readtable
               (language-parts base)
               (language-lexer base)))]
          [else
           (when (and extending-meta-name (not (language-readtable language)))
             (raise-language-error who name sfd start end
               "language ~a reads with its own parser and cannot be extended by ~a"
               name extending-meta-name))
           (copy-language language name (list library) (language-readtable language)
             (language-parts language) (language-lexer language))]))))

  (define (read-language port sfd)
    (define who 'read-language)
    (check-port who port)
    (check-sfd who sfd)
    (read-language-opening who port sfd)
    (read-language-chain who port sfd #f))

  (define (read-language-forms language port sfd)
    (define who 'read-language-forms)
    (check-language who language)
    (check-port who port)
    (check-sfd who sfd)
    (when (and (language-readtable language) (not (port-has-set-port-position!? port)))
      (assertion-violationf who "~s does not support set-port-position!" port))
    (let ([readtable (language-readtable language)] [mode (language-mode language)])
      (if readtable
          (read-all
            (if sfd
                (make-annotating-reader port sfd readtable mode)
                (make-reader port readtable mode)))
          (let ([forms ((language-parser language) port sfd)])
            (unless (list? forms)
              (assertion-violationf who "the parser of ~a returned ~s, not a list"
                (language-name language) forms))
            forms))))

  (define (read-lang-file path)
    (unless (string? path) (assertion-violationf 'read-lang-file "~s is not a string" path))
    (let-values ([(port sfd) (open-source-port path)])
      (let ([language (read-language port sfd)])
        (values language (read-language-forms language port sfd)))))

  ;; Chez's library manager keeps shared state that concurrent imports
  ;; corrupt. Chez mutexes are recursive, so a compile handler running inside
  ;; a locked import takes the lock again without deadlock.
  (define library-mutex (make-mutex))

  (define (call-with-library-lock thunk)
    (unless (procedure? thunk)
      (assertion-violationf 'call-with-library-lock "~s is not a procedure" thunk))
    (with-mutex library-mutex (thunk))))
