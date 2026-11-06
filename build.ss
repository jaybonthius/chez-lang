(unless (and (equal? (call-with-values scheme-version-number list) '(10 4 1))
             (threaded?))
  (errorf 'build "need threaded Chez Scheme 10.4.1, not ~a" (scheme-version)))

(define object-root "build/obj")

(library-directories (list (cons "src" (string-append object-root "/src"))))
(compile-imported-libraries #t)
(optimize-level 2)
(generate-inspector-information #f)
(compile-file-message #f)

(define compiled 0)
(define problems 0)

(define (report c)
  (set! problems (+ problems 1))
  (display-condition c (console-error-port))
  (newline (console-error-port)))

;; Compiling a library compiles its body twice, once to install it and once
;; for the object file, so one warning can be raised twice.
(define seen-warnings (make-hashtable string-hash string=?))

;; An object compiled with a warning is still written; deleting it keeps the
;; build failing until the source is fixed.
(define (compiling src obj compile)
  (set! compiled (+ compiled 1))
  (printf "compiling ~a\n" src)
  (let ([problems-before problems] [finished? #f])
    (dynamic-wind
      void
      (lambda () (compile src obj) (set! finished? #t))
      (lambda ()
        (unless (and finished? (= problems problems-before))
          (when (file-exists? obj) (delete-file obj)))))))

(compile-library-handler (lambda (src obj) (compiling src obj compile-library)))
(compile-program-handler (lambda (src obj) (compiling src obj compile-program)))

;; Returning from the handler lets the compile continue and report every
;; warning; a guard would stop it at the first.
(define (with-warnings-as-errors thunk)
  (guard (c [#t (report c)])
    (with-exception-handler
      (lambda (c)
        (if (warning? c)
            (let ([text (call-with-string-output-port
                          (lambda (p) (display-condition c p)))])
              (unless (hashtable-ref seen-warnings text #f)
                (hashtable-set! seen-warnings text #t)
                (report c)))
            (raise-continuable c)))
      thunk)))

(define (ensure-parent-directory file)
  (let ([dir (path-parent file)])
    (unless (or (string=? dir "") (file-directory? dir))
      (ensure-parent-directory dir)
      (mkdir dir))))

(define (find-libraries dir)
  (apply append
    (map (lambda (name)
           (let ([path (string-append dir "/" name)])
             (cond
               [(file-directory? path) (find-libraries path)]
               [(equal? (path-extension path) "sls") (list path)]
               [else '()])))
         (sort string<? (directory-list dir)))))

(define (find-programs dir)
  (if (file-directory? dir)
      (map (lambda (name) (string-append dir "/" name))
           (filter (lambda (name) (equal? (path-extension name) "sps"))
                   (sort string<? (directory-list dir))))
      '()))

;; Import adds a library's own directory to source-directories while it
;; checks and compiles the library, so its private included files resolve
;; here as they do when a program imports it from source.
(define (build-each maybe-compile sources)
  (for-each
    (lambda (src)
      (let ([obj (string-append object-root "/" (path-root src) ".so")])
        (ensure-parent-directory obj)
        (parameterize ([source-directories (cons (path-parent src) (source-directories))])
          (with-warnings-as-errors (lambda () (maybe-compile src obj))))))
    sources))

(build-each maybe-compile-library (find-libraries "src"))
(build-each maybe-compile-program
  (append (find-programs "tests") (find-programs "bench")))

(printf "build: ~a compiled, ~a problems\n" compiled problems)
(exit (if (= problems 0) 0 1))
