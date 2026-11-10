(library (chez-lang reload)
  (export reload-library)
  (import (chezscheme) (only (chez-lang language) call-with-library-lock))

  (define-record-type target
    (nongenerative)
    (sealed #t)
    (fields library source object))

  (define (library-name? x)
    (and (pair? x) (list? x) (for-all symbol? x)))

  (define (search-library who library)
    ((library-search-handler) who library (library-directories) (library-extensions)))

  (define (library-source who library)
    (let-values ([(src obj found?) (search-library who library)])
      src))

  ;; A part of a page, (page ... part), is compiled from the page's source,
  ;; and no other library shares a source, so name is either the page or a
  ;; part of it.
  (define (source-page who name source)
    (if (null? (cdr name))
        name
        (let ([parent (list-head name (fx- (length name) 1))])
          (if (equal? (library-source who parent) source) parent name))))

  (define (page-or-part? library page)
    (let ([n (length page)])
      (or (equal? library page)
          (and (fx= (length library) (fx+ n 1))
               (equal? (list-head library n) page)))))

  (define (make-library-target who library)
    (let-values ([(src obj found?) (search-library who library)])
      (unless src
        (errorf who "library ~s has no source file" library))
      (make-target library src obj)))

  ;; library-requirements adds a versioned library's version to its name;
  ;; library-list does not.
  (define (reference-name reference)
    (if (symbol? (car (last-pair reference)))
        reference
        (list-head reference (fx- (length reference) 1))))

  ;; These are the libraries an object records as imported, by which
  ;; maybe-compile-library judges it out of date; the other requirements
  ;; are what running the library needs.
  (define (library-imports library)
    (map reference-name (library-requirements library (library-requirements-options import))))

  ;; library-requirements lists a library's imports only once its
  ;; compile-time information is loaded, and a library that has only been
  ;; run, as a compiled program's imports are, has none. Importing it loads
  ;; that information, as the next import of it would. A library loaded from
  ;; source has both, and a built-in library imports nothing reloadable.
  ;; Importing a library whose object was loaded again after one of its
  ;; imports was recompiled without it fails, so only libraries lacking the
  ;; information are imported; Chez offers no test for it but this raise.
  (define (load-compile-time-information! libraries)
    (unless (null? libraries)
      (eval `(let () (import ,@(map (lambda (library) `(only (library ,library))) libraries)) #f)
            (scheme-environment))))

  (define (known-library-imports library)
    (guard (c [(violation? c) #f])
      (library-imports library)))

  (define (all-library-imports libraries)
    (let ([known (map known-library-imports libraries)])
      (load-compile-time-information!
        (fold-right
          (lambda (library required lacking)
            (if (or required (not (library-object-filename library)))
                lacking
                (cons library lacking)))
          '()
          libraries
          known))
      (map (lambda (library required) (or required (library-imports library)))
           libraries
           known)))

  (define (import-tables libraries)
    (let ([imports (make-hashtable equal-hash equal?)]
          [importers (make-hashtable equal-hash equal?)])
      (for-each
        (lambda (library required)
          (hashtable-set! imports library required)
          (for-each
            (lambda (name)
              (hashtable-update! importers name (lambda (libraries) (cons library libraries)) '()))
            required))
        libraries
        (all-library-imports libraries))
      (values imports importers)))

  ;; Each library comes before every library that imports it.
  (define (with-importers-in-order roots importers)
    (let ([seen (make-hashtable equal-hash equal?)])
      (fold-left
        (rec visit
          (lambda (ordered library)
            (cond
              [(hashtable-contains? seen library) ordered]
              [else
               (hashtable-set! seen library #t)
               (cons library (fold-left visit ordered (hashtable-ref importers library '())))])))
        '()
        roots)))

  ;; A restored library has no object file, so that the next import or
  ;; reload compiles its source again; its object stays here in case that
  ;; reload fails too. Used only under the library lock.
  (define restored-objects (make-hashtable equal-hash equal?))

  (define (previous-object target)
    (or (hashtable-ref restored-objects (target-library target) #f)
        (let ([obj (target-object target)])
          (and (file-exists? obj)
               (call-with-port (open-file-input-port obj) get-bytevector-all)))))

  ;; Compiling a library installs the new version in this process, which is
  ;; why import does not load a library it has just compiled; loading the
  ;; object as well would run its compile-time code again.
  ;; maybe-compile-library counts a library whose import was recompiled as
  ;; out of date only when compile-imported-libraries is #t.
  (define (recompile! target)
    (let ([src (target-source target)]
          [obj (target-object target)]
          [compile (compile-library-handler)]
          [compiled? #f])
      (parameterize ([compile-library-handler
                      (lambda (src-path obj-path)
                        (compile src-path obj-path)
                        (when (string=? obj-path obj) (set! compiled? #t)))]
                     [compile-imported-libraries #t]
                     [source-directories (cons (path-parent src) (source-directories))])
        (maybe-compile-library src obj))
      compiled?))

  (define (load-previous! target previous)
    (load-compiled-from-port (open-bytevector-input-port previous))
    (hashtable-set! restored-objects (target-library target) previous))

  ;; A library form that fails to compile removes the library of that name
  ;; from the process, and Chez deletes its object, so the next import would
  ;; compile the broken source again. Loading each previous object from
  ;; memory, in dependency order, puts the old versions back; deleting the
  ;; objects this call wrote keeps them from passing for the loaded ones.
  (define (restore! recompiled failed failed-previous)
    (for-each
      (lambda (entry)
        (let ([target (car entry)] [previous (cdr entry)])
          (when previous
            (delete-file (target-object target))
            (load-previous! target previous))))
      recompiled)
    (when (and failed-previous (not (member (target-library failed) (library-list))))
      (load-previous! failed failed-previous)))

  (define (recompile-targets! targets roots imports)
    (define (affected? library recompiled)
      (or (member library roots)
          (let ([required (hashtable-ref imports library '())])
            (exists (lambda (entry) (member (target-library (car entry)) required)) recompiled))))
    (let loop ([targets targets] [recompiled '()])
      (cond
        [(null? targets) (map (lambda (entry) (target-library (car entry))) (reverse recompiled))]
        [(affected? (target-library (car targets)) recompiled)
         (let* ([target (car targets)] [previous (previous-object target)])
           (cond
             [(guard (c [(serious-condition? c)
                         (restore! (reverse recompiled) target previous)
                         (raise c)])
                (recompile! target))
              (hashtable-delete! restored-objects (target-library target))
              (loop (cdr targets) (cons (cons target previous) recompiled))]
             [else (loop (cdr targets) recompiled)]))]
        [else (loop (cdr targets) recompiled)])))

  ;; A library compiled from the same source, such as a part of a page, is
  ;; out of date when that source changes, though it need not import the
  ;; library named. Every source is found before anything is recompiled, so a
  ;; library that cannot be rebuilt stops the reload before it changes
  ;; anything. Chez never drops a library, so an importer its user has
  ;; stopped using may have lost its source or no longer compile; importer?
  ;; leaves such importers out.
  (define reload-library
    (case-lambda
      [(name) (reload-library name (lambda (library) #t))]
      [(name importer?)
       (define who 'reload-library)
       (unless (library-name? name)
         (assertion-violationf who "~s is not a library name" name))
       (unless (procedure? importer?)
         (assertion-violationf who "~s is not a procedure" importer?))
       (call-with-library-lock
         (lambda ()
           (let* ([source (target-source (make-library-target who name))]
                  [page (source-page who name source)]
                  [loaded (library-list)]
                  [roots (filter (lambda (library)
                                   (or (equal? library name)
                                       (and (page-or-part? library page)
                                            (library-object-filename library)
                                            (equal? (library-source who library) source))))
                                 loaded)]
                  [considered (filter (lambda (library) (or (member library roots) (importer? library)))
                                      loaded)])
             (let-values ([(imports importers) (import-tables considered)])
               (recompile-targets!
                 (map (lambda (library) (make-library-target who library))
                      (with-importers-in-order roots importers))
                 roots
                 imports)))))])))
