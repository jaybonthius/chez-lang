(library (test doc)
  (export doc-begin up)
  (import (chezscheme))
  (export (import (chezscheme)))

  (define (up x) (list 'up x))

  ;; doc is captured from the doc-begin keyword, which the loader writes in
  ;; the page's context, so the page and its importers see it.
  (define-syntax doc-begin
    (lambda (x)
      (syntax-case x ()
        [(k form ...)
         (with-syntax ([doc (datum->syntax #'k 'doc)])
           #'(collect-doc doc () form ...))])))

  ;; A transformer cannot expand a form to learn whether it is a definition,
  ;; so definitions are recognised by their heads. Each is emitted before the
  ;; rest is examined, so an import in the body is visible to the forms after
  ;; it; expressions are collected because a definition may not follow one.
  (define-syntax collect-doc
    (lambda (x)
      (define (headed? form keyword)
        (syntax-case form ()
          [(head . rest) (and (identifier? #'head) (free-identifier=? #'head keyword))]
          [_ #f]))
      (define (defined-names form)
        (syntax-case form ()
          [(_ (name . formals) . body) (identifier? #'name) (list #'name)]
          [(_ name . rest) (identifier? #'name) (list #'name)]
          [_ (syntax-violation 'doc-begin "invalid definition" form)]))
      (syntax-case x ()
        [(_ doc (e ...))
         #'(begin (export doc) (define doc (list e ...)))]
        [(k doc es form rest ...)
         (headed? #'form #'begin)
         (syntax-case #'form ()
           [(_ inner ...) #'(k doc es inner ... rest ...)])]
        [(k doc es form rest ...)
         (headed? #'form #'import)
         #'(begin form (k doc es rest ...))]
        [(k doc es form rest ...)
         (or (headed? #'form #'define) (headed? #'form #'define-syntax))
         (with-syntax ([(name ...) (defined-names #'form)])
           #'(begin form (export name ...) (k doc es rest ...)))]
        [(k doc (e ...) form rest ...)
         #'(k doc (e ... form) rest ...)]))))
