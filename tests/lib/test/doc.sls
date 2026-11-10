(library (test doc)
  (export doc-begin up define-meta)
  (import (chezscheme) (chez-lang body))
  (export (import (chezscheme)))

  (define (up x) (list 'up x))

  ;; The page's metas part reads these forms; the page itself ignores them.
  (define-syntax define-meta
    (lambda (x)
      (syntax-case x ()
        [(_ key value) (identifier? #'key) #'(begin)]
        [_ (syntax-violation 'define-meta "expected (define-meta key value)" x)])))

  (define-property define-meta definer (lambda (form) '()))

  ;; doc and this-library are captured from the doc-begin keyword, which the
  ;; loader writes in the page's context, so the page sees both and its
  ;; importers see doc.
  (define-syntax doc-begin
    (lambda (x)
      (syntax-case x ()
        [(k library-name form ...)
         (with-syntax ([doc (datum->syntax #'k 'doc)]
                       [this-library (datum->syntax #'k 'this-library)])
           #'(begin
               (define-syntax this-library
                 (syntax-rules () [(_) 'library-name]))
               (collect-doc doc () form ...)))])))

  ;; Definitions are emitted in place, before the next form is classified, so
  ;; an import in the body is visible to the forms after it; expressions are
  ;; collected because a definition may not follow one.
  (define-syntax collect-doc
    (lambda (x)
      (lambda (lookup)
        (syntax-case x ()
          [(_ doc (e ...))
           #'(begin (export doc) (define doc (list e ...)))]
          [(k doc es (b form ...) rest ...)
           (and (identifier? #'b) (free-identifier=? #'b #'begin))
           #'(k doc es form ... rest ...)]
          [(k doc es form rest ...)
           (definition-form? #'form lookup)
           (with-syntax ([(name ...) (form-defined-names #'form lookup)])
             #'(begin form (export name ...) (k doc es rest ...)))]
          [(k doc (e ...) form rest ...)
           #'(k doc (e ... form) rest ...)])))))
