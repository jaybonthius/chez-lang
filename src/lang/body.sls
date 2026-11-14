(library (lang body)
  (export
    definer
    definition-form?
    form-defined-names
    unbound-identifier?

    body-begin)
  (import (chezscheme) (lang private definitions))

  ;; A definition may not follow an expression in a library or module body,
  ;; so each expression becomes the definition of a temporary.  Each form is
  ;; classified only after the forms before it are emitted, so a body import
  ;; is visible to the forms after it.
  (define-syntax body-begin
    (lambda (x)
      (lambda (lookup)
        (syntax-case x ()
          [(_ library-name) #'(begin)]
          [(_ library-name (b form ...) rest ...)
           (and (identifier? #'b) (free-identifier=? #'b #'begin))
           #'(body-begin library-name form ... rest ...)]
          [(_ library-name form rest ...)
           (definition-form? #'form lookup)
           (let ([names (form-defined-names #'form lookup)])
             (if (null? names)
                 #'(begin form (body-begin library-name rest ...))
                 (with-syntax ([(name ...) names])
                   #'(begin form (export name ...) (body-begin library-name rest ...)))))]
          [(_ library-name form rest ...)
           #'(begin (define t (begin form (void))) (body-begin library-name rest ...))])))))
