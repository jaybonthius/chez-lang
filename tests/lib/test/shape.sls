(library (test shape)
  (export shape-of)
  (import (chezscheme) (lang properties) (test shape property))
  (export (import (chezscheme)))

  (define-syntax shape-of
    (lambda (x)
      (syntax-case x ()
        [(k form)
         (let ([annotation (syntax->annotation #'form)])
           (with-syntax ([shape (datum->syntax #'k
                                  (if annotation
                                      (annotation-property-ref paren-shape annotation 'paren)
                                      'unannotated))])
             #''shape))]))))
