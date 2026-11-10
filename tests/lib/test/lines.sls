(library (test lines)
  (export lines-begin line)
  (import (chezscheme))

  (define (line text) text)

  (define-syntax lines-begin
    (lambda (x)
      (syntax-case x ()
        [(k library-name form ...)
         (with-syntax ([doc (datum->syntax #'k 'doc)])
           #'(begin (export doc) (define doc (list form ...))))]))))
