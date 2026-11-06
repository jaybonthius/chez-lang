(import (chezscheme) (chez-lang reader))

(include "differential.ss")

;; Characters that change how the reader splits or reads tokens.
(define alphabet
  (string->list "()[]{}\";#|\\'`,@.+-0178aeixrvuft!=%: \n"))

(define max-length
  (let ([args (command-line-arguments)])
    (if (null? args) 3 (string->number (car args)))))

(define differences '())
(define count 0)

(define (check! text)
  (set! count (+ count 1))
  (for-each
    (lambda (mode)
      (let ([d (plain-difference text mode)])
        (when d (set! differences (cons d differences)))))
    '(chez r6rs)))

(let loop ([prefix '()] [n 0])
  (unless (null? prefix) (check! (list->string prefix)))
  (when (< n max-length)
    (for-each (lambda (c) (loop (cons c prefix) (+ n 1))) alphabet)))

(printf "~a strings of up to ~a characters over ~a\n" count max-length (length alphabet))
(report-differences "exhaustive-test" (reverse differences) 40)
(exit (if (null? differences) 0 1))
