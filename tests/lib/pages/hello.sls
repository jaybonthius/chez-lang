#!test/doc
(import (pages greet))
(define (up x) (list 'page-up x))
(greet ^"reader")
"text"
