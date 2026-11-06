(library (pages greet)
  (export greet)
  (import (chezscheme))

  (define (greet name) (list "Hello, " name)))
