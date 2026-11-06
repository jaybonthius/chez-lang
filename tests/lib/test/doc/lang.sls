(library (test doc lang)
  (export language)
  (import (chezscheme) (chez-lang reader) (chez-lang language))

  (define (read-up r char start)
    (let ([datum (reader-read r)])
      (when (eof-object? datum)
        (reader-error r start "unexpected end-of-file reading ~a" "^"))
      (reader-annotate r (list 'up datum) start)))

  (define language
    (make-s-exp-language
      (readtable-extend-datum-start default-readtable #\^ read-up)
      (read-mode chez)
      '((test doc))
      'doc-begin)))
