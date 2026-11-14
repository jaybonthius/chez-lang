(library (test shape lang)
  (export language)
  (import (chezscheme) (lang reader) (lang language) (lang properties)
          (test shape property))

  (define (read-bracketed r char start)
    (let ([datum (reader-annotate r (reader-read-list r #\]) start)])
      (when (annotation? datum)
        (annotation-property-set! paren-shape datum 'bracket))
      datum))

  (define language
    (make-s-exp-language
      (readtable-extend-datum-start default-readtable #\[ read-bracketed)
      (read-mode chez)
      '((test shape) (lang body))
      'body-begin)))
