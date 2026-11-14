(library (test shape property)
  (export paren-shape)
  (import (chezscheme) (lang properties))

  (define paren-shape (make-annotation-property 'paren-shape)))
