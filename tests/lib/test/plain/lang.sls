(library (test plain lang)
  (export language)
  (import (chezscheme) (chez-lang reader) (chez-lang language))

  (define language (make-plain-language default-readtable (read-mode r6rs))))
