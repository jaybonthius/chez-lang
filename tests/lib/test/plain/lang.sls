(library (test plain lang)
  (export language)
  (import (chezscheme) (lang reader) (lang language))

  (define language (make-plain-language default-readtable (read-mode r6rs))))
