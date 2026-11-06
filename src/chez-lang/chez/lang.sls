(library (chez-lang chez lang)
  (export language)
  (import (chezscheme) (chez-lang reader) (chez-lang language))

  ;; import and load-program read a library or program in R6RS mode; a file
  ;; starts with #!chezscheme for Chez's extensions, as an .sls file does.
  (define language (make-plain-language default-readtable (read-mode r6rs))))
