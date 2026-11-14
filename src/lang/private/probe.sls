(library (lang private probe)
  (export chez-lang-probe:context)
  (import (prefix (only (chezscheme) define syntax) chez-lang-probe:))

  ;; Every name bound here has the prefix, so an identifier made in this
  ;; context with any other name is unbound.
  (chez-lang-probe:define chez-lang-probe:context
    (chez-lang-probe:syntax chez-lang-probe:context)))
