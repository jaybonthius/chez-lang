(library (test quote lang)
  (export language)
  (import (chezscheme) (lang reader) (lang language))

  (define (read-quoted r char start)
    (let ([datum (reader-read r)])
      (when (eof-object? datum)
        (reader-error r start "unexpected end-of-file reading ~a" "$"))
      (reader-annotate r (list 'quote datum) start)))

  (define language
    (make-meta-language
      (lambda (readtable) (readtable-extend-datum-start readtable #\$ read-quoted)))))
