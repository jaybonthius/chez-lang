(library (test doc lang)
  (export language)
  (import (chezscheme) (chez-lang reader) (chez-lang language))

  (define (read-up r char start)
    (let ([datum (reader-read r)])
      (when (eof-object? datum)
        (reader-error r start "unexpected end-of-file reading ~a" "^"))
      (reader-annotate r (list 'up datum) start)))

  (define (meta-entries forms)
    (fold-right
      (lambda (form entries)
        (let ([datum (syntax->datum form)])
          (if (and (list? datum) (fx= (length datum) 3) (eq? (car datum) 'define-meta))
              (cons (cons (cadr datum) (caddr datum)) entries)
              entries)))
      '()
      forms))

  (define (generate-metas name forms)
    `(library ,name
       (export metas)
       (import (chezscheme))
       (define metas ',(meta-entries forms))))

  (define language
    (language-with-part
      (make-s-exp-language
        (readtable-extend-datum-start default-readtable #\^ read-up)
        (read-mode chez)
        '((test doc))
        'doc-begin)
      'metas
      generate-metas)))
