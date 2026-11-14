(library (test lines lang)
  (export language)
  (import (chezscheme) (lang language))

  (define (line-form text start sfd)
    (let ([datum (list 'line text)])
      (if sfd
          (let ([source ((current-make-source-object) sfd start (+ start (string-length text)))])
            (make-annotation (list 'line (make-annotation text source text)) source datum))
          datum)))

  ;; The line ending after the language name ends the language line, not a
  ;; line of the page.
  (define (read-lines port sfd)
    (let skip-blanks ()
      (when (memv (peek-char port) '(#\space #\tab))
        (read-char port)
        (skip-blanks)))
    (let loop ([first? #t] [forms '()])
      (let* ([start (port-position port)] [text (get-line port)])
        (cond
          [(eof-object? text) (reverse forms)]
          [(and first? (string=? text "")) (loop #f forms)]
          [else (loop #f (cons (line-form text start sfd) forms))]))))

  (define language
    (make-parser-language read-lines '((test lines)) 'lines-begin)))
