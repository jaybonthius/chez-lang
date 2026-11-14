#!chezscheme
(library (lang include)
  (export include/lang)
  (import (chezscheme) (lang language) (lang body))

  ;; Chez's include records its file with this undocumented procedure, and no
  ;; documented procedure adds a file to an object's recompile information.
  ;; It runs at expansion because the requirement is collected then.
  (meta define (record-include-file! file)
    (#%$require-include file))

  ;; The forms take the context of the include/lang keyword, as Chez's include
  ;; gives them; the empty imports make the including code recompile when the
  ;; file's language changes.
  (define-syntax include/lang
    (lambda (x)
      (syntax-case x ()
        [(k file)
         (string? (syntax->datum #'file))
         (let ([name (syntax->datum #'file)])
           (let-values ([(language forms) (with-source-path 'include/lang name read-lang-file)])
             (record-include-file! name)
             (with-syntax ([(library-name ...) (datum->syntax #'k (language-libraries language))]
                           [(form ...) (map (lambda (form) (datum->syntax #'k form)) forms)])
               #'(begin (import (only (library library-name))) ... form ...))))]
        [_ (syntax-violation 'include/lang "expected (include/lang \"file\")" x)])))

  ;; Body macros classify forms before expansion, so include/lang must count as
  ;; a definition; the names it splices in are not known until it expands.
  (define-property include/lang definer (lambda (form) '())))
