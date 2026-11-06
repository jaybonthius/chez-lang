(import (chezscheme) (chez-lang reader) (chez-lang language))

(define failures 0)

(define (fail! name expected actual)
  (set! failures (+ failures 1))
  (parameterize ([print-length 12] [print-level 6])
    (printf "FAIL ~a\n  expected ~s\n  actual   ~s\n" name expected actual)))

(define (condition-text c)
  (call-with-string-output-port (lambda (p) (display-condition c p))))

(define-syntax check
  (syntax-rules ()
    [(_ name expected expr)
     (let ([actual (guard (c [#t (list 'raised (condition-text c))]) expr)])
       (unless (equal? actual expected) (fail! name expected actual)))]))

(define (raised thunk)
  (guard (c [#t c]) (thunk) #f))

(define (text-sfd text)
  (make-source-file-descriptor "text" (open-bytevector-input-port (string->utf8 text))))

(define (read-text text)
  (let ([port (open-input-string text)] [sfd (text-sfd text)])
    (let ([language (read-language port sfd)])
      (values language (read-language-forms language port sfd)))))

(define (forms-of text)
  (let-values ([(language forms) (read-text text)])
    (map syntax->datum forms)))

(define (span annotation)
  (let ([source (annotation-source annotation)])
    (list (source-object-bfp source) (source-object-efp source))))

(define (location annotation)
  (call-with-values
    (lambda () (locate-source-object-source (annotation-source annotation) #t #t))
    list))

(define (language-line-error text)
  (let ([c (raised (lambda () (read-text text)))])
    (if (and (lexical-violation? c) (source-condition? c))
        (list (condition-who c)
              (apply format (condition-message c) (condition-irritants c))
              (span (source-condition-form c)))
        (list 'unexpected (and c (condition-text c))))))

(define (language-error-of text)
  (let ([c (raised (lambda () (read-text text)))])
    (if (language-error? c)
        (list (language-error-name c)
              (condition-who c)
              (apply format (condition-message c) (condition-irritants c))
              (and (source-condition? c) (span (source-condition-form c))))
        (list 'unexpected (and c (condition-text c))))))

(define (assertion-of thunk)
  (let ([c (raised thunk)])
    (and (assertion-violation? c) (condition-who c))))

(define (with-file text proc)
  (let ([path "build/language-test-file.sls"])
    (call-with-output-file path (lambda (p) (put-string p text)) 'replace)
    (let ([result (proc path)])
      (delete-file path)
      result)))

(check "lowercase name" '(abc lang) (language-library-name "abc"))
(check "uppercase name" (list (string->symbol "ABC") 'lang) (language-library-name "ABC"))
(check "digit in name" (list (string->symbol "a1") 'lang) (language-library-name "a1"))
(check "+ in name" (list (string->symbol "a+b") 'lang) (language-library-name "a+b"))
(check "- in name" '(a-b lang) (language-library-name "a-b"))
(check "_ in name" (list (string->symbol "a_b") 'lang) (language-library-name "a_b"))
(check "/ in name" '(a b c lang) (language-library-name "a/b/c"))
(check "name of every allowed character read from a language line"
  (list "aZ9+-_/x" 'read-language
        (format "unknown language aZ9+-_/x: library ~s not found"
          (list (string->symbol "aZ9+-_") 'x 'lang))
        '(2 10))
  (language-error-of "#!aZ9+-_/x\n"))
(check "empty name is not a language name" 'language-library-name
  (assertion-of (lambda () (language-library-name ""))))
(check "empty segment is not a language name" 'language-library-name
  (assertion-of (lambda () (language-library-name "a//b"))))
(check "space is not in a language name" 'language-library-name
  (assertion-of (lambda () (language-library-name "a b"))))
(check "directive is not a language name" 'language-library-name
  (assertion-of (lambda () (language-library-name "r6rs"))))
(check "symbol is not a language name" 'language-library-name
  (assertion-of (lambda () (language-library-name 'a))))

(check "text after the name" '((up x) 1 2) (forms-of "#!test/doc ^x 1\n2"))
(check "tab after the name" '((up x)) (forms-of "#!test/doc\t^x"))
(check "line ending after the name" '((up x)) (forms-of "#!test/doc\r\n^x"))
(check "end of input after the name" '() (forms-of "#!test/doc"))
(check "reading leaves the port after the name" #\space
  (let ([port (open-input-string "#!test/doc 1")])
    (read-language port #f)
    (peek-char port)))

(check "no language line" '(read-language "missing language line" (0 2))
  (language-line-error "(x)"))
(check "empty file" '(read-language "missing language line" (0 0))
  (language-line-error ""))
(check "missing name at end of input" '(read-language "missing language name after #!" (0 2))
  (language-line-error "#!"))
(check "missing name before a space" '(read-language "missing language name after #!" (0 2))
  (language-line-error "#! test/doc"))
(check "script header" '(read-language "missing language name after #!" (0 2))
  (language-line-error "#!/usr/bin/env scheme\n"))
(check "bad character" '(read-language "invalid character #\\( in language name" (10 11))
  (language-line-error "#!test/doc("))
(check "bad character inside the name" '(read-language "invalid character #\\. in language name" (6 7))
  (language-line-error "#!test.doc"))
(check "empty segment" '(read-language "empty segment in language name test//doc" (2 11))
  (language-line-error "#!test//doc"))
(check "trailing slash" '(read-language "empty segment in language name test/" (2 7))
  (language-line-error "#!test/"))
(check "directive"
  '(read-language "#!r6rs is a Chez Scheme directive, not a language" (2 6))
  (language-line-error "#!r6rs\n"))
(check "name starting with a directive"
  '(read-language "language name r6rsfoo starts with the Chez Scheme directive #!r6rs" (2 9))
  (language-line-error "#!r6rsfoo\n"))
(check "name starting with eof"
  '(read-language "language name eof-lang starts with the Chez Scheme directive #!eof" (2 10))
  (language-line-error "#!eof-lang"))
(check "language line error location"
  "Exception in read-language: empty segment in language name a//b \"a//b\" at line 1, char 3 of build/language-test-file.sls"
  (with-file "#!a//b\n"
    (lambda (path)
      (condition-text (raised (lambda () (read-lang-file path)))))))
(check "language line error without a source-file descriptor"
  '(#t #f "missing language line at char 0")
  (let ([c (raised (lambda () (read-language (open-input-string "x") #f)))])
    (list (lexical-violation? c) (source-condition? c)
          (apply format (condition-message c) (condition-irritants c)))))

(check "unknown language"
  '("test/nope" read-language "unknown language test/nope: library (test nope lang) not found" (2 11))
  (language-error-of "#!test/nope\n1"))
(check "unknown language without a source-file descriptor" '(#t #f)
  (let ([c (raised (lambda () (read-language (open-input-string "#!test/nope") #f)))])
    (list (language-error? c) (source-condition? c))))
(eval '(library (test empty lang) (export) (import (chezscheme))))
(check "library without a language"
  '("test/empty" read-language "library (test empty lang) does not export a language" (2 12))
  (language-error-of "#!test/empty"))
(eval '(library (test number lang) (export language) (import (chezscheme)) (define language 1)))
(check "library exporting something other than a language"
  '("test/number" read-language "library (test number lang) does not export a language" (2 13))
  (language-error-of "#!test/number"))

(check "lang-file? on a page" #t (lang-file? "tests/lib/pages/hello.sls"))
(check "lang-file? on a parser page" #t (lang-file? "tests/lib/pages/lines.sls"))
(check "lang-file? on a library" #f (lang-file? "tests/lib/pages/greet.sls"))
(check "lang-file? on a directive" #f (with-file "#!r6rs\n(library (x) (export) (import (rnrs)))" lang-file?))
(check "lang-file? on a directive without a delimiter" #f
  (with-file "#!r6rs(library (x) (export) (import (rnrs)))" lang-file?))
(check "lang-file? on a script header" #f (with-file "#! /usr/bin/scheme --script\n" lang-file?))
(check "lang-file? on a path header" #f (with-file "#!/usr/bin/scheme --script\n" lang-file?))
(check "lang-file? on a name starting with a directive" #t (with-file "#!r6rsfoo\n" lang-file?))
(check "lang-file? on a bad name" #t (with-file "#!a//b\n" lang-file?))
(check "lang-file? on an empty file" #f (with-file "" lang-file?))
(check "lang-file? needs a path" 'lang-file? (assertion-of (lambda () (lang-file? 'x))))

(let-values ([(language forms) (read-lang-file "tests/lib/pages/hello.sls")])
  (check "s-exp language name" "test/doc" (language-name language))
  (check "s-exp language libraries" '((test doc lang)) (language-libraries language))
  (check "s-exp language forms"
    '((import (pages greet)) (define (up x) (list 'page-up x)) (greet (up "reader")) "text")
    (map syntax->datum forms))
  (check "s-exp forms are annotated" '(#t #t #t #t) (map annotation? forms))
  (check "s-exp form location" '("tests/lib/pages/hello.sls" 2 1) (location (car forms)))
  (check "handler form location" '("tests/lib/pages/hello.sls" 4 8)
    (location (cadr (annotation-expression (caddr forms))))))

(let-values ([(language forms) (read-lang-file "tests/lib/pages/lines.sls")])
  (check "parser language name" "test/lines" (language-name language))
  (check "parser language libraries" '((test lines lang)) (language-libraries language))
  (check "parser language has no readtable" #f (language-readtable language))
  (check "parser language forms" '((line "first line") (line "second line"))
    (map syntax->datum forms))
  (check "parser form location" '("tests/lib/pages/lines.sls" 3 1) (location (cadr forms))))

(check "plain datums without a source-file descriptor" '((up x))
  (let ([port (open-input-string "#!test/doc ^x")])
    (read-language-forms (read-language port #f) port #f)))
(check "parser datums without a source-file descriptor" '((line "a"))
  (let ([port (open-input-string "#!test/lines\na")])
    (read-language-forms (read-language port #f) port #f)))
(check "parser must return a list" 'read-language-forms
  (assertion-of
    (lambda ()
      (read-language-forms (make-parser-language (lambda (port sfd) 'x) '() 'begin)
        (open-input-string "") #f))))

(let ([language (make-s-exp-language default-readtable (read-mode r6rs) '((rnrs)) 'begin)])
  (check "unread language has no name" #f (language-name language))
  (check "unread language has no libraries" '() (language-libraries language))
  (check "s-exp language fields"
    (list default-readtable 'r6rs '((rnrs)) 'begin '() #f)
    (list (language-readtable language) (language-mode language) (language-imports language)
          (language-body-macro language) (language-parts language) (language-lexer language)))
  (let ([lexed (language-with-lexer language 'lexer)])
    (check "language-with-lexer sets the lexer" 'lexer (language-lexer lexed))
    (check "language-with-lexer keeps the other fields"
      (list default-readtable 'r6rs '((rnrs)) 'begin)
      (list (language-readtable lexed) (language-mode lexed) (language-imports lexed)
            (language-body-macro lexed)))
    (check "language-with-lexer leaves the original" #f (language-lexer language))))
(check "parser language fields" '(#f #f ((x)) x-begin)
  (let ([language (make-parser-language (lambda (port sfd) '()) '((x)) 'x-begin)])
    (list (language-readtable language) (language-mode language) (language-imports language)
          (language-body-macro language))))
(check "plain language fields" (list default-readtable 'chez '() #f)
  (let ([language (make-plain-language default-readtable (read-mode chez))])
    (list (language-readtable language) (language-mode language) (language-imports language)
          (language-body-macro language))))
(check "language? on a language" #t (language? (make-plain-language default-readtable (read-mode chez))))
(check "language? on a symbol" #f (language? 'x))

(check "s-exp language needs a readtable" 'make-s-exp-language
  (assertion-of (lambda () (make-s-exp-language #f (read-mode chez) '() 'b))))
(check "s-exp language needs a mode" 'make-s-exp-language
  (assertion-of (lambda () (make-s-exp-language default-readtable 'lisp '() 'b))))
(check "s-exp language needs a list of imports" 'make-s-exp-language
  (assertion-of (lambda () (make-s-exp-language default-readtable (read-mode chez) '(rnrs . x) 'b))))
(check "s-exp language needs a symbol body macro" 'make-s-exp-language
  (assertion-of (lambda () (make-s-exp-language default-readtable (read-mode chez) '() "b"))))
(check "parser language needs a procedure" 'make-parser-language
  (assertion-of (lambda () (make-parser-language 'read '() 'b))))
(check "parser language needs a list of imports" 'make-parser-language
  (assertion-of (lambda () (make-parser-language values 'x 'b))))
(check "parser language needs a symbol body macro" 'make-parser-language
  (assertion-of (lambda () (make-parser-language values '() #f))))
(check "plain language needs a readtable" 'make-plain-language
  (assertion-of (lambda () (make-plain-language 'x (read-mode chez)))))
(check "plain language needs a mode" 'make-plain-language
  (assertion-of (lambda () (make-plain-language default-readtable 3))))
(check "language-with-lexer needs a language" 'language-with-lexer
  (assertion-of (lambda () (language-with-lexer 'x 'lexer))))
(check "read-language needs a textual port" 'read-language
  (assertion-of (lambda () (read-language (open-bytevector-input-port #vu8()) #f))))
(check "read-language needs a source-file descriptor or #f" 'read-language
  (assertion-of (lambda () (read-language (open-input-string "") 'sfd))))
(check "read-language-forms needs a language" 'read-language-forms
  (assertion-of (lambda () (read-language-forms 'x (open-input-string "") #f))))
(check "read-language-forms needs a port it can reposition for a readtable" 'read-language-forms
  (assertion-of
    (lambda ()
      (read-language-forms (make-plain-language default-readtable (read-mode chez))
        (make-custom-textual-input-port "p" (lambda (s start n) 0) (lambda () 0) #f #f)
        #f))))
(check "read-lang-file needs a path" 'read-lang-file
  (assertion-of (lambda () (read-lang-file 'x))))

(check "language-error condition" '(#t #t "x/y")
  (let ([c (make-language-error "x/y")])
    (list (language-error? c) (error? c) (language-error-name c))))

(check "lock returns the thunk's values" '(1 2)
  (call-with-values (lambda () (call-with-library-lock (lambda () (values 1 2)))) list))
(check "lock is recursive" 'inner
  (call-with-library-lock (lambda () (call-with-library-lock (lambda () 'inner)))))
(check "lock needs a procedure" 'call-with-library-lock
  (assertion-of (lambda () (call-with-library-lock 'x))))

(printf "language-test: ~a failures\n" failures)
(exit (if (= failures 0) 0 1))
