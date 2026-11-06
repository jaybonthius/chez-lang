(import (chezscheme) (chez-lang reader))

(include "differential.ss")

(define failures 0)

(define (fail! name . details)
  (set! failures (+ failures 1))
  (parameterize ([print-graph #t] [print-length 12] [print-level 6])
    (printf "FAIL ~a~{ ~s~}\n" name details)))

(define-syntax check
  (syntax-rules ()
    [(_ name expected expr)
     (let ([actual (guard (c [#t (list 'raised (condition-text c))]) expr)])
       (unless (same-datum? actual expected) (fail! name 'expected expected 'actual actual)))]))

(define (condition-text c)
  (call-with-string-output-port (lambda (p) (display-condition c p))))

(define (raises? predicate thunk)
  (guard (c [#t (predicate c)]) (thunk) #f))

(define (read-text text rt)
  (read-all (make-reader (open-input-string text) rt (read-mode chez))))

(define (read-annotated text rt)
  (read-all (make-annotating-reader (open-input-string text) (text-sfd text) rt (read-mode chez))))

(define (span a)
  (let ([src (annotation-source a)])
    (list (source-object-bfp src) (source-object-efp src))))

(define (read-error-text text rt)
  (guard (c [(lexical-violation? c) (condition-text c)])
    (read-annotated text rt)
    #f))

(define (read-error-message text rt)
  (guard (c [(lexical-violation? c)
             (apply format (condition-message c) (condition-irritants c))])
    (read-text text rt)
    #f))

(define (read-up r c start)
  (let ([datum (reader-read r)])
    (when (eof-object? datum)
      (reader-error r start "unexpected end-of-file reading ~a" "^"))
    (reader-annotate r (list 'up datum) start)))

(define up-readtable (readtable-extend-datum-start default-readtable #\^ read-up))

(define (read-at r c start)
  (reader-annotate r (list 'at (reader-read r)) start))

(define at-readtable (readtable-extend-terminating default-readtable #\@ read-at))

(define (skip r c start) reader-comment)

(define skip-readtable (readtable-extend-datum-start default-readtable #\~ skip))

(define (read-braces r c start)
  (reader-annotate r (reader-read-list r #\}) start))

(define brace-readtable (readtable-extend-terminating default-readtable #\{ read-braces))

(check "datum-start handler" '((a (up b) c)) (read-text "(a ^b c)" up-readtable))
(check "datum-start character does not delimit" '(a^b) (read-text "a^b" up-readtable))
(check "datum-start handler span" '((3 7) (4 7))
  (let* ([elements (annotation-expression (car (read-annotated "(a ^\"b\" c)" up-readtable)))]
         [up (cadr elements)])
    (list (span up) (span (cadr (annotation-expression up))))))
(check "handler error location"
  "Exception in reader-read: unexpected end-of-file reading ^ \"^\" at line 2, char 3 of build/reader-test-error.ss"
  (let ([path "build/reader-test-error.ss"])
    (call-with-output-file path (lambda (p) (put-string p "(a\n  ^")) 'replace)
    (let-values ([(ip sfd) (open-source-port path)])
      (let ([text (guard (c [(lexical-violation? c) (condition-text c)])
                    (read-all (make-annotating-reader ip sfd up-readtable (read-mode chez))))])
        (delete-file path)
        text))))

(check "terminating handler splits a symbol" '(a (at b)) (read-text "a@b" at-readtable))
(check "terminating handler ends a number" '(1 (at 2)) (read-text "1@2" at-readtable))
(check "terminating handler ends a boolean" '(#t (at x)) (read-text "#t@x" at-readtable))
(check "terminating handler ends a character" '(#\a (at x)) (read-text "#\\a@x" at-readtable))
(check "terminating character after #\\ is the character" '(#\@) (read-text "#\\@" at-readtable))
(check "terminating handler inside a string" '("a@b") (read-text "\"a@b\"" at-readtable))
(check "terminating handler inside bars" (list (string->symbol "a@b")) (read-text "|a@b|" at-readtable))

(check "overriding ("
  '(#(1 2) x)
  (read-text "(1 2) x"
    (readtable-extend-datum-start default-readtable #\(
      (lambda (r c start) (list->vector (reader-read-list r #\)))))))
(check "overriding ;"
  '(a semicolon b)
  (read-text "a ; b"
    (readtable-extend-datum-start default-readtable #\; (lambda (r c start) 'semicolon))))

(define lambda-readtable
  (readtable-extend-dispatch default-readtable #\l
    (lambda (r c start) (reader-annotate r (list 'lambda '(x) (reader-read r)) start))))
(check "dispatch handler" '((lambda (x) y) #t) (read-text "#ly #t" lambda-readtable))
(check "dispatch handler span" '(0 4) (span (car (read-annotated "#l y" lambda-readtable))))

(check "comment at top level" '(a b) (read-text "~ a ~ b ~" skip-readtable))
(check "comment in a list" '((a b)) (read-text "(a ~ b ~)" skip-readtable))
(check "comment after a datum comment" '(b) (read-text "#; ~ a b" skip-readtable))

(define nested-readtable
  (readtable-extend-datum-start default-readtable #\!
    (lambda (r c start)
      (let ([datum (reader-read r up-readtable)])
        (reader-annotate r (list 'bang datum) start)))))
(check "nested read with another readtable" '((bang (up x)) ^y) (read-text "!^x ^y" nested-readtable))
(check "readtable after a nested read" default-readtable
  (let ([r (make-reader (open-input-string "!^x") nested-readtable (read-mode chez))])
    (reader-read r)
    (if (eq? (reader-readtable r) nested-readtable) default-readtable 'changed)))

(check "read-list" '((a b) c) (read-text "{a b} c" brace-readtable))
(check "read-list span" '(0 5) (span (car (read-annotated "{a b} c" brace-readtable))))
(check "read-list ends a symbol" '(a (b)) (read-text "a{b}" brace-readtable))

(define bracket-readtable
  (readtable-extend-dispatch default-readtable #\<
    (lambda (r c start) (reader-read-list r #\]))))
(check "read-list with ]" '((a b)) (read-text "#<a b]" bracket-readtable))
(check "read-list with a wrong closer" "bracketed list terminated by parenthesis at char 6 of"
  (let ([text (read-error-message "#<a b)" bracket-readtable)])
    (substring text 0 (min (string-length text) 53))))
(check "read-list at end of input" "Exception in reader-read: unexpected end-of-file reading list \"{a b\" at char 0 of text"
  (parameterize ([current-locate-source-object-source (lambda (src start? cache?) (values))])
    (read-error-text "{a b" brace-readtable)))
(check "read-list with a dot" "unexpected dot (.)"
  (let ([text (read-error-message "{a . b}" brace-readtable)])
    (substring text 0 (min (string-length text) 18))))

(check "unmatched closer" "unexpected close parenthesis"
  (let ([text (read-error-message ")" default-readtable)])
    (substring text 0 (min (string-length text) 28))))

(check "mark through a handler is shared" #t
  (let ([x (car (read-text "#0=(a ^#0#)" up-readtable))])
    (eq? x (cadr (cadr x)))))
(define vector-readtable
  (readtable-extend-datum-start default-readtable #\^
    (lambda (r c start) (vector (reader-read r)))))
(check "handler embeds an unresolved reference" #t
  (let ([x (car (read-text "#0=(a ^#0#)" vector-readtable))])
    (eq? x (vector-ref (cadr x) 0))))
(check "annotated handler embeds an unresolved reference" #t
  (let ([x (annotation-expression (car (read-annotated "#0=(a ^#0#)" vector-readtable)))])
    (eq? x (annotation-expression (vector-ref (annotation-expression (cadr x)) 0)))))
(check "reader-annotate annotates the lists a handler builds" '(#t #t #t (quote (changed x)))
  (let* ([rt (readtable-extend-datum-start default-readtable #\$
               (lambda (r c start)
                 (reader-annotate r (list 'quote (list 'changed (reader-read r))) start)))]
         [a (car (read-annotated "$x" rt))]
         [inner (cadr (annotation-expression a))])
    (list (annotation? inner)
          (annotation? (car (annotation-expression inner)))
          (equal? (map (lambda (s) (list (source-object-bfp s) (source-object-efp s)))
                       (list (annotation-source inner) (annotation-source a)))
                  '((0 2) (0 2)))
          (annotation-stripped a))))
(check "a handler's unannotated result is annotated" '(#t (a (quote x)))
  (let* ([rt (readtable-extend-datum-start default-readtable #\$
               (lambda (r c start) (list 'quote (reader-read r))))]
         [a (car (read-annotated "(a $x)" rt))])
    (list (annotation? (cadr (annotation-expression a))) (annotation-stripped a))))
(check "annotated mark through a handler is shared" #t
  (let* ([a (car (read-annotated "#0=(a ^#0#)" up-readtable))]
         [x (annotation-expression a)]
         [up (annotation-expression (cadr x))])
    (eq? x (annotation-expression (cadr up)))))

(define (assertion? thunk) (raises? assertion-violation? thunk))

(check "datum-start rejects whitespace" #t
  (assertion? (lambda () (readtable-extend-datum-start default-readtable #\space skip))))
(check "terminating rejects whitespace" #t
  (assertion? (lambda () (readtable-extend-terminating default-readtable #\x2028 skip))))
(check "dispatch rejects digits" #t
  (assertion? (lambda () (readtable-extend-dispatch default-readtable #\7 skip))))
(check "extension needs a readtable" #t
  (assertion? (lambda () (readtable-extend-dispatch 'rt #\a skip))))
(check "extension needs a character" #t
  (assertion? (lambda () (readtable-extend-datum-start default-readtable "a" skip))))
(check "extension needs a procedure" #t
  (assertion? (lambda () (readtable-extend-terminating default-readtable #\a 'skip))))
(check "readtables are not changed by extension" #f (readtable-handler default-readtable #\^))
(check "readtable-handler" read-up (readtable-handler up-readtable #\^))
(check "readtable-dispatch-handler" #f (readtable-dispatch-handler up-readtable #\^))
(check "make-reader needs a mode" #t
  (assertion? (lambda () (make-reader (open-input-string "") default-readtable 'scheme))))
(check "make-reader names itself for a mode that is not a symbol" 'make-reader
  (guard (c [(assertion-violation? c) (condition-who c)])
    (make-reader (open-input-string "") default-readtable 3)))
(check "read-mode?" '(#t #t #f #f)
  (map read-mode? (list (read-mode chez) (read-mode r6rs) 'scheme 3)))
(check "make-reader needs a textual port" #t
  (assertion? (lambda () (make-reader (open-bytevector-input-port #vu8()) default-readtable (read-mode chez)))))
(check "annotating reader needs a port it can reposition" #t
  (assertion?
    (lambda ()
      (make-annotating-reader
        (make-custom-textual-input-port "p" (lambda (s start n) 0) (lambda () 0) #f #f)
        (text-sfd "") default-readtable (read-mode chez)))))
(check "plain reader needs a port it can reposition" #t
  (assertion?
    (lambda ()
      (make-reader
        (make-custom-textual-input-port "p" (lambda (s start n) 0) (lambda () 0) #f #f)
        default-readtable (read-mode chez)))))
(check "reader-error needs a fixnum position" #t
  (assertion? (lambda () (reader-error (make-reader (open-input-string "") default-readtable (read-mode chez)) 1.0 "x"))))
(check "reader-read needs a reader" #t (assertion? (lambda () (reader-read 'r))))
(check "reader-read needs a readtable" #t
  (assertion? (lambda () (reader-read (make-reader (open-input-string "") default-readtable (read-mode r6rs)) 'rt))))

(check "read-all stops at a top-level #!eof" '(a b) (read-text "a b #!eof c" default-readtable))
(check "#!eof inside a list" (list 'a (eof-object)) (car (read-text "(a #!eof)" default-readtable)))
(check "accessors" #t
  (let* ([ip (open-input-string "")] [sfd (text-sfd "")]
         [r (make-annotating-reader ip sfd up-readtable (read-mode r6rs))])
    (and (eq? (reader-port r) ip) (eq? (reader-sfd r) sfd) (eq? (reader-readtable r) up-readtable)
         (not (reader-sfd (make-reader ip up-readtable (read-mode r6rs))))
         (reader? r))))
(check "reader-annotate without annotations" '(up x)
  (car (read-text "^x" up-readtable)))
(check "raise-read-error with a source"
  '("bad ~a" ("token") 3 "0123456789abcdefghij")
  (guard (c [(lexical-violation? c)
             (list (condition-message c) (condition-irritants c)
                   (printed-position c) (syntax->datum (source-condition-form c)))])
    (raise-read-error 'parse (text-sfd "") 3 40 "0123456789abcdefghijklmn" "bad ~a" "token")))
(check "reader error conditions" '((#t #t #t #t) (#t #t #f #t))
  (map (lambda (text)
         (let ([ip (open-input-string text)])
           (guard (c [(lexical-violation? c)
                      (list (i/o-read-error? c)
                            (and (i/o-port-error? c) (eq? (i/o-error-port c) ip))
                            (implementation-restriction-violation? c)
                            (continuation-condition? c))])
             (reader-read (make-annotating-reader ip (text-sfd text) default-readtable (read-mode chez))))))
       '("1/0" "(a")))
(check "raise-read-error conditions" '(#t #f #f #t)
  (guard (c [(lexical-violation? c)
             (list (i/o-read-error? c) (i/o-port-error? c)
                   (implementation-restriction-violation? c) (continuation-condition? c))])
    (raise-read-error 'parse (text-sfd "") 3 4 "x" "bad ~a" "token")))
(check "raise-read-error without a source"
  "Exception in parse: bad token at char 3"
  (guard (c [(lexical-violation? c) (condition-text c)])
    (raise-read-error 'parse #f 3 4 "x" "bad ~a" "token")))
(check "open-source-port" '((a "λ") (3 6))
  (let ([path "build/reader-test-source.ss"])
    (call-with-output-file path (lambda (p) (put-string p "(a \"λ\")")) 'replace)
    (let-values ([(ip sfd) (open-source-port path)])
      (let ([x (reader-read (make-annotating-reader ip sfd default-readtable (read-mode chez)))])
        (delete-file path)
        (list (annotation-stripped x) (span (cadr (annotation-expression x))))))))

(define-record-type spot (fields x (mutable y)))
(define-record-type frozen (fields x y))
(record-reader 'pt (record-type-descriptor spot))
(define-record-type point (nongenerative point) (fields x y))
(record-reader 'frozen (record-type-descriptor frozen))
(define-record-type om (opaque #t) (fields (mutable x)))
(define-record-type pi (fields x))
(record-reader 'om (record-type-descriptor om))
(record-reader 'pi (record-type-descriptor pi))

(check "graph reference inside an opaque record" #t
  (let ([x (car (read-text "#0=#[om #0#]" default-readtable))])
    (eq? x (om-x x))))
(check "delayed record inside an opaque record" #t
  (let ([x (car (read-text "#0=#[om #[pi #0#]]" default-readtable))])
    (eq? x (pi-x (om-x x)))))
(check "annotated graph reference inside an opaque record" #t
  (let ([x (annotation-expression (car (read-annotated "#0=#[om #0#]" default-readtable)))])
    (eq? x (annotation-expression (om-x x)))))
(check "annotated delayed record inside an opaque record" #t
  (let* ([x (annotation-expression (car (read-annotated "#0=#[om #[pi #0#]]" default-readtable)))]
         [inner (annotation-expression (om-x x))])
    (eq? x (annotation-expression (pi-x inner)))))

;; The type of a profile counter is built into Chez and named by its uid.
(define uid-record "#[#{profile-counter b5vnnom9h4o4uny0-2} 0]")

(define record-texts
  (list "#[pt 1 2]" "#[#{pt chez-lang-reader-test} 1 2]" "#0=#[pt 1 #0#]" "#0=#[frozen 1 #0#]" "#0=(#[frozen #0# 2])"
        "#0=#[frozen 1 #1=#[pt #0# #1#]]" "(#[frozen 1 #1#] #1=#[pt #2# 3] #2=z)"
        "#0=(#[frozen #0# 2])" "#0=#[pt #1=#[frozen #0# 1] #1#]" "(#[pt 1])" "#[pt 1 2 3]" "#[nope 1]" "#[1 2]" "#[point 1 2]" "(a #[point 1 #[point 2 3]])"
        uid-record (string-append "(" uid-record " x)") "#[#{nope abc} 1]"))

(for-each
  (lambda (text)
    (for-each
      (lambda (mode)
        (let ([d (or (plain-difference text mode) (annotated-difference text mode))])
          (when d (fail! "record" d))))
      '(chez r6rs)))
  record-texts)

(define uid-rtd
  (make-record-type-descriptor 'ruid #f (gensym "ruid" "chez-lang-reader-test-uid") #f #f
    '#((immutable x))))

(check "a uid record is read by its type" #t
  (let ([x (car (read-text "#[#{ruid chez-lang-reader-test-uid} 1]" default-readtable))])
    (and (record? x uid-rtd) (eqv? ((record-accessor uid-rtd 0) x) 1))))
(check "a record named by a plain-symbol uid" '(#t #t)
  (list (point? (car (read-text "#[point 1 2]" default-readtable)))
        (point? (annotation-expression (car (read-annotated "#[point 1 2]" default-readtable))))))
(check "readtable does not apply inside a uid record" '(^x (up y))
  (let ([x (car (read-text "(#[#{ruid chez-lang-reader-test-uid} ^x] ^y)" up-readtable))])
    (list ((record-accessor uid-rtd 0) (car x)) (cadr x))))
(check "annotated uid record span" '((1 39) (40 42))
  (let ([x (annotation-expression
             (car (read-annotated "(#[#{ruid chez-lang-reader-test-uid} 1] ^y)" up-readtable)))])
    (list (span (car x)) (span (cadr x)))))

(define texts
  '("#!fold-case ABC #\\SPACE |X| A\\BC #{Pp Qq} #:Gg"
    "#!fold-case #!no-fold-case ABC"
    "#!r6rsfoo" "#!chezschemefoo" "#!r6rs (a b) #!chezscheme {a}"
    "1/2e3" "#x1.8" "#e1@1" "1/0e5" "#e1/0e2" "#i1/0" "#e+inf.0" "#e1e400" "1e1000000000"
    "#e+\x130;" "#36rzeus" "#40r1" "1|10" "1.5|53" "+i" "1@+i" "1+2" "-nan.0x" "98##.#e2"
    "#\\nul #\\rubout #\\nel #\\x41 #\\X41 #\\377 #\\400 #\\SPACE #\\spac"
    "\"a\\x41;b\\101\\'\\\n   c\" \"\\x110000;\""
    "a\\x41;b a\\x110000;" "#vu8(1 #x10 #e1) #vu8(256) #vu8(a) #vfx(1 2) #vfl(1.0) #3vfx(1)"
    "#2vs(a) #7vs(a b c) #vs(a) #0vs()" "#10(1) #2(a b c)" "#%car #2%car #4%car #%|a b|"
    "#0=#0# (#1=#1# . #1#) #2=(a . #2#) #3=#4=(b . #3#) #4#"
    "#1000000000000000000000=a #1000000000000000000000#"
    "(a . b c) (a . ) ( . a) (a . b . c) [a . b)"
    "#;#0=a #0#" "#& #&x" "#@" "#5q" "#!base-rtd #!bwp"))

(for-each
  (lambda (text)
    (for-each
      (lambda (mode)
        (let ([d (or (plain-difference text mode) (annotated-difference text mode))])
          (when d (fail! "text" d))))
      '(chez r6rs)))
  texts)

(check "#{p u} is the interned gensym" #t
  (and (eq? (car (read-text "#{a b}" default-readtable)) (gensym "a" "b"))
       (eq? (car (read-text "#!fold-case #{Pp |Qq|}" default-readtable)) (gensym "pp" "Qq"))))
(check "gensyms with another unique name differ" #f (same-datum? (gensym "a" "b") (gensym "a" "c")))
(check "gensyms from #: compare by pretty name" #t
  (same-datum? (car (read-text "#:a" default-readtable)) (read (open-input-string "#:a"))))
(check "case-sensitive at reader creation" '(abc)
  (parameterize ([case-sensitive #f]) (read-text "ABC" default-readtable)))

(define delimiters (string->list "()[]\";# {}'`,"))

(define (code-point-texts c)
  (let ([s (string c)])
    (append
      (list s (string-append "#\\" s) (string-append "\"" s "\""))
      (apply append
        (map (lambda (d) (list (string c d) (string d c))) delimiters)))))

(let loop ([n 0] [differences 0])
  (cond
    [(> n #x10FFFF)
     (unless (= differences 0) (fail! "code points" differences))]
    [(<= #xD800 n #xDFFF) (loop (+ n 1) differences)]
    [else
     (loop (+ n 1)
       (fold-left
         (lambda (count text)
           (fold-left
             (lambda (count mode)
               (let ([d (plain-difference text mode)])
                 (when (and d (< count 10)) (fail! "code point" d))
                 (if d (+ count 1) count)))
             count '(chez r6rs)))
         differences
         (code-point-texts (integer->char n))))]))

(printf "reader-test: ~a failures\n" failures)
(exit (if (= failures 0) 0 1))
