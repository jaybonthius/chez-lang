(import (chezscheme) (chez-lang body))

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

(define (assertion-of thunk)
  (guard (c [(assertion-violation? c) (condition-who c)]) (thunk) #f))

;; Each expression is expanded where it is evaluated, so a failure to expand
;; is reported as a check rather than failing the build. defined-names emits
;; the form in a module whose interface is the names, so a name that does not
;; match a binding the form makes is a missing definition.
(define (run expression)
  (eval
    `(let ()
       (define-syntax names-of
         (lambda (x)
           (lambda (lookup)
             (syntax-case x ()
               [(_ form)
                (with-syntax ([(name ...) (form-defined-names #'form lookup)])
                  #''(name ...))]))))
       (define-syntax defined-names
         (lambda (x)
           (lambda (lookup)
             (syntax-case x ()
               [(_ form)
                (with-syntax ([(name ...) (form-defined-names #'form lookup)])
                  #'(let () (module (name ...) form) '(name ...)))]))))
       (define-syntax definition?
         (lambda (x)
           (lambda (lookup)
             (syntax-case x ()
               [(_ form) (if (definition-form? #'form lookup) #'#t #'#f)]))))
       (define-syntax define-term
         (syntax-rules ()
           [(_ name text) (define name (list 'term text))]))
       (define-property define-term definer
         (lambda (form)
           (syntax-case form ()
             [(_ name text) (list #'name)])))
       (define-syntax define-plain-term
         (syntax-rules ()
           [(_ name text) (define name (list 'term text))]))
       ,expression)
    (environment '(chezscheme) '(chez-lang body) '(chez-lang include))))

(check "define a variable" '(a) (run '(defined-names (define a 1))))
(check "define a procedure" '(b) (run '(defined-names (define (b x) x))))
(check "define without a value" '(c) (run '(defined-names (define c))))
(check "define-syntax" '(d) (run '(defined-names (define-syntax d (identifier-syntax 1)))))
(check "define-syntax with a procedure head" '(e)
  (run '(defined-names (define-syntax (e x) #'1))))
(check "alias" '(f) (run '(defined-names (alias f car))))
(check "define-values" '(g h i) (run '(defined-names (define-values (g h . i) (values 1 2 3)))))
(check "define-values with one formal" '(j) (run '(defined-names (define-values j (values)))))
(check "define-values with no formals" '() (run '(defined-names (define-values () (values)))))
(check "define-record-type with default names"
  '(point make-point point? point-x point-y point-y-set! point-z point-w get-v set-v!)
  (run '(defined-names
          (define-record-type point
            (nongenerative)
            (fields x (mutable y) (immutable z) (immutable w point-w) (mutable v get-v set-v!))))))
(check "define-record-type with explicit names"
  '(pt new-pt is-pt? pt-a)
  (run '(defined-names (define-record-type (pt new-pt is-pt?) (sealed #t) (fields a)))))
(check "define-record-type with a parent"
  '(child make-child child? child-c)
  (run '(let ()
          (define-record-type base (fields p))
          (defined-names (define-record-type child (parent base) (fields c))))))
(check "define-enumeration" '(color color-set)
  (run '(defined-names (define-enumeration color (red green) color-set))))
(check "define-condition-type" '(&oops make-oops oops? oops-detail oops-place)
  (run '(defined-names
          (define-condition-type &oops &error make-oops oops?
            (detail oops-detail) (place oops-place)))))
(check "define-ftype" '(cell)
  (run '(defined-names (define-ftype cell (struct [a int] [b double])))))
(check "define-ftype with several types" '(left right)
  (run '(defined-names (define-ftype [left int] [right (* left)]))))
(check "named module" '(m) (run '(defined-names (module m (q) (define q 1)))))
(check "anonymous module" '(r s)
  (run '(defined-names
          (module (r (s r2)) (define r 1) (define-syntax s (identifier-syntax r2)) (define r2 2)))))
(check "meta definition" '(s) (run '(defined-names (meta define s 1))))
(check "meta module" '(mm) (run '(defined-names (meta module mm (t) (define t 1)))))
(check "meta begin" '(mb1 mb2) (run '(defined-names (meta begin (define mb1 1) (define mb2 2)))))
(check "define-structure" '(make-st st? st-a set-st-a! st-b set-st-b!)
  (run '(defined-names (define-structure (st a) ((b 2))))))
(check "trace-define" '(tf) (run '(defined-names (trace-define (tf x) x))))
(check "trace-define-syntax" '(ts)
  (run '(defined-names (trace-define-syntax ts (syntax-rules () [(_) 1])))))
(check "import-only" '() (run '(names-of (import-only (chezscheme)))))
(check "indirect-export" '() (run '(names-of (indirect-export car cdr))))
(check "implicit-exports" '() (run '(names-of (implicit-exports #t))))
(check "define-property" '() (run '(names-of (define-property car definer 1))))
(check "import" '() (run '(defined-names (import (only (chezscheme) car)))))
(check "export" '() (run '(names-of (export car))))
(check "include" '() (run '(names-of (include "no-such-file.ss"))))
(check "include/lang" '(#t ()) (run '(list (definition? (include/lang "no-such-file")) (names-of (include/lang "no-such-file")))))

(check "definer of a user macro" '(#t (tuna))
  (run '(list (definition? (define-term tuna "a fish")) (defined-names (define-term tuna "a fish")))))
(check "definition macro without definer is an expression" #f
  (run '(definition? (define-plain-term tuna "a fish"))))
(check "expression" #f (run '(definition? (car '(1)))))
(check "begin is not a definition" #f (run '(definition? (begin (define a 1)))))
(check "identifier" #f (run '(definition? car)))
(check "shadowed keyword" #f (run '(let ([define list]) (definition? (define a 1)))))
(check "unbound head" #f (run '(definition? (no-such-definer a 1))))

(define (invalid who text) (list 'raised (format "Exception in ~a: invalid syntax ~a" who text)))

(check "define without a name" (invalid 'define "(define)") (run '(names-of (define))))
(check "define of a non-identifier" (invalid 'define "(define 1 2)") (run '(names-of (define 1 2))))
(check "define with an empty body" (invalid 'define "(define (f))") (run '(names-of (define (f)))))
(check "define-syntax without a value" (invalid 'define-syntax "(define-syntax s)") (run '(names-of (define-syntax s))))
(check "alias of a non-identifier" (invalid 'alias "(alias a 1)") (run '(names-of (alias a 1))))
(check "define-values with a bad formal" (invalid 'define-values "(define-values (a 1) (values 1 2))")
  (run '(names-of (define-values (a 1) (values 1 2)))))
(check "define-record-type with a bad name" (invalid 'define-record-type "(define-record-type (a b) (fields x))")
  (run '(names-of (define-record-type (a b) (fields x)))))
(check "define-record-type with a bad field" (invalid 'define-record-type "(define-record-type a (fields 1))")
  (run '(names-of (define-record-type a (fields 1)))))
(check "define-enumeration without a constructor" (invalid 'define-enumeration "(define-enumeration e (x))")
  (run '(names-of (define-enumeration e (x)))))
(check "define-condition-type with a bad field" (invalid 'define-condition-type "(define-condition-type &c &error make-c c? (f))")
  (run '(names-of (define-condition-type &c &error make-c c? (f)))))
(check "define-ftype without a type" (invalid 'define-ftype "(define-ftype 1)") (run '(names-of (define-ftype 1))))
(check "module without an interface" (invalid 'module "(module m)") (run '(names-of (module m))))
(check "meta of an expression" (invalid 'meta "(meta (car x))") (run '(names-of (meta (car x)))))
(check "meta begin of an expression" (invalid 'meta "(meta begin (car x))")
  (run '(names-of (meta begin (car x)))))
(check "names of an expression" '(raised "Exception in form-defined-names: not a definition (car x)")
  (run '(names-of (car x))))

(check "definer property that is not a procedure"
  '(raised "Exception in form-defined-names: the definer property of define-odd is #t, not a procedure")
  (run '(let ()
          (define-syntax define-odd (syntax-rules () [(_ name) (define name 1)]))
          (define-property define-odd definer #t)
          (names-of (define-odd x)))))
(check "definer that returns a non-identifier"
  '(raised "Exception in form-defined-names: the definer of define-odd returned (x), not a list of identifiers")
  (run '(let ()
          (define-syntax define-odd (syntax-rules () [(_ name) (define name 1)]))
          (define-property define-odd definer (lambda (form) '(x)))
          (names-of (define-odd x)))))
(check "definition-form? needs a lookup procedure" 'definition-form?
  (assertion-of (lambda () (definition-form? #'(define x 1) 'lookup))))
(check "form-defined-names needs a lookup procedure" 'form-defined-names
  (assertion-of (lambda () (form-defined-names #'(define x 1) 'lookup))))

(define-syntax unbound?
  (lambda (x)
    (syntax-case x ()
      [(_ id) (if (unbound-identifier? #'id) #''unbound #''bound)])))

(define page-only 'page)

(check "imported name is bound" 'bound (unbound? car))
(check "unbound name" 'unbound (unbound? no-such-name))
(check "name bound only in the program" 'bound (unbound? page-only))
(check "local name" 'bound (let ([local 1]) (unbound? local)))
(check "macro name" 'bound (unbound? check))
(check "a name the probe library binds only with its prefix is bound here" 'bound (unbound? define))
(check "unbound-identifier? needs an identifier" 'unbound-identifier?
  (assertion-of (lambda () (unbound-identifier? 'x))))

(check "body-begin"
  '((first second (1 2 term)) 1 2 term 5)
  (run '(let ()
          (define events '())
          (define (note! x) (set! events (cons x events)))
          (module terms (define-word)
            (define-syntax define-word
              (syntax-rules () [(_ name) (define name 'term)]))
            (define-property define-word definer
              (lambda (form) (syntax-case form () [(_ name) (list #'name)]))))
          (module page ()
            (body-begin (pages body)
              (note! 'first)
              (define a 1)
              (begin (define b (+ a 1)) (note! 'second))
              (import terms)
              (define-word c)
              (define-record-type rec (fields f))
              (values 1 2)
              (values)
              (note! (list a b c))))
          (import page)
          (list (reverse events) a b c (rec-f (make-rec 5))))))
(let ([path "build/body-test-include.ss"])
  (call-with-output-file path (lambda (p) (put-string p "(car '(1))\n")) 'replace)
  (check "a definition after a spliced expression is the form reported"
    '(raised "Exception: invalid context for definition (define z 2)")
    (run `(let () (module page () (body-begin (pages body) (include ,path) (define z 2))) 'ok)))
  (delete-file path))
(check "empty body-begin" 'empty
  (run '(let () (module page () (body-begin (pages body))) 'empty)))

(printf "body-test: ~a failures\n" failures)
(exit (if (= failures 0) 0 1))
