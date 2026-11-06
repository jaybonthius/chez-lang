(import (chezscheme) (chez-lang properties) (chez-lang language) (chez-lang loader))

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

(define sfd (make-source-file-descriptor "text" (open-bytevector-input-port (bytevector))))

(define (new-annotation)
  (make-annotation 'x (make-source-object sfd 0 1) 'x))

(define colour (make-annotation-property 'colour))
(define size (make-annotation-property 'size))

(check "property predicate" '(#t #f) (list (annotation-property? colour) (annotation-property? 'colour)))
(check "absent property returns the default" 'none (annotation-property-ref colour (new-annotation) 'none))
(check "set then ref" 'red
  (let ([a (new-annotation)])
    (annotation-property-set! colour a 'red)
    (annotation-property-ref colour a 'none)))
(check "set replaces" 'blue
  (let ([a (new-annotation)])
    (annotation-property-set! colour a 'red)
    (annotation-property-set! colour a 'blue)
    (annotation-property-ref colour a 'none)))
(check "properties are separate" '(red none)
  (let ([a (new-annotation)])
    (annotation-property-set! colour a 'red)
    (list (annotation-property-ref colour a 'none) (annotation-property-ref size a 'none))))
(check "annotations are separate" '(red none)
  (let ([a (new-annotation)] [b (new-annotation)])
    (annotation-property-set! colour a 'red)
    (list (annotation-property-ref colour a 'none) (annotation-property-ref colour b 'none))))
(check "properties of the same name are separate" 'none
  (let ([a (new-annotation)] [other (make-annotation-property 'colour)])
    (annotation-property-set! colour a 'red)
    (annotation-property-ref other a 'none)))

(check "a value that holds its annotation does not keep it alive" 100
  (let ([g (make-guardian)])
    (do ([i 0 (+ i 1)]) ((= i 100))
      (let ([a (new-annotation)])
        (annotation-property-set! colour a (list a))
        (g a)))
    (collect (collect-maximum-generation))
    (let count ([n 0])
      (if (g) (count (+ n 1)) n))))

(check "name must be a symbol" 'make-annotation-property
  (assertion-of (lambda () (make-annotation-property "colour"))))
(check "set needs a property" 'annotation-property-set!
  (assertion-of (lambda () (annotation-property-set! 'colour (new-annotation) 'red))))
(check "set needs an annotation" 'annotation-property-set!
  (assertion-of (lambda () (annotation-property-set! colour 'x 'red))))
(check "ref needs a property" 'annotation-property-ref
  (assertion-of (lambda () (annotation-property-ref 'colour (new-annotation) 'none))))
(check "ref needs an annotation" 'annotation-property-ref
  (assertion-of (lambda () (annotation-property-ref colour 'x 'none))))

(check "threads set and read properties at once" '(#t #t #t #t)
  (let ([mutex (make-mutex)] [finished (make-condition)] [results '()])
    (do ([i 0 (+ i 1)]) ((= i 4))
      (fork-thread
        (lambda ()
          (let* ([annotations (map (lambda (j) (new-annotation)) (iota 20000))]
                 [result
                  (guard (c [#t (condition-text c)])
                    (for-each (lambda (a j) (annotation-property-set! colour a (+ i j)))
                              annotations (iota 20000))
                    (for-all (lambda (a j) (eqv? (annotation-property-ref colour a #f) (+ i j)))
                             annotations (iota 20000)))])
            (with-mutex mutex
              (set! results (cons result results))
              (condition-signal finished))))))
    (with-mutex mutex
      (let wait ()
        (unless (= (length results) 4)
          (condition-wait finished mutex)
          (wait)))
      results)))

(define (read-text text sfd)
  (let ([port (open-input-string text)])
    (read-language-forms (read-language port sfd) port sfd)))

(define (evaluate-shapes text sfd)
  (eval (car (read-text text sfd)) (environment '(chezscheme) '(test shape))))

(define shapes-text "#!test/shape\n(list (shape-of [a b]) (shape-of (a b)) (shape-of x))\n")

(check "property set by a reader handler is read by a transformer" '(bracket paren paren)
  (evaluate-shapes shapes-text
    (make-source-file-descriptor "shapes" (open-bytevector-input-port (string->utf8 shapes-text)))))
(check "unannotated forms have no properties" '(unannotated unannotated unannotated)
  (evaluate-shapes shapes-text #f))

;; The loader reads a page and compiles it with compile-to-file in one
;; process, so the transformers see the properties the reader set.
(define page-root "build/properties-test")
(define page-source (string-append page-root "/pages/shape.sls"))
(define page-object (string-append page-root "/pages/shape.so"))

(define (write-page text)
  (for-each (lambda (dir) (unless (file-directory? dir) (mkdir dir)))
            (list page-root (path-parent page-source)))
  (call-with-output-file page-source (lambda (p) (put-string p text)) 'replace))

(define (remove-page)
  (for-each (lambda (file) (when (file-exists? file) (delete-file file)))
            (list page-source page-object))
  (for-each (lambda (dir) (when (file-directory? dir) (delete-directory dir)))
            (list (path-parent page-source) page-root)))

(write-page "#!test/shape\n(define shapes (list (shape-of [1 2]) (shape-of (1 2))))\n")
(check "properties reach the transformers of a compiled page" '((bracket paren) #t)
  (parameterize ([library-directories (cons (cons page-root page-root) (library-directories))]
                 [compile-file-message #f])
    (compile-lang-library page-source page-object)
    (list (eval 'shapes (environment '(pages shape))) (file-exists? page-object))))
(remove-page)

(printf "properties-test: ~a failures\n" failures)
(exit (if (= failures 0) 0 1))
