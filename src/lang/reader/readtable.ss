;; Threads share readtables, so their tables are immutable and extending a
;; readtable copies the table it changes.
(define-record-type char-table
  (nongenerative)
  (sealed #t)
  (fields ascii others))

(define (char-table-ref table c)
  (and table
       (let ([n (char->integer c)])
         (if (fx< n 128)
             (vector-ref (char-table-ascii table) n)
             (hashtable-ref (char-table-others table) c #f)))))

(define (char-table-extend table c value)
  (let ([ascii (if table (vector-copy (char-table-ascii table)) (make-vector 128 #f))]
        [others (if table (hashtable-copy (char-table-others table) #t) (make-eqv-hashtable))]
        [n (char->integer c)])
    (if (fx< n 128)
        (vector-set! ascii n value)
        (hashtable-set! others c value))
    (make-char-table (vector->immutable-vector ascii) (hashtable-copy others))))

(define-record-type readtable
  (nongenerative)
  (sealed #t)
  (fields starts terminating dispatches))

(define default-readtable (make-readtable #f #f #f))

(define (check-extension who rt c handler)
  (unless (readtable? rt) (assertion-violationf who "~s is not a readtable" rt))
  (unless (char? c) (assertion-violationf who "~s is not a character" c))
  (unless (procedure? handler) (assertion-violationf who "~s is not a procedure" handler)))

(define (check-not-whitespace who c)
  (when (char-whitespace? c)
    (assertion-violationf who "whitespace character ~s cannot start a datum" c)))

(define (readtable-extend-datum-start rt c handler)
  (define who 'readtable-extend-datum-start)
  (check-extension who rt c handler)
  (check-not-whitespace who c)
  (make-readtable
    (char-table-extend (readtable-starts rt) c handler)
    (readtable-terminating rt)
    (readtable-dispatches rt)))

(define (readtable-extend-terminating rt c handler)
  (define who 'readtable-extend-terminating)
  (check-extension who rt c handler)
  (check-not-whitespace who c)
  (make-readtable
    (char-table-extend (readtable-starts rt) c handler)
    (char-table-extend (readtable-terminating rt) c #t)
    (readtable-dispatches rt)))

;; A digit after # starts a numeric prefix (#2(, #0=, #16r), which a
;; dispatch handler would make unreadable.
(define (readtable-extend-dispatch rt c handler)
  (define who 'readtable-extend-dispatch)
  (check-extension who rt c handler)
  (when (char<=? #\0 c #\9)
    (assertion-violationf who "digit ~s cannot be a dispatch character" c))
  (make-readtable
    (readtable-starts rt)
    (readtable-terminating rt)
    (char-table-extend (readtable-dispatches rt) c handler)))

(define (readtable-handler rt c)
  (define who 'readtable-handler)
  (unless (readtable? rt) (assertion-violationf who "~s is not a readtable" rt))
  (unless (char? c) (assertion-violationf who "~s is not a character" c))
  (char-table-ref (readtable-starts rt) c))

(define (readtable-dispatch-handler rt c)
  (define who 'readtable-dispatch-handler)
  (unless (readtable? rt) (assertion-violationf who "~s is not a readtable" rt))
  (unless (char? c) (assertion-violationf who "~s is not a character" c))
  (char-table-ref (readtable-dispatches rt) c))
