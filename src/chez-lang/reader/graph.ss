(define-record-type placeholder
  (nongenerative)
  (sealed #t)
  (fields n bfp efp (mutable object) (mutable seen?) (mutable visited?))
  (protocol
    (lambda (new)
      (lambda (n bfp efp) (new n bfp efp #f #f #f)))))

(define-record-type delayed-record
  (nongenerative)
  (sealed #t)
  (fields rtd vals bfp efp (mutable update))
  (protocol
    (lambda (new)
      (lambda (rtd vals bfp efp) (new rtd vals bfp efp (lambda (x) x))))))

;; Chez sets an annotation's fields when it resolves graph marks, but a
;; documented annotation cannot be changed after it is made.  An annotation
;; whose expression or stripped datum is not yet known is therefore read as
;; a pending annotation, made into an annotation once both are.
(define-record-type pending-annotation
  (nongenerative)
  (sealed #t)
  (fields (mutable expression) source (mutable stripped)
          (mutable state) (mutable annotation) (mutable update))
  (protocol
    (lambda (new)
      (lambda (expression source stripped)
        (new expression source stripped 'new #f (lambda (x) x))))))

(define (unresolved? x)
  (or (placeholder? x) (delayed-record? x) (pending-annotation? x)))

(define (start-marks! r)
  (unless (reader-marks r)
    (reader-marks-set! r (make-eq-hashtable))
    (reader-records-set! r (make-eq-hashtable))))

;; record-rtd rejects an opaque record, but the walk must reach a graph
;; reference in any record the reader builds, so it keeps their types.
(define (note-record! r x rtd)
  (when (and (reader-marks r) (not (delayed-record? x)))
    (eq-hashtable-set! (reader-records r) x rtd)))

;; Chez keys marks with eq?, so a bignum mark is never found again.
(define (mark-placeholders r n bfp efp)
  (let ([cell (eq-hashtable-cell (reader-marks r) n #f)])
    (unless (cdr cell)
      (set-cdr! cell (cons (make-placeholder n bfp efp)
                           (and (reader-sfd r) (make-placeholder n bfp efp)))))
    (cdr cell)))

(define (datum-expression x)
  (cond
    [(annotation? x) (annotation-expression x)]
    [(pending-annotation? x) (pending-annotation-expression x)]
    [else x]))

(define (datum-stripped x)
  (cond
    [(annotation? x) (annotation-stripped x)]
    [(pending-annotation? x) (pending-annotation-stripped x)]
    [else x]))

(define (set-car-slot! p i v) (set-car! p v))
(define (set-cdr-slot! p i v) (set-cdr! p v))
(define (set-box-slot! b i v) (set-box! b v))

(define (set-vector-slot! v i x) (vector-set! v i x))

;; Every annotation the reader makes holds resolved values; only pending
;; annotations have slots to fill.
(define (no-slot! a i v)
  (assertion-violationf 'reader-read "unresolved graph reference inside ~s" a))

(define (fix-graph r x)
  (define work-list '())
  (define seen (make-eq-hashtable))

  (define (record-type x)
    (or (eq-hashtable-ref (reader-records r) x #f)
        (and (record? x) (not (record-type-descriptor? x)) (record-rtd x))))

  (define (field-setter rtd)
    (lambda (rec i v) ((csv7:record-field-mutator rtd i) rec v)))

  (define (seen-before? x)
    (let ([cell (eq-hashtable-cell seen x #f)])
      (or (cdr cell) (begin (set-cdr! cell #t) #f))))

  (define (add-update! dr update! obj idx)
    (let ([f (delayed-record-update dr)])
      (delayed-record-update-set! dr (lambda (v) (update! obj idx v) (f v)))))

  (define (add-pending-update! p update! obj idx)
    (let ([f (pending-annotation-update p)])
      (pending-annotation-update-set! p (lambda (v) (update! obj idx v) (f v)))))

  (define (pending-ready? p)
    (not (or (unresolved? (pending-annotation-expression p))
             (unresolved? (pending-annotation-stripped p)))))

  (define (materialize! p)
    (let ([a (make-annotation (pending-annotation-expression p)
                              (pending-annotation-source p)
                              (pending-annotation-stripped p))])
      (pending-annotation-annotation-set! p a)
      (pending-annotation-state-set! p 'done)
      ((pending-annotation-update p) a)))

  (define (set-expression-slot! p i v)
    (pending-annotation-expression-set! p v)
    (when (and (eq? (pending-annotation-state p) 'waiting) (pending-ready? p))
      (materialize! p)))

  (define (set-stripped-slot! p i v)
    (pending-annotation-stripped-set! p v)
    (when (and (eq? (pending-annotation-state p) 'waiting) (pending-ready? p))
      (materialize! p)))

  (define (fix-pending p update! obj idx)
    (when (eq? (pending-annotation-state p) 'new)
      (pending-annotation-state-set! p 'fixing)
      (fix (pending-annotation-expression p) set-expression-slot! p #f)
      (fix (pending-annotation-stripped p) set-stripped-slot! p #f)
      (pending-annotation-state-set! p 'waiting)
      (when (pending-ready? p) (materialize! p)))
    (if (eq? (pending-annotation-state p) 'done)
        (update! obj idx (pending-annotation-annotation p))
        (add-pending-update! p update! obj idx)))

  (define (resolve-placeholder x update! obj idx)
    (unless (placeholder-seen? x)
      (lexical-error r (placeholder-bfp x) (placeholder-efp x)
        "mark #~s= missing" (placeholder-n x)))
    (let ([z (placeholder-object x)])
      (if (placeholder-visited? x)
          (if (placeholder? z)
              (resolve-placeholder z update! obj idx)
              (begin
                (update! obj idx z)
                (when (delayed-record? z) (add-update! z update! obj idx))
                (when (pending-annotation? z) (fix-pending z update! obj idx))))
          (begin
            (placeholder-visited?-set! x #t)
            (update! obj idx z)
            (fix z update! obj idx)))))

  (define (fix x update! obj idx)
    (cond
      [(placeholder? x) (resolve-placeholder x update! obj idx)]
      [(pending-annotation? x) (fix-pending x update! obj idx)]
      [(not (or (pair? x) (vector? x) (box? x) (record-type x))) (void)]
      [(seen-before? x)
       (when (delayed-record? x) (add-update! x update! obj idx))]
      [(delayed-record? x)
       (fix (delayed-record-vals x) no-slot! x #f)
       (add-update! x update! obj idx)
       (set! work-list (cons x work-list))]
      [(pair? x)
       (fix (car x) set-car-slot! x #f)
       (fix (cdr x) set-cdr-slot! x #f)]
      [(vector? x)
       (let ([n (vector-length x)])
         (do ([i 0 (fx+ i 1)]) ((fx= i n))
           (fix (vector-ref x i) set-vector-slot! x i)))]
      [(box? x) (fix (unbox x) set-box-slot! x #f)]
      [(annotation? x)
       (fix (annotation-expression x) no-slot! x #f)
       (fix (annotation-stripped x) no-slot! x #f)]
      [else
       (let ([rtd (record-type x)])
         (let loop ([fields (csv7:record-type-field-names rtd)] [i 0])
           (unless (null? fields)
             (fix ((csv7:record-field-accessor rtd i) x) (field-setter rtd) x i)
             (loop (cdr fields) (fx+ i 1)))))]))

  (define (unfinished? v)
    (and (pending-annotation? v) (not (eq? (pending-annotation-state v) 'done))))

  (define (finished v)
    (if (and (pending-annotation? v) (eq? (pending-annotation-state v) 'done))
        (pending-annotation-annotation v)
        v))

  (define waiting-records 0)

  (define (construct! dr)
    (let* ([rtd (delayed-record-rtd dr)]
           [fields (csv7:record-type-field-names rtd)]
           [vals (map finished (delayed-record-vals dr))]
           [rec (apply (record-constructor rtd) vals)]
           [set-field! (field-setter rtd)])
      (note-record! r rec rtd)
      (for-each
        (lambda (f v)
          (cond
            [(delayed-record? v) (add-update! v set-field! rec f)]
            [(unfinished? v) (add-pending-update! v set-field! rec f)]))
        fields vals)
      ((delayed-record-update dr) rec)))

  ;; Chez's annotations can be changed after a record holding them is made;
  ;; a pending annotation cannot be put in an immutable field until it is
  ;; done.  Construction waits for those, while the work list below decides
  ;; progress and errors by Chez's rule, so the error reported is Chez's.
  (define (construct-when-ready! dr)
    (let* ([rtd (delayed-record-rtd dr)]
           [awaited (let loop ([fields (csv7:record-type-field-names rtd)]
                               [vals (delayed-record-vals dr)])
                      (cond
                        [(null? fields) '()]
                        [(and (unfinished? (car vals))
                              (not (csv7:record-field-mutable? rtd (car fields))))
                         (cons (car vals) (loop (cdr fields) (cdr vals)))]
                        [else (loop (cdr fields) (cdr vals))]))])
      (if (null? awaited)
          (construct! dr)
          (begin
            (set! waiting-records (fx+ waiting-records 1))
            (add-pending-update! (car awaited)
              (lambda (dr i v)
                (set! waiting-records (fx- waiting-records 1))
                (construct-when-ready! dr))
              dr #f)))))

  (define (build-delayed! dr)
    (let ([rtd (delayed-record-rtd dr)])
      (and (for-all (lambda (f v) (or (not (delayed-record? v)) (csv7:record-field-mutable? rtd f)))
                    (csv7:record-type-field-names rtd)
                    (delayed-record-vals dr))
           (begin (construct-when-ready! dr) #t))))

  (let ([top (cons x #f)])
    (fix x set-car-slot! top #f)
    (let loop ([wl work-list] [rest '()] [progress? #f])
      (cond
        [(pair? wl)
         (if (build-delayed! (car wl))
             (loop (cdr wl) rest #t)
             (loop (cdr wl) (cons (car wl) rest) progress?))]
        [(null? rest)
         (assert (fx= waiting-records 0))
         (car top)]
        [progress? (loop rest '() #f)]
        [else
         (let ([dr (car rest)])
           (lexical-error r (delayed-record-bfp dr) (delayed-record-efp dr)
             "unresolvable cycle constructing record of type ~s" (delayed-record-rtd dr)))]))))
