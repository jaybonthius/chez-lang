;; Graph marks make data cyclic, so a pair of nodes already being compared
;; is assumed equal.  #{p u} reads as the interned gensym, so two of those
;; are the same only when eq?.  A gensym from #: is fresh on every read, so
;; two of those compare by pretty name; nothing asks for their unique
;; names, so the oblist does not hold them (CSUG, "gensym").
(define (fresh-gensym? g)
  (not (memq g (oblist))))

(define (same-datum? x y)
  (define pairs (make-eq-hashtable))
  (define (visited? x y)
    (let ([cell (eq-hashtable-cell pairs x '())])
      (or (memq y (cdr cell))
          (begin (set-cdr! cell (cons y (cdr cell))) #f))))
  (let same? ([x x] [y y])
    (cond
      [(eq? x y) #t]
      [(and (gensym? x) (gensym? y))
       (and (fresh-gensym? x) (fresh-gensym? y)
            (string=? (symbol->string x) (symbol->string y)))]
      [(and (annotation? x) (annotation? y))
       (or (visited? x y)
           (let ([sx (annotation-source x)] [sy (annotation-source y)])
             (and (= (source-object-bfp sx) (source-object-bfp sy))
                  (= (source-object-efp sx) (source-object-efp sy))
                  (same? (annotation-expression x) (annotation-expression y))
                  (same? (annotation-stripped x) (annotation-stripped y)))))]
      [(and (pair? x) (pair? y))
       (or (visited? x y)
           (and (same? (car x) (car y)) (same? (cdr x) (cdr y))))]
      [(and (vector? x) (vector? y))
       (or (visited? x y)
           (and (= (vector-length x) (vector-length y))
                (let loop ([i 0])
                  (or (= i (vector-length x))
                      (and (same? (vector-ref x i) (vector-ref y i)) (loop (+ i 1)))))))]
      [(and (box? x) (box? y))
       (or (visited? x y) (same? (unbox x) (unbox y)))]
      [(and (stencil-vector? x) (stencil-vector? y))
       (or (visited? x y)
           (and (= (stencil-vector-mask x) (stencil-vector-mask y))
                (let loop ([i 0])
                  (or (= i (stencil-vector-length x))
                      (and (same? (stencil-vector-ref x i) (stencil-vector-ref y i))
                           (loop (+ i 1)))))))]
      [(and (string? x) (string? y)) (string=? x y)]
      [(and (number? x) (number? y)) (eqv? x y)]
      [(and (bytevector? x) (bytevector? y)) (bytevector=? x y)]
      [(and (fxvector? x) (fxvector? y)) (equal? x y)]
      [(and (flvector? x) (flvector? y)) (equal? x y)]
      [(and (record? x) (record? y) (not (record-type-descriptor? x)))
       (or (visited? x y)
           (let ([rtd (record-rtd x)])
             (and (eq? rtd (record-rtd y))
                  (let loop ([i 0] [fields (csv7:record-type-field-names rtd)])
                    (or (null? fields)
                        (and (same? ((csv7:record-field-accessor rtd i) x)
                                    ((csv7:record-field-accessor rtd i) y))
                             (loop (+ i 1) (cdr fields))))))))]
      [else (eqv? x y)])))

;; The location display-condition prints for c, as the position it prints;
;; returning no values makes it print the position rather than open a file.
(define (printed-position c)
  (let ([position #f])
    (parameterize ([current-locate-source-object-source
                    (lambda (src start? cache?)
                      (set! position (if start? (source-object-bfp src) (source-object-efp src)))
                      (values))])
      (display-condition c (open-output-string)))
    position))

(define-record-type read-failure
  (nongenerative)
  (sealed #t)
  (fields message irritants position kinds))

(define (condition-kinds c)
  (map (lambda (kind) (and (kind c) #t))
       (list i/o-read-error? i/o-port-error? implementation-restriction-violation?
             continuation-condition?)))

;; Without a source-file descriptor, both readers report the position in
;; the message: "~? at char ~a of ~s" with the port last.
(define (failure c shift)
  (unless (lexical-violation? c) (raise c))
  (let ([message (condition-message c)] [irritants (condition-irritants c)])
    (if (equal? message "~? at char ~a of ~s")
        (make-read-failure (car irritants) (cadr irritants) (- (caddr irritants) shift)
          (condition-kinds c))
        (make-read-failure message irritants (printed-position c) (condition-kinds c)))))

(define (read-outcome read-next shift)
  (guard (c [#t (list (failure c shift))])
    (let loop ()
      (let ([x (read-next)])
        (if (eof-object? x) '() (cons x (loop)))))))

(define (same-outcome? a b)
  (cond
    [(and (null? a) (null? b)) #t]
    [(or (null? a) (null? b)) #f]
    [(and (read-failure? (car a)) (read-failure? (car b)))
     (and (equal? (read-failure-message (car a)) (read-failure-message (car b)))
          (same-datum? (read-failure-irritants (car a)) (read-failure-irritants (car b)))
          (eqv? (read-failure-position (car a)) (read-failure-position (car b)))
          (equal? (read-failure-kinds (car a)) (read-failure-kinds (car b))))]
    [(or (read-failure? (car a)) (read-failure? (car b))) #f]
    [else (and (same-datum? (car a) (car b)) (same-outcome? (cdr a) (cdr b)))]))

(define (show-annotated x)
  (cond
    [(annotation? x)
     (let ([src (annotation-source x)])
       (list (source-object-bfp src) (source-object-efp src) (annotation-stripped x)))]
    [(and (pair? x) (annotation? (car x))) (cons (show-annotated (car x)) (cdr x))]
    [else x]))

(define (show-outcome outcome)
  (map (lambda (x)
         (if (read-failure? x)
             (list 'error (read-failure-position x)
                   (apply format (read-failure-message x) (read-failure-irritants x)))
             (show-annotated x)))
       outcome))

;; Chez's reader in R6RS mode reads the text after "#!r6rs ".
(define r6rs-prefix "#!r6rs ")

(define (chez-plain-outcome text mode)
  (let ([ip (open-input-string (if (eq? mode 'r6rs) (string-append r6rs-prefix text) text))])
    (read-outcome (lambda () (read ip))
                  (if (eq? mode 'r6rs) (string-length r6rs-prefix) 0))))

(define (chez-lang-plain-outcome text mode)
  (let ([r (make-reader (open-input-string text) default-readtable mode)])
    (read-outcome (lambda () (reader-read r)) 0)))

(define (chez-annotated-outcome text sfd mode)
  (let* ([shift (if (eq? mode 'r6rs) (string-length r6rs-prefix) 0)]
         [ip (open-input-string (if (eq? mode 'r6rs) (string-append r6rs-prefix text) text))]
         [fp 0])
    (parameterize ([current-make-source-object
                    (lambda (sfd bfp efp) (make-source-object sfd (- bfp shift) (- efp shift)))])
      (read-outcome
        (lambda ()
          (let-values ([(x end) (get-datum/annotations ip sfd fp)])
            (set! fp end)
            (if (eof-object? x) x (cons x (- end shift)))))
        shift))))

(define (chez-lang-annotated-outcome text sfd mode)
  (let* ([ip (open-input-string text)]
         [r (make-annotating-reader ip sfd default-readtable mode)])
    (read-outcome
      (lambda ()
        (let ([x (reader-read r)])
          (if (eof-object? x) x (cons x (port-position ip)))))
      0)))

(define (text-sfd text)
  (make-source-file-descriptor "text" (open-bytevector-input-port (string->utf8 text))))

(define (annotated-difference text mode)
  (let* ([sfd (text-sfd text)]
         [chez (chez-annotated-outcome text sfd mode)]
         [ours (chez-lang-annotated-outcome text sfd mode)])
    (and (not (same-outcome? chez ours))
         (list text mode (show-outcome chez) (show-outcome ours)))))

(define (plain-difference text mode)
  (let ([chez (chez-plain-outcome text mode)] [ours (chez-lang-plain-outcome text mode)])
    (and (not (same-outcome? chez ours))
         (list text mode (show-outcome chez) (show-outcome ours)))))

(define (report-differences name differences limit)
  (let loop ([ds differences] [n 0])
    (when (and (pair? ds) (< n limit))
      (parameterize ([print-graph #t] [print-length 12] [print-level 6])
        (apply printf "~s in ~s mode:\n  chez:      ~s\n  chez-lang: ~s\n" (car ds)))
      (loop (cdr ds) (+ n 1))))
  (printf "~a: ~a differences\n" name (length differences)))
