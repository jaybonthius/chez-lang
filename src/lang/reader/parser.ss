(define (annotate r x stripped bfp efp)
  (let ([source ((reader-make-source r) (reader-sfd r) bfp efp)])
    (if (and (reader-marks r) (or (unresolved? x) (unresolved? stripped)))
        (make-pending-annotation x source stripped)
        (make-annotation x source stripped))))

(define (read-datum r type value bfp efp)
  (case type
    [(datum)
     (if (reader-sfd r)
         (let ([a (annotate-built r value bfp efp)])
           (values a (datum-stripped a) efp))
         (values value (datum-stripped value) efp))]
    [(record-brack) (read-record r bfp)]
    [else
     (let-values ([(x stripped end) (read-datum-body r type value bfp efp)])
       (if (reader-sfd r)
           (values (annotate r x stripped bfp end) stripped end)
           (values x #f end)))]))

(define (read-datum-body r type value bfp efp)
  (case type
    [(atomic) (values value value efp)]
    [(lparen)
     (read-list r bfp 'rparen 'rbrack "parenthesized list terminated by bracket" "list")]
    [(lbrack)
     (read-list r bfp 'rbrack 'rparen "bracketed list terminated by parenthesis" "bracketed list")]
    [(quote) (read-quote r value bfp)]
    [(vparen) (read-vector r bfp)]
    [(vnparen) (read-sized-vector r value bfp efp)]
    [(vu8paren) (read-bytevector r bfp)]
    [(vu8nparen) (read-sized-bytevector r value bfp efp)]
    [(vfxparen) (read-fxvector r bfp)]
    [(vfxnparen) (read-sized-fxvector r value bfp efp)]
    [(vflparen) (read-flvector r bfp)]
    [(vflnparen) (read-sized-flvector r value bfp efp)]
    [(vsparen) (lexical-error r bfp efp "mask required for stencil vector")]
    [(vsnparen) (read-stencil-vector r value bfp efp)]
    [(box) (read-box r bfp)]
    [(mark) (read-mark r value bfp efp)]
    [(insert) (read-insert r value bfp efp)]
    [(fasl)
     (lexical-error r bfp efp
       "unsupported old fasl format detected---use new format with binary i/o")]
    [(rparen) (lexical-error r bfp efp "unexpected close parenthesis")]
    [(rbrack) (lexical-error r bfp efp "unexpected close bracket")]
    [(dot) (lexical-error r bfp efp "unexpected dot (.)")]
    [else (lexical-error-at-end r bfp efp "unexpected end-of-file")]))

(define (read-list r expr-bfp close other wrong what)
  (define annotating? (reader-sfd r))
  (define (wrong-close efp) (lexical-error-at-end r expr-bfp efp wrong))
  (define (element type value bfp efp)
    (let*-values ([(x stripped-x end) (read-datum r type value bfp efp)]
                  [(rest stripped-rest end) (tail)])
      (values (cons x rest) (and annotating? (cons stripped-x stripped-rest)) end)))
  (define (tail)
    (let-values ([(type value bfp efp) (scan-token r #f)])
      (cond
        [(eq? type close) (values '() '() efp)]
        [(eq? type other) (wrong-close efp)]
        [(eq? type 'eof) (eof-error r expr-bfp efp what)]
        [(eq? type 'dot) (dotted-tail)]
        [else (element type value bfp efp)])))
  (define (dotted-tail)
    (let-values ([(type value bfp efp) (scan-token r #f)])
      (cond
        [(eq? type close) (lexical-error-at-end r bfp efp "expected one item after dot (.)")]
        [(eq? type 'eof) (eof-error r expr-bfp efp what)]
        [else
         (let-values ([(x stripped-x end) (read-datum r type value bfp efp)])
           (let-values ([(type value bfp efp) (scan-token r #f)])
             (cond
               [(eq? type close) (values x stripped-x efp)]
               [(eq? type other) (wrong-close efp)]
               [(eq? type 'dot) (lexical-error r bfp efp "unexpected dot")]
               [(eq? type 'eof) (eof-error r expr-bfp efp what)]
               [else (lexical-error r bfp efp "more than one item found after dot (.)")])))])))
  (let-values ([(type value bfp efp) (scan-token r #f)])
    (cond
      [(eq? type close) (values '() '() efp)]
      [(eq? type other) (wrong-close efp)]
      [(eq? type 'eof) (eof-error r expr-bfp efp what)]
      [else (element type value bfp efp)])))

(define (read-quote r kind expr-bfp)
  (let-values ([(type value bfp efp) (scan-token r #f)])
    (if (eq? type 'eof)
        (eof-error r expr-bfp efp (symbol->string kind))
        (let-values ([(x stripped-x end) (read-datum r type value bfp efp)])
          (values (list kind x) (and (reader-sfd r) (list kind stripped-x)) end)))))

(define (read-box r expr-bfp)
  (let-values ([(type value bfp efp) (scan-token r #f)])
    (if (eq? type 'eof)
        (eof-error r expr-bfp efp "box")
        (let-values ([(x stripped-x end) (read-datum r type value bfp efp)])
          (values (box x) (and (reader-sfd r) (box stripped-x)) end)))))

(define (read-vector r expr-bfp)
  (define annotating? (reader-sfd r))
  (let loop ([i 0])
    (let-values ([(type value bfp efp) (scan-token r #f)])
      (case type
        [(rparen) (values (make-vector i) (and annotating? (make-vector i)) efp)]
        [(eof) (eof-error r expr-bfp efp "vector")]
        [else
         (let*-values ([(x stripped-x end) (read-datum r type value bfp efp)]
                       [(v stripped-v end) (loop (fx+ i 1))])
           (vector-set! v i x)
           (when annotating? (vector-set! stripped-v i stripped-x))
           (values v stripped-v end))]))))

;; Chez reports a bad length without the # and the (.
(define (check-length r n bfp efp what)
  (unless (and (fixnum? n) (fxnonnegative? n))
    (lexical-error r (+ bfp 1) (- efp 1) (format "invalid ~a length ~~s" what) n)))

(define (fill-tail! v i n ref put!)
  (when (fx< 0 i n)
    (let ([last (ref v (fx- i 1))])
      (do ([i i (fx+ i 1)]) ((fx= i n)) (put! v i last)))))

(define (read-sized-vector r n bfp efp)
  (define annotating? (reader-sfd r))
  (check-length r n bfp efp "vector")
  (let ([v (make-vector n)] [stripped-v (and annotating? (make-vector n))] [expr-bfp bfp])
    (let loop ([i 0])
      (let-values ([(type value bfp efp) (scan-token r #f)])
        (case type
          [(rparen)
           (fill-tail! v i n vector-ref vector-set!)
           (when annotating? (fill-tail! stripped-v i n vector-ref vector-set!))
           (values v stripped-v efp)]
          [(eof) (eof-error r expr-bfp efp "vector")]
          [else
           (let-values ([(x stripped-x end) (read-datum r type value bfp efp)])
             (unless (fx< i n)
               (lexical-error r expr-bfp end "too many vector elements supplied"))
             (vector-set! v i x)
             (when annotating? (vector-set! stripped-v i stripped-x))
             (loop (fx+ i 1)))])))))

;; Bytevector, fxvector and flvector elements are tokens, not datums, so
;; they are neither annotated nor resolved as graph references.
(define (read-homogeneous-vector r expr-bfp what make put! check)
  (let loop ([i 0])
    (let-values ([(type value bfp efp) (scan-token r #f)])
      (case type
        [(rparen) (let ([v (make i)]) (values v v efp))]
        [(eof) (eof-error r expr-bfp efp what)]
        [else
         (check type value bfp efp)
         (let-values ([(v stripped-v end) (loop (fx+ i 1))])
           (put! v i value)
           (values v stripped-v end))]))))

(define (read-sized-homogeneous-vector r n expr-bfp efp what make ref put! check)
  (check-length r n expr-bfp efp what)
  (let ([v (make n)])
    (let loop ([i 0])
      (let-values ([(type value bfp efp) (scan-token r #f)])
        (case type
          [(rparen) (fill-tail! v i n ref put!) (values v v efp)]
          [(eof) (eof-error r expr-bfp efp what)]
          [else
           (check type value bfp efp)
           (unless (fx< i n)
             (lexical-error r expr-bfp efp (format "too many ~a elements supplied" what)))
           (put! v i value)
           (loop (fx+ i 1))])))))

(define (check-fixnum r)
  (lambda (type value bfp efp)
    (unless (and (eq? type 'atomic) (fixnum? value))
      (lexical-error r bfp efp "non-fixnum found in fxvector"))))

(define (check-flonum r)
  (lambda (type value bfp efp)
    (unless (and (eq? type 'atomic) (flonum? value))
      (lexical-error r bfp efp "non-flonum found in flvector"))))

(define (check-octet r)
  (lambda (type value bfp efp)
    (unless (and (eq? type 'atomic) (fixnum? value) (fx<= 0 value 255))
      (if (eq? type 'atomic)
          (lexical-error r bfp efp "invalid value ~:[~s~;~a~] found in bytevector"
            (symbol? value) value)
          (lexical-error r bfp efp "non-octet found in bytevector")))))

(define (read-fxvector r bfp)
  (read-homogeneous-vector r bfp "fxvector" make-fxvector fxvector-set! (check-fixnum r)))

(define (read-sized-fxvector r n bfp efp)
  (read-sized-homogeneous-vector r n bfp efp "fxvector"
    make-fxvector fxvector-ref fxvector-set! (check-fixnum r)))

(define (read-flvector r bfp)
  (read-homogeneous-vector r bfp "flvector" make-flvector flvector-set! (check-flonum r)))

(define (read-sized-flvector r n bfp efp)
  (read-sized-homogeneous-vector r n bfp efp "flvector"
    make-flvector flvector-ref flvector-set! (check-flonum r)))

(define (read-bytevector r bfp)
  (read-homogeneous-vector r bfp "bytevector" make-bytevector bytevector-u8-set! (check-octet r)))

(define (read-sized-bytevector r n bfp efp)
  (read-sized-homogeneous-vector r n bfp efp "bytevector"
    make-bytevector bytevector-u8-ref bytevector-u8-set! (check-octet r)))

(define (read-stencil-vector r mask bfp efp)
  (define annotating? (reader-sfd r))
  (unless (and (fixnum? mask) (fxnonnegative? mask)
               (fx< mask (fxsll 1 (stencil-vector-mask-width))))
    (lexical-error r (+ bfp 1) (- efp 1) "invalid stencil vector mask ~s" mask))
  (let ([n (fxpopcount mask)] [expr-bfp bfp])
    (let loop ([i 0] [xs '()] [stripped-xs '()])
      (let-values ([(type value bfp efp) (scan-token r #f)])
        (case type
          [(rparen)
           (when (fx< i n)
             (lexical-error r bfp efp "not enough stencil vector elements supplied"))
           (values (apply stencil-vector mask (reverse xs))
                   (and annotating? (apply stencil-vector mask (reverse stripped-xs)))
                   efp)]
          [(eof) (eof-error r expr-bfp efp "stencil vector")]
          [else
           (let-values ([(x stripped-x end) (read-datum r type value bfp efp)])
             (unless (fx< i n)
               (lexical-error r expr-bfp end "too many stencil vector elements supplied"))
             (loop (fx+ i 1) (cons x xs) (cons stripped-x stripped-xs)))])))))

(define (read-mark r n bfp efp)
  (let* ([placeholders (mark-placeholders r n bfp efp)]
         [ins (car placeholders)]
         [stripped-ins (cdr placeholders)])
    (when (placeholder-seen? ins)
      (lexical-error r bfp efp "duplicate mark #~s= seen" n))
    ;; Marked seen before its datum is read, so a duplicate inside the datum
    ;; is the one reported.
    (placeholder-seen?-set! ins #t)
    (when stripped-ins (placeholder-seen?-set! stripped-ins #t))
    (let-values ([(type value datum-bfp datum-efp) (scan-token r #f)])
      (if (eq? type 'eof)
          (eof-error r bfp datum-efp "graph mark")
          (let-values ([(obj stripped-obj end) (read-datum r type value datum-bfp datum-efp)])
            (cond
              [(not stripped-ins)
               (placeholder-object-set! ins (if (eq? obj ins) black-hole obj))]
              [(eq? stripped-obj stripped-ins)
               (placeholder-object-set! ins black-hole)
               (placeholder-object-set! stripped-ins black-hole)]
              [else
               ;; The mark's own annotation covers #n= and its datum, so the
               ;; datum's annotation is dropped.
               (placeholder-object-set! ins (datum-expression obj))
               (placeholder-object-set! stripped-ins stripped-obj)])
            (values ins stripped-ins end))))))

(define (read-insert r n bfp efp)
  (let ([placeholders (mark-placeholders r n bfp efp)])
    (values (car placeholders) (cdr placeholders) efp)))

(define (scheme-object-field? decl)
  (memq (cadr decl) '(scheme-object ptr)))

(define (read-record r expr-bfp)
  (let-values ([(type name bfp efp) (scan-token r #f)])
    (cond
      [(eq? type 'eof) (eof-error r expr-bfp efp "record")]
      [(not (and (eq? type 'atomic) (symbol? name)))
       (lexical-error r bfp efp "non-symbol found after #[")]
      ;; Chez looks a gensym name up by its pretty name.
      [(record-reader (string->symbol (symbol->string name)))
       => (lambda (rtd) (read-record-fields r rtd name expr-bfp))]
      [else (read-record-by-chez r expr-bfp)])))

(define (read-record-fields r rtd name expr-bfp)
  (define annotating? (reader-sfd r))
  (let ([decls (csv7:record-type-field-decls rtd)])
    (let-values ([(vals stripped-vals end) (read-record-tail r expr-bfp (length decls) name)])
      ;; A field typed other than ptr cannot hold an annotation.
      (let ([vals (if annotating?
                      (map (lambda (decl v sv) (if (scheme-object-field? decl) v sv))
                           decls vals stripped-vals)
                      vals)])
        (let-values
            ([(x stripped-x)
              (let loop ([decls decls] [vs (if annotating? stripped-vals vals)])
                (cond
                  [(null? decls)
                   (values (apply (record-constructor rtd) vals)
                           (and annotating? (apply (record-constructor rtd) stripped-vals)))]
                  [(and (or (eq? (car (car decls)) 'immutable)
                            (not (scheme-object-field? (car decls))))
                        (or (placeholder? (car vs)) (delayed-record? (car vs))))
                   (values (make-delayed-record rtd vals expr-bfp end)
                           (and annotating? (make-delayed-record rtd stripped-vals expr-bfp end)))]
                  [else (loop (cdr decls) (cdr vs))]))])
          (note-record! r x rtd)
          (when stripped-x (note-record! r stripped-x rtd))
          (if annotating?
              (values (annotate r x stripped-x expr-bfp end) stripped-x end)
              (values x #f end)))))))

(define (read-record-tail r expr-bfp n name)
  (define annotating? (reader-sfd r))
  (let-values ([(type value bfp efp) (scan-token r #f)])
    (case type
      [(rbrack)
       (if (= n 0)
           (values '() '() efp)
           (lexical-error r expr-bfp efp "too few fields supplied for record ~s" name))]
      [(eof) (eof-error r expr-bfp efp "record")]
      [else
       (let-values ([(x stripped-x end) (read-datum r type value bfp efp)])
         (when (= n 0)
           (lexical-error r expr-bfp end "too many fields supplied for record ~s" name))
         (let-values ([(rest stripped-rest end) (read-record-tail r expr-bfp (- n 1) name)])
           (values (cons x rest) (and annotating? (cons stripped-x stripped-rest)) end)))])))

;; Chez finds the type of a record whose name record-reader does not know
;; only through an undocumented property, so Chez's reader reads that one
;; record, from its #[, and raises "unrecognized record name" itself.  The
;; readtable does not apply inside it, and graph marks do not cross it.
(define (read-record-by-chez r expr-bfp)
  (let ([ip (reader-port r)] [sfd (reader-sfd r)])
    (set-port-position! ip expr-bfp)
    (parameterize ([case-sensitive (not (reader-fold? r))])
      (if sfd
          (let-values ([(x end) (get-datum/annotations ip sfd expr-bfp)])
            (values x (annotation-stripped x) end))
          (let ([x (get-datum ip)])
            (values x #f (port-position ip)))))))
