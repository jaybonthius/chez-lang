(define excerpt-length 20)

;; The excerpt is read back from the port because the reader keeps no copy
;; of the text it has consumed.
(define (port-excerpt ip bfp efp)
  (let ([here (port-position ip)])
    (set-port-position! ip bfp)
    (let ([s (get-string-n ip (max 0 (min excerpt-length (- efp bfp))))])
      (set-port-position! ip here)
      (if (eof-object? s) "" s))))

;; Chez's reader errors carry these conditions, so a handler written for
;; Chez's reader catches chez-lang's; &format makes display-condition
;; format the message with the irritants.
(define (read-condition who port message irritants)
  (condition
    (make-lexical-violation)
    (make-i/o-read-error)
    (if port (make-i/o-port-error port) (condition))
    (make-who-condition who)
    (make-message-condition message)
    (make-irritants-condition irritants)
    (make-format-condition)))

;; display-condition prints a &source form before the location, so the
;; form is the offending text.
(define (excerpt-condition make-source sfd pos efp excerpt)
  (let ([excerpt (if (fx> (string-length excerpt) excerpt-length)
                     (substring excerpt 0 excerpt-length)
                     excerpt)])
    (make-source-condition (make-annotation excerpt (make-source sfd pos efp) excerpt))))

;; As Chez's errors do, the condition carries the continuation of the
;; error for the debugger.
(define (raise-with-continuation c)
  (call/cc
    (lambda (k)
      (raise (condition c (make-continuation-condition k))))))

(define (reader-condition r bfp efp printed message irritants)
  (let ([ip (reader-port r)] [sfd (reader-sfd r)])
    (if sfd
        (condition
          (read-condition 'reader-read ip message irritants)
          (excerpt-condition (reader-make-source r) sfd printed efp (port-excerpt ip bfp efp)))
        (read-condition 'reader-read ip "~? at char ~a of ~s"
          (list message irritants printed ip)))))

(define (raise-reader-error r bfp efp printed message irritants)
  (raise-with-continuation (reader-condition r bfp efp printed message irritants)))

(define (unrepresentable-error r bfp efp s)
  (raise-with-continuation
    (condition
      (make-implementation-restriction-violation)
      (reader-condition r bfp efp bfp "cannot represent ~a" (list s)))))

(define (lexical-error r bfp efp message . irritants)
  (raise-reader-error r bfp efp bfp message irritants))

(define (lexical-error-at-end r bfp efp message . irritants)
  (raise-reader-error r bfp efp efp message irritants))

(define (eof-error r bfp efp what)
  (raise-reader-error r bfp efp bfp "unexpected end-of-file reading ~a" (list what)))

(define (nonstandard-error r bfp efp what)
  (raise-reader-error r bfp efp bfp "~a syntax is not allowed in #!r6rs mode" (list what)))

(define (check-standard r bfp efp what)
  (when (reader-r6rs? r) (nonstandard-error r bfp efp what)))

(define (check-standard-delimiter r bfp efp c)
  (when (reader-r6rs? r)
    (raise-reader-error r bfp efp bfp "delimiter ~a is not allowed in #!r6rs mode" (list c))))
