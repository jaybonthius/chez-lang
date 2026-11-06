(define (check-reader who r)
  (unless (reader? r) (assertion-violationf who "~s is not a reader" r)))

(define (check-port who port)
  (unless (and (input-port? port) (textual-port? port))
    (assertion-violationf who "~s is not a textual input port" port))
  (unless (port-has-port-position? port)
    (assertion-violationf who "~s does not have port positions" port))
  ;; An error's excerpt, and a #[...] record named by its uid, are read again
  ;; from the port.
  (unless (port-has-set-port-position!? port)
    (assertion-violationf who "~s does not support set-port-position!" port)))

(define (check-readtable who rt)
  (unless (readtable? rt) (assertion-violationf who "~s is not a readtable" rt)))

(define (check-mode who mode)
  (unless (read-mode? mode)
    (assertion-violationf who "~s is not a read mode" mode)))

(define (check-position who pos)
  (unless (and (fixnum? pos) (fxnonnegative? pos))
    (assertion-violationf who "~s is not a valid position" pos)))

(define (make-reader port readtable mode)
  (define who 'make-reader)
  (check-port who port)
  (check-readtable who readtable)
  (check-mode who mode)
  (open-reader port #f readtable mode))

(define (make-annotating-reader port sfd readtable mode)
  (define who 'make-annotating-reader)
  (check-port who port)
  (unless (source-file-descriptor? sfd)
    (assertion-violationf who "~s is not a source-file descriptor" sfd))
  (check-readtable who readtable)
  (check-mode who mode)
  (open-reader port sfd readtable mode))

;; A handler's nested read must not resolve marks: a reference may name a
;; mark later in the enclosing datum.
(define (reading r read)
  (if (fx= (reader-depth r) 0)
      (begin
        (reader-marks-set! r #f)
        (reader-make-source-set! r (current-make-source-object))
        (let ([x (read)])
          (if (and (reader-marks r) (not (eof-object? x)))
              (fix-graph r x)
              x)))
      (read)))

(define (read-next r)
  (reading r
    (lambda ()
      (let-values ([(type value bfp efp) (scan-token r #f)])
        (if (or (eq? type 'eof)
                (and (eq? type 'atomic) (eof-object? value) (fx= (reader-depth r) 0)))
            value
            (let-values ([(x stripped end) (read-datum r type value bfp efp)])
              x))))))

(define reader-read
  (case-lambda
    [(r)
     (check-reader 'reader-read r)
     (read-next r)]
    [(r readtable)
     (check-reader 'reader-read r)
     (check-readtable 'reader-read readtable)
     (let ([outer (reader-readtable r)])
       (dynamic-wind
         (lambda () (reader-readtable-set! r readtable))
         (lambda () (read-next r))
         (lambda () (reader-readtable-set! r outer))))]))

(define (reader-read-list r close)
  (define who 'reader-read-list)
  (check-reader who r)
  (unless (char? close) (assertion-violationf who "~s is not a character" close))
  (let ([start (or (reader-handler-start r) (port-position (reader-port r)))])
    (reading r
      (lambda ()
        (let loop ()
          (let-values ([(type value bfp efp) (scan-token r close)])
            (case type
              [(close) '()]
              [(eof) (eof-error r start efp "list")]
              [(rparen)
               (if (char=? close #\])
                   (lexical-error-at-end r start efp "bracketed list terminated by parenthesis")
                   (read-datum r type value bfp efp))]
              [(rbrack)
               (if (char=? close #\))
                   (lexical-error-at-end r start efp "parenthesized list terminated by bracket")
                   (read-datum r type value bfp efp))]
              [else
               (let-values ([(x stripped end) (read-datum r type value bfp efp)])
                 (cons x (loop)))])))))))

(define (read-all r)
  (check-reader 'read-all r)
  (let loop ()
    (let ([x (read-next r)])
      (if (eof-object? x) '() (cons x (loop))))))

(define (strip x)
  (if (or (pair? x) (vector? x) (box? x))
      (strip-shared x)
      (datum-stripped x)))

;; A handler's datum may share structure or be cyclic, so each node is
;; copied once.
(define (strip-shared x)
  (let ([copies (make-eq-hashtable)])
    (let walk ([x x])
      (cond
        [(annotation? x) (annotation-stripped x)]
        [(pending-annotation? x) (pending-annotation-stripped x)]
        [(pair? x)
         (or (hashtable-ref copies x #f)
             (let ([copy (cons #f #f)])
               (hashtable-set! copies x copy)
               (set-car! copy (walk (car x)))
               (set-cdr! copy (walk (cdr x)))
               copy))]
        [(vector? x)
         (or (hashtable-ref copies x #f)
             (let ([copy (make-vector (vector-length x))])
               (hashtable-set! copies x copy)
               (do ([i 0 (fx+ i 1)]) ((fx= i (vector-length x)) copy)
                 (vector-set! copy i (walk (vector-ref x i))))))]
        [(box? x)
         (or (hashtable-ref copies x #f)
             (let ([copy (box #f)])
               (hashtable-set! copies x copy)
               (set-box! copy (walk (unbox x)))
               copy))]
        [else x]))))

(define (reader-annotate r datum start)
  (define who 'reader-annotate)
  (check-reader who r)
  (check-position who start)
  (if (reader-sfd r)
      (annotate r datum (strip datum) start (port-position (reader-port r)))
      datum))

(define (reader-error r start message . irritants)
  (define who 'reader-error)
  (check-reader who r)
  (check-position who start)
  (unless (string? message) (assertion-violationf who "~s is not a string" message))
  (raise-reader-error r start (port-position (reader-port r)) start message irritants))

(define (raise-read-error who sfd start end excerpt message . irritants)
  (define self 'raise-read-error)
  (unless (or (not sfd) (source-file-descriptor? sfd))
    (assertion-violationf self "~s is not a source-file descriptor" sfd))
  (check-position self start)
  (check-position self end)
  (unless (string? excerpt) (assertion-violationf self "~s is not a string" excerpt))
  (unless (string? message) (assertion-violationf self "~s is not a string" message))
  (raise-with-continuation
    (if sfd
        (condition
          (read-condition who #f message irritants)
          (excerpt-condition (current-make-source-object) sfd start end excerpt))
        (read-condition who #f "~? at char ~a" (list message irritants start)))))

(define (open-source-port path)
  (unless (string? path) (assertion-violationf 'open-source-port "~s is not a string" path))
  (let* ([bytes (open-file-input-port path)]
         [sfd (make-source-file-descriptor path bytes #t)]
         [text (call-with-port (transcoded-port bytes (current-transcoder)) get-string-all)])
    (values (open-input-string (if (eof-object? text) "" text)) sfd)))
