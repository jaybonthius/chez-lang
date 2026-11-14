(import (chezscheme) (lang reader))

(include "differential.ss")

(define chez-src (or (getenv "CHEZ_SRC") "tests/chez"))
(define mats (string-append chez-src "/mats"))

;; 6.ms's reader-error cases start after its read-token tests.
(define first-error-line 1806)

(define reading-procedures
  '(xmat read-test open-input-string open-string-input-port with-input-from-string))

(define (read-annotated path)
  (let-values ([(ip sfd) (open-source-port path)])
    (let loop ([fp 0])
      (let-values ([(x end) (get-datum/annotations ip sfd fp)])
        (if (eof-object? x) '() (cons x (loop end)))))))

(define (line-start text line)
  (let loop ([i 0] [line line])
    (if (= line 1)
        i
        (loop (+ i 1) (if (char=? (string-ref text i) #\newline) (- line 1) line)))))

;; Graph marks in the mats make the annotations cyclic.
(define (read-strings forms from)
  (define visited (make-eq-hashtable))
  (define (datum x) (if (annotation? x) (annotation-stripped x) x))
  (define (literals x e)
    (if (and (pair? e)
             (memq (datum (car e)) reading-procedures)
             (>= (source-object-bfp (annotation-source x)) from))
        (let ([args (map datum (if (list? (cdr e)) (cdr e) '()))])
          (filter string?
            (if (eq? (datum (car e)) 'xmat) args (list-head args (min 1 (length args))))))
        '()))
  (define (walk x)
    (if (hashtable-ref visited x #f)
        '()
        (let ([e (annotation-expression x)])
          (hashtable-set! visited x #t)
          (append
            (literals x e)
            (cond
              [(list? e) (apply append (map walk (filter annotation? e)))]
              [(vector? e) (apply append (map walk (filter annotation? (vector->list e))))]
              [else '()])))))
  (apply append (map walk forms)))

(define (mats-text path)
  (let-values ([(ip sfd) (open-source-port path)]) (get-string-all ip)))

;; 6.ms registers record names its cases use before them.
(define (register-records! forms)
  (for-each
    (lambda (form)
      (let ([x (annotation-stripped form)])
        (when (and (pair? x) (eq? (car x) 'begin)
                   (exists (lambda (y) (and (pair? y) (eq? (car y) 'record-reader))) (cdr x)))
          (eval x))))
    forms))

(define cases
  (let* ([six (string-append mats "/6.ms")] [six-forms (read-annotated six)])
    (register-records! six-forms)
    (append
      (read-strings six-forms (line-start (mats-text six) first-error-line))
      (apply append
        (map (lambda (name) (read-strings (read-annotated (string-append mats "/" name)) 0))
             (filter (lambda (name) (and (equal? (path-extension name) "ms") (not (equal? name "6.ms"))))
                     (sort string<? (directory-list mats))))))))

;; #nvu8() and #nvfl() give no elements, so the vector's contents are
;; whatever make-bytevector or make-flvector left there; only the type and
;; length can be compared.
(define (unspecified-contents? text)
  (let ([n (string-length text)])
    (and (> n 6)
         (char=? (string-ref text 0) #\#)
         (member (substring text (- n 5) n) '("vu8()" "vfl()"))
         (let loop ([i 1])
           (or (= i (- n 5))
               (and (char-numeric? (string-ref text i)) (loop (+ i 1))))))))

(define (vector-shape outcome)
  (map (lambda (x)
         (cond
           [(bytevector? x) (list 'bytevector (bytevector-length x))]
           [(flvector? x) (list 'flvector (flvector-length x))]
           [else x]))
       outcome))

(define (difference text mode)
  (if (unspecified-contents? text)
      (let ([chez (chez-plain-outcome text mode)] [ours (chez-lang-plain-outcome text mode)])
        (and (not (same-outcome? (vector-shape chez) (vector-shape ours)))
             (list text mode (show-outcome chez) (show-outcome ours))))
      (plain-difference text mode)))

(define (messages mode)
  (let ([table (make-hashtable string-hash string=?)])
    (for-each
      (lambda (text)
        (let ([outcome (chez-plain-outcome text mode)])
          (when (and (pair? outcome) (read-failure? (car (last-pair outcome))))
            (hashtable-set! table (read-failure-message (car (last-pair outcome))) #t))))
      cases)
    (hashtable-size table)))

(let ([differences
       (apply append
         (map (lambda (mode)
                (filter values (map (lambda (text) (difference text mode)) cases)))
              '(chez r6rs)))])
  (printf "~a cases; ~a distinct error messages in Chez mode, ~a in R6RS mode\n"
    (length cases) (messages 'chez) (messages 'r6rs))
  (report-differences "error-test" differences 40)
  (exit (if (null? differences) 0 1)))
