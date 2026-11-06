#!chezscheme
(define-enumeration read-mode (chez r6rs) read-modes)

(define (read-mode? x)
  (and (symbol? x) (enum-set-member? x (enum-set-universe (read-modes)))))

(define-record-type (reader new-reader reader?)
  (nongenerative)
  (sealed #t)
  (fields
    port
    sfd
    (mutable readtable)
    (mutable r6rs?)
    (mutable fold?)
    (mutable buffer)
    (mutable marks)
    (mutable records)
    (mutable make-source)
    (mutable depth)
    (mutable handler-start)))

(define (open-reader port sfd readtable mode)
  (new-reader port sfd readtable (eq? mode 'r6rs) (not (case-sensitive))
    (make-string 32) #f #f (current-make-source-object) 0 #f))

(define-record-type comment-marker
  (nongenerative)
  (sealed #t)
  (opaque #t))

(define reader-comment (make-comment-marker))

;; No documented procedure makes the broken weak pointer or the value Chez
;; reads for #0=#0#.
(define bwp-object '#!bwp)
(define base-rtd '#!base-rtd)
(define black-hole '#0=#0#)

(define (buffer-put! r i c)
  (let ([tb (reader-buffer r)])
    (if (fx< i (string-length tb))
        (string-set! tb i c)
        (let ([new (make-string (fx* 2 (string-length tb)))])
          (string-copy! tb 0 new 0 i)
          (string-set! new i c)
          (reader-buffer-set! r new)))))

(define (buffer-string r n)
  (substring (reader-buffer r) 0 n))

(define (intern r n slashed?)
  (let ([s (buffer-string r n)])
    (string->symbol (if (and (reader-fold? r) (not slashed?)) (string-foldcase s) s))))
