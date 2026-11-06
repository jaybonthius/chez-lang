(import (chezscheme) (chez-lang reader))

(define chez-src (or (getenv "CHEZ_SRC") "tests/chez"))
(define source-dir (string-append chez-src "/s"))

(define samples 5)
(define passes 10)
(define ratio-limit 2.0)

(define files
  (map (lambda (name)
         (let-values ([(ip sfd) (open-source-port (string-append source-dir "/" name))])
           (cons (get-string-all ip) sfd)))
       (filter (lambda (name) (equal? (path-extension name) "ss"))
               (sort string<? (directory-list source-dir)))))

(when (null? files)
  (errorf 'bench "no source files under ~a" source-dir))

;; After a datum with a graph mark both readers walk the whole datum, which
;; is most of Chez's time on those files, so they are also left out.
(define (has-mark? text)
  (let ([n (string-length text)])
    (let loop ([i 0])
      (and (fx< i n)
           (or (and (char=? (string-ref text i) #\#)
                    (let digits ([j (fx+ i 1)])
                      (and (fx< j n)
                           (let ([c (string-ref text j)])
                             (if (char-numeric? c)
                                 (digits (fx+ j 1))
                                 (and (fx> j (fx+ i 1)) (char=? c #\=)))))))
               (loop (fx+ i 1)))))))

(define (chez-annotated text sfd)
  (let ([ip (open-input-string text)])
    (let loop ([fp 0])
      (let-values ([(x end) (get-datum/annotations ip sfd fp)])
        (unless (eof-object? x) (loop end))))))

(define (chez-plain text sfd)
  (let ([ip (open-input-string text)])
    (let loop ()
      (unless (eof-object? (read ip)) (loop)))))

(define (chez-lang-annotated text sfd)
  (let ([r (make-annotating-reader (open-input-string text) sfd default-readtable (read-mode chez))])
    (let loop ()
      (unless (eof-object? (reader-read r)) (loop)))))

(define (chez-lang-plain text sfd)
  (let ([r (make-reader (open-input-string text) default-readtable (read-mode chez))])
    (let loop ()
      (unless (eof-object? (reader-read r)) (loop)))))

(define (median xs)
  (list-ref (sort < xs) (quotient (length xs) 2)))

(define (megabytes files)
  (/ (apply + (map (lambda (f) (string-length (car f))) files)) 1e6))

(define (ms-per-megabyte files read-file)
  (let ([size (megabytes files)])
    (median
      (map (lambda (sample)
             (collect (collect-maximum-generation))
             (let ([start (real-time)])
               (do ([i 0 (+ i 1)]) ((= i passes))
                 (for-each (lambda (f) (read-file (car f) (cdr f))) files))
               (/ (- (real-time) start) passes size)))
           (iota samples)))))

(define (measure title files)
  (let ([results
         (map (lambda (name read-file) (cons name (ms-per-megabyte files read-file)))
              '("Chez, annotated" "chez-lang, annotated" "Chez, plain" "chez-lang, plain")
              (list chez-annotated chez-lang-annotated chez-plain chez-lang-plain))])
    (printf "~a: ~a files, ~,2f MB; median of ~a samples of ~a passes\n"
      title (length files) (megabytes files) samples passes)
    (for-each (lambda (r) (printf "  ~22a ~8,2f ms/MB\n" (car r) (cdr r))) results)
    (let ([ratio (/ (cdr (assoc "chez-lang, annotated" results))
                    (cdr (assoc "Chez, annotated" results)))])
      (printf "  ratio, annotated: ~,2f (limit ~,1f)\n" ratio ratio-limit)
      ratio)))

(let* ([all (measure "all files" files)]
       [unmarked (measure "files without graph marks"
                   (filter (lambda (f) (not (has-mark? (car f)))) files))])
  (exit (if (and (<= all ratio-limit) (<= unmarked ratio-limit)) 0 1)))
