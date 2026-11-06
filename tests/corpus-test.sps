(import (chezscheme) (chez-lang reader))

(include "differential.ss")

(define chez-src (or (getenv "CHEZ_SRC") "tests/chez"))

(define corpus-directories '("s" "mats" "examples" "unicode" "csug"))
(define corpus-extensions '("ss" "sls" "sps" "ms" "scm"))

(define (corpus-files)
  (let walk ([dir chez-src] [names corpus-directories])
    (apply append
      (map (lambda (name)
             (let ([path (string-append dir "/" name)])
               (cond
                 [(file-directory? path) (walk path (sort string<? (directory-list path)))]
                 [(member (path-extension path) corpus-extensions) (list path)]
                 [else '()])))
           names))))

;; compile-file skips a script header and counts positions after it;
;; blanking it keeps every other position the same.
(define (blank-script-header text)
  (if (and (>= (string-length text) 3)
           (string=? (substring text 0 2) "#!")
           (memv (string-ref text 2) '(#\space #\/)))
      (let ([end (let loop ([i 0])
                   (if (or (= i (string-length text)) (char=? (string-ref text i) #\newline))
                       i
                       (loop (+ i 1))))])
        (string-append (make-string end #\space) (substring text end (string-length text))))
      text))

(define errors-compared 0)

(define (file-difference path mode)
  (let-values ([(ip sfd) (open-source-port path)])
    (let* ([text (blank-script-header (get-string-all ip))]
           [chez (chez-annotated-outcome text sfd mode)]
           [ours (chez-lang-annotated-outcome text sfd mode)])
      (when (and (pair? chez) (read-failure? (car (last-pair chez))))
        (set! errors-compared (+ errors-compared 1)))
      (and (not (same-outcome? chez ours))
           (list path mode (first-difference chez ours) (length chez))))))

(define (first-difference a b)
  (let loop ([a a] [b b] [i 0])
    (cond
      [(or (null? a) (null? b) (not (same-outcome? (list (car a)) (list (car b)))))
       (list i (show-outcome (if (null? a) '() (list (car a))))
               (show-outcome (if (null? b) '() (list (car b)))))]
      [else (loop (cdr a) (cdr b) (+ i 1))])))

(define (run mode)
  (set! errors-compared 0)
  (let* ([files (corpus-files)]
         [differences (filter values (map (lambda (path) (file-difference path mode)) files))])
    (printf "~a mode: ~a files, ~a ending in a reader error\n" mode (length files) errors-compared)
    (for-each (lambda (d) (parameterize ([print-length 8] [print-level 5]) (printf "  ~s\n" d)))
              differences)
    (length differences)))

(let ([differences (+ (run 'chez) (run 'r6rs))])
  (printf "corpus-test: ~a differences\n" differences)
  (exit (if (= differences 0) 0 1)))
