#!chezscheme
(define (token r type value bfp)
  (values type value bfp (port-position (reader-port r))))

(define (delimiter? c)
  (memv c '(#\space #\( #\) #\[ #\] #\" #\; #\#)))

(define (chez-delimiter? c)
  (memv c '(#\{ #\} #\' #\` #\,)))

(define (constituent? c)
  (and (fx> (char->integer c) 127)
       (memq (char-general-category c)
             '(Lu Ll Lt Lm Lo Mn Nl No Pd Pc Po Sc Sm Sk So Co))
       #t))

(define (subsequent? c)
  (and (memq (char-general-category c) '(Nd Mc Me)) #t))

(define (intraline-whitespace? c)
  (or (char=? c #\tab) (eq? (char-general-category c) 'Zs)))

(define (hex-digit? c)
  (or (char<=? #\0 c #\9) (char<=? #\a c #\f) (char<=? #\A c #\F)))

(define (digit-value c radix)
  (let ([v (cond
             [(char<=? #\0 c #\9) (fx- (char->integer c) 48)]
             [(char<=? #\A c #\Z) (fx- (char->integer c) 55)]
             [(char<=? #\a c #\z) (fx- (char->integer c) 87)]
             [else 36])])
    (and (fx< v radix) v)))

(define (terminating-char? r c)
  (char-table-ref (readtable-terminating (reader-readtable r)) c))

(define (valid-prefix? s names)
  (exists (lambda (name)
            (and (fx<= (string-length s) (string-length name))
                 (string=? s (substring name 0 (string-length s)))))
          names))

;; Called with the handler's own start, so reader-read-list can report an
;; unterminated sequence where its handler began.
(define (call-handler r handler c start)
  (let ([depth (reader-depth r)] [handler-start (reader-handler-start r)])
    (dynamic-wind
      (lambda ()
        (reader-depth-set! r (fx+ depth 1))
        (reader-handler-start-set! r start))
      (lambda () (handler r c start))
      (lambda ()
        (reader-depth-set! r depth)
        (reader-handler-start-set! r handler-start)))))

(define (handler-token r close handler c bfp)
  (let ([value (call-handler r handler c bfp)])
    (if (eq? value reader-comment)
        (scan-token r close)
        (token r 'datum value bfp))))

(define (scan-token r close)
  (let* ([ip (reader-port r)] [bfp (port-position ip)] [c (read-char ip)])
    (cond
      [(eof-object? c) (values 'eof c bfp bfp)]
      [(and close (char=? c close)) (token r 'close #f bfp)]
      [(char-table-ref (readtable-starts (reader-readtable r)) c)
       => (lambda (handler) (handler-token r close handler c bfp))]
      [(memv c '(#\space #\newline #\tab)) (scan-token r close)]
      [(or (char<=? #\a c #\z) (char<=? #\A c #\Z))
       (buffer-put! r 0 c)
       (scan-symbol r (read-char ip) 1 #f bfp finish-symbol)]
      [(char=? c #\() (token r 'lparen #f bfp)]
      [(char=? c #\)) (token r 'rparen #f bfp)]
      [(char=? c #\[) (token r 'lbrack #f bfp)]
      [(char=? c #\]) (token r 'rbrack #f bfp)]
      [(char=? c #\') (token r 'quote 'quote bfp)]
      [(char<=? #\0 c #\9)
       (buffer-put! r 0 c)
       (scan-number-or-symbol r 1 bfp)]
      [(memv c '(#\- #\+)) (scan-sign r c bfp)]
      [(char=? c #\;) (skip-line-comment r close)]
      [(char=? c #\#) (scan-hash r close bfp)]
      [(char=? c #\") (scan-string r 0 bfp)]
      [(char=? c #\.) (scan-dot r bfp)]
      [(char=? c #\,)
       (if (eqv? (peek-char ip) #\@)
           (begin (read-char ip) (token r 'quote 'unquote-splicing bfp))
           (token r 'quote 'unquote bfp))]
      [(char=? c #\`) (token r 'quote 'quasiquote bfp)]
      [(or (memv c '(#\* #\= #\< #\> #\/ #\! #\$ #\% #\& #\: #\? #\^ #\_ #\~))
           (constituent? c))
       (buffer-put! r 0 c)
       (scan-symbol r (read-char ip) 1 #f bfp finish-symbol)]
      [(memv c '(#\| #\\)) (scan-symbol r c 0 #f bfp finish-symbol)]
      [(char=? c #\{)
       (check-standard r bfp (port-position ip) "open brace")
       (token r 'atomic '{ bfp)]
      [(char=? c #\})
       (check-standard r bfp (port-position ip) "close brace")
       (token r 'atomic '} bfp)]
      [(char-whitespace? c) (scan-token r close)]
      [else (scan-symbol r c 0 #f bfp finish-nonstandard-symbol)])))

(define (skip-line-comment r close)
  (let ([ip (reader-port r)])
    (let loop ()
      (let ([c (read-char ip)])
        (if (or (eof-object? c) (memv c '(#\newline #\return #\nel #\ls)))
            (scan-token r close)
            (loop))))))

(define (finish-symbol r n slashed? bfp)
  (token r 'atomic (intern r n slashed?) bfp))

(define (finish-nonstandard-symbol r n slashed? bfp)
  (when (reader-r6rs? r)
    (nonstandard-error r bfp (port-position (reader-port r))
      (format "~a symbol" (buffer-string r n))))
  (finish-symbol r n slashed? bfp))

(define (scan-symbol r c n slashed? bfp finish)
  (define ip (reader-port r))
  (define terminating (readtable-terminating (reader-readtable r)))
  (define (stop c n slashed?)
    (unless (eof-object? c) (unread-char c ip))
    (finish r n slashed? bfp))
  (define (take c n slashed?)
    (buffer-put! r n c)
    (next (read-char ip) (fx+ n 1) slashed?))
  (define (next c n slashed?)
    (cond
      [(eof-object? c) (finish r n slashed? bfp)]
      [(char-table-ref terminating c) (stop c n slashed?)]
      [(char<=? #\a c #\z) (take c n slashed?)]
      [(memv c '(#\space #\newline #\) #\])) (stop c n slashed?)]
      [(or (char<=? #\0 c #\9)
           (memv c '(#\- #\? #\* #\! #\= #\> #\< #\$ #\% #\& #\/ #\: #\^ #\_ #\~ #\+ #\. #\@))
           (char<=? #\A c #\Z))
       (take c n slashed?)]
      [(char=? c #\\) (escape n slashed?)]
      [(char=? c #\|)
       (check-standard r bfp (port-position ip) "|...| symbol escape")
       (bar n)]
      [(memv c '(#\( #\[ #\" #\; #\#)) (stop c n slashed?)]
      [(char-whitespace? c) (stop c n slashed?)]
      [(or (constituent? c) (subsequent? c)) (take c n slashed?)]
      [(chez-delimiter? c)
       (check-standard-delimiter r bfp (port-position ip) c)
       (stop c n slashed?)]
      [else
       (when (reader-r6rs? r)
         (nonstandard-error r bfp (port-position ip) (format "character ~c in symbol" c)))
       (take c n slashed?)]))
  (define (escape n slashed?)
    (let ([c (read-char ip)])
      (cond
        [(eof-object? c) (eof-error r bfp (port-position ip) "symbol")]
        [(char=? c #\x) (hex-escape n 0 (fx- (port-position ip) 2) slashed?)]
        [else
         (check-standard r bfp (port-position ip) "non-hex back-slash symbol escape")
         (take c n #t)])))
  (define (hex-escape n value escape-bfp slashed?)
    (let ([c (read-char ip)])
      (cond
        [(eof-object? c) (eof-error r bfp (port-position ip) "symbol")]
        [(hex-digit? c) (hex-escape n (+ (* value 16) (digit-value c 16)) escape-bfp slashed?)]
        [(char=? c #\;)
         (unless (scalar-value? value)
           (lexical-error r escape-bfp (port-position ip)
             "invalid code point value ~s in symbol hex escape" value))
         (take (integer->char value) n slashed?)]
        [else
         (unread-char c ip)
         (lexical-error r escape-bfp (port-position ip)
           "invalid character ~c in symbol hex escape" c)])))
  (define (bar n)
    (let ([c (read-char ip)])
      (cond
        [(eof-object? c) (eof-error r bfp (port-position ip) "symbol")]
        [(char=? c #\|) (next (read-char ip) n #t)]
        [else (buffer-put! r n c) (bar (fx+ n 1))])))
  (next c n slashed?))

(define (scalar-value? n)
  (and (fixnum? n) (or (fx<= 0 n #xD7FF) (fx<= #xE000 n #x10FFFF))))

(define (scan-number-or-symbol r n bfp)
  (let ([ip (reader-port r)] [terminating (readtable-terminating (reader-readtable r))])
    (let loop ([n n])
      (let ([c (read-char ip)])
        (cond
          [(eof-object? c) (number-or-symbol r n bfp)]
          [(char-table-ref terminating c) (unread-char c ip) (number-or-symbol r n bfp)]
          [(or (char<=? #\0 c #\9) (char<=? #\a c #\z)
               (memv c '(#\- #\+ #\. #\/ #\@ #\|))
               (char<=? #\A c #\Z))
           (buffer-put! r n c)
           (loop (fx+ n 1))]
          [(char=? c #\#)
           (if (reader-r6rs? r)
               (begin (unread-char c ip) (number-or-symbol r n bfp))
               (begin (buffer-put! r n c) (loop (fx+ n 1))))]
          [(or (delimiter? c) (char-whitespace? c))
           (unread-char c ip)
           (number-or-symbol r n bfp)]
          [(chez-delimiter? c)
           (check-standard-delimiter r bfp (port-position ip) c)
           (unread-char c ip)
           (number-or-symbol r n bfp)]
          [else (scan-symbol r c n #f bfp finish-nonstandard-symbol)])))))

(define (number-or-symbol r n bfp)
  (let* ([s (buffer-string r n)] [z (parse-number r s)] [efp (port-position (reader-port r))])
    (cond
      [(number? z) (values 'atomic z bfp efp)]
      [(eq? z 'norep) (unrepresentable-error r bfp efp s)]
      [(eq? z '!r6rs) (nonstandard-error r bfp efp (format "~a number" s))]
      [else
       (when (reader-r6rs? r) (nonstandard-error r bfp efp (format "~a symbol" s)))
       (values 'atomic (intern r n #f) bfp efp)])))

(define (scan-number r n bfp)
  (let ([ip (reader-port r)] [terminating (readtable-terminating (reader-readtable r))])
    (let loop ([n n])
      (let ([c (read-char ip)])
        (cond
          [(eof-object? c) (number-token r n bfp)]
          [(char-table-ref terminating c) (unread-char c ip) (number-token r n bfp)]
          [(or (char<=? #\0 c #\9) (char<=? #\a c #\z) (char<=? #\A c #\Z)
               (memv c '(#\- #\+ #\. #\/ #\@ #\# #\|)))
           (buffer-put! r n c)
           (loop (fx+ n 1))]
          [(or (delimiter? c) (char-whitespace? c))
           (unread-char c ip)
           (number-token r n bfp)]
          [(chez-delimiter? c)
           (check-standard-delimiter r bfp (port-position ip) c)
           (unread-char c ip)
           (number-token r n bfp)]
          [else (buffer-put! r n c) (loop (fx+ n 1))])))))

(define (number-token r n bfp)
  (let* ([ip (reader-port r)] [s (buffer-string r n)] [z (parse-number r s)] [efp (port-position ip)])
    (cond
      [(number? z) (values 'atomic z bfp efp)]
      [(and (eq? z 'prefix) (eof-object? (peek-char ip))) (eof-error r bfp efp "number")]
      [(eq? z '!r6rs) (nonstandard-error r bfp efp (format "~a number" s))]
      [(eq? z 'norep) (unrepresentable-error r bfp efp s)]
      [else (lexical-error r bfp efp "invalid number syntax ~a" s)])))

(define (ends-bare-token? r c)
  (or (eof-object? c) (delimiter? c) (char-whitespace? c) (terminating-char? r c)))

(define (scan-sign r sign bfp)
  (let* ([ip (reader-port r)] [c (peek-char ip)] [symbol (if (char=? sign #\-) '- '+)])
    (cond
      [(ends-bare-token? r c) (token r 'atomic symbol bfp)]
      [(and (char=? sign #\-) (char=? c #\>))
       (buffer-put! r 0 sign)
       (scan-symbol r (read-char ip) 1 #f bfp finish-symbol)]
      [(chez-delimiter? c)
       (check-standard-delimiter r bfp (port-position ip) c)
       (token r 'atomic symbol bfp)]
      [else
       (buffer-put! r 0 sign)
       (scan-number-or-symbol r 1 bfp)])))

(define (scan-dot r bfp)
  (let* ([ip (reader-port r)] [c (peek-char ip)])
    (cond
      [(eqv? c #\.) (read-char ip) (scan-dot-dot r bfp)]
      [(ends-bare-token? r c) (token r 'dot #f bfp)]
      [(chez-delimiter? c)
       (check-standard-delimiter r bfp (port-position ip) c)
       (token r 'dot #f bfp)]
      [else
       (buffer-put! r 0 #\.)
       (scan-number-or-symbol r 1 bfp)])))

(define (scan-dot-dot r bfp)
  (let* ([ip (reader-port r)] [c (read-char ip)])
    (cond
      ;; Chez counts the end of file it read here and did not put back.
      [(eof-object? c)
       (let ([efp (fx+ (port-position ip) 1)])
         (check-standard r bfp efp ".. symbol")
         (values 'atomic '.. bfp efp))]
      [(char=? c #\.) (scan-dot-dot-dot r bfp)]
      [else
       (buffer-put! r 0 #\.)
       (buffer-put! r 1 #\.)
       (scan-symbol r c 2 #f bfp finish-nonstandard-symbol)])))

(define (scan-dot-dot-dot r bfp)
  (let* ([ip (reader-port r)] [c (read-char ip)])
    (cond
      [(ends-bare-token? r c)
       (unless (eof-object? c) (unread-char c ip))
       (token r 'atomic '... bfp)]
      [(chez-delimiter? c)
       (check-standard-delimiter r bfp (port-position ip) c)
       (unread-char c ip)
       (token r 'atomic '... bfp)]
      [else
       (buffer-put! r 0 #\.)
       (buffer-put! r 1 #\.)
       (buffer-put! r 2 #\.)
       (scan-symbol r c 3 #f bfp finish-nonstandard-symbol)])))

(define (scan-string r n bfp)
  (let ([ip (reader-port r)])
    (let loop ([n n])
      (let ([c (read-char ip)])
        (cond
          [(eof-object? c) (eof-error r bfp (port-position ip) "string")]
          [(char=? c #\") (token r 'atomic (buffer-string r n) bfp)]
          [(char=? c #\\) (string-escape r n bfp (fx- (port-position ip) 1))]
          [(memv c '(#\newline #\nel #\ls))
           (buffer-put! r n #\newline)
           (loop (fx+ n 1))]
          [(char=? c #\return)
           (let ([next (peek-char ip)])
             (cond
               [(eof-object? next) (eof-error r bfp (port-position ip) "string")]
               [(memv next '(#\newline #\nel))
                (read-char ip)
                (buffer-put! r n #\newline)
                (loop (fx+ n 1))]
               [else
                (buffer-put! r n #\newline)
                (loop (fx+ n 1))]))]
          [else (buffer-put! r n c) (loop (fx+ n 1))])))))

(define (string-escape r n bfp escape-bfp)
  (define ip (reader-port r))
  (define (put c)
    (buffer-put! r n c)
    (scan-string r (fx+ n 1) bfp))
  (define (invalid message . chars)
    (apply lexical-error r escape-bfp (port-position ip) message chars))
  (let ([c (read-char ip)])
    (cond
      [(eof-object? c) (eof-error r bfp (port-position ip) "string")]
      [(memv c '(#\\ #\")) (put c)]
      [(assv c '((#\n . #\newline) (#\a . #\alarm) (#\b . #\backspace) (#\f . #\page)
                 (#\r . #\return) (#\t . #\tab) (#\v . #\vtab)))
       => (lambda (a) (put (cdr a)))]
      [(char=? c #\x) (string-hex-escape r n 0 bfp escape-bfp)]
      [(or (memv c '(#\newline #\return #\nel #\ls)) (intraline-whitespace? c))
       (string-continuation r n c bfp)]
      [(char<=? #\0 c #\7)
       (check-standard r bfp (port-position ip) "octal string-character")
       (let ([c1 (read-char ip)])
         (cond
           [(eof-object? c1) (eof-error r bfp (port-position ip) "string")]
           [(char<=? #\0 c1 #\7)
            (let ([c2 (read-char ip)])
              (cond
                [(eof-object? c2) (eof-error r bfp (port-position ip) "string")]
                [(char<=? #\0 c2 #\7)
                 (let ([v (fx+ (fx* (digit-value c 8) 64) (fx* (digit-value c1 8) 8) (digit-value c2 8))])
                   (when (fx> v 255) (invalid "invalid string character \\~c~c~c" c c1 c2))
                   (put (integer->char v)))]
                [else (unread-char c2 ip) (invalid "invalid string character \\~c~c" c c1)]))]
           [else (unread-char c1 ip) (invalid "invalid string character \\~c" c)]))]
      [(char=? c #\')
       (check-standard r bfp (port-position ip) "\\' string character")
       (put c)]
      [else (invalid "invalid string character \\~c" c)])))

(define (string-hex-escape r n value bfp escape-bfp)
  (let* ([ip (reader-port r)] [c (read-char ip)])
    (cond
      [(eof-object? c) (eof-error r bfp (port-position ip) "string")]
      [(hex-digit? c) (string-hex-escape r n (+ (* value 16) (digit-value c 16)) bfp escape-bfp)]
      [(char=? c #\;)
       (unless (scalar-value? value)
         (lexical-error r escape-bfp (port-position ip)
           "invalid code point value ~s in string hex escape" value))
       (buffer-put! r n (integer->char value))
       (scan-string r (fx+ n 1) bfp)]
      [else
       (unread-char c ip)
       (lexical-error r escape-bfp (port-position ip)
         "invalid character ~c in string hex escape" c)])))

(define (string-continuation r n c bfp)
  (let ([ip (reader-port r)])
    (define (skip-indentation)
      (let ([c (peek-char ip)])
        (cond
          [(eof-object? c) (eof-error r bfp (port-position ip) "string")]
          [(intraline-whitespace? c) (read-char ip) (skip-indentation)]
          [else (scan-string r n bfp)])))
    (let loop ([c c])
      (cond
        [(eof-object? c) (eof-error r bfp (port-position ip) "string")]
        [(memv c '(#\newline #\nel #\ls)) (skip-indentation)]
        [(char=? c #\return)
         (let ([next (peek-char ip)])
           (cond
             [(eof-object? next) (eof-error r bfp (port-position ip) "string")]
             [(memv next '(#\newline #\nel)) (read-char ip) (skip-indentation)]
             [else (skip-indentation)]))]
        [(intraline-whitespace? c) (loop (read-char ip))]
        [else
         (lexical-error r bfp (port-position ip)
           "unexpected character ~c after \\<intraline whitespace> in string" c)]))))

(define (scan-hash r close bfp)
  (let* ([ip (reader-port r)] [next (peek-char ip)])
    (cond
      [(and (char? next) (char-table-ref (readtable-dispatches (reader-readtable r)) next))
       => (lambda (handler) (read-char ip) (handler-token r close handler next bfp))]
      [else (scan-hash-builtin r close bfp (read-char ip))])))

(define (scan-hash-builtin r close bfp c)
  (define ip (reader-port r))
  (define (standard what) (check-standard r bfp (port-position ip) what))
  (cond
    [(eof-object? c) (eof-error r bfp (port-position ip) "# prefix")]
    [(memv c '(#\f #\F)) (scan-boolean r #f bfp)]
    [(memv c '(#\t #\T)) (scan-boolean r #t bfp)]
    [(char=? c #\\) (scan-char r bfp)]
    [(char=? c #\() (token r 'vparen #f bfp)]
    [(char=? c #\') (token r 'quote 'syntax bfp)]
    [(char=? c #\`) (token r 'quote 'quasisyntax bfp)]
    [(char=? c #\,)
     (if (eqv? (peek-char ip) #\@)
         (begin (read-char ip) (token r 'quote 'unsyntax-splicing bfp))
         (token r 'quote 'unsyntax bfp))]
    [(char<=? #\0 c #\9)
     (buffer-put! r 0 #\#)
     (buffer-put! r 1 c)
     (scan-hash-number r (digit-value c 10) 2 bfp)]
    [(char=? c #\@) (token r 'fasl #f bfp)]
    [(char=? c #\[) (standard "#[...] record") (token r 'record-brack #f bfp)]
    [(char=? c #\{) (standard "#{...} gensym") (scan-gensym r bfp)]
    [(char=? c #\&) (standard "#& box") (token r 'box #f bfp)]
    [(char=? c #\;) (skip-datum-comment r bfp) (scan-token r close)]
    [(char=? c #\!) (scan-hash-bang r close bfp)]
    [(memv c '(#\x #\X #\o #\O #\b #\B #\d #\D #\i #\I #\e #\E))
     (buffer-put! r 0 #\#)
     (buffer-put! r 1 c)
     (scan-number r 2 bfp)]
    [(char=? c #\v) (scan-hash-v r bfp)]
    [(char=? c #\%)
     (standard "#% primitive")
     (scan-symbol r (read-char ip) 0 #f bfp finish-primitive)]
    [(char=? c #\:)
     (standard "#: gensym")
     (scan-symbol r (read-char ip) 0 #f bfp finish-uninterned-gensym)]
    [(char=? c #\|) (skip-block-comment r bfp) (scan-token r close)]
    [else (lexical-error r bfp (port-position ip) "invalid sharp-sign prefix #~c" c)]))

(define (finish-primitive r n slashed? bfp)
  (token r 'atomic (list '$primitive (intern r n slashed?)) bfp))

(define (finish-uninterned-gensym r n slashed? bfp)
  (let ([s (buffer-string r n)])
    (token r 'atomic (gensym (if (and (reader-fold? r) (not slashed?)) (string-foldcase s) s)) bfp)))

(define (skip-block-comment r bfp)
  (let ([ip (reader-port r)])
    (let loop ([depth 0])
      (let ([c (read-char ip)])
        (cond
          [(eof-object? c) (eof-error r bfp (port-position ip) "block comment")]
          [(char=? c #\|)
           (let ([next (peek-char ip)])
             (cond
               [(eof-object? next) (eof-error r bfp (port-position ip) "block comment")]
               [(char=? next #\#)
                (read-char ip)
                (unless (fx= depth 0) (loop (fx- depth 1)))]
               [else (loop depth)]))]
          [(char=? c #\#)
           (let ([next (peek-char ip)])
             (cond
               [(eof-object? next) (eof-error r bfp (port-position ip) "block comment")]
               [(char=? next #\|) (read-char ip) (loop (fx+ depth 1))]
               [else (loop depth)]))]
          [else (loop depth)])))))

(define (skip-datum-comment r expr-bfp)
  (let-values ([(type value bfp efp) (scan-token r #f)])
    (if (eq? type 'eof)
        (eof-error r expr-bfp efp "s-expression comment")
        (read-datum r type value bfp efp))))

(define (scan-boolean r value bfp)
  (let ([c (peek-char (reader-port r))])
    (cond
      [(eof-object? c) (token r 'atomic value bfp)]
      [(terminating-char? r c) (token r 'atomic value bfp)]
      [(char-alphabetic? c) (scan-long-boolean r value (if value "true" "false") 1 bfp)]
      [else (check-delimiter r value "boolean" bfp)])))

(define (scan-long-boolean r value name i bfp)
  (let* ([ip (reader-port r)] [c (read-char ip)])
    (cond
      [(eof-object? c) (eof-error r bfp (port-position ip) "boolean")]
      [(not (char-ci=? c (string-ref name i)))
       (unread-char c ip)
       (lexical-error r bfp (port-position ip) "invalid boolean #~a~c"
         (substring name 0 i) (char-downcase c))]
      [(fx= i (fx- (string-length name) 1))
       (check-standard r bfp (port-position ip) "alternative boolean")
       (check-delimiter r value "boolean" bfp)]
      [else (scan-long-boolean r value name (fx+ i 1) bfp)])))

(define (check-delimiter r value what bfp)
  (let* ([ip (reader-port r)] [c (peek-char ip)])
    (cond
      [(ends-bare-token? r c) (token r 'atomic value bfp)]
      [(chez-delimiter? c)
       (check-standard-delimiter r bfp (port-position ip) c)
       (token r 'atomic value bfp)]
      [else (lexical-error r bfp (port-position ip) "invalid delimiter ~a for ~a" c what)])))

(define (scan-to-delimiter r n c next)
  (let ([ip (reader-port r)])
    (let loop ([n n] [c c])
      (if (or (ends-bare-token? r c) (chez-delimiter? c))
          (next n c)
          (begin
            (buffer-put! r n c)
            (loop (fx+ n 1) (read-char ip)))))))

(define r6rs-char-names
  '((nul . #\nul) (alarm . #\alarm) (backspace . #\backspace) (tab . #\tab)
    (linefeed . #\newline) (newline . #\newline) (vtab . #\vtab) (page . #\page)
    (return . #\return) (esc . #\esc) (space . #\space) (delete . #\delete)))

;; Chez's reader checks a prefix against every registered character name,
;; which no documented procedure lists; these are the names Chez registers.
(define chez-char-names
  '("space" "tab" "return" "page" "linefeed" "newline" "backspace" "rubout"
    "nul" "bel" "vt" "esc" "vtab" "delete" "alarm" "nel" "ls"))

(define (scan-char r bfp)
  (let* ([ip (reader-port r)] [c (read-char ip)])
    (cond
      [(eof-object? c) (eof-error r bfp (port-position ip) "character")]
      [(char=? c #\x)
       (let ([next (peek-char ip)])
         (cond
           [(eof-object? next) (token r 'atomic c bfp)]
           [(hex-digit? next)
            (buffer-put! r 0 #\x)
            (scan-hex-char r 0 1 bfp)]
           [else (check-delimiter r c "character" bfp)]))]
      [(or (char<=? #\a c #\z) (char<=? #\A c #\Z))
       (let ([next (peek-char ip)])
         (cond
           [(eof-object? next) (token r 'atomic c bfp)]
           [(or (char<=? #\a next #\z) (char<=? #\A next #\Z))
            (scan-to-delimiter r 0 c
              (lambda (n c) (char-name-token r n c bfp)))]
           [else (check-delimiter r c "character" bfp)]))]
      [(char<=? #\0 c #\7)
       (let ([next (peek-char ip)])
         (cond
           [(eof-object? next) (token r 'atomic c bfp)]
           [(char<=? #\0 next #\7)
            (check-standard r bfp (port-position ip) "octal character")
            (let* ([c1 (read-char ip)] [c2 (read-char ip)])
              (cond
                [(eof-object? c2) (eof-error r bfp (port-position ip) "character")]
                [(char<=? #\0 c2 #\7)
                 (let ([v (fx+ (fx* (digit-value c 8) 64) (fx* (digit-value c1 8) 8) (digit-value c2 8))])
                   (when (fx> v 255)
                     (lexical-error r bfp (port-position ip) "invalid character #\\~a~a~a" c c1 c2))
                   (check-delimiter r (integer->char v) "character" bfp))]
                [else (lexical-error r bfp (port-position ip) "invalid character #\\~a~a" c c1)]))]
           [else (check-delimiter r c "character" bfp)]))]
      [else (check-delimiter r c "character" bfp)])))

(define (scan-hex-char r value n bfp)
  (let* ([ip (reader-port r)] [c (read-char ip)])
    (define (hex-char)
      (unless (scalar-value? value)
        (lexical-error r bfp (port-position ip) "invalid hex character escape ~a" (buffer-string r n)))
      (token r 'atomic (integer->char value) bfp))
    (cond
      [(eof-object? c) (hex-char)]
      [(hex-digit? c)
       (buffer-put! r n c)
       (scan-hex-char r (+ (* value 16) (digit-value c 16)) (fx+ n 1) bfp)]
      [(ends-bare-token? r c) (unread-char c ip) (hex-char)]
      [(chez-delimiter? c)
       (check-standard-delimiter r bfp (port-position ip) c)
       (unread-char c ip)
       (hex-char)]
      [else (scan-to-delimiter r n c (lambda (n c) (char-name-token r n c bfp)))])))

(define (char-name-token r n c bfp)
  (let ([ip (reader-port r)])
    (unless (eof-object? c) (unread-char c ip))
    (let* ([name (intern r n #f)]
           [char (if (reader-r6rs? r)
                     (cond [(assq name r6rs-char-names) => cdr] [else #f])
                     (char-name name))])
      (if char
          (token r 'atomic char bfp)
          (let ([s (buffer-string r n)])
            (if (and (eof-object? (peek-char ip))
                     (valid-prefix? s (if (reader-r6rs? r)
                                          (map (lambda (a) (symbol->string (car a))) r6rs-char-names)
                                          chez-char-names)))
                (eof-error r bfp (port-position ip) "character")
                (lexical-error r bfp (port-position ip) "invalid character name #\\~a" s)))))))

(define directives
  '(("r6rs" . r6rs) ("fold-case" . fold-case) ("no-fold-case" . no-fold-case)
    ("chezscheme" . chezscheme)))

;; A directive need not be delimited: #!r6rsfoo is #!r6rs followed by foo.
(define (scan-hash-bang r close bfp)
  (let ([ip (reader-port r)])
    (let loop ([i 0] [candidates directives])
      (cond
        [(find (lambda (d) (fx= (string-length (car d)) i)) candidates)
         => (lambda (d)
              (case (cdr d)
                [(r6rs) (reader-r6rs?-set! r #t)]
                [(chezscheme) (reader-r6rs?-set! r #f)]
                [(fold-case) (reader-fold?-set! r #t)]
                [(no-fold-case) (reader-fold?-set! r #f)])
              (scan-token r close))]
        [else
         (let ([c (read-char ip)])
           (if (eof-object? c)
               (eof-error r bfp (port-position ip) "#! syntax")
               (let ([matching (filter (lambda (d) (char=? (string-ref (car d) i) c)) candidates)])
                 (if (null? matching)
                     (scan-to-delimiter r i c (lambda (n c) (hash-bang-word r n c bfp)))
                     (begin
                       (buffer-put! r i c)
                       (loop (fx+ i 1) matching))))))]))))

(define (hash-bang-word r n c bfp)
  (let ([ip (reader-port r)] [s (buffer-string r n)])
    (unless (eof-object? c) (unread-char c ip))
    (let ([efp (port-position ip)])
      (define (special what value)
        (check-standard r bfp efp what)
        (values 'atomic value bfp efp))
      (cond
        [(string=? s "eof") (special "#!eof" (eof-object))]
        [(string=? s "bwp") (special "#!bwp" bwp-object)]
        [(string=? s "base-rtd") (special "#!base-rtd" base-rtd)]
        [(and (eof-object? c) (valid-prefix? s '("eof" "bwp" "base-rtd")))
         (eof-error r bfp efp "#! syntax")]
        [else (lexical-error r bfp efp "invalid syntax #!~a" s)]))))

(define (scan-hash-v r bfp)
  (let ([ip (reader-port r)])
    (scan-to-delimiter r 0 (read-char ip)
      (lambda (n c)
        (let ([s (buffer-string r n)] [efp (port-position ip)])
          (define (standard what) (check-standard r bfp efp what))
          (cond
            [(eof-object? c)
             (if (valid-prefix? s '("fx" "fl" "u8" "s"))
                 (eof-error r bfp efp "#v prefix")
                 (lexical-error r bfp efp "invalid syntax #v~a" s))]
            [(char=? c #\()
             (cond
               [(string=? s "fx") (standard "#vfx(...) fxvector") (values 'vfxparen #f bfp efp)]
               [(string=? s "fl") (standard "#vfl(...) flvector") (values 'vflparen #f bfp efp)]
               [(string=? s "u8") (values 'vu8paren #f bfp efp)]
               [(string=? s "s") (standard "#vs(...) stencil vector") (values 'vsparen #f bfp efp)]
               [else (lexical-error r bfp efp "invalid syntax #v~a(" s)])]
            [(valid-prefix? s '("fx" "fl" "u8" "s"))
             (lexical-error r bfp efp "expected left paren after #v~a prefix" s)]
            [else (lexical-error r bfp efp "invalid syntax #v~a~a" s c)]))))))

(define (scan-hash-number-v r digits size bfp)
  (let ([ip (reader-port r)])
    (scan-to-delimiter r 0 (read-char ip)
      (lambda (n c)
        (let ([s (buffer-string r n)] [efp (port-position ip)])
          (define (standard what) (check-standard r bfp efp what))
          (cond
            [(eof-object? c)
             (if (valid-prefix? s '("fx" "u8"))
                 (eof-error r bfp efp "#v prefix")
                 (lexical-error r bfp efp "invalid syntax #~v,'0dv~a" digits size s))]
            [(char=? c #\()
             (cond
               [(string=? s "fx") (standard "#<n>vfx(...) fxvector") (values 'vfxnparen size bfp efp)]
               [(string=? s "fl") (standard "#<n>vfl(...) flvector") (values 'vflnparen size bfp efp)]
               [(string=? s "u8") (standard "#<n>vu8(...) bytevector") (values 'vu8nparen size bfp efp)]
               [(string=? s "s") (standard "#<n>vs(...) stencil vector") (values 'vsnparen size bfp efp)]
               [else (lexical-error r bfp efp "invalid syntax #~v,'0dv~a(" digits size s)])]
            [(valid-prefix? s '("fx" "u8"))
             (lexical-error r bfp efp "expected left paren after #~v,'0dv~a prefix" digits size s)]
            [else (lexical-error r bfp efp "invalid syntax #~v,'0dv~a~a" digits size s c)]))))))

(define (scan-hash-number r n i bfp)
  (let* ([ip (reader-port r)] [c (read-char ip)])
    (define (standard what) (check-standard r bfp (port-position ip) what))
    (define (invalid-prefix)
      (lexical-error r bfp (port-position ip) "invalid sharp-sign prefix ~a~a" (buffer-string r i) c))
    (cond
      [(eof-object? c) (eof-error r bfp (port-position ip) "# prefix")]
      [(char<=? #\0 c #\9)
       (buffer-put! r i c)
       (scan-hash-number r (+ (* n 10) (digit-value c 10)) (fx+ i 1) bfp)]
      [(char=? c #\() (standard "#<n>(...) vector") (token r 'vnparen n bfp)]
      [(memv c '(#\r #\R))
       (standard "#<n>r number prefix")
       (buffer-put! r i c)
       (scan-number r (fx+ i 1) bfp)]
      [(memv c '(#\q #\Q)) (lexical-error r bfp (port-position ip) "outdated object file format")]
      [(char=? c #\#) (standard "#<n># insert") (start-marks! r) (token r 'insert n bfp)]
      [(char=? c #\=) (standard "#<n>= mark") (start-marks! r) (token r 'mark n bfp)]
      [(char=? c #\v) (scan-hash-number-v r (fx- i 1) n bfp)]
      [(char=? c #\%)
       (unless (memv n '(2 3)) (invalid-prefix))
       (standard "#<n>% primitive")
       (scan-symbol r (read-char ip) 0 #f bfp
         (lambda (r m slashed? bfp)
           (token r 'atomic (list '$primitive n (intern r m slashed?)) bfp)))]
      [else (invalid-prefix)])))

(define (scan-gensym r bfp)
  (let ([ip (reader-port r)])
    (define (unique-part n pretty-slashed?)
      (let ([c (read-char ip)])
        (cond
          [(eof-object? c) (eof-error r bfp (port-position ip) "gensym")]
          [(memv c '(#\space #\newline #\tab)) (unique-part n pretty-slashed?)]
          [else
           (scan-symbol r c n #f bfp
             (lambda (r m unique-slashed? bfp)
               (let ([c (read-char ip)])
                 (cond
                   ;; Chez counts the end of file it read here and did not put back.
                   [(eof-object? c) (eof-error r bfp (fx+ (port-position ip) 1) "gensym")]
                   [(char=? c #\})
                    (token r 'atomic (interned-gensym r n m pretty-slashed? unique-slashed?) bfp)]
                   [else
                    (unread-char c ip)
                    (lexical-error-at-end r bfp (port-position ip)
                      "expected close brace terminating gensym syntax")]))))])))
    (let ([c (read-char ip)])
      (if (eof-object? c)
          (eof-error r bfp (port-position ip) "gensym")
          (scan-symbol r c 0 #f bfp
            (lambda (r n slashed? bfp) (unique-part n slashed?)))))))

(define (interned-gensym r n m pretty-slashed? unique-slashed?)
  (define (part start end slashed?)
    (let ([s (substring (reader-buffer r) start end)])
      (if (and (reader-fold? r) (not slashed?)) (string-foldcase s) s)))
  (gensym (part 0 n pretty-slashed?) (part n m unique-slashed?)))
