;; The states follow s/strnum.ss one for one but track only syntax:
;; exactness and the digits' values decide only whether a complete number
;; can be represented, which string->number answers.
(define (number-syntax s r6rs?)
  (define end (string-length s))

  (define (char-at i) (char-downcase (string-ref s i)))

  (define (exponent-marker? c)
    (memv c '(#\e #\s #\f #\d #\l)))

  (define (real-ms? ms) (memq ms '(real ureal)))

  (define (finish bang valid?)
    (cond
      [(and r6rs? bang) '!r6rs]
      [valid? 'complete]
      [else 'prefix]))

  (define (finish-number bang ms) (finish bang (memq ms '(real ureal angle))))
  (define (finish-rectangular bang ms) (finish bang (memq ms '(real ureal imag))))

  (define (non-decimal bang r) (or bang (not (fx= r 10))))

  (define (prefix0 i bang r)
    (cond
      [(fx= i end) 'prefix]
      [(char=? (char-at i) #\#) (prefix1 (fx+ i 1) bang r)]
      [else (num0 i bang r)]))

  (define (prefix1 i bang r)
    (if (fx= i end)
        'prefix
        (let ([c (char-at i)])
          (cond
            [(digit-value c 10) => (lambda (d) (prefix2 (fx+ i 1) #t d))]
            [(memv c '(#\e #\i)) (prefix3 (fx+ i 1) bang r)]
            [(char=? c #\b) (prefix6 (fx+ i 1) bang 2)]
            [(char=? c #\o) (prefix6 (fx+ i 1) bang 8)]
            [(char=? c #\d) (prefix6 (fx+ i 1) bang 10)]
            [(char=? c #\x) (prefix6 (fx+ i 1) bang 16)]
            [else 'bogus]))))

  (define (prefix2 i bang r)
    (if (fx= i end)
        'prefix
        (let* ([c (char-at i)] [d (digit-value c 10)])
          (cond
            [(and d (fx< r 37)) (prefix2 (fx+ i 1) bang (+ (* r 10) d))]
            [(and (char=? c #\r) (fx< 1 r 37)) (prefix6 (fx+ i 1) bang r)]
            [else 'bogus]))))

  (define (prefix3 i bang r)
    (cond
      [(fx= i end) 'prefix]
      [(char=? (char-at i) #\#) (prefix4 (fx+ i 1) bang)]
      [else (num0 i bang r)]))

  (define (prefix4 i bang)
    (if (fx= i end)
        'prefix
        (let ([c (char-at i)])
          (cond
            [(digit-value c 10) => (lambda (d) (prefix5 (fx+ i 1) #t d))]
            [(char=? c #\b) (num0 (fx+ i 1) bang 2)]
            [(char=? c #\o) (num0 (fx+ i 1) bang 8)]
            [(char=? c #\d) (num0 (fx+ i 1) bang 10)]
            [(char=? c #\x) (num0 (fx+ i 1) bang 16)]
            [else 'bogus]))))

  (define (prefix5 i bang r)
    (if (fx= i end)
        'prefix
        (let* ([c (char-at i)] [d (digit-value c 10)])
          (cond
            [(and d (fx< r 37)) (prefix5 (fx+ i 1) bang (+ (* r 10) d))]
            [(and (char=? c #\r) (fx< 1 r 37)) (num0 (fx+ i 1) bang r)]
            [else 'bogus]))))

  (define (prefix6 i bang r)
    (cond
      [(fx= i end) 'prefix]
      [(char=? (char-at i) #\#) (prefix7 (fx+ i 1) bang r)]
      [else (num0 i bang r)]))

  (define (prefix7 i bang r)
    (cond
      [(fx= i end) 'prefix]
      [(memv (char-at i) '(#\e #\i)) (num0 (fx+ i 1) bang r)]
      [else 'bogus]))

  (define (num0 i bang r)
    (if (fx= i end)
        'prefix
        (let ([c (char-at i)])
          (cond
            [(digit-value c r) (num2 (fx+ i 1) bang r 'ureal)]
            [(char=? c #\.) (float0 (fx+ i 1) (non-decimal bang r) r 'ureal)]
            [(memv c '(#\+ #\-)) (num1 (fx+ i 1) bang r 'real)]
            [else 'bogus]))))

  (define (num1 i bang r ms)
    (if (fx= i end)
        'prefix
        (let ([c (char-at i)])
          (cond
            [(digit-value c r) (num2 (fx+ i 1) bang r ms)]
            [(char=? c #\.) (float0 (fx+ i 1) (non-decimal bang r) r ms)]
            [(char=? c #\i) (num3 (fx+ i 1) bang r ms)]
            [(char=? c #\n) (infinity-or-nan-rest (fx+ i 1) bang r ms "an.0")]
            [else 'bogus]))))

  (define (num2 i bang r ms)
    (if (fx= i end)
        (finish-number bang ms)
        (let ([c (char-at i)])
          (cond
            [(digit-value c r) (num2 (fx+ i 1) bang r ms)]
            [(char=? c #\/) (rat0 (fx+ i 1) bang r ms)]
            [(char=? c #\|) (mwidth0 (fx+ i 1) bang r ms)]
            [(char=? c #\.) (float1 (fx+ i 1) (non-decimal bang r) r ms)]
            [(char=? c #\#) (numhash (fx+ i 1) #t r ms)]
            [(exponent-marker? c) (exp0 (fx+ i 1) (non-decimal bang r) r ms)]
            [else (complex0 i bang r ms)]))))

  (define (num3 i bang r ms)
    (cond
      [(fx= i end) (finish-rectangular bang ms)]
      [(char=? (char-at i) #\n) (infinity-or-nan-rest (fx+ i 1) bang r ms "f.0")]
      [else 'bogus]))

  (define (infinity-or-nan-rest i bang r ms rest)
    (let loop ([i i] [k 0])
      (cond
        [(fx= k (string-length rest))
         (if (fx= i end) (finish-number bang ms) (complex0 i bang r ms))]
        [(fx= i end) 'prefix]
        [(char=? (char-at i) (string-ref rest k)) (loop (fx+ i 1) (fx+ k 1))]
        [else 'bogus])))

  (define (numhash i bang r ms)
    (if (fx= i end)
        (finish-number bang ms)
        (let ([c (char-at i)])
          (cond
            [(char=? c #\/) (rat0 (fx+ i 1) bang r ms)]
            [(char=? c #\.) (floathash (fx+ i 1) bang r ms)]
            [(char=? c #\#) (numhash (fx+ i 1) bang r ms)]
            [(exponent-marker? c) (exp0 (fx+ i 1) bang r ms)]
            [else (complex0 i bang r ms)]))))

  (define (rat0 i bang r ms)
    (cond
      [(fx= i end) 'prefix]
      [(digit-value (char-at i) r) (rat1 (fx+ i 1) bang r ms)]
      [else 'bogus]))

  (define (rat1 i bang r ms)
    (if (fx= i end)
        (finish-number bang ms)
        (let ([c (char-at i)])
          (cond
            [(digit-value c r) (rat1 (fx+ i 1) bang r ms)]
            [(char=? c #\#) (rathash (fx+ i 1) #t r ms)]
            [(exponent-marker? c) (exp0 (fx+ i 1) #t r ms)]
            [else (complex0 i bang r ms)]))))

  (define (rathash i bang r ms)
    (if (fx= i end)
        (finish-number bang ms)
        (let ([c (char-at i)])
          (cond
            [(char=? c #\#) (rathash (fx+ i 1) bang r ms)]
            [(exponent-marker? c) (exp0 (fx+ i 1) bang r ms)]
            [else (complex0 i bang r ms)]))))

  (define (float0 i bang r ms)
    (cond
      [(fx= i end) 'prefix]
      [(digit-value (char-at i) r) (float1 (fx+ i 1) bang r ms)]
      [else 'bogus]))

  (define (float1 i bang r ms)
    (if (fx= i end)
        (finish-number bang ms)
        (let ([c (char-at i)])
          (cond
            [(digit-value c r) (float1 (fx+ i 1) bang r ms)]
            [(char=? c #\|) (mwidth0 (fx+ i 1) bang r ms)]
            [(char=? c #\#) (floathash (fx+ i 1) #t r ms)]
            [(exponent-marker? c) (exp0 (fx+ i 1) bang r ms)]
            [else (complex0 i bang r ms)]))))

  (define (floathash i bang r ms)
    (if (fx= i end)
        (finish-number bang ms)
        (let ([c (char-at i)])
          (cond
            [(char=? c #\#) (floathash (fx+ i 1) bang r ms)]
            [(exponent-marker? c) (exp0 (fx+ i 1) bang r ms)]
            [else (complex0 i bang r ms)]))))

  (define (exp0 i bang r ms)
    (if (fx= i end)
        'prefix
        (let ([c (char-at i)])
          (cond
            [(digit-value c r) (exp2 (fx+ i 1) bang r ms)]
            [(memv c '(#\+ #\-)) (exp1 (fx+ i 1) bang r ms)]
            [else 'bogus]))))

  (define (exp1 i bang r ms)
    (cond
      [(fx= i end) 'prefix]
      [(digit-value (char-at i) r) (exp2 (fx+ i 1) bang r ms)]
      [else 'bogus]))

  (define (exp2 i bang r ms)
    (if (fx= i end)
        (finish-number bang ms)
        (let ([c (char-at i)])
          (cond
            [(digit-value c r) (exp2 (fx+ i 1) bang r ms)]
            [(char=? c #\|) (mwidth0 (fx+ i 1) bang r ms)]
            [else (complex0 i bang r ms)]))))

  (define (mwidth0 i bang r ms)
    (cond
      [(fx= i end) 'prefix]
      [(digit-value (char-at i) 10) (mwidth1 (fx+ i 1) bang r ms)]
      [else 'bogus]))

  (define (mwidth1 i bang r ms)
    (if (fx= i end)
        (finish-number bang ms)
        (let ([c (char-at i)])
          (cond
            [(digit-value c 10) (mwidth1 (fx+ i 1) bang r ms)]
            [else (complex0 i bang r ms)]))))

  ;; Entered without consuming, and never at the end of the string.
  (define (complex0 i bang r ms)
    (let ([c (char-at i)])
      (cond
        [(and (char=? c #\@) (real-ms? ms)) (complex1 (fx+ i 1) bang r)]
        [(and (memv c '(#\+ #\-)) (real-ms? ms)) (num1 (fx+ i 1) bang r 'imag)]
        [(and (char=? c #\i) (memq ms '(real imag))) (complex2 (fx+ i 1) bang ms)]
        [else 'bogus])))

  (define (complex1 i bang r)
    (if (fx= i end)
        'prefix
        (let ([c (char-at i)])
          (cond
            [(digit-value c r) (num2 (fx+ i 1) bang r 'angle)]
            [(char=? c #\.) (float0 (fx+ i 1) (non-decimal bang r) r 'angle)]
            [(memv c '(#\+ #\-)) (num1 (fx+ i 1) bang r 'angle)]
            [else 'bogus]))))

  (define (complex2 i bang ms)
    (if (fx= i end)
        (finish-rectangular bang ms)
        'bogus))

  (prefix0 0 #f 10))

(define (parse-number r s)
  (let ([outcome (number-syntax s (reader-r6rs? r))])
    (if (eq? outcome 'complete)
        (or (if (reader-r6rs? r) (r6rs:string->number s) (string->number s))
            'norep)
        outcome)))
