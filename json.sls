(library (qk json)
  (export string->json json->string json-null? json-null)
  (import (rnrs))

  (define json-null 'null)

  (define (json-null? x)
    (eq? x 'null))

  (define (string->json str)
    (let ([in (open-string-input-port str)])
      (guard (con
              ([(and (error? con)
                     (eq? 'string->json (condition-who con)))
                (apply error
                       'string->json
                       (condition-message con)
                       (append (condition-irritants con)
                               (list str)))]))
        (parse-json in))))

  (define (char-degit? c)
    (case c
      [(#\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9) #t]
      [else #f]))

  (define (numeric-char->number c)
    (- (char->integer c) 48))

  (define (whitespace? c)
    (case c
      [(#\space #\tab #\newline #\return) #t]
      [else #f]))

  (define (skip-whitespace in)
    (cond
     [(whitespace? (peek-char in))
      (get-char in)
      (skip-whitespace in)]
     [else 'done]))

  (define (fail type c)
    (error 'string->json
           (string-append (symbol->string type) " error")
           c))

  (define (parse-json in)
    (skip-whitespace in)
    (let ([c (peek-char in)])
      (cond
       [(eof-object? c) (fail 'parse-json c)]
       [(char=? c #\{)
        (parse-object in)]
       [(char=? c #\[)
        (parse-array in)]
       [(char-degit? c)
        (parse-number in)]
       [(char=? c #\")
        (parse-string in)]
       [(char-alphabetic? c)
        (case (parse-symbol in)
          [(true) #t]
          [(false) #f]
          [(null) json-null]
          [else (fail 'parse-json c)])]
       [else (fail 'parse-json c)])))

  (define (parse-object in)
    (get-char in)                       ; drop #\{
    (skip-whitespace in)
    (let ([c (peek-char in)])
      (cond
       [(eof-object? c) (fail 'parse-object c)]
       [(char=? c #\})
        (get-char in)
        '()]
       [else
        (%parse-object in)])))

  (define (%parse-object in)
    (skip-whitespace in)
    (let ([key (parse-string in)])
      (skip-whitespace in)
      (let ([value
             (let ([c (get-char in)])
               (cond
                [(eof-object? c) (fail 'parse-object c)]
                [(char=? c #\:)
                 (parse-json in)]
                [else (fail 'parse-object c)]))])
        (skip-whitespace in)
        (cons (cons key value)
              (let ([c (get-char in)])
                (cond
                 [(eof-object? c) (fail 'parse-object c)]
                 [(char=? c #\,)
                  (%parse-object in)]
                 [(char=? c #\})
                  '()]
                 [else (fail 'parse-object c)]))))))

  (define (parse-array in)
    (get-char in)                       ; drop #\[
    (list->vector
     (let ([c (peek-char in)])
       (cond
        [(eof-object? c) (fail 'parse-array c)]
        [(char=? c #\]) '()]
        [else
         (%parse-array in)]))))

  (define (%parse-array in)
    (let ([first (parse-json in)])
      (skip-whitespace in)
      (let ([c (get-char in)])
        (cons first
              (cond
               [(eof-object? c) (fail parse-array c)]
               [(char=? c #\,) (%parse-array in)]
               [(char=? c #\]) '()]
               [else (fail parse-array c)])))))

  (define (parse-string in)
    (get-char in)
    (call-with-string-output-port
      (lambda (out)
        (let loop ()
          (let ([c (get-char in)])
            (case c
              [(#\") 'done]
              [(#\\)
               (let ([c (get-char in)])
                 (case c
                   [(#\\ #\/) (put-char out c)]
                   [(#\n) (put-char out #\newline)]
                   [(#\t) (put-char out #\tab)]
                   [(#\r) (put-char out #\return)]
                   [(#\b) (put-char out #\backspace)]
                   [(#\f) (put-char out #\x000c)]
                   [(#\u)
                    (let* ([s (get-string-n in 4)])
                      (cond
                       [(string->number s 16)
                        => (lambda (n)
                             (put-char out (integer->char s)))]
                       [else (fail 'parse-string c)]))]
                   [else
                    (put-char out #\\)
                    (put-char out c)]))
               (loop)]
              [else
               (put-char out c)
               (loop)]))))))

  (define (parse-number in)
    (define number/string
      (call-with-string-output-port
        (lambda (out)
          (let loop ([decimal? #f])
            (let ([c (peek-char in)])
              (cond
               [(eof-object? c) 'done]
               [(char-degit? c)
                (get-char in)
                (put-char out c)
                (loop decimal?)]
               [(and (not decimal?) (char=? c #\.))
                (get-char in)
                (put-char out c)
                (loop #t)]
               [else 'done]))))))
    (cond
     [(string->number number/string) => values]
     [else (error 'string->json "parse-number error")]))
    
  (define (parse-symbol in)
    (string->symbol
     (call-with-string-output-port
       (lambda (out)
         (let loop ()
           (let ([c (peek-char in)])
             (cond
              [(eof-object? c) 'done]
              [(char-alphabetic? c)
               (get-char in)
               (put-char out c)
               (loop)]
              [else 'done])))))))
  
  (define (json->string sexp)
    (call-with-string-output-port
      (lambda (out)
        (%json->string
         out sexp
         (lambda ()
           (error 'json->string "fail to convert json string" sexp))))))
        
  (define (%json->string out sexp fail)
    (cond
     [(list? sexp)
      (cond
       [(null? sexp)
        (put-string out "{}")]
       [else
        (put-char out #\{)
        (let loop ([sexp sexp])
          (define (put-pair)
            (let ([pair (car sexp)])
              (cond
               [(and (pair? pair)
                     (string? (car pair)))
                (put-char out #\")
                (put-string out (car pair))
                (put-char out #\")
                (put-char out #\:)
                (%json->string out (cdr pair) fail)]
               [else (fail)])))
          (cond
           [(null? (cdr sexp))
            (put-pair)
            (put-char out #\})]
           [else
            (put-pair)
            (put-char out #\,)
            (loop (cdr sexp))]))])]
     [(vector? sexp)
      (let ([n (vector-length sexp)])
        (cond
         [(= n 0)
          (put-string out "[]")]
         [else
          (put-char out #\[)
          (let loop ([i 0])
            (cond
             [(= i (- n 1))
              (%json->string out (vector-ref sexp i) fail)
              (put-char out #\])]
             [else
              (%json->string out (vector-ref sexp i) fail)
              (put-char out #\,)
              (loop (+ i 1))]))]))]
     [(string? sexp)
      (put-char out #\")
      (string-for-each
       (lambda (c)
         (case c
           [(#\newline) (put-string out "\\n")]
           [(#\tab) (put-string out "\\t")]
           [(#\return) (put-string out "\\r")]
           [(#\backspace) (put-string out "\\b")]
           [(#\x000c) (put-string out "\\f")]
           [else
            (put-char out c)]))
       sexp)
      (put-char out #\")]
     [(integer? sexp) (display sexp out)]
     [(real? sexp)
      (display (if (exact? sexp)
                   (inexact sexp)
                   sexp)
               out)]
     [(boolean? sexp)
      (if sexp
          (put-string out "true")
          (put-string out "false"))]
     [(json-null? sexp)
      (put-string out "null")]
     [else
      (fail)])))
