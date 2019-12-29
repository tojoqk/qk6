(library (qk6 json)
  (export json-null? json-null
          get-json put-json
          string->json json->string)
  (import (rnrs base)
          (rnrs exceptions)
          (rnrs conditions)
          (rnrs io ports)
          (rnrs io simple)
          (rnrs unicode))

  (define json-null 'null)

  (define (json-null? x)
    (eq? x 'null))

  (define (string->json str)
    (let ([in (open-string-input-port str)])
      (guard (con
              ([(and (who-condition? con)
                     (eq? 'get-json (condition-who con)))
                (error 'string->json "invalid json string" str)]))
        (get-json in))))

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

  (define (fail/get type c)
    (error 'get-json
           (string-append (symbol->string type) " error")
           c))

  (define (get-json in)
    (skip-whitespace in)
    (let ([c (peek-char in)])
      (cond
       [(eof-object? c) (fail/get 'get-json c)]
       [(char=? c #\{)
        (get-json/object in)]
       [(char=? c #\[)
        (get-json/array in)]
       [(char-degit? c)
        (get-json/number in)]
       [(char=? c #\")
        (get-json/string in)]
       [(char-alphabetic? c)
        (case (get-json/symbol in)
          [(true) #t]
          [(false) #f]
          [(null) json-null]
          [else (fail/get 'get-json c)])]
       [else (fail/get 'get-json c)])))

  (define (get-json/object in)
    (get-char in)                       ; drop #\{
    (skip-whitespace in)
    (let ([c (peek-char in)])
      (cond
       [(eof-object? c) (fail/get 'get-json/object c)]
       [(char=? c #\})
        (get-char in)
        '()]
       [else
        (%get-json/object in)])))

  (define (%get-json/object in)
    (skip-whitespace in)
    (let ([key (get-json/string in)])
      (skip-whitespace in)
      (let ([value
             (let ([c (get-char in)])
               (cond
                [(eof-object? c) (fail/get 'get-json/object c)]
                [(char=? c #\:)
                 (get-json in)]
                [else (fail/get 'get-json/object c)]))])
        (skip-whitespace in)
        (cons (cons key value)
              (let ([c (get-char in)])
                (cond
                 [(eof-object? c) (fail/get 'get-json/object c)]
                 [(char=? c #\,)
                  (%get-json/object in)]
                 [(char=? c #\})
                  '()]
                 [else (fail/get 'get-json/object c)]))))))

  (define (get-json/array in)
    (get-char in)                       ; drop #\[
    (skip-whitespace in)
    (list->vector
     (let ([c (peek-char in)])
       (cond
        [(eof-object? c) (fail/get 'get-json/array c)]
        [(char=? c #\])
         (get-char in)                  ; drop #\]
         '()]
        [else
         (%get-json/array in)]))))

  (define (%get-json/array in)
    (let ([first (get-json in)])
      (skip-whitespace in)
      (let ([c (get-char in)])
        (cons first
              (cond
               [(eof-object? c) (fail/get get-json/array c)]
               [(char=? c #\,) (%get-json/array in)]
               [(char=? c #\]) '()]
               [else (fail/get get-json/array c)])))))

  (define (get-json/string in)
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
                       [else (fail/get 'get-json/string c)]))]
                   [else
                    (put-char out #\\)
                    (put-char out c)]))
               (loop)]
              [else
               (put-char out c)
               (loop)]))))))

  (define (get-json/number in)
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
     [else (error 'string->json "get-json/number error")]))
    
  (define (get-json/symbol in)
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
  
  (define (json->string json)
    (call-with-string-output-port
      (lambda (out)
        (guard (con
                ([(and (who-condition? con)
                       (eq? 'put-json (condition-who con)))
                  (error 'json->string "invalid json sexp" json)]))
          (put-json out json)))))

  (define (fail/put type json)
    (error 'put-json
           (string-append (symbol->string type) " error")
           json))

  (define (put-json/object out json)
    (cond
     [(null? json)
      (put-string out "{}")]
     [else
      (put-char out #\{)
      (let loop ([json json])
        (define (put-pair)
          (let ([pair (car json)])
            (cond
             [(and (pair? pair)
                   (string? (car pair)))
              (put-char out #\")
              (put-string out (car pair))
              (put-char out #\")
              (put-char out #\:)
              (put-json out (cdr pair))]
             [else (fail/put 'put-json/object json)])))
        (cond
         [(null? (cdr json))
          (put-pair)
          (put-char out #\})]
         [else
          (put-pair)
          (put-char out #\,)
          (loop (cdr json))]))]))

  (define (put-json/array out json)
    (let ([n (vector-length json)])
      (cond
       [(= n 0)
        (put-string out "[]")]
       [else
        (put-char out #\[)
        (let loop ([i 0])
          (cond
           [(= i (- n 1))
            (put-json out (vector-ref json i))
            (put-char out #\])]
           [else
            (put-json out (vector-ref json i))
            (put-char out #\,)
            (loop (+ i 1))]))])))

  (define (put-json/string out json)
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
     json)
    (put-char out #\"))

  (define (put-json/number out json)
    (put-string out
                (number->string
                 (cond
                  [(integer? json) json]
                  [(real? json)
                   (if (exact? json)
                       (inexact json)
                       json)]
                  [else
                   (fail/put 'put-json/number json)]))))

  (define (put-json/boolean out json)
    (if json
        (put-string out "true")
        (put-string out "false")))

  (define (put-json/null out json)
    (put-string out "null"))

  (define (put-json out json)
    (cond
     [(list? json) (put-json/object out json)]
     [(vector? json) (put-json/array out json)]
     [(string? json) (put-json/string out json)]
     [(number? json) (put-json/number out json)]
     [(boolean? json) (put-json/boolean out json)]
     [(json-null? json) (put-json/null out json)]
     [else
      (fail/put 'put-json json)]))
  )
