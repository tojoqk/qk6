(library (qk6 xml)
  (export put-xml xml->string)
  (import (rename (rnrs) [put-string %put-string]))

  (define (xml->string xml)
    (call-with-string-output-port
      (lambda (out)
        (guard (con
                ([(and (who-condition? con)
                       (eq? 'put-xml (condition-who con)))
                  (error 'xml->string "invalid xml sexp" xml)]))
          (put-xml out xml)))))

  (define-syntax put-string
    (lambda (ctx)
      (syntax-case ctx ()
        [(_ out string-literal)
         (string? (syntax->datum #'string-literal))
         #'(%put-string out string-literal)])))

  (define (tag/attr? x)
    (and (pair? x)
         (pair? (cdr x))
         (let ([tag (car x)]
               [attrs (cadr x)]
               [body (cddr x)])
           (and (symbol? tag)
                (attrs? attrs)
                (list? body)))))

  (define (attrs? x)
    (and (list? x)
         (for-all (lambda (attr)
                    (and (pair? attr)
                         (pair? (cdr attr))
                         (null? (cddr attr))
                         (symbol? (car attr))
                         (string? (cadr attr))))
                  x)))

  (define (tag? x)
    (and (pair? x)
         (symbol? (car x))
         (list? (cdr x))))

  (define (put-xml out xml)
    (cond
     [(string? xml)
      (put-xml/string out xml)]
     [(tag/attr? xml)
      (put-xml/tag-attr out xml)]
     [(tag? xml)
      (put-xml/tag out xml)]
     [else
      (error 'put-xml "invalid xml" xml)]))

  (define (put-xml/string out xml)
    (string-for-each
     (lambda (c)
       (case c
         [(#\&) (put-string out "&amp;")]
         [(#\<) (put-string out "&lt;")]
         [(#\>) (put-string out "&gt;")]
         [(#\") (put-string out "&quot;")]
         [(#\') (put-string out "&#39;")]
         [else (put-char out c)]))
     xml))

  (define (put-xml/symbol out xml)
    (put-xml/string out (symbol->string xml)))

  (define (put-xml/attr out attr)
    (put-string out " ")
    (put-xml/symbol out (car attr))
    (put-string out "=\"")
    (put-xml/string out (cadr attr))
    (put-string out "\""))

  (define (put-xml/tag-attr out xml)
    (let ([tag (car xml)]
          [attrs (cadr xml)]
          [body (cddr xml)])
      (put-string out "<")
      (put-xml/symbol out tag)
      (for-each (lambda (attr) (put-xml/attr out attr)) attrs)
      (cond
       [(null? body)
        (put-string out "/>")]
       [else
        (put-string out ">")
        (for-each (lambda (x) (put-xml out x)) body)
        (put-string out "</")
        (put-xml/symbol out tag)
        (put-string out ">")])))

  (define (put-xml/tag out xml)
    (let ([tag (car xml)]
          [body (cdr xml)])
      (put-string out "<")
      (put-xml/symbol out tag)
      (cond
       [(null? body)
        (put-string out "/>")]
       [else
        (put-string out ">")
        (for-each (lambda (x) (put-xml out x)) body)
        (put-string out "</")
        (put-xml/symbol out tag)
        (put-string out ">")]))))
