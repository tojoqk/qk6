(library (qk6 test)
  (export test-begin
          test-end
          test-assert
          test-equal
          test-eqv
          test-eq)
  (import (rnrs base)
          (rnrs io simple)
          (rnrs io ports)
          (rnrs control)
          (rnrs programs)
          (rnrs records syntactic)
          (rnrs lists)
          (rnrs conditions)
          (rnrs exceptions))

  (define test-name #f)
  (define states (list))
  (define-record-type state
    (fields (mutable passed-count)
            (mutable failed-count)
            output-port
            get-output-string))

  (define (test-ref who)
    (cond
     [(assoc test-name states) => cdr]
     [else (error who "use test-ref")]))

  (define (test-begin! n)
    (set! test-name n)
    (let-values ([(out get) (open-string-output-port)])
      (set! states (cons (cons test-name (make-state 0 0 out get))
                         states))))

  (define-syntax test-begin
    (syntax-rules ()
      [(_ name)
       (test-begin! name)]
      [(_)
       (test-begin! "test")]))

  (define (test-end!)
    (let* ([state (test-ref 'test-end)]
           [passed-count (state-passed-count state)]
           [failed-count (state-failed-count state)]
           [out (state-output-port state)]
           [get-output-string (state-get-output-string state)])
      (newline)
      (display test-name)
      (display ":")
      (newline)
      (display "  passed: ")
      (display passed-count)
      (newline)
      (display "  failed: ")
      (display failed-count)
      (newline)
      (put-string (current-output-port) (get-output-string))
      (if (zero? failed-count)
          (exit)
          (exit #f))))

  (define-syntax test-end
    (syntax-rules ()
      [(_ name)
       (test-end!)]
      [(_)
       (test-end!)]))

  (define (pass!)
    (display ".")
    (flush-output-port (current-output-port))
    (let ([state (test-ref test-name)])
      (state-passed-count-set! state
                               (+ 1 (state-passed-count state)))))

  (define (fail!)
    (display "F")
    (flush-output-port (current-output-port))
    (let ([state (test-ref test-name)])
      (state-failed-count-set! state
                               (+ 1 (state-failed-count state)))))

  (define (test-output)
    (state-output-port (test-ref test-name)))

  (define (exception-error name con)
    (let ([out (test-output)])
      (newline out)
      (display "  FAIL " out)
      (display name out)
      (newline out)
      (when (who-condition? con)
        (display "    who      : " out)
        (display (condition-who con) out)
        (newline out))
      (when (message-condition? con)
        (display "    message  :  " out)
        (display (condition-message con) out)
        (newline out))
      (when (irritants-condition? con)
        (display "    irritants:  " out)
        (display (condition-irritants con) out)
        (newline out))))

  (define (given-expected-error name form expected given)
    (let ([out (test-output)])
      (newline out)
      (display "  FAIL " out)
      (display name out)
      (newline out)
      (display "    form    : " out)
      (write form out)
      (newline out)
      (display "    expected: " out)
      (write expected out)
      (newline out)
      (display "    given   : " out)
      (write given out)
      (newline out)))

  (define-syntax test-assert
    (syntax-rules ()
      [(_ name test)
       (guard (con
               [else
                (fail!)
                (exception-error name con)])
         (if test
             (pass!)
             (begin
               (fail!)
               (given-expected-error name 'test #t #f))))]
      [(_ test)
       (test-assert 'test test)]))

  (define-syntax test-eq*
    (syntax-rules ()
      [(_ = name expected given)
       (guard (con
               [else
                (fail!)
                (exception-error name con)])
         (let ([expected/value expected]
               [given/value given])
           (if (= expected/value given/value)
               (pass!)
               (begin
                 (fail!)
                 (given-expected-error name 'given expected given/value)))))]))

  (define-syntax test-equal
    (syntax-rules ()
      [(_ name expected given)
       (test-eq* equal? name expected given)]
      [(_ expected given)
       (test-eq* equal? 'given expected given)]))

  (define-syntax test-eqv
    (syntax-rules ()
      [(_ name expected given)
       (test-eq* eqv? name expected given)]
      [(_ expected given)
       (test-eq* eqv? 'given expected given)]))

  (define-syntax test-eq
    (syntax-rules ()
      [(_ name expected given)
       (test-eq* eq? name expected given)]
      [(_ expected given)
       (test-eq* eq? 'given expected given)])))
