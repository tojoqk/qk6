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
          (rnrs exceptions))

  (define name #f)
  (define states (list))
  (define-record-type state
    (fields (mutable passed-count)
            (mutable failed-count)
            output-port
            get-output-string))

  (define (test-ref who)
    (cond
     [(assoc name states) => cdr]
     [else (error who "use test-ref")]))

  (define (test-begin! n)
    (set! name n)
    (let-values ([(out get) (open-string-output-port)])
      (set! states (cons (cons name (make-state 0 0 out get))
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
      (display name)
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

  (define (pass! who)
    (let ([state (test-ref who)])
      (state-passed-count-set! state
                               (+ 1 (state-passed-count state)))))

  (define (fail! who)
    (let ([state (test-ref who)])
      (state-failed-count-set! state
                               (+ 1 (state-failed-count state)))))

  (define (test-output who)
    (state-output-port (test-ref who)))

  (define-syntax %test-assert
    (syntax-rules ()
      [(_ who test)
       (let ([put-fail
              (lambda ()
                (let ([out (test-output 'who)])
                  (display "  FAIL " out)
                  (display 'who out)
                  (display ": " out)
                  (display 'test out)
                  (newline out)))])
         (guard (con
                 [else
                  (fail! who)
                  (put-fail)])
           (if test
               (pass! who)
               (begin
                 (fail! who)
                 (put-fail)))))]))

  (define-syntax test-assert
    (syntax-rules ()
      [(_ test)
       (%test-assert 'test-assert test)]))

  (define-syntax test-equal
    (syntax-rules ()
      [(_ expected given)
       (%test-assert 'test-equal (equal? given expected))]))

  (define-syntax test-eqv
    (syntax-rules ()
      [(_ expected given)
       (%test-assert 'test-eqv (eqv? given expected))]))

  (define-syntax test-eq
    (syntax-rules ()
      [(_ expected given)
       (%test-assert 'test-eq (eq? given expected))])))
