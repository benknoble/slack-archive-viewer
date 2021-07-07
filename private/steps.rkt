#lang racket

(provide define-steps)

(require syntax/parse/define
         (for-syntax syntax/parse/lib/function-header))

(begin-for-syntax
  (define-splicing-syntax-class step
    #:datum-literals (step)
    (pattern {~seq step message:string {~and form:expr {~not step}} ...})))

(define-syntax-parse-rule (define-steps header:function-header step:step ...)
  (define header
    (define step-number -1)
    (begin
      (set! step-number (add1 step-number))
      (displayln (format "~a. ~a" step-number step.message))
      step.form ...)
    ...))

(module+ test
  (require rackunit)

  (define-steps (foo bar)
    step "Setup"
    (define a 1)
    (define b 2)
    step "Make a list"
    (list a b bar))

  (check-equal? (with-output-to-string (thunk (check-equal? (foo 3) '(1 2 3))))
                "0. Setup\n1. Make a list\n"))
