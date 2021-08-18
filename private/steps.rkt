#lang racket

(provide define-steps)

(require syntax/parse/define
         (for-syntax syntax/parse/lib/function-header))

(define step-level (make-parameter -1))
(define (indent) (build-string (* (step-level) 3) (λ _ #\space)))

(begin-for-syntax
  (define-splicing-syntax-class step
    #:datum-literals (step)
    (pattern {~seq step message:string {~and form:expr {~not step}} ...})))

(define-syntax-parse-rule (define-steps header:function-header step:step ...+)
  (define header
    (parameterize ([step-level (add1 (step-level))])
      (define step-number -1)
      (begin
        (set! step-number (add1 step-number))
        (displayln (format "~a~a. ~a" (indent) step-number step.message))
        step.form ...)
      ...)))

(module+ test
  (require rackunit)

  (define-steps (foo bar)
    step "Setup"
    (define a 1)
    (define b 2)
    step "Make a list"
    (list a b bar))

  (define-steps (baz quux)
    step "My variables"
    (define c 3)
    step "Make a list with sublists"
    (list (foo c) quux))


  (check-equal? (with-output-to-string (thunk (check-equal? (foo 3) '(1 2 3))))
                "0. Setup\n1. Make a list\n")

  (check-equal? (with-output-to-string (thunk (check-equal? (baz 3) '((1 2 3) 3))))
                #<<EOS
0. My variables
1. Make a list with sublists
   0. Setup
   1. Make a list

EOS
                ))

;; vim: lw+=define-steps
