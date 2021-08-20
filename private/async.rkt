#lang racket/base

(provide for-each/async)

(require racket/future)

(define (for-each/async f . xss)
  (define each-runs-args (apply map list xss))
  (for/async ([args each-runs-args])
    (apply f args)))
