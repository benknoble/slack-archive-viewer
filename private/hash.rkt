#lang racket

(provide hash-update-all)

(module+ test
  (require rackunit
           rackunit/text-ui))

(define (hash-update-all the-hash updater)
  (foldl (λ (k acc) (hash-update acc k updater))
         the-hash
         (hash-keys the-hash)))

(module+ test
  (define-test-suite test-hash-update-all
    (test-equal? "empty hash"
                 (hash-update-all #hash() (const 1))
                 #hash())
    (test-equal? "identity"
                 (hash-update-all #hash([a . 2] [b . 3]) identity)
                 #hash([a . 2] [b . 3]))
    (test-equal? "updater"
                 (hash-update-all #hash([a . 2] [b . 3]) add1)
                 #hash([a . 3] [b . 4])))
  (run-tests test-hash-update-all))
