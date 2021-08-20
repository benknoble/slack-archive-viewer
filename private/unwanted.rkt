#lang racket/base

(provide clean)

(require racket/file
         "config.rkt"
         "async.rkt")

(define (clean dir)
  (define-from-config unwanted-channels (list))
  (when (not (null? unwanted-channels))
    (for-each/async delete-directory/files
                    (map (λ (p) (build-path dir p)) unwanted-channels))))
