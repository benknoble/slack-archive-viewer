#lang racket/base

(provide channels
         all-channel-files)

(require racket/path
         racket/function
         racket/list
         "json.rkt")

(define (channels dir)
  (filter (λ (name) (and (not (path-has-extension? name ".json"))
                         (directory-exists? name)))
          (directory-list dir #:build? #t)))

(define all-channel-files
  (compose1 (curry append-map find-all) channels))
