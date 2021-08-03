#lang racket/base

(provide channels
         all-channel-files)

(require racket/path
         racket/function
         racket/list
         "json.rkt")

(define (channels dir)
  (filter (conjoin (negate (curryr path-has-extension? ".json"))
                   directory-exists?)
          (directory-list dir #:build? #t)))

(define all-channel-files
  (compose1 (curry append-map find-all) channels))
