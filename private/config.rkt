#lang racket/base

(provide define-dynamic-definer
         define-from-privacy-list
         privacy-list
         define-from-config
         config)

(require (for-syntax racket/base
                     racket/syntax)
         syntax/parse/define)

(define-for-syntax (make-define-transformer name)
  (syntax-parser
    [(_ var:id {~optional default:expr})
     #`(define var (#,name 'var (~? default)))]))

(define-syntax-parse-rule (define-dynamic-definer name:id path:expr)
  #:with definer (format-id this-syntax #:source this-syntax
                            "define-from-~a" (syntax-e #'name))
  #:with path-id (format-id this-syntax #:source this-syntax "path")
  (begin
    (define (name var [default #f])
      (let ([path-id path])
        (if (file-exists? path-id)
          (dynamic-require path-id var (λ () default))
          default)))
    (define-syntax definer (make-define-transformer #'name))))

(define-dynamic-definer privacy-list "privacy-list.rkt")
(define-dynamic-definer config "slack-config.rkt")
