#lang racket/base

(provide define-dynamic-definer
         define-from-privacy-list
         privacy-list
         define-from-config
         config)

(require (for-syntax racket/base
                     racket/syntax)
         syntax/parse/define)

(define-syntax-parse-rule (define-dynamic-definer name:id path:string)
  #:with definer (format-id this-syntax #:source this-syntax
                            "define-from-~a" (syntax-e #'name))
  (begin
    (define (name var [default #f])
      (if (file-exists? path)
        (dynamic-require path var (λ () default))
        default))
    (define-syntax-parse-rule (definer var:id {~optional default:expr #:defaults ([default #'#f])})
      (define var (name 'var default)))))

(define-dynamic-definer privacy-list "privacy-list.rkt")
(define-dynamic-definer config "slack-config.rkt")
