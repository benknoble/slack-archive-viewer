#lang racket

(provide clean)

(require "channels.rkt"
         "files.rkt"
         "config.rkt"
         json)

(define (clean dir)
  (define-from-privacy-list private-user-ids (set))
  (when (not (set-empty? private-user-ids))
    (define all-json (all-channel-files dir))
    (async-edit (compose1 jsexpr->string (filter-messages-by-user-ids private-user-ids) string->jsexpr)
                all-json)))

(define ((filter-messages-by-user-ids user-ids) the-json)
  (filter (negate (λ (message)
                     (set-member? user-ids
                                  (string->symbol (hash-ref message 'user "")))))
          the-json))
