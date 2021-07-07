#lang racket

(provide clean)

(require "archive.rkt"
         "files.rkt"
         (prefix-in json: "json.rkt")
         json)

(define (clean dir)
  (define user-ids-to-exclude
    (and
      (file-exists? "privacy-list.rkt")
      (dynamic-require "privacy-list.rkt" 'private-user-ids (thunk #f))))
  (when user-ids-to-exclude
    (define channel-dirs (channels dir))
    (define all-json (append-map json:find-all channel-dirs))
    (async-edit (compose1 jsexpr->string (filter-messages-by-user-ids user-ids-to-exclude) string->jsexpr)
                all-json)))

(define ((filter-messages-by-user-ids user-ids) the-json)
  (filter (negate (λ (message)
                     (set-member? user-ids
                                  (string->symbol (hash-ref message 'user)))))
          the-json))
