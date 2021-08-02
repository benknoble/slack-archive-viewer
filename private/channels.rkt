#lang racket/base

(provide channel-json->pollen-text)

(require racket/string)

(define (channel-json->pollen-text channel)
  (string-join (map message-json->pollen-text channel) "\n"))

(define (message-json->pollen-text message)
  (format "◊~a[~a]{~a}"
          (or (hash-ref message 'subtype #f)
              "message")
          (string-join
            (for/list ([(k v) (in-hash message)])
              (format "#:~a ~v" k v))
            " ")
          (hash-ref message 'text "")))
