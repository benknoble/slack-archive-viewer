#lang racket/base

(provide channel-json->pollen-text
         ->pollen)

(require racket/string
         racket/future
         racket/list
         racket/file
         racket/function
         "json.rkt"
         "files.rkt"
         "channels.rkt"
         "steps.rkt")

(define (channel-json->pollen-text channel)
  (cons "#lang pollen" (map message-json->pollen-text channel)))

(define (message-json->pollen-text message)
  (format "◊~a[~a]{~a}"
          (or (hash-ref message 'subtype #f) "message")
          (string-join (hash-map message (curry format "#:~a ~v")) " ")
          (hash-ref message 'text "")))

(define-steps (->pollen data-dir)
  step "Make pollen directory tree"
  (mkdir! "pollen")

  step "Convert channel files to pollen"
  (define channel-file->pollen (compose1 channel-json->pollen-text file->json))
  (define channel-files (all-channel-files data-dir))
  (define output-files
    (map (λ (p)
           (define with-pollen-ext (path-replace-extension p ".html.pm"))
           (define dir+file (take-right (explode-path with-pollen-ext) 2))
           (define in-pollen (cons "pollen" dir+file))
           (path->complete-path (apply build-path in-pollen)))
         channel-files))
  (for/async ([channel-file channel-files]
              [output-file output-files])
    (define pollen-text (channel-file->pollen channel-file))
    (display-lines-to-file pollen-text output-file))
  output-files)

;; vim: lw+=define-steps
