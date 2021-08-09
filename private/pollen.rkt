#lang racket/base

(provide channel-json->pollen-text
         ->pollen)

(require racket/string
         racket/future
         racket/list
         racket/file
         racket/path
         racket/function
         "json.rkt"
         "files.rkt"
         (only-in "channels.rkt" all-channel-files)
         (rename-in "channels.rkt" [channels channel-names])
         "meta.rkt"
         (only-in "../merge-meta.rkt" users channels)
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
  (for-each mkdir!
            (map (compose1 (curry build-path "pollen") file-name-from-path)
                 (channel-names data-dir)))

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

  step "Convert metadata to jsond"
  (define meta-files (list ((meta-info-make-path channels) data-dir)
                           ((meta-info-make-path users) data-dir)))
  (define output-meta-files
    (map (λ (p)
           (define with-racket-ext (path-replace-extension p ".rkt"))
           (define bare-name (file-name-from-path with-racket-ext))
           (define in-pollen (list "pollen" bare-name))
           (path->complete-path (apply build-path in-pollen)))
         meta-files))
  (for/async ([meta-file meta-files]
              [output-meta-file output-meta-files])
    (define jsond-text (cons "#lang jsond" (cons "#:name meta" (file->lines meta-file))))
    (display-lines-to-file jsond-text output-meta-file))

  (values output-files output-meta-files))

;; vim: lw+=define-steps
