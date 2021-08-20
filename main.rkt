#lang racket/base

(provide run-main)

(require racket/list
         "private/steps.rkt"
         (rename-in "unpack.rkt" [run-main unpack])
         "private/pollen.rkt"
         "private/sass.rkt"
         "private/async.rkt")

(define (get-sass-and-css-paths static-files)
  (define sass-dirs (filter (λ (p) (regexp-match? #rx"sass" p)) static-files))
  (define css-files
    (append-map (λ (p)
                  (cond
                    [(directory-exists? p) (directory-list p #:build? #t)]
                    [(file-exists? p) p]))
                (filter (λ (p) (regexp-match? #rx"css" p))
                        static-files)))
  (values sass-dirs css-files))

(define-steps (run-main archives-dir out-dir)
  step "Unpack data"
  (unpack archives-dir)

  step "Convert to pollen"
  ;; TODO make directory names an argument and return value from more functions
  ;; like "_data" and "pollen" below
  (define generated-files (->pollen "_data"))

  step "Render pollen"
  (render)

  step "Render CSS"
  (define static-files (hash-ref generated-files 'statics))
  (define-values (sass-dirs css-files) (get-sass-and-css-paths static-files))
  (parameterize ([current-include-paths sass-dirs])
    (for-each/async (λ (f)
                      (displayln (format "Compiling SCSS ~v" f))
                      (compile/file/out f))
                    css-files))

  step "Publish"
  (publish "pollen" out-dir))

(module+ main
  (require racket/cmdline
           "private/config.rkt"
           "private/files.rkt")
  (define default-out-dir
    (let ([base-url (config 'base-url)])
      (cond
        [base-url (remove-one-dir base-url)]
        [else "_site"])))
  (command-line
    #:args (archives-dir [out-dir default-out-dir])
    (run-main archives-dir out-dir)))

;; vim: lw+=define-steps
