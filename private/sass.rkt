#lang racket/base

(provide (all-from-out sass)
         current-include-paths
         compile/file/out)

(require sass)

;; hack: sass doesn't export its parameter!
(define current-include-paths
  (begin
    (dynamic-require 'sass #f)
    (eval 'current-include-paths (module->namespace 'sass))))

(define (compile/file/out in)
  (define rendered (compile/file in))
  (define output-path (path-replace-extension in ".css"))
  (with-output-to-file output-path
    (λ () (write-string rendered)))
  output-path)
