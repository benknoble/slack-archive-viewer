#lang racket/base

;; unbuffer all output, especially for GitHub actions
;; run your program like
;; racket -t unbuffer.rkt -t yourprog.rkt <args>
(file-stream-buffer-mode (current-output-port) 'none)
(file-stream-buffer-mode (current-error-port) 'none)
