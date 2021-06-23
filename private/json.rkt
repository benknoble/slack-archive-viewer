#lang racket

(provide find-all
         unescape)

(require file/glob)

(define (find-all dir)
  (glob (build-path dir "**.json")))

(define (unescape json)
  (regexp-replace* #rx"\\\\/" json "/"))
