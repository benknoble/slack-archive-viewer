#lang racket

(provide find-all
         unescape
         file->json)

(require file/glob
         json)

(define (find-all dir)
  (glob (build-path dir "**.json")))

(define (unescape json)
  (regexp-replace* #rx"\\\\/" json "/"))

(define file->json (compose1 string->jsexpr file->string))
