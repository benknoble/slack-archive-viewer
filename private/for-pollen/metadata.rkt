#lang racket/base

;; this file is for testing private/for-pollen/pollen.rkt ONLY
;; it is NOT part of the pollen build

(require pollen/template/html)

(provide (all-defined-out))

(define nav (->html
              '((a ((href "/advent-of-code.html") (class "page-link")) "advent-of-code")
                (a ((href "/beginners.html") (class "page-link")) "beginners")
                (a ((href "/general.html") (class "page-link")) "general")
                (a ((href "/math-scribble.html") (class "page-link")) "math-scribble")
                (a ((href "/random.html") (class "page-link")) "random")
                (a ((href "/scribble-math.html") (class "page-link")) "scribble-math")
                (a ((href "/vim.html") (class "page-link")) "vim"))))

(define last-archived-date
  "2021-06-07")
