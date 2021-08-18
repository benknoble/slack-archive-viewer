#lang info
(define collection "slack-archive-viewer")
(define deps '("sass"
               "jsond"
               "pollen"
               "txexpr"
               "sugar"
               "markdown"
               "base"))
(define build-deps '("scribble-lib" "racket-doc" "rackunit-lib"))
(define compile-omit-paths '("css"
                             "_sass"
                             "_plugins"
                             "_layouts"
                             "_data"
                             "_includes"
                             "_site"
                             "sample-data"
                             "privacy-list-demo.rkt"
                             "pollen"))
(define scribblings '(("scribblings/slack-archive-viewer.scrbl" ())))
(define pkg-desc "Generates a website to view slack archives")
(define version "0.0")
(define pkg-authors '(benknoble))
