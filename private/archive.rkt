#lang racket

(provide (struct-out archive)
         extract
         copy-to-_data
         delete)

(require file/unzip
         "files.rkt")

(struct archive [zip-loc temp-dir])

(define/match (extract the-archive)
  [((archive zip-loc temp-dir))
   (parameterize ([current-directory temp-dir])
     (unzip zip-loc))])

(define/match (copy-to-_data the-archive)
  [((archive _ temp-dir))
   (define channels
     (filter (λ (name) (and (not (path-has-extension? name ".json"))
                            (directory-exists? name)))
             (directory-list temp-dir #:build? #t)))
   (for-each (λ (channel)
               (copy-directory/files* channel
                                     (build-path "_data" (file-name-from-path channel))
                                     #t))
             channels )])

(define/match (delete the-archive)
  [((archive _ temp-dir))
    (delete-directory/files temp-dir)])
