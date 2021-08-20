#lang racket

(provide (struct-out archive)
         make-archive
         find-archives
         extract
         copy-to-_data
         delete
         channels)

(require file/unzip
         file/glob
         "files.rkt"
         "channels.rkt"
         "async.rkt")

(struct archive [zip-loc temp-dir])

(define (make-archive zip-loc)
  (archive zip-loc (make-temporary-file "extract-~a" 'directory)))

(define (find-archives dir)
  (define zips (sort (glob (build-path dir "*.zip"))
                     path<?))
  (map make-archive zips))

(define/match (extract the-archive)
  [((archive zip-loc temp-dir))
   (parameterize ([current-directory temp-dir])
     (unzip zip-loc))])

(define/match (copy-to-_data the-archive)
  [((archive _ temp-dir))
   (for-each/async (λ (channel)
                     (copy-directory/files* channel
                                            (build-path "_data" (file-name-from-path channel))
                                            #t))
                   (channels temp-dir))])

(define/match (delete the-archive)
  [((archive _ temp-dir))
    (delete-directory/files temp-dir)])
