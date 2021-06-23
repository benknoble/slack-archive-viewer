#lang racket

(provide (all-defined-out))

(require racket/require
         file/unzip
         file/glob
         (prefix-in meta: "merge-meta.rkt")
         (multi-in "private" ("files.rkt")))

(struct archive (zip-loc temp-dir))

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

(define (preproc-json dir)
  (for/async ([filepath (glob (build-path dir "**.json"))])
    (define contents (file->string filepath))
    (define unescaped (regexp-replace* #rx"\\\\/" contents "/"))
    (display-to-file unescaped filepath #:exists 'truncate/replace)))

(define (run-main archives-dir)
  (displayln "0. Make _data dir if needed")
  (unless (directory-exists? "_data")
    (make-directory "_data"))

  (displayln "1. Gather the list of archives to process")
  (define zip-locs
    (sort (glob (build-path archives-dir "*.zip"))
          path<?))

  (displayln "2. Associate each with a temporary directory")
  (define archives
    (map (λ (zip-loc) (archive zip-loc (make-temporary-file "extract-~a" 'directory)))
         zip-locs))

  (displayln "3. Extract each archive")
  (for-each extract archives)

  (displayln "4. Gather all the metadata into _data")
  (meta:run-main (map archive-temp-dir archives))

  (displayln "5. Copy all the channel files into _data")
  (for-each copy-to-_data archives)

  (displayln "6. Clean up after ourselves")
  (for-each delete archives)

  (displayln "7. Pre-process json slashes")
  (preproc-json "_data"))

(module+ main
  (command-line
    #:args (archives-dir)
    (run-main archives-dir)))
