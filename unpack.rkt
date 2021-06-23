#lang racket

(provide (all-defined-out))

(require file/glob
         (prefix-in meta: "merge-meta.rkt")
         (prefix-in archive: "private/archive.rkt"))

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
    (map (λ (zip-loc) (archive:archive zip-loc (make-temporary-file "extract-~a" 'directory)))
         zip-locs))

  (displayln "3. Extract each archive")
  (for-each archive:extract archives)

  (displayln "4. Gather all the metadata into _data")
  (meta:run-main (map archive:archive-temp-dir archives))

  (displayln "5. Copy all the channel files into _data")
  (for-each archive:copy-to-_data archives)

  (displayln "6. Clean up after ourselves")
  (for-each archive:delete archives)

  (displayln "7. Pre-process json slashes")
  (preproc-json "_data"))

(module+ main
  (command-line
    #:args (archives-dir)
    (run-main archives-dir)))
