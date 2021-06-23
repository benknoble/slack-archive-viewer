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
  (define archives (archive:find-archives archives-dir))

  (displayln "2. Extract each archive")
  (for-each archive:extract archives)

  (displayln "3. Gather all the metadata into _data")
  (meta:run-main (map archive:archive-temp-dir archives))

  (displayln "4. Copy all the channel files into _data")
  (for-each archive:copy-to-_data archives)

  (displayln "5. Clean up after ourselves")
  (for-each archive:delete archives)

  (displayln "6. Pre-process json slashes")
  (preproc-json "_data"))

(module+ main
  (command-line
    #:args (archives-dir)
    (run-main archives-dir)))
