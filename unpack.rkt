#lang racket

(provide (all-defined-out))

(require
  file/unzip
  file/glob
  (prefix-in meta: "merge-meta.rkt"))

(struct archive (zip-loc temp-dir))

(define/match (extract the-archive)
  [((archive zip-loc temp-dir))
   (parameterize ([current-directory temp-dir])
     (unzip zip-loc))])

;; from https://github.com/racket/racket/blob/master/racket/collects/racket/file.rkt
;; with copy-file's exists-ok? (and a hack for directories)
(define (copy-directory/files src dest
                              [exists-ok? #f]
                              #:keep-modify-seconds? [keep-modify-seconds? #f]
                              #:preserve-links? [preserve-links? #f])
  (let loop ([src src] [dest dest])
    (cond [(and preserve-links?
                (link-exists? src))
           (make-file-or-directory-link
             (resolve-path src)
             dest)]
          [(file-exists? src)
           (copy-file src dest exists-ok?)
           (when keep-modify-seconds?
             (file-or-directory-modify-seconds
               dest
               (file-or-directory-modify-seconds src)))]
          [(directory-exists? src)
           (if exists-ok?
             (with-handlers ([exn:fail:filesystem? (const (void))])
               (make-directory dest))
             (make-directory dest))
           (for-each (lambda (e)
                       (loop (build-path src e)
                             (build-path dest e)))
                     (directory-list src))]
          [else (raise-not-a-file-or-directory 'copy-directory/files src)])))

;; from https://github.com/racket/racket/blob/master/racket/collects/racket/file.rkt
;; needed by above
(define (raise-not-a-file-or-directory who path)
  (raise
   (make-exn:fail:filesystem
    (format "~a: encountered path that is neither file nor directory\n  path: ~a"
            who
            path)
    (current-continuation-marks))))

(define/match (copy-to-_data the-archive)
  [((archive _ temp-dir))
   (define channels
     (filter (λ (name) (and (not (path-has-extension? name ".json"))
                            (directory-exists? name)))
             (directory-list temp-dir #:build? #t)))
   (for ([channel channels])
     (copy-directory/files channel
                           (build-path "_data" (file-name-from-path channel))
                           #t))])

(define/match (delete the-archive)
  [((archive _ temp-dir))
    (delete-directory/files temp-dir)])

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
  (for ([archive archives]) (extract archive))

  (displayln "4. Gather all the metadata into _data")
  (meta:run-main (map archive-temp-dir archives))

  (displayln "5. Copy all the channel files into _data")
  (for ([archive archives]) (copy-to-_data archive))

  (displayln "6. Clean up after ourselves")
  (for ([archive archives]) (delete archive))

  (displayln "7. Run the preprocessing script")
  (unless (system* "./preproc" "_data")
    (error "preproc failed")))

(module+ main
  (command-line
    #:args (archives-dir)
    (run-main archives-dir)))
