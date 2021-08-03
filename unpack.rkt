#lang racket

(provide (all-defined-out))

(require "private/files.rkt"
         (prefix-in meta: "merge-meta.rkt")
         (prefix-in archive: "private/archive.rkt")
         (prefix-in json: "private/json.rkt")
         "private/steps.rkt"
         (prefix-in privacy: "private/privacy.rkt"))

(define (preproc-json dir)
  (async-edit json:unescape (json:find-all dir)))

(define-steps (run-main archives-dir)
  step "Create or clean _data dir"
  (mkdir! "_data")

  step "Gather the list of archives to process"
  (define archives (archive:find-archives archives-dir))

  step "Extract each archive"
  (for-each archive:extract archives)

  step "Gather all the metadata into _data"
  (meta:run-main (map archive:archive-temp-dir archives))

  step "Copy all the channel files into _data"
  (for-each archive:copy-to-_data archives)

  step "Clean up after ourselves"
  (for-each archive:delete archives)

  step "Remove messages from privacy list"
  (privacy:clean "_data")

  step "Pre-process json slashes"
  (preproc-json "_data"))

(module+ main
  (command-line
    #:args (archives-dir)
    (run-main archives-dir)))

; vim: lw+=define-steps
