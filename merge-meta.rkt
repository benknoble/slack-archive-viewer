#lang racket

(provide (all-defined-out))

(require racket/require
         (multi-in "private" ("meta.rkt")))

(define data (build-path "_data"))
(define channels
  (meta-info (λ ([base data]) (build-path base "channels.json"))
             merge-channel-metas))
(define users
  (meta-info (λ ([base data]) (build-path base "users.json"))
             merge-user-metas))

(define meta-infos (list
                     channels
                     users))

(define (merge-meta the-meta-info meta-paths)
  (define make-path (meta-info-make-path the-meta-info))
  (define metadata-array (append-map (compose1 read-meta-from-file-or-null make-path) meta-paths))
  (define metadata-by-id (meta-array-to-hash-by-id metadata-array))
  (define merged-metadata ((meta-info-merger the-meta-info) metadata-by-id))
  (update-meta (make-path) merged-metadata))

(define (run-main meta-paths)
  (for-each (λ (meta-info) (merge-meta meta-info meta-paths))
            meta-infos))

(module+ main
  (command-line
    #:args meta-paths
    (run-main meta-paths)))
