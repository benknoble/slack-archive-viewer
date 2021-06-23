#lang racket

(provide (all-defined-out))

(require racket/require
         (multi-in "private" ("meta.rkt")))

(define data (build-path "_data"))
(define channels
  (meta (λ ([base data]) (build-path base "channels.json"))
        merge-channel-metas))
(define users
  (meta (λ ([base data]) (build-path base "users.json"))
        merge-user-metas))

(define metas (list
                channels
                users))

(define (merge-meta the-meta meta-paths)
  (define make-path (meta-make-path the-meta))
  (define metadata-array (append-map (compose1 read-meta-from-file-or-null make-path) meta-paths))
  (define metadata-by-id (meta-array-to-hash-by-id metadata-array))
  (define merged-metadata ((meta-merger the-meta) metadata-by-id))
  (update-meta (make-path) merged-metadata))

(define (run-main meta-paths)
  (for-each (λ (meta) (merge-meta meta meta-paths))
            metas))

(module+ main
  (command-line
    #:args meta-paths
    (run-main meta-paths)))
