#lang racket

(provide (all-defined-out))

(require
  syntax/parse/define
  racket/hash
  json)

(module+ test
  (require rackunit
           rackunit/text-ui))

(define (read-meta-from-file-or-null path)
  (if (file-exists? path)
    ;; race-condition: file may no longer exist
    (with-input-from-file path (thunk (read-json)))
    'null))

(define (update-meta path new-meta)
  (with-output-to-file path (thunk (write-json new-meta))
                       #:exists 'truncate/replace))

(define-syntax-parse-rule (define-meta NAME:id PATH:expr)
  (define NAME (read-meta-from-file-or-null PATH)))

(define (hash-update-all the-hash updater)
  (foldl (λ (k acc) (hash-update acc k updater))
         the-hash
         (hash-keys the-hash)))

(module+ test
  (define-test-suite test-hash-update-all
    (test-equal? "empty hash"
                 (hash-update-all #hash() (const 1))
                 #hash())
    (test-equal? "identity"
                 (hash-update-all #hash([a . 2] [b . 3]) identity)
                 #hash([a . 2] [b . 3]))
    (test-equal? "updater"
                 (hash-update-all #hash([a . 2] [b . 3]) add1)
                 #hash([a . 3] [b . 4])))
  (run-tests test-hash-update-all))

;; [meta] -> hash(id -> [meta])
;; meta is a hash containing keys (including id)
;; order is maintained
(define (meta-array-to-hash-by-id array)
  (hash-update-all (foldl (λ (j acc)
                            (hash-update acc (string->symbol (hash-ref j 'id)) (curry cons j) null))
                          #hash() array)
                   reverse))

(module+ test
  (define-test-suite test-to-hash-by-id
    (test-equal? "empty list"
                 (meta-array-to-hash-by-id null)
                 #hash())
    (test-equal? "singleton"
                 (meta-array-to-hash-by-id '(#hash([id . "abc"]
                                                   [meta . a])))
                 #hash([abc . (#hash([id . "abc"]
                                     [meta . a]))]))
    (test-equal? "non-duplicates"
                 (meta-array-to-hash-by-id '(#hash([id . "abc"]
                                                   [meta . a])
                                             #hash([id . "def"]
                                                   [meta . a])))
                 #hash([abc . (#hash([id . "abc"]
                                     [meta . a]))]
                       [def . (#hash([id . "def"]
                                     [meta . a]))]))
    (test-equal? "duplicates"
                 (meta-array-to-hash-by-id '(#hash([id . "abc"]
                                                   [meta . a])
                                             #hash([id . "def"]
                                                   [meta . a])
                                             #hash([id . "abc"]
                                                   [meta . b])))
                 #hash([abc . (#hash([id . "abc"]
                                     [meta . a])
                               #hash([id . "abc"]
                                     [meta . b]))]
                       [def . (#hash([id . "def"]
                                     [meta . a]))])))
  (run-tests test-to-hash-by-id))

;; keys:
;; - created (timestamp)
;; - creator (uid = str)
;; - id (str)
;; - is_archived (bool)
;; - is_general (bool)
;; - members (list uid)
;; - name (str)
;; - purpose (hash: - creator (uid)
;;                  - last_set (timestamp)
;;                  - value (str))
;; - topic (hash: - creator (uid)
;;                - last_set (timestamp)
;;                - value (str))
;;
;; we only care about purpose, name, and id (but the id is the key)
;;
;; [meta] -> meta
;; assume ordered by archive time
(define (merge-channel-meta metas)
  (define last-channel-meta (last metas))
  `#hash([id . ,(hash-ref last-channel-meta 'id)]
         [name . ,(hash-ref last-channel-meta 'name)]
         [purpose . ,(hash-ref last-channel-meta 'purpose)]))

;; keys:
;; - color (rgb str)
;; - deleted (bool)
;; - id (uid)
;; - is_admin (bool)
;; - is_app_user (bool)
;; - is_bot (bool)
;; - is_email_confirmed (bool)
;; - is_owner (bool)
;; - is_primary_owner (bool)
;; - is_restricted (bool)
;; - is_ultra_restricted (bool)
;; - name (str)
;; - profile (hash: - title (str)
;;                  - phone (str)
;;                  - skype (str)
;;                  - real_name (str)
;;                  - real_name_normalized (str)
;;                  - display_name (str)
;;                  - display_name_normalized (str)
;;                  - fields (hash: ???)
;;                  - status_text (str)
;;                  - status_emoji (str)
;;                  - status_expiration (number)
;;                  - avatar_hash (str)
;;                  - first_name (str)
;;                  - last_name (str)
;;                  - image_24 (url)
;;                  - image_32 (url)
;;                  - image_48 (url)
;;                  - image_72 (url)
;;                  - image_192 (url)
;;                  - image_512 (url)
;;                  - status_text_canonical (str)
;;                  - team (tid?))
;; - real_name (str)
;; - team_id (tid?)
;; - tz (str)
;; - tz_label (str)
;; - tz_offset (number)
;; - updated (timestamp)
;;
;; we only care about name, id (key), profile.image_32
;;
;; [meta] -> meta
;; assume ordered by archive time
(define (merge-user-meta metas)
  (define last-user-meta (last metas))
  `#hash([id . ,(hash-ref last-user-meta 'id)]
         [name . ,(hash-ref last-user-meta 'name)]
         [profile . #hash([image_32 . ,(hash-ref
                                         (hash-ref last-user-meta 'profile)
                                         'image_32)])]))

;; merger: [meta] -> meta
;; (merge-metas <merger>): hash(id -> [meta]) -> hash(id -> meta)
(define ((merge-metas merger) the-hash)
  (hash-update-all the-hash merger))

(define merge-channel-metas (merge-metas merge-channel-meta))
(define merge-user-metas (merge-metas merge-user-meta))

(module+ main

  (struct meta (make-path merger))

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
    (for ([meta metas]) (merge-meta meta meta-paths)))

  (command-line
    #:args meta-paths
    (run-main meta-paths)))