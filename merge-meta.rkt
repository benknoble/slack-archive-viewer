#lang racket

(provide (all-defined-out))

(require racket/require
         syntax/parse/define
         racket/hash
         json
         (multi-in "private" ("hash.rkt")))

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
  ;; if the channel has been renamed, we need to preserve metadata for the
  ;; renaming for the reverse-lookup by name
  (define names (set->list (apply set (map (λ (m) (hash-ref m 'name)) metas))))
  `#hash([id . ,(hash-ref last-channel-meta 'id)]
         [name . ,(hash-ref last-channel-meta 'name)]
         [names . ,names]
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

;; hash(id -> meta) -> hash(id -> meta)
(define (add-renaming-metadata the-hash)
  ;; [name -> meta]
  (define name-metas
    (for/list ([(k v) the-hash])
      ;; k id, v meta
      (define names (remove (hash-ref v 'name) (hash-ref v 'names)))
      (for/hash ([name names])
        (values (string->symbol name) (hash-set v 'name name)))))
  (apply hash-union the-hash name-metas))

(module+ test
  (define-test-suite test-add-renaming-metadata
    (test-equal? "single name"
                 (add-renaming-metadata #hash([a . #hash([id . "a"]
                                                         [name . "foo"]
                                                         [names . ("foo")])]))
                 #hash([a . #hash([id . "a"]
                                  [name . "foo"]
                                  [names . ("foo")])]))
    (test-equal? "two names"
                 (add-renaming-metadata #hash([a . #hash([id . "a"]
                                                         [name . "foo"]
                                                         [names . ("foo" "bar")])]))
                 #hash([a . #hash([id . "a"]
                                  [name . "foo"]
                                  [names . ("foo" "bar")])]
                       [bar . #hash([id . "a"]
                                    [name . "bar"]
                                    [names . ("foo" "bar")])]))
    (test-equal? "three names"
                 (add-renaming-metadata #hash([a . #hash([id . "a"]
                                                         [name . "foo"]
                                                         [names . ("foo" "bar" "baz")])]))
                 #hash([a . #hash([id . "a"]
                                  [name . "foo"]
                                  [names . ("foo" "bar" "baz")])]
                       [bar . #hash([id . "a"]
                                    [name . "bar"]
                                    [names . ("foo" "bar" "baz")])]
                       [baz . #hash([id . "a"]
                                    [name . "baz"]
                                    [names . ("foo" "bar" "baz")])] )))
  (run-tests test-add-renaming-metadata))

(define merge-channel-metas (compose1 add-renaming-metadata (merge-metas merge-channel-meta)))
(define merge-user-metas (merge-metas merge-user-meta))

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
  (for-each (λ (meta) (merge-meta meta meta-paths))
            metas))

(module+ main
  (command-line
    #:args meta-paths
    (run-main meta-paths)))
