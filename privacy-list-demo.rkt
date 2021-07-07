#lang racket/base

(provide private-user-ids)

(define private-user-ids
  ;; A set of symbols, each a user-id for the user who wishes to have their
  ;; content scrubbed
  ;;
  ;; A quick way to get this ID for a particular user is the following:
  ;; 1. Unpack the data (racket unpack.rkt <your-archives>)
  ;; 2. Use jq to find the record:
  ;;    jq 'with_entries(select(.value.name | contains("<user-name>")))' _data/users.json
  ;; 3. Copy the "id" field or the key of the object; they should be the same.
  ;;
  ;; Technically, the following gets you directly to the id, which you can pipe
  ;; into a clipboard-copy program (e.g., pbcopy on macOS), but you lose the
  ;; visual verification from the above recipe:
  ;;
  ;; jq 'to_entries | map(select(.value.name | contains("knoble")) | .key)[0]' _data/users.json | tr -d '"'
  '(
    ;; benknoble
    U01GH44F3GC
    ))
