#lang racket/base

(require syntax/parse/define
         (for-syntax racket/base)
         racket/runtime-path
         txexpr
         pollen/core
         pollen/tag
         pollen/file
         pollen/pagetree
         (only-in markdown parse-markdown)
         sugar
         net/url-string
         ;; must not be relative path because this file is used in a separate
         ;; pollen program
         (only-in slack-archive-viewer/private/config define-dynamic-definer)
         "nav.rkt"
         (prefix-in users: "users.rkt")
         (prefix-in channels: "channels.rkt"))

(provide (all-defined-out)
         (all-from-out sugar)
         (all-from-out "nav.rkt"))

(define (attrs-update attrs key value)
  ;; (list-set attrs (index-of attrs (assq key attrs)) (list key value))
  (hash->attrs (hash-set (attrs->hash attrs) key value)))

(define (apply-tag-function f attrs elems)
  ;; mbutterick went to a lot of work to make sure define-tag-function and
  ;; default-tag-function take attrs arguments from every possible place,
  ;; so while this keyword-apply works, so does regular apply.
  ;; It's unclear which is faster: keyword-apply does a bit of work turning
  ;; symbols into keywords, but is easier for the tag function to parse. OTOH,
  ;; the tag function does all the parsing *anyway*, so apply seems like the win
  ;; here.

  ;; (keyword-apply f
  ;;                (map (compose1 string->keyword symbol->string car) attrs)
  ;;                (map cadr attrs)
  ;;                elems)

  (apply f attrs elems))

(define-runtime-path index-tree "index.ptree")
(define-runtime-path -project-root (build-path 'same))
(define project-root (simplify-path -project-root))
(define-dynamic-definer config (build-path project-root "slack-config.rkt"))
(define base-url (config 'base-url "/"))
(define site-title (config 'title ""))
(define description (config 'description ""))
(define email (config 'email ""))
(define github-username (config 'github-username))
(define twitter-username (config 'twitter-username))

(define/caching (get-user-name user-id)
  (let ([user-id (->symbol user-id)])
    (case user-id
      ['(USLACKBOT) "SlackBot"]
      [else
        (define user (hash-ref users:meta user-id))
        (hash-ref user 'name)])))

(define/caching (get-image-link user-id)
  (let ([user-id (->symbol user-id)])
    (case user-id
      ['(USLACKBOT) "https://slack.global.ssl.fastly.net/66f9/img/slackbot_32.png"]
      [else
        (define user (hash-ref users:meta user-id))
        (define profile (hash-ref user 'profile))
        (hash-ref profile 'image_32)])))

(define/caching (get-channel-name channel-id)
  (let ([channel-id (->symbol channel-id)])
    (define channel (hash-ref channels:meta channel-id))
    (hash-ref channel 'name)))

(define/caching (get-channel-purpose channel-name)
  (let ([channel-name (->symbol channel-name)])
    (define channel (hash-ref channels:reverse channel-name))
    (define purpose (hash-ref channel 'purpose))
    (hash-ref purpose 'value)))

(define (slackify . text)
  ;; TODO the original replaces
  ;; - slack user @s
  ;; - slack channel #s
  ;; - slack urls
  ;; and escapes pipes to avoid triggering table mode
  ;; it does this with hacky regexs which we'll keep doing for now
  (regexp-replaces
    (apply string-append text)
    `([#rx"<@([^|>]*)(\\|([^>]*))?>"
       ,(λ (input user-id _ user-name)
          (define the-name (or user-name (get-user-name user-id)))
          (xexpr->html
            `(strong "@" ,the-name)))]
      [#rx"<#([^|>]*)(\\|([^>]*))?>"
       ,(λ (input channel-id _ channel-name)
          (define the-name (or channel-name (get-channel-name channel-id)))
          (xexpr->html
            `(strong ,(link (make-url (format "~a.html" the-name))
                            "#" the-name))))]
      [,(regexp (format "<(~v)\\|([^[:space:]]+)>" (object-name url-regexp)))
       ,(λ (input url link-text)
          (xexpr->html (link url link-text)))]
      ;; | -> \|
      ;; first escape the bar in the pattern (regexp)
      ;; \| -> \|
      ;; then escape the backslash in the replacelment (capturing groups!)
      ;; \| -> \\|
      ;; only _then_ double the slashes (b/c strings)
      [#rx"\\|" "\\\\|"])))

(define (markdownify . elems)
  (parse-markdown (apply slackify elems)))

(define (format-time ts)
  (define the-date
    (cond
      [(string? ts) (seconds->date (string->number ts))]
      [(number? ts) (seconds->date ts)]
      [(date? ts) ts]
      [else (raise-argument-error 'ts "timestamp-ish or date?" ts)]))
  (define (make-two-digit n)
    (format "~a~a" (if (n . < . 10) "0" "") n))
  (define years (date-year the-date))
  (define months (date-month the-date))
  (define days (date-day the-date))
  (define hours (make-two-digit (date-hour the-date)))
  (define minutes (make-two-digit (date-minute the-date)))
  (define seconds (make-two-digit (date-second the-date)))
  ;; don't need AM/PM: use a 24h display
  (format "~a-~a-~a ~a:~a:~a" years months days hours minutes seconds))

(define-tag-function (message attrs elems)
  (define user-id (attr-ref attrs 'user))
  (define user-name (get-user-name user-id))
  (define image-link (get-image-link user-id))
  (define time (format-time (attr-ref attrs 'ts)))
  `(@ ;; splice
     (div
       (img ((src ,image-link)))
       (div ((class "message"))
            (div ((class "username")) ,user-name)
            (div ((class "time")) ,time)
            (div ((class "msg")) ,@(apply markdownify elems))))
     (br)))

(define-syntax-parse-rule (default-message-function name:id)
  (define-tag-function (name attrs elems)
    (apply-tag-function message attrs elems)))

(default-message-function channel_join)
(default-message-function channel_name)
(default-message-function channel_purpose)
(default-message-function thread_broadcast)

(define-tag-function (file_share attrs elems)
  (define user-id (attr-ref attrs 'user))
  (define user-name (get-user-name user-id))
  (define image-link (get-image-link user-id))
  (define time (format-time (attr-ref attrs 'ts)))
  (define the-file (attr-ref attrs 'file))
  (define file-url (hash-ref the-file 'permalink_public "#"))
  (define file-title (hash-ref the-file 'title ""))
  (define file-comment (hash-ref (hash-ref the-file 'initial_comment #hash()) 'comment #f))
  `(@ ;; splice
     (div
       (img ((src ,image-link)))
       (div ((class "message"))
            (div ((class "username")) ,user-name)
            (div ((class "time")) ,time)
            (div ((class "msg"))
                 "Uploaded file: " ,(link file-url file-title))
            ,(if file-comment
               `(div ((class "msg")) "Comment: " ,file-comment)
               "")))
     (br)))

(define-tag-function (file_comment attrs elems)
  (define user-id (hash-ref (attr-ref attrs 'comment) 'user))
  ;; just a message with the correct user-id
  (apply-tag-function message (attrs-update attrs 'user user-id) elems))

(define-tag-function (bot_message attrs elems)
  `(div ((class "message"))
        (div ((class "msg"))
             (em "Bot messages not yet supported"))))

(define messages (default-tag-function 'div #:class "messages"))

(define (page-content . elems)
  (define title (select-from-metas 'title (current-metas)))
  `(div ((class "post"))
        ,(when/splice title
           `(header ((class "post-header"))
                    (h1 ((class "post-title")) ,title)))
        (article ((class "post-content"))
                 ,@elems)))

(define (link url #:class [class-name #f] . tx-elements)
  (let ([tx-elements (if (null? tx-elements) (list url) tx-elements)]
        [class-attr (if class-name `((class ,class-name)) empty)])
    `(a ((href ,url)
         ,@class-attr)
        ,@tx-elements)))

(define (make-url path)
  (define source-path (->string path))
  (define output-path (->string (->output-path source-path)))
  (define rel-path (regexp-replace (format "^~a" (regexp-quote (->string project-root))) output-path ""))
  (define sans-index (regexp-replace #rx"index\\.html$" rel-path ""))
  (format "~a~a" base-url sans-index))

(define (purpose)
  (define title (select-from-metas 'title (current-metas)))
  (when/splice title
    `(p ,(get-channel-purpose title))))

(define (make-channel-overview)
  (define title (select-from-metas 'title (current-metas)))
  (define date-pages
    (children (->symbol (format "~a.html" title))
              (get-pagetree index-tree)))
  (when/splice title
    `(ol ((class "channel-overview"))
         ,@(map (λ (date-page)
                  `(li ,(link (make-url date-page)
                              (->string date-page))))
                date-pages))))

(module setup racket/base
  (provide (all-defined-out))
  (define external-renderer '(beeswax/for-pollen external-renderer))
  (define (omitted-path? p)
    (or (regexp-match? #rx"\\.scss$" (path->string p))
        (and (directory-exists? p)
             (andmap omitted-path? (directory-list p #:build? #t))))))

;; vim: lw+=define-tag-function,define-dynamic-definer,define/caching,when/splice
