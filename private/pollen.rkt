#lang racket/base

(provide static-for-pollen
         channel-json->pollen-text
         pagetree->string
         ->pollen)

(require racket/string
         racket/future
         racket/list
         racket/file
         racket/path
         racket/function
         racket/runtime-path
         json
         sugar
         (for-syntax racket/base)
         "json.rkt"
         "files.rkt"
         (only-in "channels.rkt" all-channel-files)
         (rename-in "channels.rkt" [channels channel-paths])
         "meta.rkt"
         (only-in "../merge-meta.rkt" users channels)
         "steps.rkt")

(define-runtime-path-list static-for-pollen
                          (map (λ (p) (build-path "for-pollen" p))
                               '("css"
                                 "_sass"
                                 "pollen.rkt"
                                 "template.html.p"
                                 "index.html.pm"
                                 )))

(define (make-indent indent)
  (build-string indent (const #\space)))

;; pagetree? -> string?
(define (pagetree->string tree [indent 0])
  (string-join
    (map (curryr pagetree-subtree->string indent) tree)
    "\n"))

(define (pagetree-subtree->string subtree [indent 0])
  (cond
    [(list? subtree)
     (format "~a◊~a{~n~a}"
             (make-indent indent)
             (first subtree)
             (pagetree->string (rest subtree) (+ indent 2)))]
    [else (format "~a~a" (make-indent indent) (->string subtree))]))

(define (channel-json->pollen-text channel)
  `("#lang pollen"
    "◊page-content{"
    "  ◊messages{" ,@(map message-json->pollen-text channel) "}}"))

(define (message-json->pollen-text message)
  (format "◊~a[~a]{~a}"
          (or (hash-ref message 'subtype #f) "message")
          (string-join (hash-map message (curry format "#:~a ~v")) " ")
          (hash-ref message 'text "")))

(define-steps (->pollen data-dir)
  step "Make pollen directory tree" ;{{{
  (mkdir! "pollen")
  (define the-channel-paths (channel-paths data-dir))
  (for-each mkdir!
            (map (compose1 (curry build-path "pollen") file-name-from-path)
                 the-channel-paths))
  ;}}}

  step "Convert message files to pollen" ;{{{
  (define channel-file->pollen (compose1 channel-json->pollen-text file->json))
  (define channel-files (all-channel-files data-dir))
  (define output-channel-files
    (map (λ (p)
           (define with-pollen-ext (path-replace-extension p ".html.pm"))
           (define dir+file (take-right (explode-path with-pollen-ext) 2))
           (define in-pollen (cons "pollen" dir+file))
           (path->complete-path (apply build-path in-pollen)))
         channel-files))
  (for/async ([channel-file channel-files]
              [output-file output-channel-files])
    (define pollen-text (channel-file->pollen channel-file))
    (display-lines-to-file pollen-text output-file))
  ;}}}

  step "Generate channel overview files" ;{{{
  (define output-overview-files
    (map (compose1 (curry build-path "pollen")
                   ->path
                   (curryr path->pagetree-output ".html.pm"))
         the-channel-paths))
  (define (make-overview-content channel-path)
    (define title (->string (path-replace-extension (file-name-from-path channel-path) "")))
    (list "#lang pollen"

          (format "◊(define title ~v)" title)
          "◊(define-meta title title)"

          "◊page-content{"
          "  ◊purpose{◊(get-channel-purpose title)}"

          "  ◊(define date-pages"
          "    (children (->symbol (format \"~a.html\" title))))"

          "  ◊ol[#:class \"channel-overview\"]{"
          "    ◊(map (λ (date-page)"
          "            ◊li{◊(link (format \"~a/~a\" title date-page)"
          "                       (-> string date-page))})"
          "          date-pages)}}"))
  (for/async ([channel the-channel-paths]
              [output-overview-file output-overview-files])
    (display-lines-to-file (make-overview-content channel) output-overview-file))
  ;}}}

  step "Generate pagetree" ;{{{
  (define pagetree
    ;; validated when pollen renders
    `(index.html
       (channels
         ,@(map (λ (channel-path)
                  (cons (path->pagetree-output channel-path)
                        (map path->pagetree-output
                             (directory-list #:build? #t channel-path))))
                the-channel-paths))))
  (define pagetree-content
    (string-append "#lang pollen\n" (pagetree->string pagetree)))
  (define pagetree-file (build-path "pollen" "index.ptree"))
  (display-to-file pagetree-content pagetree-file)
  ;}}}

  step "Convert metadata to jsond" ;{{{
  (define meta-files (list ((meta-info-make-path channels) data-dir)
                           ((meta-info-make-path users) data-dir)))
  (define output-meta-files
    (map (λ (p)
           (define with-racket-ext (path-replace-extension p ".rkt"))
           (define bare-name (file-name-from-path with-racket-ext))
           (define in-pollen (list "pollen" bare-name))
           (path->complete-path (apply build-path in-pollen)))
         meta-files))
  (define (make-reverse-lookup jsdict)
    (for/hash ([(k v) (in-hash jsdict)])
      (values (string->symbol (hash-ref v 'name)) v)))
  (for/async ([meta-file meta-files]
              [output-meta-file output-meta-files])
    (define original-text (file->lines meta-file))
    (define as-meta-text (cons "#:name meta" original-text))
    (define reverse-lookup-text
      (list "#:name reverse"
            (jsexpr->string (make-reverse-lookup (string->jsexpr (string-join original-text))))))
    (define jsond-text (append '("#lang jsond") as-meta-text reverse-lookup-text))
    (display-lines-to-file jsond-text output-meta-file))
  ;}}}

  step "Copy static files" ;{{{
  (define output-static-files
    (map (λ (static-file-or-dir)
           (build-path "pollen" (file-name-from-path static-file-or-dir)))
         static-for-pollen))
  (for/async ([static-file-or-dir static-for-pollen]
              [output-static-file output-static-files])
    (copy-directory/files* static-file-or-dir output-static-file))
  ;}}}

  `#hash((messages . ,output-channel-files)
         (overviews . ,output-overview-files)
         (pagetree . ,pagetree-file)
         (metas . ,output-meta-files)
         (statics . ,output-static-files)))

(define (path->pagetree-output n [ext ".html"])
  ;; assumes the top directory is the data-dir
  ;; see ->pollen parameters
  (->symbol (remove-one-dir (path-replace-extension n ext))))

;; vim: lw+=define-steps
