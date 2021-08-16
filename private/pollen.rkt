#lang racket/base

(provide static-for-pollen
         channel-json->pollen-text
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

(define (channel-json->pollen-text channel)
  (cons "#lang pollen" (map message-json->pollen-text channel)))

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

  step "Convert channel files to pollen" ;{{{
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

  step "Generate pagetree" ;{{{
  ;; TODO extract
  (define remove-one-dir
    (compose1 (curry apply build-path) rest explode-path))
  (define (path->ptree-output n)
    (->symbol (remove-one-dir (path-replace-extension n ".html"))))
  (define page-tree
    ;; validated when pollen renders
    `(index.html
       (channels
         ,@(for/list ([channel-path the-channel-paths])
             `(,(path->ptree-output channel-path)
                ,@(for/list ([channel-file-path (directory-list #:build? #t channel-path)])
                    (path->ptree-output channel-file-path)))))))
  (define (page-tree->string tree [indent 0])
    (define indent-str (build-string indent (const #\space)))
    (string-join
      (map (λ (node)
             (cond
               [(list? node)
                (format "~a◊~a{~n~a}"
                        indent-str
                        (first node)
                        (page-tree->string (rest node) (+ indent 2)))]
               [else (format "~a~a" indent-str (->string node))]))
           tree)
      "\n"))
  (define pagetree-content
    (string-append "#lang pollen\n" (page-tree->string page-tree)))
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

  `#hash((channels . ,output-channel-files)
         (pagetree . ,pagetree-file)
         (metas . ,output-meta-files)
         (statics . ,output-static-files)))

;; vim: lw+=define-steps
