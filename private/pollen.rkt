#lang racket/base

(provide static-for-pollen
         channel-json->pollen-text
         pagetree->string
         ->pollen
         render
         publish)

(require racket/string
         racket/future
         racket/list
         racket/file
         racket/path
         racket/function
         racket/runtime-path
         racket/system
         pollen/pagetree
         json
         sugar
         (for-syntax racket/base)
         "json.rkt"
         "files.rkt"
         (only-in "channels.rkt" all-channel-files)
         (rename-in "channels.rkt" [channels channel-paths])
         "meta.rkt"
         (only-in "../merge-meta.rkt" users channels)
         "steps.rkt"
         "config.rkt"
         "async.rkt")

(define-runtime-path-list static-for-pollen
                          (map (λ (p) (build-path "for-pollen" p))
                               '("css"
                                 "_sass"
                                 "pollen.rkt"
                                 "template.html.rkt"
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
  (for-each/async mkdir!
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
          (format "◊(define-meta title ~v)" title)
          "◊page-content{"
          "  ◊(purpose)"
          "  ◊(make-channel-overview)}"))
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

  step "Generate nav" ;{{{
  (define nav-file (build-path "pollen" "nav.rkt"))
  (define nav
    (let ([channels (children 'channels pagetree)])
      (map (λ (c)
             `(a ((href ,(format "~a~a" (config 'base-url "/") (->string c)))
                  (class "page-link"))
                 ,(->string (path-replace-extension (file-name-from-path (->path c)) ""))))
           channels)))
  (define nav-content
    `("#lang racket/base"
      "(provide nav)"
      "(require pollen/template/html)"
      "(define nav (->html"
      ,(format "~v" nav)
      "))"))
  (display-lines-to-file nav-content nav-file)
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
  (for-each/async copy-directory/files* static-for-pollen output-static-files)
  ;}}}

  step "Copy slack-config.rkt, if present" ;{{{
  (define config-file (and (file-exists? "slack-config.rkt")
                           "slack-config.rkt"))
  (when config-file
    (copy-file config-file (build-path "pollen" config-file)))
  ;}}}

  `#hash((messages . ,output-channel-files)
         (overviews . ,output-overview-files)
         (pagetree . ,pagetree-file)
         (metas . ,output-meta-files)
         (statics . ,output-static-files)
         (nav . ,nav-file)
         (config . ,config-file)))

(define (path->pagetree-output n [ext ".html"])
  ;; assumes the top directory is the data-dir
  ;; see ->pollen parameters
  (->symbol (remove-one-dir (path-replace-extension n ext))))

(define (render [src-dir "pollen"])
  ;; I wanted to use
  ;; https://docs.racket-lang.org/raco/command.html#%28mod-path._raco%2Fall-tools%29
  ;; instead of using (system "raco pollen …")---I expect this to be slightly
  ;; faster as it avoids the overhead of separate processes/etc.
  ;; BUT in order to use pollen in parallel mode, pollen takes heavy advantage
  ;; of places, which DO NOT inherit parameters. So we fall back to system
  ;; instead.
  (define did-render
    (parameterize ([current-directory src-dir])
      (and (if (file-exists? "slack-config.rkt")
             (system "raco make slack-config.rkt")
             #t)
           (system "raco make template.html.rkt")
           (system "raco pollen render -ps index.ptree"))))
  (unless did-render
    (raise-user-error 'render "raco pollen render failed")))

(define (publish [src-dir "pollen"] [out-dir "_site"])
  (define did-publish (system (format "raco pollen publish ~a ~a" src-dir out-dir)))
  (unless did-publish
    (raise-user-error 'publish "raco pollen publish failed")))

;; vim: lw+=define-steps
