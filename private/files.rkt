#lang racket

(provide copy-directory/files*
         async-edit
         mkdir!
         remove-one-dir)

;; from https://github.com/racket/racket/blob/master/racket/collects/racket/file.rkt
;; with copy-file's exists-ok? (and a hack for directories)
(define (copy-directory/files* src dest
                              [exists-ok? #f]
                              #:keep-modify-seconds? [keep-modify-seconds? #f]
                              #:preserve-links? [preserve-links? #f])
  (let loop ([src src] [dest dest])
    (cond [(and preserve-links?
                (link-exists? src))
           (make-file-or-directory-link
             (resolve-path src)
             dest)]
          [(file-exists? src)
           (copy-file src dest exists-ok?)
           (when keep-modify-seconds?
             (file-or-directory-modify-seconds
               dest
               (file-or-directory-modify-seconds src)))]
          [(directory-exists? src)
           (if exists-ok?
             (with-handlers ([exn:fail:filesystem? (const (void))])
               (make-directory dest))
             (make-directory dest))
           (for-each (lambda (e)
                       (loop (build-path src e)
                             (build-path dest e)))
                     (directory-list src))]
          [else (raise-not-a-file-or-directory 'copy-directory/files src)])))

;; from https://github.com/racket/racket/blob/master/racket/collects/racket/file.rkt
;; needed by above
(define (raise-not-a-file-or-directory who path)
  (raise
   (make-exn:fail:filesystem
    (format "~a: encountered path that is neither file nor directory\n  path: ~a"
            who
            path)
    (current-continuation-marks))))

(define (async-edit editor files)
  (for/async ([filepath files])
    (define contents (file->string filepath))
    (define edited (editor contents))
    (display-to-file edited filepath #:exists 'truncate/replace)))

(define (mkdir! dir)
  (cond
    [(directory-exists? dir)
     (delete-directory/files dir)
     (make-directory dir)]
    [else (make-directory dir)]))

(define remove-one-dir
  (compose1 (curry apply build-path) rest explode-path))
