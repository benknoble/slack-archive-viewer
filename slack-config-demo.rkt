#lang racket/base

(provide base-url
         title
         description
         email
         github-username
         twitter-username)

(define base-url
  ;; subpath of your site
  ;;
  ;; must start and end with a slash if provided
  "/my-slack-archive/")

(define title
  "My Slack Archive")
(define description
  ;; describe the site
  "An archive of my community")

;; contact information
(define email "example@example.com")
(define github-username "githubhandle")
(define twitter-username "twitterhandle")
