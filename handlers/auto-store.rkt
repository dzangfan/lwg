#lang racket/base

(require "utils.rkt")
(require "../runtime.rkt")
(require "../lwg-runtime.rkt")
(require racket/string)

(define-handler auto-store
  #:use [#:var var #:set-var! set-var!]
  #:use [#:logger/debug debug]
  #:case
  [(path ... = value)
   (define path/str (string-join path "."))
   (define old-value (apply var path))
   (unless (string=? value old-value)
     (define message
       (cond [(string=? "" value)
              (format "- ~A" path/str)]
             [(string=? "" old-value)
              (format "+ ~A: ~A" path/str (shorten value))]
             [else (format "* ~A: ~A -> ~A" path/str
                           (shorten old-value)
                           (shorten value))]))
     (apply set-var! value path)
     (debug message))])

(provide-handler auto-store)
