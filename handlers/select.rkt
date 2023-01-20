#lang racket/base

(require "../runtime.rkt")
(require "../lwg-runtime.rkt")
(require racket/string)

(define-handler select
  #:use [#:data selected-list #:set-data! set-selected-list!]
  #:use [#:logger/debug debug #:logger/info info]
  #:case
  [("__FILE__" "start")
   (set-selected-list! null)]
  #:case
  [("select" path0 path ...)
   (define selected-path (cons path0 path))
   (debug "+ ~A (Total: ~A)" (string-join selected-path ".")
          (length selected-list))
   (set-selected-list! (append selected-list (list selected-path)))]
  #:use [#:gen-assignment gen]
  #:case
  [("selected" suffix ... = value)
   (define count 0)
   (for ([selected-path selected-list])
     (gen (append selected-path suffix) #:= value)
     (set! count (add1 count)))
   (set-selected-list! null)
   (info "Generate ~A assignment(s); Clear selected path." count)]
  #:case
  [("clear" "selected")
   (set-selected-list! null)
   (info "Clear selected path.")])
(provide-handler select)
