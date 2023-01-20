#lang racket/base

(require "../runtime.rkt")
(require "../lwg-runtime.rkt")
(require racket/string)

(struct state
  (need-clear? selected-list)
  #:transparent
  #:mutable
  #:extra-constructor-name make-state)

(define-handler select
  #:use [#:data state #:set-data! init-state!]
  #:use [#:logger/debug debug #:logger/info info]
  #:case
  [("__FILE__" "start")
   (init-state! (make-state #f null))]
  #:case
  [("select" path0 path ...)
   (when (state-need-clear? state)
     (set-state-selected-list! state null)
     (info "Clear selected path(s).")
     (set-state-need-clear?! state #f))
   (define selected-path (cons path0 path))
   (define selected-list (state-selected-list state))
   (set-state-selected-list! state
                             (append selected-list (list selected-path)))
   (debug "+ ~A (Total: ~A)" (string-join selected-path ".")
          (length selected-list))]
  #:use [#:gen-assignment gen]
  #:use [#:primitive-value value]
  #:case
  [("selected" suffix ... = _)
   (define count 0)
   (for ([selected-path (state-selected-list state)])
     (define target-path (append selected-path suffix))
     (gen '("__ORIGIN__") #:= (list 'REF target-path))
     (gen target-path #:= value)
     (set! count (add1 count)))
   (set-state-need-clear?! state #t)
   (info "Generate ~A assignment(s)" count)]
  #:case
  [("clear" "selected")
   (set-state-need-clear?! state null)
   (info "Clear selected path(s).")])
(provide-handler select)
