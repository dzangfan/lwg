
#lang racket/base

(require "../exn.rkt")
(require racket/string)

(define (shorten str #:limit [limit 8] #:suffix [suffix "..."])
  (define product
    (list->string (for/list ([_ (in-range limit)]
                             [c (in-string str)])
                    #:break (char-whitespace? c)
                    c)))
  (if (= (string-length product) (string-length str))
      product
      (string-append product suffix)))

(define (pushnew elt lst #:is-same? [is-same? equal?])
  (if (findf (lambda (elt*) (is-same? elt* elt)) lst)
      lst
      (append lst (list elt))))

(define (string->graphviz-id str)
  (define content
    (string-replace str "\"" "\\\""))
  (if (string-suffix? content "\\")
      (raise (exn:fail:lwg-runtime-error
              "Graphviz string cannot end with backslash"
              (current-continuation-marks)))
      (format "\"~A\"" content)))

(define (write-hash->graphviz-attr-list attr-table port)
  (when attr-table
    (for ([i (in-naturals)]
          [(attr value) (in-hash attr-table)])
      (if (zero? i)
          (display "\n  [ " port)
          (display "\n  ; " port))
      (fprintf port "~A = ~A"
               (string->graphviz-id attr)
               (string->graphviz-id value)))
    (display " ]" port)))

(provide shorten pushnew
         string->graphviz-id
         write-hash->graphviz-attr-list)
