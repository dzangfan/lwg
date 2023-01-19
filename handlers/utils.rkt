#lang racket/base

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

(provide shorten pushnew)
