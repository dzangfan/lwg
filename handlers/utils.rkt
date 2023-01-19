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

(provide shorten)
