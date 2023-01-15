#lang racket/base

(require racket/match)
(require racket/contract)
(require racket/list)
(require racket/string)
(require "exn.rkt")

(struct trie ([dict #:mutable])
  #:constructor-name make-trie)

(define (empty-trie) (make-trie null))

(provide empty-trie)

(define (trie-set!/self trie value word)
  (let ([dict (trie-dict trie)])
    (define dict+
      (remf (lambda (item) (string=? word (car item))) dict))
    (set-trie-dict! trie (cons (list word value) dict+)))
  value)

(define (exactly-trie-ref/self trie word)
  (let ([item (assoc word (trie-dict trie) string=?)])
    (and item (second item))))

(define (report-self-error)
  (raise (exn:fail:trie "__SELF__ can only be used at the end of a property"
                        (current-continuation-marks))))

(define (trie-set! trie value path)
  (define dict (trie-dict trie))
  (match path
    [(or '() (list "__SELF__"))
     (trie-set!/self trie value "__SELF__")]
    [(list "__SELF__" _ ...) (report-self-error)]
    [(list word rest-path ...)
     (let* ([maybe-trie+ (exactly-trie-ref/self trie word)]
            [trie+ (or maybe-trie+ (trie-set!/self trie (empty-trie) word))])
       (trie-set! trie+ value rest-path))]))

(define (trie-ref/all trie path)
  (define dict (trie-dict trie))
  (define (add-path path-node result)
    (map (match-lambda
           [(list path value) (list (cons path-node path) value)])
         result))
  (match path
    [(or '() (list "__SELF__"))
     (let ([item (assoc "__SELF__" dict string=?)])
       (if item (list (list null (second item))) null))]
    [(list "__SELF__" _ ...) (report-self-error)]
    [(list "*" _ ...)
     (raise (exn:fail:trie "'*' cannot be used as a right value"
                           (current-continuation-marks)))]
    [(list word rest-path ...)
     (let ([item (assoc word dict string=?)]
           [item* (assoc "*" dict string=?)])
       (append (if (not item) null
                   (add-path word (trie-ref/all (second item) rest-path)))
               (if (not item*) null
                   (add-path "*" (trie-ref/all (second item*) rest-path)))))]))

(define (select-best-match result-list)
  (match result-list
    ['() (raise (exn:fail:trie "Illegal argument: empty result list"))]
    [(list (list _ result)) result]
    [_ (define selected-result-list
         (match (group-by (lambda (result) (string=? "*" (caar result)))
                          result-list)
           [(list selected-result-list) selected-result-list]
           [(list candidate-1 candidate-2)
            (if (string=? "*" (caaar candidate-1))
                candidate-2
                candidate-1)]))
       (let ([result-list+ (map (match-lambda
                                  [(list path value) (list (cdr path) value)])
                                selected-result-list)])
         (select-best-match result-list+))]))

(define (trie-ref trie path)
  (let ([result-list (trie-ref/all trie path)])
    (if (null? result-list)
        ""
        (select-best-match result-list))))

(provide 
 (contract-out [trie-set! (-> trie? any/c (listof string?) any/c)]
               [trie-ref (-> trie? (listof string?) any/c)]))

(define (@trie trie string-path #:= [new-value #f] #:separator [sep "."])
  (let ([path (string-split string-path sep)])
    (if new-value
        (trie-set! trie new-value path)
        (trie-ref trie path))))

(provide
 (contract-out [@trie (-> trie? string?
                          #:= any/c
                          #:separator (or/c string? regexp?)
                          any/c)]))

(define (trie->list trie)
  (define (trie+self->list item)
    (match item
      [(list "__SELF__" _) item]
      [(list key trie*)
       (list key (trie->list trie*))]))
  (append-map trie+self->list (trie-dict trie)))

(module+ test
  (require rackunit)
  (test-case "Empty trie test"
    (define E (empty-trie))
    (check-equal? (@trie E "a.b.c") "")
    (check-equal? (@trie E "__SELF__") ""))
  (test-case "Simple storage test"
    (define t (empty-trie))
    (check-equal? (@trie t "a.b.c" #:= "ABC") "ABC")
    (check-equal? (@trie t "a.b" #:= "AB") "AB")
    (check-equal? (@trie t "a.b") "AB")
    (check-equal? (@trie t "a.b.c") "ABC"))
  (test-case "Simple wildcard test"
    (define t (empty-trie))
    (check-equal? (@trie t "edge.*.*.text" #:= "EDGE") "EDGE")
    (check-equal? (@trie t "edge.0.1.text") "EDGE"))
  (test-case "Conflicted wildcard test"
    (define t (empty-trie))
    (check-equal? (@trie t "edge.*.*.text" #:= "EDGE") "EDGE")
    (check-equal? (@trie t "*.0.1.text" #:= "0 = 1") "0 = 1")
    (check-equal? (@trie t "edge.*.1.text" #:= "TO 1") "TO 1")
    (check-equal? (@trie t "edge.0.*.text" #:= "FROM 0") "FROM 0")
    (check-equal? (@trie t "edge.0.1.*" #:= "NIHIL") "NIHIL")
    
    (check-equal? (@trie t "edge.0.1.text") "NIHIL")
    (check-equal? (@trie t "edge.A.B.text") "EDGE")))
