#lang racket/base

(require "exn.rkt")
(require "trie.rkt")
(require "normalize.rkt")
(require racket/contract)
(require racket/match)
(require parser-tools/lex)

(struct handler (name operation) #:constructor-name make-handler)

(struct runtime (trie [handlers #:mutable] data))

(define (empty-runtime)
  (runtime (empty-trie) null (make-hasheq)))

(define (add-handler! runtime handler)
  (let ([handlers (runtime-handlers runtime)])
    (set-runtime-handlers! runtime (append handlers (list handler)))))

(define (get-runtime-variable runtime path)
  (trie-ref (runtime-trie runtime) path))

(define (set-runtime-variable! runtime path new-value)
  (trie-set! (runtime-trie runtime) new-value path))

(define (get-runtime-data runtime handler-name)
  (hash-ref (runtime-data runtime) handler-name #f))

(define (set-runtime-data! runtime handler-name new-value)
  (hash-set! (runtime-data runtime) handler-name new-value))

(define (handle runtime assignment #:collect-result [collect? #t])
  (let ([result-table (and collect? (make-immutable-hasheq))])
    (for ([handler (runtime-handlers runtime)])
      (let* ([name (handler-name handler)]
             [result ((handler-operation handler) runtime assignment)])
        (and result-table (set! result-table
                                (hash-set result-table name result)))))
    (or result-table (void))))

(provide
 (contract-out [make-handler
                (-> symbol? (-> runtime? assignment? any/c) handler?)]
               [empty-runtime (-> runtime?)]
               [add-handler! (-> runtime? handler? any/c)]
               [get-runtime-variable (-> runtime? (listof string?) string?)]
               [set-runtime-variable!
                (-> runtime? (listof string?) string? any/c)]
               [get-runtime-data (-> runtime? symbol? any/c)]
               [set-runtime-data! (-> runtime? symbol? any/c any/c)]
               [handle (->* (runtime? assignment?)
                            (#:collect-result boolean?)
                            (or/c hash? void?))]))

(define runtime-service-table (make-hasheq))

(define (find-runtime-service kwd handler-name runtime assignment)
  (let ([service (hash-ref runtime-service-table kwd #f)])
    (if service
        (service handler-name runtime assignment)
        (raise (exn:fail:lwg-runtime-error
                (format "Unknown runtime service: ~A" kwd)
                (current-continuation-marks))))))

(define-syntax-rule (define-runtime-service kwd
                      (handler-name runtime assignment) body0 body ...)
  (hash-set! runtime-service-table 'kwd
             (lambda (handler-name runtime assignment) body0 body ...)))

(define-runtime-service #:data (handler-name runtime assignment)
  (get-runtime-data runtime handler-name))

(define-runtime-service #:set-data! (handler-name runtime assignment)
  (lambda (new-value)
    (set-runtime-data! runtime handler-name new-value)))

(define-runtime-service #:var (handler-name runtime assignment)
  (lambda path (get-runtime-variable runtime path)))

(define-runtime-service #:set-var! (handler-name runtime assignment)
  (lambda (value . path) (set-runtime-variable! runtime path value)))

(define-runtime-service #:start-pos (handler-name runtime assignment)
  (assignment-start-pos assignment))

(define-runtime-service #:end-pos (handler-name runtime assignment)
  (assignment-end-pos assignment))

(define-syntax-rule (define-handler id clauses ...)
  (define-handler-helper id (#:ini () #:fin () #:cas () #:use ()) clauses ...))

(define-syntax define-handler-helper
  (syntax-rules (=)
    [(_ id (#:ini (ini ...) #:fin fin #:cas cas #:use use)
        #:initially expr body ...)
     (define-handler-helper id
       (#:ini (ini ... expr) #:fin fin #:cas cas #:use use)
       body ...)]
    [(_ id (#:ini ini #:fin (fin ...) #:cas cas #:use use)
        #:finally expr body ...)
     (define-handler-helper id
       (#:ini ini #:fin (fin ... expr) #:cas cas #:use use)
       body ...)]
    [(_ id (#:ini ini #:fin fin #:cas (cas ...) #:use use)
        #:case [(path-pat path-pats ... = value-pat) expr exprs ...]
        body ...)
     (define-handler-helper id
       (#:ini ini #:fin fin #:cas (cas ... [(list (list path-pat path-pats ...)
                                                  value-pat)
                                            expr exprs ...])
        #:use use)
       body ...)]
    [(_ id collectors
        #:case [(path-pat path-pats ...) expr exprs ...]
        body ...)
     (define-handler-helper id collectors
       #:case [(path-pat path-pats ... = _) expr exprs ...]
       body ...)]
    [(_ id (#:ini ini #:fin fin #:cas cas #:use (use ...))
        #:use [kwd var] body ...)
     (define-handler-helper id
       (#:ini ini #:fin fin #:cas cas
        #:use (use ... [var kwd]))
       body ...)]
    [(_ id collectors #:use [] body ...)
     (define-handler-helper id collectors body ...)]
    [(_ id collectors #:use [kwd var rest ...] body ...)
     (define-handler-helper id collectors
       #:use [kwd var] #:use [rest ...] body ...)]
    [(_ id (#:ini (ini ...) #:fin (fin ...) #:cas (cas ...)
            #:use ([var kwd] ...)))
     (define id
       (make-handler 'id
                     (lambda (runtime assignment)
                       (define var
                         (find-runtime-service 'kwd 'id runtime assignment)) ...
                       ini ...
                       (begin0 (match (list (assignment-path assignment)
                                            (assignment-value assignment))
                                 cas ...
                                 [_ #f])
                         fin ...))))]))

(module+ test
  (require rackunit)
  (require racket/string)
  (require racket/list)
  (test-case "Primitive handler test"
    (define runtime (empty-runtime))
    (define output (open-output-string))
    (add-handler! runtime
                  (make-handler 'path-handler
                                (lambda (_ a)
                                  (write 1 output)
                                  (assignment-path a))))
    (add-handler! runtime
                  (make-handler 'value-handler
                                (lambda (_ a)
                                  (write 2 output)
                                  (assignment-value a))))
    (add-handler! runtime
                  (make-handler 'both-handler
                                (lambda (_ a)
                                  (write 3 output)
                                  (list (assignment-path a)
                                        (assignment-value a)))))
    (define sample (make-assignment '("a" "b") "c" #:from null #:to null))
    (check-pred void? (handle runtime sample #:collect-result #f))
    (check-equal? (handle runtime sample)
                  #hasheq((both-handler . (("a" "b") "c"))
                          (path-handler . ("a" "b"))
                          (value-handler . "c")))
    (check-equal? (get-output-string output)
                  "123123"))
  (test-case "define-handler: initial clause and final clause test"
    (define output (open-output-string))
    (define-handler ordered-handler
      #:initially (display 1 output)
      #:finally (display 2 output))
    (define-handler chaos-handler
      #:finally (display 4 output)
      #:initially (display 1 output)
      #:initially (display 2 output)
      #:finally (display 5 output)
      #:initially (display 3 output))
    (define runtime (empty-runtime))
    (add-handler! runtime ordered-handler)
    (add-handler! runtime chaos-handler)
    (handle runtime (make-assignment '("a") "b" #:from #f #:to #f))
    (check-equal? (get-output-string output) "1212345"))
  (test-case "define-handler: use-data clause test"
    (define output (open-output-string))
    (define-handler path-handler
      #:initially (fprintf output "~A;" prefix)
      #:finally (fprintf output "~A" prefix)
      #:case [(x ...) (cons prefix x)]
      #:use [#:data prefix])
    (define runtime (empty-runtime))
    (set-runtime-data! runtime 'path-handler "file://")
    (add-handler! runtime path-handler)
    (define result-table
      (handle runtime (make-assignment '("/opt") #:from #f #:to #f)))
    (check-equal? result-table
                  #hasheq((path-handler . ("file://" "/opt"))))
    (check-equal? (get-output-string output)
                  "file://;file://"))
  (test-case "Multiple assignments test"
    (define-handler graph-handler
      #:use [#:data graph-box]
      #:case [("graph" "node" name)
              (set-box! graph-box
                        (append (unbox graph-box)
                                (list (format "[N] ~A" name))))]
      #:case [("graph" "edge" from to)
              (set-box! graph-box
                        (append (unbox graph-box)
                                (list (format "[E] ~A -> ~A" from to))))]
      #:case [("graph" "name" = (and (regexp #px"\\w+\\s+Graph") name))
              (set-box! graph-box
                        (append (unbox graph-box)
                                (list (format "<<~A>>" name))))])
    (define-handler debug-handler
      #:use [#:data output]
      #:case [(path ... = value)
              (fprintf output "~A = ~A;" (string-join path ".") value)])
    (define runtime (empty-runtime))
    (define debug-output (open-output-string))
    (set-runtime-data! runtime 'graph-handler (box null))
    (set-runtime-data! runtime 'debug-handler debug-output)
    (add-handler! runtime graph-handler)
    (add-handler! runtime debug-handler)
    (define assignment-list
      (list (make-assignment '("graph" "name") "???? Graph")
            (make-assignment '("graph" "name") "My Graph")
            (make-assignment '("graph" "node" "a") "_")
            (make-assignment '("graph" "edge" "a" "b") "_")
            (make-assignment '("graph" "node" "b") "_")
            (make-assignment '("graph" "edge" "a" "c") "_")
            (make-assignment '("graph" "node" "c") "_")
            (make-assignment '("graph" "edge" "c" "d") "_")
            (make-assignment '("graph" "node" "d") "_")))
    (for ([assignment assignment-list])
      (handle runtime assignment #:collect-result #f))
    (check-equal? (unbox (get-runtime-data runtime 'graph-handler))
                  '("<<My Graph>>"
                    "[N] a"
                    "[E] a -> b"
                    "[N] b"
                    "[E] a -> c"
                    "[N] c"
                    "[E] c -> d"
                    "[N] d"))
    (define debug (get-output-string (get-runtime-data runtime 'debug-handler)))
    (check-equal? (string-split debug ";")
                  '("graph.name = ???? Graph"
                    "graph.name = My Graph"
                    "graph.node.a = _"
                    "graph.edge.a.b = _"
                    "graph.node.b = _"
                    "graph.edge.a.c = _"
                    "graph.node.c = _"
                    "graph.edge.c.d = _"
                    "graph.node.d = _")))
  (test-case "Service test -- data/set-data!"
    (define output (open-output-string))
    (define-handler state-handler
      #:use [#:data state #:set-data! set-state!]
      #:case
      [("state" "set" = value) (set-state! value)]
      #:case
      [("state" "show") (display state output)])
    (define runtime (empty-runtime))
    (add-handler! runtime state-handler)
    (set-runtime-data! runtime 'state-handler "1")
    (define assignment-list
      (list (make-assignment '("state" "show"))
            (make-assignment '("state" "set") "0")
            (make-assignment '("state" "show"))
            (make-assignment '("state" "show"))
            (make-assignment '("state" "set") "^")
            (make-assignment '("state" "show"))
            (make-assignment '("state" "set") "0")
            (make-assignment '("state" "show"))
            (make-assignment '("state" "set") "1")
            (make-assignment '("state" "show"))
            (make-assignment '("state" "show"))))
    (for ([assignment assignment-list])
      (handle runtime assignment))
    (check-equal? (get-output-string output) "100^011"))
  (test-case "Service test -- var/set-var!"
    (define output (open-output-string))
    (define-handler variable-handler
      #:use [#:var var #:set-var! set-var!]
      #:initially (set-var! "0" "always" "zero")
      #:case
      [("show" path ...) (fprintf output "/~A/;" (apply var path))]
      #:case
      [(path ... = value) (apply set-var! value path)])
    (define runtime (empty-runtime))
    (add-handler! runtime variable-handler)
    (define assignment-list
      (list (make-assignment '("show" "always" "zero"))
            (make-assignment '("show" "do" "not" "exist"))
            (make-assignment '("node" "text") "ROOT")
            (make-assignment '("show" "node" "text"))
            (make-assignment '("show" "node" "text" "__SELF__"))
            (make-assignment '("show" "node"))
            (make-assignment '("node" "__SELF__") "Marcus")
            (make-assignment '("show" "node"))
            (make-assignment '("show" "node" "__SELF__"))
            (make-assignment '("show" "node" "text"))))
    (for ([assignment assignment-list])
      (handle runtime assignment))
    (define result (get-output-string output))
    (check-equal? (string-split result ";")
                  '("/0/"
                    "//"
                    "/ROOT/"
                    "/ROOT/"
                    "//"
                    "/Marcus/"
                    "/Marcus/"
                    "/ROOT/")))
  (test-case "Service test -- start-pos/end-pos"
    (define-handler debug-handler
      #:use [#:start-pos start-pos #:end-pos end-pos]
      #:use [#:data pos-list #:set-data! set-pos-list!]
      #:initially (set-pos-list! (append pos-list
                                         (list (list start-pos end-pos)))))
    (define runtime (empty-runtime))
    (add-handler! runtime debug-handler)
    (set-runtime-data! runtime 'debug-handler null)
    (define position-list
      (list (list (make-position 0 0 0) (make-position 0 0 1))
            (list (make-position 0 1 0) (make-position 0 1 1))
            (list (make-position 1 0 0) (make-position 1 0 1))))
    (for ([pos (in-list position-list)])
      (handle runtime (make-assignment '("a") "b"
                                       #:from (first pos) #:to (second pos))))
    (check-equal? (get-runtime-data runtime 'debug-handler)
                  position-list)))
