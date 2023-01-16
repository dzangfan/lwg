#lang racket/base

(require "syntax.rkt")
(require "exn.rkt")
(require racket/contract)
(require racket/match)
(require racket/list)

(define default-value '(SINGLE-STR "NIL"))

(struct assignment (path value start-pos end-pos) #:transparent)

(define (make-assignment path [value default-value]
                         #:from start-pos #:to end-pos)
  (assignment path value start-pos end-pos))

(provide assignment make-assignment)

(define (pop-stack stack amount)
  (cond [(and (zero? amount) (pair? stack)) stack]
        [(or (null? stack) (null? (rest stack)))
         (raise
          (exn:fail:lwg-semantics "No node can be popped"
                                  (current-continuation-marks)))]
        [else (pop-stack (rest stack) (sub1 amount))]))

(define/contract (poly-graph-statement->assignment-list ast [stack null]
                                                        #:initial [initial #t])
  (->* ((struct/c AST any/c 'poly-graph-statement any/c any/c any/c))
       (list? #:initial boolean?)
       (values string? any/c))
  (match-define (list (AST _ 'node (list token-type node-name/left)
                           start-pos/node end-pos/node)
                      (AST _ 'edge-operator edge-token
                           start-pos/edge end-pos/edge)
                      graph-or-node)
    (AST-value ast))
  (define stack+#0
    (if (or initial (eq? 'PUSH+SYMBOL token-type))
        (cons node-name/left stack)
        stack))
  (define stack+
    (match edge-token
      [(list 'POP+MOVE amount) (pop-stack stack+#0 amount)]
      [_ stack+#0]))
  (define node-name
    (match edge-token
      [(list 'POP+MOVE _) (first stack+)]
      [_ node-name/left]))
  (define-values (node-name+ assignment-list+)
    (match graph-or-node
      [(AST _ 'node (list _ node-name+) start-pos end-pos)
       (values node-name+
               (list (make-assignment (list "graph" "node" node-name+)
                                      #:from start-pos #:to end-pos)))]
      [graph
       (poly-graph-statement->assignment-list graph stack+ #:initial #f)]))
  (values node-name/left
          (list* (make-assignment (list "graph" "node" node-name/left)
                                  #:from start-pos/node #:to end-pos/node)
                 (make-assignment (list "graph" "edge" node-name node-name+)
                                  #:from start-pos/edge #:to end-pos/edge)
                 assignment-list+)))

(define/contract (mono-graph-statement->assignment-list ast)
  (-> (struct/c AST any/c 'mono-graph-statement any/c any/c any/c) any/c)
  (match-let ([(list 'PUSH+SYMBOL node-name) (AST-value ast)])
    (list (make-assignment (list "graph" "node" node-name)
                           #:from (AST-start-pos ast)
                           #:to (AST-end-pos ast)))))

(define/contract (graph-statement->assignment-list ast)
  (-> (struct/c AST any/c 'graph-statement any/c any/c any/c) any/c)
  (match (AST-value ast)
    [(list (and (AST _ 'mono-graph-statement _ _ _) mono))
     (mono-graph-statement->assignment-list mono)]
    [(list (and (AST _ 'poly-graph-statement _ _ _) poly))
     (let-values ([(_ assignment-list)
                   (poly-graph-statement->assignment-list poly)])
       assignment-list)]))

(define/contract (assign-statement+->assignment-list ast prefix)
  (-> (struct/c AST any/c 'assign-statement+ any/c any/c any/c) list? any/c)
  (match (AST-value ast)
    [(list assign-statement)
     (assign-statement->assignment-list assign-statement prefix)]
    [(list assign-statement assign-statement+)
     (append (assign-statement->assignment-list assign-statement prefix)
             (assign-statement+->assignment-list assign-statement+ prefix))]))

(define/contract (assign-statement->assignment-list ast [prefix null])
  (->* ((struct/c AST any/c 'assign-statement any/c any/c any/c)) (list?) any/c)
  (match (AST-value ast)
    [(list (AST _ 'left-value (list _ name) start-pos end-pos))
     (let ([path (if (list? name) name (list name))])
       (list (make-assignment (append prefix path) default-value
                              #:from (AST-start-pos ast)
                              #:to (AST-end-pos ast))))]
    [(list left-value right-value)
     (define path
       (match (AST-value left-value)
         [(list 'SYMBOL symbol) (list symbol)]
         [(list 'PROPERTY path) path]))
     (define full-path (append prefix path))
     (match right-value
       [(AST 'branch _ (list assign-statement+) _ _)
        (assign-statement+->assignment-list assign-statement+ full-path)]
       [(AST 'leaf _ token _ _)
        (define value
          (match token
            [(list 'SYMBOL symbol) (list 'REF (list symbol))]
            [(list 'PROPERTY property) (list 'REF property)]
            [_ token]))
        (list (make-assignment full-path value
                               #:from (AST-start-pos ast)
                               #:to (AST-end-pos ast)))])]))

(define/contract (statement->assignment-list ast)
  (-> (struct/c AST any/c 'statement any/c any/c any/c) any/c)
  (match (first (AST-value ast))
    [(and (AST _ 'graph-statement _ _ _) graph-stm)
     (graph-statement->assignment-list graph-stm)]
    [(and (AST _ 'assign-statement _ _ _) assign-stm)
     (assign-statement->assignment-list assign-stm)]))

(define (normalize program-ast)
  (match (AST-value program-ast)
    [(list stm) (statement->assignment-list stm)]
    [(list stm prog)
     (append (statement->assignment-list stm)
             (normalize prog))]))

(provide
 (contract-out [normalize (-> (struct/c AST any/c 'program any/c any/c any/c)
                              list?)]))

(module+ test
  (require rackunit)
  (define (al->list assignment-list)
    (map (lambda (a) (list (assignment-path a) (assignment-value a)))
         assignment-list))
  (define (string->assignment-list/graph source)
    (let* ([program (string->AST source)]
           [graph-stm (first (AST-value (first (AST-value program))))])
      (al->list (graph-statement->assignment-list graph-stm))))
  (test-case "Single node test"
    (check-equal? (string->assignment-list/graph "^x")
                  `((("graph" "node" "x") ,default-value))))
  (test-case "Single move test"
    (check-equal? (string->assignment-list/graph "a - b")
                  `((("graph" "node" "a") ,default-value)
                    (("graph" "edge" "a" "b") ,default-value)
                    (("graph" "node" "b") ,default-value))))
  (test-case "Single pop-move test"
    (check-equal? (string->assignment-list/graph "a - ^b >> c")
                  `((("graph" "node" "a") ,default-value)
                    (("graph" "edge" "a" "b") ,default-value)
                    (("graph" "node" "b") ,default-value)
                    (("graph" "edge" "a" "c") ,default-value)
                    (("graph" "node" "c") ,default-value))))
  (define (string->assignment-list/assign source)
    (let* ([program (string->AST source)]
           [assign-stm (first (AST-value (first (AST-value program))))])
      (al->list (assign-statement->assignment-list assign-stm))))
  (test-case "Switch-style assignment test"
    (check-equal? (string->assignment-list/assign "command.exit")
                  `((("command" "exit") ,default-value)))
    (check-equal? (string->assignment-list/assign "bye")
                  `((("bye") ,default-value))))
  (test-case "Simple assignment test"
    (check-equal? (string->assignment-list/assign "a='b'")
                  '((("a") (SINGLE-STR "b")))))
  (test-case "Property assignment test"
    (check-equal? (string->assignment-list/assign "a.b='b'")
                  '((("a" "b") (SINGLE-STR "b")))))
  (test-case "Referential right-value test"
    (check-equal? (string->assignment-list/assign "1=0")
                  '((("1") (REF ("0")))))
    (check-equal? (string->assignment-list/assign "1=0.0")
                  '((("1") (REF ("0" "0"))))))
  (test-case "Textual right-value test"
    (check-equal? (string->assignment-list/assign "x='10'")
                  '((("x") (SINGLE-STR "10"))))
    (check-equal? (string->assignment-list/assign "x=\"10\"")
                  '((("x") (DOUBLE-STR "10")))))
  (test-case "Nested assignment test"
    (check-equal? (string->assignment-list/assign "point={x='0' y=point.x z}")
                  `((("point" "x") (SINGLE-STR "0"))
                    (("point" "y") (REF ("point" "x")))
                    (("point" "z") ,default-value))))
  (test-case "Complex assignment test"
    (define source
      "node.0 = { name = \"start\" style = { color = \"red\" frame = 'dot' } }")
    (check-equal? (string->assignment-list/assign source)
                  '((("node" "0" "name") (DOUBLE-STR "start"))
                    (("node" "0" "style" "color") (DOUBLE-STR "red"))
                    (("node" "0" "style" "frame") (SINGLE-STR "dot")))))
  (define (string->assignment-list source)
    (al->list (normalize (string->AST source))))
  (test-case "Program normalization test"
    (define source "a - b > ^c > d >> e
                    command.log.output = __STDOUT__
                    node.a.text = 'build'
                    edge.a.e.color = \"red\"
                    node.c = {
                      text = \"test\"
                      text.strong
                      really
                    }
                    node.e.strong = node.c.strong
                    ^f
                    node.f.text = {
                      strong big
                      __SELF__ = 'Tasks'
                    }
                    command.execute
                    EOF")
    (check-equal? (string->assignment-list source)
                  `((("graph" "node" "a") ,default-value)
                    (("graph" "edge" "a" "b") ,default-value)
                    (("graph" "node" "b") ,default-value)
                    (("graph" "edge" "a" "c") ,default-value)
                    (("graph" "node" "c") ,default-value)
                    (("graph" "edge" "c" "d") ,default-value)
                    (("graph" "node" "d") ,default-value)
                    (("graph" "edge" "a" "e") ,default-value)
                    (("graph" "node" "e") ,default-value)
                    (("command" "log" "output") (REF ("__STDOUT__")))
                    (("node" "a" "text") (SINGLE-STR "build"))
                    (("edge" "a" "e" "color") (DOUBLE-STR "red"))
                    (("node" "c" "text") (DOUBLE-STR "test"))
                    (("node" "c" "text" "strong") ,default-value)
                    (("node" "c" "really") ,default-value)
                    (("node" "e" "strong") (REF ("node" "c" "strong")))
                    (("graph" "node" "f") ,default-value)
                    (("node" "f" "text" "strong") ,default-value)
                    (("node" "f" "text" "big") ,default-value)
                    (("node" "f" "text" "__SELF__") (SINGLE-STR "Tasks"))
                    (("command" "execute") ,default-value)
                    (("EOF") ,default-value)))))
