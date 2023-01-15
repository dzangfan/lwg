#lang racket/base

(require "trie.rkt")
(require "syntax.rkt")
(require "exn.rkt")
(require racket/contract)
(require racket/match)
(require racket/list)

(define default-value "nil")

(define (make-assignment path [value default-value])
  (list path value))

(define/contract (graph-statement->assignment-list ast [stack null]
                                                   #:initial [initial #t])
  (->* ((struct/c AST any/c 'graph-statement any/c any/c any/c))
       (list? #:initial boolean?)
       (values string? any/c))
  (match (AST-value ast)
    [(list node edge-operator graph+)
     (define stack+#0
       (match (AST-value node)
         [(list 'PUSH+SYMBOL node-name) (cons node-name stack)]
         [(list _ node-name) #:when initial (cons node-name stack)]
         [_ stack]))
     (define stack+
       (match (AST-value edge-operator)
         [(list 'POP+MOVE amount)
          (let drop ([lst stack+#0] [num amount])
            (cond [(zero? num) lst]
                  [(or (null? lst) (null? (rest lst)))
                   (raise
                    (exn:fail:lwg-semantics "No node can be popped"
                                            (current-continuation-marks)))]
                  [else (drop (rest lst) (sub1 num))]))]
         [_ stack+#0]))
     (define-values (node-name+ assignment-list+)
       (graph-statement->assignment-list graph+ stack+ #:initial #f))
     (define node-name/left
       (second (AST-value node)))
     (define node-name
       (match (AST-value edge-operator)
         [(list 'POP+MOVE _) (first stack+)]
         [_ node-name/left]))
     (values node-name/left
             (list* (make-assignment (list "graph" "node" node-name/left))
                    (make-assignment (list "graph" "edge" node-name node-name+))
                    assignment-list+))]
    [(list (AST _ 'node (list _ node-name) _ _))
     (values node-name
             (list (make-assignment (list "graph" "node" node-name))))]))

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
    [(list 'PROPERTY path)
     (list (make-assignment (append prefix path) default-value))]
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
        (list (make-assignment full-path value))])]))

(module+ test
  (require rackunit)
  (define (string->assignment-list/graph source)
    (let* ([program (string->AST source)]
           [graph-stm (first (AST-value (first (AST-value program))))])
      (let-values ([(_ a-lst)
                    (graph-statement->assignment-list graph-stm)])
        a-lst)))
  (test-case "Single node test"
    (check-equal? (string->assignment-list/graph "x")
                  '((("graph" "node" "x") "nil"))))
  (test-case "Single move test"
    (check-equal? (string->assignment-list/graph "a - b")
                  '((("graph" "node" "a") "nil")
                    (("graph" "edge" "a" "b") "nil")
                    (("graph" "node" "b") "nil"))))
  (test-case "Single pop-move test"
    (check-equal? (string->assignment-list/graph "a - ^b >> c")
                  '((("graph" "node" "a") "nil")
                    (("graph" "edge" "a" "b") "nil")
                    (("graph" "node" "b") "nil")
                    (("graph" "edge" "a" "c") "nil")
                    (("graph" "node" "c") "nil"))))
  (define (string->assignment-list/assign source)
    (let* ([program (string->AST source)]
           [assign-stm (first (AST-value (first (AST-value program))))])
      (assign-statement->assignment-list assign-stm)))
  (test-case "Switch-style assignment test"
    (check-equal? (string->assignment-list/assign "command.exit")
                  '((("command" "exit") "nil"))))
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
    (check-equal? (string->assignment-list/assign "point={x='0' y=point.x}")
                  '((("point" "x") (SINGLE-STR "0"))
                    (("point" "y") (REF ("point" "x"))))))
  (test-case "Complex assignment test"
    (define source
      "node.0 = { name = \"start\" style = { color = \"red\" frame = 'dot' } }")
    (check-equal? (string->assignment-list/assign source)
                  '((("node" "0" "name") (DOUBLE-STR "start"))
                    (("node" "0" "style" "color") (DOUBLE-STR "red"))
                    (("node" "0" "style" "frame") (SINGLE-STR "dot"))))))
