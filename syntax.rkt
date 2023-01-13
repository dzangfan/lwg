#lang racket/base

(require racket/string)
(require racket/contract)
(require parser-tools/lex)
(require parser-tools/yacc)
(require (prefix-in : parser-tools/lex-sre))

(define-tokens variable-tokens
  (SYMBOL PROPERTY SINGLE-STR DOUBLE-STR POP+MOVE))

(define-empty-tokens constant-tokens
  (PUSH MOVE EQ LEFT-CURLY RIGHT-CURLY EOF))

(define-lex-abbrev symbol
  (:+ (:or (char-range #\0 #\9)
           (char-range #\a #\z)
           (char-range #\A #\Z)
           #\_)))

(define lwg-lexer
  (lexer-src-pos
   [(eof) (token-EOF)]
   [#\} (token-RIGHT-CURLY)]
   [#\{ (token-LEFT-CURLY)]
   [#\= (token-EQ)]
   [#\- (token-MOVE)]
   [#\^ (token-PUSH)]
   [whitespace (return-without-pos (lwg-lexer input-port))]
   [(:+ #\>) (token-POP+MOVE (- (string-length lexeme) 1))]
   [(:: #\" (:* (char-complement #\")) #\")
    (token-DOUBLE-STR (substring lexeme 1 (- (string-length lexeme) 1)))]
   [(:: #\' (:* (char-complement #\')) #\')
    (token-SINGLE-STR (substring lexeme 1 (- (string-length lexeme) 1)))]
   [(:: (:or symbol #\*) (:+ (:: #\. (:or symbol #\*))))
    (token-PROPERTY (string-split lexeme "."))]
   [symbol (token-SYMBOL lexeme)]))

(struct AST (node-type type value start-pos end-pos) #:transparent)
(provide
 (contract-out (struct AST
                 ((node-type (one-of/c 'leaf 'branch))
                  (type (one-of/c 'program
                                  'statement
                                  'graph-statement
                                  'assign-statement
                                  'node
                                  'edge-operator
                                  'left-value
                                  'right-value
                                  'assign-statement+))
                  (value (or/c (list/c symbol? any/c)
                               (non-empty-listof AST?)))
                  (start-pos position?)
                  (end-pos position?)))))

(define (terminal type token-type value start-pos end-pos)
  (AST 'leaf type (list token-type value) start-pos end-pos))

(define (non-terminal type subtrees start-pos end-pos)
  (define subtrees/list
    (if (list? subtrees) subtrees (list subtrees)))
  (AST 'branch type subtrees/list start-pos end-pos))

(define lwg-parser
  (parser
   (src-pos)
   (error void)
   (tokens variable-tokens constant-tokens)
   (grammar
    (program ((statement program) (non-terminal 'program
                                                (list $1 $2)
                                                $1-start-pos $2-end-pos))
             ((statement) (non-terminal 'program $1 $1-start-pos $1-end-pos)))
    (statement ((graph-statement) (non-terminal 'statement
                                                $1 $1-start-pos $1-end-pos))
               ((assign-statement) (non-terminal 'statement
                                                 $1 $1-start-pos $1-end-pos)))
    (graph-statement ((node edge-operator graph-statement)
                      (non-terminal 'graph-statement
                                    (list $1 $2 $3)
                                    $1-start-pos $3-end-pos))
                     ((node) (non-terminal 'graph-statement
                                           $1 $1-start-pos $1-end-pos)))
    (node ((SYMBOL) (terminal 'node 'SYMBOL $1 $1-start-pos $1-end-pos))
          ((PUSH SYMBOL) (terminal 'node 'PUSH+SYMBOL
                                   $2 $1-start-pos $2-end-pos)))
    (edge-operator ((MOVE) (terminal 'edge-operator 'MOVE null
                                     $1-start-pos $1-end-pos))
                   ((POP+MOVE) (terminal 'edge-operator 'POP+MOVE $1
                                         $1-start-pos $1-end-pos)))
    (assign-statement ((left-value EQ right-value)
                       (non-terminal 'assign-statement (list $1 $3)
                                     $1-start-pos $3-end-pos)))
    (left-value ((SYMBOL)
                 (terminal 'left-value 'SYMBOL $1 $1-start-pos $1-end-pos))
                ((PROPERTY)
                 (terminal 'left-value 'PROPERTY $1 $1-start-pos $1-end-pos)))
    (right-value ((SYMBOL)
                  (terminal 'right-value 'SYMBOL $1 $1-start-pos $1-end-pos))
                 ((PROPERTY)
                  (terminal 'right-value 'PROPERTY $1 $1-start-pos $1-end-pos))
                 ((SINGLE-STR)
                  (terminal 'right-value 'SINGLE-STR
                            $1 $1-start-pos $1-end-pos))
                 ((DOUBLE-STR)
                  (terminal 'right-value 'DOUBLE-STR
                            $1 $1-start-pos $1-end-pos))
                 ((LEFT-CURLY assign-statement+ RIGHT-CURLY)
                  (non-terminal 'right-value $2 $1-start-pos $3-end-pos)))
    (assign-statement+ ((assign-statement)
                        (non-terminal 'assign-statement+
                                      $1 $1-start-pos $1-end-pos))
                       ((assign-statement assign-statement+)
                        (non-terminal 'assign-statement+
                                      (list $1 $2)
                                      $1-start-pos $2-end-pos))))
   (start program)
   (end EOF)))

(module+ test
  (require rackunit)
  (require racket/match)
  (define (token->list token)
    (if (symbol? token)
        token
        (list (token-name token)
              (token-value token))))
  (define (string->tokens source)
    (let ([port (open-input-string source)])
      (let collect-next ([tokens null])
        (match (lwg-lexer port)
          [(position-token 'EOF _ _) (reverse tokens)]
          [(position-token token _ _)
           (collect-next (cons (token->list token) tokens))]))))
  (test-case "Simple graph tokenization test"
    (define source "^x-y>>>>z   \n^x -^t")
    (check-equal? (string->tokens source)
                  '(PUSH (SYMBOL "x") MOVE (SYMBOL "y")
                         (POP+MOVE 3) (SYMBOL "z")
                         PUSH (SYMBOL "x") MOVE PUSH
                         (SYMBOL "t"))))
  (test-case "Simple assignment tokenization test"
    (define source "x=10 x.y=x x={y=10 z=20}")
    (check-equal? (string->tokens source)
                  '((SYMBOL "x") EQ (SYMBOL "10") (PROPERTY ("x" "y"))
                                 EQ (SYMBOL "x") (SYMBOL "x")
                                 EQ LEFT-CURLY (SYMBOL "y")
                                 EQ (SYMBOL "10") (SYMBOL "z")
                                 EQ (SYMBOL "20") RIGHT-CURLY)))
  (test-case "Single-quoted string tokenization test"
    (define source "'hello world!\\n'")
    (check-equal? (string->tokens source)
                  '((SINGLE-STR "hello world!\\n"))))
  (test-case "Double-quoted string tokenization test"
    (define source "\"hello \\{world\\}\"")
    (check-equal? (string->tokens source)
                  '((DOUBLE-STR "hello \\{world\\}"))))
  (test-case "Properties' tokenization test"
    (define source "a.b *.a.b *.*")
    (check-equal? (string->tokens source)
                  '((PROPERTY ("a" "b"))
                    (PROPERTY ("*" "a" "b"))
                    (PROPERTY ("*" "*")))))

  (define (string->AST source)
    (let ([port (open-input-string source)])
      (lwg-parser (lambda () (lwg-lexer port)))))
  (define (AST->sexp ast)
    (match ast
      [(AST 'leaf type (list token-type token-value) _ _)
       (list type (list token-type token-value))]
      [(AST 'branch type subtrees _ _)
       (list* type (map AST->sexp subtrees))]))
  (define (string->sexp source)
    (AST->sexp (string->AST source)))
  (test-case "Simple graph-statement test"
    (define source "a - b >> c")
    (check-equal? (string->sexp source)
                  '(program
                    (statement
                     (graph-statement
                      (node (SYMBOL "a"))
                      (edge-operator (MOVE ()))
                      (graph-statement
                       (node (SYMBOL "b"))
                       (edge-operator (POP+MOVE 1))
                       (graph-statement
                        (node (SYMBOL "c")))))))))
  (test-case "Simple assign-statement test"
    (define source "node.10.name = 'ten'")
    (check-equal? (string->sexp source)
                  '(program
                    (statement
                     (assign-statement
                      (left-value (PROPERTY ("node" "10" "name")))
                      (right-value (SINGLE-STR "ten")))))))
  (test-case "Compound graph-statements test"
    (define source "a > b a > c ^b - c > d")
    (check-equal? (string->sexp source)
                  '(program
                    (statement
                     (graph-statement
                      (node (SYMBOL "a"))
                      (edge-operator (POP+MOVE 0))
                      (graph-statement (node (SYMBOL "b")))))
                    (program
                     (statement
                      (graph-statement
                       (node (SYMBOL "a"))
                       (edge-operator (POP+MOVE 0))
                       (graph-statement
                        (node (SYMBOL "c")))))
                     (program
                      (statement
                       (graph-statement
                        (node (PUSH+SYMBOL "b"))
                        (edge-operator (MOVE ()))
                        (graph-statement
                         (node (SYMBOL "c"))
                         (edge-operator (POP+MOVE 0))
                         (graph-statement
                          (node (SYMBOL "d")))))))))))
  (test-case "Compound assign-statements test"
    (define source "a = b a.b = 'a'")
    (check-equal? (string->sexp source)
                  '(program
                    (statement
                     (assign-statement
                      (left-value (SYMBOL "a"))
                      (right-value (SYMBOL "b"))))
                    (program
                     (statement
                      (assign-statement
                       (left-value (PROPERTY ("a" "b")))
                       (right-value (SINGLE-STR "a"))))))))
  (test-case "Mixed statements test"
    (define source "a - ^b >> c a = 0 b - c c.a = 'b'")
    (check-equal? (string->sexp source)
                  '(program
                    (statement
                     (graph-statement
                      (node (SYMBOL "a"))
                      (edge-operator (MOVE ()))
                      (graph-statement
                       (node (PUSH+SYMBOL "b"))
                       (edge-operator (POP+MOVE 1))
                       (graph-statement
                        (node (SYMBOL "c"))))))
                    (program
                     (statement
                      (assign-statement
                       (left-value (SYMBOL "a"))
                       (right-value (SYMBOL "0"))))
                     (program
                      (statement
                       (graph-statement
                        (node (SYMBOL "b"))
                        (edge-operator (MOVE ()))
                        (graph-statement
                         (node (SYMBOL "c")))))
                      (program
                       (statement
                        (assign-statement
                         (left-value (PROPERTY ("c" "a")))
                         (right-value (SINGLE-STR "b")))))))))))


