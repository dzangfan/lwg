#lang racket/base

(require racket/string)
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
   [(:: symbol (:+ (:: #\. symbol)))
    (token-PROPERTY (string-split lexeme "."))]
   [symbol (token-SYMBOL lexeme)]))

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
                  '((DOUBLE-STR "hello \\{world\\}")))))


