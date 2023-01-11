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
           #\-)))

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
