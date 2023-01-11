#lang racket/base
(require racket/contract)

(struct source-location (char line filename) #:transparent)
(provide
 (contract-out
  [struct source-location ((char natural-number/c)
                           (line natural-number/c)
                           (filename (or/c null? string?)))]))

(struct token (type value text location) #:transparent)
(define token-type/c
  (one-of/c 'GOTO 'THEN 'BACK-THEN
            'SYMBOL 'DOT 'EQ
            'STR-SINGLE 'STR-DOUBLE))
(provide
 (contract-out
  [struct token ((type token-type/c)
                 (value any/c)
                 (text string?)
                 (location source-location?))]))


