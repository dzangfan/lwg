#lang racket/base

(struct exn:fail:trie exn:fail () #:transparent)

(struct exn:fail:lwg-semantics exn:fail () #:transparent)

(provide exn:fail:trie
         exn:fail:lwg-semantics)
