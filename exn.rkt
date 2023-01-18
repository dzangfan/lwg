#lang racket/base

(struct exn:fail:trie exn:fail () #:transparent)

(struct exn:fail:lwg-semantics exn:fail () #:transparent)

(struct exn:fail:lwg-runtime-error exn:fail () #:transparent)

(provide exn:fail:trie
         exn:fail:lwg-semantics
         exn:fail:lwg-runtime-error)
