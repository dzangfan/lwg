#lang racket/base

(struct exn:fail:trie exn:fail () #:transparent)

(struct exn:fail:lwg-semantics exn:fail () #:transparent)

(struct exn:fail:lwg-runtime-error exn:fail () #:transparent)

(provide [struct-out exn:fail:trie]
         [struct-out exn:fail:lwg-semantics]
         [struct-out exn:fail:lwg-runtime-error])
