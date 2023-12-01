#lang racket/base

(require noise/backend
         noise/serde
         racket/lazy-require)

(lazy-require
 ["pool.rkt" (pool-get-breadcrumbs)])

(provide
 (record-out Breadcrumb))

(define-record Breadcrumb
  [(timestamp (current-seconds)) : UVarint]
  [level : Symbol]
  [message : String]
  [details : (Optional String)])

(define-rpc (get-breadcrumbs : (Listof Breadcrumb))
  (pool-get-breadcrumbs))
