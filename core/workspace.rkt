#lang racket/base

(require noise/backend
         noise/serde
         "connection-details.rkt"
         "pool.rkt")

(define-rpc (open-workspace [with-conn conn : ConnectionDetails] : UVarint)
  (pool-open conn))

(define-rpc (close-all-workspaces : Bool)
  (begin0 #t
    (pool-shutdown)))
