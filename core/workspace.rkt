#lang racket/base

(require noise/backend
         noise/serde
         "broker.rkt"
         "connection-details.rkt"
         "pool.rkt")

(define-rpc (open-workspace [with-conn conn : ConnectionDetails] : UVarint)
  (pool-open conn))

(define-rpc (get-metadata [_ id : UVarint] : Metadata)
  (pool-get-metadata id))

(define-rpc (close-all-workspaces : Bool)
  (begin0 #t
    (pool-shutdown)))
