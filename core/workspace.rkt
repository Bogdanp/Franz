#lang racket/base

(require (prefix-in k: kafka)
         noise/backend
         noise/serde
         "connection-details.rkt"
         "pool.rkt"
         "topic.rkt")

(define-rpc (open-workspace [with-conn conn : ConnectionDetails] : UVarint)
  (pool-open conn))

(define-rpc (list-topics [_ id : UVarint] : (Listof Topic))
  (sort (pool-topics id) string<? #:key Topic-name))

(define-rpc (close-all-workspaces : Bool)
  (begin0 #t
    (pool-shutdown)))
