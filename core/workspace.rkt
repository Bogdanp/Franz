#lang racket/base

(require noise/backend
         noise/serde
         "connection-details.rkt"
         "pool.rkt")

(define-rpc (open-workspace [with-conn conn : ConnectionDetails] : UVarint)
  (pool-open conn))

(define-rpc (list-topics [_ id : UVarint] : (Listof String))
  (list "topic-a" "topic-b"))

(define-rpc (close-all-workspaces : Bool)
  (begin0 #t
    (pool-shutdown)))
