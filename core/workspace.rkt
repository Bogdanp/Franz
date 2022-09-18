#lang racket/base

(require noise/backend
         noise/serde
         "connection-details.rkt"
         "pool.rkt")

(define-record Topic
  [name : String #:contract string?]
  [partitions : UVarint #:contract exact-positive-integer?])

(define-rpc (open-workspace [with-conn conn : ConnectionDetails] : UVarint)
  (pool-open conn))

(define-rpc (list-topics [_ id : UVarint] : (Listof Topic))
  (list
   (make-Topic
    #:name "Exchanges"
    #:partitions 1)
   (make-Topic
    #:name "Tokens"
    #:partitions 4)))

(define-rpc (close-all-workspaces : Bool)
  (begin0 #t
    (pool-shutdown)))
