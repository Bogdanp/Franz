#lang racket/base

(require (prefix-in k: kafka)
         noise/backend
         noise/serde
         "broker.rkt"
         "connection-details.rkt"
         "pool.rkt")

(define-rpc (open-workspace [with-conn conn : ConnectionDetails] : UVarint)
  (pool-open conn))

(define-rpc (close-workspace [with-id id : UVarint] : Bool)
  (begin0 #t
    (pool-close id)))

(define-rpc (get-metadata [_ id : UVarint] : Metadata)
  (pool-get-metadata id))

(define-rpc (create-topic [with-id id : UVarint]
                          [named name : String]
                          [partitions partitions : UVarint]
                          [and-options options : (Listof TopicOption)] : (Optional String))
  (define options-hash
    (for/hash ([opt (in-list options)])
      (values
       (TopicOption-key opt)
       (TopicOption-value opt))))
  (define res
    (car
     (k:CreatedTopics-topics
      (pool-create-topic id name partitions options-hash))))
  (k:CreatedTopic-error-message res))

(define-rpc (delete-topic [named name : String] [for-client id : UVarint] : Bool)
  (begin0 #t
    (pool-delete-topic id name)))

(define-rpc (delete-group [with-id group-id : String] [for-client id : UVarint] : Bool)
  (begin0 #t
    (pool-delete-group id group-id)))

(define-rpc (close-all-workspaces : Bool)
  (begin0 #t
    (pool-shutdown)))
