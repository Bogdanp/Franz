#lang racket/base

(require (prefix-in k: kafka)
         noise/backend
         noise/serde
         "broker.rkt"
         "connection-details.rkt"
         "pool.rkt")

(define-rpc (open-workspace [with-conn conn : ConnectionDetails]
                            [and-password password : (Optional String)] : UVarint)
  (pool-open (set-ConnectionDetails-password conn password)))

(define-rpc (close-workspace [with-id id : UVarint] : Bool)
  (begin0 #t
    (pool-close id)))

(define-rpc (get-metadata [_ id : UVarint] [forcing-reload reload : Bool] : Metadata)
  (pool-get-metadata id reload))

(define-rpc (get-resource-configs [with-id id : UVarint]
                                  [resource-type type : Symbol]
                                  [resource-name name : String] : (Listof ResourceConfig))
  (define resource
    (car (pool-get-resource-configs id type name)))
  (sort
   (for/list ([c (in-list (k:DescribedResource-configs resource))])
     (make-ResourceConfig
      #:name (k:ResourceConfig-name c)
      #:value (k:ResourceConfig-value c)
      #:is-read-only (k:ResourceConfig-read-only? c)
      #:is-default (k:ResourceConfig-default? c)
      #:is-sensitive (k:ResourceConfig-sensitive? c)))
   #:key ResourceConfig-name string<?))

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
