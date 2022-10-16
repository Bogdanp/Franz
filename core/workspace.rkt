#lang racket/base

(require (prefix-in k: kafka)
         noise/backend
         noise/serde
         "broker.rkt"
         "connection-details.rkt"
         "group.rkt"
         "pool.rkt")

(define-rpc (open-workspace [with-conn conn : ConnectionDetails]
                            [and-password password : (Optional String)] : UVarint)
  (pool-open (set-ConnectionDetails-password conn password)))

(define-rpc (close-workspace [_ id : UVarint])
  (pool-close id))

(define-rpc (get-metadata [forcing-reload reload : Bool]
                          [in-workspace id : UVarint] : Metadata)
  (pool-get-metadata id reload))

(define-rpc (get-resource-configs [for-resource-named name : String]
                                  [resource-type type : Symbol]
                                  [in-workspace id : UVarint] : (Listof ResourceConfig))
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

(define-rpc (create-topic [named name : String]
                          [with-partitions partitions : UVarint]
                          [and-options options : (Listof TopicOption)]
                          [in-workspace id : UVarint] : (Optional String))
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

(define-rpc (delete-topic [named name : String]
                          [in-workspace id : UVarint])
  (pool-delete-topic id name))

(define-rpc (delete-group [named group-id : String]
                          [in-workspace id : UVarint])
  (pool-delete-group id group-id))

(define-rpc (fetch-offsets [for-group-named group-id : String]
                           [in-workspace id : UVarint] : GroupOffsets)
  (define topics
    (k:GroupOffsets-topics
     (pool-fetch-offsets id group-id)))
  (make-GroupOffsets
   #:topics (for/list ([(topic parts) (in-hash topics)])
              (make-GroupTopic
               #:name topic
               #:partitions (for/list ([part (in-list parts)])
                              (make-GroupPartitionOffset
                               #:partition-id (k:GroupPartitionOffset-id part)
                               #:offset (k:GroupPartitionOffset-offset part)))))))

(define-rpc (close-all-workspaces)
  (void (pool-shutdown)))
