#lang racket/base

(require (prefix-in k: kafka)
         (prefix-in k: kafka/iterator)
         (prefix-in kerr: kafka/private/error) ;; FIXME
         noise/backend
         noise/serde
         "broker.rkt"
         "connection-details.rkt"
         "group.rkt"
         "iterator.rkt"
         "pool.rkt")

(define-rpc (open-workspace [with-conn conn : ConnectionDetails]
                            [and-password password : (Optional String)] : UVarint)
  (pool-open (set-ConnectionDetails-password conn password)))

(define-rpc (close-workspace [_ id : UVarint])
  (void (pool-close id)))

(define-rpc (get-metadata [forcing-reload reload : Bool]
                          [in-workspace id : UVarint] : Metadata)
  (pool-get-metadata id reload))

(define-rpc (get-resource-configs [for-resource-named name : String]
                                  [resource-type type : Symbol]
                                  [in-workspace id : UVarint] : (Listof ResourceConfig))
  (pool-get-resource-configs id type name))

(define-rpc (create-topic [named name : String]
                          [with-partitions partitions : UVarint]
                          [and-replication-factor replication-factor : UVarint]
                          [and-options options : (Listof TopicOption)]
                          [in-workspace id : UVarint])
  (define options*
    (for/hash ([opt (in-list options)])
      (values
       (TopicOption-key opt)
       (TopicOption-value opt))))
  (define created-topic
    (pool-create-topic id name partitions replication-factor options*))
  (define maybe-error-message
    (k:CreatedTopic-error-message created-topic))
  (when maybe-error-message
    (error 'create-topic maybe-error-message)))

(define-rpc (delete-topic [named name : String]
                          [in-workspace id : UVarint])
  (void (pool-delete-topic id name)))

(define-rpc (delete-group [named group-id : String]
                          [in-workspace id : UVarint])
  (void (pool-delete-group id group-id)))

(define-rpc (fetch-offsets [for-group-named group-id : String]
                           [in-workspace id : UVarint] : GroupOffsets)
  (pool-fetch-offsets id group-id))

(define-rpc (reset-topic-offsets [for-group-named group-id : String]
                                 [and-topic topic : String]
                                 [and-target target : Symbol]
                                 [in-workspace id : UVarint])
  (for ([(_ res) (in-hash (pool-reset-topic-offsets id group-id topic target))])
    (define err (k:CommitPartitionResult-error-code res))
    (unless (zero? err)
      (kerr:raise-server-error err))))

(define-rpc (reset-partition-offset [for-group-named group-id : String]
                                    [and-topic topic : String]
                                    [and-partition-id pid : UVarint]
                                    [and-target target : Symbol]
                                    [and-offset offset : (Optional UVarint)]
                                    [in-workspace id : UVarint])
  (for ([(_ res) (in-hash (pool-reset-partition-offset id group-id topic pid target offset))])
    (define err (k:CommitPartitionResult-error-code res))
    (unless (zero? err)
      (kerr:raise-server-error err))))

(define-rpc (open-iterator [for-topic topic : String]
                           [and-offset offset : IteratorOffset]
                           [in-workspace id : UVarint] : UVarint)
  (pool-open-iterator id topic (IteratorOffset-> offset)))

(define-rpc (get-records [_ id : UVarint]
                         [with-max-bytes max-bytes : UVarint] : (Listof IteratorRecord))
  (for/list ([r (in-vector (pool-get-records id max-bytes))])
    (IteratorRecord
     (k:record-partition-id r)
     (k:record-offset r)
     (k:record-timestamp r)
     (k:record-key r)
     (k:record-value r))))

(define-rpc (reset-iterator [with-id id : UVarint]
                            [to-offset offset : IteratorOffset])
  (pool-reset-iterator id (IteratorOffset-> offset)))

(define-rpc (close-iterator [with-id id : UVarint])
  (pool-close-iterator id))

(define-rpc (publish-record [to-topic topic : String]
                            [and-partition pid : UVarint]
                            [with-key key : (Optional String)]
                            [and-value value : (Optional String)]
                            [in-workspace id : UVarint])
  (define res (pool-publish-record id topic pid key value))
  (define err (k:ProduceResponsePartition-error-code (k:RecordResult-partition res)))
  (unless (zero? err)
    (kerr:raise-server-error err)))

(define-rpc (close-all-workspaces)
  (void (pool-shutdown)))
