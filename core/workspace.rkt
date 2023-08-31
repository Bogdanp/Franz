#lang racket/base

(require (prefix-in k: kafka)
         (prefix-in kerr: kafka/private/error) ;; FIXME
         lua/value
         noise/backend
         noise/serde
         "broker.rkt"
         "connection-details.rkt"
         "group.rkt"
         "iterator.rkt"
         "pool.rkt"
         "record.rkt"
         "schema-registry/generic.rkt")

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

(define-rpc (find-topic-groups [forTopic topic : String]
                               [in-workspace id : UVarint] : (Listof String))
  (pool-find-topic-groups id topic))

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
                         [with-max-bytes max-bytes : UVarint] : (Listof IteratorResult))
  (define-values (registry script records)
    (pool-get-records id max-bytes))
  (cond
    [script
     (define proc (table-ref script #"transform"))
     (unless (procedure? proc)
       (error 'get-records "script.transform is not a procedure"))
     (for*/list ([r (in-vector records)]
                 [o (in-value (if registry (decode-record registry r) r))]
                 [t (in-value (proc (record->table o)))]
                 #:unless (nil? t))
       (unless (table? t)
         (error 'get-records "script.transform must return a table or nil~n  received: ~s" t))
       (IteratorResult.transformed (table->IteratorRecord t) o))]
    [else
     (for/list ([r (in-vector records)])
       (IteratorResult.original
        (record->IteratorRecord (if registry (decode-record registry r) r))))]))

(define-rpc (reset-iterator [with-id id : UVarint]
                            [to-offset offset : IteratorOffset])
  (pool-reset-iterator id (IteratorOffset-> offset)))

(define-rpc (close-iterator [with-id id : UVarint])
  (pool-close-iterator id))

(define-rpc (publish-record [to-topic topic : String]
                            [and-partition pid : UVarint]
                            [with-key key : (Optional Bytes)]
                            [and-value value : (Optional Bytes)]
                            [in-workspace id : UVarint] : IteratorRecord)
  (define timestamp (current-milliseconds))
  (define res (pool-publish-record id topic pid key value))
  (define part (k:RecordResult-partition res))
  (define err (k:ProduceResponsePartition-error-code part))
  (unless (zero? err)
    (kerr:raise-server-error err))
  (make-IteratorRecord
   #:partition-id (k:ProduceResponsePartition-id part)
   #:offset (k:ProduceResponsePartition-offset part)
   #:timestamp timestamp
   #:key key
   #:value value
   #:headers (hash)))

(define-rpc (close-all-workspaces)
  (void (pool-shutdown)))

(module+ test
  (require rackunit
           rackunit/text-ui)

  (define test-details
    (make-ConnectionDetails
     #:name "Integration Tests"
     #:bootstrap-host "127.0.0.1"
     #:bootstrap-port 9092
     #:use-ssl #f))

  (define integration-tests
    (test-suite
     "workspace integration"
     #:before (λ () (current-pool (make-pool)))
     #:after  (λ () (close-all-workspaces))

     (test-case "publish record"
       (define id
         (open-workspace test-details #f))
       (define t "workspace-publish-test")
       (test-begin
         (create-topic t 2 1 (list (TopicOption "cleanup.policy" "compact")) id)
         (publish-record t 0 #"k" #"v" id))
       (delete-topic t id)
       (close-workspace id))

     (test-case "read record"
       (define id
         (open-workspace test-details #f))
       (define t "workspace-read-test")
       (test-begin
         (create-topic t 2 1 null id)
         (publish-record t 0 #"k" #"v" id))
       (test-begin
         (define it (open-iterator t (IteratorOffset.earliest) id))
         (define recs (map IteratorResult.original-record (get-records it (* 1024 1024))))
         (check-equal? (length recs) 1)
         (check-equal? (IteratorRecord-key (car recs)) #"k")
         (check-equal? (IteratorRecord-value (car recs)) #"v"))
       (delete-topic t id)
       (close-workspace id))))

  (when (equal? (getenv "FRANZ_INTEGRATION_TESTS") "x")
    (run-tests integration-tests)))
