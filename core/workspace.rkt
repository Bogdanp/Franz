#lang racket/base

(require (prefix-in k: kafka)
         (prefix-in k: kafka/iterator)
         (prefix-in kerr: kafka/private/error) ;; FIXME
         lua/value
         (only-in lua/private/table table-ht)
         noise/backend
         noise/serde
         "broker.rkt"
         "connection-details.rkt"
         "group.rkt"
         "iterator.rkt"
         "pool.rkt"
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
  (define-values (registry script records)
    (pool-get-records id max-bytes))
  (cond
    [script
     (define proc
       (table-ref script transform-key))
     (unless (procedure? proc)
       (error 'script "script.transform is not a procedure"))
     (for*/list ([r (in-vector records)]
                 [t (in-value (proc (record->table (if registry (decode-record registry r) r))))]
                 #:when (truthy? t))
       (unless (table? t)
         (error 'script "script.transform must return a table or nil~n  received: ~s" t))
       (table->IteratorRecord t))]
    [else
     (for/list ([r (in-vector records)])
       (record->IteratorRecord (if registry (decode-record registry r) r)))]))

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

;; help ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (record->IteratorRecord r)
  (IteratorRecord
   (k:record-partition-id r)
   (k:record-offset r)
   (k:record-timestamp r)
   (k:record-key r)
   (k:record-value r)
   (k:record-headers r)))

(define-syntax-rule (define-lua-keys [id k] ...)
  (begin (define id k) ...))

(define-lua-keys
  [transform-key #"transform"]
  [partition-id-key #"partition_id"]
  [offset-key #"offset"]
  [timestamp-key #"timestamp"]
  [key-key #"key"]
  [value-key #"value"]
  [headers-key #"headers"])

(define (record->table r)
  (make-table
   (cons partition-id-key (k:record-partition-id r))
   (cons offset-key (k:record-offset r))
   (cons timestamp-key (k:record-timestamp r))
   (cons key-key (or (k:record-key r) nil))
   (cons value-key (or (k:record-value r) nil))
   (cons headers-key (let ([t (make-table)])
                       (begin0 t
                         (for ([(k v) (in-hash (k:record-headers r))])
                           (table-set! t (string->bytes/utf-8 k) (or v nil))))))))

(define (table->IteratorRecord t)
  (define pid (table-ref t partition-id-key))
  (unless (exact-nonnegative-integer? pid)
    (error 'script "record partition ids must be nonnegative integers~n  received: ~e" pid))
  (define offset (table-ref t offset-key))
  (unless (exact-nonnegative-integer? offset)
    (error 'script "record offsets must be nonnegative integers~n  received: ~e" offset))
  (define timestamp (table-ref t timestamp-key))
  (unless (exact-nonnegative-integer? timestamp)
    (error 'script "record timestamps must be nonnegative integers~n  received: ~e" timestamp))
  (define key
    (let ([k (table-ref t key-key)])
      (if (nil? k) #f k)))
  (when (and key (not (bytes? key)))
    (error 'script "record keys must be either nil or strings~n  received: ~e~n" key))
  (define value
    (let ([v (table-ref t value-key)])
      (if (nil? v) #f v)))
  (when (and value (not (bytes? value)))
    (error 'script "record values must be either nil or strings~n  received: ~e~n" value))
  (define headers
    (let ([v (table-ref t headers-key)])
      (if (nil? v)
          (hash)
          (for/hash ([(k v) (in-hash (table-ht v))])
            (values (bytes->string/utf-8 k) (if (nil? v) #"" v))))))
  (IteratorRecord pid offset timestamp key value headers))

(define (truthy? v)
  (and (not (nil? v)) v))
