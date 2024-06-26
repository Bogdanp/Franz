#lang racket/base

(require noise/serde
         racket/contract/base
         "schema-registry/schema.rkt")

(provide
 (record-out Broker)
 (record-out TopicPartition)
 (record-out Stats)
 (record-out Topic)
 (record-out Group)
 (record-out Metadata)
 (record-out TopicOption)
 (record-out ResourceConfig))

(define-record Broker
  [id : UVarint #:contract exact-nonnegative-integer?]
  [host : String]
  [port : UVarint #:contract (integer-in 1 65535)]
  [(rack #f) : (Optional String)]
  [is-controller : Bool])

(define-record TopicPartition
  [id : UVarint #:contract exact-nonnegative-integer?]
  [leader-id : Varint]
  [replica-node-ids : (Listof UVarint)]
  [in-sync-replica-node-ids : (Listof UVarint)])

(define-record Stats
  [(min-lag -1) : Varint]
  [(max-lag 0) : Varint]
  [(sum-lag 0) : Varint])

(define-record Topic
  [name : String]
  [partitions : (Listof TopicPartition)]
  [(is-internal #f) : Bool]
  [(stats (make-Stats)) : Stats])

(define-record Group
  [id : String]
  [(stats (make-Stats)) : Stats])

(define-record Metadata
  [brokers : (Listof Broker)]
  [topics : (Listof Topic)]
  [groups : (Listof Group)]
  [schemas : (Listof Schema)])

(define-record TopicOption
  [key : String]
  [value : String])

(define-record ResourceConfig
  [name : String]
  [value : (Optional String)]
  [is-read-only : Bool]
  [is-default : Bool]
  [is-sensitive : Bool]
  [(doc-url #f) : (Optional String)])
