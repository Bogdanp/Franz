#lang racket/base

(require noise/serde
         racket/contract)

(provide
 (record-out Broker)
 (record-out TopicPartition)
 (record-out Topic)
 (record-out Metadata))

(define-record Broker
  [id : UVarint #:contract exact-nonnegative-integer?]
  [host : String]
  [port : UVarint #:contract (integer-in 1 65535)]
  [(rack #f) : (Optional String)])

(define-record TopicPartition
  [id : UVarint #:contract exact-nonnegative-integer?])

(define-record Topic
  [name : String #:contract string?]
  [partitions : (Listof TopicPartition)]
  [(is-internal #f) : Bool])

(define-record Metadata
  [brokers : (Listof Broker)]
  [topics : (Listof Topic)])
