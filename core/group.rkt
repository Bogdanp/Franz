#lang racket/base

(require noise/serde)

(provide
 (record-out GroupPartitionOffset)
 (record-out GroupTopic)
 (record-out GroupOffsets))

(define-record GroupPartitionOffset
  [partition-id : UVarint]
  [high-watermark : Varint]
  [offset : Varint]
  [member-id : (Optional String)]
  [client-id : (Optional String)]
  [client-host : (Optional String)])

(define-record GroupTopic
  [name : String]
  [partitions : (Listof GroupPartitionOffset)])

(define-record GroupOffsets
  [topics : (Listof GroupTopic)])
