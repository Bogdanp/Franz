#lang racket/base

(require noise/serde)

(provide
 (record-out GroupPartitionOffset)
 (record-out GroupTopic)
 (record-out GroupOffsets))

(define-record GroupPartitionOffset
  [partition-id : UVarint]
  [offset : Varint])

(define-record GroupTopic
  [name : String]
  [partitions : (Listof GroupPartitionOffset)])

(define-record GroupOffsets
  [topics : (Listof GroupTopic)])
