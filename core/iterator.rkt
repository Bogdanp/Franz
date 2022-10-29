#lang racket/base

(require noise/serde)

(provide
 (enum-out IteratorOffset)
 (record-out IteratorRecord))

(define-enum IteratorOffset
  [earliest]
  [latest]
  [exact {offset : UVarint}])

(define-record IteratorRecord
  [partition-id : UVarint]
  [offset : UVarint]
  [key : (Optional Bytes)]
  [value : (Optional Bytes)])
