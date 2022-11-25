#lang racket/base

(require noise/serde)

(provide
 IteratorOffset->
 (enum-out IteratorOffset)
 (record-out IteratorRecord))

(define-enum IteratorOffset
  [earliest]
  [latest]
  [timestamp {timestamp : UVarint}]
  [exact {offset : UVarint}])

(define-record IteratorRecord
  [partition-id : UVarint]
  [offset : UVarint]
  [timestamp : UVarint]
  [key : (Optional Bytes)]
  [value : (Optional Bytes)])

(define (IteratorOffset-> io)
  (cond
    [(IteratorOffset.earliest? io)
     'earliest]
    [(IteratorOffset.latest? io)
     'latest]
    [(IteratorOffset.timestamp? io)
     (list 'timestamp (IteratorOffset.timestamp-timestamp io))]
    [(IteratorOffset.exact? io)
     (list 'exact (IteratorOffset.exact-offset io))]
    [else
     (raise-argument-error 'IteratorOffset-> "IteratorOffset?" io)]))
