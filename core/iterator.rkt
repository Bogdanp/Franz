#lang racket/base

(require noise/serde)

(provide
 IteratorOffset->
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

(define (IteratorOffset-> io)
  (cond
    [(IteratorOffset.earliest? io) 'earliest]
    [(IteratorOffset.latest? io) 'latest]
    [(IteratorOffset.exact? io)
     (IteratorOffset.exact-offset io)]
    [else
     (raise-argument-error 'IteratorOffset-> "IteratorOffset?" io)]))
