#lang racket/base

(require noise/serde)

(provide
 IteratorOffset->
 (enum-out IteratorOffset)
 (record-out IteratorRecord)
 (enum-out IteratorResult))

(define-enum IteratorOffset
  [earliest]
  [latest]
  [recent {count : UVarint}]
  [timestamp {timestamp : UVarint}]
  [exact {offset : UVarint}])

(define-record IteratorRecord
  [partition-id : UVarint]
  [offset : UVarint]
  [timestamp : UVarint]
  [key : (Optional Bytes)]
  [value : (Optional Bytes)]
  [headers : (HashTable String Bytes)])

(define-enum IteratorResult
  [original {record : IteratorRecord}]
  [transformed {record : IteratorRecord}
               {original : IteratorRecord}])

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
    [(IteratorOffset.recent? io)
     (list 'recent (IteratorOffset.recent-count io))]
    [else
     (raise-argument-error 'IteratorOffset-> "IteratorOffset?" io)]))
