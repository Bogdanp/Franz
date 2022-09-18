#lang racket/base

(require noise/serde)

(provide
 (record-out Topic))

(define-record Topic
  [name : String #:contract string?]
  [partitions : UVarint #:contract exact-positive-integer?])
