#lang racket/base

(require (prefix-in k: kafka/iterator)
         lua/value
         (only-in lua/private/table table-ht)
         "iterator.rkt")

(provide
 record->IteratorRecord
 record->table
 table->IteratorRecord
 IteratorRecord->table)

(define (record->IteratorRecord r)
  (IteratorRecord
   (k:record-partition-id r)
   (k:record-offset r)
   (k:record-timestamp r)
   (k:record-key r)
   (k:record-value r)
   (k:record-headers r)))

(define (record->table r)
  (make-table
   (cons #"partition_id" (k:record-partition-id r))
   (cons #"offset" (k:record-offset r))
   (cons #"timestamp" (k:record-timestamp r))
   (cons #"key" (or (k:record-key r) nil))
   (cons #"value" (or (k:record-value r) nil))
   (cons #"headers" (make-headers-table (k:record-headers r)))))

(define (table->IteratorRecord t)
  (define pid (table-ref t #"partition_id"))
  (unless (exact-nonnegative-integer? pid)
    (error 'script "record partition ids must be nonnegative integers~n  received: ~e" pid))
  (define offset (table-ref t #"offset"))
  (unless (exact-nonnegative-integer? offset)
    (error 'script "record offsets must be nonnegative integers~n  received: ~e" offset))
  (define timestamp (table-ref t #"timestamp"))
  (unless (exact-nonnegative-integer? timestamp)
    (error 'script "record timestamps must be nonnegative integers~n  received: ~e" timestamp))
  (define key
    (let ([k (table-ref t #"key")])
      (if (nil? k) #f k)))
  (when (and key (not (bytes? key)))
    (error 'script "record keys must be either nil or strings~n  received: ~e~n" key))
  (define value
    (let ([v (table-ref t #"value")])
      (if (nil? v) #f v)))
  (when (and value (not (bytes? value)))
    (error 'script "record values must be either nil or strings~n  received: ~e~n" value))
  (define headers
    (let ([v (table-ref t #"headers")])
      (if (nil? v)
          (hash)
          (for/hash ([(k v) (in-hash (table-ht v))])
            (values (bytes->string/utf-8 k) (if (nil? v) #"" v))))))
  (IteratorRecord pid offset timestamp key value headers))

(define (IteratorRecord->table r)
  (make-table
   (cons #"partition_id" (IteratorRecord-partition-id r))
   (cons #"offset" (IteratorRecord-offset r))
   (cons #"timestamp" (IteratorRecord-timestamp r))
   (cons #"key" (or (IteratorRecord-key r) nil))
   (cons #"value" (or (IteratorRecord-value r) nil))
   (cons #"headers" (make-headers-table (IteratorRecord-headers r)))))

(define (make-headers-table ht)
  (define t (make-table))
  (begin0 t
    (for ([(k v) (in-hash ht)])
      (table-set! t (string->bytes/utf-8 k) (or v nil)))))
