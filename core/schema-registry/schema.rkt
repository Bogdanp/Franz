#lang racket/base

(require noise/serde)

(provide
 (enum-out SchemaType)
 (record-out Schema))

(define-enum SchemaType
  [avro]
  [json]
  [protobuf])

(define-record Schema
  [(id #f) : (Optional UVarint)]
  [name : String]
  [(type #f) : (Optional SchemaType)]
  [(version #f) : (Optional UVarint)]
  [(schema #f) : (Optional String)])
