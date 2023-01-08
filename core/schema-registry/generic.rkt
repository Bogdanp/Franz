#lang racket/base

(require racket/generic)

(provide
 registry?
 gen:registry
 get-schemas
 get-schema
 decode-record)

(define-generics registry
  {get-schemas registry}
  {get-schema registry name}
  {decode-record registry record})
