#lang racket/base

(require racket/generic)

(provide
 registry?
 gen:registry
 get-schemas
 get-schema
 check-schema
 create-schema
 delete-schema
 decode-record)

(define-generics registry
  {get-schemas registry}
  {get-schema registry name}
  {check-schema registry name schema}
  {create-schema registry name type schema}
  {delete-schema registry name}
  {decode-record registry record})
