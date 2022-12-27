#lang racket/base

(require racket/generic)

(provide
 registry?
 gen:registry
 decode-record)

(define-generics registry
  {decode-record registry record})
