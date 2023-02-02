#lang racket/base

(require (for-syntax racket/base
                     setup/getinfo))

(provide
 get-version)

(begin-for-syntax
  (define (info-ref id)
    ((get-info '("franz")) id)))

(define-syntax (get-version stx)
  (datum->syntax stx (info-ref 'version)))
