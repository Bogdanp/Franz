#lang racket/base

(require (for-syntax racket/base
                     racket/syntax
                     syntax/parse)
         racket/gui/easy/operator)

(provide
 define-observable-fields)

(define-syntax (define-observable-fields stx)
  (syntax-parse stx
    [(_ obs-id:id [fld-id:id accessor-expr:expr setter-expr:expr] ...+)
     #:with (set-fld-id ...) (for/list ([fld-id-stx (in-list (syntax-e #'(fld-id ...)))])
                               (format-id fld-id-stx "~a:=" fld-id-stx))
     #'(begin
         (begin
           (define fld-id (obs-id . ~> . accessor-expr))
           (define set-fld-id (λ (v) (obs-id . <~ . (λ (o) (setter-expr o v)))))) ...)]))
