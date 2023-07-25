#lang racket/base

(require (for-syntax racket/base
                     racket/syntax
                     syntax/parse)
         racket/gui/easy
         racket/gui/easy/operator)

(provide
 define-observables)

(define-syntax (define-observables stx)
  (syntax-parse stx
    [(_ [id:id init-expr:expr {~optional setter-expr:expr}] ...+)
     #:with (peeker-id ...) (for/list ([id-stx (in-list (syntax-e #'(id ...)))])
                              (format-id id-stx "^~a" id-stx))
     #:with (setter-id ...) (for/list ([id-stx (in-list (syntax-e #'(id ...)))])
                              (format-id id-stx "~a:=" id-stx))
     #'(begin
         (define/obs id init-expr) ...
         (define (setter-id v)
           (id . := . {~? (setter-expr v) v})) ...
         (define-syntax (peeker-id v)
           #'(obs-peek id)) ...)]))

(module+ test
  (require rackunit)

  (define-observables
    [@host "127.0.0.1"]
    [@port 80 (λ (s) (or (string->number s) 80))])

  (check-equal? ^@host "127.0.0.1")
  (check-equal? ^@port 80)
  (void (@port:= "9000"))
  (check-equal? ^@port 9000)
  (void (@port:= "blah"))
  (check-equal? ^@port 80))
