#lang racket/base

(require (for-syntax racket/base
                     racket/syntax
                     syntax/parse)
         racket/gui/easy
         racket/gui/easy/operator)

(provide
 define-observables
 let-observable)

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

(define-syntax (let-observable stx)
  (syntax-parse stx
    [(_ ([id:id obs-expr:expr]) body ...+)
     #'(obs-expr . ~> . (λ (id) body ...))]
    [(_ ([id:id obs-expr:expr] ...+) body ...+)
     #'(obs-combine
        (lambda (id ...)
          body ...)
        obs-expr ...)]))

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
  (check-equal? ^@port 80)

  (check-equal?
   (obs-peek
    (let-observable ([port @port])
      (add1 port)))
   (add1 ^@port)))
