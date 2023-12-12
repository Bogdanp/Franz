#lang racket/base

(require (for-syntax racket/base
                     racket/port
                     racket/string
                     racket/syntax
                     syntax/parse/pre))

(begin-for-syntax
  (define here (simplify-path (build-path (syntax-source #'here) 'up))))

(define-syntax (define-secret-transformer stx)
  (syntax-parse stx
    [(_ id:id filename:string)
     #'(define-syntax (id stx)
         (datum->syntax
          stx
          (call-with-input-file (build-path here filename)
            (lambda (in)
              (string-trim (port->string in))))))]))

(define-syntax (define-secrets stx)
  (syntax-parse stx
    [(_ [id:id filename:string] ...+)
     #:with (get-secret-id ...) (for/list ([id-stx (in-list (syntax-e #'(id ...)))])
                                  (format-id id-stx "get-~a" id-stx))
     #'(begin
         (begin
           (define-secret-transformer get-secret-id filename)
           (define id (get-secret-id))
           (provide id)) ...)]))

(define-secrets
  [license-secret "secrets/license-secret.txt"])
