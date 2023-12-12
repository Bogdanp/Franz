#lang racket/base

(require (for-syntax racket/base
                     syntax/parse/pre)
         db
         racket/port
         racket/runtime-path)

(provide
 (for-syntax #%datum)
 define-query-definer)

(define (open-virtual-statement path)
  (call-with-input-file path
    (compose1 virtual-statement port->string)))

(begin-for-syntax
  (define-syntax-class binder
    (pattern [id:id filename:string])
    (pattern id:id
             #:with filename (let ([filename (format "~a.sql" (symbol->string (syntax-e #'id)))])
                               (datum->syntax #'id filename)))))

;; define-runtime-path computes the module-relative path by the source
;; location of the application of the form (i.e. the parens), so we
;; have to take care to make sure those have the context of the
;; calling module.
(define-syntax (define-query-definer stx)
  (syntax-parse stx
    [(_ definer-id:id root:expr)
     #:with d-r-p (datum->syntax #'definer-id `(,#'define-runtime-path ,#'root-path ,#'root))
     #'(begin
         d-r-p
         (define-syntax (definer-id stx)
           (syntax-parse stx
             [(_ q:binder ...+)
              #'(begin
                  (define q.id
                    (open-virtual-statement (build-path root-path q.filename)))
                  (... ...))])))]))
