#lang racket/base

(require commonmark
         racket/match
         racket/runtime-path
         "../template.rkt")

(provide
 render)

(define-runtime-path license.md "license.md")

(define (render)
  (apply
   template
   #:title "License Agreement"
   (haml
    (header "License Agreement")
    (:section
     (.container
      (:p "Last Updated: December 1st, 2022")
      ,@(license))))))

(define (license)
  (define xexprs
    (document->xexprs
     (call-with-input-file license.md
       read-document)))
  (apply
   append
   (for/list ([e (in-list xexprs)])
     (match e
       [`(h1 ,title)
        `((br) (h1 ([class "title"]), title))]
       [`(p ,content)
        `((br) (p ,content))]
       [e (list e)]))))
