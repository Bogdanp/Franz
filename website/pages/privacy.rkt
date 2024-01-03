#lang racket/base

(require commonmark
         racket/match
         racket/runtime-path
         "../template.rkt")

(provide
 render)

(define-runtime-path privacy.md "privacy.md")

(define (render)
  (apply
   template
   #:title "Privacy Policy"
   (haml
    (header "Privacy Policy")
    (:section
     (.container
      (:p "Last Updated: December 1st, 2022")
      ,@(privacy-policy))))))

(define (privacy-policy)
  (define xexprs
    (document->xexprs
     (call-with-input-file privacy.md
       read-document)))
  (apply
   append
   (for/list ([e (in-list xexprs)])
     (match e
       [`(h1 ,title) `((br) (h1 ([class "title"]), title))]
       [`(h3 ,title) `((br) (h3 ,title))]
       [`(p ,content) `((br) (p ,content))]
       [e (list e)]))))
