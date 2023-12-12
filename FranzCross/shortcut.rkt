#lang racket/base

(require (for-syntax racket/base
                     syntax/parse/pre))

(provide
 kbd)

(define-syntax (kbd stx)
  (syntax-parse stx
    [(_ p:id ...+ k:char)
     #'(kbd* 'p ... k)]))

(define (kbd* . args)
  (define os (system-type 'os))
  (for/list ([arg (in-list args)])
    (case arg
      [(cmd)
       (case os
         [(macosx) arg]
         [else 'ctl])]
      [(option)
       (case os
         [(macosx) arg]
         [else 'alt])]
      [(alt)
       (case os
         [(macosx) 'option]
         [else arg])]
      [else arg])))
