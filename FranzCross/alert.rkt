#lang racket/gui/easy

(require "window-manager.rkt")

(provide
 alert
 confirm)

(define (alert title message
               #:renderer [renderer (get-welcome-renderer)])
  (gui:message-box
   title
   message
   (renderer-root renderer)
   '(ok caution)))

(define (confirm #:title title
                 #:message message
                 #:action-label [action-label "Delete"]
                 #:cancel-label [cancel-label "Cancel"]
                 #:renderer [renderer (get-welcome-renderer)])
  (define res
    (gui:message-box/custom
     title
     message
     action-label
     cancel-label
     #f
     (renderer-root renderer)
     '(caution default=2)))
  (eqv? res 1))
