#lang racket/base

(require franz/connection-details
         racket/class
         racket/gui/easy
         racket/lazy-require)

(lazy-require
 ["workspace-window.rkt" (workspace-window)])

(provide
 open-workspace
 remove-workspace)

;; connection id -> renderer
(define workspace-windows (make-hasheqv))

(define (open-workspace details)
  (define id
    (ConnectionDetails-id details))
  (define workspace-renderer
    (hash-ref workspace-windows id #f))
  (cond
    [workspace-renderer
     (send (renderer-root workspace-renderer) show #t)]
    [else
     (define the-renderer
       (render (workspace-window details)))
     (hash-set! workspace-windows id the-renderer)]))

(define (remove-workspace id)
  (hash-remove! workspace-windows id))
