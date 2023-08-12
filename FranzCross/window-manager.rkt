#lang racket/base

(require franz/connection-details
         (prefix-in rpc: (submod franz/workspace rpc))
         racket/class
         racket/gui/easy
         racket/lazy-require
         "keychain.rkt")

(lazy-require
 ["workspace-window.rkt" (workspace-window)])

(provide
 open-workspace
 close-workspace)

(struct workspace (details renderer))

;; workspace id -> workspace
(define workspaces (make-hasheqv))

;; ;; connection id -> workspace id
;; (define workspaces (make-hasheqv))
;; ;; workspace id -> renderer
;; (define workspace-windows (make-hasheqv))

(define (open-workspace details)
  (define connection-id
    (ConnectionDetails-id details))
  (define workspace-id
    (for/first ([w (in-hash-values workspaces)]
                #:when (= connection-id (ConnectionDetails-id (workspace-details w))))
      w))
  (cond
    [workspace-id
     (define w (hash-ref workspaces workspace-id))
     (send (renderer-root (workspace-renderer w)) show #t)]
    [else
     (define keychain (current-keychain))
     (define password (and keychain (get-password keychain (ConnectionDetails-password-id details))))
     (define the-workspace-id (rpc:open-workspace details password))
     (define the-renderer
       (render (workspace-window the-workspace-id details)))
     (hash-set! workspaces
                the-workspace-id
                (workspace details the-renderer))]))

(define (close-workspace id)
  (hash-remove! workspaces id)
  (rpc:close-workspace id))
