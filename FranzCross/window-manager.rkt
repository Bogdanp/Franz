#lang racket/gui/easy

(require franz/connection-details
         (prefix-in rpc: (submod franz/connection-details rpc))
         (prefix-in rpc: (submod franz/workspace rpc))
         racket/class
         racket/lazy-require
         "connection-dialog.rkt"
         "keychain.rkt"
         "welcome-window.rkt")

(lazy-require
 ["workspace-window.rkt" (workspace-window)])

;; welcome window ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(provide
 render-welcome-window)

(define the-welcome-renderer #f)

(define (render-connection-dialog conn action
                                  #:title [title "New Connection"]
                                  #:save-label [save-label "Connect"])
  (render
   (connection-dialog
    #:title title
    #:save-label save-label
    #:save-action
    (λ (saved-conn close!)
      (action saved-conn)
      (close!))
    conn)
   the-welcome-renderer))

(define (render-welcome-window)
  (cond
    [the-welcome-renderer
     (define the-welcome-window
       (renderer-root the-welcome-renderer))
     (send the-welcome-window show #t)]
    [else
     (set! the-welcome-renderer (do-render-welcome-window))]))

(define (do-render-welcome-window)
  (define/obs @connections
    (rpc:get-connections))
  (define (reload-connections)
    (@connections . := . (rpc:get-connections)))
  (render
   (welcome-window
    #:open-action
    (λ (details)
      (rpc:touch-connection details)
      (reload-connections)
      (open-workspace details))
    #:new-action
    (λ ()
      (render-connection-dialog
       (make-ConnectionDetails
        #:name "New Connection")
       (λ (details)
         (rpc:save-connection details)
         (reload-connections)
         (open-workspace details))))
    #:context-action
    (λ (details event)
      ;; Enqueue a low-priority callback to allow other events to be
      ;; handled before we block the eventspace.
      (when details
        (gui:queue-callback
         (lambda ()
           (render-popup-menu
            the-welcome-renderer
            (popup-menu
             (menu-item
              "Edit..."
              (λ ()
                (render-connection-dialog
                 #:title "Edit Connection"
                 #:save-label "Save"
                 details
                 (λ (updated-details)
                   (rpc:update-connection updated-details)
                   (reload-connections)))))
             (menu-item-separator)
             (menu-item
              "Delete"
              (λ ()
                (when (confirm-deletion details)
                  (remove-password
                   (current-keychain)
                   (ConnectionDetails-password-id details))
                  (rpc:delete-connection details)
                  (reload-connections)))))
            (send event get-x)
            (send event get-y)))
         #f)))
    @connections)))

(define (confirm-deletion details)
  (define res
    (gui:message-box/custom
     "Delete Connection"
     (format "Delete ~a? This action cannot be undone."
             (ConnectionDetails-name details))
     "Delete"
     "Cancel"
     #f
     (renderer-root the-welcome-renderer)
     '(caution default=2)))
  (eqv? res 1))


;; workspaces ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(provide
 open-workspace
 close-workspace)

(struct workspace (details renderer))

;; workspace id -> workspace
(define workspaces (make-hasheqv))

(define (open-workspace details)
  (define connection-id
    (ConnectionDetails-id details))
  (define workspace-id
    (for/first ([(id w) (in-hash workspaces)]
                #:when (= connection-id (ConnectionDetails-id (workspace-details w))))
      id))
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
