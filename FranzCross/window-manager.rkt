#lang racket/gui/easy

(require franz/connection-details
         (prefix-in rpc: (submod franz/connection-details rpc))
         (prefix-in rpc: (submod franz/workspace rpc))
         racket/class
         racket/lazy-require
         "connection-dialog.rkt"
         "hacks.rkt"
         "keychain.rkt"
         "preferences-window.rkt"
         "welcome-window.rkt")

(lazy-require
 ["alert.rkt" (confirm)]
 ["workspace-window.rkt" (workspace-window)])

(provide
 close-all-windows)

(define (close-all-windows)
  (for-each
   try-close-renderer
   (list*
    the-welcome-renderer
    the-preferences-renderer
    (map workspace-renderer (hash-values workspaces)))))

(define (try-close-renderer r)
  (when r (send (renderer-root r) show #f)))


;; welcome window ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(provide
 render-welcome-window
 get-welcome-renderer)

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
      (render-popup-menu*
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
           (when (confirm #:title "Delete Connection"
                          #:message (format "Delete ~a? This action cannot be undone."
                                            (ConnectionDetails-name details)))
             (remove-password
              (current-keychain)
              (ConnectionDetails-password-id details))
             (rpc:delete-connection details)
             (reload-connections)))))
       event))
    @connections)))

(define (get-welcome-renderer)
  the-welcome-renderer)


;; preferences ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(provide
 render-preferences-window)

(define the-preferences-renderer #f)
(define/obs @current-preferences-view 'general)

(define (render-preferences-window [view 'general])
  (@current-preferences-view . := . view)
  (cond
    [the-preferences-renderer
     (define the-preferences-window
       (renderer-root the-preferences-renderer))
     (send the-preferences-window show #t)]
    [else
     (set! the-preferences-renderer
           (render
            (preferences-window
             @current-preferences-view)))]))


;; workspaces ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(provide
 open-workspace
 close-workspace
 get-workspace-renderer)

(struct workspace (details renderer))

;; workspace id -> workspace
(define workspaces (make-hasheqv))

(define (open-workspace details [force? #f])
  (define connection-id
    (ConnectionDetails-id details))
  (define workspace-id
    (for/first ([(id w) (in-hash workspaces)]
                #:when (= connection-id (ConnectionDetails-id (workspace-details w))))
      id))
  (cond
    [(and workspace-id (not force?))
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

(define (get-workspace-renderer id)
  (workspace-renderer (hash-ref workspaces id)))
