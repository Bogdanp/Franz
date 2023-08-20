#lang racket/gui/easy

(require franz/connection-details
         (prefix-in rpc: (submod franz/connection-details rpc))
         (prefix-in rpc: (submod franz/metadata rpc))
         (prefix-in rpc: (submod franz/workspace rpc))
         racket/class
         racket/lazy-require
         "about-window.rkt"
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
    (get-about-renderer)
    (get-welcome-renderer)
    (get-preferences-renderer)
    (map workspace-renderer (hash-values workspaces)))))

(define (try-close-renderer r)
  (when r (send (renderer-root r) show #f)))

(define (make-singleton-renderer make-renderer-proc)
  (define the-renderer #f)
  (values
   (lambda args
     (if the-renderer
         (send (renderer-root the-renderer) show #t)
         (set! the-renderer (apply make-renderer-proc args))))
   (lambda ()
     the-renderer)))


;; welcome window ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(provide
 render-welcome-window
 get-welcome-renderer)

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
   (get-welcome-renderer)))

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
       (get-welcome-renderer)
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

(define-values (render-welcome-window get-welcome-renderer)
  (make-singleton-renderer do-render-welcome-window))


;; preferences ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(provide
 render-preferences-window)

(define-values (render-preferences-window get-preferences-renderer)
  (make-singleton-renderer
   (let ([@current-preferences-view (@ 'general)])
     (λ ([view 'general])
       (@current-preferences-view . := . view)
       (preferences-window
        @current-preferences-view)))))


;; about ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(provide
 render-about-window)

(define-values (render-about-window get-about-renderer)
  (make-singleton-renderer (λ () (render (about-window)))))


;; workspaces ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(provide
 open-workspace
 close-workspace
 get-workspace-renderer)

(struct workspace (details renderer))

;; workspace id -> workspace
(define workspaces (make-hasheqv))

(define (open-workspace details [force? #f])
  (if (rpc:is-license-valid)
      (do-open-workspace details force?)
      (render-preferences-window 'license)))

(define (do-open-workspace details force?)
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
