#lang racket/gui/easy

(require franz/connection-details
         (prefix-in rpc: (submod franz/connection-details rpc))
         (prefix-in rpc: (submod franz/metadata rpc))
         (prefix-in rpc: (submod franz/workspace rpc))
         racket/class
         racket/lazy-require
         racket/match
         (prefix-in ~ threading)
         "about-window.rkt"
         "auto-update.rkt"
         "connection-dialog.rkt"
         "hacks.rkt"
         "keychain.rkt"
         "preferences-window.rkt"
         "welcome-window.rkt")

(lazy-require
 ["alert.rkt" (confirm)]
 ["scripting-window.rkt" (scripting-window)]
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
      (close!)
      (action saved-conn))
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
         (define saved-details
           (rpc:save-connection details))
         (reload-connections)
         (open-workspace saved-details))))
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
     (lambda ([view 'general])
       (@current-preferences-view . := . view)
       (render (preferences-window @current-preferences-view))))))


;; about ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(provide
 render-about-window)

(define-values (render-about-window get-about-renderer)
  (make-singleton-renderer
   (λ () (render (about-window)))))


;; auto-update ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(provide
 render-check-for-updates-dialog)

(define (render-check-for-updates-dialog)
  (render (check-for-updates-dialog)))


;; scripting windows ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(provide
 open-scripting-window
 disable-scripting-window
 enable-scripting-window)

(struct scripting-ctx (@enabled? renderer [apply-proc #:mutable]))

;; (cons workspace-id topic) -> scripting-ctx
(define scripting-windows (make-hash))

(define (open-scripting-window id topic apply-proc)
  (define k (cons id topic))
  (cond
    [(hash-ref scripting-windows k #f)
     => (lambda (ctx)
          (define r (scripting-ctx-renderer ctx))
          (set-scripting-ctx-apply-proc! ctx apply-proc)
          (send (renderer-root r) show #t))]
    [else
     (define/obs @enabled? #t)
     (define r
       (render
        (scripting-window
         #:enabled? @enabled?
         #:apply-proc (λ (script)
                        ((scripting-ctx-apply-proc ctx) script))
         id topic)))
     (define ctx (scripting-ctx @enabled? r apply-proc))
     (begin0 r
       (hash-set! scripting-windows k ctx))]))

(define (disable-scripting-window id topic)
  (try-update-scripting-window!
   id topic
   (lambda (ctx)
     (set-scripting-ctx-apply-proc! ctx void)
     ((scripting-ctx-@enabled? ctx) . := . #f))))

(define (enable-scripting-window id topic apply-proc)
  (try-update-scripting-window!
   id topic
   (lambda (ctx)
     (set-scripting-ctx-apply-proc! ctx apply-proc)
     ((scripting-ctx-@enabled? ctx) . := . #t))))

(define (try-update-scripting-window! id topic proc)
  (define maybe-ctx
    (hash-ref scripting-windows (cons id topic) #f))
  (when maybe-ctx
    (proc maybe-ctx)))


;; workspaces ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(provide
 open-workspace
 close-workspace
 get-workspace-details
 get-workspace-renderer)

(struct workspace (details renderer)
  #:transparent)

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
  (match-define (workspace _details renderer)
    (hash-ref workspaces id))
  (hash-remove! workspaces id)
  (renderer-destroy renderer)
  (rpc:close-workspace id))

(define (get-workspace-details id)
  (workspace-details (hash-ref workspaces id)))

(define (get-workspace-renderer id)
  (workspace-renderer (hash-ref workspaces id)))
