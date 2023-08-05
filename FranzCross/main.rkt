#lang racket/gui/easy

(require franz/connection-details
         (submod franz/connection-details rpc)
         franz/main
         racket/class
         "connection-dialog.rkt"
         "renderer.rkt"
         "welcome-window.rkt")

(define/obs @connections null)

(define (reload-connections)
  (@connections . := . (get-connections)))

(define (render-connection-dialog conn action #:title [title "New Connection"])
  (render
   (connection-dialog
    #:title title
    #:save-action
    (λ (saved-conn close!)
      (action saved-conn)
      (reload-connections)
      (close!))
    conn)
   (current-renderer)))

(define (confirm-deletion details)
  (define res
    (gui:message-box/custom
     "Delete Connection"
     (format "Delete ~a? This action cannot be undone."
             (ConnectionDetails-name details))
     "Delete"
     "Cancel"
     #f
     (renderer-root (current-renderer))
     '(caution default=1)))
  (eqv? res 1))

(define (main)
  (reload-connections)
  (current-renderer
   (render
    (welcome-window
     #:new-action
     (λ ()
       (render-connection-dialog
        (make-ConnectionDetails
         #:name "New Connection")
        save-connection))
     #:context-action
     (λ (item event)
       ;; Enqueue a low-priority callback to allow other events to be
       ;; handled before we block the eventspace.
       (when item
         (gui:queue-callback
          (lambda ()
            (render-popup-menu
             (current-renderer)
             (popup-menu
              (menu-item
               "Edit..."
               (λ ()
                 (render-connection-dialog
                  #:title "Edit Connection"
                  item update-connection)))
              (menu-item-separator)
              (menu-item
               "Delete"
               (λ ()
                 (when (confirm-deletion item)
                   (delete-connection item)
                   (reload-connections)))))
             (send event get-x)
             (send event get-y)))
          #f)))
     @connections))))

(module+ main
  (gui:application-quit-handler
   (lambda ()
     (exit 0)))

  (call-with-main-parameterization
   (lambda ()
     (define eventspace (gui:make-eventspace))
     (parameterize ([gui:current-eventspace eventspace])
       (main))
     (with-handlers ([exn:break? void])
       (gui:yield eventspace))
     (void))))
