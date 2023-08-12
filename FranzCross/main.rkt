#lang racket/gui/easy

(require franz/appdata
         franz/connection-details
         (submod franz/connection-details rpc)
         franz/main
         racket/class
         "connection-dialog.rkt"
         "keychain.rkt"
         "renderer.rkt"
         "welcome-window.rkt"
         "window-manager.rkt")

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
     '(caution default=2)))
  (eqv? res 1))

(define (main)
  (reload-connections)
  (current-renderer
   (render
    (welcome-window
     #:open-action
     (λ (item)
       (open-workspace item))
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
                   (remove-password
                    (current-keychain)
                    (ConnectionDetails-password-id item))
                   (delete-connection item)
                   (reload-connections)))))
             (send event get-x)
             (send event get-y)))
          #f)))
     @connections))))

(module+ main
  (require racket/port)

  (let/cc esc
    (uncaught-exception-handler
     (lambda (e)
       (define message
         (call-with-output-string
          (lambda (out)
            (parameterize ([current-error-port out])
              ((error-display-handler) (exn-message e) e)))))
       (gui:message-box "Error" message #f '(stop ok))
       (esc)))

    (gui:application-quit-handler
     (lambda ()
       (exit 0)))

    (call-with-main-parameterization
     (lambda ()
       (parameterize ([current-keychain
                       (make-filesystem-keychain
                        (build-application-path "keychain.rktd"))])
         (define eventspace (gui:make-eventspace))
         (parameterize ([gui:current-eventspace eventspace])
           (main))
         (with-handlers ([exn:break? void])
           (gui:yield eventspace))
         (void))))))
