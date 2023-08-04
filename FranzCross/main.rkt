#lang racket/gui/easy

(require franz/connection-details
         (submod franz/connection-details rpc)
         franz/main
         racket/class
         "connection-dialog.rkt"
         "welcome-window.rkt")

(define (main)
  (define/obs @connections
    (get-connections))
  (define (reload-connections)
    (@connections . := . (get-connections)))
  (define (render-connection-dialog conn action
                                    #:title [title "New Connection"])
    (render
     (connection-dialog
      #:title title
      #:save-action (λ (saved-conn close!)
                      (action saved-conn)
                      (reload-connections)
                      (close!))
      conn)
     the-renderer))
  (define the-renderer
    (render
     (welcome-window
      #:new-action (λ ()
                     (render-connection-dialog
                      (make-ConnectionDetails
                       #:name "New Connection")
                      save-connection))
      #:context-action (λ (item event)
                         ;; Enqueue a low-priority callback to allow
                         ;; other events to be handled before we block
                         ;; the eventspace.
                         (when item
                           (gui:queue-callback
                            (lambda ()
                              (render-popup-menu
                               the-renderer
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
                                   (delete-connection item)
                                   (reload-connections))))
                               (send event get-x)
                               (send event get-y)))
                            #f)))
      @connections)))
  (void))

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
