#lang racket/gui/easy

(require franz/connection-details
         (submod franz/connection-details rpc)
         franz/main
         "connection-dialog.rkt"
         "welcome-window.rkt")

(define (new-connection-dialog action)
  (connection-dialog
   #:title "New Connection"
   #:save-action (λ (conn close!)
                   (action conn)
                   (close!))
   (make-ConnectionDetails
    #:name "New Connection")))

(define (main)
  (define/obs @connections
    (get-connections))
  (define the-renderer
    (render
     (welcome-window
      #:new-action (λ ()
                     (render
                      (new-connection-dialog
                       (λ (conn)
                         (save-connection conn)
                         (@connections . := . (get-connections))))
                      the-renderer))
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
