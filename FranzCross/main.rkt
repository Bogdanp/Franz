#lang racket/gui/easy

(require franz/connection-details
         (submod franz/connection-details rpc)
         franz/main
         "connection-dialog.rkt"
         "welcome-window.rkt")

(define (main)
  (call-with-main-parameterization
   (lambda ()
     (parameterize ([gui:current-eventspace (gui:make-eventspace)])
       (define/obs @connections
         (get-connections))
       (define the-renderer
        (render
         (welcome-window
          #:new-action (λ ()
                         (render
                          (connection-dialog
                           #:title "New Connection"
                           #:save-action (λ (conn close!)
                                           (save-connection conn)
                                           (@connections . := . (get-connections))
                                           (close!))
                           (make-ConnectionDetails
                            #:name "New Connection"))
                          the-renderer))
          @connections)))
       (void)))))

(module+ main
  (main))
