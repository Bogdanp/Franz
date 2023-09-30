#lang racket/gui/easy

(module+ main
  (require franz/appdata
           franz/main
           (prefix-in rpc: (submod franz/workspace rpc))
           racket/date
           racket/port
           "auto-update.rkt"
           "keychain.rkt"
           "window-manager.rkt")

  (define quit-evt (make-semaphore))
  (gui:application-quit-handler
   (lambda ()
     (semaphore-post quit-evt)))
  (gui:application-preferences-handler
   (lambda ()
     (parameterize ([gui:current-eventspace eventspace])
       (render-preferences-window))))
  (gui:application-about-handler
   (lambda ()
     (parameterize ([gui:current-eventspace eventspace])
       (render-about-window))))

  (define eventspace #f)
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

    (call-with-main-parameterization
     (lambda ()
       (parameterize ([current-keychain
                       (make-filesystem-keychain
                        (build-application-path "keychain.rktd"))]
                      [date-display-format 'iso-8601])
         (set! eventspace (gui:make-eventspace))
         (parameterize ([gui:current-eventspace eventspace])
           (start-auto-updater)
           (render-welcome-window))))))
  (void
   (with-handlers ([exn:break? void])
     (gui:yield (choice-evt eventspace quit-evt))))
  (rpc:close-all-workspaces))
