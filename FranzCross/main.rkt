#lang racket/gui/easy

(module+ main
  (require franz/appdata
           franz/main
           racket/port
           "keychain.rkt"
           "window-manager.rkt")

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

    (gui:application-quit-handler
     (lambda ()
       (exit 0)))

    (call-with-main-parameterization
     (lambda ()
       (parameterize ([current-keychain
                       (make-filesystem-keychain
                        (build-application-path "keychain.rktd"))])
         (set! eventspace (gui:make-eventspace))
         (parameterize ([gui:current-eventspace eventspace])
           (render-welcome-window))))))
  (void
   (with-handlers ([exn:break? void])
     (gui:yield eventspace))))
