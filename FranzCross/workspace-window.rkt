#lang racket/gui/easy

(require franz/connection-details
         (submod franz/workspace rpc)
         "keychain.rkt"
         "mixin.rkt"
         (prefix-in m: "window-manager.rkt"))

(provide
 workspace-window)

(define (workspace-window details)
  (define keychain (current-keychain))
  (define password (and keychain (get-password keychain (ConnectionDetails-password-id details))))
  (define id (open-workspace details password))
  (window
   #:title (~title details)
   #:mixin (mix-close-window
            (lambda ()
              (close-workspace id)
              (m:remove-workspace id)))
   #:min-size '(800 600)
   (menu-bar
    (menu
     "File")
    (menu
     "View"
     (menu-item "Hide Sidebar"))
    (menu
     "Help"))
   (text "Hello")))

(define (~title details)
  (format
   "~a — ~a"
   (ConnectionDetails-name details)
   (~hostname details)))

(define (~hostname details)
  (format
   "~a:~a"
   (ConnectionDetails-bootstrap-host details)
   (ConnectionDetails-bootstrap-port details)))

(module+ main
  (render
   (workspace-window
    (make-ConnectionDetails
     #:id 1
     #:name "Example"
     #:bootstrap-host "127.0.0.1"
     #:bootstrap-port 9092))))
