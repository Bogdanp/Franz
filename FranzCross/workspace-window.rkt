#lang racket/gui/easy

(require franz/connection-details
         (submod franz/workspace rpc)
         racket/format
         "keychain.rkt"
         "mixin.rkt"
         "split-view.rkt"
         (prefix-in m: "window-manager.rkt"))

(provide
 workspace-window)

(define (workspace-window details)
  (define keychain (current-keychain))
  (define password (and keychain (get-password keychain (ConnectionDetails-password-id details))))
  (define id (open-workspace details password))
  (define/obs @sidebar-visible? #t)
  (define sidebar
    (text "Sidebar"))
  (define content
    (text "Content"))
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
     (menu-item
      (@sidebar-visible? . ~> . (λ (visible?)
                                  (~a (if visible? "Hide" "Show") " Sidebar")))
      (λ<~ @sidebar-visible? not)))
    (menu
     "Help"))
   (if-view
    @sidebar-visible?
    (split-view sidebar content)
    content)))

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
