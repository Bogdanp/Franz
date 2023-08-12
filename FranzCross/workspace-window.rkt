#lang racket/gui/easy

(require franz/connection-details
         racket/format
         "mixin.rkt"
         "split-view.rkt"
         "status-bar.rkt"
         (prefix-in m: "window-manager.rkt"))

(provide
 workspace-window)

(define (workspace-window id details)
  (define/obs @sidebar-visible? #t)
  (define/obs @status "Ready")
  (define sidebar
    (text "Sidebar"))
  (define content
    (vpanel
     (status-bar @status (ConnectionDetails-name details))))
  (window
   #:title (~title details)
   #:mixin (mix-close-window
            (lambda ()
              (m:close-workspace id)))
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
  (m:open-workspace
   (make-ConnectionDetails
    #:id 1
    #:name "Example"
    #:bootstrap-host "127.0.0.1"
    #:bootstrap-port 9092)))
