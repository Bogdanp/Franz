#lang racket/base

(require franz/connection-details
         franz/version
         net/sendurl
         (prefix-in p: pict)
         racket/gui/easy
         racket/gui/easy/operator
         racket/math
         (prefix-in ~ threading)
         "canvas-list.rkt"
         "common.rkt"
         "mixin.rkt"
         "observable.rkt"
         "xmas-icon.rkt")

(provide
 welcome-window)

(define (welcome-window @connections
                        #:new-action [new-action void]
                        #:open-action [open-action void]
                        #:context-action [context-action void])
  (window
   #:title "Welcome to Franz"
   #:size '(720 380)
   #:style '(no-resize-border)
   (hpanel
    (vpanel
     #:alignment '(center center)
     (if xmas?
         (xmas-icon)
         (image
          #:size '(128 128)
          icon_512x512.png))
     (text
      #:font system-font-xl
      "Welcome to Franz")
     (text
      #:color secondary-color
      #:font system-font-s
      (format "Version ~a" franz-version))
     (button "New Connection..." new-action)
     (hpanel
      #:stretch '(#t #f)
      #:min-size '(#f 20))
     (hpanel
      #:stretch '(#t #f)
      #:alignment '(center center)
      (button "Documentation" (λ () (send-url "https://franz.defn.io/manual/")))
      (button "Support" (λ () (send-url "mailto:bogdan@defn.io?subject=Franz%20Support")))
      (button "Mastodon" (λ () (send-url "https://hachyderm.io/@franz_app")))))
    (vpanel
     #:min-size '(280 #f)
     #:stretch '(#f #t)
     (canvas-list
      #:mixin mix-initial-focus
      #:action (λ (type item event)
                 (case type
                   [(dbclick) (open-action item)]
                   [(context) (context-action item event)]
                   [else (void)]))
      @connections
      #:item-height 60
      (λ (item state dc w h)
        (p:draw-pict (connection-details-pict item state w h) dc 0 0)))))))

(define (connection-details-pict c state w h)
  (define background
    (p:filled-rectangle
     #:draw-border? #f
     #:color (case state
               [(selected) selection-background-color]
               [else white])
     w h))
  (define title
    (~~> (ConnectionDetails-name c)
         (p:text system-font-m)
         (p:colorize (case state
                       [(selected) selection-primary-color]
                       [else primary-color]))))
  (define subtitle
    (~~> (~address c)
         (p:text system-font-s)
         (p:colorize (case state
                       [(selected) selection-secondary-color]
                       [else secondary-color]))))
  (~~> (p:vl-append 2 title subtitle)
       (p:inset 8 0)
       (p:lc-superimpose background _)))

(define (~address c)
  (format "~a:~a"
          (ConnectionDetails-bootstrap-host c)
          (ConnectionDetails-bootstrap-port c)))

(module+ main
  (define/obs @connections
    (for/list ([id (in-range 20)])
      (make-ConnectionDetails
       #:id id
       #:name (format "Example ~a" id))))

  (render
   (welcome-window
    #:new-action
    (λ ()
      (@connections . <~ . (λ (conns)
                             (cons
                              (make-ConnectionDetails
                               #:id 123
                               #:name "New Connection")
                              conns))))
    @connections)))
