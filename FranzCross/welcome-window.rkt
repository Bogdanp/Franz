#lang racket/base

(require franz/connection-details
         franz/version
         (prefix-in p: pict)
         racket/gui/easy
         racket/gui/easy/font
         racket/gui/easy/operator
         (prefix-in t: threading)
         "canvas-list.rkt"
         "common.rkt")

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
     (image
      #:size '(128 128)
      icon_512x512.png)
     (text
      #:font (font system-font 28)
      "Welcome to Franz")
     (text
      #:color secondary-color
      (format "Version ~a" franz-version))
     (button "New Connection..." new-action))
    (vpanel
     #:min-size '(280 #f)
     #:stretch '(#f #t)
     (canvas-list
      #:item-height 60
      #:action (λ (type item event)
                 (case type
                   [(dbclick) (open-action item)]
                   [(context) (context-action item event)]
                   [else (void)]))
      @connections
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
    (t:~> (ConnectionDetails-name c)
          (p:text (font #:weight 500 system-font 16))
          (p:colorize (case state
                        [(selected) selection-primary-color]
                        [else primary-color]))))
  (define subtitle
    (t:~> (~address c)
          (p:text (font system-font 12))
          (p:colorize (case state
                        [(selected) selection-secondary-color]
                        [else secondary-color]))))
  (p:lt-superimpose
   background
   (t:~> (p:vl-append 3 title subtitle)
         (p:inset 10 12))))

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
