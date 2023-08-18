#lang racket/gui/easy

(require (prefix-in p: pict)
         "canvas-list.rkt"
         "common.rkt"
         "observable.rkt"
         "preference.rkt"
         "view.rkt")

(provide
 preferences-window)

(define (preferences-window @current-view)
  (window
   #:title "Preferences"
   #:min-size '(640 320)
   (hpanel
    (hpanel
     #:min-size '(180 #f)
     #:stretch '(#f #t)
     (canvas-list
      (@ '(general connections license updates))
      #:action
      (λ (type item _event)
        (case type
          [(select)
           (@current-view . := . item)]))
      #:item-height 30
      (λ (item state dc w h)
        (define the-pict
          (label-pict item state w h))
        (p:draw-pict the-pict dc 0 0))
      #:selected-item @current-view))
    (match-view @current-view
      ['general
       (define/obs @reload-ival
         (get-preference 'reload-interval 5))
       (obs-observe!
        @reload-ival
        (λ (ival)
          (put-preference 'reload-interval ival)))
       (detail-view
        "General"
        (labeled
         #:alignment '(right top)
         #:width 100
         "Reload Interval:"
         (vpanel
          #:stretch '(#t #f)
          (slider
           #:min-value 1
           #:max-value 30
           #:style '(horizontal horizontal-label plain)
           @reload-ival
           (λ:= @reload-ival))
          (text
           (let-observable ([ival @reload-ival])
             (format "Every ~a seconds." ival))))))]
      ['connections
       (detail-view
        "Connections")]
      ['license
       (detail-view
        "License")]
      ['updates
       (detail-view
        "Updates")]))))

(define (detail-view title . content)
  (vpanel
   #:alignment '(left top)
   #:margin '(10 10)
   (text
    #:font system-font-xl
    title)
   (apply
    vpanel
    #:alignment '(left top)
    #:stretch '(#t #f)
    #:margin '(0 10)
    content)))

(define (label-pict label state w h)
  (define label-str
    (string-titlecase (symbol->string label)))
  (define-values (fg-color bg-color)
    (case state
      [(hover) (values primary-color hover-background-color)]
      [(selected) (values selection-primary-color selection-background-color)]
      [else (values primary-color white)]))
  (p:lc-superimpose
   (p:filled-rectangle
    #:color bg-color
    #:border-width 1
    #:border-color bg-color
    w h)
   (p:inset
    (p:colorize
     (p:text label-str system-font-m)
     fg-color)
    8 0)))

(module+ main
  (render
   (preferences-window
    (@ 'license))))
