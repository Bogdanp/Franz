#lang racket/gui/easy

(require (prefix-in p: pict)
         racket/class
         racket/gui/easy/color
         "common.rkt")

(provide
 status-bar)

(define (status-bar @status title)
  (canvas
   @status
   #:min-size '(250 35)
   #:stretch '(#t #f)
   #:style '(transparent)
   (lambda (dc status)
     (define-values (w h)
       (send dc get-size))
     (define-values (title-w _title-h _title-baseline _title-extra)
       (send dc get-text-extent title system-font-s))
     (define-values (status-w _status-h _status-baseline _status-extra)
       (send dc get-text-extent status system-font-xs))
     (p:draw-pict
      (p:inset
       (p:lc-superimpose
        (p:filled-rounded-rectangle
         #:draw-border? #t
         #:color (color #xDDDDDDFF)
         #:border-color (color #xCCCCCCFF)
         #:border-width 1
         (- w 10)
         (- h 7)
         3 ;radius
         )
        (p:inset
         (p:hc-append
          (p:colorize
           (p:text title system-font-s)
           secondary-color)
          (p:ghost
           (p:rectangle
            (- w 10 4 5 title-w status-w) 0))
          (p:colorize
           (p:text status system-font-xs)
           secondary-color))
         5 0))
       5 5)
      dc 0 0))))

(module+ main
  (render
   (window
    #:title "Status Bar"
    #:size '(800 100)
    (status-bar (@ "Ready") "Example"))))
