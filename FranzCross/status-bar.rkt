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
     (define the-pict
       (let ([the-pict
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
                (p:colorize
                 (p:text title system-font-s)
                 secondary-color)
                5 0))])
         (define status-pict
           (p:colorize
            (p:text status system-font-xs)
            secondary-color))
         (p:inset
          (p:pin-over
           the-pict
           (- (p:pict-width the-pict)
              (p:pict-width status-pict)
              5)
           (- (/ (p:pict-height the-pict) 2)
              (/ (p:pict-height status-pict) 2))
           status-pict)
          5 5)))
     (p:draw-pict the-pict dc 0 0))))

(module+ main
  (render
   (window
    #:title "Status Bar"
    #:size '(800 100)
    (status-bar (@ "Ready") "Example Connection"))))
