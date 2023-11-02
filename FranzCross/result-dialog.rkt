#lang racket/gui/easy

(require franz/script
         racket/match
         plot
         "common.rkt")

(provide
 result-dialog)

(define (result-dialog title v)
  (dialog
   #:title title
   #:style '(no-sheet close-button)
   (result-detail v)))

(define (result-detail v)
  (match v
    [(ReduceResult.text s)
     (text-detail s)]
    [(ReduceResult.number n)
     (text-detail (number->string n))]
    [(ReduceResult.lineChart xlabel xs ylabel ys)
     (hpanel
      #:min-size '(800 600)
      (snip #f (λ (_ width height)
                 (plot-snip
                  #:width width
                  #:height height
                  #:x-label xlabel
                  #:y-label ylabel
                  (lines
                   (for/list ([x (in-list xs)]
                              [y (in-list ys)])
                     (list x y)))))))]))

(define (text-detail s)
  (hpanel
   #:margin '(20 20)
   #:min-size '(200 100)
   #:alignment '(center center)
   (text
    #:font system-font-xl
    s)))

(module+ main
  (render
   (result-dialog
    "Result"
    (ReduceResult.text "Hello"))))
