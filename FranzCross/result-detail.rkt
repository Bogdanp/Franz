#lang racket/gui/easy

(require franz/script
         plot
         racket/match
         "common.rkt")

(provide
 result-detail)

(define (result-detail v)
  (match v
    [(ReduceResult.text s)
     (text-view s)]
    [(ReduceResult.number n)
     (text-view (number->string n))]
    [(ReduceResult.barChart xlabel xs ylabel ys)
     (plot-view
      #:x-label xlabel
      #:y-label ylabel
      (discrete-histogram (map list xs ys)))]
    [(ReduceResult.lineChart xlabel xs ylabel ys)
     (plot-view
      #:x-label xlabel
      #:y-label ylabel
      (lines (map list xs ys)))]
    [(ReduceResult.table columns rows)
     (hpanel
      #:min-size '(400 200)
      (table
       columns
       (for/vector ([row (in-list rows)])
         (for/vector ([col (in-list row)])
           col))))]))

(define (text-view s)
  (hpanel
   #:margin '(20 20)
   #:min-size '(200 100)
   #:alignment '(center center)
   (text #:font system-font-xl s)))

(define (plot-view #:x-label x-label
                   #:y-label y-label
                   . trees)
  (hpanel
   #:min-size '(800 600)
   (snip #f (λ (_ width height)
              (apply
               plot-snip
               #:width width
               #:height height
               #:x-label x-label
               #:y-label y-label
               trees)))))

(module+ main
  (require "observable.rkt")
  (define-observables
    [@kind 'text])
  (render
   (window
    (vpanel
     (choice
      #:stretch '(#t #f)
      '(text number lineChart)
      #:choice->label symbol->string
      #:selection @kind
      @kind:=))
    (observable-view
     @kind
     (lambda (kind)
       (result-detail
        (match kind
          ['text (ReduceResult.text "Hello")]
          ['number (ReduceResult.number 42)]
          ['lineChart (ReduceResult.lineChart
                       "x" '(1 2 3)
                       "y" '(4 5 6))])))))))
