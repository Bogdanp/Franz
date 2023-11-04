#lang racket/gui/easy

(require franz/script
         plot
         racket/match
         "common.rkt")

(provide
 result-detail)

(define (result-detail v)
  (match v
    [(ReduceResult.chart c)
     (chart-view c)]
    [(ReduceResult.number n)
     (text-view (number->string n))]
    [(ReduceResult.table columns rows)
     (hpanel
      #:min-size '(400 200)
      (table
       columns
       (for/vector ([row (in-list rows)])
         (for/vector ([col (in-list (TableRow-columns row))])
           col))))]
    [(ReduceResult.text s)
     (text-view s)]))

(define (ChartValue-> v)
  (match v
    [(ChartValue.categorical category) category]
    [(ChartValue.numerical n) n]))

(define (chart-view c)
  (hpanel
   #:min-size '(640 480)
   (snip #f (λ (_ width height)
              (apply
               plot-snip
               #:width width
               #:height height
               #:x-label (Chart-x-label c)
               #:y-label (Chart-y-label c)
               (list
                ((match (Chart-style c)
                   [(ChartStyle.bar) discrete-histogram]
                   [(ChartStyle.line) lines]
                   [(ChartStyle.scatter) points])
                 (for/list ([x (in-list (Chart-xs c))]
                            [y (in-list (Chart-ys c))])
                   (list
                    (ChartValue-> x)
                    (ChartValue-> y))))))))))

(define (text-view s)
  (hpanel
   #:margin '(20 20)
   #:min-size '(200 100)
   #:alignment '(center center)
   (text #:font system-font-xl s)))

(module+ main
  (require "observable.rkt")
  (define-observables
    [@kind 'text])
  (render
   (window
    (vpanel
     (choice
      #:stretch '(#t #f)
      '(chart number table text)
      #:choice->label symbol->string
      #:selection @kind
      @kind:=))
    (observable-view
     @kind
     (lambda (kind)
       (result-detail
        (match kind
          ['chart (ReduceResult.chart
                   (make-Chart
                    #:style (ChartStyle.bar)
                    #:x-domain #f
                    #:x-label "x"
                    #:xs (map ChartValue.categorical '("a" "b" "c"))
                    #:y-domain #f
                    #:y-label "y"
                    #:ys (map ChartValue.numerical '(4 5 6))))]
          ['number (ReduceResult.number 42)]
          ['table (ReduceResult.table
                   '("a" "b")
                   (list
                    (TableRow '("1" "2"))
                    (TableRow '("3" "4"))))]
          ['text (ReduceResult.text "Hello")])))))))
