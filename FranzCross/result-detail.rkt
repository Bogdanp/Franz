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
    [(ReduceResult.stack s)
     (apply
      (case (Stack-direction s)
        [(horizontal) hpanel]
        [(vertical) vpanel])
      (map result-detail (Stack-children s)))]
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

(define (ChartScale-> v)
  (match v
    [#f (values #f #f)]
    [(ChartScale.numerical lo hi _) (values lo hi)]))

(define (ChartValue-> v)
  (match v
    [(ChartValue.candlestick o h l c) (list o h l c)]
    [(ChartValue.categorical category) category]
    [(ChartValue.numerical n) n]
    [(ChartValue.timestamp t) t]))

(define (chart-view c)
  (hpanel
   #:min-size '(640 480)
   (snip #f (λ (_ width height)
              (define-values (x-min x-max)
                (ChartScale-> (Chart-x-scale c)))
              (define-values (y-min y-max)
                (ChartScale-> (Chart-y-scale c)))
              (define x-date-ticks?
                (ormap ChartValue.timestamp? (Chart-xs c)))
              (define y-date-ticks?
                (ormap ChartValue.timestamp? (Chart-ys c)))
              (parameterize ([candlestick-width
                              (match (Chart-style c)
                                [(ChartStyle.candlestick width) width]
                                [_ 1])]
                             [plot-x-ticks
                              (if x-date-ticks?
                                  (date-ticks)
                                  (linear-ticks))]
                             [plot-y-ticks
                              (if y-date-ticks?
                                  (date-ticks)
                                  (linear-ticks))])
                (apply
                 plot-snip
                 #:width width
                 #:height height
                 #:x-min x-min
                 #:x-max x-max
                 #:x-label (Chart-x-label c)
                 #:y-min y-min
                 #:y-max y-max
                 #:y-label (Chart-y-label c)
                 (list
                  ((match (Chart-style c)
                     [(ChartStyle.bar) discrete-histogram]
                     [(ChartStyle.candlestick _) candlesticks]
                     [(ChartStyle.line) lines]
                     [(ChartStyle.scatter) points])
                   (for/list ([x (in-list (Chart-xs c))]
                              [y (in-list (Chart-ys c))])
                     ((match (Chart-style c)
                        [(ChartStyle.candlestick _) list*]
                        [_ list])
                      (ChartValue-> x)
                      (ChartValue-> y)))))))))))

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
      '(chart number stack table text)
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
                    #:x-scale #f
                    #:x-label "x"
                    #:xs (map ChartValue.categorical '("a" "b" "c"))
                    #:y-scale #f
                    #:y-label "y"
                    #:ys (map ChartValue.numerical '(4 5 6))))]
          ['number (ReduceResult.number 42)]
          ['stack (ReduceResult.stack
                   (make-Stack
                    #:direction 'horizontal
                    #:children (list
                                (ReduceResult.text "hello")
                                (ReduceResult.table '("a" "b") null))))]
          ['table (ReduceResult.table
                   '("a" "b")
                   (list
                    (TableRow '("1" "2"))
                    (TableRow '("3" "4"))))]
          ['text (ReduceResult.text "Hello")])))))))
