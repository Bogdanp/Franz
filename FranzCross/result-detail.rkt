#lang racket/gui/easy

(require franz/script
         plot
         racket/class
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
   (snip
    #f
    #:mixin
    (λ (%)
      (class %
        (super-new)
        (define/override (on-event event)
          (case (send event get-event-type)
            [(right-up)
             ;; TODO: render-popup-menu should be able to take a frame
             ;; for cases like this where we can't easily get access to
             ;; the root renderer.
             (define x (send event get-x))
             (define y (send event get-y))
             (define m (new gui:popup-menu%))
             (define f (send this get-top-level-window))
             (define-values (w h)
               (send this get-scaled-client-size))
             (new gui:menu-item%
                  [label "Export..."]
                  [parent m]
                  [callback (lambda (_self _event)
                              (define the-bitmap
                                (plot-chart c w h plot-bitmap))
                              (define maybe-filename
                                (gui:put-file
                                 #f ;message
                                 #f ;parent
                                 #f ;directory
                                 #f ;filename
                                 #f ;extension
                                 null ;style
                                 '(("PNG Image" "*.png"))))
                              (when maybe-filename
                                (send the-bitmap save-file maybe-filename 'png 100)))])
             (send f popup-menu m x y)]
            [else
             (super on-event event)]))))
    (λ (_ width height)
      (plot-chart c width height)))))

(define (plot-chart c width height [plotter plot-snip])
  (define-values (x-min x-max)
    (ChartScale-> (Chart-x-scale c)))
  (define-values (y-min y-max)
    (ChartScale-> (Chart-y-scale c)))
  (define x-date-ticks?
    (ormap (compose1 ChartValue.timestamp? ChartPair-x) (Chart-pairs c)))
  (define y-date-ticks?
    (ormap (compose1 ChartValue.timestamp? ChartPair-y) (Chart-pairs c)))
  (define the-candlestick-width
    (match (Chart-style c)
      [(ChartStyle.candlestick width)
       (or width 60)]
      [_ 60]))
  (parameterize ([candlestick-width the-candlestick-width]
                 [plot-x-ticks (if x-date-ticks? (date-ticks) (linear-ticks))]
                 [plot-y-ticks (if y-date-ticks? (date-ticks) (linear-ticks))])
    (apply
     plotter
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
         [(ChartStyle.area) area]
         [(ChartStyle.bar) discrete-histogram]
         [(ChartStyle.candlestick _) candlesticks]
         [(ChartStyle.line) lines]
         [(ChartStyle.scatter) points])
       (for/list ([p (in-list (Chart-pairs c))])
         ((match (Chart-style c)
            [(ChartStyle.candlestick _) list*]
            [_ list])
          (ChartValue-> (ChartPair-x p))
          (ChartValue-> (ChartPair-y p)))))))))

(define (area ps)
  (define min-y
    (apply min (map cadr ps)))
  (lines-interval
   (for/list ([p (in-list ps)])
     (list (car p) min-y))
   ps))

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
                    #:pairs
                    (for/list ([x (in-list '("a" "b" "c"))]
                               [y (in-list '(1 2 3))])
                      (ChartPair
                       (ChartValue.categorical x)
                       (ChartValue.numerical y)))
                    #:x-scale #f
                    #:x-label "x"
                    #:y-scale #f
                    #:y-label "y"))]
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
