#lang racket/gui/easy

(require racket/class
         racket/gui/easy/color
         "observable.rkt")

(provide
 split-view)

(define divider-background-color
  (color #xEDEDEDFF))

(define regular-cursor
  (make-object gui:cursor% 'arrow))
(define drag-cursor
  (make-object gui:cursor% 'size-e/w))

(define transparent-pen
  (send gui:the-pen-list find-or-create-pen "black" 0 'transparent))

(define (make-divider #:x-min [x-min 0]
                      #:position-action [position-action void]
                      #:collapse-action [collapse-action void])
  (canvas
   #f
   #:min-size '(3 #f)
   #:stretch '(#f #t)
   #:mixin
   (λ (%)
     (class %
       (super-new)

       (define dragging? #f)
       (define src #f)
       (define dst #f)
       (define/override (on-subwindow-event _receiver event)
         (case (send event get-event-type)
           [(enter)
            (define frame (send this get-top-level-window))
            (send frame set-cursor drag-cursor)]
           [(leave)
            (unless dragging?
              (define frame (send this get-top-level-window))
              (send frame set-cursor regular-cursor))]
           [(left-down)
            (set! dragging? #t)
            (set! src (send event get-x))]
           [(left-up)
            (set! dragging? #f)
            (schedule-move)
            (define frame (send this get-top-level-window))
            (send frame set-cursor regular-cursor)]
           [(motion)
            (when dragging?
              (set! dst (send event get-x))
              (schedule-move))]))

       (define move-scheduled? #f)
       (define (schedule-move)
         (unless move-scheduled?
           (set! move-scheduled? #t)
           (thread
            (lambda ()
              (sleep (/ 1.0 120))
              (gui:queue-callback
               (lambda ()
                 (when (and src dst)
                   (set! move-scheduled? #f)
                   (define dx (- dst src))
                   (when (<= dx (- x-min))
                     (collapse-action))
                   (position-action dx))))))))))
   (lambda (dc _)
     (define-values (w h)
       (send dc get-size))
     (send dc set-pen transparent-pen)
     (send dc set-brush divider-background-color 'solid)
     (send dc draw-rectangle 0 0 w h))))

(define (split-view lhs rhs
                    #:min-width-lhs [min-width-lhs 240]
                    #:collapse-action [collapse-action void])
  (define/obs @x min-width-lhs)
  (define divider
    (make-divider
     #:x-min min-width-lhs
     #:collapse-action collapse-action
     #:position-action (λ (dx)
                         (update-observable @x
                           (max (+ it dx) min-width-lhs)))))
  (hpanel
   (hpanel
    #:min-size (@x . ~> . (λ (x) (list x #f)))
    #:stretch '(#f #t)
    lhs)
   divider
   (hpanel rhs)))

(module+ main
  (render
   (window
    #:title "Split View"
    #:size '(800 600)
    (split-view
     #:min-width-lhs 200
     #:collapse-action (λ () (eprintf "collapse!~n"))
     (text "Left")
     (text "Right")))))
