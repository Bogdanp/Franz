#lang racket/gui/easy

(require racket/class
         racket/gui/easy/color)

(provide
 split-view)

(define divider-background-color
  (color #xEDEDEDFF))

(define (make-divider [x-min 0])
  (define/obs @x x-min)
  (define divider
    (canvas
     #f
     #:min-size '(3 #f)
     #:stretch '(#f #t)
     #:mixin
     (λ (%)
       (class %
         (super-new)

         (define regular-cursor
           (make-object gui:cursor% 'arrow))
         (define drag-cursor
           (make-object gui:cursor% 'size-e/w))

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
                     (define dx (- dst src))
                     (@x . <~ . (λ (x) (max (+ x dx) x-min)))
                     (set! move-scheduled? #f))))))))))
     (lambda (dc _)
       (define-values (w h)
         (send dc get-size))
       (send dc set-pen (send gui:the-pen-list find-or-create-pen "black" 0 'transparent))
       (send dc set-brush divider-background-color 'solid)
       (send dc draw-rectangle 0 0 w h))))
  (values @x divider))

(define (split-view lhs rhs)
  (define-values (@divider-x divider)
    (make-divider 240))
  (hpanel
   (hpanel
    #:min-size (@divider-x . ~> . (λ (x) (list x #f)))
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
     (text "Left")
     (text "Right")))))
