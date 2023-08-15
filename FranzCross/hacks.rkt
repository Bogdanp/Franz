#lang racket/gui/easy

(require racket/class)

(provide
 render-popup-menu*)

(define render-popup-menu*
  (case-lambda
    [(parent view e)
     (render-popup-menu*
      parent view
      (send e get-x)
      (send e get-y))]
    [(parent view x y)
     ;; Enqueue a low-priority callback to allow other events to be
     ;; handled before we block the eventspace, otherwise the popup
     ;; menu might not show up when drawing eg. a canvas-list.
     (gui:queue-callback
      (lambda ()
        (render-popup-menu parent view x y))
      #f)]))

(provide
 make-mouse-event-positions-absolute)

;; Takes a window<%> and a mouse-event% and converts the event's x and
;; y positions to be absolute relative to the top-level window's
;; origin.
(define (make-mouse-event-positions-absolute window event)
  (define-values (x-pos y-pos)
    (let loop ([x 0] [y 0] [w window])
      (define w-parent
        (send w get-parent))
      (if w-parent
          (loop (+ x (send w get-x))
                (+ y (send w get-y))
                w-parent)
          (values x y))))
  (send event set-x (+ x-pos (send event get-x)))
  (send event set-y (+ y-pos (send event get-y))))
