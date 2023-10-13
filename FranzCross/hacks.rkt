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
  (define root
    (let loop ([w window])
      (define w-parent
        (send w get-parent))
      (if w-parent (loop w-parent) w)))
  (define-values (screen-x screen-y)
    (send window
          client->screen
          (send event get-x)
          (send event get-y)))
  (define-values (root-client-x root-client-y)
    (send root
          screen->client
          screen-x
          screen-y))
  (send event set-x root-client-x)
  (send event set-y root-client-y))
