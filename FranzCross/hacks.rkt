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
