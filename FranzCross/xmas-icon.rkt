#lang racket/gui/easy

(require racket/class
         racket/math
         "common.rkt"
         "observable.rkt")

(provide
 xmas?
 xmas-icon)

(define xmas?
  (let ([d (seconds->date (current-seconds))])
    (define c (+ (* (date-month d) 100) (date-day d)))
    (or (<= c  115)
        (>= c 1115))))

(define (xmas-icon)
  (define-observables
    [@xmas-tick 0])
  (define tick-thd
    (void
     (thread
      (lambda ()
        (define r (* 2 (/ 1 25.0)))
        (define t (current-inexact-monotonic-milliseconds))
        (let loop ()
          (@xmas-tick:=
           (exact-round
            (/ (- (current-inexact-monotonic-milliseconds) t)
               (* 1000 r))))
          (sleep r)
          (loop))))))
  (add-hooks
   #:on-destroy
   (lambda ()
     (kill-thread tick-thd))
   (canvas
    @xmas-tick
    #:style '(transparent)
    #:stretch '(#f #f)
    #:min-size '(128 128)
    (lambda (dc tick)
      (define idx (modulo tick (vector-length xmas-icon-bmps)))
      (define bmp (vector-ref xmas-icon-bmps idx))
      (send dc draw-bitmap bmp 0 0)))))

(module+ main
  (render
   (window
    (xmas-icon))))
