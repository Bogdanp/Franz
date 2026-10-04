#lang racket/gui/easy

(require franz/version
         "common.rkt")

(provide
 about-window)

(define (about-window)
  (window
   #:size '(320 240)
   #:title "About Franz"
   #:style '(no-resize-border)
   (vpanel
    #:margin '(20 20)
    (image
     #:size '(64 64)
     icon_512x512.png)
    (text #:font system-font-xl "Franz")
    (text (format "Version ~a" franz-version))
    (spacer)
    (text (format "Copyright ~a CLEARTYPE SRL."
                  (date-year (seconds->date (current-seconds)))))
    (text "Released under the BSD 3-Clause License."))))

(module+ main
  (render
   (about-window)))
