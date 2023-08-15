#lang racket/gui/easy

(require "common.rkt")

(provide
 infos)

(define (infos is)
  (apply
   vpanel
   #:alignment '(left top)
   #:margin '(10 10)
   (for/list ([i (in-list is)])
     (hpanel
      #:stretch '(#t #f)
      (hpanel
       #:min-size '(100 #f)
       #:stretch '(#f #t)
       (text
        #:font system-font-s
        (car i)))
      (text
       #:font (font #:weight 'bold system-font font-size-s)
       (cdr i))))))

(module+ main
  (render
   (window
    #:title "Infos"
    #:size '(800 600)
    (infos
     `(("Address"    . "127.0.0.1:9092")
       ("Controller" . "yes"))))))
