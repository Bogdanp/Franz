#lang racket/gui/easy

(require (submod franz/script rpc)
         "editor.rkt"
         "observable.rkt")

(provide
 scripting-window)

(define (scripting-window id topic
                          #:enabled? [@enabled? (@ #t)]
                          #:apply-proc [apply-proc void])
  (define-observables
    [@script (get-script topic id)]
    [@active? #f])
  (obs-observe!
   @enabled?
   (lambda (enabled?)
     (unless enabled?
       (@active?:= #f)
       (deactivate-script topic id))))
  (window
   #:title (format "[~a] Script" topic)
   #:size '(800 600)
   (vpanel
    (hpanel
     #:stretch '(#t #f)
     #:alignment '(right center)
     (button
      #:enabled?
      (let-observable ([active? @active?]
                       [enabled? @enabled?])
        (and enabled? (not active?)))
      "Apply"
      (lambda ()
        (apply-proc (obs-peek @script))))
     (button
      #:style '(border)
      #:enabled? @enabled?
      (let-observable ([active? @active?])
        (if active? "Deactivate" "Activate"))
      (lambda ()
        (let-observable ([active? @active?])
          (if active?
              (deactivate-script topic id)
              (activate-script (obs-peek @script) topic id))
          (@active?:= (not active?))))))
    (editor
     (obs-peek @script)
     (lambda (script)
       (deactivate-script topic id)
       (@active?:= #f)
       (@script:= script))))))

(module+ main
  (require "testing.rkt")
  (call-with-testing-context
   (lambda (id)
     (render
      (scripting-window id "example-topic")))))
