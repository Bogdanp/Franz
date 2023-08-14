#lang racket/gui/easy

(require "combinator.rkt"
         "mixin.rkt"
         "view.rkt")

(provide
 new-topic-dialog)

(define (new-topic-dialog #:cancel-action [cancel-action void]
                          #:create-action [create-action void])
  (define close! void)
  (define/obs @name "example-topic")
  (define/obs @partitions 1)
  (define/obs @replication-factor 1)
  (dialog
   #:title "New Topic"
   #:size '(960 360)
   #:mixin (mix-close-window
            cancel-action
            (λ (close!-proc)
              (set! close! close!-proc)))
   (vpanel
    #:stretch '(#t #f)
    (labeled
     "Name:"
     (validated-input
      #:text->value (λ (s) (and (> (string-length s) 0) s))
      @name (drop1 (λ:= @name))))
    (hpanel
     #:stretch '(#t #f)
     (labeled
      "Partitions:"
      (validated-input
       #:text->value positive-number
       (@partitions . ~> . number->string)
       (drop1 (λ:= @partitions))))
     (labeled
      "Replication Factor:"
      (validated-input
       #:text->value positive-number
       (@replication-factor . ~> . number->string)
       (drop1 (λ:= @replication-factor)))))
    (hpanel
     #:stretch '(#t #f)
     #:alignment '(right center)
     (button
      "Cancel"
      (λ ()
        (cancel-action)
        (close!)))
     (button
      "Create Topic"
      #:style '(border)
      (λ ()
        (create-action
         (obs-peek @name)
         (obs-peek @partitions)
         (obs-peek @replication-factor)
         (hasheq))
        (close!)))))))

(define (positive-number s)
  (define maybe-number
    (string->number s))
  (and maybe-number (> maybe-number 0)))

(module+ main
  (render
   (new-topic-dialog
    #:create-action
    (λ (name partitions replication-factor options)
      (eprintf "name: ~s partitions: ~s replication-factor: ~s options: ~s~n"
               name partitions replication-factor options)))))
