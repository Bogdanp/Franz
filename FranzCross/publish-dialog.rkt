#lang racket/gui/easy

(require franz/broker
         (submod franz/workspace rpc)
         "editor.rkt"
         "mixin.rkt"
         "observable.rkt"
         "view.rkt")

(provide
 publish-dialog)

(define (publish-dialog id [selected-topic-name #f]
                        #:publish-action [publish void]
                        #:cancel-action [cancel void])
  (define close! void)
  (define metadata
    (get-metadata #t id))
  (define labeled*
    (make-labeled 80))
  (define selected-topic
    (or
     (findf
      (λ (t) (equal? (Topic-name t) selected-topic-name))
      (Metadata-topics metadata))
     (let ([topics (Metadata-topics metadata)])
       (and (not (null? topics)) (car topics)))))
  (define-observables
    [@metadata metadata]
    [@topic selected-topic]
    [@topic-partition (and selected-topic (car (Topic-partitions selected-topic)))]
    [@key "{}"]
    [@key-enabled? #t]
    [@value "{}"]
    [@value-enabled? #t])
  (dialog
   #:title "Publish Record"
   #:mixin
   (mix-close-window void (λ (close!-proc)
                            (set! close! close!-proc)))
   (vpanel
    #:margin '(10 10)
    (labeled*
     "Topic:"
     (choice
      (@metadata . ~> . Metadata-topics)
      #:choice->label (compose1 ~truncate Topic-name)
      #:selection @topic
      (lambda (topic)
        (@topic:= topic)
        (@topic-partition:= (car (Topic-partitions topic))))))
    (labeled*
     "Partition:"
     (choice
      #:enabled? (@topic . ~> . (compose1 not not))
      (let-observable ([t @topic])
        (if t (Topic-partitions t) null))
      #:choice->label (compose1 number->string TopicPartition-id)
      #:selection @topic-partition
      @topic-partition:=))
    (labeled*
     (vpanel
      #:alignment '(right top)
      (text "Key:")
      (checkbox
       #:checked? @key-enabled?
       @key-enabled?:=))
     #:alignment '(right top)
     (hpanel
      #:min-size '(#f 100)
      (editor #:lang 'json ^@key @key:=)))
    (labeled*
     (vpanel
      #:alignment '(right top)
      (text "Value:")
      (checkbox
       #:checked? @value-enabled?
       @value-enabled?:=))
     #:alignment '(right top)
     (hpanel
      #:min-size '(#f 100)
      (editor #:lang 'json ^@value @value:=)))
    (labeled*
     ""
     (hpanel
      (button
       "Publish"
       #:style '(border)
       (lambda ()
         (publish
          (Topic-name ^@topic)
          (TopicPartition-id ^@topic-partition)
          (and ^@key-enabled? ^@key)
          (and ^@value-enabled? ^@value))
         (close!)))
      (button
       "Cancel"
       (lambda ()
         (cancel)
         (close!))))))))

(module+ main
  (require "testing.rkt")
  (call-with-testing-context
   (lambda (id)
     (render
      (publish-dialog id "example-topic")))))
