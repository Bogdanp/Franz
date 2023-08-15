#lang racket/gui/easy

(require racket/list
         "combinator.rkt"
         "mixin.rkt"
         "observable.rkt"
         "view.rkt")

(provide
 new-topic-dialog)

(define (labeled* label v #:alignment [alignment '(right center)])
  (labeled
   #:alignment alignment
   #:width 70
   label v))

(define (new-topic-dialog #:cancel-action [cancel-action void]
                          #:create-action [create-action void])
  (define close! void)
  (define-observables
    [@name "example-topic"]
    [@partitions 1]
    [@replication-factor 1]
    [@options null]
    [@selection #f])
  (define (add-option [entry #f])
    (render
     (if entry
         (editor
          #:name (car entry)
          #:value (cdr entry)
          #:title "Edit Config"
          (λ (name value)
            (update-observable @options
              (define index
                (index-of it entry))
              (if index
                  (append
                   (take it index)
                   (list (cons name value))
                   (drop it (add1 index)))
                  it))))
         (editor
          (λ (name value)
            (update-observable @options
              (append it (list (cons name value)))))))))
  (dialog
   #:title "New Topic"
   #:size '(580 #f)
   #:mixin (mix-close-window
            cancel-action
            (λ (close!-proc)
              (set! close! close!-proc)))
   (vpanel
    #:margin '(10 5)
    #:stretch '(#t #f)
    (labeled*
     "Name:"
     (validated-input
      #:text->value non-empty-string
      #:mixin mix-initial-focus
      @name (drop1 @name:=)))
    (hpanel
     #:stretch '(#t #f)
     (labeled*
      "Partitions:"
      (validated-input
       #:text->value positive-number
       (@partitions . ~> . number->string)
       (drop1 @partitions:=)))
     (labeled*
      "Replication Factor:"
      (validated-input
       #:text->value positive-number
       (@replication-factor . ~> . number->string)
       (drop1 @replication-factor:=))))
    (labeled*
     "Options:"
     #:alignment '(right top)
     (vpanel
      #:alignment '(left top)
      (table
       #:min-size '(#f 100)
       #:margin '(2 0)
       '("Name" "Value")
       (@options . ~> . list->vector)
       #:entry->row
       (λ (e)
         (vector
          (car e)
          (cdr e)))
       #:column-widths
       '((0 130)
         (1 130))
       #:selection @selection
       (λ (event entries selection)
         (case event
           [(click)
            (@selection:= selection)]
           [(dclick)
            (@selection:= selection)
            (add-option (and selection (vector-ref entries selection)))])))
      (hpanel
       (button "+" add-option)
       (button "-" (λ ()
                     (define selection ^@selection)
                     (when selection
                       (update-observable @options
                         (@selection:= #f)
                         (append
                          (take it selection)
                          (drop it (add1 selection))))))))))
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
         ^@name
         ^@partitions
         ^@replication-factor
         ^@options)
        (close!)))))))

(define (editor [action void]
                #:name [name ""]
                #:value [value ""]
                #:title [title "Add Config"])
  (define close! void)
  (define-observables
    [@name name]
    [@value value]
    [@name-valid? #f]
    [@value-valid? #f])
  (dialog
   #:title title
   #:size '(480 #f)
   #:mixin (mix-close-window
            void
            (λ (close!-proc)
              (set! close! close!-proc)))
   (vpanel
    (labeled*
     "Name:"
     (validated-input
      #:text->value non-empty-string
      #:valid? @name-valid?
      #:mixin (λ (%)
                (mix-initial-focus
                 ((mix-typeahead (hash-keys completions)) %)))
      @name (drop1 @name:=)))
    (labeled*
     "Value:"
     (validated-input
      #:text->value non-empty-string
      #:valid? @value-valid?
      #:mixin (mix-typeahead
               (let-observable ([name @name])
                 (hash-ref completions name null)))
      @value (drop1 @value:=)))
    (hpanel
     #:alignment '(right center)
     (button
      "OK"
      #:style '(border)
      #:enabled?
      (let-observable ([name-valid? @name-valid?]
                       [value-valid? @value-valid?])
        (and name-valid? value-valid?))
      (λ ()
        (action ^@name ^@value)
        (close!)))))))

(define (non-empty-string s)
  (and (> (string-length s) 0) s))

(define (positive-number s)
  (define maybe-number
    (string->number s))
  (and maybe-number (> maybe-number 0)))

(define completions
  (hash
   "cleanup.policy" '("compact" "delete")
   "compression.type" '("gzip" "lz4" "producer" "snappy" "uncompressed" "zstd")
   "delete.retention.ms" null
   "file.delete.delay.ms" null
   "flush.messages" null
   "flush.ms" null
   "follower.replication.throttled.replicas" null
   "index.interval.bytes" null
   "leader.replication.throttled.replicas" null
   "max.compaction.lag.ms" null
   "max.message.bytes" null
   "message.timestamp.difference.max.ms" null
   "message.timestamp.type" '("CreateTime" "LogAppendTime")
   "min.cleanable.dirty.ratio" null
   "min.compaction.lag.ms" null
   "min.insync.replicas" null
   "preallocate" null
   "retention.bytes" null
   "retention.ms" null
   "segment.bytes" null
   "segment.index.bytes" null
   "segment.jitter.ms" null
   "segment.ms" null
   "unlcean.leader.election.enable" null
   "message.downconversion.enable" null))

(module+ main
  (render
   (new-topic-dialog
    #:create-action
    (λ (name partitions replication-factor options)
      (eprintf "name: ~s partitions: ~s replication-factor: ~s options: ~s~n"
               name partitions replication-factor options)))))
