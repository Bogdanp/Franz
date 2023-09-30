#lang racket/gui/easy

(require franz/broker
         franz/group
         (submod franz/workspace rpc)
         (prefix-in p: pict)
         racket/format
         "clipboard.rkt"
         "combinator.rkt"
         "common.rkt"
         "hacks.rkt"
         "observable.rkt"
         "mixin.rkt"
         "preference.rkt"
         "thread.rkt"
         "view.rkt"
         (prefix-in m: "window-manager.rkt"))

(provide
 group-detail)

(define (group-detail id g [call-with-status-proc (λ (proc) (proc void))]
                      #:get-parent-proc [get-parent-renderer (λ () (m:get-workspace-renderer id))])
  (define-observables
    [@offsets #f]
    [@current #f]
    [@poffset #f])
  (define/obs @lag
    (let-observable ([offsets @offsets])
      (if offsets
          (for*/sum ([t (GroupOffsets-topics offsets)]
                     [p (GroupTopic-partitions t)])
            (- (GroupPartitionOffset-high-watermark p)
               (GroupPartitionOffset-offset p)))
          0)))
  (define/obs @selection+topics
    (let-observable ([offsets @offsets]
                     [current @current])
      (define topics
        (and offsets (GroupOffsets-topics offsets)))
      (and
       topics
       (cons
        (findf
         (λ (t)
           (equal? current (GroupTopic-name t)))
         topics)
        topics))))
  (define (reload)
    (call-with-status-proc
     (lambda (status)
       (status "Fetching Offsets")
       (define offsets (fetch-offsets (Group-id g) id))
       (define topics (GroupOffsets-topics offsets))
       (define current-topic ^@current)
       (define current-topic-exists?
         (and current-topic
              (findf
               (λ (t)
                 (equal? current-topic (GroupTopic-name t)))
               topics)))
       (unless (or current-topic-exists? (null? topics))
         (@current:= (GroupTopic-name (car topics))))
       (@offsets . := . offsets))))
  (define reload-thd
    (thread*
     (with-handlers ([exn:break? void])
       (let loop ()
         (reload)
         (sleep (get-preference 'general:reload-interval 5))
         (loop)))))
  (add-hooks
   #:on-destroy
   (lambda ()
     (break-thread reload-thd))
   (vpanel
    #:alignment '(left top)
    #:margin '(10 10)
    (hpanel
     #:stretch '(#t #f)
     (vpanel
      #:alignment '(left top)
      #:stretch '(#t #f)
      (text
       #:font system-font-l
       (~truncate (Group-id g)))
      (text
       #:color secondary-color
       #:font system-font-xs
       "Group"))
     (spacer)
     (vpanel
      #:alignment '(right top)
      #:stretch '(#t #f)
      (text
       #:color secondary-color
       #:font system-font-xs
       "Messages Behind:")
      (text
       #:font system-font-l
       (@lag . ~> . ~a))))
    (state-pill @offsets)
    (match-view @selection+topics
      [#f (spacer)]
      [`(#f) (text "No committed offsets.")]
      [`(,topic . ,topics)
       (vpanel
        #:alignment '(left top)
        (hpanel
         #:stretch '(#t #f)
         (choice
          topics
          #:choice->label
          GroupTopic-name
          #:selection topic
          (λ (topic)
            (@current:= (GroupTopic-name topic))))
         (spacer)
         (button
          "Reset..."
          (lambda ()
            (render
             (reset-offset-dialog
              #:targets '(earliest latest)
              #:init-target 'latest
              #:reset-proc (lambda (target _offset)
                             (reset-topic-offsets
                              (Group-id g)
                              (GroupTopic-name topic)
                              target
                              id)
                             (reload)))
             (get-parent-renderer)))))
        (table
         '("Partition" "Offset" "Lag" "Member ID" "Host" "Client ID")
         #:column-widths
         '((0 100)
           (1 80)
           (2 80)
           (3 150)
           (4 150)
           (5 150))
         (list->vector (GroupTopic-partitions topic))
         #:entry->row
         (lambda (p)
           (vector
            (format "Partition ~a" (GroupPartitionOffset-partition-id p))
            (number->string (GroupPartitionOffset-offset p))
            (number->string (- (GroupPartitionOffset-high-watermark p)
                               (GroupPartitionOffset-offset p)))
            (~optional-str (GroupPartitionOffset-member-id p))
            (~optional-str (GroupPartitionOffset-client-id p))
            (~optional-str (GroupPartitionOffset-client-host p))))
         #:mixin
         (mix-context-event
          (lambda (event)
            (define poffset ^@poffset)
            (when poffset
              (render-popup-menu*
               (get-parent-renderer)
               (popup-menu
                (menu-item
                 "Copy Offset"
                 (λ ()
                   (put-clipboard
                    (number->string
                     (GroupPartitionOffset-offset poffset)))))
                (menu-item
                 "Copy Member ID"
                 (λ ()
                   (put-clipboard (or (GroupPartitionOffset-member-id poffset) ""))))
                (menu-item
                 "Copy Client ID"
                 (λ ()
                   (put-clipboard (or (GroupPartitionOffset-client-id poffset) ""))))
                (menu-item-separator)
                (menu-item
                 "Reset Offset..."
                 (λ ()
                   (render
                    (reset-offset-dialog
                     (GroupPartitionOffset-offset poffset)
                     #:reset-proc
                     (lambda (target offset)
                       (reset-partition-offset
                        (Group-id g)
                        (GroupTopic-name topic)
                        (GroupPartitionOffset-partition-id poffset)
                        target
                        offset
                        id)
                       (reload)))
                    (get-parent-renderer)))))
               event))))
         #:selection
         (let-observable ([o @poffset])
           (and o (for/first ([(p idx) (in-indexed (in-list (GroupTopic-partitions topic)))]
                              #:when (eqv?
                                      (GroupPartitionOffset-partition-id p)
                                      (GroupPartitionOffset-partition-id o)))
                    idx)))
         (lambda (event entries selection)
           (case event
             [(select)
              (@poffset:= (and selection (vector-ref entries selection)))]))))]))))

(define (state-pill @offsets)
  (pict-canvas
   @offsets
   #:min-size '(#f 30)
   #:stretch '(#t #f)
   #:style '(transparent)
   (λ (offsets)
     (define state (if offsets (GroupOffsets-state offsets) 'loading))
     (define-values (bg-color fg-color)
       (case state
         [(stable) (values (color #x00CC00FF) white)]
         [(dead) (values (color #xCC0000FF) white)]
         [else (values white primary-color)]))
     (define text-pict
       (p:inset
        (p:colorize
         (p:text
          (string-upcase (symbol->string state))
          (font #:weight 'bold system-font (sub1 font-size-xs)))
         fg-color)
        5 2))
     (p:inset
      (p:lc-superimpose
       (p:filled-rounded-rectangle
        (p:pict-width text-pict)
        (p:pict-height text-pict)
        5 ;corner-radius
        #:color bg-color
        #:border-color bg-color)
       text-pict)
      0 10))))

(define (reset-offset-dialog [offset 0]
                             #:targets [targets '(earliest offset latest)]
                             #:init-target [init-target 'offset]
                             #:cancel-proc [cancel void]
                             #:reset-proc [reset void])
  (define close! void)
  (define-observables
    [@target init-target]
    [@offset offset])
  (dialog
   #:title "Reset Offset..."
   #:mixin
   (mix-close-window void (λ (close!-proc)
                            (set! close! close!-proc)))
   (vpanel
    #:stretch '(#t #f)
    #:margin '(10 20)
    (labeled
     "Reset to:"
     (choice
      targets
      #:choice->label (compose1 string-titlecase symbol->string)
      #:selection @target
      @target:=))
    (match-view @target
      ['offset (labeled
                "Offset:"
                (validated-input
                 #:text->value string->number
                 @offset (drop1 @offset:=)))]
      [_ (spacer)])
    (labeled
     ""
     (hpanel
      #:alignment '(right center)
      (button
       "Cancel"
       (lambda ()
         (cancel)
         (close!)))
      (button
       #:style '(border)
       "Reset"
       (lambda ()
         (define target ^@target)
         (reset target (and (eq? target 'offset) ^@offset))
         (close!))))))))

(module+ main
  (require racket/gui/easy/debugger
           "testing.rkt")

  #;
  (render
   (reset-offset-dialog
    #:reset-proc (λ (target) (eprintf "target: ~s~n" target))
    500))

  (call-with-testing-context
   (lambda (id)
     (start-debugger)
     (define root
       (render
        (window
         #:size '(800 600)
         (group-detail
          id
          (make-Group #:id "example-group")
          #:get-parent-proc (λ () root)))))
     (void))))
