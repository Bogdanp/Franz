#lang racket/gui/easy

(require franz/broker
         franz/group
         (submod franz/workspace rpc)
         (prefix-in p: pict)
         racket/format
         "common.rkt"
         "observable.rkt"
         "preference.rkt"
         "thread.rkt"
         "view.rkt")

(provide
 group-detail)

(define (group-detail id g [call-with-status-proc void])
  (define-observables
    [@offsets #f])
  (define/obs @lag
    (let-observable ([offsets @offsets])
      (if offsets
          (for*/sum ([t (GroupOffsets-topics offsets)]
                     [p (GroupTopic-partitions t)])
            (- (GroupPartitionOffset-high-watermark p)
               (GroupPartitionOffset-offset p)))
          0)))
  (define offsets-b
    (make-weak-box @offsets))
  (thread*
   (let loop ()
     (define reload-ival (get-preference 'general:reload-interval 5))
     (define offsets (weak-box-value offsets-b))
     (when offsets
       (call-with-status-proc
        (lambda (status)
          (status "Fetching Offsets")
          (offsets . := . (fetch-offsets (Group-id g) id))))
       (sleep reload-ival)
       (loop))))
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
      (Group-id g))
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
   (state-pill @offsets)))

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

(module+ main
  (render
   (window
    #:size '(800 600)
    (group-detail
     1
     (make-Group
      #:id "ExampleConsumer")))))
