#lang racket/gui/easy

(require franz/broker
         franz/schema-registry/schema
         (prefix-in p: pict)
         racket/format
         racket/list
         racket/match
         racket/math
         racket/string
         "canvas-list.rkt"
         "combinator.rkt"
         "common.rkt"
         "mixin.rkt"
         "observable.rkt"
         "preference.rkt")

(provide
 workspace-sidebar)

(struct Header (label collapsed?)
  #:transparent)

(define chevron-e (p:bitmap chevron-e-bmp))
(define chevron-s (p:bitmap chevron-s-bmp))

(define (workspace-sidebar @metadata
                           #:select-action [select-action void]
                           #:context-action [context-action void]
                           #:new-topic-action [new-topic-action void])
  (define/obs @collapse-states
    (get-preference
     'workspace-sidebar:collapse-states
     (lambda ()
       (for/hasheq ([k (in-list '(brokers topics groups schemas))])
         (values k #f)))))
  (obs-observe!
   @collapse-states
   (lambda (states)
     (put-preference 'workspace-sidebar:collapse-states states)))
  (define/obs @filter "")
  (define/obs @items
    (let-observable ([m @metadata]
                     [s @collapse-states]
                     [f @filter])
      (match-define
        (hash-table
         ['brokers brokers-collapsed?]
         ['topics topics-collapsed?]
         ['groups groups-collapsed?]
         ['schemas schemas-collapsed?])
        s)
      (define all-items
        (flatten
         (list
          (Header 'brokers brokers-collapsed?)
          (if brokers-collapsed? null (Metadata-brokers m))
          (Header 'topics topics-collapsed?)
          (if topics-collapsed? null (Metadata-topics m))
          (Header 'groups groups-collapsed?)
          (if groups-collapsed? null (Metadata-groups m))
          (Header 'schemas schemas-collapsed?)
          (if schemas-collapsed? null (Metadata-schemas m)))))
      (if (equal? f "")
          all-items
          (filter (λ (item) (item-matches-filter? item f)) all-items))))
  (vpanel
   (canvas-list
    #:mixin mix-initial-focus
    @items
    #:item-height 30
    (λ (item state dc w h)
      (define pict
        ((cond
           [(Header? item) Header-pict]
           [(Broker? item) Broker-pict]
           [(Topic? item) Topic-pict]
           [(Group? item) Group-pict]
           [(Schema? item) Schema-pict]
           [else (error 'workspace-sidebar "unexpected item: ~s" item)])
         item state w h))
      (p:draw-pict pict dc 0 0))
    #:item=? item=?
    #:action
    (λ (type item event)
      (case type
        [(select)
         (unless (Header? item)
           (select-action item))]
        [(dbclick)
         (when (Header? item)
           (update-observable @collapse-states
             (hash-update it (Header-label item) not)))]
        [(context)
         (unless (Header? item)
           (context-action item event))]
        [else (void)])))
   (hpanel
    #:stretch '(#t #f)
    (button plus-bmp new-topic-action)
    (input @filter (drop1 (λ:= @filter))))))

(define (item=? a b)
  (cond
    [(Header? a)
     (and (Header? b)
          (eq? (Header-label a)
               (Header-label b)))]
    [(Broker? a)
     (and (Broker? b)
          (eqv? (Broker-id a)
                (Broker-id b)))]
    [(Topic? a)
     (and (Topic? b)
          (equal? (Topic-name a)
                  (Topic-name b)))]
    [(Group? a)
     (and (Group? b)
          (equal? (Group-id a)
                  (Group-id b)))]
    [(Schema? a)
     (and (Schema? b)
          (equal? (Schema-id a)
                  (Schema-id b)))]
    [else #f]))

(define (item-matches-filter? item s)
  (or (Header? item)
      (string-contains?
       (string-downcase
        ((or (and (Broker? item) Broker-host)
             (and (Topic? item) Topic-name)
             (and (Group? item) Group-id)
             (and (Schema? item) Schema-name))
         item))
       (string-downcase s))))

(define (Header-pict hdr state w h)
  (match-define (Header label collapsed?) hdr)
  (define bg-color
    (case state
      [(hover selected) hover-background-color]
      [else white]))
  (p:lc-superimpose
   (p:filled-rectangle
    #:color bg-color
    #:border-color bg-color
    #:border-width 1
    w h)
   (p:inset (if collapsed? chevron-e chevron-s) 5)
   (p:inset
    (p:text
     (case label
       [(brokers) "Brokers"]
       [(topics) "Topics"]
       [(groups) "Consumer Groups"]
       [(schemas) "Schemas"])
     system-font-s)
    26 0)))

(define (Broker-pict b state w h)
  (define label
    (format
     "~a:~a"
     (Broker-host b)
     (Broker-port b)))
  (standard-pict
   #:label label
   state w h))

(define (Topic-pict t state w h)
  (standard-pict
   #:label (Topic-name t)
   #:count (Stats-sum-lag (Topic-stats t))
   state w h))

(define (Group-pict g state w h)
  (standard-pict
   #:label (Group-id g)
   #:count (Stats-sum-lag (Group-stats g))
   state w h))

(define (Schema-pict s state w h)
  (standard-pict
   #:label (Schema-name s)
   state w h))

(define (standard-pict state w h
                       #:label label
                       #:count [count #f])
  (define l-padding 26)
  (define r-padding 10)
  (define count-str (and count (~count count)))
  (define-values (bg-color fg-color secondary-fg-color)
    (case state
      [(hover) (values hover-background-color primary-color secondary-color)]
      [(selected) (values selection-background-color selection-primary-color selection-secondary-color)]
      [else (values white primary-color secondary-color)]))
  (define label-pict
    (p:colorize
     (p:text label system-font-s)
     fg-color))
  (define count-pict
    (and count-str
         (p:colorize
          (p:text count-str system-mono-font-s)
          secondary-fg-color)))
  (define content-w
    (max (- w l-padding r-padding) 0))
  (define space-w
    (max (- content-w
            (p:pict-width label-pict)
            (if count-pict (p:pict-width count-pict) 0))
         0))
  (cond
    [(and (<= space-w 1)
          (>= (string-length label) 4))
     (standard-pict
      #:label (string-append (substring label 0 (- (string-length label) 4)) "…")
      #:count count
      state w h)]
    [else
     (define text-pict
       (if count-str
           (p:pin-over
            (p:lc-superimpose
             (p:ghost
              (p:filled-rectangle
               content-w h))
             label-pict)
            (- content-w (p:pict-width count-pict))
            (- (/ h 2)
               (/ (p:pict-height count-pict) 2))
            count-pict)
           label-pict))
     (p:lc-superimpose
      (p:filled-rectangle
       #:color bg-color
       #:border-color bg-color
       #:border-width 1
       w h)
      (p:inset text-pict l-padding 0 r-padding 0))]))

(define (~count n)
  (cond
    [(>= n 1e9) (~a (exact-round (/ n 1e9)) "B")]
    [(>= n 1e6) (~a (exact-round (/ n 1e6)) "M")]
    [(>= n 1e3) (~a (exact-round (/ n 1e3)) "k")]
    [else (~a n)]))

(module+ main
  (render
   (window
    #:title "Workspace Sidebar"
    #:size '(240 600)
    (workspace-sidebar
     #:select-action
     (λ (item)
       (eprintf "select: ~s~n" item))
     #:context-action
     (λ (item)
       (eprintf "context: ~s~n" item))
     (@ (make-Metadata
         #:brokers (list
                    (make-Broker
                     #:id 1
                     #:host "kafka-1"
                     #:port 9092
                     #:is-controller #t))
         #:topics (list
                   (make-Topic
                    #:name "Example Topic 1"
                    #:partitions (list
                                  (make-TopicPartition
                                   #:id 1
                                   #:leader-id 1
                                   #:replica-node-ids '(1)
                                   #:in-sync-replica-node-ids '(1)))
                    #:stats (make-Stats
                             #:min-lag 0
                             #:max-lag 100
                             #:sum-lag 10000))
                   (make-Topic
                    #:name "Example Topic 2"
                    #:partitions (list
                                  (make-TopicPartition
                                   #:id 1
                                   #:leader-id 1
                                   #:replica-node-ids '(1)
                                   #:in-sync-replica-node-ids '(1))))
                   (make-Topic
                    #:name "Supercalifragilisticexpialidocious"
                    #:partitions (list
                                  (make-TopicPartition
                                   #:id 1
                                   #:leader-id 1
                                   #:replica-node-ids '(1)
                                   #:in-sync-replica-node-ids '(1)))
                    #:stats (make-Stats
                             #:min-lag 0
                             #:max-lag 100
                             #:sum-lag 10000000000)))
         #:groups null
         #:schemas null))))))
