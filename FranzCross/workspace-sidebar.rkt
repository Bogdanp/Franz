#lang racket/gui/easy

(require franz/broker
         franz/schema-registry/schema
         (prefix-in p: pict)
         racket/list
         racket/match
         "canvas-list.rkt"
         "common.rkt"
         "observable.rkt"
         "preference.rkt")

(provide
 workspace-sidebar)

(struct Header (label collapsed?)
  #:transparent)

(define (workspace-sidebar @metadata
                           #:select-action [select-action void]
                           #:context-action [context-action void])
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
  (define/obs @items
    (let-observable ([m @metadata]
                     [s @collapse-states])
      (match-define
        (hash-table
         ['brokers brokers-collapsed?]
         ['topics topics-collapsed?]
         ['groups groups-collapsed?]
         ['schemas schemas-collapsed?])
        s)
      (flatten
       (list
        (Header 'brokers brokers-collapsed?)
        (if brokers-collapsed? null (Metadata-brokers m))
        (Header 'topics topics-collapsed?)
        (if topics-collapsed? null (Metadata-topics m))
        (Header 'groups groups-collapsed?)
        (if groups-collapsed? null (Metadata-groups m))
        (Header 'schemas schemas-collapsed?)
        (if schemas-collapsed? null (Metadata-schemas m))))))
  (canvas-list
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
   #:item=?
   (λ (a b)
     (if (and (Header? a)
              (Header? b))
         (eq? (Header-label a)
              (Header-label b))
         (equal? a b)))
   #:action
   (λ (type item _event)
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
          (context-action item))]
       [else (void)]))))

(define (Header-pict hdr state w h)
  (match-define (Header label _collapsed?) hdr)
  (define bg-color
    (case state
      [(hover selected) (color #xEEEEEEFF)]
      [else white]))
  (p:lc-superimpose
   (p:filled-rectangle
    #:color bg-color
    #:border-color bg-color
    #:border-width 1
    w h)
   (p:inset
    (p:text
     (case label
       [(brokers) "Brokers"]
       [(topics) "Topics"]
       [(groups) "Consumer Groups"]
       [(schemas) "Schemas"])
     system-font-s)
    5 0)))

(define (Broker-pict b state w h)
  (standard-pict
   (format
    "~a:~a"
    (Broker-host b)
    (Broker-port b))
   state w h))

(define (Topic-pict t state w h)
  (standard-pict
   (Topic-name t)
   state w h))

(define (Group-pict g state w h)
  (standard-pict
   (Group-id g)
   state w h))

(define (Schema-pict s state w h)
  (standard-pict
   (Schema-name s)
   state w h))

(define (standard-pict label state w h)
  (define-values (bg-color fg-color)
    (case state
      [(hover) (values (color #xEEEEEEFF) primary-color)]
      [(selected) (values selection-background-color selection-primary-color)]
      [else (values white primary-color)]))
  (p:lc-superimpose
   (p:filled-rectangle
    #:color bg-color
    #:border-color bg-color
    #:border-width 1
    w h)
   (p:inset
    (p:colorize
     (p:text label system-font-s)
     fg-color)
    15 0)))

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
                                   #:in-sync-replica-node-ids '(1))))
                   (make-Topic
                    #:name "Example Topic 2"
                    #:partitions (list
                                  (make-TopicPartition
                                   #:id 1
                                   #:leader-id 1
                                   #:replica-node-ids '(1)
                                   #:in-sync-replica-node-ids '(1)))))
         #:groups null
         #:schemas null))))))
