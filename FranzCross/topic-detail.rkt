#lang racket/gui/easy

(require franz/broker
         (submod franz/workspace rpc)
         racket/format
         racket/string
         "common.rkt"
         "config-table.rkt"
         "info-view.rkt"
         "observable.rkt"
         "records-table.rkt"
         "thread.rkt"
         "view.rkt"
         (prefix-in m: "window-manager.rkt"))

(provide
 topic-detail)

(define (topic-detail id t
                      [open-consumer-group void]
                      [call-with-status-proc void])
  (define-observables
    [@tab 'information]
    [@groups null]
    [@config null])
  (define-values (replicas in-sync-replicas)
    (for/fold ([replicas 0]
               [in-sync-replicas 0])
              ([p (in-list (Topic-partitions t))])
      (values
       (+ replicas (length (TopicPartition-replica-node-ids p)))
       (+ in-sync-replicas (length (TopicPartition-in-sync-replica-node-ids p))))))
  (define (reload-config)
    (thread*
     (call-with-status-proc
      (lambda (status)
        (status "Fetching Configs")
        (@config:= (get-resource-configs (Topic-name t) 'topic id))))))
  (define (reload-topic-groups)
    (thread*
     (call-with-status-proc
      (lambda (status)
        (status "Finding Groups")
        (@groups:= (find-topic-groups (Topic-name t) id))))))
  (vpanel
   #:alignment '(left top)
   #:margin '(10 10)
   (vpanel
    #:alignment '(left top)
    #:stretch '(#t #f)
    (text
     #:font system-font-l
     (~truncate (Topic-name t)))
    (text
     #:color secondary-color
     #:font system-font-xs
     "Topic"))
   (hpanel
    #:stretch '(#t #f)
    #:min-size '(#f 10))
   (tabs
    '(information records groups config)
    #:choice->label (compose1 string-titlecase symbol->string)
    (λ (event _choices selection)
      (case event
        [(select)
         (@tab:= selection)]))
    (match-view @tab
      ['information
       (vpanel
        (hpanel
         #:stretch '(#t #f)
         (infos
          `(("Partitions" . ,(~a (length (Topic-partitions t))))
            ("Replicas" . ,(~a replicas))
            ("In-sync Replicas" . ,(~a in-sync-replicas))
            ("Internal" . ,(if (Topic-is-internal t) "yes" "no")))))
        (table
         '("Partition ID" "Leader ID" "Replica IDs" "In-sync IDs")
         #:column-widths
         '((0 100)
           (1 100)
           (2 200)
           (3 200))
         (for/vector ([p (in-list (Topic-partitions t))])
           (vector
            (number->string (TopicPartition-id p))
            (number->string (TopicPartition-leader-id p))
            (string-join (map number->string (TopicPartition-replica-node-ids p)) ",")
            (string-join (map number->string (TopicPartition-in-sync-replica-node-ids p)) ",")))))]
      ['records
       (records-table id (Topic-name t) call-with-status-proc)]
      ['groups
       (reload-topic-groups)
       (vpanel
        #:alignment '(left top)
        #:margin '(10 10)
        (text
         #:font (font #:weight 'bold system-font font-size-m)
         "Consumer Groups")
        (match-view @groups
          ['() (text "This topic has no active consumers.")]
          [groups (apply
                   vpanel
                   #:alignment '(left top)
                   (for/list ([g (in-list groups)])
                     (hpanel
                      #:stretch '(#t #f)
                      (text g)
                      (spacer)
                      (button "Open" (λ () (open-consumer-group g))))))]))]
      ['config
       (reload-config)
       (config-table
        #:get-parent-proc
        (lambda ()
          (m:get-workspace-renderer id))
        #:update-action
        (lambda (c)
          (update-resource-configs
           (Topic-name t)
           'topic
           (hash
            (ResourceConfig-name c)
            (ResourceConfig-value c))
           id)
          (reload-config))
        #:delete-action
        (lambda (c)
          (update-resource-configs
           (Topic-name t)
           'topic
           (hash (ResourceConfig-name c) #f)
           id)
          (reload-config))
        @config)]))))

(module+ main
  (require "testing.rkt")
  (call-with-testing-context
   (lambda (id)
     (render
      (window
       #:title "Topic Detail"
       #:size '(800 600)
       (topic-detail
        id
        (make-Topic
         #:name "example-topic"
         #:partitions (list
                       (make-TopicPartition
                        #:id 1
                        #:leader-id 1
                        #:replica-node-ids '(1 2 3)
                        #:in-sync-replica-node-ids '(1 2 3)))
         #:is-internal #f
         #:stats (make-Stats))))))))
