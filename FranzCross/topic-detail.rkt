#lang racket/gui/easy

(require franz/broker
         (submod franz/workspace rpc)
         racket/format
         racket/string
         "common.rkt"
         "config-table.rkt"
         "info-view.rkt"
         "observable.rkt"
         "view.rkt"
         (prefix-in m: "window-manager.rkt"))

(provide
 topic-detail)

(define (topic-detail id t [make-status-proc (λ () void)])
  (define-observables
    [@tab 'information]
    [@config null])
  (define-values (replicas in-sync-replicas)
    (for/fold ([replicas 0] [in-sync-replicas 0])
              ([p (in-list (Topic-partitions t))])
      (values
       (+ replicas (length (TopicPartition-replica-node-ids p)))
       (+ in-sync-replicas (length (TopicPartition-in-sync-replica-node-ids p))))))
  (define (reload-config)
    ((make-status-proc) "Fetching Configs")
    (thread
     (lambda ()
       (define status (make-status-proc))
       (@config:= (get-resource-configs (Topic-name t) 'topic id))
       (status "Ready"))))
  (vpanel
   #:alignment '(left top)
   #:margin '(10 10)
   (vpanel
    #:alignment '(left top)
    #:stretch '(#t #f)
    (text
     #:font system-font-l
     (Topic-name t))
    (text
     #:color secondary-color
     #:font system-font-xs
     "Topic"))
   (hpanel
    #:stretch '(#t #f)
    #:min-size '(#f 10))
   (tabs
    '(information config)
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
      ['config
       (reload-config)
       (config-table
        #:get-parent-proc (λ () (m:get-workspace-renderer id))
        @config)]))))

(module+ main
  (render
   (window
    #:size '(800 600)
    (topic-detail
     1
     (make-Topic
      #:name "example-topic"
      #:partitions (list
                    (make-TopicPartition
                     #:id 1
                     #:leader-id 1
                     #:replica-node-ids '(1 2 3)
                     #:in-sync-replica-node-ids '(1 2 3)))
      #:is-internal #f
      #:stats (make-Stats))))))
