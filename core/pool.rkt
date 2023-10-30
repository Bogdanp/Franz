#lang racket/base

(require (prefix-in k: kafka)
         (prefix-in k: kafka/iterator)
         (prefix-in k: kafka/producer)
         racket/list
         racket/match
         racket/promise
         threading
         "broker.rkt"
         "connection-details.rkt"
         "group.rkt"
         "logger.rkt"
         (prefix-in sr: "schema-registry/generic.rkt")
         (prefix-in sr: "schema-registry/schema.rkt"))

(provide
 pool?
 make-pool
 current-pool
 pool-call-in-context
 pool-open
 pool-close
 pool-get-metadata
 pool-get-resource-configs
 pool-update-resource-configs
 pool-create-topic
 pool-delete-topic
 pool-delete-group
 pool-fetch-offsets
 pool-reset-topic-offsets
 pool-reset-partition-offset
 pool-find-topic-groups
 pool-activate-script
 pool-deactivate-script
 pool-activate-registry
 pool-deactivate-registry
 pool-get-schema
 pool-open-iterator
 pool-get-records
 pool-reset-iterator
 pool-close-iterator
 pool-publish-record
 pool-shutdown)

(struct pool (ch thd))

(define (make-pool)
  (define ch (make-channel))
  (define thd
    (thread/suspend-to-kill
     (lambda ()
       (let loop ([s (make-state)])
         (define res-state
           (with-handlers ([exn:fail?
                            (lambda (e)
                              (begin0 s
                                ((error-display-handler)
                                 (format "pool: ~a" (exn-message e))
                                 e)))])
             (apply
              sync
              (handle-evt
               ch
               (lambda (msg)
                 (match msg
                   [`(call-in-context ,res-ch ,nack ,proc)
                    (define res-promise
                      (delay/thread
                       (proc)))
                    (state-add-req s (req res-promise res-ch nack))]

                   [`(open ,res-ch ,nack ,client)
                    (define-values (client-id next-state)
                      (state-add-client s client))
                    (log-franz-debug "pool: opened client ~a" client-id)
                    (state-add-req next-state (req client-id res-ch nack))]

                   [`(close ,res-ch ,nack ,id)
                    (define client
                      (state-ref-client s id))
                    (when client
                      (k:disconnect-all client)
                      (log-franz-debug "pool: closed client ~a" id))
                    (~> (state-remove-client s id)
                        (state-add-req (req (and client #t) res-ch nack)))]

                   [`(get-metadata ,res-ch ,nack ,id ,reload?)
                    (define metadata
                      (delay/thread
                       (define c (state-ref-client s id))
                       (define meta
                         (if reload?
                             (k:reload-metadata c)
                             (k:client-metadata c)))
                       (define controller-id
                         (k:Metadata-controller-id meta))
                       (define schemas
                         (delay/thread
                          (cond
                            [(state-ref-registry s id) => sr:get-schemas]
                            [else null])))
                       (define groups
                         (for/list ([g (in-list (k:list-groups c))])
                           (make-Group #:id (k:Group-id g))))
                       (define brokers
                         (for/list ([b (in-list (k:Metadata-brokers meta))])
                           (define node-id
                             (k:BrokerMetadata-node-id b))
                           (make-Broker
                            #:id node-id
                            #:host (k:BrokerMetadata-host b)
                            #:port (k:BrokerMetadata-port b)
                            #:rack (k:BrokerMetadata-rack b)
                            #:is-controller (= node-id controller-id))))
                       (define topics
                         (for/list ([t (in-list (k:Metadata-topics meta))])
                           (define parts
                             (for/list ([p (in-list (k:TopicMetadata-partitions t))])
                               (make-TopicPartition
                                #:id (k:PartitionMetadata-id p)
                                #:leader-id (k:PartitionMetadata-leader-id p)
                                #:replica-node-ids (k:PartitionMetadata-replica-node-ids p)
                                #:in-sync-replica-node-ids (k:PartitionMetadata-in-sync-replica-node-ids p))))
                           (make-Topic
                            #:name (k:TopicMetadata-name t)
                            #:partitions (sort parts < #:key TopicPartition-id)
                            #:is-internal (k:TopicMetadata-internal? t))))
                       (define-values (stats-by-topic stats-by-group)
                         (compute-stats c topics groups))
                       (define topics-with-stats
                         (for/list ([t (in-list topics)])
                           (set-Topic-stats t (hash-ref stats-by-topic (Topic-name t) make-Stats))))
                       (define groups-with-stats
                         (for/list ([g (in-list groups)])
                           (set-Group-stats g (hash-ref stats-by-group (Group-id g) make-Stats))))
                       (make-Metadata
                        #:brokers (sort brokers < #:key Broker-id)
                        #:topics (sort topics-with-stats string<? #:key Topic-name)
                        #:groups (sort groups-with-stats string<? #:key Group-id)
                        #:schemas (sort (force schemas) string<? #:key sr:Schema-name))))
                    (state-add-req s (req metadata res-ch nack))]

                   [`(get-resource-configs ,res-ch ,nack ,id ,type ,name)
                    (define described-resource
                      (delay/thread
                       (define resource
                         (car
                          (k:describe-configs
                           (state-ref-client s id)
                           (k:make-DescribeResource
                            #:type type
                            #:name name))))
                       (sort
                        (for/list ([c (in-list (k:DescribedResource-configs resource))])
                          (make-ResourceConfig
                           #:name (k:ResourceConfig-name c)
                           #:value (k:ResourceConfig-value c)
                           #:is-read-only (k:ResourceConfig-read-only? c)
                           #:is-default (k:ResourceConfig-default? c)
                           #:is-sensitive (k:ResourceConfig-sensitive? c)
                           #:doc-url (maybe-make-doc-url type (k:ResourceConfig-name c))))
                        #:key ResourceConfig-name string<?)))
                    (state-add-req s (req described-resource res-ch nack))]

                   [`(update-resource-configs ,res-ch ,nack ,id ,type ,name ,configs)
                    (define altered-resource
                      (delay/thread
                       (car
                        (k:alter-configs
                         (state-ref-client s id)
                         (k:make-AlterResource
                          #:type type
                          #:name name
                          #:configs configs)))))
                    (state-add-req s (req altered-resource res-ch nack))]

                   [`(create-topic ,res-ch ,nack ,id ,topic-name ,partitions ,replication-factor ,options)
                    (define created-topic
                      (delay/thread
                       (car
                        (k:CreatedTopics-topics
                         (k:create-topics
                          (state-ref-client s id)
                          (k:make-CreateTopic
                           #:name topic-name
                           #:partitions partitions
                           #:replication-factor replication-factor
                           #:configs options))))))
                    (state-add-req s (req created-topic res-ch nack))]

                   [`(delete-topic ,res-ch ,nack ,id ,topic-name)
                    (define deleted-topics
                      (delay/thread
                       (k:delete-topics (state-ref-client s id) topic-name)))
                    (state-add-req s (req deleted-topics res-ch nack))]

                   [`(delete-group ,res-ch ,nack ,id ,group-id)
                    (define deleted-groups
                      (delay/thread
                       (k:delete-groups (state-ref-client s id) group-id)))
                    (state-add-req s (req deleted-groups res-ch nack))]

                   [`(fetch-offsets ,res-ch ,nack ,id ,group-id)
                    (define offsets
                      (delay/thread
                       (define c (state-ref-client s id))
                       (define group
                         (car (k:describe-groups c group-id)))
                       (define member-assignments
                         (for*/hash ([m (in-list (k:Group-members group))]
                                     [(topic pids) (in-hash (k:GroupMember-assignments m))]
                                     [pid (in-list pids)])
                           (values (cons topic pid) m)))
                       (define topics
                         (k:GroupOffsets-topics
                          (k:fetch-offsets c group-id)))
                       (define topic-offsets
                         (k:list-offsets c (for*/hash ([(topic parts) (in-hash topics)]
                                                       [part (in-list parts)])
                                             (define t&p (cons topic (k:GroupPartitionOffset-id part)))
                                             (values t&p 'latest))))
                       (make-GroupOffsets
                        #:group-id group-id
                        #:state (case (k:Group-state group)
                                  [(#f "Dead") 'dead]
                                  [("Empty") 'empty]
                                  [("PreparingRebalance" "CompletingRebalance") 'rebalancing]
                                  [("Stable") 'stable]
                                  [else 'unknown])
                        #:topics (sort
                                  (for/list ([(topic parts) (in-hash topics)])
                                    (make-GroupTopic
                                     #:name topic
                                     #:partitions (sort
                                                   (for/list ([part (in-list parts)])
                                                     (define pid (k:GroupPartitionOffset-id part))
                                                     (define t&p (cons topic pid))
                                                     (define m (hash-ref member-assignments t&p #f))
                                                     (define o (hash-ref topic-offsets t&p #f))
                                                     (make-GroupPartitionOffset
                                                      #:partition-id pid
                                                      #:high-watermark (or (and o (k:PartitionOffset-offset o)) 0)
                                                      #:offset (k:GroupPartitionOffset-offset part)
                                                      #:member-id (and m (k:GroupMember-id m))
                                                      #:client-id (and m (k:GroupMember-client-id m))
                                                      #:client-host (and m (k:GroupMember-client-host m))))
                                                   #:key GroupPartitionOffset-partition-id <)))
                                  #:key GroupTopic-name string<?))))
                    (state-add-req s (req offsets res-ch nack))]

                   [`(reset-topic-offsets ,res-ch ,nack ,id ,group-id ,topic ,target)
                    (define result
                      (delay/thread
                       (define c (state-ref-client s id))
                       (define t (find-topic c topic))
                       (define offsets
                         (k:list-offsets c (for/hash ([p (in-list (k:TopicMetadata-partitions t))])
                                             (define t&p (cons topic (k:PartitionMetadata-id p)))
                                             (values t&p target))))
                       (k:reset-offsets c group-id (for/hash ([(t&p o) (in-hash offsets)])
                                                     (values t&p (k:PartitionOffset-offset o))))))
                    (state-add-req s (req result res-ch nack))]

                   [`(reset-partition-offset ,res-ch ,nack ,id ,group-id ,topic ,pid ,target ,offset)
                    (define result
                      (delay/thread
                       (define c (state-ref-client s id))
                       (define offsets
                         (case target
                           [(earliest latest)
                            (for/hash ([(t&p o) (in-hash (k:list-offsets c (hash (cons topic pid) target)))])
                              (values t&p (k:PartitionOffset-offset o)))]
                           [(offset)
                            (unless offset
                              (raise-argument-error 'reset-partition-offset "exact-integer?" offset))
                            (hash (cons topic pid) offset)]
                           [else
                            (raise-argument-error 'reset-partition-offset "(or/c 'earliest 'latest 'offset)" target)]))
                       (k:reset-offsets c group-id offsets)))
                    (state-add-req s (req result res-ch nack))]

                   [`(find-topic-groups ,res-ch ,nack ,id ,topic)
                    (define result
                      (delay/thread
                       (define c (state-ref-client s id))
                       (define meta (find-topic c topic))
                       (define all-groups (k:list-groups c))
                       (define group-descriptions
                         (apply k:describe-groups c (map k:Group-id all-groups)))
                       (define-values (member-groups pending-groups)
                         (partition
                          (λ (g) (member topic (k:Group-topics g)))
                          group-descriptions))
                       (define topics&partitions
                         (hash topic (map k:PartitionMetadata-id (k:TopicMetadata-partitions meta))))
                       (define committed-groups
                         (filter values (for/list/concurrent ([g (in-list pending-groups)])
                                          (define res (k:fetch-offsets c (k:Group-id g) topics&partitions))
                                          (define parts (hash-ref (k:GroupOffsets-topics res) topic null))
                                          (define has-committed-offsets?
                                            (for/or ([p (in-list parts)])
                                              (>= (k:GroupPartitionOffset-offset p) 0)))
                                          (and has-committed-offsets? g))))
                       (sort
                        (for/list ([g (in-list (append member-groups committed-groups))])
                          (k:Group-id g))
                        string<?)))
                    (state-add-req s (req result res-ch nack))]

                   [`(activate-script ,res-ch ,nack ,id ,topic ,script)
                    (~> (state-add-script s id topic script)
                        (state-add-req (req #t res-ch nack)))]

                   [`(deactivate-script ,res-ch ,nack ,id ,topic)
                    (~> (state-remove-script s id topic)
                        (state-add-req (req #t res-ch nack)))]

                   [`(activate-registry ,res-ch ,nack ,id ,registry)
                    (~> (state-add-registry s id registry)
                        (state-add-req (req #t res-ch nack)))]

                   [`(deactivate-registry ,res-ch ,nack ,id)
                    (~> (state-remove-registry s id)
                        (state-add-req (req #t res-ch nack)))]

                   [`(get-schema ,res-ch ,nack ,id ,name)
                    (define result
                      (delay/thread
                       (define registry (state-ref-registry s id))
                       (unless registry
                         (error 'get-schema "Schema Registry not configured"))
                       (sr:get-schema registry name)))
                    (state-add-req s (req result res-ch nack))]

                   [`(open-iterator ,res-ch ,nack ,id ,topic ,offset)
                    (with-handlers ([exn:fail? (λ (e) (state-add-req s (req e res-ch nack)))])
                      (define c (state-ref-client s id))
                      (define impl (k:make-topic-iterator c topic offset))
                      (define iter (iterator id topic impl))
                      (define-values (iterator-id next-state)
                        (state-add-iterator s iter))
                      (state-add-req next-state (req iterator-id res-ch nack)))]

                   [`(get-records ,res-ch ,nack ,id ,max-bytes)
                    (define records
                      (delay/thread
                       (match-define (iterator cid topic impl)
                         (state-ref-iterator s id))
                       (values
                        (state-ref-registry s cid)
                        (state-ref-script s cid topic)
                        (k:get-records impl #:max-bytes max-bytes))))
                    (state-add-req s (req records res-ch nack))]

                   [`(reset-iterator ,res-ch ,nack ,id ,offset)
                    (define result
                      (delay/thread
                       (define iter (state-ref-iterator s id))
                       (k:reset-topic-iterator! (iterator-impl iter) offset)))
                    (state-add-req s (req result res-ch nack))]

                   [`(close-iterator ,res-ch ,nack ,id)
                    (~> (state-remove-iterator s id)
                        (state-add-req _ (req #t res-ch nack)))]

                   [`(publish-record ,res-ch ,nack ,id ,topic ,pid ,key ,value)
                    (define result
                      (delay/thread
                       (define c (state-ref-client s id))
                       (define p (k:make-producer c))
                       (define evt
                         (k:produce p topic key value #:partition pid))
                       (k:producer-stop p)
                       (sync evt)))
                    (state-add-req s (req result res-ch nack))]

                   [`(shutdown ,res-ch ,nack)
                    (for ([c (in-hash-values (state-clients s))])
                      (k:disconnect-all c))
                    (~> (state-clear-iterators s)
                        (state-clear-clients _)
                        (state-add-req _ (req #t res-ch nack)))])))
              (append
               (for/list ([r (in-list (state-reqs s))])
                 (match-define (req res res-ch _nack) r)
                 (handle-evt
                  (channel-put-evt res-ch res)
                  (lambda (_)
                    (state-remove-req s r))))
               (for/list ([r (in-list (state-reqs s))])
                 (handle-evt
                  (req-nack r)
                  (lambda (_)
                    (state-remove-req s r))))))))
         (loop res-state)))))
  (pool ch thd))

(define current-pool
  (make-parameter (make-pool)))

(define (pool-call-in-context proc [p (current-pool)])
  (force (sync (pool-send p call-in-context proc))))

(define (pool-open conf [p (current-pool)])
  (define (make-client)
    (ConnectionDetails->client conf))
  (define conn
    (pool-call-in-context make-client p))
  (sync (pool-send p open conn)))

(define (pool-close id [p (current-pool)])
  (sync (pool-send p close id)))

(define (pool-get-metadata id reload? [p (current-pool)])
  (force (sync (pool-send p get-metadata id reload?))))

(define (pool-get-resource-configs id type name [p (current-pool)])
  (force (sync (pool-send p get-resource-configs id type name))))

(define (pool-update-resource-configs id type name configs [p (current-pool)])
  (force (sync (pool-send p update-resource-configs id type name configs))))

(define (pool-create-topic id name partitions replication-factor options [p (current-pool)])
  (force (sync (pool-send p create-topic id name partitions replication-factor options))))

(define (pool-delete-topic id name [p (current-pool)])
  (force (sync (pool-send p delete-topic id name))))

(define (pool-delete-group id group-id [p (current-pool)])
  (force (sync (pool-send p delete-group id group-id))))

(define (pool-fetch-offsets id group-id [p (current-pool)])
  (force (sync (pool-send p fetch-offsets id group-id))))

(define (pool-reset-topic-offsets id group-id topic target [p (current-pool)])
  (force (sync (pool-send p reset-topic-offsets id group-id topic target))))

(define (pool-reset-partition-offset id group-id topic pid target offset [p (current-pool)])
  (force (sync (pool-send p reset-partition-offset id group-id topic pid target offset))))

(define (pool-find-topic-groups id topic [p (current-pool)])
  (force (sync (pool-send p find-topic-groups id topic))))

(define (pool-activate-script id topic script [p (current-pool)])
  (sync (pool-send p activate-script id topic script)))

(define (pool-deactivate-script id topic [p (current-pool)])
  (sync (pool-send p deactivate-script id topic)))

(define (pool-activate-registry id registry [p (current-pool)])
  (sync (pool-send p activate-registry id registry)))

(define (pool-deactivate-registry id [p (current-pool)])
  (sync (pool-send p deactivate-registry id)))

(define (pool-get-schema id name [p (current-pool)])
  (force (sync (pool-send p get-schema id name))))

(define (pool-open-iterator id topic offset [p (current-pool)])
  (sync (pool-send p open-iterator id topic offset)))

(define (pool-get-records id max-bytes [p (current-pool)])
  (force (sync (pool-send p get-records id max-bytes))))

(define (pool-reset-iterator id offset [p (current-pool)])
  (force (sync (pool-send p reset-iterator id offset))))

(define (pool-close-iterator id [p (current-pool)])
  (sync (pool-send p close-iterator id)))

(define (pool-publish-record id topic pid key value [p (current-pool)])
  (force (sync (pool-send p publish-record id topic pid key value))))

(define (pool-shutdown [p (current-pool)])
  (sync (pool-send p shutdown)))

(define-syntax-rule (pool-send p command . args)
  (make-pool-evt p 'command . args))

(define (make-pool-evt p command . args)
  (match-define (pool ch thd) p)
  (wrap-evt
   (nack-guard-evt
    (lambda (nack-evt)
      (define res-ch (make-channel))
      (begin0 res-ch
        (thread-resume thd (current-thread))
        (channel-put ch `(,command ,res-ch ,nack-evt ,@args)))))
   (lambda (res-or-err)
     (begin0 res-or-err
       (when (exn:fail? res-or-err)
         (raise res-or-err))))))


;; request ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(struct req (res res-ch nack))

;; state ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(struct state
  (reqs
   clients
   clients-seq
   scripts ;; client-id -> (topic -> table?)
   registries ;; client-id -> registry?
   iterators
   iterators-seq))

(define (make-state)
  (state null (hasheqv) 0 (hasheqv) (hasheqv) (hasheqv) 0))

(define (state-add-req s r)
  (struct-copy state s [reqs (cons r (state-reqs s))]))

(define (state-remove-req s r)
  (struct-copy state s [reqs (remq r (state-reqs s))]))

(define (state-add-client s c)
  (define id (state-clients-seq s))
  (values id (struct-copy state s
                          [clients (hash-set (state-clients s) id c)]
                          [clients-seq (add1 id)])))

(define (state-ref-client s id)
  (hash-ref (state-clients s) id #f))

(define (state-remove-client s id)
  (struct-copy state s
               [clients (hash-remove (state-clients s) id)]
               [scripts (hash-remove (state-scripts s) id)]
               [registries (hash-remove (state-registries s) id)]))

(define (state-clear-clients s)
  (struct-copy state s
               [clients (hasheqv)]
               [scripts (hasheqv)]
               [registries (hasheqv)]))

(define (state-add-script s id topic script)
  (define scripts
    (~> (hash-ref (state-scripts s) id hash)
        (hash-set topic script)))
  (struct-copy state s [scripts (hash-set (state-scripts s) id scripts)]))

(define (state-ref-script s id topic)
  (~> (hash-ref (state-scripts s) id hash)
      (hash-ref topic #f)))

(define (state-remove-script s id topic)
  (define scripts
    (~> (hash-ref (state-scripts s) id hash)
        (hash-remove topic)))
  (struct-copy state s [scripts (hash-set (state-scripts s) id scripts)]))

(define (state-add-registry s id r)
  (struct-copy state s [registries (hash-set (state-registries s) id r)]))

(define (state-ref-registry s id)
  (hash-ref (state-registries s) id #f))

(define (state-remove-registry s id)
  (struct-copy state s [registries (hash-remove (state-registries s) id)]))

(define (state-add-iterator s it)
  (define id (state-iterators-seq s))
  (values id (struct-copy state s
                          [iterators (hash-set (state-iterators s) id it)]
                          [iterators-seq (add1 id)])))

(define (state-ref-iterator s id)
  (hash-ref (state-iterators s) id #f))

(define (state-remove-iterator s id)
  (struct-copy state s [iterators (hash-remove (state-iterators s) id)]))

(define (state-clear-iterators s)
  (struct-copy state s [iterators (hasheqv)]))


;; iter ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(struct iterator (client-id topic impl))


;; help ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (find-topic c name)
  (findf
   (λ (t) (string=? (k:TopicMetadata-name t) name))
   (k:Metadata-topics (k:client-metadata c))))

(define (compute-stats c topics groups)
  (define partition-offsets-promise
    (delay/thread
     (k:list-offsets c (for*/hash ([t (in-list topics)]
                                   [p (in-list (Topic-partitions t))])
                         (define t&p (cons (Topic-name t)
                                           (TopicPartition-id p)))
                         (values t&p 'latest)))))
  (define topics-by-name
    (for/hash ([t (in-list topics)])
      (values (Topic-name t) t)))
  (define topics-by-group
    (for*/fold ([res (hash)])
               ([g (in-list (apply k:describe-groups c (map Group-id groups)))]
                [t (in-list (k:Group-topics g))])
      (hash-update res (k:Group-id g) (λ (ts) (cons t ts)) null)))
  (define group-offsets
    (for/list/concurrent ([(g topics) (in-hash topics-by-group)])
      (define topics&partitions
        (for*/hash ([name (in-list topics)]
                    [t (in-value (hash-ref topics-by-name name #f))]
                    #:when t)
          (values name (map TopicPartition-id (Topic-partitions t)))))
      (and (not (hash-empty? topics&partitions))
           (cons g (k:fetch-offsets c g topics&partitions)))))
  (define partition-offsets
    (force partition-offsets-promise))
  (for*/fold ([stats-by-topic (hash)]
              [stats-by-group (hash)])
             ([pair (in-list group-offsets)]
              #:when pair
              [g (in-value (car pair))]
              [r (in-value (cdr pair))]
              [(t gos) (in-hash (k:GroupOffsets-topics r))]
              [go (in-list gos)]
              #:when (>= (k:GroupPartitionOffset-offset go) 0)
              [po (in-value (hash-ref partition-offsets (cons t (k:GroupPartitionOffset-id go)) #f))]
              #:when (and po (>= (k:PartitionOffset-offset po) 0)))
    (define lag
      (max 0 (- (k:PartitionOffset-offset po)
                (k:GroupPartitionOffset-offset go))))
    (values
     (hash-update stats-by-topic t (λ (s) (Stats+ s lag)) make-Stats)
     (hash-update stats-by-group g (λ (s) (Stats+ s lag)) make-Stats))))

(define (Stats+ s lag)
  (define min-lag (Stats-min-lag s))
  (define max-lag (Stats-max-lag s))
  (define sum-lag (Stats-sum-lag s))
  (make-Stats
   #:min-lag (min (if (< min-lag 0) lag min-lag) lag)
   #:max-lag (max max-lag lag)
   #:sum-lag (+ sum-lag lag)))


;; docs ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define doc-root
  "https://kafka.apache.org/documentation/")

(define (maybe-make-doc-url type key)
  (define type-prefix
    (case type
      [(broker) "brokerconfigs"]
      [(topic) "topicconfigs"]
      [else #f]))
  (and type-prefix (format "~a#~a_~a" doc-root type-prefix key)))


;; tests ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(module+ test
  (require (prefix-in k: kafka/consumer)
           rackunit
           rackunit/text-ui)

  (define test-details
    (make-ConnectionDetails
     #:name "Integration Tests"
     #:bootstrap-host "127.0.0.1"
     #:bootstrap-port 9092
     #:use-ssl #f))

  (define (wait-for-topic id topic)
    (define deadline (+ (current-inexact-monotonic-milliseconds) 5000))
    (let loop ()
      (when (> (current-inexact-monotonic-milliseconds) deadline)
        (error 'wait-for-topic "timed out"))
      (unless (findf (λ (t) (equal? topic (Topic-name t)))
                     (Metadata-topics (pool-get-metadata id #t)))
        (sleep 0.1)
        (loop))))

  (define integration-tests
    (test-suite
     "pool integration"
     #:before (λ () (current-pool (make-pool)))
     #:after  (λ () (pool-shutdown))

     (test-case "client setup, teardown"
       (define id (pool-open test-details))
       (check-not-false id)
       (check-false (null? (Metadata-brokers (pool-get-metadata id #f))))
       (check-false (null? (Metadata-brokers (pool-get-metadata id #t))))
       (check-not-false (pool-close id)))

     (test-case "create topic, publish, delete"
       (define t "franz_integration_create_example")
       (define id (pool-open test-details))
       (test-begin
         (define res (pool-create-topic id t 2 1 (hash)))
         (check-equal? (k:CreatedTopic-error-code res) 0)
         (wait-for-topic id t)
         (define conf (pool-get-resource-configs id 'topic t))
         (check-false (null? conf)))
       (pool-delete-topic id t)
       (pool-close id))

     (test-case "publish, iterate"
       (define t "franz_integration_publish_iterate_example")
       (define id (pool-open test-details))
       (test-begin
         (pool-create-topic id t 2 1 (hash))
         (wait-for-topic id t)
         (define it (pool-open-iterator id t 'latest))
         (let-values ([(registry script records) (pool-get-records it (* 1 1024 1024))])
           (check-false registry)
           (check-false script)
           (check-equal? (vector-length records) 0))
         (pool-publish-record id t 0 #"transaction-1" #"100")
         (let-values ([(registry script records) (pool-get-records it (* 1 1024 1024))])
           (check-false registry)
           (check-false script)
           (check-equal? (vector-length records) 1)
           (check-equal? (k:record-key (vector-ref records 0)) #"transaction-1"))
         (pool-publish-record id t 1 #"transaction-2" #"50")
         (let-values ([(registry script records) (pool-get-records it (* 1 1024 1024))])
           (check-false registry)
           (check-false script)
           (check-equal? (vector-length records) 1)
           (check-equal? (k:record-value (vector-ref records 0)) #"50"))
         (check-true (pool-activate-script id t "a script"))
         (pool-reset-iterator it 'earliest)
         (let-values ([(registry script records) (pool-get-records it (* 1 1024 1024))])
           (check-false registry)
           (check-equal? script "a script")
           (check-equal? (vector-length records) 2))
         (check-true (pool-deactivate-script id t))
         (let-values ([(registry script records) (pool-get-records it (* 1 1024 1024))])
           (check-false registry)
           (check-false script)
           (check-equal? records (vector)))
         (pool-close-iterator it))
       (pool-delete-topic id t)
       (pool-close id))

     (test-case "consumer groups"
       (define g "franz_integration_consumer_group")
       (define t "franz_integration_consumer_groups_example")
       (define id (pool-open test-details))
       (define (consume n)
         (define c (k:make-consumer (k:make-client) g t))
         (dynamic-wind
           void
           (lambda ()
             (define records
               (let loop ([consumed 0]
                          [records null])
                 (cond
                   [(< consumed n)
                    (define-values (type data)
                      (sync (k:consume-evt c)))
                    (case type
                      [(records)
                       (loop (+ consumed (vector-length data))
                             (append records (vector->list data)))]
                      [else
                       (loop consumed records)])]
                   [else
                    records])))
             (begin0 records
               (k:consumer-commit c)))
           (lambda ()
             (k:consumer-stop c))))
       (test-begin
         (pool-create-topic id t 2 1 (hash))
         (wait-for-topic id t)
         (pool-publish-record id t 0 #"partition-0-record-0" #f)
         (pool-publish-record id t 0 #"partition-0-record-1" #f)
         (pool-publish-record id t 1 #"partition-1-record-0" #f)
         (pool-delete-group id g)
         (check-equal?
          (for/hash ([r (in-list (consume 3))])
            (values (k:record-key r) #t))
          (hash
           #"partition-0-record-0" #t
           #"partition-0-record-1" #t
           #"partition-1-record-0" #t))
         (check-not-false
          (findf (λ (group) (equal? (Group-id group) g))
                 (Metadata-groups (pool-get-metadata id #t))))
         (check-equal?
          (pool-fetch-offsets id g)
          (make-GroupOffsets
           #:group-id g
           #:topics (list
                     (make-GroupTopic
                      #:name t
                      #:partitions (list
                                    (make-GroupPartitionOffset
                                     #:partition-id 0
                                     #:high-watermark 2
                                     #:offset 2
                                     #:member-id #f
                                     #:client-id #f
                                     #:client-host #f)
                                    (make-GroupPartitionOffset
                                     #:partition-id 1
                                     #:high-watermark 1
                                     #:offset 1
                                     #:member-id #f
                                     #:client-id #f
                                     #:client-host #f))))
           #:state 'empty))
         (pool-reset-topic-offsets id g t 'earliest)
         (check-equal?
          (pool-fetch-offsets id g)
          (make-GroupOffsets
           #:group-id g
           #:topics (list
                     (make-GroupTopic
                      #:name t
                      #:partitions (list
                                    (make-GroupPartitionOffset
                                     #:partition-id 0
                                     #:high-watermark 2
                                     #:offset 0
                                     #:member-id #f
                                     #:client-id #f
                                     #:client-host #f)
                                    (make-GroupPartitionOffset
                                     #:partition-id 1
                                     #:high-watermark 1
                                     #:offset 0
                                     #:member-id #f
                                     #:client-id #f
                                     #:client-host #f))))
           #:state 'empty))
         (pool-reset-partition-offset id g t 0 'offset 1)
         (pool-reset-partition-offset id g t 1 'latest #f)
         (check-equal?
          (pool-fetch-offsets id g)
          (make-GroupOffsets
           #:group-id g
           #:topics (list
                     (make-GroupTopic
                      #:name t
                      #:partitions (list
                                    (make-GroupPartitionOffset
                                     #:partition-id 0
                                     #:high-watermark 2
                                     #:offset 1
                                     #:member-id #f
                                     #:client-id #f
                                     #:client-host #f)
                                    (make-GroupPartitionOffset
                                     #:partition-id 1
                                     #:high-watermark 1
                                     #:offset 1
                                     #:member-id #f
                                     #:client-id #f
                                     #:client-host #f))))
           #:state 'empty)))
       (pool-delete-group id g)
       (pool-delete-topic id t)
       (pool-close id))))

  (when (equal? (getenv "FRANZ_INTEGRATION_TESTS") "x")
    (run-tests integration-tests)))
