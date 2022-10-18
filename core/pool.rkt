#lang racket/base

(require (prefix-in k: kafka)
         racket/match
         racket/promise
         threading
         "broker.rkt"
         "connection-details.rkt"
         "group.rkt"
         "logger.rkt")

(provide
 pool?
 make-pool
 current-pool
 pool-open
 pool-close
 pool-get-metadata
 pool-get-resource-configs
 pool-create-topic
 pool-delete-topic
 pool-delete-group
 pool-fetch-offsets
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
                   [`(open ,res-ch ,nack ,conf)
                    (define-values (client-id-or-exn next-state)
                      (with-handlers ([exn:fail? (λ (e) (values e s))])
                        (begin0 (state-add-client s (ConnectionDetails->client conf))
                          (log-franz-debug "pool: opened client ~a" client-id-or-exn))))
                    (state-add-req next-state (req client-id-or-exn res-ch nack))]

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
                             (k:get-metadata c)
                             (k:client-metadata c)))
                       (define controller-id
                         (k:Metadata-controller-id meta))
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
                                #:id (k:PartitionMetadata-id p))))
                           (make-Topic
                            #:name (k:TopicMetadata-name t)
                            #:partitions (sort parts < #:key TopicPartition-id)
                            #:is-internal (k:TopicMetadata-internal? t))))
                       (make-Metadata
                        #:brokers (sort brokers < #:key Broker-id)
                        #:topics (sort topics string<? #:key Topic-name)
                        #:groups (sort groups string<? #:key Group-id))))
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
                           #:is-sensitive (k:ResourceConfig-sensitive? c)))
                        #:key ResourceConfig-name string<?)))
                    (state-add-req s (req described-resource res-ch nack))]

                   [`(create-topic ,res-ch ,nack ,id ,topic-name ,partitions ,options)
                    (define created-topic
                      (delay/thread
                       (car
                        (k:CreatedTopics-topics
                         (k:create-topics
                          (state-ref-client s id)
                          (k:make-CreateTopic
                           #:name topic-name
                           #:partitions partitions
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
                                                      #:high-watermark (and o (k:PartitionOffset-offset o))
                                                      #:offset (k:GroupPartitionOffset-offset part)
                                                      #:member-id (and m (k:GroupMember-id m))
                                                      #:client-id (and m (k:GroupMember-client-id m))
                                                      #:client-host (and m (k:GroupMember-client-host m))))
                                                   #:key GroupPartitionOffset-partition-id <)))
                                  #:key GroupTopic-name string<?))))
                    (state-add-req s (req offsets res-ch nack))]

                   [`(shutdown ,res-ch ,nack)
                    (for ([c (in-hash-values (state-clients s))])
                      (k:disconnect-all c))
                    (~> (state-clear-clients s)
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

(define (pool-open conf [p (current-pool)])
  (sync (pool-send p open conf)))

(define (pool-close id [p (current-pool)])
  (sync (pool-send p close id)))

(define (pool-get-metadata id reload? [p (current-pool)])
  (force (sync (pool-send p get-metadata id reload?))))

(define (pool-get-resource-configs id type name [p (current-pool)])
  (force (sync (pool-send p get-resource-configs id type name))))

(define (pool-create-topic id name partitions options [p (current-pool)])
  (force (sync (pool-send p create-topic id name partitions options))))

(define (pool-delete-topic id name [p (current-pool)])
  (force (sync (pool-send p delete-topic id name))))

(define (pool-delete-group id group-id [p (current-pool)])
  (force (sync (pool-send p delete-group id group-id))))

(define (pool-fetch-offsets id group-id [p (current-pool)])
  (force (sync (pool-send p fetch-offsets id group-id))))

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
   clients-seq))

(define (make-state)
  (state null (hasheqv) 0))

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
  (struct-copy state s [clients (hash-remove (state-clients s) id)]))

(define (state-clear-clients s)
  (struct-copy state s [clients (hasheqv)]))
