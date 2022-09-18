#lang racket/base

(require (prefix-in k: kafka)
         racket/match
         threading
         "connection-details.rkt"
         "logger.rkt"
         "topic.rkt")

(provide
 pool?
 make-pool
 current-pool
 pool-open
 pool-close
 pool-ref
 pool-topics
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
                                ((error-display-handler) (format "pool: ~a" (exn-message e)) e)))])
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
                    (state-add-req next-state (open-req client-id-or-exn res-ch nack))]

                   [`(close ,res-ch ,nack ,id)
                    (define client
                      (state-ref-client s id))
                    (when client
                      (k:disconnect-all client)
                      (log-franz-debug "pool: closed client ~a" id))
                    (~> (state-remove-client s id)
                        (state-add-req (close-req (and client #t) res-ch nack)))]

                   [`(ref ,res-ch ,nack ,id)
                    (~> (state-ref-client s id)
                        (ref-req _ res-ch nack)
                        (state-add-req s _))]

                   [`(topics ,res-ch ,nack ,id)
                    (define topics-or-exn
                      (with-handlers ([exn:fail? values])
                        (define meta (k:get-metadata (state-ref-client s id)))
                        (for/list ([t (in-list (k:Metadata-topics meta))])
                          (make-Topic
                           #:name (k:TopicMetadata-name t)
                           #:partitions (length (k:TopicMetadata-partitions t))))))
                    (state-add-req s (topics-req topics-or-exn res-ch nack))]

                   [`(shutdown ,res-ch ,nack)
                    (for-each k:disconnect-all (hash-values (state-clients s)))
                    (~> (state-clear-clients s)
                        (state-add-req _ (shutdown-req #t res-ch nack)))])))
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

(define (pool-ref id [p (current-pool)])
  (sync (pool-send p ref id)))

(define (pool-topics id [p (current-pool)])
  (sync (pool-send p topics id)))

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
(struct open-req req ())
(struct close-req req ())
(struct ref-req req ())
(struct topics-req req ())
(struct shutdown-req req ())

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
