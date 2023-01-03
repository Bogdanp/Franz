#lang racket/base

(require avro
         (prefix-in impl: confluent/schema-registry)
         json
         kafka/consumer
         racket/match
         racket/port
         "generic.rkt")

(provide
 make-confluent-registry)

(struct confluent-registry (client codecs)
  #:methods gen:registry
  [(define (decode-record self r)
     (match-define (record _ _ _ k v _) r)
     (struct-copy record r
                  [key (or (and k (decode self k)) k)]
                  [value (or (and v (decode self v)) v)]))])

(define (make-confluent-registry c)
  (confluent-registry c (make-hasheqv)))

(define (decode registry bs)
  (match-define (confluent-registry client codecs) registry)
  (and (>= (bytes-length bs) 5)
       (zero? (bytes-ref bs 0))
       (let ([id (integer-bytes->integer bs #f #t 1 5)])
         (define proc
           (hash-ref! codecs id (λ () (get-codec client id))))
         (proc bs))))

(define (get-codec client id)
  (with-handlers ([(λ (e)
                     (and (impl:exn:fail:schema-registry:client? e)
                          (eqv? (impl:exn:fail:schema-registry:client-code e) 40403)))
                   (λ (_)
                     (λ (bs) bs))])
    (define schema
      (impl:get-schema client id))
    (case (impl:Schema-type schema)
      [(avro)
       (define c
         (make-codec (impl:Schema-schema schema)))
       (λ (bs)
         (call-with-input-bytes bs
           (lambda (in)
             (void (read-bytes 5 in))
             (jsexpr->bytes (codec-read c in)))))]
      [(json)
       (λ (bs)
         (subbytes bs 5))]
      [else
       (error 'decode "unsupported schema type: ~a" (impl:Schema-type schema))])))
