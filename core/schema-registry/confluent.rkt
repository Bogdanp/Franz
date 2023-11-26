#lang racket/base

(require (prefix-in avro: avro)
         (prefix-in impl: confluent/schema-registry)
         json
         kafka/consumer
         (prefix-in protobuf: protocol-buffers)
         racket/match
         racket/port
         "generic.rkt"
         "schema.rkt"
         "varint.rkt")

(provide
 make-confluent-registry)

(struct confluent-registry (client codecs)
  #:methods gen:registry
  [(define (get-schemas self)
     (define subjects
       (impl:get-all-subjects (confluent-registry-client self)))
     (for/list ([subject (in-list subjects)])
       (make-Schema #:name subject)))

   (define (get-schema self name)
     (define client (confluent-registry-client self))
     (define schema
       (impl:get-subject-version client name 'latest))
     (make-Schema
      #:id (impl:Schema-id schema)
      #:name name
      #:type (case (impl:Schema-type schema)
               [(avro) (SchemaType.avro)]
               [(json) (SchemaType.json)]
               [(protobuf) (SchemaType.protobuf)]
               [else (error 'get-schema "unexpected schema type: ~a" (impl:Schema-type schema))])
      #:version (impl:Schema-version schema)
      #:schema (impl:Schema-schema schema)))

   (define (create-schema self name type schema)
     (define client
       (confluent-registry-client self))
     (define the-schema
       (impl:make-Schema
        #:type type
        #:schema schema))
     (impl:register-schema client name the-schema))

   (define (delete-schema self name)
     (define client (confluent-registry-client self))
     (impl:delete-subject client name))

   (define (decode-record self r)
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

;; https://docs.confluent.io/platform/current/schema-registry/serdes-develop/index.html#wire-format
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
         (avro:make-codec (impl:Schema-schema schema)))
       (λ (bs)
         (call-with-input-bytes bs
           (lambda (in)
             (void (read-bytes 5 in))
             (jsexpr->bytes (avro:codec-read c in)))))]
      [(json)
       (λ (bs)
         (subbytes bs 5))]
      [(protobuf)
       (define mod
         (call-with-input-string (impl:Schema-schema schema)
           protobuf:read-protobuf))
       (λ (bs)
         (call-with-input-bytes bs
           (lambda (in)
             (void (read-bytes 5 in))
             (define msg
               (let ([len (read-varint in)])
                 (cond
                   [(zero? len)
                    (car (protobuf:mod-messages mod))]
                   [else
                    (let loop ([len len] [thing mod])
                      (cond
                        [(zero? len) thing]
                        [else
                         (define messages
                           ((cond
                              [(protobuf:mod? thing) protobuf:mod-messages mod]
                              [(protobuf:message? thing) protobuf:message-messages]
                              [else (error 'decode "unreachable")])
                            thing))
                         (loop (sub1 len) (list-ref messages (read-varint in)))]))])))
             ;; Protocol buffer map keys can be things other than
             ;; symbols, so we have to convert those values into
             ;; jsexprs.
             (jsexpr->bytes
              (let loop ([v (protobuf:read-message msg in)])
                (cond
                  [(hash? v)
                   (if (hash-eq? v)
                       (for/hasheq ([(k v) (in-hash v)])
                         (values k (loop v)))
                       (for/list ([(k v) (in-hash v)])
                         (hasheq 'key k 'value v)))]
                  [(list? v)
                   (map loop v)]
                  [else v]))))))]
      [else
       (error 'decode "unsupported schema type: ~a" (impl:Schema-type schema))])))
