#lang racket/base

(require (prefix-in csr: confluent/schema-registry)
         (only-in db sql-null sql-null->false)
         (prefix-in http: net/http-easy)
         noise/backend
         noise/serde
         racket/contract
         racket/match
         threading
         (prefix-in meta: "metadata.rkt")
         "pool.rkt"
         "schema-registry/confluent.rkt"
         "schema-registry/schema.rkt")

(provide
 (enum-out SchemaRegistryKind)
 (record-out SchemaRegistry))

(define-enum SchemaRegistryKind
  [confluent])

(define-record SchemaRegistry
  [(id #f) : (Optional UVarint)]
  [(kind (SchemaRegistryKind.confluent)) : SchemaRegistryKind]
  [url : String #:contract string?]
  [(username #f) : (Optional String) #:contract (or/c #f string?)]
  [(password-id #f) : (Optional String) #:contract (or/c #f string?)])

(define (meta->SchemaRegistryKind m)
  (case m
    [(confluent) (SchemaRegistryKind.confluent)]
    [else (raise-argument-error 'meta->SchemaRegistryKind "schema-registry-kind/c" m)]))

(define (SchemaRegistryKind->meta m)
  (match m
    [(SchemaRegistryKind.confluent) 'confluent]))

(define (meta->SchemaRegistry r)
  (make-SchemaRegistry
   #:id (meta:schema-registry-id r)
   #:kind (meta->SchemaRegistryKind (meta:schema-registry-kind r))
   #:url (meta:schema-registry-url r)
   #:username (sql-null->false (meta:schema-registry-username r))
   #:password-id (sql-null->false (meta:schema-registry-password-id r))))

(define (SchemaRegistry->meta r)
  (define meta:r
    (meta:make-schema-registry
     #:kind (SchemaRegistryKind->meta (SchemaRegistry-kind r))
     #:url (SchemaRegistry-url r)
     #:username (or (SchemaRegistry-username r) sql-null)
     #:password-id (or (SchemaRegistry-password-id r) sql-null)))
  (cond
    [(SchemaRegistry-id r)
     => (λ (id) (meta:set-schema-registry-id meta:r id))]
    [else meta:r]))

(define (SchemaRegistry->impl r [password #f])
  (match (SchemaRegistry-kind r)
    [(SchemaRegistryKind.confluent)
     (define c
       (if (SchemaRegistry-username r)
           (csr:make-client
            (SchemaRegistry-url r)
            (http:basic-auth (SchemaRegistry-username r) (or password "")))
           (csr:make-client
            (SchemaRegistry-url r))))
     (make-confluent-registry c)]))

(define-rpc (get-schema-registry [id : UVarint] : SchemaRegistry)
  (meta->SchemaRegistry
   (meta:get-schema-registry id)))

(define-rpc (save-schema-registry [_ r : SchemaRegistry] : SchemaRegistry)
  (meta->SchemaRegistry
   (meta:insert-schema-registry!
    (SchemaRegistry->meta r))))

(define-rpc (update-schema-registry [_ r : SchemaRegistry] : SchemaRegistry)
  (meta->SchemaRegistry
   (meta:update-schema-registry!
    (SchemaRegistry->meta r))))

(define-rpc (delete-schema-registry [_ id : UVarint])
  (meta:delete-schema-registry! id))

(define-rpc (activate-schema-registry [_ r : SchemaRegistry]
                                      [with-password password : (Optional String)]
                                      [in-workspace id : UVarint])
  (~> (pool-call-in-context (λ () (SchemaRegistry->impl r password)))
      (pool-activate-registry id _)))

(define-rpc (deactivate-schema-registry [in-workspace id : UVarint])
  (pool-deactivate-registry id))

(define-rpc (get-schema [named name : String]
                        [in-workspace id : UVarint] : Schema)
  (pool-get-schema id name))
