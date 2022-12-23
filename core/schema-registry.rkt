#lang racket/base

(require (only-in db sql-null sql-null->false)
         noise/backend
         noise/serde
         racket/contract
         (prefix-in meta: "metadata.rkt"))

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
  (case m
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
