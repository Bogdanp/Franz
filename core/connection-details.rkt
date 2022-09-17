#lang racket/base

(require (only-in db sql-null sql-null->false)
         noise/backend
         noise/serde
         racket/contract
         racket/string
         threading
         (prefix-in meta: "metadata.rkt"))

(provide
 (record-out ConnectionDetails))

(define-record ConnectionDetails
  [(id #f) : (Optional UVarint)]
  [name : String #:contract non-empty-string?]
  [(bootstrap-host "127.0.0.1") : String #:contract non-empty-string?]
  [(bootstrap-port 9092) : UVarint #:contract (integer-in 0 65535)]
  [(username #f) : (Optional String) #:contract (or/c #f string?)]
  [(password #f) : (Optional String) #:contract (or/c #f string?)]
  [(use-ssl #f) : Bool #:contract boolean?])

(define (meta->ConnectionDetails c)
  (make-ConnectionDetails
   #:id (meta:connection-details-id c)
   #:name (meta:connection-details-name c)
   #:bootstrap-host (meta:connection-details-bootstrap-host c)
   #:bootstrap-port (meta:connection-details-bootstrap-port c)
   #:username (sql-null->false (meta:connection-details-username c))
   #:password (sql-null->false (meta:connection-details-password c))
   #:use-ssl (meta:connection-details-ssl-on? c)))

(define (ConnectionDetails->meta c)
  (define meta:c
    (meta:make-connection-details
     #:name (ConnectionDetails-name c)
     #:bootstrap-host (ConnectionDetails-bootstrap-host c)
     #:bootstrap-port (ConnectionDetails-bootstrap-port c)
     #:username (or (ConnectionDetails-username c) sql-null)
     #:password (or (ConnectionDetails-password c) sql-null)
     #:ssl-on? (ConnectionDetails-use-ssl c)))
  (cond
    [(ConnectionDetails-id c)
     => (λ (id) (meta:set-connection-details-id meta:c id))]
    [else meta:c]))

(define-rpc (get-connections : (Listof ConnectionDetails))
  (map meta->ConnectionDetails (meta:get-connections)))

(define-rpc (save-connection [_ c : ConnectionDetails] : UVarint)
  (meta:connection-details-id
   (meta:insert-connection!
    (ConnectionDetails->meta c))))

(define-rpc (touch-connection [_ c : ConnectionDetails] : Bool)
  (and
   (and~> (ConnectionDetails-id c)
          (meta:touch-connection!))
   #t))
