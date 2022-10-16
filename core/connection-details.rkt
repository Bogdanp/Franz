#lang racket/base

(require (only-in db sql-null sql-null->false)
         (prefix-in k: kafka)
         noise/backend
         noise/serde
         (only-in openssl ssl-secure-client-context)
         racket/contract
         racket/port
         racket/random
         racket/string
         (prefix-in sasl: sasl/plain)
         threading
         (prefix-in meta: "metadata.rkt"))

(provide
 (record-out ConnectionDetails)
 ConnectionDetails->client)

(define-record ConnectionDetails
  [(id #f) : (Optional UVarint)]
  [name : String #:contract non-empty-string?]
  [(bootstrap-host "127.0.0.1") : String #:contract non-empty-string?]
  [(bootstrap-port 9092) : UVarint #:contract (integer-in 0 65535)]
  [(username #f) : (Optional String) #:contract (or/c #f string?)]
  [(password #f) : (Optional String) #:contract (or/c #f string?)]
  [(password-id #f) : (Optional String) #:contract (or/c #f string?)]
  [(use-ssl #f) : Bool #:contract boolean?])

(define (meta->ConnectionDetails c)
  (make-ConnectionDetails
   #:id (meta:connection-details-id c)
   #:name (meta:connection-details-name c)
   #:bootstrap-host (meta:connection-details-bootstrap-host c)
   #:bootstrap-port (meta:connection-details-bootstrap-port c)
   #:username (sql-null->false (meta:connection-details-username c))
   #:password-id (sql-null->false (meta:connection-details-password-id c))
   #:use-ssl (meta:connection-details-ssl-on? c)))

(define (ConnectionDetails->meta c)
  (define meta:c
    (meta:make-connection-details
     #:name (ConnectionDetails-name c)
     #:bootstrap-host (ConnectionDetails-bootstrap-host c)
     #:bootstrap-port (ConnectionDetails-bootstrap-port c)
     #:username (or (ConnectionDetails-username c) sql-null)
     #:password-id (or (ConnectionDetails-password-id c) sql-null)
     #:ssl-on? (ConnectionDetails-use-ssl c)))
  (cond
    [(ConnectionDetails-id c)
     => (λ (id) (meta:set-connection-details-id meta:c id))]
    [else meta:c]))

(define (ConnectionDetails->client c)
  (k:make-client
   #:id "Franz"
   #:bootstrap-host (ConnectionDetails-bootstrap-host c)
   #:bootstrap-port (ConnectionDetails-bootstrap-port c)
   #:sasl-mechanism&ctx (let ([username (ConnectionDetails-username c)]
                              [password (ConnectionDetails-password c)])
                          (and username
                               password
                               `(plain ,(sasl:plain-client-message username password))))
   #:ssl-ctx (and (ConnectionDetails-use-ssl c)
                  (ssl-secure-client-context))))

(define-rpc (get-connections : (Listof ConnectionDetails))
  (map meta->ConnectionDetails (meta:get-connections)))

(define-rpc (save-connection [_ c : ConnectionDetails] : ConnectionDetails)
  (meta->ConnectionDetails
   (meta:insert-connection!
    (ConnectionDetails->meta c))))

(define-rpc (update-connection [_ c : ConnectionDetails] : ConnectionDetails)
  (meta->ConnectionDetails
   (meta:update-connection!
    (ConnectionDetails->meta c))))

(define-rpc (touch-connection [_ c : ConnectionDetails])
  (and~> (ConnectionDetails-id c)
         (meta:touch-connection!)))

(define-rpc (delete-connection [_ c : ConnectionDetails])
  (and~> (ConnectionDetails-id c)
         (meta:delete-connection!)))

(define-rpc (generate-password-id : String)
  (call-with-output-string
   (lambda (out)
     (write-string "franz-" out)
     (for ([b (in-bytes (crypto-random-bytes 32))])
       (when (< b 16)
         (write-string "0" out))
       (write-string (number->string b 16) out)))))
