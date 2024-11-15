#lang racket/base

(require (only-in db sql-null sql-null->false)
         (prefix-in k: kafka)
         noise/backend
         noise/serde
         racket/contract/base
         racket/lazy-require
         racket/match
         racket/path
         racket/port
         racket/random
         racket/string
         threading
         (prefix-in meta: "metadata.rkt"))

(lazy-require
 [openssl
  (ssl-make-client-context
   ssl-secure-client-context)]
 [sasl/aws-msk-iam
  ([make-aws-msk-iam-ctx sasl:make-aws-msk-iam-ctx])]
 [sasl/scram
  ([make-scram-client-ctx sasl:make-scram-client-ctx])]
 [sasl/plain
  ([plain-client-message sasl:plain-client-message])])

(provide
 (enum-out AuthMechanism)
 (record-out ConnectionDetails)
 ConnectionDetails->client)

(define-enum AuthMechanism
  [plain]
  [scramSHA256]
  [scramSHA512]
  [aws])

(define-record ConnectionDetails
  [(id #f) : (Optional UVarint)]
  [name : String #:contract non-empty-string?]
  [(http-proxy-addr #f) : (Optional String) #:contract (or/c #f non-empty-string?)]
  [(bootstrap-host "127.0.0.1") : String #:contract non-empty-string?]
  [(bootstrap-port 9092) : UVarint #:contract (integer-in 0 65535)]
  [(auth-mechanism (AuthMechanism.plain)) : AuthMechanism]
  [(username #f) : (Optional String) #:contract (or/c #f string?)]
  [(password #f) : (Optional String) #:contract (or/c #f string?)]
  [(password-id #f) : (Optional String) #:contract (or/c #f string?)]
  [(aws-region #f) : (Optional String) #:contract (or/c #f string?)]
  [(aws-access-key-id #f) : (Optional String) #:contract (or/c #f string?)]
  [(use-ssl #f) : Bool #:contract boolean?]
  [(ssl-key-path #f) : (Optional String) #:contract (or/c #f string?) #:mutable]
  [(ssl-cert-path #f) : (Optional String) #:contract (or/c #f string?) #:mutable]
  [(schema-registry-id #f) : (Optional UVarint) #:mutable #:contract (or/c #f exact-nonnegative-integer?)])

(define (meta->AuthMechanism m)
  (case m
    [(plain) (AuthMechanism.plain)]
    [(scram-sha-256) (AuthMechanism.scramSHA256)]
    [(scram-sha-512) (AuthMechanism.scramSHA512)]
    [(aws-msk-iam) (AuthMechanism.aws)]
    [else (raise-argument-error 'meta->AuthMechanism "auth-mechanism/c" m)]))

(define (AuthMechanism->meta m)
  (match m
    [(AuthMechanism.plain) 'plain]
    [(AuthMechanism.scramSHA256) 'scram-sha-256]
    [(AuthMechanism.scramSHA512) 'scram-sha-512]
    [(AuthMechanism.aws) 'aws-msk-iam]))

(define (meta->ConnectionDetails c)
  (make-ConnectionDetails
   #:id (meta:connection-details-id c)
   #:name (meta:connection-details-name c)
   #:http-proxy-addr (sql-null->false (meta:connection-details-http-proxy-addr c))
   #:bootstrap-host (meta:connection-details-bootstrap-host c)
   #:bootstrap-port (meta:connection-details-bootstrap-port c)
   #:auth-mechanism (meta->AuthMechanism (meta:connection-details-auth-mechanism c))
   #:username (sql-null->false (meta:connection-details-username c))
   #:password-id (sql-null->false (meta:connection-details-password-id c))
   #:aws-region (sql-null->false (meta:connection-details-aws-region c))
   #:aws-access-key-id (sql-null->false (meta:connection-details-aws-access-key-id c))
   #:use-ssl (meta:connection-details-ssl-on? c)
   #:ssl-key-path (sql-null->false (meta:connection-details-ssl-key-path c))
   #:ssl-cert-path (sql-null->false (meta:connection-details-ssl-cert-path c))
   #:schema-registry-id (sql-null->false (meta:connection-details-schema-registry-id c))))

(define (ConnectionDetails->meta c)
  (define meta:c
    (meta:make-connection-details
     #:name (ConnectionDetails-name c)
     #:http-proxy-addr (or (ConnectionDetails-http-proxy-addr c) sql-null)
     #:bootstrap-host (ConnectionDetails-bootstrap-host c)
     #:bootstrap-port (ConnectionDetails-bootstrap-port c)
     #:auth-mechanism (AuthMechanism->meta (ConnectionDetails-auth-mechanism c))
     #:username (or (ConnectionDetails-username c) sql-null)
     #:password-id (or (ConnectionDetails-password-id c) sql-null)
     #:aws-region (or (ConnectionDetails-aws-region c) sql-null)
     #:aws-access-key-id (or (ConnectionDetails-aws-access-key-id c) sql-null)
     #:ssl-on? (ConnectionDetails-use-ssl c)
     #:ssl-key-path (or (ConnectionDetails-ssl-key-path c) sql-null)
     #:ssl-cert-path (or (ConnectionDetails-ssl-cert-path c) sql-null)
     #:schema-registry-id (or (ConnectionDetails-schema-registry-id c) sql-null)))
  (cond
    [(ConnectionDetails-id c)
     => (λ (id) (meta:set-connection-details-id meta:c id))]
    [else meta:c]))

(define (ConnectionDetails->client c)
  (k:make-client
   #:id "Franz"
   #:bootstrap-host (ConnectionDetails-bootstrap-host c)
   #:bootstrap-port (ConnectionDetails-bootstrap-port c)
   #:sasl-mechanism&ctx (match (ConnectionDetails-auth-mechanism c)
                          [(AuthMechanism.plain)
                           (define username (ConnectionDetails-username c))
                           (define password (ConnectionDetails-password c))
                           (and username
                                password
                                `(plain ,(sasl:plain-client-message username password)))]
                          [(AuthMechanism.scramSHA256)
                           (define username (ConnectionDetails-username c))
                           (define password (ConnectionDetails-password c))
                           (and username
                                password
                                `(SCRAM-SHA-256 ,(lambda (_host _port)
                                                   (sasl:make-scram-client-ctx 'sha256 username password))))]
                          [(AuthMechanism.scramSHA512)
                           (define username (ConnectionDetails-username c))
                           (define password (ConnectionDetails-password c))
                           (and username
                                password
                                `(SCRAM-SHA-512 ,(lambda (_host _port)
                                                   (sasl:make-scram-client-ctx 'sha512 username password))))]
                          [(AuthMechanism.aws)
                           (define region (ConnectionDetails-aws-region c))
                           (define access-key-id (ConnectionDetails-aws-access-key-id c))
                           (define secret-access-key (ConnectionDetails-password c))
                           (and region
                                access-key-id
                                secret-access-key
                                `(AWS_MSK_IAM ,(lambda (host _port)
                                                 (sasl:make-aws-msk-iam-ctx
                                                  #:region region
                                                  #:access-key-id access-key-id
                                                  #:secret-access-key secret-access-key
                                                  #:server-name host))))])
   #:ssl-ctx (and (ConnectionDetails-use-ssl c)
                  (let ([key-path (ConnectionDetails-ssl-key-path c)]
                        [crt-path (ConnectionDetails-ssl-cert-path c)])
                    (cond
                      [(or key-path crt-path)
                       (define key
                         (and key-path
                              (case (path-get-extension key-path)
                                [(#".der") `(der ,key-path)]
                                [else `(pem ,key-path)])))
                       (ssl-make-client-context
                        #:private-key key
                        #:certificate-chain crt-path
                        'auto)]
                      [else
                       (ssl-secure-client-context)])))
   #:proxy (and (ConnectionDetails-http-proxy-addr c)
                (match-let ([(regexp #rx"([^:]+):(.+)" (list _ host (app string->number port)))
                             (ConnectionDetails-http-proxy-addr c)])
                  (k:make-http-proxy host port)))))

(define-rpc (test-connection [_ c : ConnectionDetails] : (Optional String))
  (with-handlers ([exn:fail? exn-message])
    (define client (ConnectionDetails->client c))
    (begin0 (and (k:client-metadata client) #f)
      (k:disconnect-all client))))

(define-rpc (get-connections : (Listof ConnectionDetails))
  (map meta->ConnectionDetails (meta:get-connections)))

(define-rpc (get-connection [_ id : UVarint] : (Optional ConnectionDetails))
  (and~> (meta:get-connection id)
         (meta->ConnectionDetails)))

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
