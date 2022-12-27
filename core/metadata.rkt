#lang racket/base

(require (for-syntax racket/base)
         db
         deta
         noise/backend
         noise/serde
         racket/contract
         racket/date
         racket/port
         racket/runtime-path
         racket/sequence
         racket/string
         threading
         "license.rkt"
         "query.rkt")

(provide
 current-connection
 migrate!

 (schema-out connection-details)
 get-connections
 insert-connection!
 update-connection!
 touch-connection!
 delete-connection!

 (schema-out schema-registry)
 get-schema-registry
 insert-schema-registry!
 update-schema-registry!
 delete-schema-registry!

 reset-trial-deadline!)

(define-logger metadata)

(define-runtime-path migrations
  "migrations")

(define-query-definer define-queries
  "metadata")

(define id/c
  exact-nonnegative-integer?)

(define/contract current-connection
  (parameter/c (or/c #f connection?))
  (make-parameter #f))

(define-syntax (conn stx)
  (syntax-case stx ()
    [id (identifier? #'id) #'(current-connection)]))

(define (migrate!)
  (define-queries
    [create-stmt "create-migrations-table.sql"]
    [track-stmt "track-migration.sql"]
    [applied?-stmt "migration-is-applied.sql"])
  (query-exec conn create-stmt)
  (for ([filename (in-list (sort (directory-list migrations) path<?))])
    (define filename-str (path->string filename))
    (when (zero? (query-value conn applied?-stmt filename-str))
      (log-metadata-debug "running migration '~a'" filename-str)
      (call-with-transaction conn
        (lambda ()
          (define statements
            (call-with-input-file (build-path migrations filename)
              (lambda (in)
                (regexp-split #rx"\\s*;\\s*" (port->string in)))))
          (for ([statement (in-list statements)])
            (unless (string=? "" (string-trim statement))
              (query-exec conn statement)))
          (query-exec conn track-stmt filename-str (current-seconds)))))))


;; connection-details ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define auth-mechanism/c
  (or/c 'plain 'scram-sha-256 'scram-sha-512 'aws-msk-iam))

(define-schema connection-details
  #:table "connection_details"
  ([id id/f #:primary-key #:auto-increment]
   [name string/f #:contract non-empty-string?]
   [(bootstrap-host "127.0.0.1") string/f #:contract non-empty-string?]
   [(bootstrap-port 9092) integer/f #:contract (integer-in 0 65535)]
   [(auth-mechanism 'plain) symbol/f #:contract auth-mechanism/c]
   [(username sql-null) string/f #:nullable]
   [(password-id sql-null) string/f #:nullable]
   [(aws-region sql-null) string/f #:nullable]
   [(aws-access-key-id sql-null) string/f #:nullable]
   [(ssl-on? #f) boolean/f]
   [(schema-registry-id sql-null) integer/f #:nullable]
   [(created-at (current-seconds)) integer/f]
   [(updated-at (current-seconds)) integer/f]
   [(last-used-at (current-seconds)) integer/f])
  #:pre-persist-hook
  (lambda (c)
    (set-connection-details-updated-at c (current-seconds))))

(define/contract (get-connections)
  (-> (listof connection-details?))
  (sequence->list
   (in-entities conn (~> (from connection-details #:as c)
                         (order-by ([c.last-used-at #:desc]))))))

(define/contract (insert-connection! c)
  (-> connection-details? connection-details?)
  (insert-one! conn c))

(define/contract (update-connection! c)
  (-> connection-details? connection-details?)
  (update-one! conn c #:force? #t))

(define/contract (touch-connection! id)
  (-> id/c void?)
  (query-exec conn (~> (from connection-details #:as c)
                       (update [updated-at ,(current-seconds)]
                               [last-used-at ,(current-seconds)])
                       (where (= c.id ,id)))))

(define/contract (delete-connection! id)
  (-> id/c void?)
  (query-exec conn (~> (from connection-details #:as c)
                       (where (= c.id ,id))
                       (delete))))

(module+ test
  (require rackunit)

  (define (call-with-test-connection proc)
    (let ([conn #f])
      (dynamic-wind
        (lambda ()
          (set! conn (sqlite3-connect #:database 'memory)))
        (lambda ()
          (parameterize ([current-connection conn])
            (migrate!)
            (proc conn)))
        (lambda ()
          (disconnect conn)))))

  (test-case "migrate!"
    (call-with-test-connection void))

  (test-case "connection crud"
    (call-with-test-connection
     (lambda (_)
       (define c
         (insert-connection!
          (make-connection-details
           #:name "Unnamed Connection"
           #:ssl-on? #t)))
       (check-not-false (member c (get-connections)))
       (set! c (update-connection!
                (set-connection-details-ssl-on? c #f)))
       (check-not-false (member c (get-connections)))
       (delete-connection! (connection-details-id c))
       (check-true (null? (get-connections)))))))


;; schema-registry ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define schema-registry-kind/c
  (or/c 'confluent))

(define-schema schema-registry
  #:table "schema_registries"
  ([id id/f #:primary-key #:auto-increment]
   [(kind 'confluent) symbol/f #:contract schema-registry-kind/c]
   [url string/f]
   [(username sql-null) string/f #:nullable]
   [(password-id sql-null) string/f #:nullable]
   [(created-at (current-seconds)) integer/f]
   [(updated-at (current-seconds)) integer/f])
  #:pre-persist-hook
  (lambda (c)
    (set-schema-registry-updated-at c (current-seconds))))

(define/contract (get-schema-registry id)
  (-> id/c (or/c #f schema-registry?))
  (lookup conn (~> (from schema-registry #:as r)
                   (where (= r.id ,id)))))

(define/contract (insert-schema-registry! r)
  (-> schema-registry? schema-registry?)
  (insert-one! conn r))

(define/contract (update-schema-registry! r)
  (-> schema-registry? schema-registry?)
  (update-one! conn r #:force? #t))

(define/contract (delete-schema-registry! id)
  (-> id/c void?)
  (query-exec conn (~> (from schema-registry #:as r)
                       (where (= r.id ,id))
                       (delete))))

(module+ test
  (test-case "registry crud"
    (call-with-test-connection
     (lambda (_)
       (define r
         (insert-schema-registry!
          (make-schema-registry
           #:url "http://localhost:8080"
           #:username "example"
           #:password-id "password123")))
       (define c
         (insert-connection!
          (make-connection-details
           #:name "Example Connection"
           #:schema-registry-id (schema-registry-id r))))
       (check-equal? (get-schema-registry (connection-details-schema-registry-id c)) r)

       (define updated-r
         (~> (set-schema-registry-username r sql-null)
             (set-schema-registry-password-id sql-null)))
       (check-equal?
        (update-schema-registry! updated-r)
        (get-schema-registry (schema-registry-id r)))))))


;; license ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-schema metadata
  #:table "metadata"
  ([key symbol/f #:primary-key]
   [value string/f]
   [(updated-at (current-seconds)) integer/f])
  #:pre-persist-hook
  (lambda (m)
    (set-metadata-updated-at m (current-seconds))))

(define (metadata-query key)
  (~> (from metadata #:as m)
      (where (= m.key ,(symbol->string key)))))

(define (get-metadata key [default #f])
  (call-with-transaction conn
    (lambda ()
      (cond
        [(lookup conn (metadata-query key)) => metadata-value]
        [(procedure? default)
         (define value (default))
         (begin0 value
           (put-metadata! key value))]
        [else
         #f]))))

(define (put-metadata! key value)
  (void
   (call-with-transaction conn
     (lambda ()
       (cond
         [(lookup conn (metadata-query key))
          => (lambda (m)
               (~> (set-metadata-value m value)
                   (update-one! conn _)))]
         [else
          (~> (make-metadata #:key key #:value value)
              (insert-one! conn _))])))))

(define (reset-trial-deadline! year month day)
  (define seconds (find-seconds 0 0 0 day month year #t))
  (put-metadata! 'trial-deadline (number->string seconds)))

(define-rpc (is-license-valid : Bool)
  (or (get-license)
      (> (get-trial-deadline)
         (current-seconds))))

(define-rpc (get-trial-deadline : Varint)
  (string->number
   (get-metadata
    'trial-deadline
    (lambda ()
      (number->string
       (+ (current-seconds) (* 30 86400)))))))

(define-rpc (get-license : (Optional String))
  (and~> (get-metadata 'license)
         (parse-license)
         (~license-key)))

(define-rpc (activate-license [_ key : String] : Bool)
  (and (parse-license key)
       (put-metadata! 'license key)
       #t))

(module+ test
  (test-case "get-trial-deadline"
    (call-with-test-connection
     (lambda (_)
       (define deadline (get-trial-deadline))
       (check-true (> deadline (current-seconds)))
       (check-equal? (get-trial-deadline) deadline))))

  (test-case "reset-trial-deadline"
    (call-with-test-connection
     (lambda (_)
       (define deadline (get-trial-deadline))
       (check-true (> deadline (current-seconds)))
       (define target-seconds (find-seconds 0 0 0 1 3 2023 #t))
       (reset-trial-deadline! 2023 3 1)
       (check-equal? (get-trial-deadline) target-seconds))))

  (test-case "get license"
    (test-case "no licenses"
      (call-with-test-connection
       (lambda (_)
         (check-false (get-license)))))

    (test-case "activated license"
      (call-with-test-connection
       (lambda (_)
         (define key (~license-key (generate-random-license)))
         (check-not-false (activate-license key))
         (define the-license (get-license))
         (check-not-false the-license)
         (check-equal? the-license key))))))
