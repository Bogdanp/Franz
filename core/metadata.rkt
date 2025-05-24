#lang racket/base

(require (for-syntax racket/base)
         buid
         db
         deta
         noise/backend
         noise/serde
         racket/contract/base
         racket/date
         racket/port
         racket/runtime-path
         racket/sequence
         racket/string
         threading
         "license.rkt"
         "query.rkt")

(provide
 (schema-out connection-details)
 (schema-out schema-registry)
 (contract-out
  [current-connection (parameter/c (or/c #f connection?))]
  [migrate! (-> void?)]

  [get-connections (-> (listof connection-details?))]
  [get-connection (-> id/c (or/c #f connection-details?))]
  [insert-connection! (-> connection-details? connection-details?)]
  [update-connection! (-> connection-details? connection-details?)]
  [touch-connection! (-> id/c void?)]
  [delete-connection! (-> id/c void?)]

  [get-schema-registry (-> id/c (or/c #f schema-registry?))]
  [insert-schema-registry! (-> schema-registry? schema-registry?)]
  [update-schema-registry! (-> schema-registry? schema-registry?)]
  [delete-schema-registry! (-> id/c void?)]

  [maybe-adjust-trial-deadline (-> void?)]
  [get-buid (-> string?)]))

(define-logger metadata)

(define-runtime-path migrations
  "migrations")

(define-query-definer define-queries
  "metadata")

(define id/c
  exact-nonnegative-integer?)

(define current-connection
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
   [(http-proxy-addr sql-null) string/f #:nullable]
   [(bootstrap-host "127.0.0.1") string/f #:contract non-empty-string?]
   [(bootstrap-port 9092) integer/f #:contract (integer-in 0 65535)]
   [(auth-mechanism 'plain) symbol/f #:contract auth-mechanism/c]
   [(username sql-null) string/f #:nullable]
   [(password-id sql-null) string/f #:nullable]
   [(aws-region sql-null) string/f #:nullable]
   [(aws-access-key-id sql-null) string/f #:nullable]
   [(aws-session-token sql-null) string/f #:nullable]
   [(ssl-on? #f) boolean/f]
   [(ssl-key-path sql-null) string/f #:nullable]
   [(ssl-cert-path sql-null) string/f #:nullable]
   [(schema-registry-id sql-null) integer/f #:nullable]
   [(created-at (current-seconds)) integer/f]
   [(updated-at (current-seconds)) integer/f]
   [(last-used-at (current-seconds)) integer/f])
  #:pre-persist-hook
  (lambda (c)
    (set-connection-details-updated-at c (current-seconds))))

(define (get-connections)
  (sequence->list
   (in-entities conn (~> (from connection-details #:as c)
                         (order-by ([c.last-used-at #:desc]))))))

(define (get-connection id)
  (lookup conn (~> (from connection-details #:as c)
                   (where (= c.id ,id)))))

(define (insert-connection! c)
  (insert-one! conn c))

(define (update-connection! c)
  (update-one! conn c #:force? #t))

(define (touch-connection! id)
  (query-exec conn (~> (from connection-details #:as c)
                       (update [updated-at ,(current-seconds)]
                               [last-used-at ,(current-seconds)])
                       (where (= c.id ,id)))))

(define (delete-connection! id)
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

(define (get-schema-registry id)
  (lookup conn (~> (from schema-registry #:as r)
                   (where (= r.id ,id)))))

(define (insert-schema-registry! r)
  (insert-one! conn r))

(define (update-schema-registry! r)
  (update-one! conn r #:force? #t))

(define (delete-schema-registry! id)
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

(define trial-duration (* 30 86400))
(define trial-grace-period (* 30 86400))

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
       (+ (current-seconds) trial-duration))))))

(define-rpc (get-license : (Optional String))
  (and~> (get-metadata 'license)
         (parse-license)
         (~license-key)))

(define-rpc (activate-license [_ key : String] : Bool)
  (and~> (parse-license key)
         (do-activate-license)))

(define (do-activate-license l)
  (cond
    [(trial-reset-license? l)
     (define d (seconds->date (+ (license-seconds l) trial-duration)))
     (begin0 #t
       (reset-trial-deadline! (date-year d) (date-month d) (date-day d)))]
    [else
     (begin0 #t
       (put-metadata! 'license (~license-key l)))]))

(define (maybe-adjust-trial-deadline)
  (unless (is-license-valid)
    (define delta
      (- (current-seconds)
         (get-trial-deadline)))
    (when (>= delta trial-grace-period)
      (define d (seconds->date (+ (current-seconds) trial-duration)))
      (reset-trial-deadline! (date-year d) (date-month d) (date-day d)))))

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
         (check-equal? the-license key)))))

  (test-case "deadline adjustment"
    (test-case "not necessary"
      (call-with-test-connection
       (lambda (_)
         (define deadline (get-trial-deadline))
         (maybe-adjust-trial-deadline)
         (check-equal? deadline (get-trial-deadline)))))
    (test-case "necessary"
      (call-with-test-connection
       (lambda (_)
         (define d (seconds->date (- (current-seconds) trial-grace-period)))
         (reset-trial-deadline! (date-year d) (date-month d) (date-day d))
         (maybe-adjust-trial-deadline)
         (check-true (> (get-trial-deadline) (current-seconds))))))))


;; buids ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (get-buid)
  (get-metadata 'buid (lambda () (buid))))
