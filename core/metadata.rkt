#lang racket/base

(require (for-syntax racket/base)
         db
         deta
         racket/contract
         racket/port
         racket/runtime-path
         racket/sequence
         racket/string
         threading
         "query.rkt")

(provide
 engine/c
 current-connection
 migrate!

 (schema-out connection-details)
 get-connections
 insert-connection!
 touch-connection!
 delete-connection!)

(define-logger metadata)

(define-runtime-path migrations
  "migrations")

(define-query-definer define-queries
  "metadata")

(define id/c
  exact-nonnegative-integer?)

(define engine/c
  (or/c 'postgresql))

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
      (query-exec conn (call-with-input-file (build-path migrations filename) port->string))
      (query-exec conn track-stmt filename-str (current-seconds)))))

(define-schema connection-details
  #:table "connection_details"
  ([id id/f #:primary-key #:auto-increment]
   [name string/f #:contract non-empty-string?]
   [(bootstrap-host "127.0.0.1") string/f #:contract non-empty-string?]
   [(bootstrap-port 9092) integer/f #:contract (integer-in 0 65535)]
   [(username sql-null) string/f #:nullable]
   [(password sql-null) string/f #:nullable]
   [(ssl-on? #f) boolean/f]
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
            (proc conn)))
        (lambda ()
          (disconnect conn)))))

  (test-case "migrate!"
    (call-with-test-connection
     (lambda (_)
       (migrate!))))

  (test-case "connection crud"
    (call-with-test-connection
     (lambda (_)
       (migrate!)
       (define c
         (insert-connection!
          (make-connection-details
           #:name "Unnamed Connection"
           #:ssl-on? #t)))
       (check-not-false (member c (get-connections)))
       (delete-connection! (connection-details-id c))
       (check-true (null? (get-connections)))))))
