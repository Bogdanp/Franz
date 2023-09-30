#lang racket/base

(require db
         franz/connection-details
         franz/metadata
         franz/pool
         racket/date
         racket/gui)

(provide
 call-with-testing-context)

(define (call-with-testing-context proc)
  (define conn (sqlite3-connect #:database 'memory))
  (parameterize ([current-connection conn]
                 [date-display-format 'iso-8601])
    (migrate!)
    (define id #f)
    (define eventspace (make-eventspace))
    (parameterize ([current-eventspace eventspace])
      (define conf
        (make-ConnectionDetails
         #:id 1
         #:name "Example"
         #:bootstrap-host "kafka-1"
         #:bootstrap-port 9092))
      (dynamic-wind
        (λ () (set! id (pool-open conf)))
        (λ () (proc id))
        (λ ()
          (with-handlers ([exn:break? void])
            (yield eventspace))
          (pool-close id))))))
