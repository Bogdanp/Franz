#lang racket/base

(require (only-in db sqlite3-connect)
         noise/backend
         noise/serde

         "appdata.rkt"
         "metadata.rkt"

         ;; For RPC:
         "connection-details.rkt"
         "workspace.rkt")

(provide
 main)

(define-rpc (ping : String)
  "pong")

(define (main in-fd out-fd)
  (module-cache-clear!)
  (collect-garbage)
  (define database-path
    (build-application-path "metadata.sqlite3"))
  (define stop
    (parameterize ([current-connection
                    (sqlite3-connect
                     #:use-place 'os-thread
                     #:database database-path
                     #:mode 'create)])
      (migrate!)
      (serve in-fd out-fd)))
  (with-handlers ([exn:break? (λ (_) (stop))])
    (sync never-evt)))
