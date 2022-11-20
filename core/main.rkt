#lang racket/base

(require (only-in db sqlite3-connect)
         (prefix-in dbg: debugging/server)
         noise/backend
         noise/serde

         "appdata.rkt"
         "metadata.rkt"

         ;; For RPC:
         "connection-details.rkt"
         "lang-lua.rkt"
         "script.rkt"
         "workspace.rkt")

(provide
 main)

(define-rpc (ping : String)
  "pong")

(define (main in-fd out-fd)
  (module-cache-clear!)
  (collect-garbage)
  (define stop-debugging
    (if (getenv "FRANZ_DEBUG") (dbg:serve) void))
  (define database-path
    (build-application-path "metadata.sqlite3"))
  (define stop
    (parameterize ([current-connection
                    (sqlite3-connect
                     #:use-place 'os-thread
                     #:database database-path
                     #:mode 'create)])
      (migrate!)
      (reset-trial-deadline! 2023 3 1)
      (serve in-fd out-fd)))
  (with-handlers ([exn:break? void])
    (sync never-evt))
  (stop)
  (stop-debugging))
