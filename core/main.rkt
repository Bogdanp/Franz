#lang racket/base

(require (only-in db sqlite3-connect)
         noise/backend
         noise/serde
         racket/file

         "appdata.rkt"
         "logger.rkt"
         "metadata.rkt"

         ;; For RPC:
         "connection-details.rkt"
         "lang-lua.rkt"
         "schema-registry.rkt"
         "script.rkt"
         "workspace.rkt")

(provide
 main)

(define-rpc (ping : String)
  "pong")

(define (main in-fd out-fd)
  (module-cache-clear!)
  (collect-garbage)
  (define-values (database-path reset-trial-deadline?)
    (cond
      [(getenv "FRANZ_DATABASE_PATH")
       => (λ (p)
            (define path (make-temporary-file "franz~a.sqlite3"))
            (delete-file path)
            (unless (string=? p "x")
              (copy-file p path))
            (values path #f))]
      [else
       (values (build-application-path "metadata.sqlite3") #t)]))
  (log-franz-debug "database path: ~a" database-path)
  (define stop
    (parameterize ([current-connection
                    (sqlite3-connect
                     #:use-place 'os-thread
                     #:database database-path
                     #:mode 'create)])
      (migrate!)
      (when reset-trial-deadline?
        (reset-trial-deadline! 2023 2 15))
      (serve in-fd out-fd)))
  (with-handlers ([exn:break? void])
    (sync never-evt))
  (stop))
