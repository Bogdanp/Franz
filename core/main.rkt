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
  (define database-path
    (cond
      [(getenv "FRANZ_DATABASE_PATH")
       => (λ (p)
            (define path (make-temporary-file "franz~a.sqlite3"))
            (delete-file path)
            (begin0 path
              (unless (string=? p "x")
                (copy-file p path))))]
      [else
       (build-application-path "metadata.sqlite3")]))
  (log-franz-debug "database path: ~a" database-path)
  (define stop
    (parameterize ([current-connection
                    (sqlite3-connect
                     #:use-place 'os-thread
                     #:database database-path
                     #:mode 'create)])
      (migrate!)
      (serve in-fd out-fd)))
  (with-handlers ([exn:break? void])
    (sync never-evt))
  (stop))
