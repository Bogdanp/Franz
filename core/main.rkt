#lang racket/base

(require (only-in db sqlite3-connect)
         (prefix-in http: net/http-easy)
         noise/backend
         noise/serde
         racket/file
         sentry

         "appdata.rkt"
         "logger.rkt"
         "metadata.rkt"
         "platform.rkt"
         "version.rkt"

         ;; For RPC:
         "auto-update.rkt"
         "connection-details.rkt"
         "lexer.rkt"
         "release.rkt"
         "schema-registry.rkt"
         "script.rkt"
         "secret.rkt"
         "workspace.rkt")

(provide
 call-with-main-parameterization
 main)

(define-rpc (ping : String)
  "pong")

(define (call-with-main-parameterization proc)
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
  (parameterize ([current-connection (make-database database-path)]
                 [current-sentry (make-sentry sentry-dsn #:release franz-version)])
    (migrate!)
    (maybe-adjust-trial-deadline)
    (parameterize ([current-sentry-user (make-sentry-user #:id (get-buid))]
                   [http:current-user-agent (make-user-agent)])
      (dynamic-wind
        (λ () (void))
        (λ () (proc))
        (λ () (sentry-stop))))))

(define (main in-fd out-fd)
  (call-with-main-parameterization
   (lambda ()
     (define stop
       (serve in-fd out-fd))
     (with-handlers ([exn:break? void])
       (sync never-evt))
     (stop))))

(define (make-database database-path)
  (sqlite3-connect
   #:use-place 'os-thread
   #:database database-path
   #:mode 'create))

(define (make-user-agent)
  (format "Franz ~a (Racket ~a; ~a; ~a)"
          franz-version
          (version)
          (os-version)
          (get-buid)))
