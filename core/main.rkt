#lang racket/base

(require (only-in db sqlite3-connect)
         (prefix-in http: net/http-easy)
         noise/backend
         noise/serde
         racket/file

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
    (parameterize ([current-connection (make-database database-path)])
      (migrate!)
      (maybe-adjust-trial-deadline)
      (parameterize ([http:current-user-agent (make-user-agent)])
        (serve in-fd out-fd))))
  (with-handlers ([exn:break? void])
    (sync never-evt))
  (stop))

(define (make-database database-path)
  (sqlite3-connect
   #:use-place 'os-thread
   #:database database-path
   #:mode 'create))

(define (make-user-agent)
  (format "Franz ~a (Racket ~a; ~a; ~a)"
          (get-version)
          (version)
          (os-version)
          (get-buid)))

(define (maybe-adjust-trial-deadline)
  (unless (is-license-valid)
    (define delta
      (- (current-seconds)
         (get-trial-deadline)))
    (when (>= delta (* 30 86400))
      (define d (seconds->date (+ (current-seconds) (* 30 86400))))
      (reset-trial-deadline! (date-year d) (date-month d) (date-day d)))))
