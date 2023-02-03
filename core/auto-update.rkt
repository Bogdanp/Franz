#lang racket/base

(require noise/backend
         noise/serde
         racket/match
         "auto-updater.rkt"
         "release.rkt")

(provide
 start-auto-updater
 check-for-updates
 stop-auto-updater)

(define updater-custodian (make-custodian))
(define updater #f)

(define-record UpdateAvailable
  [changelog : String]
  [release : Release])

(define-callout (announce-update [changelog : String]
                                 [release : Release]))

(define-rpc (start-auto-updater [with-frequency frequency : (Optional UVarint)]
                                [and-arch arch : Symbol]
                                [and-version version : String])
  (set! updater (parameterize ([current-custodian updater-custodian])
                  (make-auto-updater
                   announce-update
                   #:arch arch
                   #:version version
                   #:frequency frequency))))

(define-rpc (check-for-updates : (Optional UpdateAvailable))
  (match (auto-updater-check updater)
    [(list #f #f) #f]
    [(list changelog release)
     (make-UpdateAvailable
      #:changelog changelog
      #:release release)]))

(define-rpc (stop-auto-updater)
  (void (auto-updater-stop updater)))
