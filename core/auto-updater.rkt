#lang racket/base

(require (for-syntax racket/base
                     racket/syntax
                     syntax/parse)
         racket/match
         "logger.rkt"
         "release.rkt")

(provide
 auto-updater?
 make-auto-updater)

(define-logger auto-updater
  #:parent franz-logger)

(struct auto-updater (thd ch)
  #:transparent)

(struct req (res res-ch nack))

(define (make-check-evt frequency)
  (alarm-evt (+ (current-inexact-monotonic-milliseconds) frequency) #t))

(define (make-auto-updater handler-proc
                           #:arch [arch (system-type 'arch)]
                           #:version sw-version
                           #:frequency [frequency (* 1 3600 1000)])
  (define ch (make-channel))
  (define (do-check-for-updates)
    (log-auto-updater-info "checking for updates (version: ~a, arch: ~a)" sw-version arch)
    (with-handlers ([exn:fail?
                     (lambda (e)
                       (log-auto-updater-error
                        "failed to check for updates: ~a"
                        (exn-message e)))])
      (define release (get-latest-release arch))
      (if (and release (string>? (Release-version release) sw-version))
          (values (get-changelog) release)
          (values #f #f))))
  (define thd
    (thread/suspend-to-kill
     (lambda ()
       (let loop ([running? #t]
                  [requests null])
         (when (or running? (not (null? requests)))
           (apply
            sync
            (handle-evt
             (if running? ch never-evt)
             (lambda (msg)
               (match msg
                 [`(stop ,res-ch ,nack)
                  (loop #f (cons (req #t res-ch nack) requests))]
                 [`(check ,res-ch ,nack)
                  (define-values (changelog release)
                    (do-check-for-updates))
                  (define res
                    (list changelog release))
                  (loop running? (cons (req res res-ch nack) requests))]
                 [_
                  (log-auto-updater-warning "ignoring invalid message: ~e" msg)
                  (loop running? requests)])))
            (handle-evt
             (if (and running? frequency) (make-check-evt frequency) never-evt)
             (lambda (_)
               (define-values (changelog release)
                 (do-check-for-updates))
               (when (and changelog release)
                 (handler-proc changelog release))
               (loop #t requests)))
            (append
             (for/list ([r (in-list requests)])
               (handle-evt
                (channel-put-evt
                 (req-res-ch r)
                 (req-res r))
                (lambda (_)
                  (loop running? (remq r requests)))))
             (for/list ([r (in-list requests)])
               (handle-evt
                (req-nack r)
                (lambda (_)
                  (loop running? (remq r requests)))))))))
       (log-auto-updater-debug "stopped"))))
  (auto-updater thd ch))

(define (auto-updater-evt au id . args)
  (nack-guard-evt
   (lambda (nack)
     (define res-ch
       (make-channel))
     (begin0 res-ch
       (thread-resume
        (auto-updater-thd au)
        (current-thread))
       (channel-put
        (auto-updater-ch au)
        `(,id ,@args ,res-ch ,nack))))))

(define-syntax (define-auto-updater-methods stx)
  (syntax-parse stx
    [(_ [method-id:id arg-id:id ...] ...+)
     #:with (proc-id ...) (for/list ([method-id-stx (in-list (syntax-e #'(method-id ...)))])
                            (format-id method-id-stx "auto-updater-~a" method-id-stx))
     #'(begin
         (provide proc-id ...)
         (define (proc-id au arg-id ...)
           (sync (auto-updater-evt au 'method-id arg-id ...))) ...)]))

(define-auto-updater-methods
  [stop]
  [check])
