#lang racket/gui/easy

(require franz/auto-updater
         franz/release
         franz/version
         racket/match
         "common.rkt"
         "mixin.rkt"
         "preference.rkt")

(provide
 start-auto-updater
 stop-auto-updater
 restart-auto-updater
 check-for-updates-dialog)

(define the-auto-updater #f)

(define (start-auto-updater)
  (set! the-auto-updater
        (parameterize ([current-custodian (make-custodian)])
          (make-auto-updater
           (lambda (changelog release)
             (when (and changelog release)
               (void)))
           #:arch (system-type 'arch)
           #:version franz-version
           #:frequency (and
                        (get-preference 'auto-update:check? #t)
                        (get-preference 'auto-update:interval 3600))))))

(define (stop-auto-updater)
  (void (auto-updater-stop the-auto-updater)))

(define (restart-auto-updater)
  (stop-auto-updater)
  (start-auto-updater))

(define (updates-available-window changelog release)
  (define close! void)
  (window
   #:title "Updates Available"
   #:size '(720 #f)
   #:mixin (mix-close-window
            void
            (lambda (close!-proc)
              (set! close! close!-proc)))
   (hpanel
    #:alignment '(left top)
    #:margin '(10 10)
    (image
     #:size '(64 64)
     icon_512x512.png)
    (vpanel
     (input
      #:style '(multiple)
      #:min-size '(#f 180)
      changelog)
     (hpanel
      #:alignment '(right center)
      #:stretch '(#t #f)
      (button
       "Cancel"
       (λ () (close!)))
      (button
       #:style '(border)
       "Download Update"
       void))))))

(define (check-for-updates-dialog)
  (define close! void)
  (define/obs @value 0)
  (define value-thd
    (thread
     (lambda ()
       (with-handlers ([exn:break? (λ (_) (@value . := . 100))])
         (let loop ([value 0])
           (sleep (/ 1.0 60))
           (@value . := . value)
           (loop (modulo (add1 value) 100)))))))
  (thread
   (lambda ()
     (match (auto-updater-check the-auto-updater)
       [(list #f #f)
        (break-thread value-thd)
        (render
         (up-to-date-dialog))]
       [(list changelog release)
        (break-thread value-thd)
        (void) ;; FIXME
        ])
     (gui:queue-callback close!)))
  (dialog
   #:title "Checking for Updates"
   #:size '(480 #f)
   #:mixin (mix-close-window
            (lambda ()
              (break-thread value-thd)
              (sync (system-idle-evt)))
            (lambda (close!-proc)
              (set! close! close!-proc)))
   (hpanel
    #:margin '(10 20)
    (image
     #:size '(48 48)
     icon_512x512.png)
    (vpanel
     #:alignment '(left center)
     #:stretch '(#t #f)
     (text "Checking for updates...")
     (progress @value)))))

(define (up-to-date-dialog)
  (define close! void)
  (dialog
   #:size '(240 #f)
   #:title "Up to Date"
   #:mixin (mix-close-window
            void
            (lambda (close!-proc)
              (set! close! close!-proc)))
   (vpanel
    #:margin '(10 20)
    (image
     #:size '(64 64)
     icon_512x512.png)
    (text "You're up to date!")
    (button
     #:style '(border)
     "OK"
     (λ () (close!))))))

(module+ main
  (require racket/port)
  (define here (syntax-source #'here))
  (start-auto-updater)
  (render (check-for-updates-dialog))
  (render
   (updates-available-window
    (call-with-input-file (build-path here 'up 'up "website" "versions" "changelog.txt")
      port->string)
    (make-Release
     #:arch (system-type 'arch)
     #:version franz-version
     #:mac-url "https://example.com"))))
