#lang racket/gui/easy

(require franz/auto-updater
         franz/release
         franz/version
         (prefix-in http: net/http-easy)
         net/sendurl
         racket/class
         racket/list
         racket/match
         racket/string
         "common.rkt"
         "mixin.rkt"
         "preference.rkt"
         "thread.rkt")

(provide
 start-auto-updater
 stop-auto-updater
 restart-auto-updater
 check-for-updates-dialog)

(define the-auto-updater #f)

(define (start-auto-updater [check-now? #t])
  (define check-for-updates?
    (get-preference 'auto-update:check? #t))
  (define check-interval
    (* (get-preference 'auto-update:interval 3600) 1000))
  (define updater
    (parameterize ([current-custodian (make-custodian)])
      (make-auto-updater
       do-update-available
       #:arch (system-type 'arch)
       #:version franz-version
       #:frequency (and check-for-updates? check-interval))))
  (when (and check-for-updates? check-now?)
    (thread*
     (match (auto-updater-check updater)
       [`(#f #f) (void)]
       [`(,changelog ,release)
        (do-update-available changelog release)])))
  (set! the-auto-updater updater))

(define (stop-auto-updater)
  (void (auto-updater-stop the-auto-updater)))

(define (restart-auto-updater)
  (stop-auto-updater)
  (start-auto-updater #f))

(define (do-update-available changelog release)
  (when (and changelog release (Release-url release))
    (updates-available-window changelog release)))

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
      #:mixin (λ (%)
                (class %
                  (inherit get-editor)
                  (super-new)
                  (send (get-editor) scroll-to-position 0)))
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
       (λ ()
         (define dst-directory
           (gui:get-directory))
         (when dst-directory
           (define/obs @progress 0)
           (define close-dialog! void)
           (define download-thd
             (thread
              (lambda ()
                (define dst-path
                  (download-release release dst-directory (λ:= @progress)))
                (when dst-path
                  (sync (system-idle-evt))
                  (close-dialog!)
                  (close!)
                  (send-url/file dst-path)))))
           (render
            (dialog
             #:title "Downloading Update"
             #:min-size '(240 #f)
             #:mixin (mix-close-window
                      void
                      (lambda (close!-proc)
                        (set! close-dialog! close!-proc)))
             (hpanel
              #:margin '(10 10)
              #:alignment '(left top)
              (image
               #:size '(32 32)
               icon_512x512.png)
              (vpanel
               #:alignment '(right top)
               (progress @progress)
               (button "Cancel" (λ () (close-dialog!)))))))
           (break-thread download-thd)))))))))

(define (Release-url r)
  ((case (system-type 'os)
     [(unix) Release-linux-url]
     [(macosx) Release-mac-url]
     [(windows) Release-windows-url])
   r))

(define (download-release release dst-directory on-progress)
  (define url
    (Release-url release))
  (define head-response (http:head url))
  (define content-length
    (string->number
     (bytes->string/utf-8
      (http:response-headers-ref head-response 'content-length))))
  (define dst-path (build-path dst-directory (last (string-split url "/"))))
  (define res (http:get #:stream? #t url))
  (define ok?
    (call-with-output-file dst-path
      #:exists 'truncate/replace
      (lambda (out)
        (with-handlers ([exn:break? (λ (_) #f)])
          (parameterize-break #t
            (define buf (make-bytes (* 1024 1024)))
            (let loop ([total 0])
              (on-progress (inexact->exact (round (* (/ total content-length) 100))))
              (define n-read (read-bytes! buf (http:response-output res)))
              (unless (eof-object? n-read)
                (write-bytes buf out 0 n-read)
                (loop (+ total n-read)))))
          'ok))))
  (unless ok?
    (delete-file dst-path))
  (and ok? dst-path))

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
  (define (stop-value-thd!)
    (break-thread value-thd)
    (sync (system-idle-evt)))
  (thread
   (lambda ()
     (match-define (list changelog release)
       (auto-updater-check the-auto-updater))
     (stop-value-thd!)
     (render
      (if (and changelog release (Release-url release))
          (updates-available-window changelog release)
          (up-to-date-dialog)))
     (gui:queue-callback close!)))
  (dialog
   #:title "Checking for Updates"
   #:size '(480 #f)
   #:mixin (mix-close-window
            (lambda ()
              (stop-value-thd!))
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
  (define-values (here-dir _filename _is-dir?)
    (split-path here))
  (start-auto-updater)
  (render (check-for-updates-dialog))
  (render
   (updates-available-window
    (call-with-input-file (build-path here-dir 'up "website" "versions" "changelog.txt")
      port->string)
    (make-Release
     #:arch (system-type 'arch)
     #:version franz-version
     #:mac-url "https://franz.defn.io/releases/Franz%201.0.0006.universal.dmg"
     #:linux-url #f
     #:windows-url #f))))
