#lang racket/gui/easy

(require (submod franz/script rpc)
         racket/port
         "editor.rkt"
         "observable.rkt"
         "shortcut.rkt")

(provide
 scripting-window)

(define (scripting-window id topic
                          #:enabled? [@enabled? (@ #t)]
                          #:apply-proc [apply-proc void])
  ;; HACK: Use @script/init to feed "Open"ed scripts to editor to
  ;; avoid update cycles. Editor could be improved to avoid this, but
  ;; this is less work and good enough for now.
  (define-observables
    [@script (get-script topic id)]
    [@script/init (get-script topic id)]
    [@active? #f]
    [@changed? #f]
    [@filename #f])
  (define/obs @can-apply?
    (let-observable ([active? @active?]
                     [enabled? @enabled?])
      (and enabled? (not active?))))
  (define/obs @activate-label
    (let-observable ([active? @active?])
      (if active? "&Deactivate" "&Activate")))
  (obs-observe!
   @enabled?
   (lambda (enabled?)
     (unless enabled?
       (@active?:= #f)
       (deactivate-script topic id))))
  (define (do-apply)
    (apply-proc (obs-peek @script)))
  (define (do-toggle-active)
    (update-observable [active? @active?]
      (if active?
          (deactivate-script topic id)
          (activate-script (obs-peek @script) topic id))
      (not active?)))
  (define (do-open)
    (define maybe-filename
      (gui:get-file
       #f ;message
       #f ;parent
       #f ;directory
       #f ;filename
       #f ;extension
       null ;style
       '(("Lua Script" "*.lua"))))
    (when maybe-filename
      (define text (call-with-input-file maybe-filename port->string))
      (@script:= text)
      (@script/init:= text)
      (@filename:= maybe-filename)))
  (define (do-save filename)
    (call-with-output-file filename
      #:exists 'truncate/replace
      (lambda (out)
        (display (obs-peek @script) out)))
    (@changed?:= #f))
  (define (do-save-as)
    (define maybe-filename
      (gui:put-file
       #f ;message
       #f ;parent
       #f ;directory
       #f ;filename
       #f ;extension
       null ;style
       '(("Lua Script" "*.lua"))))
    (when maybe-filename
      (do-save maybe-filename)
      (@filename:= maybe-filename)))
  (window
   #:title (let-observable ([changed? @changed?])
             (format "[~a] Script~a" topic (if changed? "*" "")))
   #:size '(800 600)
   (menu-bar
    (menu
     "&File"
     (menu-item
      "&Open..."
      #:shortcut (kbd cmd #\o)
      do-open)
     (menu-item
      (let-observable ([filename @filename])
        (if filename "&Save" "&Save..."))
      #:shortcut (kbd cmd #\s)
      (lambda ()
        (cond
          [(obs-peek @filename) => do-save]
          [else (do-save-as)])))
     (menu-item
      "Save &as..."
      #:shortcut (kbd cmd shift #\s)
      do-save-as))
    (menu
     "&Script"
     (menu-item
      "&Apply"
      #:shortcut (kbd cmd shift #\return)
      do-apply)
     (menu-item
      @activate-label
      #:shortcut (kbd cmd #\return)
      do-toggle-active)))
   (vpanel
    (hpanel
     #:stretch '(#t #f)
     #:alignment '(right center)
     (button
      "Apply"
      #:enabled? @can-apply?
      do-apply)
     (button
      #:style '(border)
      #:enabled? @enabled?
      @activate-label
      do-toggle-active))
    (editor
     @script/init
     (lambda (script)
       (deactivate-script topic id)
       (@active?:= #f)
       (@changed?:= #t)
       (@script:= script))))))

(module+ main
  (require "testing.rkt")
  (call-with-testing-context
   (lambda (id)
     (render
      (scripting-window id "example-topic")))))
