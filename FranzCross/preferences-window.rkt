#lang racket/gui/easy

(require browser/external
         (submod franz/metadata rpc)
         (prefix-in p: pict)
         racket/date
         racket/gui/easy/font
         racket/string
         "canvas-list.rkt"
         "combinator.rkt"
         "common.rkt"
         "mixin.rkt"
         "observable.rkt"
         "preference.rkt"
         "view.rkt")

(provide
 preferences-window)

(define (preferences-window @current-view)
  (window
   #:title "Preferences"
   #:min-size '(640 320)
   (hpanel
    (hpanel
     #:min-size '(180 #f)
     #:stretch '(#f #t)
     (canvas-list
      #:mixin mix-initial-focus
      (@ '(general connections license updates))
      #:action
      (λ (type item _event)
        (case type
          [(select)
           (@current-view . := . item)]))
      #:item-height 30
      (λ (item state dc w h)
        (define the-pict
          (label-pict item state w h))
        (p:draw-pict the-pict dc 0 0))
      #:selected-item @current-view))
    (match-view @current-view
      ['general
       (define/obs @reload-ival
         (get-preference 'general:reload-interval 5))
       (obs-observe! @reload-ival (λ (ival) (put-preference 'general:reload-interval ival)))
       (detail-view
        "General"
        (labeled
         #:alignment '(right top)
         #:width 100
         "Reload Interval:"
         (vpanel
          #:stretch '(#t #f)
          (slider
           #:min-value 1
           #:max-value 30
           #:style '(horizontal horizontal-label plain)
           @reload-ival
           (λ:= @reload-ival))
          (text
           (let-observable ([ival @reload-ival])
             (format "Every ~a seconds." ival))))))]
      ['connections
       (detail-view
        "Connections")]
      ['license
       (define-observables
         [@activated? (and (get-license) #t)]
         [@license ""])
       (detail-view
        "License"
        (if-view
         @activated?
         (vpanel
          #:alignment '(left top)
          (text
           #:font (font #:weight 'bold system-font font-size-l)
           "Full Version Activated")
          (text
           #:font system-font-m
           (string-join
            '("Thank you for supporting Franz development by"
              "purchasing a license.")
            "\n")))
         (vpanel
          #:alignment '(left top)
          #:stretch '(#t #f)
          (text
           (format
            "Your trial ~a on ~a."
            (if (is-license-valid) "expires" "expired")
            (date->string (seconds->date (get-trial-deadline)))))
          (hpanel
           (hpanel
            (text "License:")
            (input @license (drop1 (λ:= @license))))
           (button
            "Activate"
            (lambda ()
              (when (activate-license ^@license)
                (@activated?:= #t)))))
          (button
           "Purchase a License"
           (lambda ()
             (send-url "https://franz.defn.io/buy.html"))))))]
      ['updates
       (define-observables
         [@check-for-updates? (get-preference 'auto-update:check? #t)]
         [@update-interval (get-preference 'auto-update:interval 14400)])
       (obs-observe! @check-for-updates? (λ (check?) (put-preference 'auto-update:check? check?)))
       (obs-observe! @update-interval (λ (ival) (put-preference 'auto-update:interval ival)))
       (detail-view
        "Updates"
        (labeled
         ""
         (checkbox
          (λ:= @check-for-updates?)
          #:label "Automatically check for updates"
          #:checked? @check-for-updates?))
        (labeled
         "Check interval:"
         (choice
          '(3600 14400 86400)
          #:choice->label
          (λ (ival)
            (case ival
              [(3600) "Every hour"]
              [(14400) "Every four hours"]
              [(86400) "Every day"]))
          #:selection @update-interval
          #:enabled? @check-for-updates?
          (λ:= @update-interval))))]))))

(define (detail-view title . content)
  (vpanel
   #:alignment '(left top)
   #:margin '(15 10)
   (text
    #:font system-font-xl
    title)
   (apply
    vpanel
    #:alignment '(left top)
    #:stretch '(#t #f)
    #:margin '(0 25)
    content)))

(define (label-pict label state w h)
  (define label-str
    (string-titlecase (symbol->string label)))
  (define-values (fg-color bg-color)
    (case state
      [(hover) (values primary-color hover-background-color)]
      [(selected) (values selection-primary-color selection-background-color)]
      [else (values primary-color white)]))
  (p:lc-superimpose
   (p:filled-rectangle
    #:color bg-color
    #:border-width 1
    #:border-color bg-color
    w h)
   (p:inset
    (p:colorize
     (p:text label-str system-font-m)
     fg-color)
    8 0)))

(module+ main
  (require db
           franz/metadata)
  (current-connection
   (sqlite3-connect
    #:database 'memory))
  (migrate!)
  (render
   (preferences-window
    (@ 'license))))
