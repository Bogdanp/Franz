#lang racket/gui/easy

(require browser/external
         franz/broker
         "clipboard.rkt"
         "combinator.rkt"
         "hacks.rkt"
         "mixin.rkt"
         "observable.rkt"
         "view.rkt")

(provide
 config-table)

(define (config-table @configs
                      #:get-parent-proc [get-parent-renderer void]
                      #:update-action [update-action void])
  (define-observables
    [@revealed-names (hash)]
    [@selection #f])
  (define (edit entry)
    (render
     (edit-dialog
      entry
      #:save-action
      (lambda (updated-c)
        (update-observable [configs @configs]
          (for/list ([c (in-list configs)])
            (if (equal?
                 (ResourceConfig-name c)
                 (ResourceConfig-name updated-c))
                updated-c
                c)))
        (update-action updated-c)))
     (get-parent-renderer)))
  (table
   '("Name" "Value" "Default?")
   #:column-widths
   '((0 180)
     (1 180)
     (2 80))
   #:entry->row
   (λ (c)
     (vector
      (ResourceConfig-name c)
      (or (ResourceConfig-value c) "")
      (if (ResourceConfig-is-default c)
          "yes"
          "no")))
   (let-observable ([configs @configs]
                    [revealed @revealed-names])
     (for/vector ([c (in-list configs)])
       (if (and (ResourceConfig-is-sensitive c)
                (not (hash-has-key? revealed (ResourceConfig-name c))))
           (set-ResourceConfig-value c "********")
           c)))
   #:mixin
   (mix-context-event
    (lambda (event)
      (define entry ^@selection)
      (when entry
        (define name (ResourceConfig-name entry))
        (render-popup-menu*
         (get-parent-renderer)
         (apply
          popup-menu
          (menu-item "Copy Key" (λ () (put-clipboard name)))
          (menu-item "Copy Value" (λ () (put-clipboard (ResourceConfig-value entry))))
          (append
           (let ([doc-url (ResourceConfig-doc-url entry)])
             (if doc-url
                 (list
                  (menu-item
                   "Open Docs..."
                   (lambda ()
                     (send-url doc-url))))
                 null))
           (if (ResourceConfig-is-read-only entry)
               null
               (list
                (menu-item-separator)
                (menu-item "Edit..." (λ () (edit entry)))))
           (if (ResourceConfig-is-sensitive entry)
               (list
                (menu-item-separator)
                (if (hash-has-key? ^@revealed-names name)
                    (menu-item
                     "Hide"
                     (λpdate-observable @revealed-names
                       (hash-remove it name)))
                    (menu-item
                     "Reveal"
                     (λpdate-observable @revealed-names
                       (hash-set it name #t)))))
               null)))
         event))))
   (λ (event entries selection)
     (case event
       [(select)
        (@selection:= (and selection (vector-ref entries selection)))]
       [(dclick)
        (when selection
          (edit (vector-ref entries selection)))]))))

(define (edit-dialog c
                     #:save-action [on-save void]
                     #:cancel-action [on-cancel void])
  (define close! void)
  (define labeled* (make-labeled 50))
  (define-observables
    [@v (ResourceConfig-value c)])
  (dialog
   #:title (format "Edit ~a" (ResourceConfig-name c))
   #:mixin (mix-close-window void (λ (close!-proc)
                                    (set! close! close!-proc)))
   (vpanel
    #:margin '(10 10)
    (labeled*
     "Value:"
     (input
      (@v . ~> . ~optional-str)
      (drop1 (compose1 @v:= ->optional-str))))
    (labeled*
     ""
     (hpanel
      (button
       "Save"
       #:style '(border)
       (lambda ()
         (close!)
         (on-save
          (set-ResourceConfig-value c ^@v))))
      (button
       "Cancel"
       (lambda ()
         (close!)
         (on-cancel))))))))

(module+ main
  (define root
    (render
     (window
      #:size '(800 600)
      (config-table
       #:get-parent-proc (λ () root)
       (@ (list
           (make-ResourceConfig
            #:name "cleanup.policy"
            #:value "compact"
            #:is-read-only #f
            #:is-default #f
            #:is-sensitive #f
            #:doc-url "https://kafka.apache.org/documentation/#brokerconfigs_log.cleanup.policy")
           (make-ResourceConfig
            #:name "password"
            #:value "hunter2"
            #:is-read-only #f
            #:is-default #f
            #:is-sensitive #t)
           (make-ResourceConfig
            #:name "host"
            #:value "kafka-1"
            #:is-read-only #t
            #:is-default #t
            #:is-sensitive #f))))))))
