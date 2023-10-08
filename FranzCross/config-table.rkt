#lang racket/gui/easy

(require franz/broker
         "clipboard.rkt"
         "hacks.rkt"
         "mixin.rkt"
         "observable.rkt")

(provide
 config-table)

(define (config-table @configs
                      #:get-parent-proc [get-parent-renderer void])
  (define-observables
    [@revealed-names (hash)]
    [@selection #f])
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
              null))
         event))))
   (λ (event entries selection)
     (case event
       [(select)
        (@selection:= (and selection (vector-ref entries selection)))]))))

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
            #:is-sensitive #f)
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
