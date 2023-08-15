#lang racket/gui/easy

(require franz/broker
         (submod franz/workspace rpc)
         racket/format
         racket/gui/easy/font
         "common.rkt"
         "config-table.rkt"
         "observable.rkt"
         "view.rkt"
         (prefix-in m: "window-manager.rkt"))

(provide
 broker-detail)

(define (broker-detail id b [make-status-proc (λ () void)])
  (define-observables
    [@tab 'information]
    [@config null])
  (define (reload-config)
    ((make-status-proc) "Fetching Configs")
    (thread
     (lambda ()
       (define status (make-status-proc))
       (@config:= (get-resource-configs (~a (Broker-id b)) 'broker id))
       (status "Ready"))))
  (vpanel
   #:alignment '(left top)
   #:margin '(10 10)
   (vpanel
    #:alignment '(left top)
    #:stretch '(#t #f)
    (text
     #:font system-font-l
     (Broker-host b))
    (text
     #:color secondary-color
     #:font system-font-xs
     "Broker"))
   (hpanel
    #:stretch '(#t #f)
    #:min-size '(#f 10))
   (tabs
    '(information config)
    #:choice->label (compose1 string-titlecase symbol->string)
    (λ (event _choices selection)
      (case event
        [(select)
         (@tab:= selection)]))
    (match-view @tab
      ['information
       (infos
        `(("Address"    . ,(~a (Broker-host b) ":" (Broker-port b)))
          ("Node ID"    . ,(~a (Broker-id b)))
          ("Controller" . ,(if (Broker-is-controller b) "yes" "no"))))]
      ['config
       (reload-config)
       (config-table
        #:get-parent-proc (λ () (m:get-workspace-renderer id))
        @config)]))))

(define (infos is)
  (apply
   vpanel
   #:alignment '(left top)
   #:margin '(10 10)
   (for/list ([i (in-list is)])
     (hpanel
      #:stretch '(#t #f)
      (hpanel
       #:min-size '(100 #f)
       #:stretch '(#f #t)
       (text
        #:font system-font-s
        (car i)))
      (text
       #:font (font #:weight 'bold system-font font-size-s)
       (cdr i))))))

(module+ main
  (render
   (window
    #:size '(800 600)
    (broker-detail
     1
     (make-Broker
      #:id 1
      #:host "kafka-1"
      #:port 9092
      #:is-controller #t)))))
