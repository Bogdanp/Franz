#lang racket/gui/easy

(require franz/broker
         (submod franz/workspace rpc)
         racket/format
         "common.rkt"
         "config-table.rkt"
         "info-view.rkt"
         "observable.rkt"
         "thread.rkt"
         "view.rkt"
         (prefix-in m: "window-manager.rkt"))

(provide
 broker-detail)

(define (broker-detail id b [call-with-status-proc void])
  (define-observables
    [@tab 'information]
    [@config null])
  (define (reload-config)
    (thread*
     (call-with-status-proc
      (lambda (status)
        (status "Fetching Configs")
        (@config:= (get-resource-configs (~a (Broker-id b)) 'broker id))))))
  (vpanel
   #:alignment '(left top)
   #:margin '(10 10)
   (vpanel
    #:alignment '(left top)
    #:stretch '(#t #f)
    (text
     #:font system-font-l
     (~truncate (Broker-host b)))
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
        #:get-parent-proc
        (lambda ()
          (m:get-workspace-renderer id))
        #:update-action
        (lambda (configs)
          (update-resource-configs
           (for/hash ([c (in-list configs)])
             (values
              (ResourceConfig-name c)
              (ResourceConfig-value c)))
           (number->string (Broker-id b))
           'broker
           id)
          (reload-config))
        #:reset-action reload-config
        @config)]))))

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
