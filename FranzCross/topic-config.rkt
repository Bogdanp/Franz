#lang racket/gui/easy

(require franz/connection-details
         racket/match
         "common.rkt"
         "observable.rkt"
         "preference.rkt"
         "view.rkt")


;; config ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(provide
 (struct-out topic-config)
 get-topic-config
 get-topic-config*
 put-topic-config
 put-topic-config*
 topic-config-buffer-bytes
 topic-config-request-bytes)

(struct topic-config
  (key-format
   val-format
   sort-direction
   request-size
   buffer-size)
  #:prefab)

(define (make-default-config)
  (topic-config 'binary 'binary 'desc 1 2))

(define (get-topic-config conn topic)
  (get-preference
   `(topic-config ,(ConnectionDetails-id conn) ,topic)
   (lambda ()
     (get-topic-config* conn))))

(define (get-topic-config* conn)
  (get-preference
   `(topic-config ,(ConnectionDetails-id conn))
   make-default-config))

(define (put-topic-config conn topic conf)
  (put-preference `(topic-config ,(ConnectionDetails-id conn) ,topic) conf))

(define (put-topic-config* conn conf)
  (put-preference `(topic-config ,(ConnectionDetails-id conn)) conf))

(define (topic-config-buffer-bytes c)
  (* (topic-config-buffer-size c) 1024 1024))

(define (topic-config-request-bytes c)
  (* (topic-config-request-size c) 1024 1024))


;; form ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(provide
 topic-config-form)

(define (topic-config-form conf [action void])
  (match-define (topic-config key-format val-format sort-direction request-size buffer-size)
    conf)
  (define-observables
    [@key-format key-format]
    [@val-format val-format]
    [@sort-direction sort-direction]
    [@request-size request-size]
    [@buffer-size buffer-size])
  (vpanel
   #:margin '(10 10)
   (labeled
    "Key Format:"
    (format-choices @key-format @key-format:=))
   (labeled
    "Value Format:"
    (format-choices @val-format @val-format:=))
   (labeled
    "Sort:"
    (choice
     '(descending ascending)
     #:choice->label (compose1 string-titlecase symbol->string)
     #:selection @sort-direction
     @sort-direction:=))
   (labeled
    "Request Size:"
    #:alignment '(right top)
    (size-slider
     @request-size
     @request-size:=))
   (labeled
    "Buffer Size:"
    #:alignment '(right top)
    (size-slider
     #:steps 12
     @buffer-size
     @buffer-size:=))
   (labeled
    ""
    (button
     "Save"
     #:style '(border)
     (lambda ()
       (define conf
         (topic-config
          ^@key-format
          ^@val-format
          ^@sort-direction
          ^@request-size
          ^@buffer-size))
       (action conf))))))

(define (format-choices @fmt [action void])
  (choice
   '(binary json text)
   #:choice->label ~format
   #:selection @fmt
   action))

(define (size-slider @size [action void] #:steps [steps 10])
  (vpanel
   (slider
    (let-observable ([size @size])
      (add1 (inexact->exact (round (log size 2)))))
    (lambda (n)
      (action (expt 2 (sub1 n))))
    #:min-value 1
    #:max-value steps
    #:style '(horizontal plain))
   (hpanel
    #:stretch '(#t #f)
    #:alignment '(right center)
    (text
     #:font system-font-s
     (let-observable ([size @size])
       (format "~a MB" size))))))

(define (~format v)
  (case v
    [(binary) "Binary"]
    [(json) "JSON"]
    [(text) "Text"]
    [else (raise-argument-error '~format "(or/c 'binary 'json 'text)" v)]))

(module+ main
  (require "testing.rkt")
  (call-with-testing-context
   (lambda (_id)
     (render
      (dialog
       (topic-config-form
        (make-default-config)))))))
