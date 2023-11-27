#lang racket/gui/easy

(require (submod franz/schema-registry rpc)
         franz/schema-registry/schema
         json/pretty
         racket/format
         racket/match
         racket/port
         "alert.rkt"
         "common.rkt"
         "editor.rkt"
         "info-view.rkt"
         "observable.rkt"
         "thread.rkt"
         "view.rkt"
         "window-manager.rkt")

(provide
 schema-detail)

(define (schema-detail id s [call-with-status-proc void])
  (define-observables
    [@tab 'information]
    [@schema s])
  (define (fetch-schema)
    (thread*
     (call-with-status-proc
      (lambda (status)
        (status "Fetching Schema")
        (@schema:= (get-schema (Schema-name s) id))))))
  (fetch-schema)
  (vpanel
   #:alignment '(left top)
   #:margin '(10 10)
   (hpanel
    #:stretch '(#t #f)
    (vpanel
     #:alignment '(left top)
     #:stretch '(#t #f)
     (text
      #:font system-font-l
      (~truncate (Schema-name s)))
     (text
      #:color secondary-color
      #:font system-font-xs
      "Schema")))
   (tabs
    '(information source)
    #:choice->label (compose1 string-titlecase symbol->string)
    (lambda (event _choices choice)
      (case event
        [(select)
         (@tab:= choice)]))
    (match-view @tab
      ['information
       (observable-view
        @schema
        (lambda (schema)
          (vpanel
           (infos
            `(("Type" . ,(~Schema-type schema))
              ("Latest Version" . ,(~a (Schema-version schema))))))))]
      ['source
       (define/obs @lang
         (let-observable ([s @schema])
           (match (Schema-type s)
             [(? SchemaType.avro?) 'json]
             [(? SchemaType.json?) 'json]
             [(? SchemaType.protobuf?) 'protobuf]
             [_ 'json])))
       (define/obs @init-schema
         (let-observable ([s @schema]
                          [l @lang])
           (define code
             (or (Schema-schema s) ""))
           (case l
             [(json) (call-with-output-string
                      (lambda (out)
                        (call-with-input-string code
                          (lambda (in)
                            (pretty-print-json in out)))))]
             [else code])))
       (define-observables
         [@update-schema (obs-peek @init-schema)])
       (vpanel
        (editor
         #:lang @lang
         @init-schema
         @update-schema:=)
        (hpanel
         #:stretch '(#t #f)
         #:alignment '(right center)
         (button "Reset" fetch-schema)
         (button
          "Check Compatibility"
          (lambda ()
            (call-with-status-proc
             (lambda (status)
               (status "Checking Schema Compatibility")
               (define ok? (check-schema (Schema-name s) ^@update-schema id))
               (unless ok?
                 (alert
                  "Check Schema"
                  "This schema is incompatible with the latest version."
                  #:renderer (get-workspace-renderer id)))))))
         (button
          "Update Schema"
          (lambda ()
            (let ([s ^@schema])
              (call-with-status-proc
               (lambda (status)
                 (status "Updating Schema")
                 (create-schema
                  (Schema-name s)
                  (match (Schema-type s)
                    [(SchemaType.avro) 'avro]
                    [(SchemaType.json) 'json]
                    [(SchemaType.protobuf) 'protobuf])
                  ^@update-schema
                  id)))
              (fetch-schema))))))]))))

(define (~Schema-type s)
  (match (Schema-type s)
    [(? SchemaType.avro?) "AVRO"]
    [(? SchemaType.json?) "JSON"]
    [(? SchemaType.protobuf?) "Protocol Buffers"]
    [_ "Unknown"]))

(module+ main
  (render
   (window
    #:size '(800 600)
    (schema-detail
     1
     (make-Schema
      #:id 1
      #:name "Example Schema"
      #:type (SchemaType.avro)
      #:version 1
      #:schema "{}")))))
