#lang racket/gui/easy

(require (submod franz/schema-registry rpc)
         franz/schema-registry/schema
         json/pretty
         racket/format
         racket/match
         racket/port
         "common.rkt"
         "editor.rkt"
         "info-view.rkt"
         "observable.rkt"
         "thread.rkt"
         "view.rkt")

(provide
 schema-detail)

(define (schema-detail id s [call-with-status-proc void])
  (define-observables
    [@tab 'information]
    [@schema s])
  (thread*
   (call-with-status-proc
    (lambda (status)
      (status "Fetching Schema")
      (@schema:= (get-schema (Schema-name s) id)))))
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
      (Schema-name s))
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
          (infos
           `(("Type" . ,(~Schema-type schema))
             ("Latest Version" . ,(~a (Schema-version schema)))))))]
      ['source
       (define/obs @lang
         (let-observable ([s @schema])
           (match (Schema-type s)
             [(? SchemaType.avro?) 'json]
             [(? SchemaType.json?) 'json]
             [(? SchemaType.protobuf?) 'protobuf]
             [_ 'json])))
       (editor
        #:lang @lang
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
            [else code])))]))))

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
