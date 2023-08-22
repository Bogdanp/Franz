#lang racket/gui/easy

(require franz/schema-registry/schema
         racket/format
         racket/match
         "common.rkt"
         "editor.rkt"
         "info-view.rkt"
         "observable.rkt"
         "view.rkt")

(provide
 schema-detail)

(define (schema-detail _id s)
  (define-observables
    [@tab 'information])
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
       (infos
        `(("Type" . ,(~Schema-type s))
          ("Latest Version" . ,(~a (Schema-version s)))))]
      ['source
       (editor
        #:lang (match (Schema-type s)
                 [(? SchemaType.avro?) 'json]
                 [(? SchemaType.json?) 'json]
                 [(? SchemaType.protobuf?) 'protobuf]
                 [_ 'json])
        (Schema-schema s))]))))

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
