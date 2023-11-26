#lang racket/gui/easy

(require racket/port
         racket/runtime-path
         "combinator.rkt"
         "editor.rkt"
         "mixin.rkt"
         "observable.rkt"
         "view.rkt")

(provide
 create-schema-dialog)

(define-runtime-path avro-example.json "assets/schema/avro-example.json")
(define-runtime-path json-example.json "assets/schema/json-example.json")
(define-runtime-path protobuf-example.proto "assets/schema/protobuf-example.proto")

(define (get-example type)
  (call-with-input-file
    (case type
      [(avro) avro-example.json]
      [(json) json-example.json]
      [(protobuf) protobuf-example.proto])
    port->string))

(define (create-schema-dialog #:create-action [create void]
                              #:cancel-action [cancel void])
  (define close! void)
  (define-observables
    [@name ""]
    [@type 'avro]
    [@init-schema (get-example 'avro)]
    [@schema ""])
  (define/obs @lang
    (let-observable ([type @type])
      (case type
        [(avro json) 'json]
        [(protobuf) 'protobuf])))
  (dialog
   #:title "Create Schema"
   #:mixin (mix-set!-closer close!)
   (vpanel
    #:margin '(10 10)
    #:stretch '(#t #f)
    (labeled
     "Name:"
     (input @name (drop1 @name:=)))
    (labeled
     "Schema Type:"
     (choice
      '(avro json protobuf)
      #:choice->label (compose1 string-titlecase symbol->string)
      #:selection @type
      (lambda (type)
        (define schema ^@schema)
        (when (or (string=? schema "")
                  (string=? schema (get-example ^@type)))
          (@init-schema:= (get-example type)))
        (@type:= type))))
    (labeled
     "Schema:"
     #:alignment '(right top)
     (hpanel
      #:min-size '(600 400)
      (editor
       #:lang @lang
       @init-schema
       @schema:=)))
    (hpanel
     #:alignment '(right center)
     (button
      "Cancel"
      (lambda ()
        (close!)
        (cancel)))
     (button
      "Create"
      #:style '(border)
      #:enabled? (let-observable ([name @name])
                   (not (string=? name "")))
      (lambda ()
        (create ^@name ^@type ^@schema)
        (close!)))))))

(module+ main
  (render
   (create-schema-dialog)))
