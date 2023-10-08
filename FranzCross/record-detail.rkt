#lang racket/gui/easy

(require franz/iterator
         (submod franz/lexer rpc)
         racket/format
         racket/list
         "common.rkt"
         "editor.rkt"
         "observable.rkt"
         "view.rkt")

(provide
 record-detail-window)

(define (record-detail-window r topic
                              #:key-format [key-format 'binary]
                              #:val-format [val-format 'binary])
  (define-observables
    [@tab 'value])
  (window
   #:size '(600 400)
   #:title (format "~a: Record ~a@~a"
                   topic
                   (IteratorRecord-offset r)
                   (IteratorRecord-partition-id r))
   (menu-bar
    (menu
     "&File"
     (menu-item
      "Save &as..."
      (lambda ()
        (define-values (what ext bs)
          (case (obs-peek @tab)
            [(key) (values "key" "dat" (or (IteratorRecord-key r) #""))]
            [(value) (values "value" "dat" (or (IteratorRecord-value r) #""))]
            [else (values #f #f #f)]))
        (when what
          (define path
            (gui:put-file
             (format "Save ~a" what)
             #f ;parent
             #f ;directory
             #f ;filename
             ext ;extension
             null ;style
             null ;filters
             ))
          (when path
            (call-with-output-file path
              #:exists 'truncate/replace
              (lambda (out)
                (write-bytes bs out)))))))))
   (tabs
    '(metadata key value headers)
    #:selection @tab
    #:choice->label (compose1 string-titlecase symbol->string)
    (lambda (event _choices selection)
      (case event
        [(select) (@tab:= selection)]))
    #:alignment '(left top)
    (match-view @tab
      ['metadata
       (vpanel
        #:stretch '(#t #f)
        #:alignment '(left top)
        (labeled "Partition ID:" (text (~a (IteratorRecord-partition-id r))))
        (labeled "Offset:" (text (~a (IteratorRecord-offset r))))
        (labeled "Timestamp:" (text (~timestamp (quotient (IteratorRecord-timestamp r) 1000)))))]
      ['key
       (data
        #:format key-format
        (or (IteratorRecord-key r) #""))]
      ['value
       (data
        #:format val-format
        (or (IteratorRecord-value r) #""))]
      ['headers
       (table
        '("Header" "Value")
        (for/vector ([(k v) (in-hash (IteratorRecord-headers r))])
          (vector k (if v (bytes->string/utf-8 v #\uFFFD) ""))))]))))

(define (data bs #:format [fmt 'binary])
  (define-observables
    [@fmt fmt]
    [@data bs])
  (vpanel
   #:alignment '(left top)
   (match-view @fmt
     ['binary (hex-table @data)]
     [lang (editor
            #:lang lang
            (let-observable ([bs @data])
              (bytes->string/utf-8 bs #\uFFFD)))])
   (hpanel
    #:stretch '(#t #f)
    (choice
     '(binary json text)
     #:selection fmt
     #:choice->label
     (lambda (choice)
       (case choice
         [(binary) "Binary"]
         [(json) "JSON"]
         [(text) "Text"]))
     @fmt:=)
    (spacer)
    (button
     "Format"
     #:enabled?
     (let-observable ([fmt @fmt])
       (eq? fmt 'json))
     (lambda ()
       (update-observable [bs @data]
         (string->bytes/utf-8
          (pp-json (bytes->string/utf-8 bs #\uFFFD)))))))))

(define (hex-table @data)
  (observable-view
   @data
   (lambda (bs)
     (define n (bytes-length bs))
     (define (~byte pos)
       (if (< pos n)
           (~hex (bytes-ref bs pos))
           ""))
     (table
      (make-list 9 "")
      #:font system-mono-font-s
      #:style '(single)
      #:column-widths
      (cons '(0 90) (for/list ([i (in-range 1 8)]) `(,i 35)))
      (for/vector ([addr (in-range 0 (bytes-length bs) 8)])
        (vector
         (~hex addr 8)
         (~byte (+ addr 0))
         (~byte (+ addr 1))
         (~byte (+ addr 2))
         (~byte (+ addr 3))
         (~byte (+ addr 4))
         (~byte (+ addr 5))
         (~byte (+ addr 6))
         (~byte (+ addr 7))))))))

(define (~hex n [min-width 2])
  (~a "0x" (~r n #:base 16 #:min-width min-width #:pad-string "0")))

(module+ main
  (require "testing.rkt")
  (call-with-testing-context
   (lambda (_id)
     (define r
       (make-IteratorRecord
        #:partition-id 1
        #:offset 12
        #:timestamp (current-milliseconds)
        #:key #"{\"a\": 42, \"b\": [{}, {\"c\":\"d\"}]}"
        #:value #"world"
        #:headers (hash)))
     (render
      (record-detail-window r "example-topic")))))
