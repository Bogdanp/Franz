#lang racket/gui/easy

(require franz/iterator
         racket/format
         "common.rkt"
         "editor.rkt"
         "observable.rkt"
         "view.rkt")

(provide
 record-detail-window)

(define (record-detail-window r topic)
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
      ['key (data (or (IteratorRecord-key r) #""))]
      ['value (data (or (IteratorRecord-value r) #""))]
      ['headers (table
                 '("Header" "Value")
                 (for/vector ([(k v) (in-hash (IteratorRecord-headers r))])
                   (vector k (if v (bytes->string/utf-8 v #\uFFFD) ""))))]))))

(define (data bs)
  (define-observables
    [@fmt 'binary])
  (vpanel
   #:alignment '(left top)
   (match-view @fmt
     ['binary (hex-table bs)]
     [lang (editor #:lang lang (bytes->string/utf-8 bs #\uFFFD))])
   (choice
    '(binary json text)
    #:choice->label
    (lambda (choice)
      (case choice
        [(binary) "Binary"]
        [(json) "JSON"]
        [(text) "Text"]))
    @fmt:=)))

(define (hex-table bs)
  (define n (bytes-length bs))
  (define (~byte pos)
    (if (< pos n)
        (~hex (bytes-ref bs pos))
        ""))
  (table
   '("Addr" "c0" "c1" "c2" "c3" "c4" "c5" "c6" "c7")
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
      (~byte (+ addr 7))))))

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
        #:key #"hello, cruel world!"
        #:value #"world"
        #:headers (hash)))
     (render
      (record-detail-window r "example-topic")))))
