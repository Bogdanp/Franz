#lang racket/gui/easy

(require franz/iterator
         (submod franz/workspace rpc)
         racket/date
         racket/fixnum
         racket/match
         racket/vector
         "observable.rkt"
         "preference.rkt"
         "thread.rkt")

(provide
 records-table)

(define (records-table id topic [call-with-status-proc (λ (proc) (proc void))])
  (define max-size (* 1 1024 1024))
  (define max-buffer (* 2 1024 1024))
  (define-observables
    [@records (vector)]
    [@live? #t]
    [@fetching? #f])
  (define it
    (call-with-status-proc
     (lambda (status)
       (status "Opening iterator...")
       (open-iterator topic (IteratorOffset.recent 20) id))))
  (define (fetch)
    (call-with-status-proc
     (lambda (status)
       (status "Fetching records...")
       (@fetching?:= #t)
       (define new-records
         (get-records it max-size))
       (unless (null? new-records)
         (update-observable [old-records @records]
           (define ress (vector-append old-records (list->vector new-records)))
           (vector-sort! ress IteratorResult>)
           (define last-idx
             (for/fold ([total 0] [idx 0] #:result idx)
                       ([(res idx) (in-indexed (in-vector ress))])
               (define next-total (+ (IteratorResult-size res) total))
               #:break (> next-total max-buffer)
               (values next-total idx)))
           (if (< last-idx (sub1 (vector-length ress)))
               (vector-take ress last-idx)
               ress)))
       (@fetching?:= #f))))
  (define fetch-thd
    (thread*
     (with-handlers ([exn:break? void])
       (let loop ()
         (when ^@live? (fetch))
         (sleep (get-preference 'general:reload-interval 5))
         (loop)))))
  (add-hooks
   #:on-destroy
   (lambda ()
     (break-thread fetch-thd)
     (close-iterator it))
   (vpanel
    (table
     '("Partition" "Offset" "Timestamp" "Key" "Value")
     #:column-widths
     '((0 80)
       (1 80)
       (2 150)
       (3 150)
       (4 300))
     @records
     #:entry->row
     (lambda (res)
       (define r (IteratorResult->record res))
       (define k (IteratorRecord-key r))
       (define v (IteratorRecord-value r))
       (vector
        (number->string (IteratorRecord-partition-id r))
        (number->string (IteratorRecord-offset r))
        (~timestamp (quotient (IteratorRecord-timestamp r) 1000))
        (~data k)
        (~data v)))
     void)
    (hpanel
     #:stretch '(#t #f)
     (text
      (let-observable ([records @records])
        (format "Records: ~a (~a)"
                (vector-length records)
                (~size records))))
     (spacer)
     (button
      (let-observable ([live? @live?])
        (if live?
            "Stop streaming"
            "Start streaming"))
      (λ () (@live? . <~ . not)))
     (button
      #:enabled?
      (let-observable ([live? @live?]
                       [fetching? @fetching?])
        (not (or live? fetching?)))
      "Load more..."
      (λ () (thread fetch)))))))

(define (IteratorResult->record res)
  (match res
    [(IteratorResult.original record) record]
    [(IteratorResult.transformed _original record) record]))

(define (IteratorResult-size res)
  (define r (IteratorResult->record res))
  (define k (IteratorRecord-key r))
  (define v (IteratorRecord-value r))
  (+ (if k (bytes-length k) 0)
     (if v (bytes-length v) 0)
     (for/sum ([(k v) (in-hash (IteratorRecord-headers r))])
       (+ (if k (string-length k) 0)
          (if v (bytes-length v) 0)))))

(define (IteratorResult> a b)
  (let ([a (IteratorResult->record a)]
        [b (IteratorResult->record b)])
    (if (or (eqv? (IteratorRecord-partition-id a)
                  (IteratorRecord-partition-id b))
            (eqv? (IteratorRecord-timestamp a)
                  (IteratorRecord-timestamp b)))
        (> (IteratorRecord-offset a)
           (IteratorRecord-offset b))
        (> (IteratorRecord-timestamp a)
           (IteratorRecord-timestamp b)))))

(define (get-size ress)
  (for/sum ([res (in-vector ress)])
    (IteratorResult-size res)))

(define (~size ress)
  (format "~aMiB" (quotient (quotient (get-size ress) 1024) 1024)))

(define (~data bs)
  (cond
    [(not bs)
     "NULL"]
    [else
     (let ([bs (bytes-take bs 30)])
       (if (text? bs)
           (bytes->string/utf-8 bs)
           "BINARY DATA"))]))

(define (~timestamp s)
  (date->string (seconds->date s #t) #t))

(define (bytes-take bs n)
  (if (> (bytes-length bs) n)
      (subbytes bs 0 n)
      bs))

(define (text? bs)
  (for/fold ([ok? #t]
             [cont 0]
             #:result (and ok? (zero? cont)))
            ([b (in-bytes bs)])
    #:break (not ok?)
    (cond
      [(fx= b 0) (values #f 0)]
      [(fx= (fxand b #b11110000) #b11110000) (values #t 3)]
      [(fx= (fxand b #b11100000) #b11100000) (values #t 2)]
      [(fx= (fxand b #b11000000) #b11000000) (values #t 1)]
      [(fx= (fxand b #b10000000) #b10000000) (values (> cont 0) (sub1 cont))]
      [else (values (zero? cont) 0)])))

(module+ main
  (require "testing.rkt")
  (call-with-testing-context
   (lambda (id)
     (render
      (window
       #:size '(800 600)
       (records-table id "example-topic"))))))
