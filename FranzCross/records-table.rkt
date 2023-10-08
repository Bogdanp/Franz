#lang racket/gui/easy

(require franz/connection-details
         franz/iterator
         (submod franz/script rpc)
         (submod franz/workspace rpc)
         racket/class
         racket/date
         racket/fixnum
         racket/list
         racket/match
         racket/vector
         "combinator.rkt"
         "common.rkt"
         "mixin.rkt"
         "observable.rkt"
         "preference.rkt"
         "record-detail.rkt"
         "thread.rkt"
         "topic-config.rkt"
         "validator.rkt"
         "view.rkt"
         (prefix-in m: "window-manager.rkt"))

(provide
 records-table)

(define (records-table id topic
                       [call-with-status-proc (λ (proc) (proc void))]
                       #:get-details-proc [get-details (λ () (m:get-workspace-details id))]
                       #:get-parent-proc [get-parent (λ () (m:get-workspace-renderer id))])
  (define conn (get-details))
  (define conn-id (ConnectionDetails-id conn))
  (define-observables
    [@config (get-topic-config conn topic)]
    [@records (vector)]
    [@live? #t]
    [@fetching? #f])
  (define/obs @buttons-enabled?
    (let-observable ([live? @live?]
                     [fetching? @fetching?])
      (not (or live? fetching?))))
  (define it
    (call-with-status-proc
     (lambda (status)
       (status "Opening iterator...")
       (open-iterator topic (IteratorOffset.recent 20) id))))
  (define (fetch [replace? #f])
    (define config ^@config)
    (define max-rows (* 10 1000))
    (define max-size (topic-config-request-bytes config))
    (define max-buffer (topic-config-buffer-bytes config))
    (define sort-direction (topic-config-sort-direction config))
    (call-with-status-proc
     (lambda (status)
       (status "Fetching records...")
       (@fetching?:= #t)
       (define new-records
         (get-records it max-size))
       (define have-new?
         (not (null? new-records)))
       (cond
         [have-new?
          (update-observable [old-records @records]
            (define ress
              (if replace?
                  (list->vector new-records)
                  (vector-append old-records (list->vector new-records))))
            (case sort-direction
              [(ascending)
               (vector-sort! ress IteratorResult<)
               (define last-idx ;; from the end
                 (for/fold ([total 0] [idx 0] [n 0] #:result idx)
                           ([idx (in-range (sub1 (vector-length ress)) 0 -1)])
                   (define res (vector-ref ress idx))
                   (define next-total (+ (IteratorResult-size res) total))
                   #:break (or (= n max-rows)
                               (> next-total max-buffer))
                   (values next-total idx (add1 n))))
               (if (> last-idx 0)
                   (vector-drop ress (add1 last-idx))
                   ress)]
              [else
               (vector-sort! ress IteratorResult>)
               (define last-idx
                 (for/fold ([total 0] [idx 0] #:result idx)
                           ([(res idx) (in-indexed (in-vector ress))])
                   (define next-total (+ (IteratorResult-size res) total))
                   #:break (or (= idx max-rows)
                               (> next-total max-buffer))
                   (values next-total idx)))
               (if (< last-idx (sub1 (vector-length ress)))
                   (vector-take ress last-idx)
                   ress)]))]
         [replace?
          (@records:= (vector))]
         [else
          (void)])
       (begin0 have-new?
         (@fetching?:= #f)))))
  (define fetch-thd
    (thread*
     (with-handlers ([exn:break? void])
       (let loop ([misses 0])
         (define fetched-new-records?
           (if ^@live? (fetch) #f))
         (sleep
          (min (* 0.05 (expt 2 misses))
               (get-preference 'general:reload-interval 5)))
         (loop (if fetched-new-records? 0 (add1 misses)))))))
  (define (get-column-widths*)
    (get-preference
     `(records-table ,conn-id ,topic column-widths)
     '((0 60)
       (1 70)
       (2 150)
       (3 100)
       (4 300))))
  (define (do-apply-script script)
    (update-observable [ress @records]
      (define originals
        (for/list ([res (in-vector ress)])
          (IteratorResult->original res)))
      (list->vector
       (apply-script script originals id))))
  (add-hooks
   #:on-create
   (lambda ()
     (m:enable-scripting-window id topic do-apply-script))
   #:on-destroy
   (lambda ()
     (m:disable-scripting-window id topic)
     (call-with-status-proc
      (lambda (status)
        (status "Closing iterator...")
        (break-thread fetch-thd)
        (close-iterator it)
        (put-preference
         `(records-table ,conn-id ,topic column-widths)
         (get-column-widths*)))))
   (vpanel
    (table
     '("Partition" "Offset" "Timestamp" "Key" "Value")
     #:column-widths
     (get-column-widths*)
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
     #:mixin
     (λ (%)
       (class %
         (inherit get-column-width)
         (super-new)
         (set! get-column-widths*
               (lambda ()
                 (for/list ([i (in-range 5)])
                   (define-values (width _min-width _max-width)
                     (get-column-width i))
                   (list i width))))))
     (lambda (event entries selection)
       (case event
         [(dclick)
          (when selection
            (define r
              (IteratorResult->record (vector-ref entries selection)))
            (render
             (record-detail-window
              #:key-format (topic-config-key-format ^@config)
              #:val-format (topic-config-val-format ^@config)
              r topic)))])))
    (hpanel
     #:stretch '(#t #f)
     (text
      (let-observable ([records @records])
        (format "Records: ~a (~a)"
                (vector-length records)
                (~size (get-size records)))))
     (spacer)
     (button
      code-bmp
      (lambda ()
        (m:open-scripting-window id topic do-apply-script)))
     (spacer)
     (observable-view
      @live?
      (lambda (live?)
        (button
         (if live? pause-bmp play-bmp)
         (lambda ()
           (unless live?
             (@records:= (vector)))
           (@live?:= (not live?))))))
     (button
      gear-bmp
      #:enabled? @buttons-enabled?
      (lambda ()
        (define close! void)
        (render
         (dialog
          #:title (format "~a Preferences" topic)
          #:mixin (mix-close-window void (λ (close!-proc)
                                           (set! close! close!-proc)))
          (topic-config-form
           ^@config
           (lambda (conf)
             (put-topic-config conn topic conf)
             (@config:= conf)
             (close!))))
         (get-parent))))
     (button
      viewfinder-ellipsis-bmp
      #:enabled? @buttons-enabled?
      (lambda ()
        (define close! void)
        (define-observables
          [@target 'recent]
          [@offset 0]
          [@recent-n 20]
          [@timestamp (current-seconds)])
        (define labeled*
          (make-keyword-procedure
           (lambda (kws kw-args . args)
             (keyword-apply labeled #:width 80 kws kw-args args))))
        (render
         (dialog
          #:title "Jump..."
          #:mixin (mix-close-window void (λ (close!-proc)
                                           (set! close! close!-proc)))
          (vpanel
           #:margin '(10 10)
           #:stretch '(#t #f)
           (labeled*
            "Target:"
            (choice
             '(earliest timestamp recent latest offset)
             #:choice->label (compose1 string-titlecase symbol->string)
             #:selection @target
             @target:=))
           (match-view @target
             ['earliest (spacer)]
             ['recent
              (labeled*
               "Delta:"
               (validated-input
                #:text->value positive-number
                (@recent-n . ~> . number->string)
                (drop1 @recent-n:=)))]
             ['timestamp
              (labeled*
               "Timestamp:"
               (validated-input
                #:text->value parse-timestamp
                (@timestamp . ~> . ~timestamp)
                (drop1 @timestamp:=)))]
             ['latest (spacer)]
             ['offset
              (labeled*
               "Offset:"
               (validated-input
                #:text->value nonnegative-number
                (@offset . ~> . number->string)
                (drop1 @offset:=)))])
           (labeled*
            ""
            (button
             "Jump"
             #:style '(border)
             (lambda ()
               (thread*
                (call-with-status-proc
                 (lambda (status)
                   (status "Resetting iterator...")
                   (reset-iterator
                    it (case ^@target
                         [(earliest) (IteratorOffset.earliest)]
                         [(timestamp) (IteratorOffset.timestamp (* ^@timestamp 1000))]
                         [(recent) (IteratorOffset.recent ^@recent-n)]
                         [(latest) (IteratorOffset.latest)]
                         [(offset) (IteratorOffset.exact ^@offset)]
                         [else (raise-argument-error 'reset-iterator "target/c" ^@target)]))))
                (fetch #t))
               (close!))))))
         (get-parent))))
     (button
      chevron-e-bmp
      #:enabled? @buttons-enabled?
      (λ () (thread fetch)))))))

(define (IteratorResult->original res)
  (match res
    [(IteratorResult.original record) record]
    [(IteratorResult.transformed _record original) original]))

(define (IteratorResult->record res)
  (match res
    [(IteratorResult.original record) record]
    [(IteratorResult.transformed record _original) record]))

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
  (IteratorResult-cmp a b >))

(define (IteratorResult< a b)
  (IteratorResult-cmp a b <))

(define (IteratorResult-cmp a b n>m)
  (let ([a (IteratorResult->record a)]
        [b (IteratorResult->record b)])
    (if (or (eqv? (IteratorRecord-partition-id a)
                  (IteratorRecord-partition-id b))
            (eqv? (IteratorRecord-timestamp a)
                  (IteratorRecord-timestamp b)))
        (n>m (IteratorRecord-offset a)
             (IteratorRecord-offset b))
        (n>m (IteratorRecord-timestamp a)
             (IteratorRecord-timestamp b)))))

(define (get-size ress)
  (for/sum ([res (in-vector ress)])
    (IteratorResult-size res)))

(define (~size n)
  (let loop ([n n]
             [s '("B" "KiB" "MiB" "GiB" "TiB" "PiB" "EiB" "ZiB" "YiB")])
    (if (< n 1024)
        (format "~a~a" n (car s))
        (loop (quotient n 1024) (cdr s)))))

(define (~data bs)
  (cond
    [(not bs)
     "NULL"]
    [else
     (let ([bs (bytes-take bs 30)])
       (if (text? bs)
           (bytes->string/utf-8 bs)
           "BINARY DATA"))]))

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

(define (parse-timestamp s)
  (define parts
    (or (regexp-match #rx"(....)-(..)-(..)[T ](..):(..):(..)" s) null))
  (match (filter-map string->number parts)
    [(list y M d h m s)
     (with-handlers ([exn:fail? (λ (_e) #f)])
       (find-seconds s m h d M y #t))]
    [_ #f]))

(module+ main
  (require "testing.rkt")
  (call-with-testing-context
   (lambda (id)
     (define r
       (render
        (window
         #:size '(800 600)
         (records-table
          id "example-topic"
          #:get-details-proc
          (λ ()
            (make-ConnectionDetails
             #:id 1
             #:name "Example"))
          #:get-parent-proc
          (λ () r)))))
     r)))
