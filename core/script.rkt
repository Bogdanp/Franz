#lang racket/base

(require (for-syntax racket/base
                     syntax/parse/pre)
         avro
         kafka/private/serde/internal
         lua/env
         lua/value
         messagepack
         noise/backend
         noise/serde
         racket/contract/base
         racket/date
         racket/file
         racket/match
         racket/port
         racket/runtime-path
         "iterator.rkt"
         "pool.rkt"
         "record.rkt")

;; results ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(provide
 (record-out ApplyResult)
 (record-out Chart)
 (record-out ChartPair)
 (record-out Stack)
 (record-out TableRow)
 (enum-out ChartScale)
 (enum-out ChartScaleType)
 (enum-out ChartStyle)
 (enum-out ChartValue)
 (enum-out ReduceResult))

(define-record TableRow
  [columns : (Listof String)])

(define-enum ChartScaleType
  [linear]
  [log])

(define-enum ChartScale
  [numerical
   {lo : Float64}
   {hi : Float64}
   {type : ChartScaleType}])

(define-enum ChartStyle
  [area]
  [bar]
  [candlestick
   {width : (Optional UVarint)}]
  [line]
  [scatter])

(define-enum ChartValue
  [candlestick
   {o : Float64}
   {h : Float64}
   {l : Float64}
   {c : Float64}]
  [categorical {s : String}]
  [numerical {n : Float64}]
  [timestamp {t : UVarint}])

(define-record ChartPair
  [x : ChartValue]
  [y : ChartValue])

(define-record Chart
  [style : ChartStyle]
  [pairs : (Listof ChartPair)]
  [x-scale : (Optional ChartScale)]
  [x-label : String]
  [y-scale : (Optional ChartScale)]
  [y-label : String])

(define-enum ReduceResult
  [chart {c : Chart}]
  [number {n : Float64}]
  [stack {s : (Delay Stack)}]
  [table
   {cols : (Listof String)}
   {rows : (Listof TableRow)}]
  [text {s : String}])

(define-record Stack
  [direction : Symbol #:contract (or/c 'horizontal 'vertical)]
  [children : (Listof ReduceResult)])

(define-record ApplyResult
  [items : (Listof IteratorResult)]
  [(output #"") : Bytes]
  [(reduced #f) : (Optional ReduceResult)])

(define (->ChartScale v)
  (cond
    [(nil? v) #f]
    [(table? v)
     (ChartScale.numerical
      (table-ref v #"lo")
      (table-ref v #"hi")
      (case (table-ref v #"typ")
        [(#"linear") (ChartScaleType.linear)]
        [(#"log") (ChartScaleType.log)]
        [else (error '->ChartScale "invalid scale type")]))]
    [else (error '->ChartScale "invalid scale value: ~s" v)]))

(define (->ChartStyle type v)
  (case type
    [(#"AreaChart")
     (ChartStyle.area)]
    [(#"BarChart")
     (ChartStyle.bar)]
    [(#"CandlestickChart")
     (ChartStyle.candlestick
      (let ([width (table-ref v #"candlestick_width")])
        (if (nil? width) #f (inexact->exact width))))]
    [(#"LineChart")
     (ChartStyle.line)]
    [(#"ScatterChart")
     (ChartStyle.scatter)]
    [else (error '->ReduceResult "invalid chart type: ~s" type)]))

(define (->ChartValue v)
  (cond
    [(bytes? v)
     (ChartValue.categorical (~lua v))]
    [(number? v)
     (ChartValue.numerical v)]
    [(table? v)
     (define type
       (table-ref v #"__type"))
     (case type
       [(#"Candlestick")
        (ChartValue.candlestick
         (table-ref-number v #"o")
         (table-ref-number v #"h")
         (table-ref-number v #"l")
         (table-ref-number v #"c"))]
       [(#"Timestamp")
        (ChartValue.timestamp
         (inexact->exact
          (table-ref-number v #"ts")))]
       [else
        (error '->ChartValue "invalid table: ~s" v)])]
    [else (error '->ChartValue "invalid value: ~s" v)]))

(define (->ChartPair t)
  (make-ChartPair
   #:x (->ChartValue (table-ref t #"x"))
   #:y (->ChartValue (table-ref t #"y"))))

(define (->ReduceResult v)
  (cond
    [(not v) #f]
    [(nil? v) #f]
    [(bytes? v) (ReduceResult.text (~lua v))]
    [(number? v) (ReduceResult.number v)]
    [(table? v)
     (define type
       (table-ref v #"__type"))
     (case type
       [(#"AreaChart" #"BarChart" #"CandlestickChart" #"LineChart" #"ScatterChart")
        (ReduceResult.chart
         (make-Chart
          #:style (->ChartStyle type v)
          #:pairs (for/list ([p (in-table (table-ref v #"values"))])
                    (->ChartPair p))
          #:x-scale (->ChartScale (table-ref v #"xscale"))
          #:x-label (~lua (table-ref v #"xlabel"))
          #:y-scale (->ChartScale (table-ref v #"yscale"))
          #:y-label (~lua (table-ref v #"ylabel"))))]
       [(#"HStack" #"VStack")
        (ReduceResult.stack
         (make-Stack
          #:direction
          (case type
            [(#"HStack") 'horizontal]
            [(#"VStack") 'vertical]
            [else (error '->ReduceResult "invalid stack type: ~s" type)])
          #:children (for/list ([r (in-table (table-ref v #"children"))])
                       (->ReduceResult r))))]
       [(#"Table")
        (ReduceResult.table
         (for/list ([col-bs (in-table (table-ref v #"columns"))])
           (~lua col-bs))
         (for/list ([row (in-table (table-ref v #"rows"))])
           (TableRow
            (for/list ([v (in-table row)])
              (~lua v)))))]
       [else (error '->ReduceResult "invalid table: ~s" v)])]
    [else (error '->ReduceResult "unexpected result: ~s" v)]))

(define (table-ref-number t k)
  (define v (table-ref t k))
  (begin0 v
    (unless (number? v)
      (error 'table-ref-number "expected a number, but found: ~s" v))))


;; scripts ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-rpc (get-script [for-topic topic : String]
                        [in-workspace id : UVarint] : String)
  (case topic
    [("__consumer_offsets") consumer-offsets-script]
    [else default-script]))

(define-rpc (activate-script [_ script : String]
                             [for-topic topic : String]
                             [in-workspace id : UVarint])
  (pool-activate-script id topic (make-script 'activate-script script)))

(define-rpc (apply-script [_ script : String]
                          [to-records records : (Listof IteratorRecord)]
                          [in-workspace id : UVarint] : ApplyResult)
  (define output
    (open-output-bytes))
  (define-values (result items)
    (parameterize ([current-output-port output]
                   [current-error-port output])
      (do-apply-script script records)))
  (ApplyResult
   (reverse items)
   (get-output-bytes output)
   (->ReduceResult result)))

(define (do-apply-script script records)
  (define script-tbl
    (make-script 'apply-script script))
  (define transform-proc (table-ref script-tbl #"transform"))
  (unless (procedure? transform-proc)
    (error 'apply-script "script.transform is not a procedure"))
  (define reduce-proc (table-ref script-tbl #"reduce"))
  (unless (or (nil? reduce-proc) (procedure? reduce-proc))
    (error 'apply-script "script.reduce is not a procedure"))
  (define render-proc (table-ref script-tbl #"render"))
  (unless (or (nil? render-proc) (procedure? render-proc))
    (error 'apply-script "script.render is not a procedure"))
  (define (transform r)
    (define table
      (transform-proc (IteratorRecord->table r)))
    (cond
      [(table? table) table]
      [(nil? table) nil]
      [else (error 'apply-script "script.transform must return a table or nil~n  received: ~s" table)]))
  (define (reduce r s)
    (cond
      [(nil? reduce-proc) s]
      [else (reduce-proc r s)]))
  (define (render s)
    (cond
      [(nil? render-proc) s]
      [else (render-proc s)]))
  (define-values (state items)
    (for*/fold ([s nil] [items null])
               ([r (in-list records)]
                [t (in-value (transform r))]
                #:unless (nil? t))
      (define item
        (IteratorResult.transformed (table->IteratorRecord t) r))
      (values
       (reduce t s)
       (cons item items))))
  (values (render state) items))

(define-rpc (deactivate-script [for-topic topic : String]
                               [in-workspace id : UVarint])
  (pool-deactivate-script id topic))


;; help ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (make-script who script)
  (define script-res
    (lua-eval script))
  (unless (pair? script-res)
    (error who "the script must return a value"))
  (define script-ob
    (car script-res))
  (unless (table? script-ob)
    (error who "the script return value must be a table"))
  script-ob)

(define default-script
  #<<SCRIPT
local script = {}

-- Returning `false` or `nil` from this function removes the `record`
-- from the result set.  Modifications you make to `record` in this
-- function will be visible in the data table.
--
-- See Help -> Manual... to learn more.
function script.transform(record)
  return record
end

-- Use this function to aggregate records. The initial value of the
-- `state` argument is `nil`.
function script.reduce(record, state)
  return (state or 0) + 1
end

-- Use this function to render the final state of `script.reduce`.
-- Returning `nil` from this function avoids rendering anything.
function script.render(state)
  return nil
end

return script
SCRIPT
  )

(define consumer-offsets-script
  #<<SCRIPT
local script = {}

function script.transform(record)
  local event_type, data = kafka.parse_committed_offset(record)
  if not event_type then
    return record
  end
  record.key = event_type
  record.value = tostring(data)
  return record
end

return script
SCRIPT
  )

(define (make-#lang-lua-port str)
  (define-values (in out)
    (make-pipe #f "script.lua"))
  (begin0 in
    (thread
     (lambda ()
       (displayln "#lang lua" out)
       (display str out)
       (close-output-port out)))))

(define (lua-eval str)
  (define dir #f)
  (define path #f)
  (dynamic-wind
    (lambda ()
      (set! dir (make-temporary-directory "~a-franz-scripts"))
      (set! path (build-path dir "script.lua")))
    (lambda ()
      (call-with-output-file path
        #:exists 'truncate/replace
        (lambda (out)
          (define in (make-#lang-lua-port str))
          (copy-port in out)))
      (call-with-lua-extlib
       (lambda ()
         (define modpath `(file ,(path->string path)))
         (dynamic-require `(submod ,modpath configure-runtime) #f void)
         (dynamic-require modpath '#%chunk))))
    (lambda ()
      (delete-directory/files dir)
      (module-cache-clear!))))


;; extlib ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define nanos/second
  (* 1 1000 1000 1000))

(define (date->isostring d)
  (parameterize ([date-display-format 'iso-8601])
    (string->bytes/utf-8 (date->string d #t))))

(define (isostring->date s)
  (with-handlers ([(λ (e)
                     (or (regexp-match? #rx"match-define: no matching clause" (exn-message e))
                         (regexp-match? #rx"find-seconds: non-existent date" (exn-message e))))
                   (λ (_) #f)])
    (match-define
      (regexp #px"^(....)-(..)-(..)[ T](..):(..):(..)(\\.([0-9]{1,9}))?(Z|([-+]..:..))?$"
              (list _
                    (app string->number year)
                    (app string->number month)
                    (app string->number day)
                    (app string->number hour)
                    (app string->number minute)
                    (app string->number second)
                    _ ;; nanosecond with prefix
                    nanosecond-str
                    timezone-str
                    _ ;; +- offset
                    ))
      (bytes->string/utf-8 s))
    (define nanosecond
      (let ([n (or (and nanosecond-str (string->number nanosecond-str)) 0)])
        (if (zero? n)
            0
            (inexact->exact
             (* (/ n (expt 10 (add1 (floor (log n 10))))) nanos/second)))))
    (define utc-seconds
      (find-seconds second minute hour day month year #f))
    (define utc-date
      (struct-copy date*
                   (seconds->date utc-seconds #f)
                   [nanosecond nanosecond]))
    (match timezone-str
      [(or "" "Z" "+00:00" "-00:00") utc-date]
      [(regexp #rx"([-+])([0-9]+):([0-9]+)"
               (list _
                     sign-str
                     (app string->number hours)
                     (app string->number minutes)))
       (define offset
         (let ([offset (+ (* hours 3600)
                          (* minutes 60))])
           (case sign-str
             [("-")    offset]
             [("+") (- offset)])))
       (seconds->date (+ (date*->seconds utc-date #f) offset) #f)])))

(define-syntax (defmod stx)
  (syntax-parse stx
    [(_ name:id)
     #:with path (datum->syntax #'name (format "extlib/~a" (syntax->datum #'name)))
     #'(define-runtime-module-path-index name path)]))

(define-syntax-rule (defmods name ...)
  (begin (defmod name) ...))

(defmods
  avro.lua
  class.lua
  kafka.lua
  msgpack.lua
  render.lua
  timestamp.lua)

(define symbol->bytes
  (compose1 string->bytes/utf-8 symbol->string))

(define avro-null-hash
  (hasheq
   'type "null"
   'value 'null))

(define (->lua v)
  (cond
    [(msgpack-nil? v)
     nil]
    [(list? v)
     (define t (make-table))
     (begin0 t
       (for ([(v idx) (in-indexed (in-list v))])
         (table-set! t (add1 idx) (->lua v))))]
    [(hash? v)
     (cond
       [(equal? v avro-null-hash) nil]
       [else
        (define t (make-table))
        (begin0 t
          (for ([(k v) (in-hash v)])
            (table-set! t (->lua k) (->lua v))))])]
    [(string? v)
     (string->bytes/utf-8 v)]
    [(symbol? v)
     (symbol->bytes v)]
    [else
     v]))

(define (make-class-module mod mod-id class-name [binds null])
  (cons mod-id (λ (env _name)
                 (for ([bind (in-list binds)])
                   (apply table-set! env bind))
                 (define the-class (car (dynamic-require mod '#%chunk)))
                 (table-set! env class-name the-class))))

(define (make-table-module mod mod-id [binds null])
  (cons mod-id (λ (env name)
                 (for ([bind (in-list binds)])
                   (apply table-set! env bind))
                 (define table (car (dynamic-require mod '#%chunk)))
                 (table-set! env name table))))

(define (make-extlib-modules)
  (list
   (make-class-module class.lua #"class" #"Class")
   (make-class-module
    timestamp.lua #"timestamp" #"Timestamp"
    `((#"#%date->isostring" ,date->isostring)
      (#"#%isostring->date" ,isostring->date)))
   (make-table-module
    avro.lua #"avro"
    `((#"#%avro-make-codec" ,make-codec)
      (#"#%avro-codec-read" ,codec-read)
      (#"#%avro-tolua" ,->lua)))
   (make-table-module
    kafka.lua #"kafka"
    `((#"#%parse-Internal" ,parse-Internal)
      (#"#%InternalOffsetCommit?" ,InternalOffsetCommit?)
      (#"#%InternalOffsetCommit-group" ,InternalOffsetCommit-group)
      (#"#%InternalOffsetCommit-topic" ,InternalOffsetCommit-topic)
      (#"#%InternalOffsetCommit-partition-id" ,InternalOffsetCommit-partition-id)
      (#"#%InternalOffsetCommit-offset" ,InternalOffsetCommit-offset)
      (#"#%InternalGroupMetadata?" ,InternalGroupMetadata?)
      (#"#%InternalGroupMetadata-group" ,InternalGroupMetadata-group)
      (#"#%InternalGroupMetadata-generation" ,InternalGroupMetadata-generation)
      (#"#%InternalGroupMetadata-protocol-type" ,InternalGroupMetadata-protocol-type)
      (#"#%InternalGroupMetadata-protocol-data" ,InternalGroupMetadata-protocol-data)
      (#"#%InternalGroupMetadata-leader" ,InternalGroupMetadata-leader)
      (#"#%InternalGroupMetadata-members" ,InternalGroupMetadata-members)
      (#"#%InternalGroupMember-id" ,InternalGroupMember-id)
      (#"#%InternalGroupMember-client-id" ,InternalGroupMember-client-id)
      (#"#%InternalGroupMember-client-host" ,InternalGroupMember-client-host)
      (#"#%InternalGroupMember-rebalance-timeout" ,InternalGroupMember-rebalance-timeout)
      (#"#%InternalGroupMember-session-timeout" ,InternalGroupMember-session-timeout)
      (#"#%InternalGroupMember-subscription" ,InternalGroupMember-subscription)
      (#"#%InternalGroupMember-assignment" ,InternalGroupMember-assignment)))
   (make-table-module
    msgpack.lua #"msgpack"
    `((#"#%msgpack-read" ,read-msgpack)
      (#"#%msgpack-tolua" ,->lua)))
   (make-table-module render.lua #"render")))

(define (call-with-lua-extlib proc)
  (define mods
    (append (current-standard-library-modules)
            (make-extlib-modules)))
  (parameterize ([current-standard-library-modules mods])
    (proc)))

(module+ test
  (require rackunit)

  (define-runtime-path fixtures
    "fixtures")

  (define (eval-fixture name)
    (call-with-input-file (build-path fixtures name)
      (lambda (in)
        (lua-eval (port->string in)))))

  (check-equal? (lua-eval "return 42") '(42))
  (check-true (car (eval-fixture "avro-basics.lua")))
  (check-true (car (eval-fixture "kafka-basics.lua")))
  (check-true (car (eval-fixture "msgpack-basics.lua")))
  (check-true (car (eval-fixture "render-basics.lua")))
  (check-true (car (eval-fixture "timestamp.lua"))))
