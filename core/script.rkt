#lang racket/base

(require avro
         kafka/private/serde/internal
         lua/env
         lua/value
         messagepack
         noise/backend
         noise/serde
         racket/file
         racket/port
         racket/runtime-path
         "iterator.rkt"
         "pool.rkt"
         "record.rkt")

;; results ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(provide
 (record-out ApplyResult)
 (enum-out ReduceResult))

(define-enum ReduceResult
  [text {s : String}]
  [number {n : Float64}]
  [lineChart
   {xlabel : String}
   {xs : (Listof Float64)}
   {ylabel : String}
   {ys : (Listof Float64)}])

(define-record ApplyResult
  [items : (Listof IteratorResult)]
  [(reduced #f) : (Optional ReduceResult)])

(define (->ReduceResult v)
  (cond
    [(not v) #f]
    [(nil? v) #f]
    [(bytes? v) (ReduceResult.text (bytes->string/utf-8 v #\uFFFD))]
    [(number? v) (ReduceResult.number v)]
    [(table? v)
     (define type
       (table-ref v #"__type"))
     (case type
       [(equal? #"LineChart")
        (define xs (table-ref v #"xs"))
        (define ys (table-ref v #"ys"))
        (ReduceResult.lineChart
         (bytes->string/utf-8 (table-ref v #"xlabel"))
         (for/list ([i (in-range 1 (add1 (table-length xs)))])
           (table-ref xs i))
         (bytes->string/utf-8 (table-ref v #"ylabel"))
         (for/list ([i (in-range 1 (add1 (table-length ys)))])
           (table-ref ys i)))]
       [else (error '->ReduceResult "invalid table: ~s" v)])]
    [else (error '->ReduceResult "unexpected result: ~s" v)]))


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
  (define script-tbl (make-script 'apply-script script))
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
  (ApplyResult
   (reverse items)
   (->ReduceResult
    (render state))))

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
  (define path #f)
  (dynamic-wind
    (lambda ()
      (set! path (make-temporary-file "franz-~a.lua")))
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
      (delete-file path))))


;; extlib ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-runtime-module-path-index avro.lua "extlib/avro.lua")
(define-runtime-module-path-index class.lua "extlib/class.lua")
(define-runtime-module-path-index kafka.lua "extlib/kafka.lua")
(define-runtime-module-path-index msgpack.lua "extlib/msgpack.lua")
(define-runtime-module-path-index render.lua "extlib/render.lua")

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

(define (make-extlib-modules)
  `((#"avro" . ,(λ (env name)
                  (table-set! env #"#%avro-make-codec" make-codec)
                  (table-set! env #"#%avro-codec-read" codec-read)
                  (table-set! env #"#%avro-tolua" ->lua)
                  (define mod (car (dynamic-require avro.lua '#%chunk)))
                  (table-set! env name mod)))
    (#"class" . ,(λ (env _name)
                   (define Class (car (dynamic-require class.lua '#%chunk)))
                   (table-set! env #"Class" Class)))
    (#"kafka" . ,(λ (env name)
                   (table-set! env #"#%parse-Internal" parse-Internal)
                   (table-set! env #"#%InternalOffsetCommit?" InternalOffsetCommit?)
                   (table-set! env #"#%InternalOffsetCommit-group" InternalOffsetCommit-group)
                   (table-set! env #"#%InternalOffsetCommit-topic" InternalOffsetCommit-topic)
                   (table-set! env #"#%InternalOffsetCommit-partition-id" InternalOffsetCommit-partition-id)
                   (table-set! env #"#%InternalOffsetCommit-offset" InternalOffsetCommit-offset)
                   (table-set! env #"#%InternalGroupMetadata?" InternalGroupMetadata?)
                   (table-set! env #"#%InternalGroupMetadata-group" InternalGroupMetadata-group)
                   (table-set! env #"#%InternalGroupMetadata-generation" InternalGroupMetadata-generation)
                   (table-set! env #"#%InternalGroupMetadata-protocol-type" InternalGroupMetadata-protocol-type)
                   (table-set! env #"#%InternalGroupMetadata-protocol-data" InternalGroupMetadata-protocol-data)
                   (table-set! env #"#%InternalGroupMetadata-leader" InternalGroupMetadata-leader)
                   (table-set! env #"#%InternalGroupMetadata-members" InternalGroupMetadata-members)
                   (table-set! env #"#%InternalGroupMember-id" InternalGroupMember-id)
                   (table-set! env #"#%InternalGroupMember-client-id" InternalGroupMember-client-id)
                   (table-set! env #"#%InternalGroupMember-client-host" InternalGroupMember-client-host)
                   (table-set! env #"#%InternalGroupMember-rebalance-timeout" InternalGroupMember-rebalance-timeout)
                   (table-set! env #"#%InternalGroupMember-session-timeout" InternalGroupMember-session-timeout)
                   (table-set! env #"#%InternalGroupMember-subscription" InternalGroupMember-subscription)
                   (table-set! env #"#%InternalGroupMember-assignment" InternalGroupMember-assignment)
                   (define mod (car (dynamic-require kafka.lua '#%chunk)))
                   (table-set! env name mod)))
    (#"msgpack" . ,(λ (env name)
                     (table-set! env #"#%msgpack-read" read-msgpack)
                     (table-set! env #"#%msgpack-tolua" ->lua)
                     (define mod (car (dynamic-require msgpack.lua '#%chunk)))
                     (table-set! env name mod)))
    (#"render" . ,(λ (env name)
                    (define mod (car (dynamic-require render.lua '#%chunk)))
                    (table-set! env name mod)))))

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
  (check-true (car (eval-fixture "msgpack-basics.lua"))))
