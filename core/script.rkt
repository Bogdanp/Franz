#lang racket/base

(require (for-syntax racket/base)
         avro
         lua/env
         lua/value
         noise/backend
         noise/serde
         racket/file
         racket/port
         racket/runtime-path
         "pool.rkt")

(define-rpc (get-script [for-topic topic : String]
                        [in-workspace id : UVarint] : String)
  default-script)

(define-rpc (activate [script : String]
                      [for-topic topic : String]
                      [in-workspace id : UVarint])
  (define script-res
    (lua-eval script))
  (unless (pair? script-res)
    (error 'activate "the script must return a value"))
  (define script-ob
    (car script-res))
  (unless (table? script-ob)
    (error 'activate "the script return value must be a table"))
  (pool-activate-script id topic script-ob))

(define-rpc (deactivate-script [for-topic topic : String]
                               [in-workspace id : UVarint])
  (pool-deactivate-script id topic))


;; help ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define default-script
  #<<SCRIPT
local script = {}

-- Returning `false` or `nil` from this function removes the `record`
-- from the result set.  Modifications you make to `record` in this
-- function will be visible in the data table.
--
-- See Help -> Manual... to learn more.
function script.transform(record)
  local key = record.key
  local value = record.value
  if record.partition_id ~= 0 then
    return nil
  end
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
         (dynamic-require path '#%chunk))))
    (lambda ()
      (delete-file path))))


;; extlib ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-runtime-module-path-index avro.lua "extlib/avro.lua")

(define symbol->bytes
  (compose1 string->bytes/utf-8 symbol->string))

(define null-hash
  (hasheq
   'type "null"
   'value 'null))

(define (avro->lua v)
  (cond
    [(list? v)
     (define t (make-table))
     (begin0 t
       (for ([(v idx) (in-indexed (in-list v))])
         (table-set! t (add1 idx) (avro->lua v))))]
    [(hash? v)
     (cond
       [(equal? v null-hash) nil]
       [else
        (define t (make-table))
        (begin0 t
          (for ([(k v) (in-hash v)])
            (table-set! t (symbol->bytes k) (avro->lua v))))])]
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
                  (table-set! env #"#%avro-tolua" avro->lua)
                  (define mod (car (dynamic-require avro.lua '#%chunk)))
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
  (check-true (car (eval-fixture "avro-basics.lua"))))
