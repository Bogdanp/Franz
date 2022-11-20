#lang racket/base

(require lua/value
         noise/backend
         noise/serde
         racket/file
         racket/port
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
      (dynamic-require path '#%chunk))
    (lambda ()
      (delete-file path))))
