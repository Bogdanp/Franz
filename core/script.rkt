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

function script.filter(record)
  return true
end

function script.map(record)
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
  (define path (make-temporary-file))
  (call-with-output-file path
    #:exists 'truncate/replace
    (lambda (out)
      (define in (make-#lang-lua-port str))
      (copy-port in out)))
  (dynamic-require path '#%chunk))
