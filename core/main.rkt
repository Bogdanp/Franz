#lang racket/base

(require noise/backend
         noise/serde
         racket/contract
         racket/string)

(provide
 main)

(define-record ConnectionDetails
  [(id #f) : (Optional UVarint)]
  [name : String #:contract non-empty-string?]
  [(bootstrap-host "127.0.0.1") : String #:contract non-empty-string?]
  [(bootstrap-port 9092) : UVarint #:contract (integer-in 0 65535)]
  [(username #f) : (Optional String) #:contract (or/c #f string?)]
  [(password #f) : (Optional String) #:contract (or/c #f string?)]
  [(use-ssl #f) : Bool #:contract boolean?])

(define-rpc (hello : String)
  "Hello, world!")

(define (main in-fd out-fd)
  (module-cache-clear!)
  (collect-garbage)
  (define stop
    (serve in-fd out-fd))
  (with-handlers ([exn:break? (λ (_) (stop))])
    (sync never-evt)))
