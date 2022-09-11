#lang racket/base

(require db
         noise/backend
         noise/serde)

(provide
 main)

(define-rpc (hello : String)
  "Hello, world!")

(define (main in-fd out-fd)
  (module-cache-clear!)
  (collect-garbage)
  (define stop
    (serve in-fd out-fd))
  (with-handlers ([exn:break? (λ (_) (stop))])
    (sync never-evt)))
