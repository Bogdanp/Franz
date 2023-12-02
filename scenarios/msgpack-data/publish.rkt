#lang racket/base

(require kafka
         kafka/producer
         messagepack
         racket/port)

(define c (make-client))
(define p (make-producer c))
(create-topics c (make-CreateTopic
                  #:name "msgpack"
                  #:partitions 1))
(with-handlers ([exn:break? void])
  (let loop ()
    (for ([_ (in-range 1000)])
      (produce p "msgpack" #f (call-with-output-bytes
                               (lambda (out)
                                 (write-msgpack
                                  (hash
                                   "int" -65
                                   "list" (list msgpack-nil #t #f 1 2.5 -5)
                                   "string" "hello!"
                                   #"bytes" #"bytes")
                                  out)))))
    (sleep 1)
    (loop)))
(disconnect-all c)
