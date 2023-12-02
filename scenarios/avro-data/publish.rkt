#lang racket/base

(require avro
         kafka
         kafka/producer
         racket/port
         racket/random)

(define schema #<<EOF
  {
    "type": "record",
    "name": "Person",
    "fields": [
      {
        "name": "Name",
        "type": "string"
      },
      {
        "name": "Age",
        "type": "int"
      }
    ]
  }
EOF
  )

(define codec (make-codec schema))
(define people '("Bogdan" "Jim" "Bob" "Alex"))
(define ages '(30 25 12 18 47 52 80))

(define c (make-client))
(define p (make-producer c))
(create-topics c (make-CreateTopic
                  #:name "people"
                  #:partitions 2))
(with-handlers ([exn:break? void])
  (let loop ()
    (for ([_ (in-range 1000)])
      (produce p "people" #f (call-with-output-bytes
                              (lambda (out)
                                (define person
                                  (hasheq
                                   'Name (random-ref people)
                                   'Age (random-ref ages)))
                                (codec-write codec person out)))))
    (sleep 1)
    (loop)))
(disconnect-all c)
