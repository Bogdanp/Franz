#lang racket/base

(require kafka
         racket/string)

(create-topics
 (make-client)
 (make-CreateTopic
  #:name (make-string 128 #\a)
  #:partitions 2)
 (make-CreateTopic
  #:name (string-join
          (for/list ([_ (in-range 10)])
            "abc")
          "-")
  #:partitions 3)
 (make-CreateTopic
  #:name (string-join
          (for/list ([_ (in-range 10)])
            "BCd")
          "-")
  #:partitions 128))
