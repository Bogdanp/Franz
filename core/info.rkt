#lang info

(define collection "franz")
(define version "1.0")
(define deps '("amazon-msk-sasl-lib"
               "avro-lib"
               "base"
               "db-lib"
               "deta-lib"
               "kafka-lib"
               "lua-lib"
               "noise-serde-lib"
               ["sasl-lib" #:version "1.2"]
               "threading-lib"))
(define build-deps '("rackunit-lib"))
(define compile-omit-paths '("fixtures"))
