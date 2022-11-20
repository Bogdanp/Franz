#lang info

(define collection "franz")
(define version "1.0")
(define deps '("base"
               "db-lib"
               "deta-lib"
               "kafka-lib"
               "lua-lib"
               "noise-serde-lib"
               "sasl-lib"
               "threading-lib"))
(define build-deps '("rackunit-lib"))
