#lang info

(define collection "franz")
(define version "1.0")
(define deps '(["amazon-msk-sasl-lib" #:version "0.2"]
               ["avro-lib" #:version "1.1"]
               "base"
               "confluent-schema-registry-lib"
               "db-lib"
               ["deta-lib" #:version "0.11"]
               "json-lexer-lib"
               ["kafka-lib" #:version "0.4"]
               "libsqlite3"
               "lua-lib"
               "messagepack-lib"
               ["noise-serde-lib" #:version "0.2"]
               ["sasl-lib" #:version "1.3"]
               "threading-lib"))
(define build-deps '("rackunit-lib"))
(define compile-omit-paths '("fixtures"))
