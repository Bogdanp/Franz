#lang info

(define collection "franz")
(define version "1.1")
(define deps '(["amazon-msk-sasl-lib" #:version "0.2"]
               ["avro-lib" #:version "1.1"]
               "base"
               "buid"
               ["confluent-schema-registry-lib" #:version "0.2.1"]
               "db-lib"
               ["deta-lib" #:version "0.11"]
               "http-easy-lib"
               ["json-lexer-lib" #:version "0.1.1"]
               ["kafka-lib" #:version "0.7"]
               "libsqlite3"
               ["lua-lib" #:version "0.1.2"]
               "messagepack-lib"
               ["noise-serde-lib" #:version "0.4"]
               "protocol-buffers-lib"
               ["sasl-lib" #:version "1.3"]
               "threading-lib"))
(define build-deps '("rackunit-lib"))
(define compile-omit-paths '("fixtures"))
