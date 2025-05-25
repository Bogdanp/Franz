#lang info

(define collection "franz")
(define version "1.7")
(define deps '(["amazon-msk-sasl-lib" #:version "0.3"]
               ["avro-lib" #:version "1.1"]
               "base"
               "buid-lib"
               ["confluent-schema-registry-lib" #:version "0.2.3"]
               "db-lib"
               ["deta-lib" #:version "0.11"]
               "http-easy-lib"
               ["json-lexer-lib" #:version "0.1.1"]
               ["kafka-lib" #:version "0.12.1"]
               ["libsqlite3" #:version "3.43.1"]
               "libzstd"
               ["lua-lib" #:version "0.4.1"]
               "messagepack-lib"
               ["noise-serde-lib" #:version "0.7"]
               ["protocol-buffers-lib" #:version "0.1.2"]
               ["sasl-lib" #:version "1.3"]
               "snappy-lib"
               "threading-lib"))
(define build-deps '("rackunit-lib"))
(define compile-omit-paths '("fixtures"))
