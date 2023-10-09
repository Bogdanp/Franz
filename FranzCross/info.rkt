#lang info

(define collection "FranzCross")
(define deps '("base"
               "buid"
               "canvas-list"
               ["franz" #:version "1.2"]
               ["gui-easy-lib" #:version "0.15"]
               "gui-lib"
               "http-easy-lib"
               "json-lexer-lib"
               "pict-lib"
               "threading-lib"))
(define build-deps '("rackunit-lib"))
