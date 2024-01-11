#lang info

(define version "1.6")
(define build-number 2)

(define collection "FranzCross")
(define deps '("base"
               "buid-lib"
               "canvas-list"
               "db-lib"
               ["franz" #:version "1.2"]
               ["gui-easy-lib" #:version "0.15"]
               "gui-lib"
               "http-easy-lib"
               "json-lexer-lib"
               "net-lib"
               "pict-lib"
               "plot-lib"
               "plot-gui-lib"
               "threading-lib"))
(define build-deps '("rackunit-lib"))
