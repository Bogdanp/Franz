#lang info

(define collection "FranzCross")
(define version "1.0")
(define deps '("base"
               "buid"
               "canvas-list"
               ["franz" #:version "1.1"]
               ["gui-easy-lib" #:version "0.13"]
               "gui-lib"
               "http-easy-lib"
               "pict-lib"
               "threading-lib"))
(define build-deps '("rackunit-lib"))
