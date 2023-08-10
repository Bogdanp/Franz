#lang racket/base

(require "keychain/filesystem.rkt"
         "keychain/generic.rkt")

(provide
 current-keychain
 (all-from-out "keychain/filesystem.rkt"
               "keychain/generic.rkt"))

(define current-keychain
  (make-parameter #f))
