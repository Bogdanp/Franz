#lang racket/base

(require ffi/unsafe)

(provide
 os-version)

(define-cstruct _osversioninfo
  ([size _long]
   [major _long]
   [minor _long]
   [build _long]
   [platform _long]
   [pack (_array/vector _byte 128)]))

(define GetVersionExA
  (get-ffi-obj "GetVersionExA" #f (_fun _pointer -> _bool)))

(define (os-version)
  (define info (make-osversioninfo (ctype-sizeof _osversioninfo) 0 0 0 0 (make-vector 128 0)))
  (unless (GetVersionExA info)
    (error 'GetVersionExA "failed"))
  (format "Windows Version ~a.~a (Build ~a)"
          (osversioninfo-major info)
          (osversioninfo-minor info)
          (osversioninfo-build info)))
