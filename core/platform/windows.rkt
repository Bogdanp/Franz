#lang racket/base

(require ffi/unsafe)

(provide
 os-version)

(define-cstruct _osversioninfoexa
  ([size        _long]
   [major       _long]
   [minor       _long]
   [build       _long]
   [platform    _long]
   [pack        (_array/vector _byte 128)]
   [packMajor   _word]
   [packMinor   _word]
   [suiteMask   _word]
   [productType _byte]
   [reserved    _byte]))

(define GetVersionExA
  (get-ffi-obj "GetVersionExA" #f (_fun _pointer -> _bool)))

(define (os-version)
  (define info
    (make-osversioninfoexa (ctype-sizeof _osversioninfoexa) 0 0 0 0 (make-vector 128 0) 0 0 0 0 0))
  (unless (GetVersionExA info)
    (error 'GetVersionExA "failed"))
  (format "Windows Version ~a.~a (Build ~a)"
          (osversioninfoexa-major info)
          (osversioninfoexa-minor info)
          (osversioninfoexa-build info)))
