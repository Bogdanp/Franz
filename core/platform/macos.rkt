#lang racket/base

(require ffi/unsafe/nsstring
         ffi/unsafe/objc)

(provide
 os-version)

(import-class NSProcessInfo)

(define (os-version)
  (tell #:type _NSString (tell NSProcessInfo processInfo) operatingSystemVersionString))
