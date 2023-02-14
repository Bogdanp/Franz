#lang racket/base

(require ffi/unsafe/nsstring
         ffi/unsafe/objc
         racket/format)

(provide
 os-version)

(import-class NSProcessInfo)

(define (os-version)
  (~a "macOS " (tell #:type _NSString (tell NSProcessInfo processInfo) operatingSystemVersionString)))
