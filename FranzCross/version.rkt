#lang racket/base

(require (for-syntax racket/base
                     racket/format
                     setup/getinfo))

(provide
 franz-cross-version)

(begin-for-syntax
  (define (info-ref id)
    ((get-info '("FranzCross")) id)))

(define-syntax (get-version stx)
  (datum->syntax stx (format "~a.~a"
                             (info-ref 'version)
                             (~r #:min-width 4
                                 #:pad-string "0"
                                 (info-ref 'build-number)))))

(define franz-cross-version
  (get-version))
