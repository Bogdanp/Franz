#lang racket/base

(require (for-syntax racket/base
                     racket/path
                     setup/getinfo))

(provide
 franz-version)

(begin-for-syntax
  (define this-path (build-path (path-only (syntax-source #'here))))
  (define info-ref (get-info/full this-path)))

(define-syntax (get-version stx)
  (datum->syntax stx (info-ref 'version)))

(define franz-version
  (get-version))
