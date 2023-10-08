#lang racket/base

(provide
 non-empty-string
 positive-number
 nonnegative-number)

(define (non-empty-string s)
  (and (> (string-length s) 0) s))

(define (positive-number s)
  (define maybe-number
    (string->number s))
  (and maybe-number (> maybe-number 0) maybe-number))

(define (nonnegative-number s)
  (define maybe-number
    (string->number s))
  (and maybe-number (>= maybe-number 0) maybe-number))
