#lang racket/base

(require racket/gui)

(provide
 put-clipboard)

(define (put-clipboard s)
  (send the-clipboard set-clipboard-string s (current-seconds)))
