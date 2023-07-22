#lang racket/base

(provide
 drop1)

(define ((drop1 proc) _ignored v)
  (proc v))
