#lang racket/base

(require racket/class)

(provide
 mix-close-window)

(define ((mix-close-window on-close-proc [out-proc void]) %)
  (class %
    (super-new)
    (define/augment (on-close)
      (on-close-proc))
    (define/public (do-close)
      (send this show #f))
    (out-proc (λ () (send this do-close)))))
