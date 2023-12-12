#lang racket/base

(require (for-syntax racket/base
                     syntax/parse/pre))

(provide
 thread*)

(define-syntax (thread* stx)
  (syntax-parse stx
    [(_ body ...+)
     #'(thread
        (lambda ()
          body ...))]))

(module+ test
  (require rackunit)

  (let ([s (make-semaphore)])
    (thread* (semaphore-post s))
    (check-not-false (sync/timeout 1 s))))
