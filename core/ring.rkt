#lang racket/base

(require racket/fixnum
         racket/match)

(provide
 make-ring
 ring?
 ring-length
 ring-push!
 ring->list)

(struct ring (data [len #:mutable] [idx #:mutable]))

(define (make-ring cap)
  (ring (make-vector cap #f) 0 0))

(define (ring-length r)
  (ring-len r))

(define (ring-push! r v)
  (match-define (ring data len idx) r)
  (define cap (vector-length data))
  (vector-set! data idx v)
  (set-ring-len! r (fxmin cap (fx+ 1 len)))
  (set-ring-idx! r (fxmodulo (fx+ 1 idx) cap)))

(define (ring->list r)
  (match-define (ring data len idx) r)
  (define cap (vector-length data))
  (define start (fxmodulo (fx- idx len) cap))
  (for/list ([i (in-range len)])
    (vector-ref data (fxmodulo (fx+ start i) cap))))

(module+ test
  (require rackunit)
  (define r (make-ring 2))
  (check-equal? (ring-length r) 0)
  (check-equal? (ring->list r) null)
  (ring-push! r 1)
  (check-equal? (ring->list r) '(1))
  (ring-push! r 2)
  (check-equal? (ring->list r) '(1 2))
  (ring-push! r 3)
  (check-equal? (ring->list r) '(2 3))
  (ring-push! r 4)
  (check-equal? (ring->list r) '(3 4)))
