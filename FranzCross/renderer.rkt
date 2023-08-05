#lang racket/base

(provide
 current-renderer)

;; Can't use a proper parameter here since the GUI eventspace won't
;; inherit it.
(define current-renderer
  (let ([renderer #f])
    (case-lambda
      [() renderer]
      [(v) (set! renderer v)])))
