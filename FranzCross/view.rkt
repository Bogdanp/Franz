#lang racket/base

(require racket/gui/easy)

(provide
 labeled
 password)

(define (labeled label v #:width [width 120])
  (hpanel
   (hpanel
    #:alignment '(right center)
    #:min-size `(,width #f)
    #:stretch '(#f #t)
    (text label))
   v))

(define password
  (make-keyword-procedure
   (lambda (kws kw-args . args)
     (keyword-apply input kws kw-args args #:style '(single password)))))
