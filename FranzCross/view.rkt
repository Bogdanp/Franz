#lang racket/base

(require racket/format
         racket/gui/easy
         racket/gui/easy/operator
         "keyword.rkt")

(provide
 labeled
 password
 validated)

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

(define validated
  (make-keyword-procedure
   (lambda (kws kw-args @data action . args)
     (let*-values ([(kw-ht) (keywords->hash kws kw-args)]
                   [(valid? kw-ht) (hash-pop kw-ht '#:valid? (λ () (λ (_) #t)))]
                   [(kws kw-args) (hash->keywords kw-ht)])
       (define/obs @v
         (~a (obs-peek @data)))
       (define (wrapped-action event v)
         (@v . := . v)
         (when (valid? v)
           (action event v)))
       (keyword-apply
        input
        kws kw-args
        @v wrapped-action args
        #:background-color (@v . ~> . (λ (v) (if (valid? v) #f (color "red")))))))))

(module+ main
  (render
   (window
    (vpanel
     (validated
      #:label "Anything:"
      (@ "hello") void)
     (validated
      #:label "Numbers:"
      #:valid? string->number
      (@ 42) void)))))
