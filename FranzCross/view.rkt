#lang racket/base

(require (for-syntax racket/base)
         racket/format
         racket/gui/easy
         racket/gui/easy/operator
         racket/match
         "keyword.rkt"
         "observable.rkt")

(provide
 match-view
 labeled
 password
 validated-input)

(define-syntax-rule (match-view obs-expr clause0 clause ...)
  (observable-view obs-expr (match-lambda clause0 clause ...)))

(define (labeled label v
                 #:width [width 120]
                 #:alignment [alignment '(right center)])
  (hpanel
   (hpanel
    #:min-size `(,width #f)
    #:alignment alignment
    #:stretch '(#f #t)
    (text label))
   v))

(define password
  (make-keyword-procedure
   (lambda (kws kw-args . args)
     (keyword-apply input kws kw-args args #:style '(single password)))))

(define invalid-bg-color
  (color "red"))

(define validated-input
  (make-keyword-procedure
   (lambda (kws kw-args @data action . args)
     (let*-values ([(kw-ht) (keywords->hash kws kw-args)]
                   [(text->value kw-ht) (hash-pop kw-ht '#:text->value (λ () (λ (_) #t)))]
                   [(valid? kw-ht) (hash-pop kw-ht '#:valid? (λ () #f))]
                   [(kws kw-args) (hash->keywords kw-ht)])
       (define/obs @text (~a (obs-peek @data)))
       (obs-observe! @data (compose1 (λ:= @text) ~a))
       (when valid?
         (valid? . := . (not (not (text->value (obs-peek @text))))))
       (define (wrapped-action event text)
         (@text . := . text)
         (define maybe-value (text->value text))
         (when maybe-value
           (action event maybe-value))
         (when valid?
           (valid? . := . (not (not maybe-value)))))
       (keyword-apply
        input
        kws kw-args
        @text wrapped-action args
        #:background-color (let-observable ([text @text])
                             (if (text->value text)
                                 #f
                                 invalid-bg-color)))))))

(module+ main
  (require "combinator.rkt")
  (define/obs @n 42)
  (render
   (window
    #:size '(400 #f)
    (vpanel
     (validated-input
      #:label "Anything:"
      (@ "hello") void)
     (validated-input
      #:label "Numbers:"
      #:text->value string->number
      @n (drop1 (λ:= @n)))
     (button "Increment" (λ () (@n . <~ . add1)))))))
