#lang racket/base

(provide
 hash-pop
 keywords->hash
 hash->keywords)

(define (hash-pop ht k [default (λ () (error 'hash-pop "no value found for key ~s" k))])
  (values
   (hash-ref ht k default)
   (hash-remove ht k)))

(define (keywords->hash kws kw-args)
  (for/hasheq ([kw (in-list kws)]
               [kw-arg (in-list kw-args)])
    (values kw kw-arg)))

(define (hash->keywords ht)
  (for/lists (_kws _kw-args)
             ([kw (in-list (sort (hash-keys ht) keyword<?))])
    (values kw (hash-ref ht kw))))
