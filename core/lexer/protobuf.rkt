#lang racket/base

(require (prefix-in impl: protocol-buffers/private/lexer)
         "generic.rkt")

(provide
 make-protobuf-lexer)

(struct protobuf-lexer (impl)
  #:methods gen:lexer
  [(define (lexer-take l)
     (define t
       (impl:lexer-take (protobuf-lexer-impl l)))
     (and (not (eq? (impl:token-type t) 'eof))
          (Token
           (case (impl:token-type t)
             [(boolean) (TokenType.keyword)]
             [(ident full-ident) (TokenType.name)]
             [else (->TokenType (impl:token-type t))])
           (TokenSpan
            (sub1 (impl:token-pos t))
            (string-length (impl:token-str t))))))])

(define (make-protobuf-lexer in)
  (protobuf-lexer
   (impl:make-lexer
    #:skip-comments? #f
    #:partial-strings? #t
    in)))
