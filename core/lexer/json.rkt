#lang racket/base

(require (prefix-in impl: json/lexer)
         "generic.rkt")

(provide
 make-json-lexer)

(struct json-lexer (impl)
  #:methods gen:lexer
  [(define (lexer-take l)
     (define t
       (impl:lexer-take (json-lexer-impl l)))
     (and (not (eq? (impl:token-type t) 'eof))
          (Token
           (->TokenType (impl:token-type t))
           (TokenSpan
            (sub1 (impl:token-pos t))
            (string-length (impl:token-str t))))))])

(define (make-json-lexer in)
  (json-lexer
   (impl:make-lexer
    #:partial-strings? #t
    in)))
