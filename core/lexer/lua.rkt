#lang racket/base

(require (prefix-in impl: lua/lang/lexer)
         "generic.rkt")

(provide
 make-lua-lexer)

(struct lua-lexer (impl)
  #:methods gen:lexer
  [(define (lexer-take l)
     (define t
       (impl:lexer-take (lua-lexer-impl l)))
     (and (not (eq? (impl:token-type t) 'eof))
          (Token
           (->TokenType (impl:token-type t))
           (TokenSpan
            (sub1 (impl:token-pos t))
            (string-length (impl:token-str t))))))])

(define (make-lua-lexer in)
  (lua-lexer
   (impl:make-lexer
    #:skip-comments? #f
    #:partial-strings? #t
    in)))
