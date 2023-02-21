#lang racket/base

(require noise/serde
         racket/generic)

(provide
 ->TokenType
 (enum-out TokenType)
 (record-out TokenSpan)
 (record-out Token)

 (enum-out Lexer)
 lexer?
 lexer-take
 gen:lexer)

(define-enum TokenType
  [whitespace]
  [punctuation]
  [comment]
  [keyword]
  [number]
  [string]
  [name])

(define ->TokenType
  (let ([memo (make-hasheq)])
    (lambda (type)
      (define (default)
        (case type
          [(whitespace) (TokenType.whitespace)]
          [(comment) (TokenType.comment)]
          [(keyword op) (TokenType.keyword)]
          [(number) (TokenType.number)]
          [(string) (TokenType.string)]
          [(name) (TokenType.name)]
          [else (TokenType.punctuation)]))
      (hash-ref! memo type default))))

(define-record TokenSpan
  [pos : UVarint]
  [len : UVarint])

(define-record Token
  [type : TokenType]
  [span : TokenSpan])

(define-enum Lexer
  [json]
  [lua]
  [protobuf])

(define-generics lexer
  {lexer-take lexer})
