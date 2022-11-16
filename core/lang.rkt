#lang racket/base

(require noise/serde)

(provide
 (enum-out TokenType)
 (record-out TokenSpan)
 (record-out Token))

(define-enum TokenType
  [whitespace]
  [punctuation]
  [comment]
  [keyword]
  [number]
  [string]
  [name])

(define-record TokenSpan
  [pos : UVarint]
  [len : UVarint])

(define-record Token
  [type : TokenType]
  [span : TokenSpan])
