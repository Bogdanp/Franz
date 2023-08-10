#lang racket/base

(require racket/generic)

(provide
 (all-defined-out))

(define-generics keychain
  {put-password keychain id password}
  {get-password keychain id}
  {remove-password keychain id})
