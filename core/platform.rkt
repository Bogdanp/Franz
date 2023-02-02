#lang racket/base

(require racket/runtime-path)

(provide
 os-version)

(define-runtime-module-path macos.rkt "platform/macos.rkt")

(define (os-version)
  (case (system-type 'os)
    [(macosx) ((dynamic-require macos.rkt 'os-version))]
    [else (error 'os-version "not implemented")]))
