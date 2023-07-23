#lang racket/base

(require racket/runtime-path)

(provide
 os-version)

(define-runtime-module-path macos.rkt "platform/macos.rkt")
(define-runtime-module-path unix.rkt "platform/unix.rkt")
(define-runtime-module-path windows.rkt "platform/windows.rkt")

(define (os-version)
  (case (system-type 'os)
    [(macosx) ((dynamic-require macos.rkt 'os-version))]
    [(unix) ((dynamic-require unix.rkt 'os-version))]
    [(windows) ((dynamic-require windows.rkt 'os-version))]
    [else (error 'os-version "not implemented")]))
