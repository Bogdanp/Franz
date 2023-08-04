#lang racket/base

(require racket/class
         (prefix-in gui: racket/gui)
         racket/gui/easy/font
         racket/runtime-path)

;; fonts ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(provide
 system-font
 system-mono-font)

(define-values (system-font system-mono-font)
  (case (system-type 'os*)
    [(macosx) (values "SF Pro Display" "SF Mono")]
    [(linux) (values "Ubuntu" "Ubuntu Monospace")]
    [else (values "Segoe UI" "Consolas")]))

(provide
 font-size-s
 font-size-m
 font-size-l
 font-size-xl)

(define-values (font-size-s font-size-m font-size-l font-size-xl)
  (case (system-type 'os*)
    [(macosx) (values 12 14 16 28)]
    [else (values 10 12 14 18)]))

(provide
 system-font-s
 system-font-m
 system-font-l
 system-font-xl)

(define system-font-s (font system-font font-size-s))
(define system-font-m (font system-font font-size-m))
(define system-font-l (font system-font font-size-l))
(define system-font-xl (font system-font font-size-xl))


;; colors ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (color r g b)
  (make-object gui:color% r g b))

(define-syntax-rule (define-colors [id r g b] ...)
  (begin
    (provide id ...)
    (define id (color r g b)) ...))

(define-colors
  [white #xFF #xFF #xFF]
  [primary-color #x00 #x00 #x00]
  [secondary-color #x50 #x50 #x50]
  [selection-background-color #x08 #x4A #xDA]
  [selection-primary-color #xFF #xFF #xFF]
  [selection-secondary-color #xC3 #xDE #xFF])


;; assets ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-runtime-path assets "assets")

(define-syntax-rule (define-asset id)
  (begin
    (provide id)
    (define id (build-path assets (symbol->string 'id)))))

(define-syntax-rule (define-assets id0 id ...)
  (begin
    (define-asset id0)
    (define-asset id) ...))

(define-assets
  icon_512x512.png)
