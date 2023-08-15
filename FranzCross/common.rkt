#lang racket/base

(require racket/class
         (prefix-in gui: racket/gui)
         racket/gui/easy/color
         racket/gui/easy/font
         racket/runtime-path)

;; fonts ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(provide
 system-font
 system-mono-font)

(define-values (system-font system-mono-font)
  (case (system-type 'os*)
    [(macosx) (values "Lucida Grande" "Monaco")]  ;; pango can't deal with more modern fonts
    [(linux) (values "Ubuntu" "Ubuntu Mono")]
    [else (values "Segoe UI" "Consolas")]))

(provide
 font-size-xs
 font-size-s
 font-size-m
 font-size-l
 font-size-xl)

(define-values (font-size-xs font-size-s font-size-m font-size-l font-size-xl)
  (case (system-type 'os*)
    [(macosx) (values 10 12 14 18 22)]
    [else (values 8 10 12 14 18)]))

(provide
 system-font-xs
 system-font-s
 system-font-m
 system-font-l
 system-font-xl)

(define system-font-xs (font system-font font-size-xs))
(define system-font-s (font system-font font-size-s))
(define system-font-m (font system-font font-size-m))
(define system-font-l (font system-font font-size-l))
(define system-font-xl (font system-font font-size-xl))

(provide
 system-mono-font-xs
 system-mono-font-s
 system-mono-font-m
 system-mono-font-l
 system-mono-font-xl)

(define system-mono-font-xs (font system-mono-font font-size-xs))
(define system-mono-font-s (font system-mono-font font-size-s))
(define system-mono-font-m (font system-mono-font font-size-m))
(define system-mono-font-l (font system-mono-font font-size-l))
(define system-mono-font-xl (font system-mono-font font-size-xl))


;; colors ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-syntax-rule (define-colors [id c] ...)
  (begin
    (provide id ...)
    (define id (color c)) ...))

(define-colors
  [white #xFFFFFFFF]
  [primary-color #x000000FF]
  [secondary-color #x505050FF]
  [selection-background-color #x084ADAFF]
  [selection-primary-color #xFFFFFFFF]
  [selection-secondary-color #xC3DEFFFF])


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
  chevron-s.png
  chevron-e.png
  icon_512x512.png)
