#lang racket/base

(require racket/format
         racket/string)

(provide
 shortcut
 shortcut-alt)

(define (sym->str s)
  (case s
    [(ctl) "⌃"]
    [(opt) "⌥"]
    [(cmd) "⌘"]
    [(space) "⎵"]
    [(shift) "⇧"]
    [(return) "↩"]
    [(delete) "⌫"]
    [(escape) "⎋"]
    [(comma) ","]
    [(up) "↑"]
    [(down) "↓"]
    [else (if (symbol? s)
              (symbol->string s)
              (~a s))]))

(define (sym->name s)
  (case s
    [(ctl) "control"]
    [(alt) "alt"]
    [(opt) "option"]
    [(cmd) "command"]
    [(space) "space"]
    [(shift) "shift"]
    [(return) "return"]
    [(delete) "delete"]
    [(escape) "escape"]
    [(comma) ","]
    [(up) "up"]
    [(down) "down"]
    [else (if (symbol? s)
              (symbol->string s)
              (~a s))]))

(define (shortcut . syms)
  (define strs (map sym->str syms))
  (string-join strs " "))

(define (shortcut-alt . syms)
  (define names (map sym->name syms))
  (string-join names "-"))
