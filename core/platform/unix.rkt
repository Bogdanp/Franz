#lang racket/base

(require ffi/unsafe
         racket/match)

(provide
 os-version)

;; https://man.freebsd.org/cgi/man.cgi?query=uname&apropos=0&sektion=2&manpath=FreeBSD%201.1-RELEASE&arch=default&format=html

;; struct utsname {
;;  char sysname[SYS_NMLN];
;;  char nodename[SYS_NMLN];
;;  char release[SYS_NMLN];
;;  char version[SYS_NMLN];
;;  char machine[SYS_NMLN];
;; };

;; In Racket, we don't have access to the SYS_NMLN constant, so we can't
;; define our own utsname cstruct. Instead, we allocate a slab of raw
;; bytes that we pass to uname, then parse out the fields manually.

(define the-os-version
  (let* ([len (* 1024 1024)]
         [buf (malloc len)])
    (memset buf 0 len)
    (define uname (get-ffi-obj "uname" #f (_fun _pointer -> _int)))
    (unless (zero? (uname buf))
      (error 'uname "failed"))

    (define bs
      (list->bytes
       (cast buf _pointer (_list o _byte len))))
    (match-define (list* sysname _nodename release _version machine _)
      (map bytes->string/utf-8 (regexp-split #rx#"\0+" bs)))
    (begin0 (format "~a Version ~a (~a)" sysname release machine)
      (collect-garbage))))

(define (os-version)
  the-os-version)
