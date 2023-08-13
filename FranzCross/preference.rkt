#lang racket/base

(require franz/appdata)

(provide
 get-preference
 put-preference)

(define (get-preference id [default (λ () (error 'get-preference "no preference found for id ~s" id))])
  (hash-ref (read-prefs) id default))

(define (put-preference id v)
  (write-prefs (hash-set (read-prefs) id v)))

(define (get-preferences-path)
  (build-application-path "preferences.rktd"))

(define (read-prefs)
  (with-handlers ([exn:fail:filesystem? (λ (_) (hasheq))])
    (call-with-input-file (get-preferences-path) read)))

(define (write-prefs ht)
  (call-with-output-file (get-preferences-path)
    #:exists 'truncate/replace
    (lambda (out)
      (write ht out))))
