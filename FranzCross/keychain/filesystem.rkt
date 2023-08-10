#lang racket/base

(require "generic.rkt")

(provide
 filesystem-keychain?
 make-filesystem-keychain)

(define (make-filesystem-keychain path)
  (filesystem-keychain path))

;; An insecure keychain implementation intended to be used for
;; testing only.
(struct filesystem-keychain (path)
  #:methods gen:keychain
  [(define (put-password kc id password)
     (write-ht kc (hash-set (read-ht kc) id password)))

   (define (get-password kc id)
     (hash-ref (read-ht kc) id #f))

   (define (remove-password kc id)
     (write-ht kc (hash-remove (read-ht kc) id)))])

(define (read-ht kc)
  (with-handlers ([exn:fail:filesystem? (λ (_) (hash))])
    (call-with-input-file (filesystem-keychain-path kc) read)))

(define (write-ht kc ht)
  (call-with-output-file (filesystem-keychain-path kc)
    #:exists 'truncate/replace
    (lambda (out)
      (write ht out))))

(module+ test
  (require racket/file
           rackunit)

  (define kc
    (make-filesystem-keychain
     (let ([path (make-temporary-file)])
       (begin0 path
         (delete-file path)))))

  (check-exn
   #rx"password with id .+ not found"
   (λ () (get-password kc "example")))

  (check-equal?
   (put-password kc "example" "hunter2")
   (void))

  (check-equal?
   (get-password kc "example")
   "hunter2")

  (check-equal?
   (remove-password kc "example")
   (void))

  (check-exn
   #rx"password with id .+ not found"
   (λ () (get-password kc "example"))))
