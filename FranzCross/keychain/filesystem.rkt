#lang racket/base

(require racket/match
         racket/port
         racket/runtime-path
         "generic.rkt")

(provide
 filesystem-keychain?
 make-filesystem-keychain)

(define-runtime-module-path-index win32.rkt
  "win32.rkt")

(define-values (platform-protect-proc platform-unprotect-proc)
  (case (system-type 'os)
    [(windows)
     (values
      (dynamic-require win32.rkt 'protect-data)
      (dynamic-require win32.rkt 'unprotect-data))]
    [else
     (values values values)]))

(define (make-filesystem-keychain path
                                  [protect-proc platform-protect-proc]
                                  [unprotect-proc platform-unprotect-proc])
  (filesystem-keychain path protect-proc unprotect-proc))

(struct filesystem-keychain (path protect-proc unprotect-proc)
  #:methods gen:keychain
  [(define (put-password kc id password)
     (write-ht kc (hash-set (read-ht kc) id password)))

   (define (get-password kc id)
     (hash-ref (read-ht kc) id #f))

   (define (remove-password kc id)
     (write-ht kc (hash-remove (read-ht kc) id)))])

(define (read-ht kc)
  (match-define (filesystem-keychain path _protect unprotect) kc)
  (with-handlers ([exn:fail:filesystem? (λ (_) (hash))])
    (call-with-input-file path
      (lambda (in)
        (call-with-input-bytes (unprotect (port->bytes in)) read)))))

(define (write-ht kc ht)
  (match-define (filesystem-keychain path protect _unprotect) kc)
  (call-with-output-file path
    #:exists 'truncate/replace
    (lambda (out)
      (define bs
        (protect
         (call-with-output-bytes
          (lambda (bs-out)
            (write ht bs-out)))))
      (void (write-bytes bs out)))))

(module+ test
  (require racket/file
           rackunit)

  (define kc
    (make-filesystem-keychain
     (let ([path (make-temporary-file)])
       (begin0 path
         (delete-file path)))))

  (check-false
   (get-password kc "example"))

  (check-equal?
   (put-password kc "example" "hunter2")
   (void))

  (check-equal?
   (get-password kc "example")
   "hunter2")

  (check-equal?
   (remove-password kc "example")
   (void))

  (check-false
   (get-password kc "example")))
