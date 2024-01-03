#lang racket/base

(require racket/path
         racket/runtime-path
         racket/system)

(define-runtime-path build.rkt "build.rkt")
(define-runtime-path pages-dir "pages")
(define-runtime-path template.rkt "template.rkt")

(define racket
  (find-executable-path "racket"))

(define (build reason)
  (eprintf "Rebuilding (~a)...~n" reason)
  (system* racket build.rkt))

(define (something-changed-evt)
  (apply choice-evt
         (map
          (λ (path)
            (define change-evt
              (filesystem-change-evt path))
            (nack-guard-evt
             (lambda (nack-evt)
               (thread
                (lambda ()
                  (sync nack-evt)
                  (filesystem-change-evt-cancel change-evt)))
               (handle-evt
                change-evt
                (λ (_evt) path)))))
          (list*
           build.rkt
           template.rkt
           (for/list ([p (in-directory pages-dir)]
                      #:unless (regexp-match? #rx#"^\\.#" (file-name-from-path p)))
             p)))))

(define (main)
  (with-handlers ([exn:break? void])
    (build "initial")
    (let loop ()
      (sync/enable-break
       (handle-evt
        (filesystem-change-evt pages-dir)
        (lambda (_evt)
          (loop)))
       (handle-evt
        (something-changed-evt)
        (lambda (changed-path)
          (build (file-name-from-path changed-path))
          (loop)))))))

(module+ main
  (main))
