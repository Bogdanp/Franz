#lang racket/base

(require racket/file
         racket/path
         racket/runtime-path
         xml)

(provide
 build)

(define-runtime-path assets-dir "assets")
(define-runtime-path atom.xml "atom.xml")
(define-runtime-path build-dir "build")
(define-runtime-path manual-dir "manual")
(define-runtime-path pages-dir "pages")
(define-runtime-path versions-dir "versions")

(define (build)
  (delete-directory/files build-dir #:must-exist? #f)
  (make-directory* build-dir)
  (copy-directory/files assets-dir (build-path build-dir "assets"))
  (copy-directory/files manual-dir (build-path build-dir "manual"))
  (copy-directory/files versions-dir (build-path build-dir "versions"))
  (copy-file atom.xml (build-path build-dir "atom.xml"))
  (for ([p (in-directory pages-dir)]
        #:unless (regexp-match? #rx#"^\\.#" (file-name-from-path p))
        #:when (equal? (path-get-extension p) #".rkt"))
    (define-values (_dir name _is-dir?)
      (split-path p))
    (define target-path
      (build-path build-dir (path-replace-extension name #".html")))
    (define xexpr
      ((dynamic-require `(file ,(path->string p)) 'render)))
    (call-with-output-file target-path
      (lambda (out)
        (displayln "<!DOCTYPE HTML>" out)
        (parameterize ([current-unescaped-tags html-unescaped-tags])
          (write-xexpr xexpr out))))))

(module+ main
  (build))
