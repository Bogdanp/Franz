#lang racket/base

(require racket/file)

(provide
 current-application-data-directory
 build-application-path)

(define app-id "io.defn.Franz")

(define (current-application-data-directory)
  (case (system-type 'os)
    [(macosx)
     (build-path
      (find-system-path 'home-dir)
      "Library"
      "Application Support"
      app-id)]

    [else
     (error 'current-application-data-directory "not implemented")]))

(define (build-application-path . args)
  (apply build-path (current-application-data-directory) args))

(make-directory* (current-application-data-directory))
