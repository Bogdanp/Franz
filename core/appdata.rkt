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

    [(unix)
     (build-path
      (or (getenv "XDG_CONFIG_HOME")
          (expand-user-path "~/.config"))
      app-id)]

    [(windows)
     (build-path
      (or (getenv "APPDATA")
          (expand-user-path "~/AppData/Roaming"))
      app-id)]

    [else
     (error 'current-application-data-directory "not implemented")]))

(define (build-application-path . args)
  (apply build-path (current-application-data-directory) args))

(make-directory* (current-application-data-directory))
