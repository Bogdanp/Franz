#lang racket/base

(require (prefix-in http: net/http-easy)
         noise/backend
         noise/serde
         racket/format)

(provide
 (record-out Release)
 get-changelog
 get-releases
 get-latest-release)

(define versions-service-url
  "https://franz.defn.io/versions/")

(define-record Release
  [arch : Symbol]
  [version : String]
  [macURL : String])

(define-rpc (get-changelog : String)
  (bytes->string/utf-8
   (http:response-body (http:get (~a versions-service-url "changelog.txt")))))

(define-rpc (get-releases [for-arch arch : Symbol] : (Listof Release))
  (define data
    (http:response-json
     (http:get (~a versions-service-url "versions.json"))))
  (define releases
    (for*/list ([release-data (in-list data)]
                [release-arch (in-value (string->symbol (hash-ref release-data 'arch)))]
                #:when (eq? release-arch arch))
      (make-Release
       #:arch release-arch
       #:version (hash-ref release-data 'version)
       #:macURL (hash-ref release-data 'macURL))))
  (sort releases string>? #:key Release-version))

(define-rpc (get-latest-release [for-arch arch : Symbol] : (Optional Release))
  (define releases
    (get-releases arch))
  (and (not (null? releases))
       (car releases)))
