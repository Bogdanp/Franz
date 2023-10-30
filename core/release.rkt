#lang racket/base

(require (prefix-in http: net/http-easy)
         noise/backend
         noise/serde
         racket/file
         racket/format
         racket/port)

(provide
 (record-out Release)
 get-changelog
 get-releases
 get-latest-release
 fetch-release)

(define versions-service-url
  (or (getenv "FRANZ_VERSIONS_SERVICE_URL") "https://franz.defn.io/versions/"))

(define-record Release
  [arch : Symbol]
  [version : String]
  [mac-url : (Optional String)]
  [linux-url : (Optional String)]
  [windows-url : (Optional String)])

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
       #:mac-url (hash-ref release-data 'macURL #f)
       #:linux-url (hash-ref release-data 'linuxURL #f)
       #:windows-url (hash-ref release-data 'windowsURL #f))))
  (sort releases string>? #:key Release-version))

(define-rpc (get-latest-release [for-arch arch : Symbol] : (Optional Release))
  (define releases
    (get-releases arch))
  (and (not (null? releases))
       (car releases)))

(define-rpc (fetch-release [release r : Release] : String)
  (define release-path (make-temporary-file "Franz ~a.dmg"))
  (call-with-output-file release-path
    #:exists 'replace
    (lambda (out)
      (define res (http:get #:stream? #t (Release-mac-url r)))
      (unless (= (http:response-status-code res) 200)
        (error 'fetch-release "unexpected response from server: ~a" (http:response-body res)))
      (copy-port (http:response-output res) out)))
  (path->string release-path))
