#lang at-exp racket/base

(require racket/format
         racket/string
         threading
         web-server/dispatch
         web-server/http
         "../core/license.rkt")

(define (webhook req)
  (define quantity
    (or (and~> (bindings-assq #"quantity" (request-bindings/raw req))
               (binding:form-value)
               (bytes->string/utf-8)
               (string->number))
        1))
  (define licenses
    (for/list ([_ (in-range quantity)])
      (~license-key (generate-random-license))))
  (response/output
   #:mime-type #"text/plain"
   (lambda (out)
     (displayln @~a{# Thank You for Purchasing Franz

                    @(if (= quantity 1)
                         @~a{Here is your license key:}
                         @~a{Here are your license keys:})

                    * @(string-join licenses "\n* ")} out))))

(define-values (app _)
  (dispatch-rules
   [("api" "_webhook")
    #:method (or "get" "post")
    webhook]
   [else (λ (_req)
           (response/output
            #:code 404
            (lambda (out)
              (displayln "not found" out))))]))

(module+ main
  (require racket/async-channel
           racket/cmdline
           racket/contract
           web-server/servlet-dispatch
           web-server/web-server)
  (define-values (host port)
    (let ([host "127.0.0.01"])
      (command-line
       #:once-each
       ["--host" HOST "the host to listen on"
                 (set! host HOST)]
       #:args [PORT]
       (values host (string->number PORT)))))
  (unless ((integer-in 0 65535) port)
    (eprintf "error: invalid PORT~n")
    (exit 1))

  (define ready-or-exn-ch
    (make-async-channel))
  (define stop
    (serve
     #:port port
     #:listen-ip host
     #:dispatch (dispatch/servlet app)
     #:confirmation-channel ready-or-exn-ch))
  (define maybe-exn (sync ready-or-exn-ch))
  (when (exn:fail? maybe-exn)
    (raise maybe-exn))
  (with-handlers ([exn:break? void])
    (sync never-evt))
  (stop))
