#lang racket/gui/easy

(require buid
         franz/connection-details
         (submod franz/connection-details rpc)
         racket/format
         racket/lazy-require
         racket/match
         (prefix-in ~ threading)
         "combinator.rkt"
         "keychain.rkt"
         "mixin.rkt"
         "observable.rkt"
         "view.rkt")

(lazy-require
 ["alert.rkt" (alert)])

(provide
 connection-dialog)

(define (connection-dialog details
                           #:title [title "Edit Connection"]
                           #:save-label [save-label "Connect"]
                           #:save-action [save void]
                           #:cancel-action [cancel void])
  (define close! void)
  (define keychain (current-keychain))
  (define-observables
    [@name (ConnectionDetails-name details)]
    [@host (ConnectionDetails-bootstrap-host details)]
    [@port (ConnectionDetails-bootstrap-port details)]
    [@mechanism (ConnectionDetails-auth-mechanism details)]
    [@username (~optional-str (ConnectionDetails-username details))]
    [@password  (~optional-str
                 (or (ConnectionDetails-password details)
                     (and keychain (~and~> (ConnectionDetails-password-id details)
                                           (get-password keychain _)))))]
    [@aws-region (~optional-str (ConnectionDetails-aws-region details))]
    [@aws-access-key-id (~optional-str (ConnectionDetails-aws-access-key-id details))]
    [@use-ssl? (ConnectionDetails-use-ssl details)]
    [@ssl-key-path (ConnectionDetails-ssl-key-path details)]
    [@ssl-cert-path (ConnectionDetails-ssl-cert-path details)]
    [@use-http-proxy? (not (not (ConnectionDetails-http-proxy-addr details)))]
    [@http-proxy-host (or (~and~> (ConnectionDetails-http-proxy-addr details) get-http-proxy-host) "127.0.0.1")]
    [@http-proxy-port (or (~and~> (ConnectionDetails-http-proxy-addr details) get-http-proxy-port) 1080)]
    [@testing-connection? #f]
    [@test-connection-label "Test Connection"])
  (define (get-details)
    (define http-proxy-addr
      (and ^@use-http-proxy?
           (format "~a:~a"
                   ^@http-proxy-host
                   ^@http-proxy-port)))
    (define password (->optional-str ^@password))
    (define password-id
      (or (ConnectionDetails-password-id details)
          (buid)))
    (when keychain
      (if password
          (put-password keychain password-id password)
          (remove-password keychain password-id)))
    (make-ConnectionDetails
     #:id (ConnectionDetails-id details)
     #:name (or (->optional-str ^@name) "Unnamed Connection")
     #:http-proxy-addr http-proxy-addr
     #:bootstrap-host ^@host
     #:bootstrap-port ^@port
     #:auth-mechanism ^@mechanism
     #:username (->optional-str ^@username)
     #:password-id password-id
     #:aws-region (->optional-str ^@aws-region)
     #:aws-access-key-id (->optional-str ^@aws-access-key-id)
     #:use-ssl ^@use-ssl?
     #:ssl-key-path ^@ssl-key-path
     #:ssl-cert-path ^@ssl-cert-path))
  (dialog
   #:title title
   #:size '(520 #f)
   #:mixin (mix-close-window
            cancel
            (λ (close!-proc)
              (set! close! close!-proc)))
   (vpanel
    #:margin '(10 5)
    #:stretch '(#t #f)
    (labeled "Name:" (input @name (drop1 @name:=)))
    (hpanel
     (labeled "Bootstrap Host:" (input @host (drop1 @host:=)))
     (labeled "Port:" (validated-input @port (drop1 @port:=) #:text->value string->port) #:width #f))
    (vpanel
     (labeled
      ""
      (checkbox
       #:label "Use HTTP Proxy"
       #:checked? @use-http-proxy?
       @use-http-proxy?:=))
     (if-view
      @use-http-proxy?
      (hpanel
       (labeled
        "HTTP Proxy:"
        (validated-input
         @http-proxy-host
         (drop1 @http-proxy-host:=)))
       (labeled
        "Port:"
        (validated-input
         @http-proxy-port
         (drop1 @http-proxy-port:=)
         #:text->value string->port)
        #:width #f))
      (spacer)))
    (labeled
     "Auth Mechanism:"
     (choice
      (list
       (AuthMechanism.plain)
       (AuthMechanism.scramSHA256)
       (AuthMechanism.scramSHA512)
       (AuthMechanism.aws))
      #:choice->label ~AuthMechanism
      #:selection @mechanism
      @mechanism:=))
    (match-view @mechanism
      [(or (AuthMechanism.plain)
           (AuthMechanism.scramSHA256)
           (AuthMechanism.scramSHA512))
       (hpanel
        (labeled "Username:" (input @username (drop1 @username:=)))
        (labeled "Password:" (password @password (drop1 @password:=)) #:width #f))]
      [(AuthMechanism.aws)
       (vpanel
        (hpanel
         (labeled "Region:" (input @aws-region (drop1 @aws-region:=)))
         (labeled "Access Key:" (input @aws-access-key-id (drop1 @aws-access-key-id:=)) #:width #f))
        (labeled "Secret Key:" (password @password (drop1 @password:=))))])
    (hpanel
     (labeled
      ""
      (checkbox
       #:label "Use SSL"
       #:checked? @use-ssl?
       @use-ssl?:=))
     (button
      (make-browse-label @ssl-key-path "SSL Key")
      #:enabled? @use-ssl?
      (lambda ()
        (define path
          (get-file ^@ssl-key-path '(("PKCS8 Private Key" "*.der; *.pem; *.key"))))
        (@ssl-key-path . := . (~and~> path path->string))))
     (button
      (make-browse-label @ssl-cert-path "SSL Cert")
      #:enabled? @use-ssl?
      (lambda ()
        (define path
          (get-file ^@ssl-cert-path '(("X.509 Certificate" "*.crt; *.pem"))))
        (@ssl-cert-path . := . (~and~> path path->string)))))
    (hpanel
     #:alignment '(right center)
     (button
      "Cancel"
      (lambda ()
        (cancel)
        (close!)))
     (button
      @test-connection-label
      #:enabled? (@testing-connection? . ~> . not)
      (lambda ()
        (@testing-connection?:= #t)
        (thread
         (lambda ()
           (@test-connection-label:= "Testing Connection...")
           (match (test-connection (get-details))
             [#f
              (@testing-connection?:= #f)
              (@test-connection-label:= "Connection OK")
              (sleep 3)
              (@test-connection-label:= "Test Connection")]
             [message
              (@testing-connection?:= #f)
              (alert #:renderer #f "Connection Failed" message)])))))
     (button
      save-label
      #:style '(border)
      #:enabled? (@testing-connection? . ~> . not)
      (lambda ()
        (save (get-details) close!)))))))

(define (~AuthMechanism v)
  (match v
    [(AuthMechanism.plain) "PLAIN"]
    [(AuthMechanism.scramSHA256) "SCRAM-SHA-256"]
    [(AuthMechanism.scramSHA512) "SCRAM-SHA-512"]
    [(AuthMechanism.aws) "AWS-MSK-IAM"]))

(define (string->port v)
  (define n (string->number v))
  (and n (>= n 0) (<= n 65535) n))

(define http-proxy-addr-re
  #rx"([^:]+):([1-9][0-9]*)")

(define (get-http-proxy-host v)
  (cadr (regexp-match http-proxy-addr-re v)))

(define (get-http-proxy-port v)
  (string->number (caddr (regexp-match http-proxy-addr-re v))))

(define (make-browse-label o label)
  (o . ~> . (λ (v) (~a (if v (~a label "*") label) "..."))))

(define (get-file maybe-path filters)
  (define-values (directory filename _must-be-dir?)
    (if maybe-path
        (split-path maybe-path)
        (values #f #f #f)))
  (gui:get-file
   #f ;message
   #f ;parent
   directory
   filename
   #f ;extension
   null ;style
   filters))

(module+ main
  (render
   (connection-dialog
    #:save-action (λ (d _close!) (eprintf "details: ~s~n" d))
    #:cancel-action (λ () (eprintf "canceled~n"))
    (make-ConnectionDetails
     #:name "Example"
     #:bootstrap-host "127.0.0.1"
     #:bootstrap-port 9092))))
