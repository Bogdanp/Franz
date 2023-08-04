#lang racket/base

(require franz/connection-details
         racket/gui/easy
         racket/match
         "combinator.rkt"
         "mixin.rkt"
         "observable.rkt"
         "view.rkt")

(provide
 connection-dialog)

(define (connection-dialog details
                           #:title [title "Edit Connection"]
                           #:save-label [save-label "Connect"]
                           #:save-action [save void]
                           #:cancel-action [cancel void])
  (define close! void)
  (define-observables
    [@name (ConnectionDetails-name details)]
    [@host (ConnectionDetails-bootstrap-host details)]
    [@port (ConnectionDetails-bootstrap-port details)]
    [@mechanism (ConnectionDetails-auth-mechanism details)]
    [@username (~optional-str (ConnectionDetails-username details))]
    [@password (~optional-str (ConnectionDetails-password details))]
    [@aws-region (~optional-str (ConnectionDetails-aws-region details))]
    [@aws-access-key-id (~optional-str (ConnectionDetails-aws-access-key-id details))]
    [@use-ssl? (ConnectionDetails-use-ssl details)])
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
    (labeled
     ""
     (checkbox
      #:label "Use SSL"
      #:checked? @use-ssl?
      @use-ssl?:=))
    (hpanel
     #:alignment '(right center)
     (button
      "Cancel"
      (lambda ()
        (cancel)
        (close!)))
     (button
      #:style '(border)
      save-label
      (lambda ()
        (define saved-details
          (make-ConnectionDetails
           #:id (ConnectionDetails-id details)
           #:name (or (->optional-str ^@name) "Unnamed Connection")
           #:bootstrap-host ^@host
           #:bootstrap-port ^@port
           #:auth-mechanism ^@mechanism
           #:username (->optional-str ^@username)
           #:password (->optional-str ^@password)
           #:aws-region (->optional-str ^@aws-region)
           #:aws-access-key-id (->optional-str ^@aws-access-key-id)
           #:use-ssl ^@use-ssl?))
        (save saved-details close!)))))))

(define (~AuthMechanism v)
  (match v
    [(AuthMechanism.plain) "PLAIN"]
    [(AuthMechanism.scramSHA256) "SCRAM-SHA-256"]
    [(AuthMechanism.scramSHA512) "SCRAM-SHA-512"]
    [(AuthMechanism.aws) "AWS-MSK-IAM"]))

(define (~optional-str s)
  (or s ""))

(define (->optional-str v)
  (if (string=? v "") #f v))

(define (string->port v)
  (define n (string->number v))
  (and n (>= n 0) (<= n 65535) n))

(module+ main
  (render
   (connection-dialog
    #:save-action (λ (d _close!) (eprintf "details: ~s~n" d))
    #:cancel-action (λ () (eprintf "canceled~n"))
    (make-ConnectionDetails
     #:name "Example"
     #:bootstrap-host "127.0.0.1"
     #:bootstrap-port 9092))))
