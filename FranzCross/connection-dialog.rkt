#lang racket/base

(require franz/connection-details
         racket/gui/easy
         racket/gui/easy/operator
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
  (define/obs @details details)
  (define-observable-fields @details
    [@name ConnectionDetails-name
           (λ (d name)
             (set-ConnectionDetails-name d (if (string=? name "") "Unnamed Connection" name)))]
    [@bootstrap-host ConnectionDetails-bootstrap-host set-ConnectionDetails-bootstrap-host]
    [@bootstrap-port (compose1 number->string ConnectionDetails-bootstrap-port)
                     (λ (d port)
                       (define n (string->number port))
                       (if (and n
                                (>= n 0)
                                (<= n 65535))
                           (set-ConnectionDetails-bootstrap-port d n)
                           d))]
    [@mechanism ConnectionDetails-auth-mechanism set-ConnectionDetails-auth-mechanism]
    [@username (compose1 ~optional-str ConnectionDetails-username)
               (λ (d username) (set-ConnectionDetails-username d (->optional-str username)))]
    [@password (compose1 ~optional-str ConnectionDetails-password)
               (λ (d password) (set-ConnectionDetails-password d (->optional-str password)))]
    [@aws-region (compose1 ~optional-str ConnectionDetails-aws-region)
                 (λ (d region) (set-ConnectionDetails-aws-region d (->optional-str region)))]
    [@aws-access-key-id (compose1 ~optional-str ConnectionDetails-aws-access-key-id)
                        (λ (d access-key-id) (set-ConnectionDetails-aws-access-key-id d (->optional-str access-key-id)))]
    [@use-ssl? ConnectionDetails-use-ssl set-ConnectionDetails-use-ssl])
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
     (labeled "Bootstrap Host:" (input @bootstrap-host (drop1 @bootstrap-host:=)))
     (labeled "Port:" (input @bootstrap-port (drop1 @bootstrap-port:=)) #:width #f))
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
    (observable-view
     @mechanism
     (λ (mechanism)
       (match mechanism
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
           (labeled "Secret Key:" (password @password (drop1 @password:=))))])))
    (labeled
     ""
     (checkbox
      #:label "Use SSL"
      #:checked? @use-ssl?
      @use-ssl?:=))
    (hpanel
     #:alignment '(right center)
     (button "Cancel" (λ ()
                        (cancel)
                        (close!)))
     (button
      #:style '(border)
      save-label (λ () (save (obs-peek @details) close!)))))))

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

(module+ main
  (render
   (connection-dialog
    #:save-action (λ (d _close!) (eprintf "details: ~s~n" d))
    #:cancel-action (λ () (eprintf "canceled~n"))
    (make-ConnectionDetails
     #:name "Example"
     #:bootstrap-host "127.0.0.1"
     #:bootstrap-port 9092))))
