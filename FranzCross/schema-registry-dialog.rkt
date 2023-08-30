#lang racket/gui/easy

(require buid
         franz/schema-registry
         (prefix-in ~ threading)
         "combinator.rkt"
         "keychain.rkt"
         "mixin.rkt"
         "observable.rkt"
         "view.rkt")

(provide
 schema-registry-dialog)

(define (schema-registry-dialog details #:save-action [save-action void])
  (define close! void)
  (define keychain (current-keychain))
  (define-observables
    [@url (~optional-str (SchemaRegistry-url details))]
    [@username (~optional-str (SchemaRegistry-username details))]
    [@password (~optional-str (~and~> (SchemaRegistry-password-id details)
                                      (get-password keychain _)))])
  (dialog
   #:size '(420 #f)
   #:title "Schema Registry"
   #:mixin (mix-close-window
            void
            (λ (close!-proc)
              (set! close! close!-proc)))
   (vpanel
    #:margin '(10 10)
    (labeled
     "Type:"
     (choice
      '(confluent)
      #:choice->label
      (λ (choice)
        (case choice
          [(confluent) "Confluent Schema Registry"]))
      void))
    (labeled
     "Registry URL:"
     (input @url (drop1 (λ:= @url))))
    (hpanel
     (labeled
      "Username:"
      (input @username (drop1 (λ:= @username))))
     (labeled
      #:width #f
      "Password:"
      (password @password (drop1 (λ:= @password)))))
    (hpanel
     #:alignment '(right top)
     (button
      "Cancel"
      (λ () (close!)))
     (button
      "Save"
      #:style '(border)
      (lambda ()
        (define url ^@url)
        (define password (->optional-str ^@password))
        (define password-id
          (or (SchemaRegistry-password-id details)
              (buid)))
        (when keychain
          (if password
              (put-password keychain password-id password)
              (remove-password keychain password-id)))
        (define saved-details
          (and (not (string=? url ""))
               (make-SchemaRegistry
                #:id (SchemaRegistry-id details)
                #:url ^@url
                #:username (->optional-str ^@username)
                #:password-id password-id)))
        (save-action saved-details)
        (close!)))))))

(module+ main
  (render
   (schema-registry-dialog
    #:save-action (λ (r) (eprintf "registry: ~s~n" r))
    (make-SchemaRegistry
     #:id 1
     #:url "https://example.com"))))
