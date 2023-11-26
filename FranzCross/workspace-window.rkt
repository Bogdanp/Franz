#lang racket/gui/easy

(require franz/broker
         franz/connection-details
         (submod franz/connection-details rpc)
         franz/schema-registry
         (submod franz/schema-registry rpc)
         franz/schema-registry/schema
         (submod franz/workspace rpc)
         net/sendurl
         racket/format
         racket/match
         "alert.rkt"
         "broker-detail.rkt"
         "group-detail.rkt"
         "hacks.rkt"
         "keychain.rkt"
         "mixin.rkt"
         "observable.rkt"
         "publish-dialog.rkt"
         "new-topic-dialog.rkt"
         "schema-detail.rkt"
         "schema-registry-dialog.rkt"
         "shortcut.rkt"
         "split-view.rkt"
         "status-bar.rkt"
         "thread.rkt"
         "topic-detail.rkt"
         "view.rkt"
         (prefix-in m: "window-manager.rkt")
         "workspace-sidebar.rkt")

(provide
 workspace-window)

(struct state (id cookie status metadata))

(define (make-state id)
  (define metadata
    (make-Metadata
     #:brokers null
     #:topics null
     #:groups null
     #:schemas null))
  (state id 0 "Ready" metadata))

(define (workspace-window id details)
  (define close! void)
  (define-observables
    [@state (make-state id)]
    [@sidebar-visible? #t]
    [@selected-item #f])
  (define (call-with-status-proc proc)
    (match-define (state _id cookie _status _metadata)
      (update-observable @state
        (struct-copy state it [cookie (add1 (state-cookie it))])))
    (define (status s)
      (update-observable @state
        (if (eqv? (state-cookie it) cookie)
            (struct-copy state it [status s])
            it))
      (void))
    (dynamic-wind
      (λ () (void))
      (λ () (proc status))
      (λ () (status "Ready"))))
  (define (reload-metadata #:force? [force? #t])
    (thread*
     (call-with-status-proc
      (lambda (status)
        (status "Fetching Metadata")
        (update-observable @state
          (struct-copy state it [metadata (get-metadata force? id)]))))))
  (define (new-topic)
    (render
     (new-topic-dialog
      #:create-action
      (lambda (name partitions replication-factor options)
        (define the-options
          (for/list ([o (in-list options)])
            (make-TopicOption
             #:key (car o)
             #:value (cdr o))))
        (create-topic name partitions replication-factor the-options id)
        (reload-metadata)))
     (m:get-workspace-renderer id)))
  (define (publish [item ^@selected-item])
    (render
     (publish-dialog
      id (and item
              (Topic? item)
              (Topic-name item))
      #:publish-action
      (lambda (topic-name partition-id key value)
        (publish-record
         topic-name
         partition-id
         (and key (string->bytes/utf-8 key))
         (and value (string->bytes/utf-8 value))
         id)))
     (m:get-workspace-renderer id)))
  (define (open-consumer-group gid)
    (define group
      (for/first ([g (in-list (Metadata-groups (state-metadata ^@state)))]
                  #:when (equal? (Group-id g) gid))
        g))
    (when group
      (@selected-item:= group)))
  (define registry-id
    (ConnectionDetails-schema-registry-id details))
  (when registry-id
    (do-activate-schema-registry id (get-schema-registry registry-id)))
  (reload-metadata #:force? #f)
  (define sidebar
    (workspace-sidebar
     #:selected-item @selected-item
     #:select-action
     (λ:= @selected-item)
     #:context-action
     (λ (item event)
       (define workspace-renderer
         (m:get-workspace-renderer id))
       (cond
         [(Group? item)
          (render-popup-menu*
           workspace-renderer
           (popup-menu
            (menu-item
             "Delete"
             (lambda ()
               (define message
                 (format "Delete ~a? This action cannot be undone." (Group-id item)))
               (when (confirm #:title "Delete Group"
                              #:message message
                              #:renderer workspace-renderer)
                 (delete-group (Group-id item) id)
                 (@selected-item:= #f)
                 (reload-metadata)))))
           event)]
         [(Topic? item)
          (render-popup-menu*
           workspace-renderer
           (popup-menu
            (menu-item
             "Publish Record..."
             (λ () (publish item)))
            (menu-item-separator)
            (menu-item
             "Delete"
             (lambda ()
               (define message
                 (format "Delete ~a? This action cannot be undone." (Topic-name item)))
               (when (confirm #:title "Delete Topic"
                              #:message message
                              #:renderer workspace-renderer)
                 (delete-topic (Topic-name item) id)
                 (@selected-item:= #f)
                 (reload-metadata)))))
           event)]
         [(Schema? item)
          (render-popup-menu*
           workspace-renderer
           (popup-menu
            (menu-item
             "Delete"
             (lambda ()
               (define message
                 (format "Delete ~a? This action cannot be undone." (Schema-name item)))
               (when (confirm #:title "Delete Schema"
                              #:message message
                              #:renderer workspace-renderer)
                 (delete-schema (Schema-name item) id)
                 (@selected-item:= #f)
                 (reload-metadata)))))
           event)]
         [else (void)]))
     #:new-topic-action new-topic
     details (@state . ~> . state-metadata)))
  (define content
    (vpanel
     (status-bar
      (@state . ~> . state-status)
      (ConnectionDetails-name details))
     (match-view @selected-item
       [(? Broker? b) (broker-detail id b call-with-status-proc)]
       [(? Topic? t) (topic-detail id t open-consumer-group call-with-status-proc)]
       [(? Group? g) (group-detail id g call-with-status-proc)]
       [(? Schema? s) (schema-detail id s call-with-status-proc)]
       [_ default-view])))
  (window
   #:title (~title details)
   #:mixin (mix-close-window
            (lambda ()
              (m:close-workspace id))
            (lambda (close-proc)
              (set! close! close-proc)))
   #:min-size '(800 600)
   (menu-bar
    (menu
     "&File"
     (menu-item
      "New &Window"
      #:shortcut (kbd cmd shift #\n)
      (λ () (m:open-workspace details #t)))
     (menu-item-separator)
     (menu-item
      "&Preferences"
      #:shortcut (kbd cmd #\;)
      m:render-preferences-window)
     (menu-item-separator)
     (menu-item
      "E&xit"
      (λ () (m:close-all-windows))))
    (menu
     "&Connection"
     (menu-item
      "&Reload"
      #:shortcut (kbd cmd #\r)
      reload-metadata)
     (menu-item-separator)
     (menu-item
      "&Close"
      #:shortcut (kbd cmd #\w)
      (λ () (close!))))
    (menu
     "&Topic"
     (menu-item
      "&New Topic..."
      #:shortcut (kbd cmd #\n)
      new-topic)
     (menu-item "&Publish Record..." publish))
    (menu
     "Schema &Registry"
     (menu-item
      "Configure..."
      (lambda ()
        (let* ([details (get-connection (ConnectionDetails-id details))]
               [registry-id (ConnectionDetails-schema-registry-id details)])
          (render
           (schema-registry-dialog
            (if registry-id
                (get-schema-registry registry-id)
                (make-SchemaRegistry #:url ""))
            #:save-action
            (lambda (saved-registry)
              (when registry-id
                (deactivate-schema-registry id))
              (define updated-details
                (cond
                  [saved-registry
                   (define saved-registry-id
                     (SchemaRegistry-id
                      (if registry-id
                          (update-schema-registry saved-registry)
                          (save-schema-registry saved-registry))))
                   (do-activate-schema-registry id saved-registry)
                   (set-ConnectionDetails-schema-registry-id details saved-registry-id)]
                  [registry-id
                   (deactivate-schema-registry id)
                   (delete-schema-registry id)
                   (set-ConnectionDetails-schema-registry-id details #f)]
                  [else
                   details]))
              (update-connection updated-details)
              (reload-metadata)))
           (m:get-workspace-renderer id))))))
    (menu
     "&View"
     (menu-item
      #:shortcut (kbd cmd shift #\s)
      (let-observable ([visible? @sidebar-visible?])
        (~a (if visible? "Hide" "Show") " &Sidebar"))
      (λ<~ @sidebar-visible? not)))
    (menu
     "&Help"
     (menu-item
      "Franz &Manual"
      (λ () (send-url "https://franz.defn.io/manual/")))
     (menu-item
      "&Welcome to Franz"
      #:shortcut (kbd cmd shift #\1)
      m:render-welcome-window)
     (menu-item-separator)
     (menu-item
      "Check for Updates..."
      m:render-check-for-updates-dialog)
     (menu-item-separator)
     (menu-item
      "About &Franz"
      m:render-about-window)))
   (if-view
    @sidebar-visible?
    (split-view
     #:collapse-action (λ () (@sidebar-visible? . := . #f))
     sidebar content)
    content)))

(define (~title details)
  (format
   "~a — ~a"
   (ConnectionDetails-name details)
   (~hostname details)))

(define (~hostname details)
  (format
   "~a:~a"
   (ConnectionDetails-bootstrap-host details)
   (ConnectionDetails-bootstrap-port details)))

(define default-view
  (hpanel
   #:alignment '(center center)
   (text "Select a Broker, Topic or Consumer Group")))

(define (do-activate-schema-registry id registry)
  (define keychain (current-keychain))
  (define password (and keychain (get-password keychain (SchemaRegistry-password-id registry))))
  (activate-schema-registry registry password id))

(module+ main
  (require "testing.rkt")
  (call-with-testing-context
   (lambda (_id)
     (m:open-workspace
      (make-ConnectionDetails
       #:id 1
       #:name "Example"
       #:bootstrap-host "kafka-1"
       #:bootstrap-port 9092))
     (m:get-workspace-renderer 1))))
