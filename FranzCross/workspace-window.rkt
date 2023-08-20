#lang racket/gui/easy

(require browser/external
         franz/broker
         franz/connection-details
         (submod franz/workspace rpc)
         racket/format
         racket/match
         "alert.rkt"
         "auto-update.rkt"
         "broker-detail.rkt"
         "hacks.rkt"
         "mixin.rkt"
         "observable.rkt"
         "new-topic-dialog.rkt"
         "split-view.rkt"
         "status-bar.rkt"
         "topic-detail.rkt"
         "view.rkt"
         (prefix-in m: "window-manager.rkt")
         "workspace-sidebar.rkt")

(provide
 workspace-window)

(struct state (id cookie status metadata))

(define (make-state id)
  (state id 0 "Ready" (make-Metadata
                       #:brokers null
                       #:topics null
                       #:groups null
                       #:schemas null)))

(define (reload-metadata @state #:force? [force? #t])
  (match-define (state id cookie _status _metadata)
    (update-observable @state
      (struct-copy state it
                   [status "Fetching Metadata"]
                   [cookie (add1 (state-cookie it))])))
  (thread
   (lambda ()
     (define metadata
       (get-metadata force? id))
     (update-observable @state
       (if (eqv? cookie (state-cookie it))
           (struct-copy state it
                        [status "Ready"]
                        [metadata metadata])
           it)))))

(define (workspace-window id details)
  (define-observables
    [@state (make-state id)]
    [@sidebar-visible? #t]
    [@selected-item #f])
  (reload-metadata #:force? #f @state)
  (define close! void)
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
        (reload-metadata @state)))
     (m:get-workspace-renderer id)))
  (define (call-with-status-proc proc)
    (match-define (state _id cookie _status _metadata)
      (update-observable @state
        (struct-copy state it [cookie (add1 (state-cookie it))])))
    (define (status s)
      (update-observable @state
        (when (eqv? (state-cookie it) cookie)
          (struct-copy state it [status s])))
      (void))
    (proc status))
  (define sidebar
    (workspace-sidebar
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
               (when (confirm #:title "Delete Group"
                              #:message (format "Delete ~a? This action cannot be undone." (Group-id item))
                              #:renderer workspace-renderer)
                 (delete-group (Group-id item) id)
                 (reload-metadata @state)))))
           event)]
         [(Topic? item)
          (render-popup-menu*
           workspace-renderer
           (popup-menu
            (menu-item
             "Delete"
             (lambda ()
               (when (confirm #:title "Delete Topic"
                              #:message (format "Delete ~a? This action cannot be undone." (Topic-name item))
                              #:renderer workspace-renderer)
                 (delete-topic (Topic-name item) id)
                 (reload-metadata @state)))))
           event)]
         [else (void)]))
     #:new-topic-action new-topic
     (@state . ~> . state-metadata)))
  (define content
    (vpanel
     (status-bar
      (@state . ~> . state-status)
      (ConnectionDetails-name details))
     (match-view @selected-item
       [(? Broker? b) (broker-detail id b call-with-status-proc)]
       [(? Topic? t) (topic-detail id t call-with-status-proc)]
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
      (λ () (m:open-workspace details #t)))
     (menu-item-separator)
     (menu-item
      "&Preferences"
      m:render-preferences-window)
     (menu-item-separator)
     (menu-item
      "E&xit"
      (λ () (m:close-all-windows))))
    (menu
     "&Connection"
     (menu-item
      "&Reload"
      (λ () (reload-metadata @state)))
     (menu-item-separator)
     (menu-item
      "&Close"
      (λ () (close!))))
    (menu
     "&Topic"
     (menu-item
      "&New Topic..."
      new-topic))
    (menu
     "&View"
     (menu-item
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

(module+ main
  (require db franz/metadata)
  (current-connection
   (sqlite3-connect #:database 'memory))
  (migrate!)
  (m:open-workspace
   (make-ConnectionDetails
    #:id 1
    #:name "Example"
    #:bootstrap-host "kafka-1"
    #:bootstrap-port 9092)))
