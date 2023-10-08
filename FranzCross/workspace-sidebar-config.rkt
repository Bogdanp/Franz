#lang racket/gui/easy

(require franz/connection-details
         racket/match
         "observable.rkt"
         "preference.rkt"
         "view.rkt")


;; struct ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(provide
 (struct-out workspace-sidebar-config)
 make-default-workspace-sidebar-config
 get-workspace-sidebar-config
 put-workspace-sidebar-config)

(struct workspace-sidebar-config (topic-stat group-stat)
  #:prefab)

(define (make-default-workspace-sidebar-config)
  (workspace-sidebar-config 'partition-count 'sum-partition-lag))

(define (get-workspace-sidebar-config conn)
  (get-preference
   `(workspace-sidebar ,(ConnectionDetails-id conn))
   make-default-workspace-sidebar-config))

(define (put-workspace-sidebar-config conn conf)
  (put-preference `(workspace-sidebar ,(ConnectionDetails-id conn)) conf))

;; view ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(provide
 workspace-sidebar-config-form)

(define (workspace-sidebar-config-form conf [action void])
  (match-define (workspace-sidebar-config topic-stat group-stat)
    conf)
  (define-observables
    [@topic-stat topic-stat]
    [@group-stat group-stat])
  (vpanel
   #:alignment '(left top)
   #:stretch '(#t #f)
   (labeled
    "Topic Stat:"
    (stat-choice @topic-stat @topic-stat:=))
   (labeled
    "Group Stat:"
    (stat-choice @group-stat @group-stat:=))
   (labeled
    ""
    (button
     "Save"
     #:style '(border)
     (lambda ()
       (action (workspace-sidebar-config ^@topic-stat ^@group-stat)))))))

(define (stat-choice @stat [action void])
  (choice
   '(off partition-count min-partition-lag max-partition-lag sum-partition-lag)
   #:choice->label ~stat
   #:selection @stat
   action))

(define (~stat v)
  (case v
    [(off) "Off"]
    [(partition-count) "Partition Count"]
    [(min-partition-lag) "Min Partition Lag"]
    [(max-partition-lag) "Max Partition Lag"]
    [(sum-partition-lag) "Sum Partition Lag"]
    [else (raise-argument-error '~stat "stat/c" v)]))

(module+ main
  (require "testing.rkt")
  (call-with-testing-context
   (lambda (_id)
     (render
      (window
       (workspace-sidebar-config-form
        (make-default-workspace-sidebar-config)))))))
