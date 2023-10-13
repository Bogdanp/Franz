#lang racket/gui/easy

(require racket/class
         racket/string
         "hacks.rkt")


;; text ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(provide
 mix-typeahead)

(define ((mix-typeahead completions) %)
  (class %
    (field [last-text #f] [editing? #f]
           [the-completions (if (obs? completions)
                                (obs-peek completions)
                                completions)])
    (init-field parent label init-value enabled callback style font
                vert-margin horiz-margin min-width min-height
                stretchable-width stretchable-height)
    (super-new
     [parent parent]
     [label label]
     [init-value init-value]
     [enabled enabled]
     [style style]
     [font font]
     [vert-margin vert-margin]
     [horiz-margin horiz-margin]
     [min-width min-width]
     [min-height min-height]
     [stretchable-width stretchable-width]
     [stretchable-height stretchable-height]

     [callback (λ (self event)
                 (unless editing?
                   (define editor (send self get-editor))
                   (define start
                     (let ([s-box (box #f)])
                       (send editor get-position s-box)
                       (unbox s-box)))
                   (define non-selected-text
                     (send editor get-text 0 start))
                   (unless (or (equal? non-selected-text last-text)
                               (equal? non-selected-text ""))
                     (set! last-text non-selected-text)
                     (define the-match
                       (for/first ([completion (in-list the-completions)]
                                   #:when (string-prefix? completion non-selected-text))
                         completion))
                     (when the-match
                       (define rest-s
                         (substring the-match start))
                       (set! editing? #t)
                       (send editor begin-edit-sequence)
                       (send editor insert rest-s)
                       (send editor set-position start (send editor get-end-position))
                       (send editor end-edit-sequence)
                       (set! editing? #f)))
                   (callback self event)))])
    (when (obs? completions)
      (obs-observe! completions (λ (cs)
                                  (gui:queue-callback
                                   (lambda ()
                                     (set! the-completions cs))))))))


;; window ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(provide
 mix-close-window)

(define ((mix-close-window on-close-proc [out-proc void]) %)
  (class %
    (inherit show)
    (super-new)
    (define/augment (on-close)
      (on-close-proc))
    (out-proc (λ () (show #f)))))


;; window<%> ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(provide
 mix-context-event
 mix-initial-focus)

(define ((mix-context-event proc) %)
  (class %
    (super-new)
    (define/override (on-subwindow-event receiver event)
      (case (send event get-event-type)
        [(right-down)
         (define maybe-item-index
           (cond
             [(is-a? receiver gui:list-box%)
              (define item-height
                (quotient
                 (send receiver get-height)
                 (add1 (send receiver number-of-visible-items))))
              (define y-pos
                (let ([y (send event get-y)])
                  (case (system-type 'os)
                    [(windows) ;; does not include scroll offset
                     (+ y (* item-height (send receiver get-first-visible-item)))]
                    [else y])))
              (define item-index
                (let* ([index (quotient y-pos item-height)]
                       [index (case (system-type 'os)
                                [(macosx) index] ;; the first non-header row has y=0
                                [else (sub1 index)])])
                  (and (< index (send receiver get-number)) index)))
              (begin0 item-index
                (when item-index
                  (send receiver select item-index #t)))]
             [else #f]))
         (make-mouse-event-positions-absolute receiver event)
         (begin0 #t
           (proc event maybe-item-index))]
        [else #f]))))

(define (mix-initial-focus %)
  (class %
    (inherit focus)
    (super-new)
    (gui:queue-callback
     (λ () (focus)))))


(module+ main
  (render
   (window
    #:size '(320 #f)
    (input
     #:mixin (λ (%)
               (let ([mix-typeahead (mix-typeahead
                                     '("cleanup.policy"
                                       "compression.type"))])
                 (mix-initial-focus (mix-typeahead %))))
     ""))))
