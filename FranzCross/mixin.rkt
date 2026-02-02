#lang racket/gui/easy

(require (for-syntax racket/base)
         racket/class
         racket/string
         "hacks.rkt")


;; combinator ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(provide
 compose-mixins)

(define ((compose-mixins . ms) %)
  (let loop ([ms ms])
    ((car ms)
     (if (pair? (cdr ms))
         (loop (cdr ms))
         %))))


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
 mix-close-window
 mix-set!-closer)

(define ((mix-close-window on-close-proc [out-proc void]) %)
  (class %
    (inherit show)
    (super-new)
    (define/augment (on-close)
      (on-close-proc))
    (out-proc (λ () (show #f)))))

(define-syntax (mix-set!-closer stx)
  (syntax-case stx ()
    [(_ close!-id)
     #'(mix-close-window void (λ (close!-proc)
                                (set! close!-id close!-proc)))]))


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
                 (case (system-type 'os)
                   [(unix) (send receiver number-of-visible-items)]
                   [else (add1 (send receiver number-of-visible-items))])))
              (define y-pos
                (let ([y (send event get-y)])
                  (case (system-type 'os)
                    [(unix windows) ;; does not include scroll offset
                     (+ y (* item-height (send receiver get-first-visible-item)))]
                    [else y])))
              (define item-index
                (let* ([index (quotient y-pos item-height)]
                       [index (case (system-type 'os)
                                [(macosx unix) index] ;; the first non-header row has y=0
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
     #:mixin
     (compose-mixins
      (mix-typeahead
       '("cleanup.policy"
         "compression.type"))
      mix-initial-focus)
     ""))))
