#lang racket/base

(require (prefix-in cl: canvas-list)
         racket/class
         racket/gui/easy
         racket/list
         threading
         "hacks.rkt")

(provide canvas-list)

(define (make-canvas-list% %)
  (class* object% (view<%>)
    (init-field @items paint callback item=? item-height)
    (super-new)

    (define/public (dependencies)
      (list @items))

    (define/public (create parent)
      (define items (obs-peek @items))
      (define the-canvas-list
        (new (context-mixin %)
             [parent parent]
             [items items]
             [item-height item-height]
             [paint-item-callback (and paint
                                       (λ (_self item state dc w h)
                                         (paint item state dc w h)))]
             [action-callback (λ (_self item event)
                                (callback 'dbclick item event))]
             [selection-callback (λ (_self item event)
                                   (callback 'select item event))]
             [context-action-callback (λ (self item event)
                                        (make-mouse-event-positions-absolute self event)
                                        (callback 'context item event))]))
      (unless (null? items)
        (send the-canvas-list select-first)
        (send the-canvas-list scroll-to-selection))
      the-canvas-list)

    (define/public (update v what val)
      (case/dep what
        [@items
         (define maybe-index
           (and~> (send v get-selected-item)
                  (index-of val _ item=?)))
         (send v set-items val)
         (send v select-index maybe-index)]))

    (define/public (destroy v)
      (send v clear-context))))

(define (canvas-list @items
                     [paint #f]
                     #:action [callback void]
                     #:item=? [item=? equal?]
                     #:item-height [item-height 20]
                     #:mixin [mix values])
  (new (make-canvas-list% (mix cl:canvas-list%))
       [@items @items]
       [paint paint]
       [callback callback]
       [item=? item=?]
       [item-height item-height]))

(module+ main
  (require racket/gui/easy/operator)
  (render
   (window
    #:size '(300 400)
    (canvas-list
     (@ '(1 2 3))
     #:action (compose1 displayln list)))))
