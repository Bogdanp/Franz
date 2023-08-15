#lang racket/base

(require (prefix-in cl: canvas-list)
         racket/class
         racket/gui/easy
         racket/list
         threading)

(provide canvas-list)

(define canvas-list%
  (class* object% (view<%>)
    (init-field @items paint callback item=? item-height)
    (super-new)

    (define/public (dependencies)
      (list @items))

    (define/public (create parent)
      (new (context-mixin cl:canvas-list%)
           [parent parent]
           [items (obs-peek @items)]
           [item-height item-height]
           [paint-item-callback (and paint
                                     (λ (_self item state dc w h)
                                       (paint item state dc w h)))]
           [action-callback (λ (_self item event)
                              (callback 'dbclick item event))]
           [selection-callback (λ (_self item event)
                                 (callback 'select item event))]
           [context-action-callback (λ (self item event)
                                      (define-values (x-pos y-pos)
                                        (let loop ([x 0] [y 0] [w self])
                                          (define w-parent
                                            (send w get-parent))
                                          (if w-parent
                                              (loop (+ x (send w get-x))
                                                    (+ y (send w get-y))
                                                    w-parent)
                                              (values x y))))
                                      (send event set-x (+ x-pos (send event get-x)))
                                      (send event set-y (+ y-pos (send event get-y)))
                                      (callback 'context item event))]))

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
                     #:item-height [item-height 20])
  (new canvas-list%
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
