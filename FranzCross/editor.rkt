#lang racket/gui/easy

(require (for-syntax racket/base)
         (submod franz/lexer rpc)
         franz/lexer/generic
         racket/class
         racket/gui/easy/private/view/keymap
         racket/match
         "common.rkt")

(define-syntax-rule (define-style id base [meth arg ...] ...)
  (define id
    (let ([dt (new gui:style-delta%)])
      (send dt meth arg ...) ...
      (send gui:the-style-list find-or-create-style base dt))))

(define basic-style
  (send gui:the-style-list basic-style))

(define-style base-style basic-style
  (set-face system-mono-font)
  (set-delta 'change-size 14)
  (set-delta 'change-weight 'normal)
  (set-delta-foreground (color #x1D #x1D #x1D)))

(define-style comment-style base-style
  (set-delta-foreground (color #x4A #x56 #x60)))

(define-style keyword-style base-style
  (set-delta-foreground (color #x9B #x24 #x93)))

(define-style string-style base-style
  (set-delta-foreground (color #xC5 #x1B #x17)))

(define-style number-style base-style
  (set-delta-foreground (color #x1C #x04 #xCE)))

(define (make-code-editor% highlight-proc change-proc)
  (class gui:text%
    (super-new)

    (define/augment (on-delete _start _len)
      (queue-highlight this))
    (define/augment (on-insert _start _len)
      (queue-highlight this))
    (define/augment (after-delete _start _len)
      (change-proc (send this get-text)))
    (define/augment (after-insert _start _len)
      (change-proc (send this get-text)))

    (define pending? #f)
    (define (queue-highlight editor)
      (unless pending?
        (set! pending? #t)
        (thread
         (lambda ()
           (sleep (/ 1.0 30.0))
           (gui:queue-callback
            (lambda ()
              (highlight-proc editor)
              (set! pending? #f)))))))))

(define editor%
  (class* object% (view<%>)
    (init-field @code @lang change-proc)
    (super-new)

    (define/public (dependencies)
      (list @code @lang))

    (define/public (create parent)
      (define the-editor
        (new (context-mixin
              (make-code-editor% highlight change-proc))))
      (define the-canvas
        (new gui:editor-canvas%
             [parent parent]
             [editor the-editor]
             [style '(no-border)]))
      (send the-editor set-style-list gui:the-style-list)
      (send the-editor change-style base-style)
      (send the-editor set-keymap the-default-keymap)
      (send the-editor set-context 'lang (obs-peek @lang))
      (replace the-editor (obs-peek @code))
      the-canvas)

    (define/public (update v what val)
      (define editor (send v get-editor))
      (case/dep what
        [@code (replace editor val)]
        [@lang (send editor set-context 'lang val)
               (highlight editor)]))

    (define/public (destroy v)
      (send (send v get-editor) clear-context))

    (define (replace editor code)
      (send editor erase)
      (send editor change-style base-style)
      (send editor insert code 0))

    (define (highlight editor)
      (define text (send editor get-text))
      (define lexer
        (case (send editor get-context 'lang)
          [(json) (Lexer.json)]
          [(lua) (Lexer.lua)]
          [(protobuf) (Lexer.protobuf)]
          [else (error 'highlight "unexpected lang")]))
      (for ([token (in-list (lex text lexer))])
        (define span (Token-span token))
        (define pos (TokenSpan-pos span))
        (define len (TokenSpan-len span))
        (define style
          (match (Token-type token)
            [(TokenType.comment) comment-style]
            [(TokenType.keyword) keyword-style]
            [(TokenType.string) string-style]
            [(TokenType.number) number-style]
            [_ base-style]))
        (send editor change-style style pos (+ pos len))))))

(define (editor code
                [action void]
                #:lang [lang 'lua])
  (new editor%
       [@code (@ code)]
       [@lang (@ lang)]
       [change-proc action]))

(module+ main
  (define json-example #<<JSON
{
  "a": 42,
  "b": [true, false]
}
JSON
    )

  (define lua-example #<<LUA
local script = {}

-- Returning `false` or `nil` from this function removes the `record`
-- from the result set.  Modifications you make to `record` in this
-- function will be visible in the data table.
--
-- See Help -> Manual... to learn more.
function script.transform(record)
  return record
end

return script
LUA
    )

  (define protobuf-example #<<PROTOBUF
syntax = "proto3";
import public "other.proto";
option java_package = "com.example.foo";
enum EnumAllowingAlias {
  option allow_alias = true;
  EAA_UNSPECIFIED = 0;
  EAA_STARTED = 1;
  EAA_RUNNING = 1;
  EAA_FINISHED = 2 [(custom_option) = "hello world"];
}
message Outer {
  option (my_option).a = true;
  message Inner {   // Level 2
    int64 ival = 1;
  }
  repeated Inner inner_message = 2;
  EnumAllowingAlias enum_field = 3;
  map<int32, string> my_map = 4;
}
PROTOBUF
    )

  (define/obs @lang 'lua)
  (define/obs @code lua-example)
  (render
   (window
    #:size '(800 600)
    (vpanel
     (hpanel
      #:stretch '(#t #f)
      (choice
       #:choice->label symbol->string
       #:selection @lang
       '(json lua protobuf)
       (lambda (choice)
         (case choice
           [(json)
            (@lang . := . 'json)
            (@code . := . json-example)]
           [(lua)
            (@lang . := . 'lua)
            (@code . := . lua-example)]
           [(protobuf)
            (@lang . := . 'protobuf)
            (@code . := . protobuf-example)]))))
     (editor #:lang @lang @code)))))
