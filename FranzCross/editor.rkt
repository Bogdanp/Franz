#lang racket/gui/easy

(require (for-syntax racket/base)
         (submod franz/lexer rpc)
         franz/lexer/generic
         racket/class
         racket/gui/easy/private/view/keymap
         racket/match
         racket/string
         "common.rkt")

(provide
 editor)

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
  (set-delta-foreground (color #x1D1D1DFF)))

(define-style comment-style base-style
  (set-delta-foreground (color #x4A5660FF)))

(define-style keyword-style base-style
  (set-delta-foreground (color #x9B2493FF)))

(define-style string-style base-style
  (set-delta-foreground (color #xC51B17FF)))

(define-style number-style base-style
  (set-delta-foreground (color #x1C04CEFF)))

(define indent-phrase-re
  (let ([phrases '("function" "local function" "if" "elseif" "else" "for" "while" "do" "repeat")])
    (pregexp (string-append "[[:space:]]*" "(" (string-join (map regexp-quote phrases) "|") ")"))))

(define indent-sym-re
  (regexp (string-append "[" (regexp-quote (string #\( #\[ #\{)) "]$")))

(define dedent-re
  #px"[[:space:]]{2,}(elseif|else|end|until|[)}\\]])[[:space:]]*")

(define tab-size 2)
(define indent-str
  (string->immutable-string
   (make-string tab-size #\space)))

(define (save-excursion editor proc)
  (define pos #f)
  (dynamic-wind
    (lambda ()
      (send editor begin-edit-sequence #f)
      (set! pos (send editor get-start-position)))
    (lambda ()
      (proc editor))
    (lambda ()
      (send editor set-position pos)
      (send editor end-edit-sequence))))

(define (get-line-span editor)
  (define pos (send editor get-start-position))
  (define line (send editor position-line pos))
  (define start-pos (send editor line-start-position line))
  (define end-pos (send editor line-end-position line))
  (values start-pos end-pos))

(define (get-line-text editor)
  (define-values (start-pos end-pos)
    (get-line-span editor))
  (send editor get-text start-pos end-pos))

(define (get-indent line)
  (string-length (car (regexp-match #px"[[:space:]]*" line))))

(define (get-next-indent editor event)
  (save-excursion
   editor
   (lambda (_)
     (previous-line editor event)
     (define line (get-line-text editor))
     (define prev-indent (get-indent line))
     (if (or (regexp-match? indent-phrase-re line)
             (regexp-match? indent-sym-re line))
         (+ prev-indent tab-size)
         prev-indent))))

(define (newline-and-indent editor event)
  (send editor insert "\n")
  (define indent (get-next-indent editor event))
  (unless (zero? indent)
    (send editor insert (make-string indent #\space))))

(define (indent editor _event)
  (define line-text (get-line-text editor))
  (cond
    [(regexp-match-exact? #px"[[:space:]]*" line-text)
     (send editor insert indent-str)]
    [else
     (define-values (start-pos _end-pos)
       (get-line-span editor))
     (send editor insert indent-str start-pos)]))

(define (dedent editor _event)
  (define line-text (get-line-text editor))
  (when (>= (get-indent line-text) tab-size)
    (do-dedent editor)))

(define (maybe-dedent editor)
  (define line-text (get-line-text editor))
  (when (regexp-match-exact? dedent-re line-text)
    (do-dedent editor)))

(define (do-dedent editor)
  (define-values (start-pos _end-pos)
    (get-line-span editor))
  (send editor delete start-pos (+ start-pos tab-size)))

(define keymap
  (let ([keymap (new gui:keymap%)])
    (begin0 keymap
      (send keymap add-function "newline-and-indent" newline-and-indent)
      (send keymap map-function "c:j" "newline-and-indent")
      (send keymap map-function "return" "newline-and-indent")
      (send keymap add-function "indent" indent)
      (send keymap map-function "tab" "indent")
      (send keymap add-function "dedent" dedent)
      (send keymap map-function "s:tab" "dedent")
      (send keymap chain-to-keymap the-default-keymap #f))))

(define (make-code-editor% highlight-proc action-proc)
  (class gui:text%
    (super-new)

    (define/augment (on-change)
      (queue-highlight)
      (action-proc (send this get-text)))

    (define/augment (on-display-size)
      (queue-highlight))

    (define/augment (after-insert _start _len)
      (maybe-dedent this))

    (define/override (after-scroll-to)
      (super after-scroll-to)
      (queue-highlight))

    (define pending? #f)
    (define (queue-highlight)
      (unless pending?
        (set! pending? #t)
        (define deadline-evt
          (alarm-evt (+ (current-inexact-monotonic-milliseconds) 16) #t))
        (thread
         (lambda ()
           (sync deadline-evt)
           (gui:queue-callback
            (lambda ()
              (set! pending? #f)
              (highlight-proc this)))))))))

(define editor%
  (class* object% (view<%>)
    (init-field @code @lang action)
    (super-new)

    (define/public (dependencies)
      (list @code @lang))

    (define/public (create parent)
      (define the-editor
        (new (context-mixin
              (make-code-editor% highlight action))))
      (define the-canvas
        (new gui:editor-canvas%
             [parent parent]
             [editor the-editor]
             [style '(no-border)]))
      (send the-canvas set-scroll-via-copy #t)
      (send* the-editor
        (set-keymap keymap)
        (set-style-list gui:the-style-list)
        (set-max-undo-history 1000)
        (set-context 'lang (obs-peek @lang)))
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
      (send* editor
        (begin-edit-sequence)
        (erase)
        (change-style base-style)
        (insert code 0)
        (end-edit-sequence)))

    (define (highlight editor)
      (define text (send editor get-text))
      (define lexer
        (case (send editor get-context 'lang)
          [(json) (Lexer.json)]
          [(lua) (Lexer.lua)]
          [(protobuf) (Lexer.protobuf)]
          [else (error 'highlight "unexpected lang")]))
      (send editor begin-edit-sequence #f)
      (define-values (start-pos end-pos)
        (let ([sb (box #f)]
              [eb (box #f)])
          (send editor get-visible-position-range sb eb)
          (values (unbox sb) (unbox eb))))
      (for ([token (in-list (lex text lexer))])
        (define span (Token-span token))
        (define token-start-pos (TokenSpan-pos span))
        (define token-end-pos (+ token-start-pos (TokenSpan-len span)))
        (when (or (and (>= token-start-pos start-pos)
                       (<= token-start-pos end-pos))
                  (and (>= token-end-pos start-pos)
                       (<= token-end-pos end-pos)))
          (define style
            (match (Token-type token)
              [(TokenType.comment) comment-style]
              [(TokenType.keyword) keyword-style]
              [(TokenType.string) string-style]
              [(TokenType.number) number-style]
              [_ base-style]))
          (send editor change-style style token-start-pos token-end-pos)))
      (send editor end-edit-sequence))))

(define (editor code
                [action void]
                #:lang [lang 'lua])
  (new editor%
       [@code (@ code)]
       [@lang (@ lang)]
       [action action]))

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
         (@lang . := . choice)
         (@code . := . (case choice
                         [(json) json-example]
                         [(lua) lua-example]
                         [(protobuf) protobuf-example])))))
     (editor #:lang @lang @code)))))
