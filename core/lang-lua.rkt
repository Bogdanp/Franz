#lang racket/base

(require (prefix-in lua: lua/lang/lexer)
         noise/backend
         noise/serde
         "lang.rkt")

(define-rpc (lex-lua [code : String] : (Listof Token))
  (define in (open-input-string code))
  (define lex (lua:make-lexer in #f))
  (let loop ([tokens null])
    (define t
      (let retry ()
        (with-handlers* ([exn:fail? (λ (_)
                                      ;; On error, skip to the next whitespace bit and try again.
                                      (void (regexp-match #rx"[\r\t\n ]" in))
                                      (retry))])
          (lua:lexer-take lex))))
    (case (lua:token-type t)
      [(eof) (reverse tokens)]
      [else (loop (cons (->Token t) tokens))])))

(define (->Token t)
  (Token
   (->TokenType (lua:token-type t))
   (TokenSpan
    (sub1 (lua:token-pos t))
    (string-length (lua:token-str t)))))

(define ->TokenType
  (let ([memo (make-hasheq)])
    (lambda (type)
      (hash-ref! memo type (λ ()
                             (case type
                               [(whitespace) (TokenType.whitespace)]
                               [(comment) (TokenType.comment)]
                               [(keyword op) (TokenType.keyword)]
                               [(number) (TokenType.number)]
                               [(string) (TokenType.string)]
                               [(name) (TokenType.name)]
                               [else (TokenType.punctuation)]))))))
