#lang racket/base

(require (prefix-in lua: lua/lang/lexer)
         noise/backend
         noise/serde
         "lang.rkt")

(define-rpc (lex-lua [code : String] : (Listof Token))
  (define in (open-input-string code))
  (define l (lua:make-lexer #:skip-comments? #f #:partial-strings? #t in))
  (let loop ([tokens null])
    (define t
      (let retry ()
        ;; On error, skip to the next whitespace bit and try again.
        ;; The match procedure will skip all the way to EOF if it
        ;; doesn't find any whitespace, so this should always
        ;; terminate.
        (with-handlers* ([exn:fail?
                          (lambda (_)
                            (void (regexp-match #rx"[\r\t\n ]" in))
                            (retry))])
          (lua:lexer-take l))))
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
      (define (default)
        (case type
          [(whitespace) (TokenType.whitespace)]
          [(comment) (TokenType.comment)]
          [(keyword op) (TokenType.keyword)]
          [(number) (TokenType.number)]
          [(string) (TokenType.string)]
          [(name) (TokenType.name)]
          [else (TokenType.punctuation)]))
      (hash-ref! memo type default))))
