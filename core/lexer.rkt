#lang racket/base

(require (prefix-in json: json/pretty)
         noise/backend
         noise/serde
         racket/match
         racket/port
         "lexer/generic.rkt"
         "lexer/json.rkt"
         "lexer/lua.rkt")

(define-rpc (pp-json [code : String] : String)
  (call-with-output-string
   (lambda (out)
     (call-with-input-string code
       (lambda (in)
         (json:pretty-print-json in out))))))

(define-rpc (lex [code : String]
                 [using lexer : Lexer] : (Listof Token))
  (define in
    (open-input-string code))
  (define l
    (match lexer
      [(Lexer.json) (make-json-lexer in)]
      [(Lexer.lua) (make-lua-lexer in)]))
  (let loop ([tokens null])
    (define token
      (let retry ()
        (with-handlers ([exn:fail?
                         (lambda (_)
                           (skip-to-next-whitespace in)
                           (retry))])
          (lexer-take l))))
    (if token
        (loop (cons token tokens))
        (reverse tokens))))

(define (skip-to-next-whitespace in)
  (void (regexp-match #rx"[\r\t\n ]" in)))

(module+ test
  (require rackunit)

  (test-case "json lexing"
    (check-equal?
     (lex "[1, true, false]" (Lexer.json))
     (list
      (Token (TokenType.punctuation) (TokenSpan 0 1))
      (Token (TokenType.number) (TokenSpan 1 1))
      (Token (TokenType.punctuation) (TokenSpan 2 1))
      (Token (TokenType.whitespace) (TokenSpan 3 1))
      (Token (TokenType.keyword) (TokenSpan 4 4))
      (Token (TokenType.punctuation) (TokenSpan 8 1))
      (Token (TokenType.whitespace) (TokenSpan 9 1))
      (Token (TokenType.keyword) (TokenSpan 10 5))
      (Token (TokenType.punctuation) (TokenSpan 15 1)))))

  (test-case "json lexing with error"
    (check-equal?
     (lex "[1, #f, true]" (Lexer.json))
     (list
      (Token (TokenType.punctuation) (TokenSpan 0 1))
      (Token (TokenType.number) (TokenSpan 1 1))
      (Token (TokenType.punctuation) (TokenSpan 2 1))
      (Token (TokenType.whitespace) (TokenSpan 3 1))
      (Token (TokenType.keyword) (TokenSpan 8 4))
      (Token (TokenType.punctuation) (TokenSpan 12 1)))))

  (test-case "lua lexing"
    (check-equal?
     (lex "local function foo() return 42 end" (Lexer.lua))
     (list
      (Token (TokenType.keyword) (TokenSpan 0 5))
      (Token (TokenType.whitespace) (TokenSpan 5 1))
      (Token (TokenType.keyword) (TokenSpan 6 8))
      (Token (TokenType.whitespace) (TokenSpan 14 1))
      (Token (TokenType.name) (TokenSpan 15 3))
      (Token (TokenType.punctuation) (TokenSpan 18 1))
      (Token (TokenType.punctuation) (TokenSpan 19 1))
      (Token (TokenType.whitespace) (TokenSpan 20 1))
      (Token (TokenType.keyword) (TokenSpan 21 6))
      (Token (TokenType.whitespace) (TokenSpan 27 1))
      (Token (TokenType.number) (TokenSpan 28 2))
      (Token (TokenType.whitespace) (TokenSpan 30 1))
      (Token (TokenType.keyword) (TokenSpan 31 3))))))
