#lang racket/base

(require file/sha1
         racket/date
         racket/format
         racket/match
         racket/port
         racket/random
         (prefix-in s: "secret.rkt"))

(provide
 generate-random-license
 parse-license
 ~license-key
 license-seconds)

(define license-secret
  (string->bytes/utf-8 s:license-secret))

;; license    ::= version '-' issue-date rand '-' checksum
;; version    ::= u8
;; issue-date ::= u16be
;; rand       ::= u8{8}
;; checksum   ::= u8{8}
(struct license (version issue-date rand))

(define (generate-random-license)
  (license 1 (current-date-offset) (crypto-random-bytes 8)))

(define (parse-license s)
  (match s
    [(regexp #rx"^(..)(....)-([0-9A-F]+)-([0-9A-F]+)$"
             (list _ version-str issue-date-str rand-str checksum-str))
     (define payload
       (call-with-output-bytes
        (lambda (out)
          (write-string version-str out)
          (write-string issue-date-str out)
          (write-char #\- out)
          (write-string rand-str out))))
     (define checksum
       (checksum-bytes payload))
     (and (bytes=? checksum (hex-string->bytes checksum-str))
          (let ([version (integer-bytes->integer (hex-string->bytes version-str) #f #t)]
                [issue-date (integer-bytes->integer (hex-string->bytes issue-date-str) #f #t)]
                [rand (hex-string->bytes rand-str)])
            (license version issue-date rand)))]

    [_
     #f]))

(define (~license-key the-license)
  (match-define (license version issue-date rand)
    the-license)
  (define payload
    (call-with-output-bytes
     (lambda (out)
       (write-string (~byte version) out)
       (display-bytes (integer->integer-bytes issue-date 2 #f #t) out)
       (write-char #\- out)
       (display-bytes rand out))))
  (call-with-output-string
   (lambda (out)
     (write-bytes payload out)
     (write-char #\- out)
     (display-bytes (checksum-bytes payload) out))))

(define (license-seconds l)
  (+ epoch (* (license-issue-date l) 86400)))

(define (checksum-bytes payload)
  (subbytes (sha1-bytes (bytes-append payload license-secret)) 0 12))

(define (~byte b)
  (string-upcase
   (if (< b 16)
       (~a "0" (number->string b 16))
       (number->string b 16))))

(define (display-bytes bs out)
  (for ([b (in-bytes bs)])
    (write-string (~byte b) out)))


;; trials ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(provide
 set-trial-reset-flag
 trial-reset-license?)

(define (set-trial-reset-flag l)
  (struct-copy license l [version (bitwise-ior #x80 (license-version l))]))

(define (trial-reset-license? l)
  (= #x80 (bitwise-and #x80 (license-version l))))


;; date ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define epoch
  (find-seconds 0 0 0 1 10 2022 #f))

(define (current-date-offset [now (current-seconds)])
  (quotient (- now epoch) 86400))
