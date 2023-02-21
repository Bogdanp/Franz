#lang racket/base

(provide
 read-varint
 read-uvarint
 write-varint
 write-uvarint)

(define (read-varint [in (current-input-port)])
  (define n (read-uvarint in))
  (if (zero? (bitwise-and n 1))
      (arithmetic-shift n -1)
      (bitwise-not (arithmetic-shift n -1))))

(define (write-varint v [out (current-output-port)])
  (write-uvarint
   (bitwise-xor
    (arithmetic-shift v 1)
    (if (< v 0) -1 0))
   out))

(define (read-uvarint [in (current-input-port)])
  (let loop ([s 0])
    (define b (read-byte in))
    (cond
      [(eof-object? b)
       (error 'read-uvarint "unexpected EOF while reading uvarint")]
      [(zero? (bitwise-and b #x80))
       (arithmetic-shift b s)]
      [else
       (+ (arithmetic-shift (bitwise-and b #x7F) s)
          (loop (+ s 7)))])))

(define (write-uvarint v [out (current-output-port)])
  (define bs
    (let loop ([bs null] [n v])
      (define-values (q r)
        (quotient/remainder n #x80))
      (if (zero? q)
          (apply bytes (reverse (cons r bs)))
          (loop (cons (bitwise-ior r #x80 r) bs) q))))
  (write-bytes bs out))
