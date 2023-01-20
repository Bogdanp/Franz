#lang racket/base

(require commonmark
         commonmark/struct
         gregor
         racket/format
         racket/match
         racket/runtime-path
         (only-in xml write-xml/content xexpr->xml))

(define (~datetime m)
  (~t m "EEE, dd LLL yyyy HH:mm:ss Z"))

(define-runtime-path atom.xml
  "atom.xml")

(define-runtime-path changelog.txt
  "versions/changelog.txt")

(define changelog
  (call-with-input-file changelog.txt read-document))

(define (make-item blocks)
  (match-define (cons (heading title _) content)
    (reverse blocks))
  (match-define (regexp #rx"^([^ ]+) \\(Build (.+)\\)"
                        (list _
                              release-date-str
                              (app string->number build)))
    title)
  (define release-date
    (parse-moment release-date-str "yyyy.MM.dd"))
  (define release-url
    (format "https://franz.defn.io/releases/Franz%201.0.~a.universal.dmg"
            (~r build
                #:min-width 4
                #:pad-string "0")))
  `(item
    (title ,title)
    (link ,release-url)
    (guid ,release-url)
    (pubDate ,(~datetime release-date))
    (description ,(document->html (document content null)))))

(define items
  (let loop ([blocks (document-blocks changelog)]
             [pending null]
             [items null])
    (cond
      [(null? blocks)
       (reverse (cons (make-item pending) items))]
      [else
       (define block
         (car blocks))
       (match block
         [(heading _ 1)
          (loop
           (cdr blocks)
           (list block)
           (if (null? pending)
               items
               (cons (make-item pending) items)))]
         [_
          (loop (cdr blocks) (cons block pending) items)])])))

(define feed
  `(rss
    ([xmlns:atom "http://www.w3.org/2005/Atom"]
     [version "2.0"])
    (channel
     (title "Franz: Native macOS Kafka Client")
     (link "https://franz.defn.io")
     (description "Franz Releases")
     (language "en-US")
     (lastBuildDate ,(~datetime (now/moment)))
     (atom:link
      ([rel "self"]
       [href "https://franz.defn.io/atom.xml"]
       [type "application/rss+xml"]))
     ,@items)))

(call-with-output-file atom.xml
  #:exists 'replace
  (lambda (out)
    (write-xml/content (xexpr->xml feed) out)))
