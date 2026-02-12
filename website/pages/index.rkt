#lang racket/base

(require racket/match
         "../template.rkt")

(provide
 render)

(define (render)
  (apply
   template
   #:title "Native macOS and Windows Desktop Client for Apache Kafka"
   #:description #<<DESC
Take control of your Kafka clusters with Franz, a native desktop client
for Apache Kafka and Kafka-compatible brokers like RedPanda. Manage
broker and topic configs, browse your data, configure consumer groups
and more.
DESC
   (hero)
   (append
    (for/list ([(feature idx) (in-indexed (in-list features))])
      (match-define (list title description img-src img-alt) feature)
      (define alt? (zero? (modulo idx 2)))
      (haml
       (:section
        ([:class (if alt? "feature alt" "feature")])
        (.container
         (if alt?
             (haml
              (.row.prefer-right
               (.col
                (:h1.title title)
                (:p description))
               (.col
                (:img.screenshot
                 ([:alt img-alt]
                  [:src img-src])))))
             (haml
              (.row.prefer-left
               (.col
                (:img.screenshot
                 ([:alt img-alt]
                  [:src img-src])))
               (.col
                (:h1.title title)
                (:p description)))))))))
    (list
     (haml
      (:script
       #<<SCRIPT
document.querySelector("#variant-selector").addEventListener("change", function(e) {
  document.querySelector("#download-button").href = e.target.value;
});
SCRIPT
       ))))))

(define (hero)
  (haml
   (:section.hero
    (.container
     (.row.prefer-left
      (.col
       (haml
        (:img.screenshot
         ([:alt "a screenshot of the welcome window"]
          [:src "/assets/01-welcome-window.png"]))))
      (.col.hero__info
       (logo)
       (:h1.title "Franz")
       (:h2.subtitle
        "Native macOS and Windows Desktop Client for "
        (:a
         ([:href "https://kafka.apache.org"])
         "Apache Kafka"))
       (:br)
       (.ctas
        (:select#variant-selector
         ,@(for/list ([variant (in-list variants)])
             (match-define (list label value) variant)
             `(option
               ([value ,(cadr variant)]
                ,@(if (regexp-match #rx"^macOS " label)
                      '([selected "selected"])
                      null))
               ,(car variant))))
        (:br)
        (:br)
        (:a#download-button.ctas__download-button
         ([:href "https://franz.defn.io/releases/Franz%20Latest.universal.dmg"])
         "Download")
        &nbsp
        (:a.ctas__purchase-button
         ([:href "/buy.html"])
         "Purchase a License"))
       (:br)
       (:br)
       (:br)))))))

(define variants
  '(("Linux (x86_64)"         "https://franz.defn.io/releases/Franz%20Latest.linux.x86_64.tar.gz")
    ("macOS 15 (Universal)"   "https://franz.defn.io/releases/Franz%201.7.0001.universal.dmg")
    ("macOS 26+ (Universal)"  "https://franz.defn.io/releases/Franz%20Latest.universal.dmg")
    ("Windows 10+ (x86_64)"   "https://franz.defn.io/releases/Franz%20Latest.win32.x86_64.zip")))

(define features
  '(("Explore Topics"
     #<<DESC
Browse through Kafka topics and records. Intelligently stream data to
debug issues and publish new data directly to your topics.
DESC
     "/assets/02-topic-records-table.png"
     "a screenshot of the topic records table")
    ("Manage Groups"
     #<<DESC
Quickly check the state of your consumer groups and manage consumer
offsets and group membership.
DESC
     "/assets/03-consumer-group.png"
     "a screenshot of a consumer group")
    ("Connect to MSK and Red Panda"
     #<<DESC
Compatible with alternative implementations like Amazon Managed Kafka
IAM and Red Panda out of the box.
DESC
     "/assets/04-aws-msk-iam.png"
     "a screenshot of the New Connection window")
    ("Filter & Process Data"
     #<<DESC
Filter and process the data in your topics using a full Lua, programming
environment with support for decoding JSON, Apache Avro, MessagePack and
other formats.
DESC
     "/assets/05-lua-scripting.png"
     "a screenshot of a Lua script decoding Apache Avro data")
    ("Visualize Your Data"
     #<<DESC
Leverage the Lua programming environment to aggregate and visualize data
from your topics.
DESC
     "/assets/06-data-visualization.png"
     "a screenshot of candlestick graphs visualizing financial candle data on a topic")))
