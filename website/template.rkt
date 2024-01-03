#lang racket/base

(require koyo/haml
         racket/date
         racket/match)

(provide
 (all-from-out koyo/haml)
 template
 header
 logo)

(define (template #:title title
                  #:description [description #f]
                  . content)
  (let ([title (format "Franz: ~a" title)])
    (haml
     (:html
      ([:lang "en-US"])
      (:head
       (:meta ([:charset "utf-8"]))
       (:meta ([:name "viewport"] [:content "width=device-width, initial-scale=1"]))
       (:link ([:rel "icon"] [:type "image/png"] [:href "/assets/favicon.png"]))
       (:title title)
       (:link ([:rel "stylesheet"] [:type "text/css"] [:href "/assets/screen.css"]))
       ,@(if description
             (haml
              (:meta ([:name "description"]
                      [:content description]))
              ,@(open-graph title description))
             null))
      (:body
       ,@content
       (:section.footer
        (.container
         (:ul.footer__links
          (footer-link "/" "Home")
          (footer-link "/buy.html" "Pricing")
          (footer-link "/manual/" "Manual")
          (footer-link "/license.html" "License")
          (footer-link "/privacy.html" "Privacy")
          (footer-link "/atom.xml" '((type "application/rss+xml")) "RSS Feed")
          (footer-link "https://www.youtube.com/channel/UCtVE9Z4cqIaAUIDaU7dFvJg/" "YouTube")
          (footer-link "https://hachyderm.io/@franz_app" '((rel "me")) "Mastodon")
          (footer-link "mailto:bogdan@defn.io?subject=Franz" "Contact Me"))
         (:p.footer__copyright
          &copy (format " ~a CLEARTYPE SRL" (date-year (current-date)))))))))))

(define (header subtitle)
  (haml
   (.hero
    (.container
     (.row.prefer-left
      (.col.hero__info
       (logo)
       (:h1.title "Franz")
       (:h1.subtitle subtitle)))))))

(define (logo)
  (haml
   (:a
    ([:href "https://franz.defn.io"])
    (:img.logo
     ([:alt "the Franz logo"]
      [:src "/assets/logo.png"])))))

(define (open-graph title description)
  (define props
    `((og:url "https://franz.defn.io")
      (og:type "website")
      (og:title ,title)
      (og:description ,description)
      (og:image "https://franz.defn.io/assets/og-hero.png")
      (twitter:card "summary_large_image")
      (twitter:domain "franz.defn.io")
      (twitter:url "https://franz.defn.io")
      (twitter:title ,title)
      (twitter:description ,description)
      (twitter:image ,"https://franz.defn.io/assets/og-hero.png")))
  (for/list ([prop (in-list props)])
    (match-define (list property content) prop)
    (define prop-attr
      (case property
        [(twitter:card twitter:title twitter:description twitter:image) 'name]
        [else 'property]))
    `(meta
      ([,prop-attr ,(symbol->string property)]
       [content ,content]))))

(define footer-link
  (case-lambda
    [(uri label)
     (haml
      (:li.footer__link
       (:a ([:href uri]) label)))]
    [(uri attrs label)
     `(li
       ([class "footer__link"])
       (a ([href ,uri] ,@attrs) ,label))]))
