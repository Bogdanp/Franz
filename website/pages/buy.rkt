#lang racket/base

(require "../template.rkt")

(provide
 render)

(define (render)
  (apply
   template
   #:title "License & Pricing"
   (haml
    (header "License & Pricing")
    (:section.feature.alt
     (.container
      (.row.align-top
       (.col
        (:h1.title "Commercial Licenses")
        (:p
         #<<DESC
If you are a company with employees that use Franz, this is the license
for you. Commercial licenses are perpetual and transferrable within your
organization, but each license may only be used on one device at a time.
Commercial licenses cost $79, including VAT.
DESC
         (:br)
         (:br)
         (:center
          (:a#purchase-commercial.ctas__purchase-button
           ([:href "javascript:;"])
           (:strong "Purchase a Commercial License")))))
       (.col
        (:h1.title "Personal Licenses")
        (:p
         #<<DESC
If you are an indivdual paying for Franz out of pocket, then this is
the license for you. Personal licenses are non-transferrable, and may
be activated on up to three of your personal devices. Personal licenses
cost $49, including VAT.
DESC
         )
        (:br)
        (:center
         (:a#purchase-personal.ctas__purchase-button
          ([:href "javascript:;"])
          (:strong "Purchase a Personal License")))
        (:br)
        (:p
         "Discounts for students are also available. "
         "Please email " email-addr " if you would like a discount.")))))
    (:script
     ([:src "https://cdn.paddle.com/paddle/paddle.js"]))
    (:script
     ([:type "text/javascript"])
     #<<SCRIPT
var opts = {
  vendor: 161445,
  productCommercial: 803305,
  productPersonal: 802128,
};
if (window.location.host === "localhost:8000") {
  Paddle.Environment.set("sandbox");
  opts = {
    vendor: 8684,
    productCommercial: 40749,
    productPersonal: 37147,
  }
}

Paddle.Setup({vendor: opts.vendor});
document.querySelector("#purchase-commercial").addEventListener("click", function() {
  Paddle.Checkout.open({ product: opts.productCommercial });
});
document.querySelector("#purchase-personal").addEventListener("click", function() {
  Paddle.Checkout.open({ product: opts.productPersonal });
});
SCRIPT
     ))))


(define email-addr
  (haml
   (:a
    ([:href "mailto:bogdan@defn.io"])
    "bogdan@defn.io")))
