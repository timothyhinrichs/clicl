;SERVLETS
(defservlet auction-listing :updates (fetch-all-auctions) :page auction-listing :Entry t)
; SIGNATURES AND SCHEMA
;GUARDS
;(defguard dani-guard
;	(=> (dani-sign.field-a ?x) (dani-sign.field-b ?y) (= ?x ?y)) "field1 must be the same as field2"
;	(=> (dani-sign.field-c ?x) (and (gte ?x 0) (lte ?x 9))) "field3 must be between 0 and 9"
;	)
;PAGES
(defhtml auction-listing header auction-listing-page footer)
;(defhtml dani ("dani.html" :forms(("dani-form" dani-form)))) ;table?
(defhtml auction-listing-page ("auction-listing.html" :tables(("list" auction-listing-table)))) 
;FORM
;(defform dani-form :schema dani-sign :target dani-test2 :guards (dani-guard)) ;guards are in schema..also here?
(deftable auction-listing-table :schema item)

;UPDATES
(defupdate fetch-all-auctions (:language posneg)
  (<= (pos (item.id ?auc)) (session.username ?user) (db.auction.owner ?auc ?user))
  (<= (pos (item.title ?tit)) (session.username ?user) (db.auction.owner ?auc ?user)(db.auction.title ?auc ?tit))
  ;...others

)
;BUILTINS



