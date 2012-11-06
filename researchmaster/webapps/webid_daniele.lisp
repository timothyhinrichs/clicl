;SERVLETS
(defservlet auction-listing :guards (loggedin) :updates (fetch-your-auctions) :page auction-listing :Entry t)
(defservlet search :updates(search-auctions fetch-auctions) :page auction-listing :Entry t)
(defservlet search-by-categ :updates(search-by-categ fetch-auctions) :page auction-listing :Entry t)
(defservlet adv-search-results :updates(adv-search-auctions0 adv-search-auctions1 adv-search-auctions2 adv-search-auctions3 fetch-auctions) :page auction-listing :Entry t)
(defservlet adv-search :page adv-search :Entry t)
; SIGNATURES AND SCHEMA
(defsignature list-item (item :arity 4))
(defschema list-item :signature list-item)

(defsignature advsearch titdesc keywords closed category lowprice highprice buyitnow buyitnowonly ending)
(defschema advsearch :signature advsearch)

;GUARDS 
(defguard adv-search-basic
)
; list all categorie
;(defupdate fetch-by-categories(:language posneg)
; (<= (pos (category.id ?x)) (db.auction.category ?y ?x)) 
;)
(defguard list-category-basic
  ;how to prevent empty selection?
  ;dynamic list
  (=>  (category.id ?id) (or (= ?id "cat1") (= ?id "cat2")))
  
)

;PAGES
(defhtml adv-search header side-panel adv-search-page footer)
(defhtml adv-search-page ("adv-search.html" :forms (("adv-search" adv-search)))) 

(defhtml auction-listing header side-panel auction-listing-page footer)
(defhtml auction-listing-page ("auction-listing.html" :tables(("list" auction-listing-table)))) 

;why baby-search form doesn't appear after submitting the same form? --landing page is the same
(defhtml side-panel ("side-panel.html" :forms (("baby-search" baby-search) ("categories" categories))));

;FORM & TABLES
(deftable auction-listing-table :schema list-item)
(defform categories :schema category :target search-by-categ :guards(list-category-basic))
(defform baby-search :schema babysearch :target search)
(defform adv-search :schema advsearch :target adv-search-results :guards (adv-search-basic))


;UPDATES

;list your auction
(defupdate fetch-your-auctions (:language posneg)

  (<= (pos (list-item.item ?user ?title ?category ?quantity)) 
  	(session.username ?user) (db.auction.owner ?auc ?user) 
  	(db.auction.title ?auc ?title)
  	(db.auction.category ?auc ?category)
  	(db.auction.quantity ?auc ?quantity))
  ;...other fields

)

(defupdate search-by-categ(:language posneg)
 (<= (pos (item.id ?auc)) (db.auction.category ?auc ?k) (category.id ?k)) 
)

;search auction by basic keywords
(defupdate search-auctions(:language posneg)
	;keywords similarity LIKE?
	(<= (pos (item.id ?auc)) (db.auction.title ?auc ?k)(babysearch.keywords ?k)) 
	(<= (pos (item.id ?auc)) (db.auction.category ?auc ?k) (babysearch.keywords ?k)) 
  ;other fields
)
; search->item
(defupdate fetch-auctions(:language posneg)
	(<= (pos (list-item.item ?title ?category ?quantity)) (item.id ?id)
    (db.auction.title ?id ?title)
    (db.auction.category ?id ?category)
    (db.auction.quantity ?id ?quantity))
    ;other fields
)
; adv-search -> item
(defupdate adv-search-auctions0(:language posneg)
  (<= (pos (item.id ?auc)) (db.auction.id ?auc)) 
)

(defupdate adv-search-auctions1(:language posneg)
  (<= (neg (item.id ?auc)) (db.auction.title ?auc ?k)(advsearch.titdesc ?x) (not (= ?x ?k))) 
)

(defupdate adv-search-auctions2(:language posneg)
  (<= (neg (item.id ?auc)) (db.auction.category ?auc ?k)(advsearch.category ?x) (not (= ?x ?k))) 
)
(defupdate adv-search-auctions3(:language posneg)
  (<= (neg (item.id ?auc)) (db.auction.category ?auc ?k)(advsearch.category ?x) (not (= ?x ?k))) 
)


