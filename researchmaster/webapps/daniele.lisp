;SERVLETS
;#how can I generate dynamic forms? *one for each category..
(defservlet auction-listing :guards (loggedin) :updates (fetch-your-auctions fetch-categories) :page auction-listing :Entry t)
(defservlet search :updates(fetch-categories search-auctions fetch-auctions) :page auction-listing :Entry t)
; SIGNATURES AND SCHEMA
(defsignature list-item (item :arity 4))
(defschema list-item :signature list-item)

;GUARDS
(defguard baby-search-basic
  ;(=> (babysearch.category ?cat) (or (= ?cat "cat1") (= ?cat "cat2"))) ""
  ;(=> (babysearch.category ?cat) (db.auction.category ?x ?y))) "category must exists" ;?????
)


;PAGES
(defhtml auction-listing header side-panel auction-listing-page footer)
(defhtml auction-listing-page ("auction-listing.html" :tables(("list" auction-listing-table)))) 
;why baby-search form doesn't appear after submitting the same form? --landing page is the same
(defhtml side-panel ("side-panel.html" :forms (("baby-search" baby-search)) :tables(("categories" categories-table))))

;FORM & TABLES
(deftable auction-listing-table :schema list-item)
(deftable categories-table :schema category)
(defform baby-search :schema babysearch :target search :guards (baby-search-basic))


;UPDATES

;list your auction
(defupdate fetch-your-auctions (:language posneg)

  (<= (pos (list-item.item ?user ?title ?category ?quantity)) 
  	(session.username ?user) (db.auction.owner ?auc ?user) 
  	(db.auction.title ?auc ?title)
  	(db.auction.category ?auc ?category)
  	(db.auction.quantity ?auc ?quantity)) ;tostring?!
  ;...other fields

)

; list all categorie
(defupdate fetch-categories(:language posneg)
	(<= (pos (category.id ?x)) (db.auction.category ?y ?x))
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
;BUILTINS



