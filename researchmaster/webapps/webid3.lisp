(load (loadfn "daniele"))
(load (loadfn "gabriele"))

; http://herge.sisl.rites.uic.edu/notamper/WeBid
; user: user01, pass01
;;;;;;;;; Using <= for datalog and everything else for FOL

(reset-weblog)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;; Schemas ;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; signature is a collection of symbols possibly with types and arities (default arity is 1, default type is a cross-product of strings)
;   e.g. (defsignature author (id :type number) (name :arity 2 :type (number string)) (address :arity 2 :type (number string)))
; schema is a signature with a number of guards

; Special case: a HTML form's signature is one where all arities are 1.
; Special case: a HTML table's signature is one in triple format (see below).

; Each signature3 is schema with 1 unary relation (the first one listed) and some number of binary relations (those that remain).
;   Such a signature represents a single DB table in triples format.

; Syntax: Period can only be used in names of signatures/schemas -- not in predicates of those schemas.  
;    More precisely, predicates must be valid JS and HTML identifiers (safe to use alphanum and _)
;    In logic, reference via <signaturename>.<predicate>

(defsignature db (nextfreeauctionid :type integer))

(defsignature3 db.user id name pass newsletter regdate birthmonth birthday birthyear address city state zip country telephone) 
(defschema db.user :signature db.user :guards (db.user))

(defsignature3 status msg)
(defschema status :signature status)

(defsignature login id pass)
(defschema login :signature login)

(defsignature profile username name pass pass2 birthmonth birthday birthyear address city state country zip telephone newsletter accttype)
(defschema profile :signature profile)

(defsignature search titdesc keywords closed category lowprice highprice buyitnow buyitnowonly ending)
(defschema search :signature search)

(defsignature paper (id :type integer) title (author :type (string string string)))
(defsignature3 db.paper id title (author :type (integer string string string)))
(defschema paper :signature paper)
(defschema db.paper :signature db.paper)

; note that the type of DESCRIPTION is now inferred from (i) db.auction.DESCRIPTION and (ii) updates that store this description in db.auction.description
(defsignature item id category title subtitle description type quantity startprice shippingfee reserveprice buynow bidinc startdate_day startdate_month startdate_year startdate_time duration shipping_conditions shipping_international shipping_terms payment options relists)
(defschema item :signature item :guards (item))
(defsignature itemid id)
(defschema itemid :signature itemid)

(defsignature3 db.auction (id :type integer) owner category title subtitle (description :type (integer html)) type (quantity :type (integer integer)) startprice shippingfee reserveprice buynow bidinc startdate_day startdate_month startdate_year startdate_time duration shipping_conditions shipping_international shipping_terms payment options relists)
(defschema db.auction :signature db.auction :guards (item))

(defsignature babysearch keywords)
(defschema babysearch :signature babysearch)

(defsignature3 category id)
(defschema category :signature category)

(defsignature3 news msg date)
(defschema news :signature news)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;; Guards ;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; each guard is a datalog theory (or a combination of theories)

(defguard paper
   (=> (paper.id ?x) (paper.id ?y) (= ?x ?y))
   (=> (paper.title ?x) (paper.title ?y) (= ?x ?y))
   (=> (paper.author ?x1 ?x2 ?x3) (paper.author ?y1 ?y2 ?y3) (and (= ?x1 ?y1) (= ?x2 ?y2) (= ?x3 ?y3)))
)

(defguard item
    (=> (item.type ?x) (or (= ?x "standard") (= ?x "dutch"))) "Auctions must be Standard or Dutch"
    (=> (item.duration ?x) (or (= ?x "1 day") (= ?x "2 days") (= ?x "1 week") (= ?x "2 weeks") (= ?x "1 month")))
    (=> (item.shipping_conditions ?x) (or (= ?x "buyerpays") (= ?x "sellerpays")))
    (=> (item.payment ?x) (or (= ?x "wire") (= ?x "check")))
    (=> (item.options ?x) (or (= ?x "featured") (= ?x "bold") (= ?x "highlighted")))
    (=> (item.relists ?x) (or (= ?x "0") (= ?x "1") (= ?x "2") (= ?x "3") (= ?x "4")))

  (=> (item.id ?x) (item.id ?y) (= ?x ?y))
  (=> (ITEM.CATEGORY ?X) (ITEM.CATEGORY ?Y) (= ?X ?Y)) 
  (=> (ITEM.TITLE ?X) (ITEM.TITLE ?Y) (= ?X ?Y)) 
  (=> (ITEM.SUBTITLE ?X) (ITEM.SUBTITLE ?Y) (= ?X ?Y)) 
  (=> (ITEM.DESCRIPTION ?X) (ITEM.DESCRIPTION ?Y) (= ?X ?Y)) 
  (=> (ITEM.TYPE ?X) (ITEM.TYPE ?Y) (= ?X ?Y)) 
  (=> (ITEM.QUANTITY ?X) (ITEM.QUANTITY ?Y) (= ?X ?Y)) 
  (=> (ITEM.STARTPRICE ?X) (ITEM.STARTPRICE ?Y) (= ?X ?Y)) 
  (=> (ITEM.SHIPPINGFEE ?X) (ITEM.SHIPPINGFEE ?Y) (= ?X ?Y)) 
  (=> (ITEM.RESERVEPRICE ?X) (ITEM.RESERVEPRICE ?Y) (= ?X ?Y)) 
  (=> (ITEM.BUYNOW ?X) (ITEM.BUYNOW ?Y) (= ?X ?Y)) 
  (=> (ITEM.BIDINC ?X) (ITEM.BIDINC ?Y) (= ?X ?Y)) 
  (=> (ITEM.STARTDATE_DAY ?X) (ITEM.STARTDATE_DAY ?Y) (= ?X ?Y)) 
  (=> (ITEM.STARTDATE_MONTH ?X) (ITEM.STARTDATE_MONTH ?Y) (= ?X ?Y)) 
  (=> (ITEM.STARTDATE_YEAR ?X) (ITEM.STARTDATE_YEAR ?Y) (= ?X ?Y)) 
  (=> (ITEM.STARTDATE_TIME ?X) (ITEM.STARTDATE_TIME ?Y) (= ?X ?Y)) 
  (=> (ITEM.DURATION ?X) (ITEM.DURATION ?Y) (= ?X ?Y)) 
  (=> (ITEM.SHIPPING_CONDITIONS ?X) (ITEM.SHIPPING_CONDITIONS ?Y) (= ?X ?Y)) 
  (=> (ITEM.SHIPPING_INTERNATIONAL ?X) (ITEM.SHIPPING_INTERNATIONAL ?Y) (= ?X ?Y)) 
  (=> (ITEM.SHIPPING_TERMS ?X) (ITEM.SHIPPING_TERMS ?Y) (= ?X ?Y)) 
  (=> (ITEM.RELISTS ?X) (ITEM.RELISTS ?Y) (= ?X ?Y))    

)

(defguard profile-entry
  (=> (profile.country ?x) (or (= ?x "United States") (= ?x "United Kingdom")))
  (=> (PROFILE.USERNAME ?X) (PROFILE.USERNAME ?Y) (= ?X ?Y)) 
  (=> (PROFILE.NAME ?X) (PROFILE.NAME ?Y) (= ?X ?Y)) 
  (=> (PROFILE.PASS ?X) (PROFILE.PASS ?Y) (= ?X ?Y)) 
  (=> (PROFILE.PASS2 ?X) (PROFILE.PASS2 ?Y) (= ?X ?Y)) 
  (=> (PROFILE.BIRTHMONTH ?X) (PROFILE.BIRTHMONTH ?Y) (= ?X ?Y)) 
  (=> (PROFILE.BIRTHDAY ?X) (PROFILE.BIRTHDAY ?Y) (= ?X ?Y)) 
  (=> (PROFILE.BIRTHYEAR ?X) (PROFILE.BIRTHYEAR ?Y) (= ?X ?Y)) 
  (=> (PROFILE.ADDRESS ?X) (PROFILE.ADDRESS ?Y) (= ?X ?Y)) 
  (=> (PROFILE.CITY ?X) (PROFILE.CITY ?Y) (= ?X ?Y)) 
  (=> (PROFILE.STATE ?X) (PROFILE.STATE ?Y) (= ?X ?Y)) 
  (=> (PROFILE.COUNTRY ?X) (PROFILE.COUNTRY ?Y) (= ?X ?Y)) 
  (=> (PROFILE.ZIP ?X) (PROFILE.ZIP ?Y) (= ?X ?Y)) 
  (=> (PROFILE.TELEPHONE ?X) (PROFILE.TELEPHONE ?Y) (= ?X ?Y)) 
  (=> (PROFILE.NEWSLETTER ?X) (PROFILE.NEWSLETTER ?Y) (= ?X ?Y)) 
  (=> (PROFILE.ACCTTYPE ?X) (PROFILE.ACCTTYPE ?Y) (= ?X ?Y)) 
)

(defguard profile-uniqueness 
  (=> (profile.username ?x) (not (db.user ?x))) "Username already appears in database"
)

(defguard profile-basic
  (=> (profile.pass2 ?x) (profile.pass ?y) (= ?x ?y))   "Passwords must be the same"  ; (exists ?y (and (profile.pass ?y) (= ?x ?y))))
  (=> (profile.accttype ?accttype) (or (= ?accttype "buyer") (= ?accttype "buyertoseller"))) "Account type must be either buyer or buyertoseller"
)

(defguard auction-owner
    (=> (not (item.id "")) (item.id ?x) (db.auction.owner ?x ?y)  (session.username ?y))   "Must own an auction to edit it"
) 

(defguard profile-loggedin
  (<=> (profile.username ?x) (session.username ?x)) "Must be logged in"
)

(defguard loggedin
    (exists ?x (session.username ?x))  "Must be logged in"
)

(defguard search-basic
  (=> (search.category ?x) (db.category ?x))
  (=> (search.country ?x) (db.country ?x)))

(defguard login-entry
  (=> (login.id ?x) (login.id ?y) (= ?x ?y)) "Can only provide 1 login ID"
  (=> (login.pass ?x) (login.pass ?y) (= ?x ?y)) "Can only provide 1 password"
)

(defguard login-basic 
  (exists ?x (login.id ?x))   "Must provide login ID"
  (exists ?y (login.pass ?y))  "Must provide login password"
  (=> (login.id ?id) (login.pass ?pass) (db.user.pass ?id ?pass)) "Login incorrect"
  (=> (login.id ?x) (login.id ?y) (= ?x ?y))  "Can only provide 1 login ID"
  (=> (login.pass ?x) (login.pass ?y) (= ?x ?y))  "Can only provide 1 password"
)

(defguard category 
  (=> (category.id ?x) (db.category ?x))
  (=> (category.id ?x) (category.id ?y) (= ?x ?y)))

(defguard topcategory
  (category.id 0)
  (=> (category.id ?x) (category.id ?y) (= ?x ?y)))

(defguard auctionid
    (=> (itemid.id ?x) (itemid.id ?y) (= ?x ?y))
    (=> (itemid.id ?x) (db.auction.id ?x)))

;(defguard db :inherits (db.user))

(defguard db.user :inherits (db.user-basic db.user-misc))

(defguard db.user-basic
  (=> (db.user ?x) (exists (db.name ?x ?y)))
  (=> (db.user ?x) (exists (db.pass ?x ?y)))
  (=> (db.user ?x) (exists (db.newsletter ?x ?y)))
  (=> (db.user ?x) (exists (db.regdate ?x ?y))))

(defguard password  ; not inserted into DB since we store the hash of the PWD in the DB.
  (=> (pass ?pass) (gte (len ?pass) 6)))

(defguard db.user-misc
  (=> (db.user.email ?user ?email) (in ?email "[0-9a-zA-Z]@[0-9a-zA-Z\\.]"))
  (=> (db.user.telephone ?user ?telephone) (in ?telephone "\d\d\d\-\d\d\d-\d\d\d\d"))
  (=> (db.user.accttype ?user ?acct)
      (or (= ?acct seller)
  	  (= ?acct buyer)
	  (= ?acct buyertoseller)
	  (= ?acct unique)))

  (=> (db.user.newsletter ?user ?x) (or (= ?x true) (= ?x false)))
)


#| NEED TO THINK THROUGH BUILTINS
  (=> (db.user.birthmonth ?user ?m)
      (db.user.birthday ?user ?d)
      (db.user.birthyear ?user ?y)
      (validdate ?m ?d ?y))

  (<= (validdate ?m ?d ?y)
       (and (posint ?m)
  	    (posint ?d)
	    (posint ?y)
	    (monthday ?m ?days)
	    (lte 1 ?d)
	    (lte ?d ?days)
	    (leapyear ?m ?d ?y)))
  (<= (leapyear ?m ?d ?y)
      (=> (= ?m 2) 
	  (= ?d 29)
	  (and (= (mod ?y 4) 0)
	       (or
		(not (= (mod ?y 100) 0))
		(= (mod ?y 400) 0)))))

  (monthday 1 31)
  (monthday 2 29)
  (monthday 3 31)
  (monthday 4 30)
  (monthday 5 31)
  (monthday 6 30)
  (monthday 7 31)
  (monthday 8 31)
  (monthday 9 30)
  (monthday 10 31)
  (monthday 11 30)
  (monthday 12 31)
)
|#


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;; Initial Database ;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defdb
  ; DO NOT DELETE THIS
  (db.nextfreeauctionid 2)

  (db.user "thinrich")
  (db.user.name "thinrich" "Tim")
  (db.user.pass "thinrich" "pass")
  (db.user.birthmonth "thinrich" "May")
  (db.user.birthday "thinrich" "18")
  (db.user.birthyear "thinrich" "1979")
  (db.user.address "thinrich" "123 Here")
  (db.user.city "thinrich" "Dixon")
  (db.user.state "thinrich" "IL")
  (db.user.country "thinrich" "United States")
  (db.user.zip "thinrich" "60637")
  (db.user.telephone "thinrich" "123-456-7890")
  (db.user.newsletter "thinrich" "true")
  (db.user.accttype "thinrich" "buyer")

  (db.auction.id 1)
  (db.auction.owner 1 "thinrich")
  (db.auction.category 1 "")
  (db.auction.title 1 "My auction")
  (db.auction.subtitle 1 "")
  (db.auction.description 1 "")
  (db.auction.type 1 "standard")
  (db.auction.quantity 1 1)
  (db.auction.startprice 1 "")
  (db.auction.shippingfee 1 "")
  (db.auction.reserveprice 1 "")
  (db.auction.buynow 1 "")
  (db.auction.bidinc 1 "")
  (db.auction.startdate_day 1 "")
  (db.auction.startdate_month 1 "")
  (db.auction.startdate_year 1 "")
  (db.auction.startdate_time 1 "")
  (db.auction.duration 1 "undefined")
  (db.auction.shipping_conditions 1 "undefined")
  (db.auction.shipping_international 1 "")
  (db.auction.shipping_terms 1 "")
  (db.auction.relists 1 "undefined")
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;; DB Updates ;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defupdate genpaperid (:language posneg)
  (<= (pos (paper.id ?x)) (db.nextfreeauctionid ?x))
  (<= (neg (paper.id ?x)) (paper.id ?x))
  (<= (pos (db.nextfreeauctionid ?y)) (db.nextfreeauctionid ?x) (+ ?x 1 ?y))
  (<= (neg (db.nextfreeauctionid ?x)) (db.nextfreeauctionid ?x)))

(defupdate savepaper (:language posneg)
  (<= (pos (db.paper.id ?x)) (paper.id ?x))
  (<= (pos (db.paper.title ?x ?y)) (paper.id ?x) (paper.title ?y))
  (<= (pos (db.paper.author ?w ?x ?y ?z)) (paper.id ?w) (paper.author ?x ?y ?z)))

(defupdate lookuppaper (:language posneg)
  (<= (pos (paper.title ?x ?y)) (paper.id ?x) (db.paper.title ?x ?y))
  (<= (pos (paper.author ?x ?y ?z)) (paper.id ?x) (db.paper.author ?x ?y ?z ?w)))

(defupdate genitemid (:language posneg)
  (<= (pos (item.id ?x)) (db.nextfreeauctionid ?x))
  (<= (neg (item.id ?x)) (item.id ?x))
  (<= (pos (db.nextfreeauctionid ?y)) (db.nextfreeauctionid ?x) (+ ?x 1 ?y))
  (<= (neg (db.nextfreeauctionid ?x)) (db.nextfreeauctionid ?x)))

(defupdate saveauction (:language posneg)

  ; Add the new and delete the old (pos overrides neg)
  (<= (pos (db.auction.id ?x)) (item.id ?x))
  (<= (pos (db.auction.owner ?x ?y)) (item.id ?x) (session.username ?y))
  (<= (pos (db.auction.category ?x ?y)) (item.id ?x) (ITEM.CATEGORY ?y))
  (<= (pos (db.auction.title ?x ?y)) (item.id ?x) (item.title ?y))
  (<= (pos (db.auction.subtitle ?x ?y)) (item.id ?x) (item.subtitle ?y))
  (<= (pos (db.auction.description ?x ?y)) (item.id ?x) (item.description ?y))
  (<= (pos (db.auction.type ?x ?y)) (item.id ?x) (item.type ?y))
  (<= (pos (db.auction.quantity ?x ?y)) (item.id ?x) (item.quantity ?y))
  (<= (pos (db.auction.startprice ?x ?y)) (item.id ?x) (item.startprice ?y))
  (<= (pos (db.auction.shippingfee ?x ?y)) (item.id ?x) (item.shippingfee ?y))
  (<= (pos (db.auction.reserveprice ?x ?y)) (item.id ?x) (item.reserveprice ?y))
  (<= (pos (db.auction.buynow ?x ?y)) (item.id ?x) (item.buynow ?y))
  (<= (pos (db.auction.bidinc ?x ?y)) (item.id ?x) (item.bidinc ?y))
  (<= (pos (db.auction.startdate_day ?x ?y)) (item.id ?x) (item.startdate_day ?y))
  (<= (pos (db.auction.startdate_month ?x ?y)) (item.id ?x) (item.startdate_month ?y))
  (<= (pos (db.auction.startdate_year ?x ?y)) (item.id ?x) (item.startdate_year ?y))
  (<= (pos (db.auction.startdate_time ?x ?y)) (item.id ?x) (item.startdate_time ?y))
  (<= (pos (db.auction.duration ?x ?y)) (item.id ?x) (item.duration ?y))
  (<= (pos (db.auction.shipping_conditions ?x ?y)) (item.id ?x) (item.shipping_conditions ?y))
  (<= (pos (db.auction.shipping_international ?x ?y)) (item.id ?x) (item.shipping_international ?y))
  (<= (pos (db.auction.shipping_terms ?x ?y)) (item.id ?x) (item.shipping_terms ?y))
  (<= (pos (db.auction.relists ?x ?y)) (item.id ?x) (item.relists ?y))

  (<= (neg (db.auction.id ?x)) (item.id ?x))
  (<= (neg (db.auction.owner ?x ?y)) (item.id ?x) (db.auction.owner ?x ?y))
  (<= (neg (db.auction.category ?x ?y)) (item.id ?x) (db.auction.category ?x ?y))
  (<= (neg (db.auction.title ?x ?y)) (item.id ?x) (db.auction.title ?x ?y))
  (<= (neg (db.auction.subtitle ?x ?y)) (item.id ?x) (db.auction.subtitle ?x ?y))
  (<= (neg (db.auction.description ?x ?y)) (item.id ?x) (db.auction.description ?x ?y))
  (<= (neg (db.auction.type ?x ?y)) (item.id ?x) (db.auction.type ?x ?y))
  (<= (neg (db.auction.quantity ?x ?y)) (item.id ?x) (db.auction.quantity ?x ?y))
  (<= (neg (db.auction.startprice ?x ?y)) (item.id ?x) (db.auction.startprice ?x ?y))
  (<= (neg (db.auction.shippingfee ?x ?y)) (item.id ?x) (db.auction.shippingfee ?x ?y))
  (<= (neg (db.auction.reserveprice ?x ?y)) (item.id ?x) (db.auction.reserveprice ?x ?y))
  (<= (neg (db.auction.buynow ?x ?y)) (item.id ?x) (db.auction.buynow ?x ?y))
  (<= (neg (db.auction.bidinc ?x ?y)) (item.id ?x) (db.auction.bidinc ?x ?y))
  (<= (neg (db.auction.startdate_day ?x ?y)) (item.id ?x) (db.auction.startdate_day ?x ?y))
  (<= (neg (db.auction.startdate_month ?x ?y)) (item.id ?x) (db.auction.startdate_month ?x ?y))
  (<= (neg (db.auction.startdate_year ?x ?y)) (item.id ?x) (db.auction.startdate_year ?x ?y))
  (<= (neg (db.auction.startdate_time ?x ?y)) (item.id ?x) (db.auction.startdate_time ?x ?y))
  (<= (neg (db.auction.duration ?x ?y)) (item.id ?x) (db.auction.duration ?x ?y))
  (<= (neg (db.auction.shipping_conditions ?x ?y)) (item.id ?x) (db.auction.shipping_conditions ?x ?y))
  (<= (neg (db.auction.shipping_international ?x ?y)) (item.id ?x) (db.auction.shipping_international ?x ?y))
  (<= (neg (db.auction.shipping_terms ?x ?y)) (item.id ?x) (db.auction.shipping_terms ?x ?y))
  (<= (neg (db.auction.relists ?x ?y)) (item.id ?x) (db.auction.relists ?x ?y))

)

(defupdate lookupauction (:language posneg)
  (<= (pos (item.category ?y)) (item.id ?x) (db.auction.CATEGORY ?x ?y))
  (<= (pos (item.title ?y)) (item.id ?x) (db.auction.title ?x ?y))
  (<= (pos (item.subtitle ?y)) (item.id ?x) (db.auction.subtitle ?x ?y))
  (<= (pos (item.description ?y)) (item.id ?x) (db.auction.description ?x ?y))
  (<= (pos (item.type ?y)) (item.id ?x) (db.auction.type ?x ?y))
  (<= (pos (item.quantity ?y)) (item.id ?x) (db.auction.quantity ?x ?y))
  (<= (pos (item.startprice ?y)) (item.id ?x) (db.auction.startprice ?x ?y))
  (<= (pos (item.shippingfee ?y)) (item.id ?x) (db.auction.shippingfee ?x ?y))
  (<= (pos (item.reserveprice ?y)) (item.id ?x) (db.auction.reserveprice ?x ?y))
  (<= (pos (item.buynow ?y)) (item.id ?x) (db.auction.buynow ?x ?y))
  (<= (pos (item.bidinc ?y)) (item.id ?x) (db.auction.bidinc ?x ?y))
  (<= (pos (item.startdate_day ?y)) (item.id ?x) (db.auction.startdate_day ?x ?y))
  (<= (pos (item.startdate_month ?y)) (item.id ?x) (db.auction.startdate_month ?x ?y))
  (<= (pos (item.startdate_year ?y)) (item.id ?x) (db.auction.startdate_year ?x ?y))
  (<= (pos (item.startdate_time ?y)) (item.id ?x) (db.auction.startdate_time ?x ?y))
  (<= (pos (item.duration ?y)) (item.id ?x) (db.auction.duration ?x ?y))
  (<= (pos (item.shipping_conditions ?y)) (item.id ?x) (db.auction.shipping_conditions ?x ?y))
  (<= (pos (item.shipping_international ?y)) (item.id ?x) (db.auction.shipping_international ?x ?y))
  (<= (pos (item.shipping_terms ?y)) (item.id ?x) (db.auction.shipping_terms ?x ?y))
  (<= (pos (item.relists ?y)) (item.id ?x) (db.auction.relists ?x ?y))
)

(defupdate saveprofile (:language posneg)

  ; delete entire old profile and add entire new profile: compiler should optimize
  (<= (pos (db.user ?x)) (profile.username ?x))
  (<= (pos (db.user.name ?x ?y)) (and (profile.username ?x) (profile.name ?y)) )

  (<= (pos (db.user.pass ?x ?y)) (and (profile.username ?x) (profile.pass ?y)) )
  (<= (pos (db.user.birthmonth ?x ?y)) (and (profile.username ?x) (profile.birthmonth ?y)) )
  (<= (pos (db.user.birthday ?x ?y)) (and (profile.username ?x) (profile.birthday ?y)) )
  (<= (pos (db.user.birthyear ?x ?y)) (and (profile.username ?x) (profile.birthyear ?y)) )
  (<= (pos (db.user.address ?x ?y)) (and (profile.username ?x) (profile.address ?y)) )
  (<= (pos (db.user.city ?x ?y)) (and (profile.username ?x) (profile.city ?y)) )
  (<= (pos (db.user.state ?x ?y)) (and (profile.username ?x) (profile.state ?y)) )
  (<= (pos (db.user.country ?x ?y)) (and (profile.username ?x) (profile.country ?y)) )
  (<= (pos (db.user.zip ?x ?y)) (and (profile.username ?x) (profile.zip ?y)) )
  (<= (pos (db.user.telephone ?x ?y)) (and (profile.username ?x) (profile.telephone ?y)) )
  (<= (pos (db.user.newsletter ?x ?y)) (and (profile.username ?x) (profile.newsletter ?y)) )
  (<= (pos (db.user.accttype ?x ?y)) (and (profile.username ?x) (profile.accttype ?y)) )
  ;(<= (db.user.regdate ?x ?now) (profile.username ?x) (builtin.now ?now) )

  (<= (neg (db.user ?x)) (profile.username ?x))
  (<= (neg (db.user.name ?x ?y)) (and (profile.username ?x) (db.user.name ?x ?y)) )

  (<= (neg (db.user.pass ?x ?y)) (and (profile.username ?x) (db.user.pass ?x ?y)) )
  (<= (neg (db.user.birthmonth ?x ?y)) (and (profile.username ?x) (db.user.birthmonth ?x ?y)) )
  (<= (neg (db.user.birthday ?x ?y)) (and (profile.username ?x) (db.user.birthday ?x ?y)) )
  (<= (neg (db.user.birthyear ?x ?y)) (and (profile.username ?x) (db.user.birthyear ?x ?y)) )
  (<= (neg (db.user.address ?x ?y)) (and (profile.username ?x) (db.user.address ?x ?y)) )
  (<= (neg (db.user.city ?x ?y)) (and (profile.username ?x) (db.user.city ?x ?y)))
  (<= (neg (db.user.state ?x ?y)) (and (profile.username ?x) (db.user.state ?x ?y)) )
  (<= (neg (db.user.country ?x ?y)) (and (profile.username ?x) (db.user.country ?x ?y)) )
  (<= (neg (db.user.zip ?x ?y)) (and (profile.username ?x) (db.user.zip ?x ?y)) )
  (<= (neg (db.user.telephone ?x ?y)) (and (profile.username ?x) (db.user.telephone ?x ?y)) )
  (<= (neg (db.user.newsletter ?x ?y)) (and (profile.username ?x) (db.user.newsletter ?x ?y)) )
  (<= (neg (db.user.accttype ?x ?y)) (and (profile.username ?x) (db.user.accttype ?x ?y)) )

    
#|  Notice that the following is much easier to write than the above.
  (=> (profile.username ?x) (db.user ?x))
  (=> (and (profile.username ?x) (profile.name ?y)) (db.user.name ?x ?y))
  (=> (and (profile.username ?x) (profile.pass ?y)) (db.user.pass ?x ?y))
  (=> (and (profile.username ?x) (profile.birthmonth ?y)) (db.user.birthmonth ?x ?y))
  (=> (and (profile.username ?x) (profile.birthday ?y)) (db.user.birthday ?x ?y))
  (=> (and (profile.username ?x) (profile.birthyear ?y)) (db.user.birthyear ?x ?y))
  (=> (and (profile.username ?x) (profile.address ?y)) (db.user.address ?x ?y))
  (=> (and (profile.username ?x) (profile.city ?y)) (db.user.city ?x ?y))
  (=> (and (profile.username ?x) (profile.state ?y)) (db.user.state ?x ?y))
  (=> (and (profile.username ?x) (profile.country ?y)) (db.user.country ?x ?y))
  (=> (and (profile.username ?x) (profile.zip ?y)) (db.user.zip ?x ?y))
  (=> (and (profile.username ?x) (profile.telephone ?y)) (db.user.telephone ?x ?y))
  (=> (and (profile.username ?x) (profile.newsletter ?y)) (db.user.newsletter ?x ?y))
  (=> (profile.username ?x) (builtin.now ?now) (db.user.regdate ?x ?now))

  ; data constraint resolution
  (malleable db.user*)
|#
)

; defined with constraints
; note: could combine lookupprofile and saveprofile into 1 updater with a conditioned mall1eable and <=> instead of =>
(defupdate lookupprofile ()
  (<= (pos (profile.name ?y)) (profile.username ?x) (db.user.name ?x ?y))

  (<= (pos (profile.pass ?y)) (profile.username ?x) (db.user.pass ?x ?y))
  (<= (pos (profile.birthmonth ?y)) (profile.username ?x) (db.user.birthmonth ?x ?y))
  (<= (pos (profile.birthday ?y)) (profile.username ?x) (db.user.birthday ?x ?y))
  (<= (pos (profile.birthyear ?y)) (profile.username ?x) (db.user.birthyear ?x ?y))
  (<= (pos (profile.address ?y)) (profile.username ?x)  (db.user.address ?x ?y))
  (<= (pos (profile.city ?y)) (profile.username ?x) (db.user.city ?x ?y))
  (<= (pos (profile.state ?y)) (profile.username ?x) (db.user.state ?x ?y))
  (<= (pos (profile.country ?y)) (profile.username ?x) (db.user.country ?x ?y))
  (<= (pos (profile.zip ?y)) (profile.username ?x)  (db.user.zip ?x ?y))
  (<= (pos (profile.telephone ?y)) (profile.username ?x) (db.user.telephone ?x ?y))
  (<= (pos (profile.newsletter ?y)) (profile.username ?x) (db.user.newsletter ?x ?y))
  (<= (pos (profile.accttype ?y)) (profile.username ?x) (db.user.accttype ?x ?y))

)
(defupdate session2profile ()
  (<= (pos (profile.username ?x)) (session.username ?x))
)

(defupdate statusok ()
    (<= (pos (status "ok")))
)

; described with constraints
(defupdate runsearch ()
; see March 5, 2011 for how to construct query from constraints

  ; need to think through schemas here
; output constraints (incomplete list)
  (=> (not search.titdesc) (search.keywords ?x) (auclist.id ?id) (auclist.title ?id ?y) (includes ?y ?x))
  (=> search.titdesc (search.keywords ?x) (auclist.id ?id) (auclist.description ?id ?desc) 
       (auclist.title ?id ?title) (or (includes ?desc ?x) (includes ?title ?x)))

  (=> (not search.closed) (forall ?id (=> (auclist.id ?id) (auclist.closingdate ?id ?d) (date< now ?d))))
  (=> (search.category ?x) (forall ?id (=> (auclist.id ?id) 
					   (exists ?y (and (descendent ?x ?y) (auclist.category ?y))))))
  (=> (search.lowprice ?x) (forall ?id (=> (auclist ?id) (auclist.price ?id ?p) (< ?x ?p))))
  (=> (search.highprice ?x) (forall ?id (=> (auclist.id ?id) (auclist.price ?id ?p) (< ?p ?x))))
  (=> (not search.buyitnow) (forall ?id (=> (auclist.id ?id) (not (auclist.buyitnow ?id)))))
  (=> search.buyitnowonly (forall ?id (=> (auclist.id ?id) (auclist.buyitnow ?id))))

  ; ending times: we should probably be inclusive, not require the auction to end exactly 3 days from today.
  (=> (search.ending today) (forall ?id (=> (auclist.id ?id) (auclist.closingdate ?id today))))
  (=> (search.ending 3days) (forall ?id (=> (auclist.id ?id) (auclist.closingdate ?id ?c) (date+ today 3 ?c))))

  ; ensure all tuples are from the right database query.  Put an upper limit on what's in the search output.
  (=> (auclist ?id)
      (auclist.title ?id ?title)
      (auclist.description ?id ?desc)
      (auclist.closingdate ?id ?closing)
      (auclist.category ?id ?cat)
      (auclist.price ?id ?p)
      (auclist.buyitnow ?id ?buyitnow)
      (auclist.payments ?id ?pay)
      (auclist.seller ?id ?seller)
      (auclist.country ?id ?country)
      (auclist.zip ?id ?zip)
      (auclist.ending ?id ?ending)
      (auclist.auctiontype ?id ?auctype)
      (and 
       (db.auction ?id)
       (db.auction.title ?id ?title)
       (db.auction.description ?id ?desc)
       (db.auction.closingdate ?id ?closing)
       (db.auction.category ?id ?cat)
       (db.auction.price ?id ?p)
       (db.auction.buyitnow ?id ?buyitnow)
       (db.auction.payments ?id ?pay)
       (db.auction.seller ?id ?seller)
       (db.auction.country ?id ?country)
       (db.auction.zip ?id ?zip)
       (db.auction.ending ?id ?ending)
       (db.auction.auctiontype ?id ?auctype)))

  ; sort by is a pain; it's a property across rows.  There aren't so many of those, and perhaps we can
  ;   push it to the presentation part of the spec.
  (=> (search.sortby itemsendingfirst) (ordered auclist search.output.closingdate))

; output constraint resolution
  (malleable search.output.*)
)

(defupdate logout ()
    (<= (neg (session.username ?x)) (session.username ?x))
)
(defupdate login (:guards (login-basic))
;  (=> (login.id ?id) (builtin.freshsession ?sess) (and (cookie.session ?sess) (session.id ?sess)))
;  (malleable cookie.session) 
;  (malleable session.*)
  (<= (pos (session.username ?x)) (login.id ?x))
)

(defupdate profile2login ()
   (<= (pos (login.id ?x)) (profile.username ?x))
   (<= (pos (login.pass ?x)) (profile.pass ?x))
)

(defupdate news ()
   (<= (pos (news ?x)) (db.news ?x)))

(defupdate itemid2item ()
  (<= (pos (item.id ?x)) (itemid.id ?x))
  (<= (neg (itemid.id ?x)) (itemid.id ?x))
)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;; Forms/Tables ;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; (defform <name> :schema <schemaname> :target <servletname> :constraints <constraintsname>)
; (deftable <name> :schema <schemaname>)
; Question: to use forward, we need the DB query used to populate a table; given a servlet, and a table name can we extract that query automatically?
(defform new-profile :schema profile :target register :guards (profile-basic profile-entry))  
(defform edit-profile :schema profile :target save-profile :guards (profile-basic profile-entry))   
(deftable status :schema status)
(defform login :schema login :target login :guards (login-entry))
;(defform search :schema search :target search :guards (search-basic))
(defform edit-auction :schema item :target save-auction)
(defform choose-auction :schema itemid :guards (auctionid) :target show-auction)

;(deftable auction :schema auction-listing)
(defform credentials :schema login :target login)
;(defform babysearch :schema babysearch :target search)
(defform category :schema categoryid :target browse-categories :guards (category))
;(defform topcategory :schema categoryid :target browse-categories :guards (topcategory))
(deftable news :schema news)
(deftable auction :schema item)

(defform paper :schema paper :guards (paper) :target save-paper)
; (a) we should be able to derive profile-basic based on the :target.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;; Servlets ;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; (defservlet <name> :forms <list of forms> :tables <list of tables> :actions <list of updates> :display <htmlpage>
; A servlet always replaces the forms and tables listed with generated code (for forms the schema dictates which fields appear, for tables the columns)
; To compute initial data for those forms and tables, the servlet runs the list of actions and populates the forms/tables with results
; Each form in the list is <form> :target <servlet> where :target <servlet> overrides the default :target of <form>

; TODO: add error-handling.  Add :errorpage field that is the page to use when an error occurs; 
;    probably also want :onerror to be a list of updates to apply to the data before the :errorpage is rendered with that data.    
; TODO: allow user to interleave guards with updates arbitrarily.

; Registration creation
(defservlet new-registration :page new-profile-page :entry t)
; Registration processing: store profile, convert profile username/pwd to login schema, login, return simple page saying "Registration saved"
(defservlet register :guards (profile-uniqueness profile-basic) :updates (saveprofile profile2login logout login) :page success :entry t)
; Profile lookup for editing
(defservlet show-profile :updates (session2profile lookupprofile) :page edit-profile-page)
; Profile saving: store profile, login, repopulate profile page with profile and set status message to OK.
(defservlet save-profile :guards (profile-loggedin profile-basic) :updates (saveprofile) :page success)

(defservlet show-login :page login-page :entry t)
(defservlet login :updates (logout login) :page success :entry t)
(defservlet logout :updates (logout) :page success)

; See June 28, 2012 for notes on why we're skipping the select_category servlet
(defservlet new-auction :guards (loggedin) :page new-auction-page)
(defservlet save-auction :guards (loggedin auction-owner) :updates (genitemid saveauction) :page success)
(defservlet choose-auction :page choose-auction-page)
(defservlet show-auction :updates (itemid2item lookupauction) :page show-auction-page)

(defservlet edit-auction :guards (loggedin) :updates (lookupauction) :page new-auction-page)

(defservlet new-paper :page new-paper-page)
(defservlet save-paper :updates (genpaperid savepaper) :page success)

; Advanced search page: combine search fields and results onto single page 
;(defservlet search :guards (search-basic) :actions (runsearch) :page search :entry t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;; Pages ;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; should probably just use HTML (or at least allow for the possibility of using just HTML), at least in the writeup.

; (defhtml <name> sequence of pages or HTML files to concatenate, where each page can be followed by substitutions )

(defhtml header "header.html") ;("header.html" :forms (("search" babysearch) ("category" category))))
(defhtml footer "footer.html")

;(defhtml home header "3colsstart.html" categories empty home-extras "3colsend.html")
;(defhtml categories ("categories.html" :forms (("category" category) ("allcategories" topcategory))))
;(defhtml home-extras login news)
;(defhtml news ("news.html" :tables (("news" news news))))   ;(<tablename> <schemaname> <data>)  if <data> is nil, use global data
;(defhtml empty "empty.html")

(defhtml new-profile-page header new-profile footer)
(defhtml new-profile ("profile.html" :forms (("registration" new-profile)) :tables (("status" status))))

(defhtml login-page header login footer)
(defhtml login ("login.html" :forms (("login" login))))

(defhtml edit-profile-page header edit-profile footer)
(defhtml edit-profile ("profile.html" :forms (("registration" edit-profile)) :tables (("status" status))))

(defhtml new-auction-page header new-auction footer)
(defhtml new-auction ("profile.html" :forms (("registration" edit-auction))))

(defhtml choose-auction-page header choose-auction footer)
(defhtml choose-auction ("profile.html" :forms (("registration" choose-auction))))

(defhtml show-auction-page header show-auction footer)
(defhtml show-auction ("auction.html" :tables (("auction" auction))))

(defhtml new-paper-page header new-paper footer)
(defhtml new-paper ("profile.html" :forms (("registration" paper))))

;(defhtml search-page header search footer)
;(defhtml search ("search.html" :forms (("search" search)) :tables (("auctions" auctions))))

(defhtml success header "success.html" footer)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;; Tests ;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(deftest :servlet show-registration)
(deftest :servlet register 
    :input ((PROFILE.USERNAME "prithvi2") (PROFILE.NAME "a") (PROFILE.PASS "b") (PROFILE.PASS2 "b") (PROFILE.BIRTHMONTH "5") (PROFILE.BIRTHDAY "18") (PROFILE.BIRTHYEAR "1979") (PROFILE.ADDRESS "123 Here") (PROFILE.CITY "Chicago") (PROFILE.STATE "IL") (PROFILE.COUNTRY "USA") (PROFILE.ZIP "60637") (PROFILE.TELEPHONE "123-456-7890") (PROFILE.NEWSLETTER "true") (PROFILE.ACCTTYPE "buyer") (PROFILE.TIME "3549633634"))
    :dbchange ((db.user "prithvi2") (db.user.name "prithvi2" "a") (db.user.pass "prithvi2" "b") (db.user.birthmonth "prithvi2" "5") (db.user.birthday "prithvi2" "18") (db.user.birthyear "prithvi2" "1979") (db.user.address "prithvi2" "123 Here") (db.user.city "prithvi2" "Chicago") (db.user.state "prithvi2" "IL") (db.user.country "prithvi2" "USA") (db.user.zip "prithvi2" "60637") (db.user.telephone "prithvi2" "123-456-7890") (db.user.newsletter "prithvi2" "true") (db.user.accttype "prithvi2" "buyer"))
    :cookiechange ((__cookie.session ?x))
    :sessionchange ((session.username "prithvi2"))
)

; in temporal logic
#|
(next (and (pos (__cookie.session ?x)) (pos (session.username "prithvi2")) 
	   (pos (db.user "prithvi2")) (pos (db.user.name "prithvi2" "a")) (pos (db.user.pass "prithvi2" "b")) (pos (db.user.birthmonth "prithvi2" "5")) (pos (db.user.birthday "prithvi2" "18")) (pos (db.user.birthyear "prithvi2" "1979")) (pos (db.user.address "prithvi2" "123 Here")) (pos (db.user.city "prithvi2" "Chicago")) (pos (db.user.state "prithvi2" "IL")) (pos (db.user.country "prithvi2" "USA")) (pos (db.user.zip "prithvi2" "60637")) (pos (db.user.telephone "prithvi2" "123-456-7890")) (pos (db.user.newsletter "prithvi2" "true")) (pos (db.user.accttype "prithvi2" "buyer")))      
      (register ((PROFILE.USERNAME "prithvi2") (PROFILE.NAME "a") (PROFILE.PASS "b") (PROFILE.PASS2 "b") (PROFILE.BIRTHMONTH "5") (PROFILE.BIRTHDAY "18") (PROFILE.BIRTHYEAR "1979") (PROFILE.ADDRESS "123 Here") (PROFILE.CITY "Chicago") (PROFILE.STATE "IL") (PROFILE.COUNTRY "USA") (PROFILE.ZIP "60637") (PROFILE.TELEPHONE "123-456-7890") (PROFILE.NEWSLETTER "true") (PROFILE.ACCTTYPE "buyer") (PROFILE.TIME "3549633634")) nil))
|#

(deftest :servlet show-login)
(deftest :servlet login
  :input ((login.id "prithvi2") (login.pass "b")) :sessionchange ((session.username "prithvi2")))

(deftest :servlet show-profile
  :cookie ((__cookie.session ?x))
  :output ((PROFILE.USERNAME "prithvi2") (PROFILE.NAME "a") (PROFILE.PASS "b") (PROFILE.PASS2 "b") (PROFILE.BIRTHMONTH "5") (PROFILE.BIRTHDAY "18") (PROFILE.BIRTHYEAR "1979") (PROFILE.ADDRESS "123 Here") (PROFILE.CITY "Chicago") (PROFILE.STATE "IL") (PROFILE.COUNTRY "USA") (PROFILE.ZIP "60637") (PROFILE.TELEPHONE "123-456-7890") (PROFILE.NEWSLETTER "true") (PROFILE.ACCTTYPE "buyer") (PROFILE.TIME "3549633634"))
)

(deftest :servlet save-profile
  :cookie ((__cookie.session ?x))
  :input ((PROFILE.USERNAME "prithvi2") (PROFILE.NAME "Prithvi Bisht") (PROFILE.PASS "b") (PROFILE.PASS2 "b") (PROFILE.BIRTHMONTH "5") (PROFILE.BIRTHDAY "18") (PROFILE.BIRTHYEAR "1979") (PROFILE.ADDRESS "123 Here") (PROFILE.CITY "Chicago") (PROFILE.STATE "IL") (PROFILE.COUNTRY "USA") (PROFILE.ZIP "60637") (PROFILE.TELEPHONE "123-456-7890") (PROFILE.NEWSLETTER "true") (PROFILE.ACCTTYPE "buyer") (PROFILE.TIME ?newtime))
  :dbchange ((PROFILE.NAME "Prithvi Bisht")))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;; Access Control ;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; the profile (name, address, etc.) for user x can only be changed by user x
; the real (name, address, etc.) of user x can only be seen by user x
; the password for user x can be changed by anyone (the user gets to choose the password; anyone can 'reset' it)
;   This is interesting---there may be conditions on how the changes are made

; some fields of the User table that are maintained by the system (and cannot be set by the user)
;   registration_date, suspended, last_login

; the auctions of the site can be viewed by everyone (at least some of the properties)
; only the owner of auction x can edit x
; everyone can bid on an auction (append the contents of the bids field)

; auction fields: starts, duration, closed; closed is maintained by the database.  An access control policy
;   will deny any user from changing it, or maybe it will allow the user to change it but only to the appropriate value
; suspended is a field maintained by the app itself; only admins can change it, though the system can change it too
;   e.g. when the auction is created it is not suspended

; read rights

(defaccesscontrol 

    (<= (deny (tuple ?x ?y) read)
	(db.user.name ?x ?y)
	(session.username ?user)
	(not (= ?user ?x)))
;	(or (not (exists ?user (session.username ?user)))
;	    (exists ?user (and (session.username ?user) (not (= ?user ?x))))))
#|
    (<= (deny (tuple ?x ?y) read)
	(db.user.name ?x ?y)
	(not (exists ?user (session.username ?user))))

    (<= (allow (tuple ?x) read)
	(db.user.name ?x ?y))
    
    (<= (allow (tuple ?y) read)
	(db.user.name ?x ?y))

    (<= (allow (tuple ?user) read)
	(session.username ?user))

|#
#|
    (<= (deny (db.user.name ?x ?y) write)   ; write is insert or delete
	(db.user.name ?x ?y)
	(not (exists ?user (session.username ?user))))

    (<= (deny (db.user.name ?x ?y) write)   ; write is insert or delete
	(session.username ?user) 
	(not (= ?user ?x)))
|#
#|

    (<= (deny ?user (db.user.pass ?x ?y) read)
	(not (= ?x ?user)))

    (<= (deny ?user (db.user.newsletter ?x ?y) read)
	(not (= ?x ?user)))

    (<= (deny ?user (db.user.regdate ?x ?y) read)
	(not (= ?x ?user)))

    (<= (deny ?user (db.user.birthmonth ?x ?y) read)
	(not (= ?x ?user)))

    (<= (deny ?user (db.user.birthday ?x ?y) read)
	(not (= ?x ?user)))

    (<= (deny ?user (db.user.birthyear ?x ?y) read)
	(not (= ?x ?user)))

    (<= (deny ?user (db.user.address ?x ?y) read)
	(not (= ?x ?user)))

    (<= (deny ?user (db.user.city ?x ?y) read)
	(not (= ?x ?user)))

    (<= (deny ?user (db.user.state ?x ?y) read)
	(not (= ?x ?user)))

    (<= (deny ?user (db.user.zip ?x ?y) read)
	(not (= ?x ?user)))

    (<= (deny ?user (db.user.country ?x ?y) read)
	(not (= ?x ?user)))

    (<= (deny ?user (db.user.telephone ?x ?y) read)
	(not (= ?x ?user)))

    (<= (deny ?user (db.auction.relists ?x ?y) read)
	(db.auction.owner ?x ?z)
	(not (= ?z ?user)))
|#
)

(infer-types)
(insert-type-checking-and-coersion)

