
;;;;;;;;; Using <= for datalog and everything else for FOL

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;; Datastores ;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; these constraints/definitions have global scope


;;;;;;; Cookie and session

; cookie and session are datastores that are handled directly by the framework
(<=> (currentuser ?x) (exists ?y (cookie.session ?y) (session.username ?y ?x)))
(<=> anonymous (not (exists ?x (currentuser ?x))))

;;;;;;; Database schema
(datastore db.user)
(datastore db.user.name)
(datastore db.user.pass)
(datastore db.user.newsletter)
(datastore db.user.regdate)
(datastore db.user.birthmonth)
(datastore db.user.birthday)
(datastore db.user.birthyear)
(datastore db.user.address)
(datastore db.user.city)
(datastore db.user.state)
(datastore db.user.zip)
(datastore db.user.country)
(datastore db.user.telephone)

; db integrity constraints...
(=> (db.user ?x) (exists (db.name ?x ?y)))
(=> (db.user ?x) (exists (db.pass ?x ?y)))
(=> (db.user ?x) (exists (db.newsletter ?x ?y)))
(=> (db.user ?x) (exists (db.regdate ?x ?y)))

; password lengths (here we'll assume passwords are hashed by some other means)
(=> (db.user ?user) (gte (len ?user) 6))
(=> (db.user.pass ?user ?pass) (gte (len ?pass) 6))
(=> (db.user.email ?user ?email) (in ?email "[0-9a-zA-Z]@[0-9a-zA-Z\\.]"))
(=> (db.user.telephone ?user ?telephone) (in ?telephone "\d\d\d\-\d\d\d-\d\d\d\d"))

(=> (db.user.accttype ?user ?acct)
    (or (= ?acct seller)
	(= ?acct buyer)
	(= ?acct buyertoseller)
	(= ?acct unique)))

(=> (db.user.newsletter ?user ?x) (or (= ?x true) (= ?x false)))

; birthdate
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

(=> (lte ?x ?y) (number ?x) (number ?y))

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



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;; Servlets ;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;; Empty servlet
; no output schema; no constraints; no constraint resolution
; Ouptut data is always empty
(servlet empty)


;;;;;;; Registration-create form
; This form is presentation only.  We just hook up the preso module to servlet EMPTY.

;;;;;;; Registration-processing form
(servlet register)
(servlet.outputschema register status)

; input constraints
  (=> (profile.pass2 ?x) (exists ?y (and (profile.pass ?y) (= ?x ?y))))
  (=> (profile.accttype ?id ?accttype) (or (= ?accttype buyer) (= ?accttype buyertoseller)))
  (=> (profile.username ?x) (not (exists (db.user ?x))))

; input constraint resolution: none -- any errors cause inputs to be rejected
   
; data constraints 
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
  (=> (and (profile.username ?x) (profile.newsletter ?y)) (db.user.newsletter ?x ?y)))
  (=> (profile.username ?x) (builtin.now ?now) (db.user.regdate ?x ?now))

; data constraint resolution
  (malleable db.user*)

; output constraints
  (status "ok")

; output constraint resolution
  (malleable status)


;;;;;;; Profile edit form.

; shouldn't the profile servlet ignore pass2 entirely?
(servlet profile)
(servlet.outputschema profile profile-schema)
(servlet.outputschema profile status-schema)

; input constraints
  (=> (profile.pass ?x) (exists ?y (and (profile.pass2 ?y) (= ?x ?y))))
  (=> (profile.accttype ?id ?accttype) (or (= ?accttype buyer) (= ?accttype buyertoseller)))
  (=> (profile.username ?x) (exists ?y (cookie.session ?y) (session.username ?y ?x)))

; input constraint resolution: none -- any errors cause inputs to be rejected

; data constraints
  (<=> (profile.username ?x) (db.user ?x))					
  (<=> (and (profile.username ?x) (profile.name ?y)) (db.user.name ?x ?y))
  (<=> (and (profile.username ?x) (profile.pass ?y)) (db.user.pass ?x ?y))
  (<=> (and (profile.username ?x) (profile.birthmonth ?y)) (db.user.birthmonth ?x ?y))
  (<=> (and (profile.username ?x) (profile.birthday ?y)) (db.user.birthday ?x ?y))
  (<=> (and (profile.username ?x) (profile.birthyear ?y)) (db.user.birthyear ?x ?y))
  (<=> (and (profile.username ?x) (profile.address ?y)) (db.user.address ?x ?y))
  (<=> (and (profile.username ?x) (profile.city ?y)) (db.user.city ?x ?y))
  (<=> (and (profile.username ?x) (profile.state ?y)) (db.user.state ?x ?y))
  (<=> (and (profile.username ?x) (profile.country ?y)) (db.user.country ?x ?y))
  (<=> (and (profile.username ?x) (profile.zip ?y)) (db.user.zip ?x ?y))
  (<=> (and (profile.username ?x) (profile.telephone ?y)) (db.user.telephone ?x ?y))
  (<=> (and (profile.username ?x) (profile.newsletter ?y)) (db.user.newsletter ?x ?y)))

; Data constraint resolution
  (ifthenelse (empty profile) (malleable profile.*) (malleable db.user*))

; Output constraints
  (status "ok")

; Output constraint resolution
  (malleable status)


;;;;;;; Search form

(servlet search)
(servlet.outschema search auction-schema)

; input constraints
  (=> (search.category ?x) (db.category ?x))
  (=> (search.country ?x) (db.country ?x))

; input constraint resolution: none

; data constraints/constraint resolution: none

; output constraints (incomplete list)
  (=> (not search.titdesc) (search.keywords ?x) (output ?id) (output.title ?id ?y) (includes ?y ?x))
  (=> search.titdesc (search.keywords ?x) (output ?id) (output.description ?id ?desc) 
       (output.title ?id ?title) (or (includes ?desc ?x) (includes ?title ?x)))

  (=> (not search.closed) (forall ?id (=> (search.output ?id) (search.output.closingdate ?id ?d) (date< now ?d))))
  (=> (search.category ?x) (forall ?id (=> (search.output ?id) 
					   (exists ?y (and (descendent ?x ?y) (search.output.category ?y))))))
  (=> (search.lowprice ?x) (forall ?id (=> (search.output ?id) (search.output.price ?id ?p) (< ?x ?p))))
  (=> (search.highprice ?x) (forall ?id (=> (search.output ?id) (search.output.price ?id ?p) (< ?p ?x))))
  (=> (not search.buyitnow) (forall ?id (=> (search.output ?id) (not (search.output.buyitnow ?id)))))
  (=> search.buyitnowonly (forall ?id (=> (search.output ?id) (search.output.buyitnow ?id))))

  ; ending times: we should probably be inclusive, not require the auction to end exactly 3 days from today.
  (=> (search.ending today) (forall ?id (=> (search.output ?id) (search.closingdate ?id today))))
  (=> (search.ending 3days) (forall ?id (=> (search.output ?id) (search.closingdate ?id ?c) (date+ today 3 ?c))))

  ; ensure all tuples are from the right database query.  Put an upper limit on what's in the search output.
  (=> (search.output ?id)
      (search.output.title ?id ?title)
      (search.output.description ?id ?desc)
      (search.output.closingdate ?id ?closing)
      (search.output.category ?id ?cat)
      (search.output.price ?id ?p)
      (search.output.buyitnow ?id ?buyitnow)
      (search.output.payments ?id ?pay)
      (search.output.seller ?id ?seller)
      (search.output.country ?id ?country)
      (search.output.zip ?id ?zip)
      (search.output.ending ?id ?ending)
      (search.output.auctiontype ?id ?auctype)
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
  (=> (search.sortby itemsendingfirst) (ordered search.output search.output.closingdate))

; output constraint resolution
  (malleable search.output.*)



;;;;;;; Login servlet

(servlet login)
(servlet.output login.status)

; input constraints
  (=> (login.id ?id) (login.pass ?pass) (db.user.pass ?id ?pass))

; input constraint resolution: none

; data constraints
  (=> (login.id ?id) (builtin.freshsession ?sess) (and (cookie.session ?sess) (session.id ?sess)))

; data constraint resolution
  (malleable cookie.session) 
  (malleable session.*)

; output constraints
  (login.status "ok")

; output constraint resolution
  (malleable login.status)


;;;;;;; Categories servlet
; Returns all categories
(servlet grab-categories)
(servlet.output category)

; output constraints
  (<=> (category ?x) (db.category ?x))

; output constraint resolution
  (malleable category)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;; Schemas ;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(schema auction-schema)
(schema.element auction-schema auction.title)
(schema.element auction-schema auction.description)
(schema.element auction-schema auction.closingdate)
(schema.element auction-schema auction.category)
(schema.element auction-schema auction.price)
(schema.element auction-schema auction.buyitnow)
(schema.element auction-schema auction.payments)
(schema.element auction-schema auction.seller)
(schema.element auction-schema auction.country)
(schema.element auction-schema auction.zip)
(schema.element auction-schema auction.ending)
(schema.element auction-schema auction.auctiontype)

; (schema.element <schemaname> <relationname> <field1type> ... <fieldntype>)
(schema profile-schema)
(schema.element profile-schema profile.name string)
(schema.element profile-schema profile.accttype sellerbuyer)
(schema.element profile-schema profile.username string)
(schema.element profile-schema profile.pass string)
(schema.element profile-schema profile.pass2 string)
(schema.element profile-schema profile.email string)
(schema.element profile-schema profile.birthmonth string)
(schema.element profile-schema profile.birthday string)
(schema.element profile-schema profile.birthyear string)
(schema.element profile-schema profile.address string)
(schema.element profile-schema profile.city string)
(schema.element profile-schema profile.state string)
(schema.element profile-schema profile.country string)
(schema.element profile-schema profile.zip string)
(schema.element profile-schema profile.telephone string)
(schema.element profile-schema profile.newsletter boolean)

(sellerbuyer seller) 
(sellerbuyer buyer)

(schema search-schema)
(schema.element search-schema query string)

(schema categoryid-schema)
(schema.element categoryid-schema category.id category)
(<= (category ?x) (db.category ?y ?x))

(schema status-schema)
(schema.element status-schema status string)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;; Forms ;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; Below just written as data, but more generally each form (page) is built with a rectifier.
;   The constraints are between form.element, database entries, and potentially inputs 
;   (e.g.  the output of a servlet).  
;   This allows us to build forms whose elements are conditioned on the database/inputs
;   We want to use the rectifier to ensure that we do the right thing for parameter tampering

(form search)
(form.schema search search-schema)

(form gobrowse)
(form.schema gobrowse categoryid-schema)

(form registration)
(form.schema registration profile-schema)
(form.constraint registration `(<=> (profile.pass ?x) (profile.pass2 ?x)))

(form profile)
(form.schema profile profile-schema)    

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;; Pages ;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;; Header subpage
(subpage header)
(subpage.form header search find-auction auctions.php)
(subpage.subpage header browse-category)

(subpage browse-category)
(subpage.form browse-category gobrowse find-subcategories categories.php)


;;;;;;;; Registration page
(page register.php)
; (page.form <pagename> <formname> <servletname> <displaypagename>)
(page.form register.php registration register status.php)
(page.subpage register.php header)

;;;;;;;; Status page
(page status.php)
; (page.text <pagename> <schemaname>)
(page.text status.php status-schema)
(page.subpage status.php header)

;;;;;;;; Profile editing page
(page profile.php)
(page.form profile.php profile profile status.php)
(page.subpage profile.php header)

;;;;;;;; Display page for set of auctions
(page auctions.php)
(page.text auctions.php auction-schema)
(page.subpage auctions.php header)

;;;;;;;; Display page for a set of categories
(page categories.php)
(page.text categories.php categoryid-schema)
(page.subpage categories.php header)


;;;;;;;;; Home page
; For the home page, there are 2 copies of the categories form. One displays the choices
;    as a drop down, and the other displays all the choices as links.  
; Demonstrates that we may need to tie the renderer object to the the same chunk in different ways.
(page index.php)
(page.subpage index.php header)
(page.subpage index.php browse-category)
(page.form index.php login)
(page.form index.php language)
(page.form index.php help)
(page.text index.php news)





