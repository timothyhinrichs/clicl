
;;;;;;;;; Using <= for datalog and everything else for FOL

;;;;;;; Cookie and session

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;; Schemas ;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; signature is a collection of symbols possibly with types and arities (default arity is 2, default type is string)
; schema is a signature with a number of guards

(defsignature db db.user db.user.name db.user.pass db.user.newsletter db.user.regdate db.user.birthmonth db.user.birthday db.user.birthyear db.user.address db.user.city db.user.state db.user.zip db.user.country db.user.telephone) 
(defschema db :signature db :guards (db-uniqueness db-misc db-password))

(defsignature status (status :arity 1))
(defschema status :signature status)

(defsignature login (login.id :arity 1) login.pass)
(defschema login :signature login)

(defsignature profile (profile.username :arity 1) profile.name profile.pass profile.pass2 profile.birthmonth profile.birthday profile.birthyear profile.address profile.city profile.state profile.country profile.zip profile.telephone profile.newsletter)
(defschema profile :signature profile)

(defsignature search search.titdesc search.keywords search.closed search.category search.lowprice search.highprice search.buyitnow search.buyitnowonly search.ending)
(defschema search :signature search)

(defsignature auction-listing (auclist.id :arity 1) auclist.title aulist.description auclist.closingdate auclist.category auclist.price auclist.buyitnow auclist.payments auclist.seller auclist.country auclist.zip auclist.ending auclist.auctiontype)
(defschema auction-listing :signature auction-listing)

(defsignature babysearch search.keywords)
(defschema babysearch :signature babysearch)

(defsignature categoryid (category.id :arity 1))
(defschema categoryid :signature categoryid)

(defsignature news (news :arity 1))
(defschema new :signature news)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;; Guards ;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; each guard is a datalog theory (or a combination of theories)

(defguard profile-uniqueness 
  (=> (profile.username ?x) (not (db.user ?x))) "Username already appears in database"
)

(defguard profile-basic
  (=> (profile.pass2 ?x) (exists ?y (and (profile.pass ?y) (same ?x ?y))))
  (=> (profile.accttype ?accttype) (or (= ?accttype "buyer") (= ?accttype "buyertoseller"))))

(defguard profile-loggedin
  (=> (profile.username ?x) (exists ?y (cookie.session ?y) (session.username ?y ?x))))

(defguard search-basic
  (=> (search.category ?x) (db.category ?x))
  (=> (search.country ?x) (db.country ?x)))

(defguard login-basic 
  (=> (login.id ?id) (login.pass ?pass) (db.user.pass ?id ?pass))
  (=> (login.id ?x) (login.id ?y) (same ?x ?y))
  (=> (login.pass ?x) (login.pass ?y) (same ?x ?y)))

(defguard category 
  (=> (category.id ?x) (db.category ?x))
  (=> (category.id ?x) (category.id ?y) (= ?x ?y)))

(defguard topcategory
  (category.id 0)
  (=> (category.id ?x) (category.id ?y) (= ?x ?y)))

(defguard db :inherits (db-basic db-misc))

(defguard db-basic
  (=> (db.user ?x) (exists (db.name ?x ?y)))
  (=> (db.user ?x) (exists (db.pass ?x ?y)))
  (=> (db.user ?x) (exists (db.newsletter ?x ?y)))
  (=> (db.user ?x) (exists (db.regdate ?x ?y))))

(defguard password  ; not inserted into DB since we store the hash of the PWD in the DB.
  (=> (pass ?pass) (gte (len ?pass) 6)))

(defguard db-misc
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


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;; DB Updates ;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; defined as constraints + malleable
(defupdate saveprofile (:language posneg)

  ; dumb implementation: delete entire old profile and add entire new profile
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
  ;(<= (db.user.regdate ?x ?now) (profile.username ?x) (builtin.now ?now) )

  (<= (neg (db.user.name ?x ?y)) (and (profile.username ?x) (db.user.name ?x ?y)) )
  (<= (neg (db.user.pass ?x ?y)) (and (profile.username ?x) (db.user.pass ?x ?y)) )
  (<= (neg (db.user.birthmonth ?x ?y)) (and (profile.username ?x) (db.user.birthmonth ?x ?y)) )
  (<= (neg (db.user.birthday ?x ?y)) (and (profile.username ?x) (db.user.birthday ?x ?y)) )
  (<= (neg (db.user.birthyear ?x ?y)) (and (profile.username ?x) (db.user.birthyear ?x ?y)) )
  (<= (neg (db.user.address ?x ?y)) (and (profile.username ?x) (db.user.address ?x ?y)) )
  (<= (neg (db.user.city ?x ?y)) (and (profile.username ?x) (db.user.city ?x ?y)) )
  (<= (neg (db.user.state ?x ?y)) (and (profile.username ?x) (db.user.state ?x ?y)) )
  (<= (neg (db.user.country ?x ?y)) (and (profile.username ?x) (db.user.country ?x ?y)) )
  (<= (neg (db.user.zip ?x ?y)) (and (profile.username ?x) (db.user.zip ?x ?y)) )
  (<= (neg (db.user.telephone ?x ?y)) (and (profile.username ?x) (db.user.telephone ?x ?y)) )
  (<= (neg (db.user.newsletter ?x ?y)) (and (profile.username ?x) (db.user.newsletter ?x ?y)) )
    
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
; note: could combine lookupprofile and saveprofile into 1 updater with a conditioned malleable and <=> instead of =>
(defupdate lookupprofile ()
  ; data constraints (note these are not datalog)
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
)
(defupdate session2profile ()
  (<= (pos (profile.username ?x)) (session.user ?x))
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

(defupdate login (:guards (login-basic))
;  (=> (login.id ?id) (builtin.freshsession ?sess) (and (cookie.session ?sess) (session.id ?sess)))
;  (malleable cookie.session) 
;  (malleable session.*)

  (<= (pos (session.user ?x)) (login.id ?x))
)

(defupdate profile2login ()
   (<= (pos (login.id ?x)) (profile.username ?x))
   (<= (pos (login.pass ?x)) (profile.pass ?x))
)

(defupdate news ()
   (<= (pos (news ?x)) (db.news ?x)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;; Forms/Tables ;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; (defform <name> :schema <schemaname> :target <servletname> :constraints <constraintsname>)
; (deftable <name> :schema <schemaname>)
; Question: to use forward, we need the DB query used to populate a table; given a servlet, and a table name can we extract that query automatically?
(defform new-profile :schema profile :target register :guards (profile-basic))  
(defform edit-profile :schema profile :target edit-profile :guards (profile-basic))   
(deftable status :schema status)
(defform search :schema search :target search :guards (search-basic))
(deftable auction :schema auction-listing)
(defform credentials :schema login :target login)
(defform babysearch :schema babysearch :target search)
(defform category :schema categoryid :target browse-categories :guards (category))
(defform topcategory :schema categoryid :target browse-categories :guards (topcategory))
(deftable news :schema news)


; (a) we should be able to derive profile-basic based on the :target.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;; Servlets ;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; (defservlet <name> :forms <list of forms> :tables <list of tables> :actions <list of updates> :display <htmlpage>
; A servlet always replaces the forms and tables listed with generated code (for forms the schema dictates which fields appear, for tables the columns)
; To compute initial data for those forms and tables, the servlet runs the list of actions and populates the forms/tables with results
; Each form in the list is <form> :target <servlet> where :target <servlet> overrides the default :target of <form>

; Registration creation
(defservlet show-registration :page new-profile :entry t)
; Registration processing: store profile, convert profile username/pwd to login schema, login, return simple page saying "Registration saved"
(defservlet register :guards (profile-uniqueness profile-basic) :updates (saveprofile profile2login login) :page success :entry t)
; Profile lookup for editing
(defservlet show-profile :updates (session2profile lookupprofile) :page edit-profile-page)
; Profile saving: store profile, login, repopulate profile page with profile and set status message to OK.
(defservlet update-profile :guards (profile-loggedin profile-basic) :updates (session2profile saveprofile statusok) :page edit-profile-page)

; Advanced search page: combine search fields and results onto single page 
;(defservlet search :guards (search-basic) :actions (runsearch) :page search :entry t)

; login servlet 
;(defservlet show-login :forms (credentials) :tables (status) :page html-login)
;(defservlet login :actions (login) :display html-success)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;; Pages ;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


; should probably just use HTML (or at least allow for the possibility of using just HTML), at least in the writeup.

; (defhtml <name> sequence of pages or HTML files to concatenate, where each page can be followed by substitutions )
(defhtml home header "3colsstart.html" categories empty home-extras "3colsend.html")
(defhtml header ("header.html" :forms (("search" babysearch) ("category" category))))
(defhtml categories ("categories.html" :forms (("category" category) ("allcategories" topcategory))))
(defhtml home-extras login news)
(defhtml login ("login.html" :forms (("login" login))))
(defhtml news ("news.html" :table (("news" news news))))   ;(<tablename> <schemaname> <data>)  if <data> is nil, use global data
(defhtml empty "empty.html")
(defhtml footer "footer.html")

(defhtml new-profile-page header new-profile footer)
(defhtml new-profile ("profile.html" :forms (("profile" . new-profile)) :tables (("status" . status))))

(defhtml edit-profile-page header edit-profile footer)
(defhtml edit-profile ("profile.html" :forms (("profile" . edit-profile)) :tables (("status" . status))))

(defhtml search-page header search footer)
(defhtml search ("search.html" :forms (("search" search)) :tables (("auctions" auctions))))

; want ALL forms to be created by us, right?  Otherwise, who knows what would happen?  So we either drop or replace all <form>s appearing in HTML (after parsing).

(defhtml success "success.html")
(defhtml lostsession "lostsession.html")
