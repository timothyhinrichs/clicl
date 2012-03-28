
;;;;;;;;; Using <= for datalog and everything else for FOL

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;; Datastores ;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; these constraints/definitions have global scope


;;;;;;; Cookie and session

; cookie and session are datastores that are handled directly by the framework
(<= (currentuser ?x) (exists ?y (cookie.session ?y) (session.username ?y ?x)))
(<= anonymous (not (exists ?x (currentuser ?x))))

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

; password lengths (here we'll assume passwords are hashed by another process)
(=> (db.user.pass ?user ?pass) (gte (len ?pass) 6))
(=> (db.user.email ?user ?email) (in ?email "[0-9a-zA-Z]@[0-9a-zA-Z\\.]"))
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


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;; Constraints, Resolutions, Rectifiers ;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defrectifier noop ())

(defconstraints profiledata-con ()
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
(defresolution profiledata-res () (malleable db.user*))
(defrectifier profiledata-rec () :constraints (profiledata-con) :resolution (profiledata-res))

(defconstraints registration-con ()
  (=> (profile.pass ?x) (exists ?y (and (profile.pass2 ?y) (= ?x ?y))))
  (=> (profile.accttype ?id ?accttype) (or (= ?accttype buyer) (= ?accttype buyertoseller))))

(defrectifier registration-rec () :constraints (registration-con))

(defconstraints status-con (status) (status "ok"))
(defresolution status-res (status) (malleable status))
(defrectifier status-rec (status) :constraints (status-con status) :resolution (status-res status))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;; Access Control ;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(=> (servlet register)
  (=> (profile.username ?x) (not (exists (db.user ?x)))))

(=> (servlet profile)
    (=> (currentuser ?user) (profile.username ?user)))

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
; output is a builtin rectifier that deletes all data except the relations given
(servlet.output register profile.status)
(servlet.sequence register (listof registration-rec profiledata-rec (status-rec profile.status)))

; missing constraint
(=> (profile.username ?x) (builtin.now ?now) (db.user.regdate ?x ?now))


; The database may have additional constraints, of course.  We don't want the programmer
;    to write these.  However, we need to be careful about how the conflict resolution
;    is inferred b/c we don't want to fix violations in the DB constraints by, e.g.
;    deleting data.  Need to think about this.



;;;;;;; Profile edit form.
(servlet profile)
(servlet.output profile profile.name)
(servlet.output profile profile.accttype)
(servlet.output profile profile.username)
(servlet.output profile profile.pass)
(servlet.output profile profile.pass2)
(servlet.output profile profile.email)
(servlet.output profile profile.birthmonth)
(servlet.output profile profile.birthday)
(servlet.output profile profile.birthyear)
(servlet.output profile profile.address)
(servlet.output profile profile.city)
(servlet.output profile profile.state)
(servlet.output profile profile.country)
(servlet.output profile profile.zip)
(servlet.output profile profile.telephone)
(servlet.output profile profile.newsletter)
(servlet.output profile profile.action)
(servlet.sequence profile (listof registration-rec))

; input constraint resolution: none


; Data constraint resolution (also same as for registration page) -- can utilize same dbupdate module
; Data constraint resolution
(malleable db.user*)


; Output constraints (again can use the same output dbupdate module as for registration)
(profile.status "ok")

; Output constraint resolution
(malleable profile.status)



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;; Admin Countries servlet
(form admin.countries)
; fields with types
(<=> (admin.countries.country ?x) (or (db.country ?x) (exists ?y (db.auction.country ?y ?x))))

; conflict resolution
(=> (currentuser ?x) (admin ?x) 
    (ifthenelse (empty admin.countries.country)
		(malleable admin.countries.country)
		(malleable db.country)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;; Admin Bid increments servlet
(form admin.bid)
; fields with types

(<=> (and (admin.bid.increments ?id)
          (admin.bid.increments.low ?id ?x)
	  (admin.bid.increments.inc ?id ?y)
	  (admin.bid.increments.hi ?id ?z))
     (and (db.bid.increments ?x ?y ?z)
	  (idfor ?x ?y ?z ?id)))    ; a builtin (DB table) that hashes xyz to an ID

; conflict resolution

(=> (currentuser ?x) (admin ?x) 
    (ifthenelse (empty admin.bid.increments)
		(malleable admin.bid.increments)
		(malleable db.bid.increments)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;; Admin send newsletter servlet: find email addresses and actually send mail.
(form admin.newsletter)
; fields

(=> (admin.newsletter.subject ?subj)
    (admin.newsletter.body ?body)
    (admin.newsletter.to ?to)
    (name2email ?to ?email)
    (db.users.email ?x ?email)
    (mail ?email ?subj ?body))

(<=> (name2email all ?email)
     (exists ?x (db.users.email ?x ?email)))

(<=> (name2email suspended ?email)
     (exists ?x (and (db.users.email ?x ?email) (db.users.status ?x 9))))

; several other cases for name2email as well.


; conflict resolution

(=> (currentuser ?x) (admin ?x) 
    (ifthenelse (empty admin.newsletter)
		(malleable admin.newsletter)
		(malleable mail)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;; advanced search servlet
; sortby fields: sortby
; where fields: seller, country, zip, enddate, auctiontype, payment, price lower, price upper, category, closed, keywords

; input schema (we include the type of each if it is a builtin)
(form search)
(form.input search search.keywords     string)
(form.input search search.titdesc      boolean)
(form.input search search.closed       boolean)
(form.input search search.category)
(form.input search search.lowprice     number)
(form.input search search.highprice    number)
(form.input search search.buyitnow     boolean)
(form.input search search.buyitnowonly boolean)
(form.input search search.paypal       boolean)
(form.input search search.wiretransfer boolean)
(form.input search search.seller       string)
(form.input search search.country)
(form.input search search.zip          string)
(form.input search search.ending       (enum today tomorrow 3days 5days))
(form.input search search.sortby       (enum itemsending newlylisted lowprices highprices)
(form.input search search.auctiontype  (enum dutchauction standardauction))
; output schema
(form.output search search.output)
(form.output search search.output.title)
(form.output search search.output.description)
(form.output search search.output.closingdate)
(form.output search search.output.category)
(form.output search search.output.price)
(form.output search search.output.buyitnow)
(form.output search search.output.payments)
(form.output search search.output.seller)
(form.output search search.output.country)
(form.output search search.output.zip)
(form.output search search.output.ending)
(form.output search search.output.auctiontype)

;;; constraints on the form's input and the database -- no conflict resolution here

; types are included in form.input above
(=> (search.category ?x) (db.category ?x))
(=> (search.country ?x) (db.country ?x))

;;; constraints that define the output in terms of the input and the database
(=> (search.keywords ?x) (forall ?id (=> (search.output ?id) (search.output.title ?id ?y) (includes ?y ?x))))
(=> search.titdesc (search.keywords ?x) 
    (forall ?id (=> (search.output ?id) (search.output.description ?id ?y) (includes ?y ?x))))
(=> (not search.closed) (forall ?id (=> (search.output ?id) (search.output.closingdate ?id ?d) (date< now ?d))))
(=> (search.category ?x) (forall ?id (=> (search.output ?id) 
					 (exists ?y (and (descendent ?x ?y) (search.output.category ?y))))))
(=> (search.lowprice ?x) (forall ?id (=> (search.output ?id) (search.output.price ?id ?p) (< ?x ?p))))
(=> (search.highprice ?x) (forall ?id (=> (search.output ?id) (search.output.price ?id ?p) (< ?p ?x))))
(=> (not search.buyitnow) (forall ?id (=> (search.output ?id) (not (search.output.buyitnow ?id)))))
(=> search.buyitnowonly (forall ?id (=> (search.output ?id) (search.output.buyitnow ?id))))
; similar for payment choice, seller, located In, zip code, auction type

; Constraints for a couple of the ending times
; Notice that we should probably be inclusive, not require the auction to end exactly 3 days from today.
(=> (search.ending today) (forall ?id (=> (search.output ?id) (search.closingdate ?id today))))
(=> (search.ending 3days) (forall ?id (=> (search.output ?id) (search.closingdate ?id ?c) (date+ today 3 ?c))))
; sort by is a pain; it's a property across rows.  There aren't so many of those, and perhaps we can
;   push it to the presentation part of the spec.
(=> (search.sortby itemsendingfirst) (ordered search.output search.output.closingdate))

; notice that all of the constraints above are constraints on a single tuple of the output table.  
; Could syntactically eliminate the universal quantifiers over ?id (and make them implicit)

; conflict resolution is probably unnecessary since the output db is always malleable (right?), 
;   but let's make it explicit for now
(malleable search.output.*)

; could include conflict resolution for the input schema (and/or db) and move it to the client 

; b) how do we extract DB queries for the code above?  Semantically, what's the search.output table we're
;    generating?




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;; Links and Access Control

; should access control be on database tables or on pages?  Ideally on data, right?

; on database
(<= (allow ?user write db.admin.*)
    (db.admin ?user))

; on pages
(<= (allow ?user admin.*)
    (db.admin ?user))

; are these interrelated?  Are both necessary?  Need to ensure consistency if we allow both