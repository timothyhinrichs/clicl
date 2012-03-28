
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
(servlet.output register status)

; input constraints
  (=> (profile.pass ?x) (exists ?y (and (profile.pass2 ?y) (= ?x ?y))))
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
(servlet.output profile status)

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
(servlet.output search search.output)
(servlet.output search search.output.title)
(servlet.output search search.output.description)
(servlet.output search search.output.closingdate)
(servlet.output search search.output.category)
(servlet.output search search.output.price)
(servlet.output search search.output.buyitnow)
(servlet.output search search.output.payments)
(servlet.output search search.output.seller)
(servlet.output search search.output.country)
(servlet.output search search.output.zip)
(servlet.output search search.output.ending)
(servlet.output search search.output.auctiontype)

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



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;; Forms and Pages ;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; Below just written as data, but more generally each form (page) is built with a rectifier.
;   The constraints are between form.element, database entries, and potentially inputs 
;   (e.g.  the output of a servlet).  
;   This allows us to build forms whose elements are conditioned on the database/inputs
;   We want to use the rectifier to ensure that we do the right thing for parameter tampering

; need to write servlets to handle search queries
(form search)
(form.element search query)
(malleable form.element)

; need to write servlets/pages to handle browse queries
(form gobrowse)
(form.element search category.id)
(malleable form.element)

(form registration)
(form.element registration profile.name)
(form.element registration profile.accttype)
(form.element registration profile.username)
(form.element registration profile.pass)
(form.element registration profile.pass2)
(form.element registration profile.email)
(form.element registration profile.birthmonth)
(form.element registration profile.birthday)
(form.element registration profile.birthyear)
(form.element registration profile.address)
(form.element registration profile.city)
(form.element registration profile.state)
(form.element registration profile.country)
(form.element registration profile.zip)
(form.element registration profile.telephone)
(form.element registration profile.newsletter)
(malleable form.element)

; (link <sourcepage> <form> <servlet> <page> <linktext>)
(link ?x registration register displaystatus "Register")


(form category)
(form.element category category.id)
(malleable form.element)

;;;;;;;; Registration page
(page register.php)
; (page.servlet <pagename> <servletname> <list of relations/atoms> <presoname>)
(page.form register.php search)
(page.form register.php gobrowse)
(page.form register.php registration)
(malleable page.*)

;;;;;;;; Status page
(page displaystatus)
(page.element displaystatus status)
(page.element displaystatus register)
(malleable page.*)

(link ?x register empty register.php "Registration")  ; a normal link to the registration page from a page element


;;;;;;;;; Home page
(page index.php)
(page.form index.php search)
(page.form index.php gobrowse)

; For the home page, the number of forms depends on the database.  
; And the content for those forms needs to be filled in before displaying the form.  Or perhaps we can do
;  this with basic page elements and links.  Maybe we need a special link that allows us to write down
;  the data explicitly.  

(=> (db.category ?x) (not (exists ?y (db.category.parent ?x ?y))) (prettyname ?x ?pretty) 
    (and (page.element index.php (category ?x))
	 (link index.php `((category.id ,x)) category.php ?pretty)))
(malleable page.element)
(malleable link)


