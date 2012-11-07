(defdb
  (db.nextfreeid 2)
)

;SERVLETS
(defservlet submit-paper :page submit-paper-page)
(defservlet save-paper :updates (generate-paper-id save-paper) :page success)


; SIGNATURES AND SCHEMA
(defsignature paper (id :type integer) title (author :type (string string string)) abstract category keywords comment)
(defsignature3 db.paper id title (author :type (integer string string string)) abstract category keywords comment)
(defschema paper :signature paper)
(defschema db.paper :signature db.paper)


;GUARDS 
(defguard paper-basics
   (=> (paper.id ?x) (paper.id ?y) (= ?x ?y))
   (=> (paper.abstract ?x) (paper.abstract ?y) (= ?x ?y))
   (=> (paper.keywords ?x) (paper.keywords ?y) (= ?x ?y))
   (=> (paper.comment ?x) (paper.comment ?y) (= ?x ?y))
   (=> (paper.category ?x) (or (= ?x "foundations") (= ?x "security") (= ?x "whatever")))
   (=> (paper.category ?x) (paper.category ?y) (= ?x ?y))
   (=> (paper.title ?x) (paper.title ?y) (= ?x ?y))
   (=> (paper.author ?x1 ?x2 ?x3) (paper.author ?y1 ?y2 ?y3) (and (= ?x1 ?y1) (= ?x2 ?y2) (= ?x3 ?y3)))
   (=> (paper.author ?x1 ?x2 ?x3) (in ?x3 "[0-9a-zA-Z]@[0-9a-zA-Z\\.]"))
)

;PAGES
(defhtml submit-paper-page ("wsr_submit.html" :forms (("submit" paper-submit-form))))

(defhtml success "wsr_success.html")

;FORM & TABLES
(defform paper-submit-form :schema paper :guards (paper-basics) :target save-paper)



;UPDATES

(defupdate generate-paper-id (:language posneg)
  (<= (pos (paper.id ?x)) (db.nextfreeid ?x))
  (<= (neg (paper.id ?x)) (paper.id ?x))
  (<= (pos (db.nextfreeid ?y)) (db.nextfreeid ?x) (+ ?x 1 ?y))
  (<= (neg (db.nextfreeid ?x)) (db.nextfreeid ?x)))

(defupdate save-paper (:language posneg)
  (<= (pos (db.paper.id ?x)) (paper.id ?x))
  (<= (pos (db.paper.title ?x ?y)) (paper.id ?x) (paper.title ?y))
  (<= (pos (db.paper.abstract ?x ?y)) (paper.id ?x) (paper.abstract ?y))
  (<= (pos (db.paper.keywords ?x ?y)) (paper.id ?x) (paper.keywords ?y))
  (<= (pos (db.paper.category ?x ?y)) (paper.id ?x) (paper.category ?y))
  (<= (pos (db.paper.author ?w ?x ?y ?z)) (paper.id ?w) (paper.author ?x ?y ?z))
  (<= (pos (db.paper.comment ?x ?y)) (paper.id ?x) (paper.comment ?y)))
  


