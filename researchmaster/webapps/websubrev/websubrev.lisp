;SERVLETS
(defservlet submit-paper :page submit-paper-page)
(defservlet save-paper :updates (generate-paper-id save-paper) :page success)


; SIGNATURES AND SCHEMA
(defsignature paper (id :type integer) title (author :type (string string string)) abstract category keywords comment)
(defsignature3 db.paper id title (author :type (integer string string string)) abstract category keywords comment)
(defschema paper :signature paper)
(defschema db.paper :signature db.paper)


;GUARDS 
(defguard paper
   (=> (paper.id ?x) (paper.id ?y) (= ?x ?y))
   (=> (paper.title ?x) (paper.title ?y) (= ?x ?y))
   (=> (paper.author ?x1 ?x2 ?x3) (paper.author ?y1 ?y2 ?y3) (and (= ?x1 ?y1) (= ?x2 ?y2) (= ?x3 ?y3)))
)

;PAGES
(defhtml submit-paper-page header submit-paper footer)
(defhtml submit-paper ("wsr_submit.html" :forms (("submit" paper))))

(defhtml success header "wsr_success.html" footer)

;FORM & TABLES
(defform paper :schema paper :guards (paper) :target save-paper)



;UPDATES

(defupdate generate-paper-id (:language posneg)
  (<= (pos (paper.id ?x)) (db.nextfreeauctionid ?x))
  (<= (neg (paper.id ?x)) (paper.id ?x))
  (<= (pos (db.nextfreeauctionid ?y)) (db.nextfreeauctionid ?x) (+ ?x 1 ?y))
  (<= (neg (db.nextfreeauctionid ?x)) (db.nextfreeauctionid ?x)))

(defupdate save-paper (:language posneg)
  (<= (pos (db.paper.id ?x)) (paper.id ?x))
  (<= (pos (db.paper.title ?x ?y)) (paper.id ?x) (paper.title ?y))
  (<= (pos (db.paper.author ?w ?x ?y ?z)) (paper.id ?w) (paper.author ?x ?y ?z))
  (<= (pos (db.paper.comment ?x ?y)) (paper.id ?x) (paper.comment ?y)))

(defupdate lookuppaper (:language posneg)
  (<= (pos (paper.title ?x ?y)) (paper.id ?x) (db.paper.title ?x ?y))
  (<= (pos (paper.author ?x ?y ?z)) (paper.id ?x) (db.paper.author ?x ?y ?z ?w))
  (<= (pos (paper.comment ?x ?y)) (paper.id ?x) (db.paper.comment ?x ?y)))


