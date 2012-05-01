
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Secure by Construction Web Apps
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; 

(defstruct dataform server client datastore options name)
(defstruct servlet input output data)
; should call this dbrectify
(defstruct dbupdate (schema nil) (logic nil) (resolution nil))

(defvar *servlet-datastore* nil "the global datastore for the web application")

(defparameter *scwa-webid* 
  (make-dataform
   :name 'webid
   :server (make-servlet :input (make-dbupdate :schema '(reg.user reg.email) :logic nil :resolution nil)
			 :data (make-dbupdate :schema '(db.user.email) :logic '((<= (db.user.email ?x ?e) (and (reg.user ?x) (reg.email ?e)))) :resolution '((<= (pos (db.user.email ?x ?y)) (reg.user ?x) (reg.email ?y))))
			 :output (make-dbupdate :schema '(reg.status) :logic '((reg.status ok)) :resolution `((<= (pos (reg.status ok))))))
   :client (list (make-widget :name 'reg.user :req nil :init nil :desc "Username"
			 :style 'textbox :incundef nil :unique t :typename 'string)
		 (make-widget :name 'reg.email :req nil :init nil :desc "Email"
			 :style 'textbox :incundef nil :unique t :typename 'string)
		 (make-widget :name 'reg.status :req nil :init nil :desc "Status"
			 :style 'textbox :incundef nil :unique t :typename 'string)
		 (make-widget :name 'internaldb :req nil :init nil :desc "InternalDB"
			 :style 'textarea :incundef nil :unique t :typename 'string))
   :datastore nil
   :options nil))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Web interface
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; 

(defvar *webapp-forms* nil "Registry of all web forms for this app")

(defun webapp-register-form (dataform)
    ; ensure our main datastructure is a hash table
  (unless (hash-table-p *webapp-forms*) (setq *webapp-forms* (make-hash-table)))
  ; register this form
  (setf (gethash (dataform-name dataform) *webapp-forms*) dataform))
  
(defmethod process (s (file (eql 'scwa)) postlines)
    (let (df)
      (setq df (gethash (read-user-string (getf-post "formname" postlines)) *webapp-forms*))
      (cond ((not df) (html-websheet-problem s (format nil "Unknown dataform: ~A" df)))
	    (t (process-scwa s postlines *scwa-webid*)))))

(defun process-scwa (s postlines dataform)
  (let ((*servlet-datastore* (dataform-datastore dataform)) out postdata internal)
    (setq postdata (post2data postlines))
    (setq out (serve (dataform-server dataform) postdata))
    (cond ((eq out 'error)
	   (setq internal (format nil "~S" *servlet-datastore*))
	   (data2form (cons `(internaldb ,internal) postdata) dataform s))
	  (t
	   (setf (dataform-datastore dataform) *servlet-datastore*)
	   (setq internal (format nil "~S" *servlet-datastore*))
	   (data2form (cons `(internaldb ,internal) out) dataform s)))))

(defun post2data (postlines)
  (mapcarnot #'(lambda (x) (if (equal (car x) "dataform") nil (list (read-user-string (car x)) (cdr x)))) postlines))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Server + Client for a single form
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; 

(defun data2form (db dataform s)  ; create a plato input for the form and FIX submit so it hits infomaster properly
  "(DATA2FORM DB DATAFORM S) takes a database DB, a DATAFORM, and a stream S and outputs DB to S as
   an HTML form as dictated by DATAFORM.  ASSUMES ALL DATA IS MONADIC."
  (let (th h a)
    ; build Plato input
    (push `(formname ,(dataform-name dataform)) th)
    (push `(constraints 'nil) th)
    (push `(definitions 'nil) th)
    (push `(option completep t) th)
    (push `(option casesensitive t) th)
    (push `(option allowconflicts t) th)
    (push `(option debug t) th)
    ; Update each widget's default value slow and dumb way.  
    ;   Notice we're updating the dataform client directly
    (setq db (group-by db #'first))
    (dolist (d db)
      (mapc #'(lambda (x) (when (eq (widget-name x) (first d)) 
			    (setf (widget-init x) (cons 'listof (mapcar #'second (cdr d)))))) 
	    (dataform-client dataform)))
    ; print out each widget to the theory--hack city
    (dolist (c (dataform-client dataform))
      (setq a (make-stringbuffer))
      (format a "~S" c) 
      (push (read-user-string (subseq a 2)) th))

    ; compile webform and output to stream
    (setq h (compile-websheet th))
    (setf (htmlform-action h) "/plato/scwa?")
    (output-htmlform s h)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Server
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; 

#|
(serve
 (make-servlet :input (make-dbupdate :schema '(reg.user reg.email) :logic nil :resolution nil)
	       :data (make-dbupdate :schema '(db.user.email) :logic '((<= (db.user.email ?x ?e) (and (reg.user ?x) (reg.email ?e)))) :resolution '((<= (pos (db.user.email ?x ?y)) (reg.user ?x) (reg.email ?y))))
	       :output (make-dbupdate :schema '(reg.status) :logic '((reg.status ok)) :resolution `((<= (pos (reg.status ok))))))
 '((reg.user tim) (reg.email "tim@abc.com")))
|#

(defun serve (servlet inputdata)
  "(SERVE SERVLET INPUTDATA) applies servlet to inputdata and either returns an error
  or returns outputdata and possibly updates *servlet-datastore*"
  (let (newin newdata newout)
    ; input validation
    (setq newin (db-update (append inputdata *servlet-datastore*) (servlet-input servlet)))
    (when (eq newin 'error) (return-from serve newin))
    (setq newin (remove-if-not #'(lambda (x) (member (relation x) (dbupdate-schema (servlet-input servlet)))) newin))
    ;(format t "newin: ~A~%" newin)
    ; data processing
    (setq newdata (db-update (append newin *servlet-datastore*) (servlet-data servlet)))
    (when (eq newdata 'error) (return-from serve newdata))
    (setq newdata (remove-if-not #'(lambda (x) (member (relation x) (dbupdate-schema (servlet-data servlet)))) newdata))
    ;(format t "newdata: ~A~%" newdata)
    ; output generation
    (setq newout (db-update (append newin newdata) (servlet-output servlet)))
    (when (eq newout 'error) (return-from serve newout))
    (setq newout (remove-if-not #'(lambda (x) (member (relation x) (dbupdate-schema (servlet-output servlet)))) newout))
    ;(format t "newout: ~A~%" newout)
    ; internal database update
    (setq *servlet-datastore* newdata)
    newout))

; (db-update '((reg.user tim) (reg.email "tlh@abc.com")) nil (make-dbupdate :schema '(db.user.email) :logic '((<= (db.user.email ?x ?e) (and (reg.user ?x) (reg.email ?e)))) :resolution '((<= (pos (db.user.email ?x ?y)) (reg.user ?x) (reg.email ?y)))))

(defun db-update (db dbupdate)
  "(UPDATEDB DB DBUPDATE) applies DBUPDATE to DB and either returns an error
   or the updated DB."
  (let (newdb patches)
    (when (db-satisfies db (dbupdate-logic dbupdate)) (return-from db-update db))
    (setq patches (db-implies (append db (dbupdate-resolution dbupdate)) (dbupdate-schema dbupdate)))
    (setq newdb (db-patch db patches))
    (unless (db-satisfies newdb (dbupdate-logic dbupdate)) (return-from db-update 'error))
    newdb))

; (db-satisfies '((p a) (p b) (p c) (q c)) '((exists ?x (p ?x)) (=> (q ?x) (p ?x))))
(defun db-satisfies (data logic)
  "(DB-SATISFIES DATA LOGIC) returns T iff none of the LOGIC formulas are falsified by the database DATA."
  (let (entry datalog th univ dca)
    ; convert logic into datalog
    (setq univ 'univ)
    (setq dca (compute-dca (append data logic)))
    (setq dca (mapcar #'(lambda (x) (list univ x)) dca))
    (multiple-value-setq (entry datalog) (lloyd-topor (maksand (mapcar #'quantify logic))))
    (setq datalog (mapcar #'(lambda (x) (order-datalog-rule x :defaulttype univ)) datalog))
    (setq th (define-prologtheory (make-instance 'prologtheory) "" (append data datalog dca)))
    (viewfindp entry th)))

(defun db-patch (db patches)
  (let (pos neg)
    (multiple-value-setq (pos neg) (split #'(lambda (x) (assert (listp x)) (eq (first x) 'pos)) patches)) 
    (setq db (remove-if #'(lambda (x) (member `(neg ,x) neg :test #'equal)) db))
    (setq db (union (mapcar #'second pos) db :test #'equal))
    db))
 
; (db-implies nil nil '((<= (pos (p ?x)) (q ?x)) (q a) (q b) (<= (neg (q ?x)) (p ?x)) (p c) (p d)) '(p q)) 
(defun db-implies (datalog schema)
  "(DB-IMPLIES DB DATASTORE DATALOG) takes as input an external database DB, an internal database DATASTORE,
   a DATALOG theory, and a database SCHEMA.  Computes the updates to SCHEMA that are implied by 
   DB U Datastore U Datalog."
  (let (posqueries negqueries res)
    (setq posqueries (mapcar #'(lambda (x) `(pos (,x @x))) schema))
    (setq negqueries (mapcar #'(lambda (x) `(neg (,x @x))) schema))
    (setq res (mapcan #'(lambda (x) (viewfinds x x datalog)) (nconc posqueries negqueries)))
    res))

