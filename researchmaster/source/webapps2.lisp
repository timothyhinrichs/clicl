;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; To Do
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Security (parameter tampering, see journal for list)
; Error-handling (allow each guard to have its own error handling; allow errors and updates to be interleaved) 
; Workflows (e.g. show-profile should redirect to login and back to show-profile, unless already logged in)
; Update language (constraints+malleable, views+frameaxioms, etc.)
; Model Checking (testing, access control via implication, see journal for list)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Global variables
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; make sure to end with / 
(defparameter *fs-path* "/Users/thinrich/Research/code/clicl/researchmaster/")
(defparameter *web-path* "/docserver/infoserver/examples/researchmaster/")
(defparameter *component-paths* '((:app . "webapps/webid/")
				  (:html . "webapps/webid/")
				  (:compile . "webapps/webid/compiled/")))
(defun fspath (component path)
  (let ((res (find component *component-paths* :key #'car)))
    (assert res nil (format nil "Couldn't find component ~A when computing absolute path for ~A" component path))
    (stringappend *fs-path* (cdr res) path)))

(defun webpath (component path)
  (let ((res (find component *component-paths* :key #'car)))
    (assert res nil (format nil "Couldn't find component ~A when computing absolute path for ~A" component path))
    (stringappend *web-path* (cdr res) path)))


(defconstant *cookie-session-name* '__cookie.session)
(defun find-cookie-session (cookie) (viewfindx '?x `(,*cookie-session-name* ?x) cookie))
(defun drop-cookie-session (cookie) (drop `(,*cookie-session-name* ?x) cookie #'matchp) cookie)

; internal globals
(defvar *sessions* (make-hash-table))
(defvar *output-stream* t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Program Parsing
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar *signatures* (make-hash-table))
(define-condition invalid-signature (error) ((comment :initarg :comment :accessor comment)))
(define-condition unknown-signature (error) ((comment :initarg :comment :accessor comment)))
(defun find-signature (x)
  (multiple-value-bind (y present) (gethash x *signatures*)
    (unless present (error 'unknown-signature :comment x))
    y))

(defmacro defsignature (p &rest l)
  "(DEFSIGNATURE P &REST L) takes a signature name and a list of symbol declarations.
   Each symbol declaration is either a symbol or a list (symbol [:arity n] [:type thing])"
  `(define-signature ',p ',l))
  
(defun define-signature (p l)
  (let ((arity nil) (h nil) (type nil) name (sig nil))
    ; walk over the symbol declarations and turn each into a parameter
    (dolist (x l)
      (cond ((atom x) (setq name x))
	    ((listp x) 
	     (setq name (car x))
	     (setq h (grab-keyvals (cdr x) '(:arity :type) 'invalid-signature))
	     (setq arity (gethash :arity h))
	     (setq type (gethash :type h)))
	    (t (error 'invalid-signature :comment (format nil "Unknown symbol declaration type: ~A" x))))
      (unless (symbolp name) (error 'invalid-signature :comment (format nil "Unknown symbol name type: ~A" x )))
      ; make sure type is a list of things, when it is provided
      (when (and type (atom type)) (setq type (list type)))
      ; make sure reasonable values for arity and type, when provided
      (when (and arity (or (not (integerp arity)) (< arity 0)))
	(error 'invalid-signature :comment (format nil "Non-integral or non-positive arity ~A for symbol ~A" arity x)))
       (when (and type (not (every #'symbolp type)))
	(error 'invalid-signature :comment (format nil "Typename not a symbol in ~A" x)))
      ; infer arity/type if not provided
       (cond ((and (not arity) (not type)) (setq arity 1) (setq type (list 'string)))
	    ((and arity (not type)) (setq type (n-copies arity 'string)))
	    ((and (not arity) type) (setq arity (length type)))
	    (t (unless (= (length type) arity)
		 (error 'invalid-signature :comment (format nil "Type and arity incompatible in ~A" x)))))
      ; make the parameter
      (push (make-parameter :symbol name :arity arity :type type) sig))
    ; store list of parameters *in the order the programmer declared them*
    (setf (gethash p *signatures*) (nreverse sig))))

(defmacro defsignature1 (p &rest l)
  "(DEFSIGNATURE1 P &REST L) takes a signature name and a list of symbol declarations.
   Each symbol declaration is either a symbol or a list (symbol [:type thing]).
   Ensures the arity of each symbol is 1, regardless of declaration."
  `(define-signature1 ',p ',l))

(defun define-signature1 (p l)
  (mapc #'(lambda (x) (setf (parameter-arity x) 1)) (define-signature p l)))

(defmacro defsignature3 (p &rest l)
  "(DEFSIGNATURE3 P &REST L) takes a signature name and a list of symbol declarations.
   Each symbol declaration is either a symbol or a list (symbol [:type thing]).
   Ensures the signature represents a table in triples format, i.e. the first relation is the key
   and has arity 1, and the rest are attributes and have arity 2, where the first argument is the key."
  `(define-signature3 ',p ',l))

(defun define-signature3 (p l)
  (setq p (define-signature p l))
  (setf (parameter-arity (car p)) 1)
  (mapc #'(lambda (x) (setf (parameter-arity x) 2)) (cdr p)))



(defvar *schemas* (make-hash-table))
(defstruct schema name guards signature)
(define-condition invalid-schema (error) ((comment :initarg :comment :accessor comment)))
(define-condition unknown-schema (error) ((comment :initarg :comment :accessor comment)))
(defun find-schema (x)
  (multiple-value-bind (y present) (gethash x *schemas*)
    (unless present (error 'unknown-schema :comment x))
    y))

(defmacro defschema (p &rest l)
  "(DEFSCHEMA P &REST L) "
  `(define-schema ',p ',l))

(defun define-schema (p l)
  (let (h)
    (setq h (grab-keyvals l '(:signature :guards) 'invalid-schema))
    ;(when (gethash p *schemas*) (warn (format nil "Redefining schema ~A" p)))
    (setf (gethash p *schemas*) (make-schema :name p :signature (gethash :signature h) :guards (gethash :guards h)))))

(defvar *guards* (make-hash-table))
(defconstant *kernel-guards* '(__single-cookie-session __globalstate))
(defstruct guard name logic supers datalog)  ; datalog is the version *we* use--keep both around for debugging
(define-condition invalid-guard (error) ((comment :initarg :comment :accessor comment)))
(define-condition unknown-guard (error) ((comment :initarg :comment :accessor comment)))
(defun find-guard (x)
  (multiple-value-bind (y present) (gethash x *guards*)
    (unless present (error 'unknown-guard :comment x))
    y))   

(defmacro defguard (p &rest l)
  "(DEFGUARD P &REST L)"
   `(define-guard ',p ',l))

(defun define-guard (p l)
  (let (supers)
    (when (eq (first l) :inherits) (setq supers (second l)) (setq l (cddr l)))
    (when (and (member p *kernel-guards*) (gethash p *guards*)) 
      (warn (format nil "Redefining Kernel guard ~A" p)))
    (setf (gethash p *guards*) (make-guard :name p :supers supers :logic l :datalog (guard-to-datalog l)))))

(defun guard-to-datalog (l)
  "(GUARD-TO-DATALOG L) transforms a list of datalog constraints and rules (each of which may have)
   a text snippet explaining the constraint) into a set of rules
   where each constraint becomes a rule whose head is (__error ?text).
   Rules are either (i) atomic or (ii) begin with <=.   Every other sentence is a constraint."
  (let (rules constraints)
    (setq l (extract-kif-plus-text l))   ; now l is a list of (kif . "explanation")
    ;  add errors to the heads of all constraints, convert to rules, and index everything 
    (multiple-value-setq (rules constraints) 
      (split #'(lambda (x) (or (atomicp (car x)) (and (listp (car x)) (eq (caar x) '<=)))) l))
    (setq constraints (mapcan #'(lambda (x) (to-canonical-datalog `(<= (__error ,(cdr x)) ,(maknot (quantify (car x)))))) constraints))
    (setq constraints (mapcar #'order-datalog-rule constraints))
    (define-theory (make-instance 'prologtheory) "" (append constraints rules))))

(defun extract-kif-plus-text (l)
  (let (th)
    (setq th nil)
    (do ((ls l (cdr ls)) (cnt 0 (1+ cnt)))
	((null ls) (nreverse th))
      (cond ((stringp (second ls)) 
	     (push (cons (first ls) (second ls)) th)
	     (setq ls (cdr ls)))
	    (t
	     (push (cons (first ls) (format nil "constraint ~A" cnt)) th))))))

(defvar *appdb* (make-instance 'prologtheory))
(defmacro defdb (&rest l)
  "(DEFDB &REST L)"
  `(definemore *appdb* ',l))

(defvar *updates* (make-hash-table))
(defstruct update guards logic datalog)
(define-condition invalid-update (error) ((comment :initarg :comment :accessor comment)))
(define-condition unknown-update (error) ((comment :initarg :comment :accessor comment)))
(defun find-update (x)
  (multiple-value-bind (y present) (gethash x *updates*)
    (unless present (error 'unknown-update :comment x))
    y))

(defmacro defupdate (p opts &rest l)
  "(DEFUPDATE P &REST L)"
  `(define-update ',p ',opts ',l))

(defun define-update (p opts l)
  (let (h)
    (setq h (grab-keyvals opts '(:language :guards) 'invalid-update))
    ;(when (gethash p *updates*) (warn (format nil "Redefining update ~A" p)))
    (setf (gethash p *updates*) (make-update :guards (gethash :guards h) :logic l :datalog (update-to-datalog l (gethash :language h))))))

(defun update-to-datalog (l lang)
  "(UPDATE-TO-DATALOG L) turns a list of sentences into an indexed Datalog theory.
   For now, assume l is already datalog."
  (declare (ignore lang))
  (define-theory (make-instance 'prologtheory) "" l))  

(defvar *forms* (make-hash-table))
(defstruct form schema target guards)
(define-condition invalid-form (error) ((comment :initarg :comment :accessor comment)))
(define-condition unknown-form (error) ((comment :initarg :comment :accessor comment)))
(defun find-form (x)
  (multiple-value-bind (y present) (gethash x *forms*)
    (unless present (error 'unknown-form :comment x))
    y))
(defun find-form-signaturename (x) (schema-signature (find-schema (form-schema (find-form x)))))

(defmacro defform (p &rest l)
  "(DEFFORM P &REST L)"
  `(define-form ',p ',l))

(defun define-form (p l)
  (let (h)
    (setq h (grab-keyvals l '(:guards :schema :target) 'invalid-form))
    ;(when (gethash p *forms*) (warn (format nil "Redefining form ~A" p)))
    (setf (gethash p *forms*) (make-form :guards (gethash :guards h) :target (gethash :target h) :schema (gethash :schema h)))))

(defvar *tables* (make-hash-table))
(define-condition invalid-table (error) ((comment :initarg :comment :accessor comment)))
(define-condition unknown-table (error) ((comment :initarg :comment :accessor comment)))
(defun find-table (x)
  (multiple-value-bind (y present) (gethash x *tables*)
    (unless present (error 'unknown-table :comment x))
    y))

(defmacro deftable (p &rest l)
  "(DEFFORM P &REST L)"
  `(define-table ',p ',l))

(defun define-table (p l)
  (let (schema)
    (when (eq (first l) :schema) (setq schema (second l)) (setq l (cddr l)))
    (unless schema (error 'invalid-table :comment (format nil "No schema for table ~A" p)))
    ;(when (gethash p *tables*) (warn (format nil "Redefining table ~A" p)))
    (setf (gethash p *tables*) schema)))

(defvar *servlets* (make-hash-table))
(defstruct servlet name page updates guards entry)
(define-condition invalid-servlet (error) ((comment :initarg :comment :accessor comment)))
(define-condition unknown-servlet (error) ((comment :initarg :comment :accessor comment)))
(defun find-servlet (x)
  (multiple-value-bind (y present) (gethash x *servlets*)
    (unless present (error 'unknown-servlet :comment x))
    y))

(defmacro defservlet (p &rest l)
  "(DEFSERVLET P &REST L)"
  `(define-servlet ',p ',l))

(defun define-servlet (p l)
  (let (h)
    (setq h (grab-keyvals l '(:page :entry :updates :guards) 'invalid-servlet))
    ;(when (gethash p *servlets*) (warn (format nil "Redefining servlet ~A" p)))
    (setf (gethash p *servlets*) (make-servlet :name p :page (gethash :page h) :guards (gethash :guards h) 
					       :updates (gethash :updates h) :entry (gethash :entry h)))))

; HTML keys are lists of (htmlpagenames or filenames or HTMLREPL objs)
(defvar *html* (make-hash-table))
(defstruct htmlrepl target forms tables)
(define-condition invalid-html (error) ((comment :initarg :comment :accessor comment)))
(define-condition unknown-html (error) ((comment :initarg :comment :accessor comment)))
(defun find-html (x)
  (multiple-value-bind (y present) (gethash x *html*)
    (unless present (error 'unknown-html :comment x))
    y))

(defmacro defhtml (p &rest l)
  "(DEFHTML P &REST L)"
  `(define-html ',p ',l))

(defun define-html (p l)
  (let (h (newl nil))
    (dolist (e l)
      (cond ((symbolp e) (push e newl))
	    ((stringp e) (push e newl))
	    ((listp e) 
	     (unless (or (symbolp (car e)) (stringp (car e))) (error 'invalid-html :comment (format nil "~A in ~A" e p)))
	     (setq h (grab-keyvals (cdr e) '(:forms :tables) 'invalid-html))
	     (push (make-htmlrepl :target (car e) :forms (gethash :forms h) :tables (gethash :tables h)) newl))
	    (t (error 'invalid-html :comment p))))
    (setf (gethash p *html*) (nreverse newl))))
    
; Tests
(defparameter *tests* nil)  ; gets reset to NIL every time loaded (defparameter)
(defstruct wtest servlet input cookie error output dbchange sessionchange cookiechange)
(define-condition invalid-test (error) ((comment :initarg :comment :accessor comment)))

(defmacro deftest (&rest l)
  "(DEFTEST &REST L)"
  `(define-test ',l))

(defun define-test (l)
  (let (h)
    (setq h (grab-keyvals l '(:servlet :input :output :cookie :error :dbchange :sessionchange :cookiechange) 'invalid-test))
    (push (make-wtest :servlet (gethash :servlet h) :input (gethash :input h) :output (gethash :output h) 
		      :cookie (gethash :cookie h) :error (gethash :error h) :dbchange (gethash :dbchange h)
		      :sessionchange (gethash :sessionchange h) :cookiechange (gethash :cookiechange h))
	  *tests*)))


;;; Helper functions
(defun grab-keyvals (l keys error)
  "(GRAB-KEYVALS L KEYS ERROR) takes a list and a set of keys.  It grabs the key/value pairs
   throwing an error of type ERROR if it finds a key not in the list."
  (do ((ys l (cdr ys))
       (h (make-hash-table)))
      ((null ys) h)
    (cond ((or (not keys) (member (first ys) keys))
	   (setf (gethash (first ys) h) (second ys))
	   (setq ys (cdr ys)))
	  (t (error error  :comment (format nil "Unknown option: ~A" ys))))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Builtin program elements
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; must be defined *after* all of the above--they're macros, after all.

; internal guard ensuring each cookie has a single session key
(define-guard '__single-cookie-session `((=> (,*cookie-session-name* ?x) (,*cookie-session-name* ?y) (same ?x ?y))))

; internal guard to check against the global state
(defguard __globalstate :inherits (db cookie session __single-cookie-session))
; can all be re-defined by the app
(defguard db)
(defguard cookie)
(defguard session)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Program Analysis
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
#|
(defun test-all ()
  (let (oldsession newsession oldcookie newcookie possession negsession poscookie negcookie posdb negdb)
    (dolist (e *tests*)
      (let ((olddb *appdb*) bl)
	(setq oldsession (find-cookie-session (wtest-cookie e)))
	(when oldsession 
	  (setq oldsession (gethash oldsession *sessions*)))
	(with-output-to-string (s)
	    (setq newcookie (serve (wtest-input e) (wtest-cookie e) (wtest-servlet e) s)))
	(setq newsession (find-cookie-session newcookie))
	(when newsession
	  (setq newsession (gethash newsession *sessions*)))
	(setq oldcookie (wtest-cookie e))
	
	(setq possession (set-difference newsession oldsession :test #'equal))
	(setq negsession (set-difference oldsession newsession :test #'equal))
	(setq poscookie (set-difference newcookie oldcookie :test #'equal))
	(setq negcookie (set-difference oldcookie newcookie :test #'equal))
	(setq posdb (set-difference (contents *appdb*) (contents olddb) :test #'equal))
	(setq negdb (set-difference (contents olddb) (contents *appdb*) :test #'equal))

#|     ; Don't really know what the output is: the whole bloody database is included currently
	(unless (setequal out (wtest-output e) :test #'equal)
	  (format t "Output error~%")
	  (format t "Actual output: ~%~S~%" out)
	  (format t "Spec: ~%~S~%" (wtest-sessionchange e)))
|#
	(setq bl '((t . t)))
	(unless (test-one possession negsession (wtest-sessionchange e) bl)
	  (format t "Session error~%")
	  (format t "Added session elements: ~%~S~%" (plug possession bl))
	  (format t "Deleted session elements: ~%~S~%" (plug negsession bl))
	  (format t "Spec: ~%~S~%" (plug (wtest-sessionchange e) bl)))

	(unless (test-one poscookie negcookie (wtest-cookiechange e) bl)
	  (format t "Cookie error~%")
	  (format t "Added cookie elements: ~%~S~%" (plug poscookie bl))
	  (format t "Deleted cookie elements: ~%~S~%" (plug negcookie bl))
	  (format t "Spec: ~%~S~%" (plug (wtest-cookiechange e) bl)))

	(unless (test-one posdb negdb (wtest-dbchange e) bl)
	  (format t "DB error~%")
	  (format t "Added DB elements: ~%~S~%" (plug posdb bl))
	  (format t "Deleted DB elements: ~%~S~%" (plug negdb bl))
	  (format t "Spec: ~%~S~%" (plug (wtest-dbchange e) bl)))))))

(defun test-one (pos neg spec bl)
  (multiple-value-bind (specpos specneg) 
      (split #'(lambda (x) (eq (relation x) 'neg)) spec)
    (setq specpos (mapcar #'drop-posneg specpos))
    (setq specneg (mapcar #'drop-posneg specneg))
    (and (setequal pos specpos) (setequal neg specneg))))

;(defstruct wtest servlet input cookie error output dbchange sessionchange cookiechange)
|#	    
      
      

(defun check () (and (check-servlets) (check-html)))
(defun check-servlets (&optional (s nil))
  (let (errors)
    (dolist (x (if s (list (gethash s *servlets*)) (mapcar #'cdr (hash2bl *servlets*))))
      (unless (gethash (servlet-page x) *html*) 
	(push (format nil "In servlet ~A, undefined page ~A" x (servlet-page x)) errors))
      (dolist (y (servlet-guards x))
	(unless (gethash y *guards*) 
	  (push (format nil "In servlet ~A, undefined guard ~A" x y) errors)))
      (dolist (y (servlet-updates x))
	(unless (gethash y *updates*)
	  (push (format nil "In servlet ~A, undefined update ~A" x y) errors))))
    (dolist (v (nreverse errors))
      (format t ";~A~%" v))
    (not errors)))

(defun check-html (&optional (h nil))
  (let (errors)
    (dolist (x (if h (list h) (mapcar #'cdr (hash2bl *html*))))
      (dolist (y x)
	(cond ((symbolp y) 
	       (unless (gethash y *html*) (push (format nil "In html ~A, undefined html ~A" x y) errors)))
	      ((and (listp y) (symbolp (first y)))
	       ; still ought to check that the form used for replacement is defined, but for now this is good enough.
	       (unless (gethash (first y) *html*) (push (format nil "In html ~A, undefined html ~A" x (first y)) errors))))))
    (dolist (v (nreverse errors))
      (format t ";~A~%" v))
    (not errors)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Compiler
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; right now we're throwing away results, but we ought to be storing them and making this an
;   actual compilation step
(defun guard-logic-full (guard)
  "(GUARD-LOGIC-FULL GUARD) returns the list of all sentences in the guard named GUARD or in 
   one of the guards inherited from GUARD (recursively).  Memoized."
  (remove-duplicates (guard-logic-full-aux guard (make-hash-table)) :test #'equal))

(defun guard-logic-full-aux (guardname done)
  "(GUARD-LOGIC-FULL-AUX GUARDNAME DONE) takes a guard name GUARDNAME and a hash table DONE.
   Returns the guard-logic for GUARD and memoizes results in DONE."
  (let (logic guard)
    (setq logic (gethash guardname done))
    (cond (logic logic)
	  (t (setq guard (find-guard guardname))
	     (setf (gethash guardname done) 
		   (append (apply #'append (mapcar #'(lambda (x) (guard-logic-full-aux x done)) (guard-supers guard)))
			   (guard-logic guard)))))))

; TODO: figure out uniqueness from constraints; more generally, do something smarter than dropping quantified constraints
(defun compile-form (formname &key (html nil) (js t))
  "(COMPILE-FORM FORMNAME) runs Plato to generate the form described by FORMNAME and saves
  to an HTML/JS file in *compile-dir*."   

  (let (th form target schema guards h)    
    ; extract info
    (setq form (find-form formname))
    (setq target (form-target form))
    (setq schema (find-schema (form-schema form)))
    (setq guards nil)
    (setq guards (union (form-guards form) (schema-guards schema)))
    (setq guards (remove-if-not #'quantifier-free-sentencep (remove-duplicates (mapcan #'guard-logic-full guards) :test #'equal)))
    (setq guards (and2list (del-namespace (maksand guards) (find-form-signaturename formname))))
    (setq th (prep-plato-theory formname target nil))
    (push `(constraints ',guards) th)
    (setq h (compile-websheet th))
    (setf (htmlform-action h) (format nil "/scwa?"))
    (when html
      (with-open-file (f (fspath :compile (tostring formname ".html")) :direction :output :if-does-not-exist :create :if-exists :supersede)
	(output-htmlform-form f h)))
    (when js
      (with-open-file (f (fspath :compile (tostring formname ".js")) :direction :output :if-does-not-exist :create :if-exists :supersede)
	(princ (htmlform-javascript h) f)
	(format f "function getBigBoxName (x) { return x + 'BIGBOX'; }~%")))
    'done))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Execution Engine
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defvar *debug-webapps* nil)
(define-condition guard-violation (error) ((comment :initarg :comment :accessor comment)))
(define-condition lost-session (error) ((comment :initarg :comment :accessor comment)))
(define-condition internal-error (error) ((comment :initarg :comment :accessor comment)))
; internal servlet

; TODO: parameter tampering defense so we don't need formname as data
(defmethod process-cookies ((file (eql 'scwa)) postlines)  
  (let (servletname formname cookies in)
  (handler-case (progn

    ; grab target (only necessary b/c deployed using interpreter)
    (setq servletname (read-user-string (getf-post "servlet" postlines)))
    (unless (gethash servletname *servlets*)
      (setq *content* 
	    (with-output-to-string (s)
	      (let ((*output-stream* s))
		(showerror in (pairs2data *cookies*) servletname (make-instance 'unknown-servlet)))))
      (return-from process-cookies *cookies*))

    ; grab source and correct for namespace
    (setq formname (read-user-string (getf-post "formname" postlines)))
    (setq in (remove-if #'(lambda (x) (member (car x) '("servlet" "formname") :test #'equal)) postlines)) 
    (when formname
      (setq in (add-namespace in (tostring (schema-signature (find-schema (form-schema (find-form formname))))))))
    (add-namespace cookies "cookie")

    ; make relations symbols
    (setq in (pairs2data in))
    (setq cookies (pairs2data *cookies*))

    ; run the servlet and return its cookies
    (setq *content*
	  (with-output-to-string (stream)
	    (setq cookies (serve in cookies servletname stream))))
    ; turn cookies database back to pairs
    (data2pairs cookies))
    (condition () 
      (setq *content* 
	    (with-output-to-string (s) 
	      (let ((*output-stream* s))					       
		(showerror in (pairs2data *cookies*) servletname (make-instance 'internal-error)))))
      *cookies*))))

(defun add-namespace (l prefix)
  "(ADD-NAMESPACE L PREFIX) takes a postlines L and a string PREFIX."
  (assert (and (stringp prefix) (every #'(lambda (x) (stringp (car x))) l)) nil 
	  (format nil "ADD-NAMESPACE requires strings as arguments"))
  (mapcar #'(lambda (x) (if (atom x)
			    (tosymbol (stringappend prefix "." x))
			    (cons (stringappend prefix "." (car x)) (cdr x))))
		    l))

(defun del-namespace (l prefix)
  (let* (pre lpre)
    (setq pre (tostring prefix "."))
    (setq lpre (length pre))
    (mapatoms #'(lambda (x) (let ((p (tostring (relation x))))
			      (if (and (>= (length p) lpre) (string= pre p :end2 lpre))
				  (if (atom x)
				      (tosymbol (subseq p lpre))
				      (cons (tosymbol (subseq p lpre)) (cdr x)))
				  x)))
	      l)))

; process is now called *after* process-cookies, which for us does all the work.
; So we just print the result of process-cookies, which was saved in *content*
(defmethod process (s (file (eql 'scwa)) postlines)
  (declare (ignore postlines))
  (princ *content* s))

(defun pairs2data (postlines)
  (mapcar #'(lambda (x) (list (read-user-string (car x)) (cdr x))) postlines))

(defun data2pairs (data)
  (mapcar #'(lambda (x) (cons (first x) (second x))) data))

(defun serve (in cookie servletname s)
  "(SERVE IN COOKIE SERVLETNAME) executes the servlet with name SERVLETNAME on data IN and COOKIE
   and prints the resulting HTML page to stream S and returns new cookie data.  All data is represented
   as lists of KIF atoms."
  (let (sessionid session present (*attachments* t) (*output-stream* s))
    ; look up session if necessary
    (setq sessionid (find-cookie-session cookie))
    (setq session nil)
    (when sessionid
      (setq sessionid (read-user-string sessionid))
      (when (listp cookie) (setq cookie (define-theory (make-instance 'prologtheory) "" cookie)))
      ; ensure no cookie session pollution
      (handler-case (guard-check '__single-cookie-session cookie)
	(guard-violation (e) (showerror in cookie servletname e) (return-from serve (contents cookie))))
      ; check if we have the session
      (multiple-value-setq (session present) (gethash sessionid *sessions*))
      (handler-case (unless present (error 'lost-session :comment (list sessionid nil)))
	(lost-session (e) (showerror in cookie servletname e) (return-from serve (contents (drop-cookie-session cookie))))))
    ; serve
    (drop-cookie-session cookie)
    (serve-session in cookie session sessionid servletname)))


(defun serve-session (in cookie session sessionid servletname)
  "(SERVE-NORMAL IN COOKIE SESSION SERVLETNAME S) takes theories for IN, COOKIE, SESSION, a SERVLETNAME, and a stream S.
   It outputs the result of the servlet to stream S and returns the new cookie data as a list. "
  (let (th servlet oldcookie)
    (setq oldcookie (contents cookie))
    ; lookup servlet
    (setq servlet (find-servlet servletname))
    ; create data structure (a theory) representing the server's global state and including in, cookie, and session
    (setq th (get-server-state in cookie session))
    ; process data according to servlet semantics
    (handler-case (progn
                    ; check guards
		    (mapc #'(lambda (x) (guard-check x th)) (servlet-guards servlet))
		    ; run updates on theory TH
		    (transduce-all-data (servlet-updates servlet) th)
		    ; check integrity constraints for all our data stores
		    (guard-check '__globalstate th))
      (condition (e) (showerror in cookie servlet e) (return-from serve-session oldcookie)))
    ; set server's state using results of transduction
    (setq cookie (set-server-state th sessionid))
    ; render the page to the user  
    (render (servlet-page servlet) th)
    ; free memory by unincluding all theories so that when TH goes out of scope, the memory is freed
    (decludes th)
    ; return the cookie
    cookie))

(defun serve-lost-session (in cookie servletname)
  (declare (ignore servletname))
  (render 'lostsession (append in cookie)))

(defun get-server-state (in cookie session)
  "(GET-SERVER-STATE IN COOKIE SESSION) returns a theory (possibly with includees) that contains 
   all of IN, COOKIE, SESSION, and *appdb*"
  (let (th theories)
    (setq theories (list *appdb*))
    (if (listp in) (setq th (nconc th in)) (push in theories))
    (if (listp cookie) (setq th (nconc th cookie)) (push cookie theories))
    (if (listp session) (setq th (nconc th session)) (push session theories))
    (setq th (define-theory (make-instance 'prologtheory) "" th))
    (mapc #'(lambda (x) (includes th x)) theories)  ; this ensures TH only gets deallocated if we declude it when we're done
    th))

(defun set-server-state (th sessionid)
  "(SET-SERVER-STATE TH SESSIONID) updates the server's state and returns the cookie for the user to update her state."
  (let (cookie session)
    (setq *appdb* (define-theory (make-instance 'prologtheory) "" (find-appdb-data th)))
    (setq session (find-session-data th))
    (setq cookie (drop-cookie-session (find-cookie-data th)))
    ; grab session; ensure that there is a cookie/session link iff session is non-nil
    (cond (session  ; ensure a cookie/session link
	   ; set session
	   (unless sessionid (setq sessionid (random 1000)))
	   (setf (gethash sessionid *sessions*) session)
	   ; store session id in cookie
	   (push `(,*cookie-session-name* ,sessionid) cookie))
	  (t
	   ; if no session, delete sessionid; cookie session has already been deleted
	   (when sessionid (remhash sessionid *sessions*))))
    cookie))

(defun find-cookie-data (th) (find-data 'cookie th))
(defun find-appdb-data (th) (find-data 'db th))
(defun find-session-data (th) (find-data 'session th))
; HORRIBLE PERFORMANCE
(defun find-data (namespace th)
  (let (l)
    (setq namespace (tostring namespace "."))
    (setq l (length namespace))
    (remove-if-not #'(lambda (x) (and (listp x) (>= (length (tostring (car x))) l) 
				      (string= (subseq (tostring (car x)) 0 l) namespace))) (sentences '? th))))

(defun transduce-all-data (updates data)
  "(TRANSDUCE-ALL-DATA UPDATES DATA) applies a sequence of database names UPDATES to DATA and
   returns the result.  DATA is assumed to be indexed; returns an indexed DATA."
  (when *debug-webapps* (format t "~&Initial Database: ~%~S~%" (sentences '? data)))
  (dolist (u updates data)
    (when *debug-webapps* (format t "~&Applying update ~A~%" u))
    (setq data (transduce-data u data))
    (when *debug-webapps* (format t "~&Database after applying update ~A: ~%~S~%" u (sentences '? data)))))

(defun transduce-data (up data)
  "(TRANSDUCE-DATA UP DATA) applies the update named UP to DATA (including any guards checking for UP)
   and returns the resulting data.  Assumes DATA and (update-datalog UP) are already indexed.
   Returns an indexed data set."
  (let (pos neg)
    (setq up (gethash up *updates*))
    (unless up (error 'unknown-update :comment up))
    ; check the guards
    (mapc #'(lambda (x) (guard-check x data)) (update-guards up))
    ; grab all the intensional relations from the datalog rules
    ;(setq preds (mapcar #'head (contents (update-datalog up))))
    ;(setq preds (remove-duplicates (mapcar #'(lambda (x) (make-parameter :symbol (relation x) :arity (arity x))) preds)))
    ; apply viewfinds to all the intensional relations in UP
    (includes data (update-datalog up))
    (setq pos (viewfinds '?x `(pos ?x) data))
    (setq neg (viewfinds '?x `(neg ?x) data))
    (when *debug-webapps* (format t "~&Pos update: ~%~S~%" pos))
    (when *debug-webapps* (format t "~&Neg update: ~%~S~%" neg))

;    (setq th (mapcan #'(lambda (x) (let ((p (cons (parameter-symbol x) 
;						  (maptimes #'newindvar (parameter-arity x)))))
;				     (viewfinds p p data)))
;	  preds))
    (unincludes data (update-datalog up))
    (mapc #'(lambda (x) (drop* x data)) neg)  ; using drop* to ensure removed from all included theories
    (mapc #'(lambda (x) (save x data)) pos)
    data))

(defun guard-check (guard data) 
  "(GUARD-CHECK GUARD DATA) takes a GUARD name (a set of constraints) and DATA and checks
   that DATA U GUARD is satisfiable.  If not, throws an error and includes an explanation
   for why satisfaction fails.  Should always be able to assume DATA is complete; however,
   there might be datalog view definitions in GUARD."
  (assert (not (listp data)) nil (format nil "guard-check assumes DATA is a real theory, not a list"))
  (let (th preds errs)
    ;(setq *tmp* th)
    (setq th (guard-datalog (find-guard guard)))
    (includes th data)
    (setq preds (mapcar #'parameter-symbol (get-vocabulary (maksand (contents data)))))
    ;(setq *tmp2* preds)
    (setq errs (viewsupports '?text '(__error ?text) th #'(lambda (x) (member x preds))))
    (unincludes th data)
    (when errs (error 'guard-violation :comment (list guard errs)))))

;(class-name (class-of err))
(defmethod showerror (in cookie servlet err)
  (setq err (class-name (class-of err)))
  (format *output-stream* "<html><head><title>~A</title></head><body>" err)
  (format *output-stream* "~&<P>Error in servlet ~A: ~A.~%" servlet err)
  (showerror-in in)
  (showerror-cookie cookie)
  (format *output-stream* "</body></html>"))

(defmethod showerror (in cookie servlet (err unknown-servlet))
  (format *output-stream* "<html><head><title>Unknown servlet</title></head><body>")
  (format *output-stream* "~&<P>Unknown servlet: ~A.~%" servlet)
  (showerror-in in)
  (showerror-cookie cookie)
  (format *output-stream* "</body></html>"))
  
(defmethod showerror (in cookie servlet (err lost-session))
  (declare (ignore servlet))
  (format *output-stream* "<html><head><title>Lost session</title></head><body>")
  (format *output-stream* "~&<P>Session invalid.  Please login again.~%")
  (showerror-in in)
  (showerror-cookie cookie)
  (format *output-stream* "</body></html>"))

(defmethod showerror (in cookie servlet (err guard-violation))
  ; servlet name
  (format *output-stream* "<html><head><title>Error caught by ~A</title></head><body>" (servlet-name servlet))
  (format *output-stream* "~&<P>Servlet ~A caught an error~%" (servlet-name servlet))
  ; errors and causes
  (format *output-stream* "~&<p>Found error in guard ~A~%" (first (comment err)))
  (format *output-stream* "<ul>")
  (dolist (e (second (comment err)))
    (format *output-stream* "<li>Error ~S caused by ~S~%" (first e) (second e)))
  (format *output-stream* "</ul>~%")
  (showerror-in in)
  (showerror-cookie cookie)
  (format *output-stream* "</body></html>"))

(defun showerror-in (in)
  (format *output-stream* "<p>Data received by server:~%")
  (format *output-stream* "<ul>~%")
  (dolist (i (contents in))
    (format *output-stream* "<li>~S~%" i))
  (format *output-stream* "</ul>~%"))

(defun showerror-cookie (cookie)
  (format *output-stream* "<p>Cookies received by server:~%")
  (format *output-stream* "<ul>~%")
  (dolist (i (contents cookie))
    (format *output-stream* "<li>~S~%" i))
  (format *output-stream* "</ul>~%"))



(defstruct htmlbl forms tables)

(defun render (htmlname data)
  "(RENDER HTMLNAME DATA) takes an HTMLNAME, DATA to populate that page (overwriting
   any data extracted by PAGENAME as a default), and outputs that page to the default stream.  
   Current implementation only does form/table replacement after html page is fully created;
   to do it properly we need an XML/HTML parser that appropriately deals with partial XML
   files (in a non-lossy way).  For pages with multiple forms/tables that have the same name,
   this implementation is incorrect.  Also, assuming that formname.widgetname gives a unique ID,
   which is again incorrect for pages with multiple forms."
  (multiple-value-bind (s repls) (render-to-string htmlname)
    (princ (html-subst s (htmlrepls2htmlbl repls) data) *output-stream*)))

(defun htmlrepls2htmlbl (repls) 
  (make-htmlbl :forms (mapcan #'(lambda (x) (copy-list (htmlrepl-forms x))) repls)
	       :tables (mapcan #'(lambda (x) (copy-list (htmlrepl-tables x))) repls)))

(defun render-to-string (thing)
  "(RENDER-TO-STRING THING) returns a string representing the html file named HTMLFILE
   as well as the sequence of htmlrepls that ought to be applied.  Note: does not apply
   those HTMLREPLS as it ought to.  Super slow---copying html fragments over and over."
  (let (subhtml out bl)
    (setq out (make-stringbuffer))
    (setq bl nil)
    (cond ((stringp thing) (format out "~A" (read-static-file thing)))
	  ((symbolp thing) 
	   (setq subhtml (gethash thing *html*))
	   (dolist (x subhtml)
	     (multiple-value-bind (s b) (render-to-string x)
	       (format out "~A" s) (setq bl (nconc b bl)))))
	  ((htmlrepl-p thing) 
	   (multiple-value-bind (s b) (render-to-string (htmlrepl-target thing))
	     (setq out s) (setq bl (cons thing b))))
	  (t (error 'invalid-html :comment thing)))
    (values out bl)))

(defun read-static-file (name) (read-any-file (fspath :html name)))

(defun html-subst (str htmlrepl data)
  "(PAGE-SUBST STR REPLACEMENTS) takes a valid HTML fragment STR as a string and a list
   of the form :forms ((\"search\" search)) :tables ((\"auctions\" auctions)) and replaces
   each occurrence of a form \"search\" with a rendering of the form SEARCH; similarly for table."
  (xmls:toxml (html-subst-aux (xmls:parse str) htmlrepl data) :shortend nil))

(defun html-subst-aux (html repl data)
  (cond ((atom html) html)
	((not (listp html)) (error 'invalid-html :comment html))
	(t
	 (let (tag)
	   (handler-case (setq tag (tosymbol (car html)))
	     (condition () (setq tag nil)))
	   (case tag
	     (head (html-augment-head html *corejs* *corecss*))
	     (form (html-subst-form html repl data))
	     (table (html-subst-table html repl data))
	     (otherwise (mapcar #'(lambda (x) (html-subst-aux x repl data)) html)))))))

#|
(defun xml2html (x)
  (cond ((atom x) x)
	((not (listp x)) (error 'invalid-html :comment x))
	(t (list* (tosymbol (car x)) 
		  (mapcar #'(lambda (x) (cons (tosymbol (first x)) (rest x))) (second x)) 
		  (mapcar #'xml2html (cddr x))))))
|#
(defun html-augment-head (head jsfiles cssfiles) 
  (append head (mapcar #'js-include-xml jsfiles) (mapcar #'css-include-xml cssfiles))) 
(defun js-include-xml (jsfile) (xmls:parse (js-include jsfile)))
(defun js-include (jsfile) (with-output-to-string (s) (output-htmlform-js jsfile s)))
(defun css-include-xml (cssfile) (xmls:parse (css-include cssfile)))
(defun css-include (cssfile) (with-output-to-string (s) (output-htmlform-style cssfile s)))

(defun html-subst-form (form bls data)
  (let (name bl)
    (setq name (second (find "name" (second form) :key #'first :test #'equalp)))
    (setq bl (find name (htmlbl-forms bls) :key #'first :test #'equal))
    (if bl
	(xmls:parse (generate-form (second bl) data))
	(xmls:parse "<form></form>"))))

(defun html-subst-table (table bls data)
  (let (name bl)
    (setq name (second (find "name" (second table) :key #'first :test #'equalp)))
    (setq bl (find name (htmlbl-tables bls) :key #'first :test #'equal))
    (if bl
	(generate-table (second bl) data)
	(list* (first table) (second table) (mapcar #'(lambda (x) (html-subst-aux x bls data)) (cddr table))))))

(defun generate-form (formname data)
  "(GENERATE-FORM FORMNAME DATA) output an HTML form as described by FORMNAME
   where the initial values are drawn from DATA.  Assume JS for error-checking has already been compiled."
  (let (form target th html)
    (setq form (find-form formname))
    (setq target (form-target form))
    (setq data (del-namespace (contents data) (find-form-signaturename formname)))
    (setq th (prep-plato-theory formname target data))
    (push `(constraints 'nil) th)
    (setq html (compile-websheet th))
    (setf (htmlform-action html) (format nil "/scwa?"))
    (setf (htmlform-submitprep html) "true")
    (with-output-to-string (s)
      (format s "<p>") ; to ensure parsing handles both elements
      (when (probe-file (fspath :compile (tostring formname ".js"))) 
	(princ (js-include (webpath :compile (tostring formname ".js"))) s))
      (output-htmlform-form s html)
      (format s "</p>"))))

; TODO: have plato's output be wrapped in a JS namespace so that it actually works.  
;   Need to call a version of JS init() for each of the forms.
;   Should only have 1 copy of the basic functions (e.g. spreadsheet.js), but then cellvalue needs to be specific to a form.
; TODO: need to analyze guards to figure out which fields are unique, which are required, etc.
(defun prep-plato-theory (formname target data)
  "(PREP-PLATO-THEORY FORMNAME DATA) builds a theory for Plato describing FORMNAME, except constraints."
  (let (th signame name req init desc style incundef unique type formdata values)
    ;(print (contents data))
    (setq formdata (extract-form-data formname data))
    ;(print formdata)
    ; create basic Plato theory 
    (push `(formname ,formname) th)
    (push `(option completep t) th)
    (push `(option casesensitive t) th)
    (push `(option allowconflicts t) th)
    (push `(option debug nil) th)
    (push `(definitions ',nil) th)
    ; add widgets
    (setq signame (schema-signature (find-schema (form-schema (find-form formname)))))
    (dolist (p (find-signature signame))
      (assert (= (parameter-arity p) 1) nil 
	      (format nil "All symbols used during form generation must have arity 1; ~A has arity ~A" (parameter-symbol p) (parameter-arity p)))
      (setq name (parameter-symbol p))
      (setq init (viewfinds '?x `(,(tosymbol (tostring signame ".") name) ?x) formdata))
      (when init (setq init (cons 'listof init)))
      (setq desc (tostring name))
      (setq incundef t)
      (multiple-value-setq (style unique req type values) (extract-widget-info formname p))
      (when values (push `(type ,type ,values) th))
      (push `(widget :name ,name :req ,req :init ,init :desc ,desc :style ,style :incundef ,incundef :unique ,unique :typename ,type) th))
    ; providing all the fields for this widget since otherwise load-formstructure does something stupid
    (push `(widget :name servlet :init (listof ,target) :style hidden :desc "" :req nil :incundef nil :unique t :typename string) th)
    (nreverse th)))

(defun extract-widget-info (formname param)
  "(EXTRACT-WIDGET-INFO FORMNAME PARAM) analyzes the guards for FORMNAME to figure out the properties
  for the widget PARAM and returns (i) style, (ii) unique, (iii) required, (iv) typename, (v) list of datalog defining typename.
  Here we're just looking for some simple syntactic patterns; if instead we were to do this at
  compilation time, we could see what statements were entailed."
  (let (sig style unique required values typename paramsymbol form schema)
    ; prefix the parameter name by the signature
    (setq sig (schema-signature (find-schema (form-schema (find-form formname)))))
    (setq paramsymbol (tosymbol (tostring sig ".") (parameter-symbol param)))
    ; analyze logic
    (setq values t)
    (setq form (find-form formname))
    (setq schema (find-schema (form-schema form)))
    (dolist (g (union (form-guards form) (schema-guards schema)))
      (dolist (p (guard-logic (find-guard g)))
	(unless (stringp p)
	; required
	  (when (samep p `(exists ?x (,paramsymbol ?x))) (setq required t))
	; convert to clausal and check each clause
	  (dolist (q (clauses p))
	  ; uniqueness
	    (when (sentequal q `(or (not (,paramsymbol ?x)) (not (,paramsymbol ?y)) (= ?x ?y)) :test #'samep)
	      (setq unique t))
	  ; enumeration of values: last one wins
	    (multiple-value-bind (list isenum) (enumerated-type q paramsymbol)
	      (when isenum (setq values list)))))))
    ; figure out info to return
    (cond ((listp values)  ; found an enumerator
	   (setq style 'selector)
	   (setq typename (tosymbol (gentemp "type")))
	   (setq values (cons 'listof values)))
	  (t
	   (setq style 'textbox)
	   (setq typename (first (parameter-type param)))
	   (setq values nil)))
    (values style unique required typename values)))
	    
(defun enumerated-type (p sym)
  "(ENUMERATED-TYPE P SYM) checks if clause P is equivalent to (or (not (p ?x)) (= ?x a1) (= ?x a2) ...)
   Returns 2 values: the list of (a1 a2 ...) and whether or not the sentence is of the desired form (
  to handle the case (not (p ?x))."
  (cond ((not (listp p)) (values nil nil))
	((and (atomicp p) (samep p `(not (,sym ?x)))) (values nil t))
	((not (eq (car p) 'or)) (values nil nil))
	(t
	 (setq p (cdr p))
	 (let (v vals)
           ; check only one occurrence of (not (p ?x)), grab var.
	   (multiple-value-bind (notps rest) (split #'(lambda (x) (samep x `(not (,sym ?x)))) p)
	     (when (or (not notps) (cdr notps)) (return-from enumerated-type (values nil nil)))
	     (setq notps (first notps))
	     (setq v (first (vars notps)))
	     (setq p rest))
           ; check that remainder are all of the form (= ,var string)
	   (dolist (a p (values (nreverse vals) t))
	     (if (eq (first a) '=)
		 (if (eq (second a) v)
		     (if (atom (third a))
			 (push (third a) vals)
			 (return (values nil nil)))
		     (if (eq (third a) v)
			 (if (atom (second a))
			     (push (second a) vals)
			     (return (values nil nil)))))
		 (return (values nil nil))))))))

(defun extract-form-data (formname data)
  "(EXTRACT-FORM-DATA FORMNAME DATA) finds the portion of DATA appropriate for form FORMNAME
   and returns it (leaving the namespace in tact to avoid hitting Epilog's builtin functions)."
  (let (newdata sig sigl p preds)
    (setq sig (find-form-signaturename formname))
    (setq preds (find-signature sig))
    (setq sig (tostring sig "."))
    (setq sigl (length sig))
    (dolist (d (contents data))
      (setq p (tostring (relation d)))
      (when (eq (search sig p) 0)
	(setq p (tosymbol (subseq p sigl)))
	(when (member p preds :key #'parameter-symbol)
	  (push d newdata))))
;	  (if (atom d)
;	      (push p newdata)
;	      (push (cons p (cdr d)) newdata)))))
    (nreverse newdata)))
	

(defun generate-table (tablename data)
  (declare (ignore tablename data))
  (xmls:parse "<table name=\"tim\"></table>"))

