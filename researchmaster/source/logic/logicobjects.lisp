
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; logicobjects.lisp
;;;     object-oriented versions of logic
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun configit-objects-to-spreadsheet-objects (o)
  "(CONFIGIT-OBJECTS-TO-SPREADSHEET-OBJECTS O) translates an object-oriented
   configit configuration management specification to one more amenable to
   the webform compiler.  Returns a list of class definitions (including one for Main)
   and some datalog rules for defining basic types."
  (labels ((mutate-type (o) (mapcar #'(lambda (x) (list (second o) x)) (third o)))
	   (mutate-class (o datalog classhash) 
	     "(MUTATE-CLASS-BODY O DATALOG CLASSHASH) takes a class definition O,
              DATALOG defining basic types, and a hash-table of types.  Returns 2 values:
              a new class definition and a set of new datalog statements."
	     ;(format t "~&(mutate-class-body ~A)~%" (first o))
	     (let (class vars constraints newdatalog range)
	       (setq range (make-hash-table))  ; a hash table tracking which ranges have already been materialized
	       (setq class (second o))  ; class name
	       (setq o (cddr o))
	       (multiple-value-setq (vars constraints) (split #'(lambda (x) (eq (first x) 'vardec)) o))
	       ; mutate all vardecs -- name all ranges
	       (when vars
		 (multiple-value-setq (vars newdatalog) 
		   (mapcaraccum #'(lambda (x)
				    (cond ((listp (fourth x))  ; a range definition
					   (let ((low (second (fourth x))) (high (third (fourth x)))
						 highrange lowrange type datalog)
					     (setq lowrange (gethash low range))
					     (when lowrange (setq highrange (gethash high lowrange)))
					     ; assign a type: either by lookup or by construction
					     (cond ((and lowrange highrange)  ; type already exists
						    (setq type highrange))
						   (t  ; create new type
					             ; create the datalog
						    (setq type (tosymbol (gentemp "tlhtype")))
						    (dotimes (i (1+ (- high low)))
						      (push (list type (+ i low)) datalog))
					            ; set the lower hash table if necessary
						    (unless lowrange 
						      (setq lowrange (make-hash-table))
						      (setf (gethash low range) lowrange))
					            ; set the high hash table to the new type
						    (setf (gethash high lowrange) type)))
					     ; return the new vardec
					     (values (list (first x) (second x) (third x) type) datalog)))
					  (t (values x nil)))) vars)))
	       ; mutate all of the constraint sets
	       (when constraints
		 (setq constraints (apply #'nconc (mapcar #'cdr constraints)))
		 (setq constraints (mapcar #'(lambda (x) (configit-mutate-constraint x class datalog classhash)) constraints))
		 (setq constraints (and2list (flatten-operator (maksand constraints)))))  ; flatten all toplevel ands
	       (values (list* 'class class (nconc vars constraints)) newdatalog))))
  
    ; Assuming just one typeclass and one main per configit problem definition
    (let ((result nil) datalog (classtypehash (make-hash-table))
	  types classes main)
      ; break apart objects into types, classes, main, and error if anything else
      (dolist (h o)
	(cond ((atom h) (push h result))
	      ((eq (car h) 'typeclasses)
	       (assert (and (not types) (not classes)) nil 
		       "Shouldn't have multiple TYPECLASS definitions.")
	       (assert (every #'(lambda (x) (and (listp x) (member (car x) '(type class)))) (cdr h))
		       nil "Typeclasses should contain a list containing (type ...) or (class ...)")
	       (multiple-value-setq (types classes) (split #'(lambda (x) (eq (car x) 'type)) (cdr h))))
	      ((eq (car h) 'main)
	       (assert (not main) nil "Shouldn't have multiple MAIN definitions.") 
	       (setq main h))
	      (t (push h result))))
      (assert (null result) nil "Found unprocessable object components: ~%~S~%" result)

      ; process big pieces
      (setq datalog (mapcan #'mutate-type types))
      (setq datalog (list* '(boolean true) '(boolean false) datalog))

      ; prep classes
      (setq classes (cons (cons 'class main) classes))
      (setq classtypehash (configit-class-vars classes))
      ;(dolist (h (hash2list classtypehash))
        ; (format t "~&class ~A:~%" (first h))
	;(pretty-print (second h))
	;(format t "~%"))

      ; mutate classes
      (multiple-value-bind (newobjs newdatalog)
	  (mapcaraccum #'(lambda (x) (mutate-class x datalog classtypehash)) classes)
	(values newobjs (nconc datalog newdatalog))))))

(defun configit-mutate-constraint (p class data types)
  "(CONFIGIT-MUTATE-CONSTRAINT P CLASS DATA TYPES) takes a constraint P from class CLASS
   where DATA defines basic types, and TYPES defines
   variable types for all classes: hashing on the class name returns a hash table keyed on variable
   names within that class whose value is (scope <class name or basic type>).
   Massages constraint and returns the result."
  (cond ((atom p) (list p 'true))
	((member (car p) '(forall exists and or not => <= <=>))
	 (cons (car p) (mapcar #'(lambda (x) (configit-mutate-constraint x class data types)) (cdr p))))
	((eq (car p) '=<) (configit-mutate-constraint `(>= ,(third p) ,(second p)) class data types))
	((eq (car p) '>=)
	 (assert (and (listp (second p)) (listp (third p)) 
		      (eq (first (second p)) 'order) (eq (first (third p)) 'order)
		      (or (stringp (second (second p))) (stringp (second (third p)))))
		 nil
		 (format nil "Implementation hole: >= and =< must take two (order X) args and one X must be a string: ~A" p))
	 (let ((x (second (second p))) (y (second (third p))) type newp)
	   (cond ((and (stringp x) (stringp y)) (if (string>= x y) 'true 'false))
		 ((stringp x)
		  (setq type (configit-dot-type y class types))
		  (setq type (viewfinds '?x `(,type ?x) data))
		  (setq type (delete-if-not #'(lambda (z) (string>= x z)) type))
		  (setq newp (maksor (mapcar #'(lambda (z) `(= ,y ,z)) type))))
		 (t
		  (setq type (configit-dot-type x class types))
		  (setq type (viewfinds '?x `(,type ?x) data))
		  (setq type (delete-if-not #'(lambda (z) (string>= z y)) type))
		  (setq newp (maksor (mapcar #'(lambda (z) `(= ,x ,z)) type)))))
	   (configit-mutate-constraint newp class data types)))
	((eq (car p) 'case)
	 (maksand (mapcarnot #'(lambda (x) 
				 (if (eq (first x) 'default) 
				     (if (eq (second x) 'true)
					 nil
					 (configit-mutate-constraint (maksor (mapcar #'(lambda (y) `(= ,(second p) ,(first y)))
										     (butlast (cddr p))))
								     class data types))
				     (configit-mutate-constraint `(=> (= ,(second p) ,(first x)) ,(second x))
								 class data types)))
			     (cddr p))))
	((eq (car p) '<>) (configit-mutate-constraint `(not (= ,(second p) ,(third p))) class data types))
	((eq (car p) '=)
	 (let ((var2 (configit-varp (second p) class types))
	       (var3 (configit-varp (third p) class types)))
	   (setq p (list '= (configit-mutate-term (second p) class types) (configit-mutate-term (third p) class types)))
	   (if var2 
	       (if var3
		   `(forall ?x (<=> (,(second p) ?x) (,(third p) ?x)))
		   `(,(second p) ,(third p)))
	       (if var3
		   (list (third p) (second p))
		   p))))
	((configit-varp p class types)
	 (let ((type (configit-dot-type p class types)))
	   (assert (eq type 'boolean) nil (format nil "Found non-boolean term where atom should be: ~S" p))
	   (list (configit-mutate-term p class types) 'true)))	 
	(t (cons (car p) (mapcar #'(lambda (x) (configit-mutate-term x class types)) (cdr p))))))

(defun configit-class-vars (classdefs)
  "(CONFIGIT-CLASS-VARS CLASSDEFS) returns a single hash table keyed on class names
   where each value is a hash table giving the `(scope type) for each internal variable."
  (let (vars (h (make-hash-table)) class)
    (dolist (o classdefs)  
      (setq class (make-hash-table))
      (setq vars (remove-if-not #'(lambda (x) (and (listp x) (eq (first x) 'vardec))) (cddr o)))
      (mapc #'(lambda (x) (setf (gethash (third x) class) (list (second x) (fourth x)))) vars)
      (setf (gethash (second o) h) class))
    h))
	       
(defun configit-mutate-term (term class classhash)
  "(CONFIGIT-MUTATE-TERM TERM CLASSHASH) checks that the term is syntactically valid inside CLASS
   (ignoring scoping) and if not errors; otherwise, it mutates the term."
  (labels ((rewrite-dot (term)
	     (cond ((atom term) term)
		   (t (tosymbol (format nil "~A.~A" (second term) (rewrite-dot (third term))))))))
    (cond ((stringp term) term)
	  ((numberp term) term)
	  ((member term '(true false)) term)
	  ((configit-varp term class classhash) (if (atom term) term (rewrite-dot term)))
	  (t 
	   (assert nil nil 
		   (format nil "Found term inside of class ~A that is neither a boolean, a number, a string, nor a dot-term: ~S" class term))))))

(defun configit-dot-type (term class classhash)
  "(CONFIGIT-DOT-TYPE TERM CLASS CLASSHASH) given a TERM, a CLASSname, and the class-variable-scope/type hash CLASSHASH,
   finds the type of the given term (while ignoring scope)."
  (assert (or (atom term) (eq (car term) 'dot)) nil (format nil "Asked to compute type of non-atom and non-dot: ~A" term))
  (if (atom term)
      (second (configit-var-type term class classhash))
      (configit-dot-type (third term) (second (configit-var-type (second term) class classhash)) classhash)))
  
(defun configit-var-type (var class classhash)
  (let (tmp)
    (setq tmp (gethash class classhash))
    (assert tmp nil (format nil "Asked for type in unknown class ~A" class))
    (setq tmp (gethash var tmp))
    (assert tmp nil (format nil "Asked for type of unknown var ~A in known class ~A" var class))
    tmp))

(defun configit-varp (term class classhash)
  "(CONFIGIT-VARP TERM CLASS CLASSHASH) determines if TERM is a variable (reference) in CLASS.
   Ignores scope."
  (let (upperclass scopetype)
    (setq upperclass (gethash class classhash))
    (if (hash-table-p upperclass)
	(cond ((atom term) (gethash term upperclass))
	      ((eq (car term) 'dot)
	       ; check that the uppermost reference is a local variable
	       (setq scopetype (gethash (second term) upperclass))
	       ; recurse on lower component
	       (and scopetype (configit-varp (third term) (second scopetype) classhash)))
	      (t nil)))))

(defun dotjoin (x y) (tosymbol (format nil "~A.~A" x y)))

(defun configit-flatten-classes (classdefs)
  "(CONFIGIT-FLATTEN-CLASSES CLASSNAME CLASSDEFS) constructs a new class definition that is
   equivalent to CLASSNAME but that includes no fields that are objects; all fields are 
   explicitly-defined types."
  (labels ((flatten (name defs classnames h)
	     ;(format t "~&(flatten ~A ~A ~A ~A)~%" name defs classnames h)
	     (let (vars constraints classvars newvars newconstraints a avars aconstraints bl)
	       (multiple-value-setq (vars constraints) (split #'(lambda (x) (and (listp x) (eq (car x) 'vardec)))
							      (cddr (find name defs :key #'second))))
	       (multiple-value-setq (classvars vars) (split #'(lambda (x) (member (fourth x) classnames)) vars))
	       (dolist (v classvars)
		 (unless (gethash (fourth v) h)  ; unless already flattened
		   (flatten (fourth v) defs classnames h))
		 (setq a (gethash (fourth v) h))
		 (multiple-value-setq (avars aconstraints) (split #'(lambda (x) (and (listp x) (eq (car x) 'vardec))) (cddr a)))
		 
		 (setq bl (mapcar #'(lambda (x) (cons (third x) (dotjoin (third v) (third x)))) avars))
		 ;(setq avars (sublis bl avars))
		 (setq avars (mapcar #'(lambda (x) (list (first x) (second x) (sublis bl (third x)) (fourth x))) avars))
		 (setq aconstraints (sublis bl aconstraints))
		 (setq newvars (nconc newvars avars))
		 (setq newconstraints (nconc newconstraints aconstraints)))
	       ; add to hash
	       (setf (gethash name h) (list* 'class name (nconc vars classvars newvars constraints newconstraints))))))
	       
  (let ((h (make-hash-table)) (names (mapcar #'second classdefs)))
    (dolist (c classdefs)
      (unless (gethash (second c) h)
	(flatten (second c) classdefs names h)))
    (mapcar #'second (hash2list h)))))

(defun configit-class-constraints (o) (remove-if #'configit-vardecp (cddr o)))
(defun configit-vardecp (x) (and (listp x) (eq (car x) 'vardec)))

(defun compress-spreadsheet-object (o)
  "(COMPRESS-SPREADSHEET-OBJECTS O) takes a spreadsheet object and returns 2
   values: a new object but where the constraints have been compressed and a set of datalog statements
   defining the new symbols in the compressed object."
  (multiple-value-bind (vars constraints) (split #'configit-vardecp (cddr o))
    (multiple-value-bind (newc datalog) (mapcaraccum #'compress-to-database constraints)
      (values (list* (first o) (second o) (nconc vars newc)) datalog))))


; testing compression impact on resolution performance
; example of constructing objectpairs for configit-time-compression problem
; (configit-time-compression (configit-create-compressed-objectpairs "/Users/thinrich/Research/projects/spreadsheet/configit/bike.kif") 300)

(defun configit-create-compressed-objectpairs (kiffile)
  (let (objects compressedobjects objectpairs)
    ; ignoring datalog that is also returned by configit-objects-to-spreadsheet-objects
    (setq objects (configit-objects-to-spreadsheet-objects (first (read-file kiffile))))
    (setq compressedobjects (mapcar #'compress-spreadsheet-object objects))
    (setq objectpairs (mapcar #'list objects compressedobjects))
    (setq objectpairs (sort objectpairs #'< :key #'(lambda (x) (length (configit-class-constraints (second x))))))
    objectpairs))

(defun configit-time-compression (objectpairs &key (to 5) (uva t) (outfile "/Users/thinrich/Research/projects/spreadsheet/out-timings.kif"))
  "(CONFIGIT-TIME-COMPRESSION OBJECTPAIRS TO) takes a list of (obj compressedobj) as OBJECTPAIRS,
   a timeout value TO in seconds, and a file for outputting timings.  Runs resolution on constraints of all objects
   and stores timings values in the specified file."
  (flet ((adduva (p)
	   (if uva
	       (let ((mons (remove-if-not #'(lambda (x) (= (parameter-arity x) 1)) (preds p))))
		 (maksand (cons p (mapcar #'(lambda (x) `(<= (= ?x ?y) (,(parameter-symbol x) ?x) (,(parameter-symbol x) ?y))) mons))))
	       p)))	     
    (let (clauses clausems resclauses compressed resms p)
      (dolist (pair objectpairs) 
	; change object pair to constraint pair but with class name prepended; also add uva
	(setq pair (list (second (first pair)) 
			 (adduva (maksand (configit-class-constraints (first pair)))) 
			 (adduva (maksand (configit-class-constraints (second pair))))))
					; make sure no object constants that snark can't handle
	(setf (second pair) (sublis (mapcar #'(lambda (x) (cons x (tosymbol (gentemp "o")))) (objs (second pair))) (second pair) :test #'equalp))
	(setf (third pair) (sublis (mapcar #'(lambda (x) (cons x (tosymbol (gentemp "o")))) (objs (third pair))) (third pair) :test #'equalp))
	(format t "~&Processing ~A ... " (first pair))
	(dotimes (i 2)
	  (if (= i 0) (setq p (third pair) compressed t) (setq p (second pair) compressed nil)) 
	  (with-open-file (f outfile 
			     :direction :output :if-does-not-exist :create :if-exists :append)
	    (multiple-value-setq (clauses clausems) (run-time to #'(lambda () (and2list (cnf p)))))
	    (cond ((eq clauses 'timeout) (setq clauses (list '?)) (setq clausems to) (setq resclauses nil) (setq resms '?))
		  (t
		   (multiple-value-setq (resclauses resms) (run-time to *resolution-closure* clauses 100000))
		   (when (eq resclauses 'timeout) (setq resclauses (list 'timeout)) (setq resms to))))
	    (format f "~&(class ~A compressed ~A complexity ~A cnftime ~A clausecount ~A clausecomplexity ~A restime ~A resclausecount ~A)~%" 
		    (first pair) compressed (complexity p) clausems (length clauses) (complexity (maksand clauses)) resms (length resclauses)))
	  (if (= i 0) (format t "[finished compressed] ") (format t "[finished uncompressed]~%"))) ))))

(defun configit-time-csv (file &optional (stream t))
  (let ((data (read-file file)))
    (dolist (d data)
      (format stream "~&~A, ~A, ~A, ~A, ~A, ~A, ~A, ~A~%" 
	      (second d) (fourth d) (sixth d) (eighth d) (tenth d) (nth 11 d) (nth 13 d) (nth 15 d)))
    nil))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
