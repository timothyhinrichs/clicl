
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
              DATALOG defining basic types, and a hash-table of types.  Returns a new class definition"
	     ;(format t "~&(mutate-class-body ~A)~%" (first o))
	     (let (class vars constraints newconstraints)
	       (setq class (second o))  ; class name
	       (setq o (cddr o))
	       (multiple-value-setq (vars constraints) (split #'(lambda (x) (eq (first x) 'vardec)) o))
	       ; mutate all of the constraint sets
	       (dolist (c constraints)
		 (push (cons 'constraints 
			     (mapcar #'(lambda (x) (configit-mutate-constraint x class datalog classhash))
				     (cdr c)))
		       newconstraints))
	       (nconc vars newconstraints))))
  
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
  (cond ((atom p) p)
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
		   `(forall ?x (<=> ((second p) ?x) (,(third p) ?x)))
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
	  ((member term '(true false)) term)
	  ((configit-varp term class classhash) (if (atom term) term (rewrite-dot term)))
	  (t 
	   (assert nil nil 
		   (format nil "Found term inside of class ~A that is neither a boolean, a string, nor a dot-term: ~S" class term))))))

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
  

(defun configit-flatten-classes (classname classdefs)
  "(CONFIGIT-FLATTEN-CLASSES CLASSNAME CLASSDEFS) constructs a new class definition that is
   equivalent to CLASSNAME but that includes no fields that are objects; all fields are 
   explicitly-defined types."
  (declare (ignore classname classdefs)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
