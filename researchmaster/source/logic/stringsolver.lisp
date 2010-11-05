; all regexps are pcre (perl compatible regular expressions)
; requires Kaluza as a helper, which only runs on Linux
(defun r () 
  (load "/homes/thinrich/tool/solver/ss/stringsolver.lisp")
  (load "/homes/thinrich/tool/solver/ss/stringsolverapps.lisp")
  (load "/homes/thinrich/tool/solver/ss/stringsolvertests.lisp"))

(defvar *ss-max-length* 300 
  "maximum length of a string")
(defvar *ss-max-val* 999 
  "maximum value for a numeric variable")
(defvar *ss-guess-maxlen* t
  "whether or not to guess the value for *ss-max-length* by examining constraints")

(defvar *quiet* t
  "quiet basic progress and error printouts")
(defvar *ss-debug* nil
  "print out debugging info if t")
(defvar *break-on-external-solver-error* t
  "whether or not to break when hampi returns an error")
(defvar *check-dynamic-regexp* t
  "whether or not to ensure that our dynamically created regexps can be translated")
(defvar *command-line* nil
  "whether being executed from the command line.  If NIL, errors result in CCL breaks; otherwise, errors quit.")

; external helper routines
(defparameter *hampi-regtest-tmp* "/tmp/hampiregtest.txt"
  "location for testing regular expressions")
(defparameter *hampi-checkreg-tmp* "/tmp/bar.hmp"
  "location for testing a given string against a regular expression")
(defparameter *hampi-reg* "hampireg" "script for converting pcre regexps to hampi regexps")
(defparameter *hampi-regtest* "hampiregtest" "script for checking regexp translator works")
(defparameter *reg-check* "regcheck" "script that checks whether a given string belongs to a given regexp")

#|
; hampi invocation info: right now using server version 
;     (server version is faster for repeated calls than standalone version)
(defparameter *solver-input-file* "/tmp/ss/b.hmp" 
  "base temp filename for hampi input")
(defparameter *solver-max-storage* 100 
  "max number of files stored on disk for any one solution attempt")
(defparameter *solver-init* "hampi_s")
(defparameter *solver-init-args* '("4445"))
(defparameter *solver-invoke* "hampi_c")
(defparameter *solver-invoke-args* '("4445"))
(defparameter *solver-kill* "hampi_c")
(defparameter *solver-kill-args* '("4445" "SHUTDOWN"))
|#

; Kaluza invocation info
(defvar *solver-input-file* "/tmp/ss/b.kaluza" 
  "base temp filename for Kaluza input")
(defvar *solver-max-storage* 100 
  "max number of files stored on disk for any one solution attempt")
(defvar *store-external-solver-files* nil
  "whether to store all external solver attempts (up to *solver-max-storage*)")

(defparameter *solver-init* "echo")
(defparameter *solver-init-args* '("noop"))
(defparameter *solver-invoke* "kaluza")
(defparameter *solver-invoke-args* nil)
(defparameter *solver-kill* "echo")
(defparameter *solver-kill-args* '("noop"))


; internal globals
(defparameter *reg-special-chars* (list #\\ #\^ #\$ #\( #\) #\[ #\] #\{ #\} #\. #\* #\? #\| #\+ #\-))
(define-condition ss-type-error (error) ((comment :initarg :comment)))

(defun ss-regesc (s &optional (charlist *reg-special-chars*))
  "(SS-REGESC S) translates s so that all special characters are escaped for regular expressions."
  (let ((res nil) char)
    (dotimes (i (length s))
      (setq char (aref s i))
      (if (member char charlist)
	  (push (tostring (list #\\ char)) res)
	  (push char res)))
    (apply #'stringappend (nreverse res))))

(defparameter *charreg* 
  (stringappend "[" (ss-regesc (apply #'stringappend *reg-special-chars*)) "/ \\w!@#%&:=\"><;,'`~" "]"))
(defparameter *boolreg* "true | false")
(defparameter *strreg* (stringappend *charreg* "*"))
(defparameter *strreg1* (stringappend *charreg* "+"))
(defparameter *numreg* "[0-9]+")

(defvar *counter* 1)
(defvar *ss-solve-count* 0)
(defvar *ss-varmapping* nil "binding list for cleansing variables")
(defvar *ss-vars* nil "var list for cleansing variables")
(defvar *external-solver-errored* nil "whether or not the external solver errored")

; input problem structure
; possible statuses: inconsistent, valid, contingent
(defstruct ss-prob 
  ; user-supplied 
  (name nil) phi (space 'true) (types nil)  (unique t) (required t) (init nil)
  (status :contingent) (metafields nil)
  ; internal
  (varnames nil) (variantstatus nil))

(defun ss-hampi-init () (run-program *solver-init* *solver-init-args*) (sleep 1))
(defun ss-hampi-kill () (run-program *solver-kill* *solver-kill-args*))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Exceptional behavior ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun ss-assert (condition dc msg)
  (declare (ignore dc))
  (unless condition (ss-error msg)))

(defun ss-break (msg)
  (ss-error msg))

(defun ss-error (msg)
  (let ((s t))
    (cond (*command-line*
	   (format s "Fatal error. ~%")
	   (print msg s)
	   (quit))
	  (t (break msg)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Test Routines ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; grab a test by name
(defun ss-findtest (name)
  "(SS-TEST NAME) finds the problem with name NAME."
  (find name (ss-deftests) :key #'ss-prob-name))

; test stringsolver on all variants of all test cases  
(defun sst (&key (name nil) (variant nil) (stopearly t) (s t) (extratests t) 
	    (required 'unknown) (errorfile "/tmp/ss/sst.error") (delerrorfile nil))
  "(SST &optional NAME STOPEARLY S) 
    Toplevel testing routine for string solver.  NAME limits the testing to the
    specified test case NAME.  If NAME is not supplied, runs through all test cases.
    STOPEARLY tells it to stop once a failure occurs (default is T), and 
    S is the stream to which results are written.
    If EXTRATESTS is T (the default), also runs solver tests on the negation of each
    problem, and each disjunct of the DNF of the problem and its negation.
    NAME can also be a filename consisting of (formname variant) pairs.
    VARIANT is either a number of a list of numbers."
  (ss-hampi-init)
  (let (variants phi res status i l failures *external-solver-errored* total namehash varis)
    (setq failures 0)
    (setq total 0)
    (setq namehash (make-hash-table))
    (when delerrorfile (delete-file errorfile))

    ; set up data structure recording desired test cases
    (ss-assert (or (not variant) name) nil (format nil "sst requires a test :NAME if :VARIANT is supplied"))
    (cond ((not name))
	  ((stringp name) 
	   (ss-assert (probe-file name) nil (format nil "the filename passed to sst does not exist: ~A" name)) 
	   (setq name (read-file name)))
	  ((and name (symbolp name))
	   (cond ((not variant) (setq name (list (list name t))))
		 ((listp variant) (setq name (mapcar #'(lambda (x) (list name x)) variant)))
		 (t (setq name (list (list name variant))))))
	  (t (ss-break (format nil "sst requires :NAME be either NIL, a filename, or a symbol; found ~A" name))))
    (dolist (n (group-by name #'first))
      (setf (gethash (first n) namehash) (mapcar #'second (cdr n))))

    ; walk over all the tests, running the ones we want.  (Not worth doing better b/c variants are referenced
    ;    by the order in which they are generated by the following code.)
    (dolist (p (ss-deftests) t)
      (setq varis (gethash (ss-prob-name p) namehash))
      (when (or (not name) varis)
	(format s "~&Beginning tests of ~A~%" (ss-prob-name p))
	(setq p (ss-cleanse p))
	(when (eq required 'unknown) (setq required (ss-prob-required p)))
	(setq phi (ss-prob-phi p))
	(setq status (ss-prob-status p))
	(setq variants nil)
	(when extratests 
	  (setq variants (append (list (list (maknot phi) (sst-invert-status status)))
				 (mapcar #'(lambda (x) (list x status)) (or2list (dnf phi)))
				 (mapcar #'(lambda (x) (list x (sst-invert-status status))) 
					 (or2list (dnf (maknot phi)))))))
	(push (list phi status) variants)
	(setq l (length variants))
	(setq i 1)
	(dolist (v variants)
	  (when (or (not name) (member t varis) (member i varis))
	    (format s "~&****** Testing ~S (variant ~A of ~A)~%" (ss-prob-name p) i l)
	    (setq status (second (assoc i (ss-prob-variantstatus p))))
	    (unless status (setq status (second v)))
	    (setq res (sst-aux (ss-prob-name p) i (first v) status (ss-prob-types p) required (ss-prob-space p) s errorfile))
	    (setq total (1+ total))
	    (when (not res) (setq failures (1+ failures)))
	    (when (and stopearly (not res)) (return-from sst nil)))
	  (setq i (1+ i)))))
    (if (= failures 0) 
	(format s "~&Complete Success~%")
	(format s "~&Failure on ~A test~A out of ~A~%" failures (if (> failures 1) "s" "") total))
    (ss-hampi-kill)
    (= failures 0)))

(defun sst-invert-status (x) (case x (:inconsistent :valid) (:contingent :contingent) (:valid :inconsistent)))

(defun sst-aux (name variant phi status types required space s errorfile)
  (when (and required (atom required)) (setq required (ss-guess-required phi types)))
  (let (result correctp (tme (get-universal-time)))
    (setq *external-solver-errored* nil)
    (multiple-value-setq (correctp result) (ss-test-solve phi status types required space))
    (setq tme (- (get-universal-time) tme))
    (cond (correctp
	   (when *ss-debug* 
	     (dolist (v result) (print v s)))
	   (format s "~&****** Success in ~A seconds~%" tme))
	  (t
	   (format s "~&****** Failure in ~A seconds on ~A problem~%" tme status)
	   (format s "EXTERNAL SOLVER ERROR: ~A~%" *external-solver-errored*)
	   (format s "REQUIRED: ~A~%" required)
	   (format s "PHI: ~S~%TYPES: ~%" phi)
	   (dolist (v (hash2bl types)) (print v s))
	   (format s "~&SPACE: ~S~%" space)
	   (format s "~&PROBLEM STATUS: ~A~%" status)
	   (format s "~&SOLVER RETURNED: ~%")
	   (dolist (v result) (print v s))
	   (append-file errorfile (list name variant))))
    correctp))

(defun ss-delimit-regexp (s)
  (tostring (list "'^(" (ss-regesc s '(#\')) ")$'")))

(defun ss-test-all-regexps (p)
  (let (r)
    ; find regexps
    (setq r (find-terms #'(lambda (x) (and (listp x) (eq (car x) 'reg))) p))
    (setq r (mapcar #'second (remove-duplicates r :test #'equal)))
    ; write to a file
    (with-open-file (f *hampi-regtest-tmp* 
		       :direction :output :if-exists :supersede :if-does-not-exist :create)
      (dolist (s r)
	(princ (ss-delimit-regexp s) f)
	(princ #\Newline f)))
    ; run tester and report results
    (setq r (with-output-to-string (strstream)
	      (run-program *hampi-regtest* (list *hampi-regtest-tmp*) :output strstream)))
    (dolist (l (split-string r '(#\Newline)) t)
      (unless (equal l "")
	(unless (search "SUCCESS" l) 
	  (when *ss-debug* (format t "~&Unhandled regexp: ~S~%" l))
	  (return nil))))))

(defun find-terms (predicate p)
  (cond ((atom p) (if (funcall predicate p) (list p) nil))
	((find (car p) '(forall exists)) 
	 (apply #'nconc (mapcar #'(lambda (x) (find-terms predicate x)) (cddr p))))
	((find (car p) '(and or not <= => <=>)) 
	 (apply #'nconc (mapcar #'(lambda (x) (find-terms predicate x)) (cdr p))))
	((funcall predicate p) (list p))
	(t (apply #'nconc (mapcar #'(lambda (x) (find-terms predicate x)) (cdr p))))))

(defun ss-test-solve (p status types required space) 
  (let (bl)
    (setq p (makand p space))
    (setq bl (hash2bl (ss-solve p :types types :required required)))
    (unless *quiet* (format t "~&Solution: ~S~%" bl))
    (cond ((eq status :inconsistent) (values (not bl) bl))
	  ((not bl) (values nil nil))
	  (t (values (ss-checksat p types (append bl '((t . t)))) bl)))))

(defun ss-checksat (p types bl)
  (and (ss-checksat-types bl types)
       (eq (handler-case (ss-evaluate (plug p bl))
	     (ss-type-error () 'false))
	   'true)))

(defun ss-checksat-types (bl types)
  (let (y)
    (dolist (b bl t)
      (when (varp (car b))
	(setq y (gethash (car b) types))
	(when y
	  (cond ((eq (car y) 'typedecl)
		 (cond ((eq (third y) 'bool) 
			(unless (member (cdr b) '(true false))
			  (when *ss-debug* (format t "~&Checksat type failure: ~A should be bool" (cdr b)))
			  (return nil)))
		       ((eq (third y) 'num)
			(unless (numberp (cdr b))
			  (when *ss-debug* (format t "~&Checksat type failure: ~A should be num" (cdr b)))
			  (return nil)))
		       ((eq (third y) 'str)
			(unless (stringp (cdr b))
			  (when *ss-debug* (format t "~&Checksat type failure: ~A should be str" (cdr b)))
			  (return nil)))
		       (t 
			(when *ss-debug* (format t "~&Checksat type failure: unknown typedecl ~A" (third y)))
			(return nil))))			
		(t
		 (unless (ss-checkreg (ss-cast-tostring (cdr b)) (ss-makreg (third y)))
		   (when *ss-debug* (format t "~&Checksat type failure: ~A~%" b))
		   (return nil)))))))))
#|
; not sure why I thought this was useful.  Now we just simplify.
(defun ss-checksat-aux (p)
  (3val-true (3val-rewrite p)))

(defun 3val-unknown (x) (eq x 'unknown))
(defun 3val-true (x) (and x (not (3val-unknown x))))
(defun 3val2num (v) 
  (cond ((3val-true v) 3)
	((3val-unknown v) 2)
	(t 1)))
(defun num23val (num) (case num (1 nil) (2 'unknown) (3 t)))
(defun 3val-not (val) (if (3val-unknown val) 'unknown (not val)))
(defun 3val-rewrite (p)
  (cond ((atom p) p)
	((eq (car p) 'and) (num23val (apply #'min (mapcar #'3val2num (mapcar #'3val-rewrite (cdr p))))))
	((eq (car p) 'or) (num23val (apply #'max (mapcar #'3val2num (mapcar #'3val-rewrite (cdr p))))))
	((eq (car p) 'not) (3val-not (3val-rewrite (second p))))
	((not (groundp p)) 'unknown)
	(t (ss-simplify p))))

(defun ss-checksat-aux-simplify (p)
  "(SS-CHECKSAT-AUX-SIMPLIFY P) returns T/NIL for sentences or a value
   for a term, assuming P is ground."
  (cond ((atom p) p)
	((eq (car p) 'len) (length (second p)))
	((eq (car p) 'concat)
	((find (car p) '(contains notcontains))
	 (setq p (ss-cast p))
	 (let (v)
	   (setq v (search (third p) (second p)))
	   (if (eq (car p) 'contains) v (not v))))
	((find (car p) '(in notin nin)) 
	 (setq p (ss-cast p))
	 (let (v)
	   (setf v (ss-checkreg (second p) (second (third p))))
	   (if (eq (car p) 'in) v (not v))))
	(t
	 (setq p (ss-cast (cons (car p) (mapcar #'ss-checksat-aux-simplify (cdr p)))))
	 (when (eq p 'error) (return-from ss-checksat-aux-simplify nil))
	 (case (car p)
	   (= (equal (second p) (third p)))
	   (!= (not (equal (second p) (third p))))
	   (lt (< (second p) (third p)))
	   (gt (> (second p) (third p)))
	   (lte (<= (second p) (third p)))
	   (gte (>= (second p) (third p)))
	   (otherwise nil)))))
|#



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; String Solving ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun stringfindx (thing p th)
  (let (h)
    (setq h (ss-solve (makand p (maksand (contents th))) :required (vars thing)))
    (if h (plug thing (nconc (hash2bl h) truth)) nil))) 

(defun ss-solve (p &key (types nil) (required t) (forbidden nil))
  "(SS-SOLVE P) is the top-level solver routine.  P is assumed to be a logical
   KIF expression. SS-SOLVE converts P to DNF  and solves each conjunction independently.
   TYPES is either NIL or a list of IN expressions giving types for each variable.
   REQUIRED is a list of variables that are required to have values or T to indicate
   all variables occurring in P.  FORBIDDEN is a list of variables not allowed to
   be assigned values.  Return is a hash table of mappings from variables to values
   for all variables either occurring in P or belonging to REQUIRED.  "
  (setq *ss-solve-count* (1+ *ss-solve-count*))
  (let (e (*ss-max-length* *ss-max-length*))
    (when (eq required 't) (setq required (vars p)))
    (when (and types (not (hash-table-p types)))  ; when types is a list
      (setq e (make-hash-table))
      (mapc #'(lambda (x) (setf (gethash (second x) e) x)) types)
      (setq types e))
#|
    (when types  ; ensure all vars in p have a type -- might need this if Kaluza buggy
      (setq e (vars (hash2list types))) 
      (setq e (set-difference (vars p) e))
      (mapc #'(lambda (x) (setf (gethash x types) `(in ,x (reg ,*strreg*)))) e))
|#

    ; Ugly hack to ensure bool/num types are satisfied.  If solver allowed typedecls, wouldn't need this.
    ;     There's no good way to declare boolean or numeric variables to Kaluza.
    ;     Instead of adding a boolean variable decl, we simply add a toplevel disjunct var=true | var=false.
    ;     Instead of adding a numeric variable decl, we add var < 0 | var >= 0.
    (dolist (y (hash2bl types))
      (setq y (cdr y))
      (when (eq (car y) 'typedecl)
	(cond ((eq (third y) 'num)
	       (setq p (makand `(or (gte ,(second y) 0) (lt ,(second y) 0)) p)))
	      ((eq (third y) 'bool)
	       (setq p (makand `(or (= ,(second y) true) (= ,(second y) false)) p))))))

    (unless *store-external-solver-files* (setq *counter* 1))

    ;(setq e (ss-invalid-inputs p))
    ;(when e 
    ;  (unless *quiet* (format t "Syntax error: ~A~%" e))
    ;  (return-from ss-solve :error))

    ; convert to dnf, remove syntactic sugar, convert to DNF again, and solve disjuncts independently.
    ;    Seems faster to convert to DNF twice.
    (dolist (origconj (or2list (dnf p)) nil)
      (dolist (c (or2list (dnf (ss-drop-syntactic-sugar origconj))))
	(setq c (copy-tree c))   ; so that we can use destructive ops below
	(handler-case (setq c (ss-solve-atoms (mapcar #'ss-simplify-not (and2list c)) types required forbidden))
	  (ss-type-error () (setq c nil)))
	(when c (return-from ss-solve c))))))

#| Version with Hampi   
(defun ss-solve (p &key (types nil) (required t))
  "(SS-SOLVE P) is the top-level solver routine.  P is assumed to be a logical
   KIF expression. SS-SOLVE converts P to DNF  and solves each conjunction independently.
   TYPES is either NIL or a list of IN expressions giving types for each variable.
   REQUIRED is a list of variables that are required to have values or T to indicate
   all variables occurring in P.  Return is a hash table of mappings from variables to values
   for all variables either occurring in P or belonging to REQUIRED.  "
  (setq *ss-solve-count* (1+ *ss-solve-count*))
  (let (e (*ss-max-length* *ss-max-length*))
    (when *ss-guess-maxlen* (setq *ss-max-length* (ss-guess-maxlen p)))
    (when (and required (atom required)) (setq required (ss-guess-required p types)))
    (when (and types (not (hash-table-p types)))
      (setq e (make-hash-table))
      (mapc #'(lambda (x) (setf (gethash (second x) e) x)) types)
      (setq types e))
    (when types  ; ensure all vars in p have a type
      (setq e (vars (hash2list types))) 
      (setq e (set-difference (vars p) e))
      (mapc #'(lambda (x) (setf (gethash x types) `(in ,x (reg ,*strreg*)))) e))

    (setq *counter* 1)
    ; check syntax
    (setq e (ss-invalid-inputs p))
    (when e 
      (unless *quiet* (format t "Syntax error: ~A~%" e))
      (return-from ss-solve :error))

    ; convert to dnf and solve independently
    (dolist (c (or2list (dnf p)))
      (setq c (copy-tree c))   ; so that we can use destructive ops below
      (setq c (ss-solve-atoms (mapcar #'ss-simplify-not (and2list c)) types required))
      (when c (return c)))))
|#

(defun ss-drop-syntactic-sugar (p) (ss-drop-sugar-sent p))
(defun ss-drop-sugar-sent (p) (mapopands #'ss-drop-sugar-atom p))

(defun ss-drop-sugar-atom (p)
  (cond ((varp p) `(= ,p true))
	((atom p) p)
	(t (multiple-value-bind (args extra) (mapcaraccum #'ss-drop-sugar-term (cdr p))
	     (setq p (cons (car p) args))
	     (setq p (ss-drop-sugar p (car p)))
	     (setq extra (mapcar #'ss-drop-sugar-sent extra))
	     (maksand (cons p extra))))))

(defun ss-drop-sugar-term (term)
  (cond ((atom term) (ss-drop-sugar term term))
	(t (multiple-value-bind (args extra) (mapcaraccum #'ss-drop-sugar-term (cdr term))
	     (setq term (cons (car term) args))
	     (multiple-value-bind (newterm extra2) (ss-drop-sugar term (signifier term))
	       (values newterm (nconc extra extra2)))))))
#|
(defmethod ss-drop-sugar (term (type (eql 'unflatterm)))
  (multiple-value-bind (newterm extra) (flatten-term term)
    (when extra
      (multiple-value-bind (sentnew sentextra) (ss-drop-sugar (maksand extra) 'sentence)
	(setq extra (cons sentnew sentextra))))
    (values newterm extra)))
|#

(defgeneric ss-drop-sugar (term type))
;  "(SS-DROP-SUGAR TERM TYPE) removes the sugar from TERM, which is either
;   an atom or a functional term whose arguments have already been desugared.
;   Returns 2 values: the new term and a list of supporting statements.")

(defmethod ss-drop-sugar (term (type (eql 'ltrim)))
  (let ((thing (second term))
	(chars (if (cddr term) (third term) '("\\t" "\\n" "\\r" " "))))
    (let ((y (newindvar)) 
	  (z (newindvar)) 
	  (white (stringappend "[" (apply #'stringappend chars) "]"))) 
      (values y `((= ,thing (concat ,z ,y))
		  (in ,z (reg ,(stringappend white "*")))
		  (nin ,y (reg ,(stringappend "^" white "+"))))))))

(defmethod ss-drop-sugar (term (type (eql 'rtrim)))
  (let ((thing (second term))
	(chars (if (cddr term) (third term) '("\\t" "\\n" "\\r" " "))))
    (let ((y (newindvar)) 
	  (z (newindvar)) 
	  (white (stringappend "[" (apply #'stringappend chars) "]"))) 
      (values y `((= ,thing (concat ,y ,z))
		  (in ,z (reg ,(stringappend white "*")))
		  (nin ,y (reg ,(stringappend white "+$"))))))))

(defmethod ss-drop-sugar (term (type (eql 'trim)))
  (if (cddr term)
      (ss-drop-sugar-term (list 'ltrim (cons 'rtrim (third term)) (third term)))
      (ss-drop-sugar-term `(ltrim (rtrim ,(second term))))))

(defmethod ss-drop-sugar (term (type (eql 'chop)))
  (ss-drop-sugar term 'rtrim))

(defmethod ss-drop-sugar (term (type (eql 'strlen)))
  (list 'len (second term)))

(defmethod ss-drop-sugar (term (type (eql 'strpos)))
  (let* ((leftvar (newindvar))
	 (remainvar (newindvar))
	 (sents))
    (setq sents
	  `((= ,(second term) (concat (concat ,leftvar ,(third term)) ,remainvar))))
    (when (cdddr term)
      (push `(gte (len ,leftvar) ,(fourth term)) sents))
    (values `(len ,leftvar) sents)))

(defmethod ss-drop-sugar (term (type (eql 'strstr)))
  (let* ((leftvar (newindvar))
	 (remainvar (newindvar))
	 (sents))
    (setq sents
	  `((= ,(second term) (concat (concat ,leftvar ,(third term)) ,remainvar))))
    (values `(concat ,(third term) ,remainvar) sents)))

(defmethod ss-drop-sugar (term (type (eql 'substr)))
  ; (substr string start <length>)
  (let* ((leftvar (newindvar))
	 (returnvar (newindvar))
	 (rightvar (newindvar))
	 sents)
    (if (fourth term)
	(setq sents `((= ,(second term) (concat ,leftvar (concat ,returnvar ,rightvar)))
		      (= (len ,leftvar) ,(third term))
		      (= (len ,returnvar) ,(fourth term))))
	(setq sents `((= ,(second term) (concat ,leftvar ,returnvar))
		      (= (len ,leftvar) ,(third term)))))
    (values returnvar sents)))

(defmethod ss-drop-sugar (term (type (eql 'substr_replace)))
  ; substr_replace(string,replacement,start,<length>)
  ;  assume start>=0 and length>=0 since underlying solver has only positive numbers.
  (let* ((leftvar (newindvar))
	 (centervar (newindvar))
	 (rightvar (newindvar))
	 sents)
    (setq sents `((= ,(second term) (concat ,leftvar (concat ,centervar ,rightvar)))
		  (= (len ,leftvar) ,(fourth term))
		  ,(if (fifth term)
		       `(= (len ,centervar) ,(fifth term))
		       `(= (len ,centervar) (len ,(third term))))))
    (values `(concat ,leftvar (concat ,(third term) ,rightvar))
	    sents)))

(defmethod ss-drop-sugar (term (type (eql 'empty)))
   ; PHP 5 notion of empty (skipping NULL and objects)
  `(or (= ,(second term) "")
       (= ,(second term) 0)
       (= ,(second term) "0")
       (= ,(second term) false)
       (forbid ,(second term))))

(defmethod ss-drop-sugar (term (type (eql 'isset)))
  ; PHP 5 notion of isset
  `(require ,(second term)))

(defmethod ss-drop-sugar (term type)
  (declare (ignore type))
  term)
#|
  (cond ((atom term) term)
	(t
	 (multiple-value-bind (args extra) 
	     (mapcaraccum #'(lambda (x) (ss-drop-sugar x (signifier x))) (cdr term))
	   (values (cons (car term) args) extra)))))
|#


;;;;;;;;;;;; Vocabulary checking ;;;;;;;;;;;;
;  OUT OF DATE--NOT BEING USED CURRENTLY.
; only tricky thing.  regexp "L" represented as (reg "L")
; variables represented as ?x
(defparameter *ssvocab* (list (make-parameter :symbol 'lt :arity 2 :type 'relation)
			      (make-parameter :symbol 'lte :arity 2 :type 'relation)
			      (make-parameter :symbol 'gt :arity 2 :type 'relation)
			      (make-parameter :symbol 'gte :arity 2 :type 'relation)
			      (make-parameter :symbol '= :arity 2 :type 'relation)
			      (make-parameter :symbol '!= :arity 2 :type 'relation)
			      (make-parameter :symbol 'in :arity 2 :type 'relation)
			      (make-parameter :symbol 'notin :arity 2 :type 'relation)
			      (make-parameter :symbol 'nin :arity 2 :type 'relation)
			      (make-parameter :symbol 'reg :arity 1 :type 'function)
			      (make-parameter :symbol 'len :arity 1 :type 'function)
			      (make-parameter :symbol 'concat :arity 2 :type 'function)
			      (make-parameter :symbol 'contains :arity 2 :type 'relation)
			      (make-parameter :symbol 'notcontains :arity 2 :type 'relation)))

(defun ss-invalid-inputs (p) 
  (if (not (quantifier-free-sentencep p)) 
      'quantifiers
      (set-difference (remove-if #'isobject (get-vocabulary p))
		      *ssvocab*
		      :test #'equalp)))



;;;;;;;;;;;; Conjunctive Constraint solving ;;;;;;;;;;;;
; actual solving.

(defun ss-solve-atoms (ps types &optional required forbidden)
  "(SS-SOLVE-ATOMS PS TYPES REQUIRED) uses External Solver to find a solution to the atoms given as input.
   Each atom takes the following form or one of the symmetric instances of the below.
   REQUIRED is the minimal set of variables that must be given values.  
   FORBIDDEN is a set of vars not allowed to be assigned values.
   Returns a variable assignment including all
   variables occurring in REQUIRED plus those occurring in PS (to witness satisfiability).
   Tailored for Kaluza.
       numop :=  lt | lte | gt | gte
       eqop  :=  = | !=
       regop :=  in | notin | nin
       op    :=  numop | eqop | regop
       term  :=  var | str | num | bool | (len term) | (concat term term)
       atom  :=  (op term term)"
  (let (bl bl2 blvars psvars solvervars h vs r f)
    ; rip out require/forbid atoms and add to required/forbidden lists
    (multiple-value-setq (r ps) (split #'(lambda (x) (member (reln x) '(require forbid))) ps))
    (multiple-value-setq (r f) (split #'(lambda (x) (eq (reln x) 'require)) r))
    (setq required (union (mapcarnot #'(lambda (x) (if (varp (second x)) (second x) nil)) r) required))
    (setq forbidden (union (mapcarnot #'(lambda (x) (if (varp (second x)) (second x) nil)) f) forbidden))

    ; simplify constraints, evaluating what we can
    (setq ps (ss-solve-atoms-simplify ps))
    (when (not (listp ps)) (return-from ss-solve-atoms nil))
    (setq psvars (vars ps))
    (when (intersectionp psvars forbidden) (return-from ss-solve-atoms nil))  ; if var shows up in PS, we're finding value
    (setq ps (and2list (flatten-operator (flatten-functions (maksand ps) t))))

    ; remove equality for (= var ground); leave (= var unground) since we need flat terms for Kaluza.
    (multiple-value-setq (ps bl) (delete-simple= ps :test #'equal))
    (when (eq ps :unsat) (return-from ss-solve-atoms nil))
    ; remove != for boolean variables with ground arg
    (multiple-value-setq (ps bl2) (delete!=bool ps :test #'equal))
    (when (eq ps :unsat) (return-from ss-solve-atoms nil))
    ; remove duplicates
    (setq ps (remove-duplicates ps :test #'equal))
    ; combine variable bindings
    (setq bl (compose-mgus (list bl bl2)))

    ; simplify again
    (setq ps (ss-solve-atoms-simplify ps))
    (when (eq ps :unsat) (return-from ss-solve-atoms nil))

    ; special case: translate ?x != "" to (> (len ?x) 0)
    (setq ps (mapcar #'ss-tr-noteq-empty ps))

    ; compute important variable sets
    ;  psvs: variables in PS, originally
    ;  blvs: variables in bl that can be assigned given values for other vars in PS
    ;  solvervars: (psvs + required) - blvs (the set of variables we need to solve for)
    ;  Note: we need to solve for all of SOLVERVARS to ensure the entire set of formulas is satisfiable.
    (setq blvars (delete t (mapcar #'first bl)))
    (setq solvervars (set-difference (union* psvars required) blvars))

    ; add user-specified typing constraints for solver variables.
    (when types
      (dolist (v solvervars)
	(when (setq v (gethash v types))
	  (unless (eq (car v) 'typedecl)   ; already taken care of by ss-solve
	    (push v ps)))))

    ; add types for vars without types; otherwise Kaluza chokes
    (dolist (v (set-difference solvervars (mapcar #'car (ss-type-inference ps))))
      (push `(in ,v ,*strreg*) ps))

    ; add types for vars only appearing in functional terms; otherwise, Kaluza ignores them (bug in Kaluza)
    (setq vs (copy-list solvervars))
    (dolist (p ps)
      (when (listp p)
	(dolist (a (cdr p))
	  (when (varp a) (setq vs (delete a vs))))))
    (dolist (v vs)  ; all vars appearing in functional terms are string variables
      (push `(in ,v ,*strreg*) ps))

    ; crunch the numbers
    (setq h (ss-invoke-kaluza solvervars ps))
    (if (eq h :unsat)
	nil
	(ss-augment-result3 (bl2hash h) bl required))))

#|
(defun ss-solve-atoms-hampi (ps types required)
  "(SS-SOLVE-ATOMS PS TYPES REQUIRED) uses Hampi to find a solution to the atoms given as input.
   Each atom takes the following form or one of the symmetric instances of the below.
   REQUIRED is the minimal set of variables that must be given values.  TYPES is a hash
   table from variables to IN constraints.  Returns a variable assignment including all
   variables occurring in REQUIRED plus those occurring in PS (to witness satisfiability).
       numop :=  lt | lte | gt | gte
       eqop  :=  = | !=
       regop :=  in | notin 
       op    :=  numop | eqop | regop
       term  :=  var | str | num | bool | (len term) | (reg term)
       atom  :=  (op term term)"
  (let (len range unsat reason errs bl bl2 blvars psvars solvervars h)
    ; simplify constraints, evaluating what we can
    (setq ps (ss-solve-atoms-simplify ps))
    (when (not (listp ps)) (return-from ss-solve-atoms nil))

    ; remove equality mentioning at least one variable. 
    (multiple-value-setq (ps bl) (delete= ps))
    ; remove != for boolean variables with ground arg
    (multiple-value-setq (ps bl2) (delete!=bool ps))
    ; remove duplicates
    (setq ps (remove-duplicates ps :test #'equal))
    ; combine variable bindings
    (setq bl (compose-mgus (list bl bl2)))

    ; simplify again
    (setq ps (ss-solve-atoms-simplify ps))
    (when (eq ps 'unsat) (return-from ss-solve-atoms nil))

    ; special case: translate ?x != "" to (> (len ?x) 0)
    (setq ps (mapcar #'ss-tr-noteq-empty ps))

    ; compute important variable sets
    ;  psvs: variables in PS
    ;  blvs: variables in bl that can be assigned given values for other vars in PS
    ;  solvervars: (psvs + required) - blvs (the set of variables we need to solve for)
    ;  Note: we need to solve for all of SOLVERVARS to ensure the entire set of formulas is satisfiable.
    (setq psvars (vars ps))
    (setq blvars (delete t (mapcar #'first bl)))
    (setq solvervars (set-difference (union* psvars required) blvars))

    ; unless types are provided, compute them for all variables.
    (unless types
      (multiple-value-setq (types errs) (ss-type-inference ps bl))
      (when errs
	(unless *quiet*
	  (format t "~&Type errors found.  Aborting.~%")
	  (dolist (e errs) (format t "  ~S~%" e)))
	(return-from ss-solve-atoms 'error)))
    (when *ss-debug*
      (format t "Types:~%")
      (pretty-print types))

    ; compute length bounds and reduce PS
    (multiple-value-setq (ps len unsat reason) (ss-len-bounds ps))
    (when *ss-debug*
      (format t "~&unused: ~S~%" ps)
      (format t "len bounds:~%")
      (pretty-print len))
    (when unsat 
      (unless *quiet* 
	(format t "~&Unsatisfiability found during len bound computation:  ~&  ~S~%" reason))
      (return-from ss-solve-atoms nil))

    ; compute range bounds (implying numeric variables) and reduce PS
    (multiple-value-setq (ps range unsat reason) (ss-range-bounds ps))
    (when *ss-debug*
      (format t "unused: ~S~%" ps)
      (format t "range bounds:~%")
      (pretty-print range))
    (when unsat 
      (unless *quiet* 
	(format t "~&Unsatisfiability found during range bound computation:  ~&  ~S~%" reason))
      (return-from ss-solve-atoms nil))

    ; add len/range for solvervars that have no bounds so far
    (multiple-value-setq (len range unsat reason) (ss-solve-lenrange solvervars len range))
    (when *ss-debug*
      (format t "final len bounds:~%")
      (pretty-print len)
      (format t "final range bounds:~%")
      (pretty-print range))
    (when unsat
      (unless *quiet*
	(format t "~&Unsatisfiability found during len/range bound combination:  ~& ~S~%" reason))
      (return-from ss-solve-atoms nil))

    ; Translate var != str into (notin var (reg "str"))
    (setq ps (mapcar #'ss-tr-noteq ps))  

    ; add guards ensuring all variables we might solve for are typed correctly.
    ;   this doesn't change which variables we solve for because the sentences we add don't change
    ;     the connected components.
    (dolist (v solvervars)
      (push (gethash v types) ps))

    ; crunch the numbers
    (setq h (ss-solve-iterate2 (mapcar #'ss-translate-regexps ps) solvervars len range))
    (when h (setq h (ss-augment-result h bl required len range)))
    h))
|#

(defun ss-solve-atoms-simplify (ps)
  "(SS-SOLVE-ATOMS SIMPLIFY PS) takes a list of atoms and simplifies them.  Throws ss-type-error
   if an error was found.  Returns :unsat if unsatisfiability discovered.  Otherwise, returns
   simplification of PS."
  (let (simps)
    (setq simps (mapcar #'(lambda (x) (ss-simplify (ss-orient x))) ps))
    (when (member 'false simps)
      (unless *quiet*
	(format t "~%Unsatisfiability found during simplification.  Simplified result: ~&  ~S~%" simps))
      (return-from ss-solve-atoms-simplify :unsat))
    (delete 'true simps)))

(defun ss-tr-noteq (p)
  "(SS-TR-NOTEQ P) turns (!= ?x <atom>) into a regular expression constraint.  
   Assumes p has been oriented and that <atom> is not the empty string."
  (cond ((atom p) p)
	((and (eq (car p) '!=) (varp (second p)) (atom (third p)) (not (varp (third p))))
	 (ss-assert (not (equal (third p) "")) nil 
		    (format nil "~&ss-tr-noteq encountered the empty string in: ~S~%" p))
	 `(nin ,(second p) ,(ss-makreg (ss-regesc (ss-cast-tostring (third p))))))
	(t p)))

(defun ss-tr-noteq-empty (p)
  "(SS-TR-NOTEQ-EMPTY P) turns (!= ?x "") into (gt (len ?x) 0).  
   Assumes P has been oriented."
  (cond ((atom p) p)
	((and (eq (car p) '!=) (varp (second p)) (equal (third p) ""))
	 (ss-orient `(gt (len ,(second p)) 0)))
	(t p)))

(defun delete!=bool (lits &key (test #'eq))
  "(DELETE!=BOOL LITS) takes a list of literals and returns a logically equivalent list of literals
   without any literals of the form x != <bool> where x is a variable and <bool> is either true or false
   (and the binding list as a second value).  Destructive.  Also removes any ground != ground that
   are true, returning :UNSAT if false."
  (let (l var val)
    ; find a substitution to perform
    (setq l (find-if #'(lambda (x) (and (positive-literalp x) (eq (relation x) '!=) 
					(or (and (varp (second x)) (boolp (third x))) 
					    (and (varp (third x)) (boolp (second x)))
					    (and (groundp (second x)) (groundp (third x))))))
		     lits))
    (cond ((not l) (values lits '((t . t))))
	  ((and (groundp (second l)) (groundp (third l)))
	   (if (funcall test (second l) (third l))
	       (values :unsat nil)
	       (delete!=bool (delete l lits :test #'equal) :test test)))
	  (t 
	   (if (varp (second l)) 
	       (setq var (second l) val (maknot (third l))) 
	       (setq var (third l) val (maknot (second l))))
	   (multiple-value-bind (newlits bl) 
	       (delete!=bool (nsubst val var (delete l lits :test #'equal)) :test test)
	     (values newlits (acons var val bl)))))))

(defun ss-solve-atoms-makesafe (ps types)
  "(SS-SOLVE-ATOMS-MAKESAFE PS TYPES) ensures all variables occurring in
   PS are constrained to the correct type.  This handles a current limitation of 
   Hampi--the need for all variables to be safe, i.e. variables cannot only appear 
   within NOTIN statements."
  ; find unsafe vars
  (let ((vs (vars ps)) pos)

    ; grab all positive vars
    (setq pos (mapcarnot #'(lambda (x) (if (eq (car x) 'in) (second x) nil)) ps))

    ; add a guard for each variable that is positive
    (dolist (v vs)
      (unless (member v pos)
	(push (gethash v types) ps)))
    ps))

(defun ss-augment-result (h bl vars len range)
  (let (val)
    (dolist (v vars h)
      (setq val (ss-augment-result-val v h bl))
      (when (varp val) (setq val (ss-augment-result-var v len range)))
      (if val
	  (setf (gethash v h) val)
	  nil))))

(defun ss-augment-result3 (h bl vars)
  (let (val)
    (dolist (v vars h)
      (setq val (ss-augment-result-val v h bl))
      (when (varp val) (setq val "42"))  ; no constraints on var => can pick arbitrary value
      (if val
	  (setf (gethash v h) val)
	  nil))))

(defun ss-augment-result-var (var len range)
  (let (l r)
    (setq l (gethash var len))
    (setq r (gethash var range))
    (cond ((and (not l) (not r)) "42")
	  ((and (not l) r) (first (first r)))
	  ((and l (not r)) (apply #'stringappend (maptimes #'(lambda () "a") (first (first l)))))
	  (t (ss-solve-len-range l r)))))

(defun ss-solve-len-range (l r)
  "(SS-SOLVE-LEN-RANGE L R) takes a length and a range bound, each of the form
     ((low high) forbid1 ... forbidn) and finds a solution."
  (let (llen rlen bound forbidden low high rforbid unsat)
    ; compute len bounds for range
    (setq llen (length (tostring (first (first r)))))
    (setq rlen (length (tostring (second (first r)))))
    ; find new len bounds
    (multiple-value-setq (bound unsat) (ss-bounds-merge (first l) (list llen rlen)))
    (when unsat (return-from ss-solve-len-range nil))
    (multiple-value-setq (bound forbidden) (ss-bounds-adj bound (cdr l)))
    (when (not bound) (return-from ss-solve-len-range nil))
    ; loop from the lowest number in the new len range until find one that is not forbidden
    ;    or we hit the end of the range.
    (setq low (max (ss-num-of-len (first bound)) (first (first r))))
    (setq high (1+ (min (1- (ss-num-of-len (1+ (second bound)))) (second (first r)))))
    (setq rforbid (cdr r))
    (print low) (print high) (print rforbid)
    (do ((i low (1+ i)))
	((= i high) nil)
      (when (and (not (member i rforbid)) (not (member (length (tostring i)) forbidden)))
	(return (tostring i))))))

(defun ss-num-of-len (n)
  (read-from-string 
   (apply #'stringappend 
	  (cons "1" (maptimes #'(lambda () "0") (1- n))))))

(defun ss-augment-result-val (var h bl)
  (let (val val2)
    (cond ((setq val (gethash var h)) val)
	  ((setq val (cdr (assoc var bl))) 
	   (cond ((varp val)
		  (ss-augment-result-val val h bl))
		 ((lenp val)
		  (setq val2 (ss-augment-result-val (second val) h bl))
		  (if (stringp val2)
		      (length val2)
		      `(len ,val2)))
		 (t val)))
	  (t var))))

(defun ss-translate-regexps (p)
  (cond ((atom p) p)
	((eq (car p) 'reg) (multiple-value-bind (name def) (ss-translate-regexp (second p))
			     (list 'reg name def (second p))))
	(t (cons (car p) (mapcar #'ss-translate-regexps (cdr p))))))

(defun ss-translate-regexp (str)
  (let (n out)
    (setq n (tostring (gentemp "tlh")))
    (setq out (with-output-to-string (strstream) 
		(run-program *hampi-reg* (list (stringappend "'" (ss-regesc str '(#\')) "'") n) 
			     :output strstream)))
    (ss-assert (and (not (search "error" out))
		 (not (search "pcre_tohampi" out)))
	    nil (format nil "Couldn't translate regexp ~S~%" str))
    (values (stringappend n "_flax" 0) out)))

;;;;;;;;;;;; Simplification ;;;;;;;;;;;;

(defun ss-orient (p)
  "(SS-ORIENT P) orients atomics so atoms on left.  If both atoms, var is on left."
  (cond ((atom p) p)
	((find (car p) '(<= => or and not <=> forall exists))
	 (cons (car p) (mapcar #'ss-orient (cdr p))))
	; (op non-atom atom)
	((find (car p) '(in notin nin contains notcontains require forbid)) p)
	((and (not (atom (second p))) (atom (third p)))
	     (list (ss-symmetric (car p)) (third p) (second p)))
	; (op non-atom-non-var var)
	((and (atom (second p)) (atom (third p)) 
	      (not (varp (second p))) (varp (third p)))
	 (list (ss-symmetric (car p)) (third p) (second p)))
	(t p)))

(defun ss-symmetric (op)
  (case op
    (gt 'lt)
    (lt 'gt)
    (gte 'lte)
    (lte 'gte)
    (= '=)
    (!= '!=)
    (otherwise 'noop)))

(defun boolp (x) (member x '(true false)))
(defun lenp (x) (and (listp x) (eq (first x) 'len)))
(defun regp (x) (stringp x))  ;(and (listp x) (eq (first x) 'reg)))
(defun concatp (x) (and (listp x) (eq (first x) 'concat)))

(deftheory ss-grammer ""
  (str "abc")
  (num 2)
  (bool true)
  (var '?x)
  (<= (flatterm ?x) (or (str ?x) (num ?x) (bool ?x) (var ?x)))
  (<= (lenp (len ?x)) (flatterm ?x))
  (<= (regp (reg ?x)) (flatterm ?x))
  (<= (term ?x) (or (flatterm ?x) (lenp ?x) (regp ?x)))
  (numop lt)
;  (numop gt)
;  (numop lte)
;  (numop gte)
;  (eqop =)
  (eqop !=)
  (regop in)
;  (regop notin)
  (<= (op ?x) (or (numop ?x) (eqop ?x) (regop ?x)))
  (<= (atom (?x ?y ?z)) (op ?x) (term ?y) (term ?z)))
  
(defun ss-evaluate (p)
  "(SS-EVALUATE P) assumes P is ground with the exception of modals.  Returns true or false."
  (labels ((e (p)
	     (cond ((atom p) p)
		   ((find (car p) '(or and not)) (remove-truth1 (cons (car p) (mapcar #'e (cdr p)))))
		   ((eq (car p) 'require) (if (varp (second p)) 'false 'true))
		   ((eq (car p) 'forbid) (if (varp (second p)) 'true 'false))
		   (t p))))
    (e (ss-simplify p))))


(defun ss-simplify (p)
  "(SS-SIMPLIFY P) Evaluates p if it is a ground atom to either true or false.
   Returns new p or throws an ss-type-error.  Also casts where necessary."
  ;(declare (notinline ss-simplify))
  (cond ((atom p) p)
	((find (car p) '(or and not))
	 (remove-truth1 (cons (car p) (mapcar #'ss-simplify (cdr p)))))

	; concat
	((concatp p)
	 (setq p (cons (car p) (mapcar #'(lambda (x) (ss-cast-tostring (ss-simplify x))) (cdr p))))
	 (if (every #'stringp (cdr p))
	     (apply #'stringappend (cdr p)) 
	     p))

	; len
	((lenp p)
	 (setq p (ss-cast-tostring (ss-simplify (second p))))
	 (cond ((stringp p) (length p))
	       ((varp p) `(len ,p))
	       (t (error 'ss-type-error)))) 

	; modals
	((eq (car p) 'require) (if (varp (second p)) p 'true))
	((eq (car p) 'forbid) (if (varp (second p)) p 'false))

	; relational operators
	((member (car p) '(= != lt lte gt gte in notin nin contains notcontains))
	 (setq p (cons (car p) (mapcar #'ss-simplify (cdr p))))
	 (setq p (ss-cast p))  ; get types right to the extent that we can
	 (cond ((and (not (varp (second p))) (not (varp (third p))))
		(cond ((and (atom (second p)) (atom (third p))) ; ground objs
		       (cond ((eq (car p) '=) (if (equal (second p) (third p)) 'true 'false))
			     ((eq (car p) '!=) (if (equal (second p) (third p)) 'false 'true))
			     ((eq (car p) 'lt) (if (< (second p) (third p)) 'true 'false))
			     ((eq (car p) 'lte) (if (<= (second p) (third p)) 'true 'false))
			     ((eq (car p) 'gt) (if (> (second p) (third p)) 'true 'false)) 
			     ((eq (car p) 'gte) (if (>= (second p) (third p)) 'true 'false))
			     ((eq (car p) 'contains) (if (search (third p) (second p)) 'true 'false))
			     ((eq (car p) 'notcontains) (if (not (search (third p) (second p))) 'true 'false))
			     ((eq (car p) 'in) (if (ss-checkreg (second p) (third p)) 'true 'false))
			     ((eq (car p) 'notin) (if (ss-checkreg (second p) (third p)) 'false 'true))
			     ((eq (car p) 'nin) (if (ss-checkreg (second p) (third p)) 'false 'true))
			     (t p)))
		      (t p)))
	       (t p)))
	(t (ss-cast p))))

(defun ss-cast (p)
  "(SS-CAST P) takes an atom and casts its arguments to the correct type
   and returns the result or ERROR.  Assumes P has already been simplified via ss-simplify."
  (cond ((find (car p) '(lt lte gt gte))
	 (list (car p) (ss-cast-tonum (second p)) (ss-cast-tonum (third p))))
	((find (car p) '(= !=))
	 (cond ((and (listp (second p)) (eq (car (second p)) 'reg)) (error 'ss-type-error))
	       ((and (listp (third p)) (eq (car (third p)) 'reg)) (error 'ss-type-error))
	       ((stringp (second p)) (list (car p) (second p) (ss-cast-tostring (third p))))
	       ((stringp (third p))  (list (car p) (ss-cast-tostring (second p)) (third p)))
	       ((numberp (second p)) (list (car p) (second p) (ss-cast-tonum (third p))))
	       ((numberp (third p)) (list (car p) (ss-cast-tonum (second p)) (third p)))
	       ((boolp (second p)) (list (car p) (second p) (ss-cast-tobool (third p))))
	       ((boolp (third p)) (list (car p) (ss-cast-tobool (second p)) (third p)))
	       ((lenp (second p)) (list (car p) (second p) (ss-cast-tonum (third p))))
	       ((lenp (third p)) (list (car p) (ss-cast-tonum (second p)) (third p)))
	       (t p)))
	((find (car p) '(in notin nin))
	 (list (car p) (ss-cast-tostring (second p)) (ss-cast-toreg (third p))))
	((find (car p) '(contains notcontains))
	 (list (car p) (ss-cast-tostring (second p)) (ss-cast-tostring (third p))))
	(t p)))
#|
(defun ss-toerr (x) 
  (cond ((atom x) (if (eq x 'error) 'error x))
	((listp x) (if (member 'error x) 'error x))
	(t x)))
|#

(defun ss-cast-tonum (x) 
  (cond ((boolp x) (if (eq x 'true) 1 0))
	((numberp x) x)
	((stringp x) 
	 (cond ((equal x "") (error 'ss-type-error)) 
	       (t (setq x (ignore-errors (read-from-string x nil nil))) 
		  (if (numberp x) x (error 'ss-type-error)))))
	((varp x) x)
	((lenp x) x)
	(t (error 'ss-type-error))))

(defun ss-cast-tostring (x)
  (cond ((boolp x) (tostring x))
	((numberp x) (tostring x))
	((stringp x) x)
	((varp x) x)
	((lenp x) x)
	(t (error 'ss-type-error))))

(defun ss-cast-tobool (x)
  (cond ((boolp x) x)
	((numberp x) (if (= x 0) 'false 'true))
	((stringp x) (cond ((or (equal x "1") (equal x "true")) 'true)
			   ((or (equal x "0") (equal x "false")) 'false)
			   (t (error 'ss-type-error))))
	((varp x) x)
	((lenp x) x)
	(t (error 'ss-type-error))))

(defun ss-cast-toreg (x)
  (if (or (stringp x) (numberp x) (boolp x))
      (tostring x)
      (error 'ss-type-error)))
    
(defun ss-simplify-not (p)
  "(SS-SIMPLIFY-NOT P) Given a literal p, translates to equivalent literal
   without any negation."
  (cond ((atom p) p)
	((eq (car p) 'not)
	 (setq p (second p))
	 (cond ((eq (car p) '=) (cons '!= (cdr p)))
	       ((eq (car p) '!=) (cons '= (cdr p)))
	       ((eq (car p) 'lt) (cons 'gte (cdr p)))
	       ((eq (car p) 'lte) (cons 'gt (cdr p)))
	       ((eq (car p) 'gt) (cons 'lte (cdr p)))
	       ((eq (car p) 'gte) (cons 'lt (cdr p)))
	       ((eq (car p) 'in) (cons 'nin (cdr p)))
	       ((eq (car p) 'notin) (cons 'in (cdr p)))
	       ((eq (car p) 'nin) (cons 'in (cdr p)))
	       ((eq (car p) 'contains) (cons 'notcontains (cdr p)))
	       ((eq (car p) 'notcontains) (cons 'contains (cdr p)))
	       ((eq (car p) 'require) (cons 'forbid (cdr p)))
	       ((eq (car p) 'forbid) (cons 'require (cdr p)))))       
	(t p)))

(defun ss-simplify-aggressive (plist)
  "(SS-SIMPLIFY-AGGRESSIVE PLIST) takes a list of atoms PLIST and returns an
   equivalent but simplified list of atoms or :unsat.  "
  (let (bl bl2 len range unsat var low high ne)
    ; drop = and simplify !=, but keep bl to add back on at end
    (multiple-value-setq (plist bl) (delete= plist))
    (multiple-value-setq (plist bl2) (delete!=bool plist))
    (when (eq plist :unsat) (return-from ss-simplify-aggressive :unsat))
    (setq bl (compose-mgus (list bl bl2)))
    ; look for unsat via required/forbidden
    (when (tautp plist '((require . forbid))) (return-from ss-simplify-aggressive :unsat))
    ; extract len bounds
    (setq plist (mapcar #'ss-orient plist))
    (multiple-value-setq (plist len unsat) (ss-len-bounds plist))
    (when unsat (return-from ss-simplify-aggressive :unsat))
    ; extract range bounds
    (multiple-value-setq (plist range unsat) (ss-range-bounds plist))
    (when unsat (return-from ss-simplify-aggressive :unsat))
    ; evaluate what we can
    (setq plist (mapcar #'ss-simplify plist))
    (when (member 'false plist) (return-from ss-simplify-aggressive :unsat))
    ; run poor man's subsumption: should probably do some simple in/notin/nin comparisons
    (setq plist (remove-duplicates plist :test #'equal))

    ; NEED TO FIX LEN BOUNDS AND ADD RANGE BOUNDS
    ; add extracted info back, but canonically
    (setq plist (nconc (mapcarnot #'(lambda (x) (if (varp (car x)) `(= ,(car x) ,(cdr x)) nil)) bl) plist))
    ; var lengths
    (dolist (bl (hash2bl len))
      (setq var (car bl))
      (setq low (first (car (cdr bl))))
      (setq high (second (car (cdr bl))))
      (setq ne (cdr (cdr bl)))
      (when low (push `(lte ,low (len ,var)) plist))
      (when high (push `(gte (len ,var) ,high) plist))
      (when ne (setq plist (nconc (mapcar #'(lambda (x) `(!= (len ,var) ,x)) ne) plist))))
    ; var values
    (dolist (bl (hash2bl range))
      (setq var (car bl))
      (setq low (first (car (cdr bl))))
      (setq high (second (car (cdr bl))))
      (setq ne (cdr (cdr bl)))
      (when low (push `(lte ,low ,var) plist))
      (when high (push `(gte ,var ,high) plist))
      (when ne (setq plist (nconc (mapcar #'(lambda (x) `(!= ,var ,x)) ne) plist))))
    plist))


;;;;;;;;;;;; Type inference/checking ;;;;;;;;;;;;
; kinds of terms: str num bool reg  (variables have computed types)

#|
(defun ss-type-inference (ps bl)
  "(SS-TYPE-INFERENCE PS) given a list of atoms, compute types of variables.
   Returns two values: a hash keyed on variables giving a type and a list of errors. "
  (let (vs vs2 al errs (posstypes (make-hash-table)) th val empty)
    ; grab all types for all variables appearing in PS, using datalog
    (setq vs (vars ps))
    (setq al (mapcar #'cons vs (maptimes #'(lambda () (gentemp "a")) (length vs))))
    (push '(t . t) al)
    (setq al (nreverse al))
    (setq ps (mapcar #'(lambda (x) (cons 'c x)) (plug ps al)))
    (setq th (define-theory (make-instance 'theory) "" ps))
    (includes th 'sstypes)
    (setq al (cdr (nreverse al)))
    (setq val (mapcar #'(lambda (x) (cons (car x) (viewfinds '?t `(argtype ,(cdr x) ?t) th)))
			    al))
    (unincludes th 'sstypes)
    (empty th)
    (mapc #'(lambda (x) (setf (gethash (car x) posstypes) (cdr x))) val)

    ; add in types from bindinglist
    (setq empty (make-hash-table))  ; using errs for empty hash
    (setq vs2 (union (vars (mapcar #'car bl)) (vars (mapcar #'cdr bl))))
    (dolist (v vs2)
      (setq val (ss-augment-result-val v empty bl))
      (cond ((stringp val) (setq val (list 'str)))
	    ((numberp val) (setq val (list 'num)))
	    ((boolp val) (setq val (list 'bool)))
	    ((varp val) (setq val (gethash val posstypes)))
	    ((lenp val) (setq val (list 'num)))
	    (t (setq val nil)))
      (when val (setf (gethash v posstypes) (union val (gethash v posstypes)))))

    ; add missing types and check if too many types
    (setq errs nil)
    (dolist (v (union vs vs2))
      (setq val (gethash v posstypes))
      (unless val (setq val (list 'str)))   ; str is default type
      (when (member 'reg val) (push (format nil "~A cannot have type regular expression" v) errs))
      (when (cdr val) (push (format nil "~A has multiple types: ~A" v val) errs))
      (setf (gethash v posstypes) (first val)))

    ; turn type names into regexps
    (maphash #'(lambda (key val) (cond ((eq val 'bool) 
					(setf (gethash key posstypes) `(in ,key (reg ,*boolreg*))))
				       ((eq val 'str)
					(setf (gethash key posstypes) `(in ,key (reg ,*strreg*))))
				       ((eq val 'num)
					(setf (gethash key posstypes) `(in ,key (reg ,*numreg*))))))
	     posstypes)
    (values posstypes errs)))
|#

(defun ss-type-inference (ps)
  "(SS-TYPE-INFERENCE PS) given a list of atoms, compute types of variables.
   Returns two values: a hash keyed on variables giving a type and a list of errors. "
  (let (vs al th val val2)
    ; grab all types for all variables appearing in PS, using datalog
    (setq vs (vars ps))
    (setq al (mapcar #'(lambda (x) (cons x (quotify x))) vs))
    (push '(t . t) al)
    (setq al (nreverse al))
    (setq ps (plug ps al))
    (setq th (define-theory (make-instance 'prologtheory) "" (mapcar #'(lambda (x) (list 'atom x)) ps)))
    (includes th 'sstypes)
    (setq val nil)
    (dolist (v vs)
      (let ((*depth* (* 3 (length vs))) 
	    (*ancestry* t))  ; never need to go more than depth 3, except for = over vars
	(setq val2 nil)
	(when (viewfindp `(argtype ,(quotify v) str) th) (push 'str val2))
	(when (viewfindp `(argtype ,(quotify v) num) th) (push 'num val2))
	(when (viewfindp `(argtype ,(quotify v) bool) th) (push 'bool val2)))
      (when val2 (push (cons v val2) val)))
    (unincludes th 'sstypes)
    (empty th)
    val))

(defun ss-type-check (ps vs)
  "(SS-TYPE-CHECK PS) checks that atom set PS meets our type constraints.
   Returns the subset of PS for which type checking fails."
  (mapcarnot #'(lambda (x) (ss-type-check-aux x vs)) ps))

(defun ss-type-check-aux (p vs)
  (cond ((atom p) nil)
	(t
	 (let (mytypes)
	   (setq mytypes (mapcar #'(lambda (x) (cond ((stringp x) 'str)
						     ((numberp x) 'num)
						     ((vallen x) 'num)
						     ((valreg x) 'reg)
						     ((varp x) (second (assoc x vs)))
						     (t 'invalid)))
				 (cdr p)))
	   (if (viewfindp (list* 'type (car p) mytypes) 'sstypes) nil p)))))


(defun setify (l &key (test #'eq))
  (let ((s (list (car l))))
    (dolist (v (cdr l) s)
      (setq s (adjoin v s :test test)))))

(defun vallen (x)
  (if (lenp x) (or (stringp (second x))
		   (varp (second x)))))

(defun valreg (x)
  (cond ((atom x) nil)
	((eq (car x) 'reg) (stringp (second x)))))
			       
(deftheory sstypes ""
  (atype str)
  (atype num)
  (atype bool)

  (numericop lt)
  (numericop lte)
  (numericop gt)
  (numericop gte)
  (equality =)
  (equality !=)
  (regexp in)
  (regexp notin)
  (regexp nin)
  (contop contains)
  (contop notcontains)
  (bool true)
  (bool false)


  ;;;; basic types
  (<= (argtype num num))
  (<= (argtype bool bool))
  (<= (argtype str str))
  (<= (argtype ?x bool) (bool ?x))
  (<= (argtype ?x bool) (atom ?x) (evaluate (varp ?x) t))
  (<= (argtype ?x num) (evaluate (numberp ?x) t))
  (<= (argtype ?x str) (evaluate (stringp ?x) t))


  ;;;;; functional term types
  ; len
  (<= (argtype (len ?x) num))
  (<= (argtype ?x str) (term (len ?x)))
  ; concat
  (<= (argtype (concat ?x ?y) str))
  (<= (argtype ?x str) (term (concat ?x ?y)))
  (<= (argtype ?x str) (term (concat ?y ?x)))
  ; ltrim
  (<= (argtype (ltrim ?x) str))
  (<= (argtype ?x str) (term (ltrim ?x)))
  ; rtrim
  (<= (argtype (rtrim ?x) str))
  (<= (argtype ?x str) (term (rtrim ?x)))
  ; trim
  (<= (argtype (trim ?x) str))
  (<= (argtype ?x str) (term (trim ?x)))
  ; chop
  (<= (argtype (chop ?x) str))
  (<= (argtype ?x str) (term (chop ?x)))
  ; strlen
  (<= (argtype (strlen ?x) str))
  (<= (argtype ?x str) (term (strlen ?x)))
  ; strpos (string, string, num) num is optional
  (<= (argtype (strpos @x) num))
  (<= (argtype ?x str) (term (strpos ?x ?y)))
  (<= (argtype ?y str) (term (strpos ?x ?y)))
  (<= (argtype ?x str) (term (strpos ?x ?y ?z)))
  (<= (argtype ?y str) (term (strpos ?x ?y ?z)))
  (<= (argtype ?z num) (term (strpos ?x ?y ?z)))
  ; strstr(string, string)
  (<= (argtype (strstr @x) str))
  (<= (argtype ?x str) (term (strstr ?x ?y)))
  (<= (argtype ?y str) (term (strstr ?x ?y)))
  ; substr(string, start, length) length optional
  (<= (argtype (substr @x) str))
  (<= (argtype ?x str) (term (substr ?x ?y)))
  (<= (argtype ?y str) (term (substr ?x ?y)))
  (<= (argtype ?x str) (term (substr ?x ?y ?z)))
  (<= (argtype ?y str) (term (substr ?x ?y ?z)))
  (<= (argtype ?z num) (term (substr ?x ?y ?z)))
  ; substr_replace(string, replacement, start, <length>)
  (<= (argtype ?x str) (term (substr_replace ?x ?y ?z)))
  (<= (argtype ?y str) (term (substr_replace ?x ?y ?z)))
  (<= (argtype ?z num) (term (substr_replace ?x ?y ?z)))
  (<= (argtype ?x str) (term (substr_replace ?x ?y ?z ?w)))
  (<= (argtype ?y str) (term (substr_replace ?x ?y ?z ?w)))
  (<= (argtype ?z num) (term (substr_replace ?x ?y ?z ?w)))
  (<= (argtype ?w num) (term (substr_replace ?x ?y ?z ?w)))
  
  ; basic operations
  (<= (argtype ?x str) (atom (type ?x ?y)))
  (<= (argtype ?x str) (atom (?p ?x ?y)) (regexp ?p))
  (<= (argtype ?y str) (atom (?p ?x ?y)) (regexp ?p))
  (<= (argtype ?x str) (atom (?p ?x ?y)) (contop ?p))
  (<= (argtype ?y str) (atom (?p ?x ?y)) (contop ?p))
  (<= (argtype ?x num) (atom (?p ?x ?y)) (numericop ?p))
  (<= (argtype ?y num) (atom (?p ?x ?y)) (numericop ?p))
  (<= (argtype ?x ?type) (atom (?p ?x ?y)) (equality ?p) (argtype ?y ?type))
  (<= (argtype ?x ?type) (atom (?p ?y ?x)) (equality ?p) (argtype ?y ?type))
  (<= (argtype ?x ?type) (atom (type ?x ?y)) (argtype ?y ?type))

  (<= (term ?x) (atom (?p ?x)))
  (<= (term ?x) (atom (?p ?x ?y)))
  (<= (term ?y) (atom (?p ?x ?y)))
  (<= (term ?x) (atom (?p ?x ?y ?z)))
  (<= (term ?y) (atom (?p ?x ?y ?z)))
  (<= (term ?z) (atom (?p ?x ?y ?z)))


)

;;;;;;;;;;;; Len And range bounds ;;;;;;;;;;;;

(defun ss-len-bounds (ps)
  "(SS-LEN-BOUNDS PS) given a set of atoms of the form above, computes a set of 
   bounds for some of the variables occurring in PS.  Returns (1) unused sentences
   (2) a hash table keyed on variables with length bounds (3) whether or not the 
   atoms are unsat and (4) an explanation for one of the vars that is unsat.
   Each length bound takes the form ((low high) inval1 inval2 ... invaln)
    where each invalj means the length cannot be the one specified.
   Assumes PS have all been oriented."
  (let ((h (make-hash-table)) v n new unsat (unused nil) inval)
    (dolist (p ps)
      (cond ((and (numberp (second p)) (lenp (third p)) (varp (second (third p))))
	     (setq n (second p))
	     (setq v (second (third p)))  ; arg to len must be a variable by now
	     (unless (gethash v h) (setf (gethash v h) (cons nil nil)))
	     (cond ((eq (car p) '=) (setq new (list n n) inval nil))
		   ((eq (car p) '!=) (setq new (list 0 '*) inval n))
		   ((eq (car p) 'gt) (setq new (list 0 (1- n)) inval nil))
		   ((eq (car p) 'gte) (setq new (list 0 n) inval nil))
		   ((eq (car p) 'lt) (setq new (list (1+ n) '*) inval nil))
		   ((eq (car p) 'lte) (setq new (list n '*) inval nil))
		   (t (push p unused) (setq new nil inval nil)))
	     (when new
	       (multiple-value-setq (new unsat) (ss-bounds-merge (car (gethash v h)) new))
	       (when unsat 
		 (return-from ss-len-bounds 
		   (values unused h t (list 'varlen v 'old (car (gethash v h)) 'new new))))
	       (setf (car (gethash v h)) new))
	     (when inval
	       (setf (cdr (gethash v h)) (adjoin inval (cdr (gethash v h))))))
	    (t (push p unused))))
    (maphash #'(lambda (key val) 
		 (multiple-value-bind (b f) 
		     (ss-bounds-adj (ss-bound-to-nums (car val) *ss-max-length*) (cdr val))
		   (when (not b) 
		     (return-from ss-len-bounds 
		       (values unused h t (list 'bound (car val) 'forbidden (cdr val)))))
		   (setf (gethash key h) (cons b f))))
	     h)
    (values (nreverse unused) h)))

(defun ss-range-bounds (ps)
  "(SS-RANGE-BOUNDS PS) given a set of atoms of the form above, compute a set of 
   bounds for the numeric variables occurring in PS. Returns (1) unused subset of PS 
   (2) a hash table keyed on variables with range bounds (3) whether or not the 
   atoms are unsat and (4) an explanation for one of the vars that is unsat.
   Each range bound takes the form ((low high) inval1 inval2 ... invaln)
    where each invalj means the assigned value cannot be the one specified.
   Assumes PS have all been oriented."
  (let ((h (make-hash-table)) v n new unsat (unused nil) inval)
    (dolist (p ps)
      (cond ((and (varp (second p)) (numberp (third p))) 
	     (setq n (third p))
	     (setq v (second p))   ; note: order of var/num reversed from len-bounds
	     (unless (gethash v h) (setf (gethash v h) (cons nil nil)))
	     (cond ((eq (car p) '=) (setq new (list n n) inval nil))
		   ((eq (car p) '!=) (setq new (list 0 '*) inval n))
		   ((eq (car p) 'gt) (setq new (list (1+ n) '*) inval nil))
		   ((eq (car p) 'gte) (setq new (list n '*) inval nil))
		   ((eq (car p) 'lt) (setq new (list 0 (1- n)) inval nil))
		   ((eq (car p) 'lte) (setq new (list 0 n) inval nil))
		   (t (push p unused) (setq new nil inval nil)))
	     (when new
	       (multiple-value-setq (new unsat) (ss-bounds-merge (car (gethash v h)) new))
	       (when unsat 
		 (return-from ss-range-bounds 
		   (values unused h t (list 'varlen v 'old (car (gethash v h)) 'new new))))
	       (setf (car (gethash v h)) new))
	     (when inval
	       (setf (cdr (gethash v h)) (adjoin inval (cdr (gethash v h))))))
	    (t (push p unused))))
    (maphash #'(lambda (key val) 
		 (multiple-value-bind (b f) 
		     (ss-bounds-adj (ss-bound-to-nums (car val) *ss-max-val*) (cdr val))
		   (when (not b) 
		     (return-from ss-range-bounds 
		       (values unused h t (list 'bound (car val) 'forbidden (cdr val)))))
		   (setf (gethash key h) (cons b f))))
	     h)
    (values (nreverse unused) h)))

(defun ss-bounds-merge (old new)
  "(SS-BOUNDS-MERGE OLD NEW) takes two expressions representing length bounds
   where an unknown is represented as an *.  Returns the merge of
   those length bounds (utilizing mgu under the hood) combined with a bit
   representing whether or not those two bounds are empty."
  (cond ((not old) new)
	(t (let (l u)
	     (setq l (if (or (eq (first old) '*) (eq (first new) '*)) '* (max (first old) (first new))))
	     (setq u (cond ((eq (second old) '*) (second new))
			   ((eq (second new) '*) (second old))
			   (t (min (second old) (second new)))))
	     (cond ((eq l '*) (values new t))
		   ((eq u '*) (list l u))
		   ((< u l) (values new t))
		   (t (list l u)))))))

(defun ss-bound-to-nums (b max)
  (let ((newb))
    (if (eq (second b) '*) (push max newb) (push (second b) newb))
    (if (eq (first b) '*) (push max newb) (push (first b) newb))
    newb))

(defun ss-bounds-adj (bound forbidden)
  "(SS-BOUNDs-ADJ BOUND FORBIDDEN) takes a bound and a list of forbidden items.  Simplifies
   bound and forbidden, returning both."
  (cond ((> (first bound) (second bound)) nil)
	((member (first bound) forbidden) 
	 (ss-bounds-adj (list (1+ (first bound)) (second bound)) (remove (first bound) forbidden)))
	((member (second bound) forbidden) 
	 (ss-bounds-adj (list (first bound) (1- (second bound))) (remove (second bound) forbidden)))
	(t (values bound forbidden))))

(defun ss-solve-lenrange (vars len range)
  "(SS-SOLVE-LENRANGE VARS LEN RANGE) Add to LEN and RANGE for variables without
   length and ranges as appropriate.  For now, we're just ensuring 
   every variable has a LEN.  Returns new LEN and RANGE as well as T if 
   one of the ranges was empty."
  (let (l r bounds empty)
    (dolist (v vars)
      (setq l (gethash v len))
      (setq r (gethash v range))
      (cond ((and (not l) (not r)) 
	     (setf (gethash v len) (list (list 0 *ss-max-length*))))
	    ((and (not l) r) 
	     (setf (gethash v len) (list (ss-num2len-range (first r)))))
	    ((and l (not r)))
	    (t 
	     (multiple-value-setq (bounds empty) 
	       (ss-bounds-merge (first l) (ss-num2len-range (first r))))
	     (setf (gethash v len) (cons bounds (second l)))
	     (when empty 
	       (return-from ss-solve-lenrange 
		 (values len range t 
			 (format nil "Empty len and range combination for var ~A: len ~A, range ~A" v l r)))))))
    (values len range)))

(defun ss-num2len-range (r)
  (list (length (tostring (first r))) (length (tostring (second r)))))

;;;;;;;;;;;; Core Constraint Solving ;;;;;;;;;;;;
	   
(defun ss-varordering (vs ps len)
  "(SS-VARORDERING VS PS LEN) chooses variable ordering and therefore search space.
   Returns VS re-ordered."
  (ss-varordering-smallfirst vs ps len))

(defun ss-varordering-smallfirst (vs ps len)
  "(SS-VARORDERING-SMALLFIRST VS PS LEN) orders VS so that if variables are unsat, we avoid searching
   a large space that is irrelevant.  Thus, we order variables in order of increasing size."
  (declare (ignore ps))
  (mapcar #'first (sort (mapcar #'(lambda (x) (list x (* -1 (apply #'- (car (gethash x len)))))) vs)
			#'< :key #'second)))

(defun ss-solve-iterate2 (ps vars len range)
  "(SS-SOLVE-ITERATE PS) uses Hampi to find a solution to the atoms given as input.  Return
   assignment that includes (at least) assignments for all of VARS.
   Assumes all atoms are one of 
        (= (len var) (len var))            us  Easy to check
        (!= var var)                       us  Easy to check
        (!= var (len var))                 us  Easy to check
        (!= (len var) (len var))           us  Easy to check
        (numop var var)                    us  Fold into search
        (numop var (len var))              us  Fold into search
        (numop (len var) (len var))        us  Easy to check
        (regop var (reg str))              hampi"
  (let (h top bot ham vs comps)
    ; compute connected components, throw out components not including a v in VARS,
    ;   and solve each component separately (b/c they do not interact).
    (setq comps (agraph-connected-components (undirected-dependency-graph-vars ps nil) :test #'equalp))
    (setq comps (delete-if-not #'(lambda (x) (some #'(lambda (y) (member y vars)) x)) comps))
    (setq h (make-hash-table))
    (multiple-value-setq (top bot ham) (ss-index-constraints2 ps))
    (dolist (c comps)
      (setq vs (ss-varordering (intersection (vars c) vars) nil len))
      (setq h (ss-solve-iterate-aux2 vs h len range top bot ham))
      (when (not h) (return)))
    ; hash values returned are (len val), so here we simplify to just val.
    (when h (maphash #'(lambda (key val) (setf (gethash key h) (second val))) h))
    h))

(defun ss-index-constraints2 (ps)
  "(SS-INDEX-CONSTRAINTS2 PS) returns 3 hash tables, each keyed on variables.
   We include all constraints in which each variable appears."
  (let (top bot ham vs h)
    (setq top (make-hash-table))
    (setq bot (make-hash-table))
    (setq ham (make-hash-table))
    (dolist (p ps)
      (setq vs (vars p))
      (cond ((atom p))
	    ((and (eq (car p) '!=) (varp (second p)) (varp (third p))) (setq h bot))
	    ((find (car p) '(in notin contains notcontains)) (setq h ham))
	    (t (setq h top)))
      (dolist (v vs)
	(setf (gethash v h) (cons p (gethash v h)))))
    (values top bot ham)))

(defun ss-solve-iterate-aux2 (vs state len range top bottom hampi)
  ;(declare (notinline ss-solve-iterate-aux2))
  (cond ((null vs) state)
	(t
	 (let (sofar interval forbids before after h al val strp)
	   (setq strp (if (gethash (car vs) range) nil t))
	   (setq interval (gethash (car vs) (if strp len range)))
	   (setq forbids (cdr interval))
	   (setq interval (car interval))
	   (setq before (gethash (car vs) top))
	   (setq after (gethash (car vs) bottom))
	   (setq h (gethash (car vs) hampi))
	   (setq sofar nil)
           (unless *quiet* 
	     (format t "~&Solving var ~A " (car vs)))
	   (when *ss-debug*
	     (format t "~&before: ~A~%after: ~A~%hampi: ~A~%" before after h))
           (unless *quiet* 
	     (if strp
		 (format t "~&Searching for string var ~A at size" (car vs))
		 (format t "~&Searching for numeric var ~A at value" (car vs))))
	   (do ((i (first interval) (1+ i)))
	       ((> i (second interval)) nil)
	     (unless *quiet* (format t " ~A" i))
	     ;(setf (gethash (car vs) *hampi*) (gethash 'univ *hampi*))
	     (unless (member i forbids)
	       (cond (strp
		      (when (ss-sat-len2 (car vs) i state before len range)
			(setq sofar nil)
			(setq val t)
			(do () ((eq val nil))
			  (setq val (ss-invoke-hampi (car vs) i (ss-construct-hinput (car vs) sofar h)))
			  (unless *quiet* (format t " (f:~A,val: ~S)" (1- *counter*) val)) 
			  (unless val (return))
			  (push val sofar)
			  (when (ss-sat-val2 (car vs) val state after len range)
			    (setf (gethash (car vs) state) (list i val))
			    (setq al (ss-solve-iterate-aux2 (cdr vs) 
							    state
							    len range top bottom hampi))
			    (when al (return-from ss-solve-iterate-aux2 al))))))
		     (t (when (and (ss-sat-val2 (car vs) i state before len range)
				   (ss-sat-val2 (car vs) i state after len range))
			  (setq val (tostring i))
			  (when (every #'(lambda (x) (let ((r (ss-checkreg val (fourth (third x)))))
						       (if (eq (car x) 'in) r (not r)))) 
				       h)
			    (setf (gethash (car vs) state) (list (length val) val))
			    (setq al (ss-solve-iterate-aux2 (cdr vs)
							    state
							    len range top bottom hampi))
			    (when al (return-from ss-solve-iterate-aux2 al))))))))
	   (setf (gethash (car vs) state) nil)))))

(defun ss-checkreg (str reg)
  "(SS-CHECKREG STR REG) checks that string STR matches the string regular expression REG."
  (let (m)
    (with-open-file (f *hampi-checkreg-tmp* :direction :output :if-does-not-exist :create :if-exists :supersede)
      (princ str f)
      (format f "~%")
      (princ (ss-delimit-regexp reg) f)
      (format f "~%"))
    (setq m (with-output-to-string (strstream) 
	      (run-program *reg-check* (list *hampi-checkreg-tmp*) :output strstream)))
    (if (> (length m) 7)
	(equalp (subseq m 0 7) "success")
	nil)))

(defun ss-sat-val2 (var val state ps len range)
  "(SS-SAT-VAL2 VAR VAL STATE PS) checks if VAR/VAL together with the other assignments in STATE
   satisfies the constraints PS."
  (ss-sat-constraints var val state ps len range #'ss-sat-val2-val))

(defun ss-sat-len2 (var l state ps len range)
  "(SS-SAT-LEN2 VAR LEN STATE PS) checks if LEN for VAR satisfies all of PS
  in the context of STATE."
  (ss-sat-constraints var l state ps len range #'ss-sat-len2-val))

(defun ss-sat-val2-val (arg var val state)
  (cond ((eq arg var) val)
	((varp arg) (setq val (second (gethash arg state))) (if val val arg))
	((lenp arg) (setq val (ss-sat-val2-val (second arg) var val state))
	 (if (not (varp val)) (length (ss-cast-tostring val)) `(len ,val)))
	(t arg)))

(defun ss-sat-len2-val (arg var len state)
  "(SS-SAT-LEN2-VAL ARG VAR LEN STATE) given that we're attempting to find a value for VAR of length
   LEN, return the value of ARG in the current STATE.  VAR and LEN are useful only when ARG
   is (len VAR).  Returns NIL if value for ARG is unknown."
  (cond ((varp arg) (setq len (second (gethash arg state))) (if len len arg))
	((lenp arg) (cond ((eq (second arg) var)  len)
			  (t
			   (setq len (ss-sat-len2-val (second arg) var len state)) 
			   (if (not (varp len)) (length (ss-cast-tostring len)) `(len ,len)))))
	(t arg)))

(defun ss-sat-constraints (var thing state ps len range valfunc)
  "(SS-SAT-CONSTRAINTS VAR THING STATE PS VALFUNC) checks if PS is satisfied by STATE
   together with VAR given property THING.  VALFUNC encodes the semantics of THING.  It
   is a function that given an argument, VAR, THING, and STATE, simplifies ARG to the extent
   possible and returns the result."
  (let (op a1 a2 val)
    (dolist (p ps t)
      (setq op (first p))
      (setq a1 (funcall valfunc (second p) var thing state))
      (setq a2 (funcall valfunc (third p) var thing state))
      (setq p (ss-cast (ss-orient (list op a1 a2))))
      (setq op (first p) a1 (second p) a2 (third p))
      (cond ((and (groundp a1) (groundp a2))
	     (case op
	       (!= (when (equal a1 a2) (return nil)))
	       (=  (when (not (equal a1 a2)) (return nil)))
	       (lt (when (not (< a1 a2)) (return nil)))
	       (lte (when (not (<= a1 a2)) (return nil)))
	       (gt (when (not (> a1 a2)) (return nil)))
	       (gte (when (not (>= a1 a2)) (return nil)))
	       (otherwise (ss-break (format nil "Unknown operator in ss-sat-constraints: ~S" p)))))

	    ; second is ground: first must be a var by ss-orient
	    ((and (varp a1) (groundp a2))
	     (setq val (first (gethash a1 range)))
	     (setq a2 (ss-cast-tonum a2))
	     (when (and val (or (< a2 (first val)) (> a2 (second val)))) (return nil)))

	    ; first is ground: second must be len by ss-orient
	    ((and (groundp a1) (lenp a2))
	     (setq val (first (gethash (second a2) len)))
	     (setq a1 (ss-cast-tonum a1))
	     (when (and val (or (< a1 (first val)) (> a1 (second val)))) (return nil)))))))
	     

;;;;;;;;;;;; Kaluza interface ;;;;;;;;;;;;

(defun ss-invoke-kaluza (vars constraints) 
  "(SS-INVOKE-KALUZA CONSTRAINTS) runs Kaluza to find a binding list of variables VARS
   that satisfy CONSTRAINTS, which is a list of literals treated as a conjunction."
  (cond ((or (eq constraints 'true) (null constraints)) (mapcar #'(lambda (x) (cons x "42")) vars))
	((eq constraints 'false) :unsat)
	((atom constraints) 
	 (ss-assert nil nil (format nil "ss-invoke-kaluza requires a list of constraints; found ~A" constraints)))
	(t
	 (let ((f (tostring (list *solver-input-file* *counter*))) starttime out newvars allvars)
	   (setq allvars (vars constraints))
	   (setq starttime (get-universal-time))
	   (setq newvars (mapcar #'(lambda (x) (read-user-string (format nil "TLH_~A" (devariable x)))) allvars))
	   (setq constraints (sublis (mapcar #'cons allvars newvars) constraints))
	   (if (< *counter* *solver-max-storage*) (setq *counter* (1+ *counter*)) (setq *counter* 0))
	   (with-open-file (out f :direction :output :if-does-not-exist :create :if-exists :supersede)
	     (ss-invoke-kaluza-writefile constraints out))
	   (setq out (with-output-to-string (s) 
		       (run-program *solver-invoke* (append *solver-invoke-args* (list f "TLH_")) :output s)))
	   (setq out (ss-invoke-kaluza-parse out))
	   ; kaluza doesn't return empty strings, so we assume any var not included is ""
	   (when (listp out)
	     (dolist (v newvars)
	       (unless (assoc v out) (push (cons v "") out))))
	   (format t "~&Kaluza required ~A seconds~%" (- (get-universal-time) starttime))
	   (cond ((eq out :error)
		  (setq *external-solver-errored* t)
		  (if *break-on-external-solver-error*
		      (ss-break (format nil "Kaluza errored in file ~A~%" f))
		      nil))
		 ((eq out :unsat) :unsat)
		 (t (delete-if-not #'(lambda (x) (member (car x) vars))
				   (nsublis (mapcar #'cons newvars allvars) out))))))))

(defun ss-invoke-kaluza-writefile (constraints s)
  "(SS-INVOKE-KALUZA-WRITEFILE CONSTRAINTS S) writes Kaluza-legal 
   input representing CONSTRAINTS to stream s.  Kaluza language is
   the following:
      atom: <var>, <num>, <string>, <boolean>
      numatom: <var>, <num>
      terms: <atom>
             (<numfunc> <numterm> <numterm>)   // can only appear in (== <var> (<numfunc> <numterm> <numterm>))
             (len <atom>)
             (concat <atom1> <atom2>)
      atoms: (<op> <term1> <term2>)
      <numfunc> ::= 
      <numop> ::= lt, lte, gt, gte
      <regop> ::= in, notin, nin
      <eqop> ::= !=, =
      <conop> ::= contains, notcontains
      <op> ::= <numop> | <regop> | <eqop> | <conop>"
  (let (var)
    (dolist (c constraints)
      (when (negative-literalp c) (format s "!") (setq c (second c)))  ; shouldn't need this, but just in case
      (cond ((eq (car c) '!=)  ; patch for Kaluza bug
	     (setq var (read-from-string (tostring (gentemp "temp"))))
	     (ss-invoke-kaluza-writefile-constraint `(assign ,var ,c) s)
	     (ss-invoke-kaluza-writefile-constraint `(assert ,var) s))
	    (t (ss-invoke-kaluza-writefile-constraint c s))))))

(defun ss-invoke-kaluza-writefile-constraint (p s)
  (ss-invoke-kaluza-writefile-atom p s)
  (format s ";~%"))

(defun ss-invoke-kaluza-writefile-atom (p s)
  (flet ((p2i (c)
	   (ss-invoke-kaluza-writefile-term (second c) s)
	   (format s " ")
	   (ss-invoke-kaluza-writefile-op (first c) s)
	   (format s " ")
	   (ss-invoke-kaluza-writefile-term (third c) s)))
    (cond ((atom p) (ss-invoke-kaluza-writefile-term p s))
	  ((eq (car p) 'assign)
	   (p2i (list (first p) (second p) (list 'meta (third p)))))
	  ((eq (car p) 'assert)
	   (format s "ASSERT(")
	   (ss-invoke-kaluza-writefile-term (second p) s)
	   (format s ")"))
	  ((eq (car p) 'type)
	   (p2i (list 'in (second p) (list 'reg (stringappend "^(" (third p) ")$")))))
	  ((member (car p) '(in notin nin))
	   (p2i (list (first p) (second p) (list 'reg (third p)))))
	  ((eq (car p) 'contains)
	   (p2i (list 'in (second p) (list 'reglit (third p)))))
	  ((eq (car p) 'notcontains)
	   (p2i (list 'notin (second p) (list 'reglit (third p)))))
	  (t (p2i p)))))

(defun ss-invoke-kaluza-writefile-term (term s)
  (cond ((varp term) (format s "~A" (devariable term)))
	((boolp term) (format s "~(~A~)" term))
	((atom term) (format s "~S" term))
	((eq (car term) 'meta) (ss-invoke-kaluza-writefile-atom (second term) s))
	((eq (car term) 'reg)
	 (ss-assert (stringp (second term)) nil (format nil "Reg terms must be (reg <string>); found ~A" term))
	 (format s "CapturedBrack(/~A/,0)" (ss-regesc (second term) '(#\/))))		 
	((eq (car term) 'reglit)
	 (ss-assert (stringp (second term)) nil (format nil "Reglit terms must be (reglit <string>); found ~A" term))
	 (format s "CapturedBrack(~S,0)" (second term)))		 
	((eq (car term) 'len)
	 (ss-assert (atom (second term)) nil (format nil "Len terms must be (len <atom>); found ~A" term))
	 (format s "Len(") 
	 (ss-invoke-kaluza-writefile-term (second term) s)
	 (format s ")"))
	((eq (car term) 'concat) 
	 (ss-assert (and (atom (second term)) (third term)) nil (format nil "Concat terms must be (concat <atom> <atom>); found ~A" term))
	 (ss-invoke-kaluza-writefile-term (second term) s)
	 (format s " . ")
	 (ss-invoke-kaluza-writefile-term (second term) s))
	((member (car term) '(* / + -))
	 (ss-assert (and (or (varp (second term)) (numberp (second term)))
		      (or (varp (third term)) (numberp (third term)))) 
		 nil 
		 (format nil "Numeric terms must be (<numfunc> <numterm> <numterm>); found ~A" term))
	 (ss-invoke-kaluza-writefile-term (second term) s)
	 (format s " ~A " (first term))
	 (ss-invoke-kaluza-writefile-term (third term) s))
	(t (ss-assert nil nil (format nil "Unknown term for Kaluza: ~A" term)))))

(defun ss-invoke-kaluza-writefile-op (op s)
  (case op
    (!= (setq op '!=))
    (= (setq op '==))
    (lt (setq op '<))
    (gt (setq op '>))
    (lte (setq op '<=))
    (gte (setq op '>=))
    (in (setq op "\\in"))
    (nin (setq op "\\nin"))
    (notin (setq op "\\notin"))
    (assign (setq op ":=")))
  (format s "~A" op))

(defun ss-invoke-kaluza-parse (str)
  "(SS-INVOKE-KALUZA-PARSE STR) parses Hampi's output to return a value."
  (setq str (string-trim '(#\Space) str))
  (cond ((eq (length str) 0) :unsat)
	(t
	 (let (l bl)
	   (setq l (split-string str '(#\Newline)))
	   (dolist (i l bl)   ; walk over result and find var assign or unsat
	     (cond ((= (length i) 0))
		   ((search "ASSERT" i)
		    (let (vari eqi q1i q2i)
		      (setq vari 7)  ; after ASSERT(
		      (setq eqi (search "=" i :start2 vari))
		      (setq q1i (search "''" i :start2 eqi))
		      (setq q2i (search "''" i :from-end t))
		      (push (cons (read-from-string (subseq i vari (1- q1i))) 
				  (subseq i (+ 2 q1i) q2i))
			    bl)))
		   ((search "(=" i)
		    (setq i (read-user-string i))
		    (push (cons (second i) (third i)) bl))
		   (t (return :error))))))))

(defun ss-kaluza-init () (run-program *solver-init* *solver-init-args*) (sleep 1))
(defun ss-kaluza-kill () (run-program *solver-kill* *solver-kill-args*))


;;;;;;;;;;;; Hampi interface ;;;;;;;;;;;;

(defun ss-construct-hinput (var vals constraints)
  "(SS-CONSTRUCT-HINPUT VAR FAILEDVALS CONSTRAINTS) given a variable VAR, 
   a list of strings VALS, and a set of CONSTRAINTS, construct a constraint set
   that is equivalent to CONSTRAINTS but forbids VAR from taking on any of VALS."
  (cond ((null vals) constraints)
	(t
	 (let (u (n (tostring (gentemp "reg"))))
	   (setq u (mapcar #'(lambda(x) (ss-regesc (format nil "\"~A\"" x) '(#\\))) vals))
	   (setq u (cons (car u) (mapcan #'(lambda (x) (list "," x)) (cdr u))))
	   (setq u (format nil "or(~A)" (tostring u)))
	   (cons (list 'notin var (list 'reg n (format nil "reg ~A := ~A;~%" n u))) constraints)))))

(defun ss-invoke-hampi (var len constraints) 
  "(SS-INVOKE-HAMPI VAR LEN CONSTRAINTS) runs Hampi to find a string of length LEN
   that when assigned to VAR satisfies CONSTRAINTS."
  (cond ((null constraints) (if (= len 0) "" (tostring (maptimes #'(lambda () "a") len)))) 
	(t
	 (let ((f (tostring (list *solver-input-file* *counter*))) out)
	   (if (< *counter* *solver-max-storage*) (setq *counter* (1+ *counter*)) (setq *counter* 0))
	   (with-open-file (out f :direction :output :if-does-not-exist :create :if-exists :supersede)
	     (ss-invoke-hampi-writefile var len constraints out))
	   (setq out (with-output-to-string (s) 
		       (run-program *solver-invoke* (append *solver-invoke-args* (list f)) :output s)))
	   (setq out (ss-invoke-hampi-parse out))
	   (cond ((eq out 'error)
		  (setq *external-solver-errored* t)
		  (if *break-on-external-solver-error*
		      (ss-break (format nil "Hampi errored for var ~A with length ~A found in file ~A~%"
				     var len f))
		      nil))
		 (t out))))))

(defun ss-invoke-hampi-writefile (var len constraints s)
  "(SS-INVOKE-HAMPI-WRITEFILE VAR LEN CONSTRAINTS S) writes Hampi-legal 
   input representing a search for variable VAR of length LEN constrained
   by CONSTRAINTS to stream s.  Assumes each element of constraints is
   one of the following.
			var != string           *hampi
		       	var in L                *hampi
	       		var notin L             *hampi
		       	string in L             *hampi
		       	string notin L          *hampi"
  (setq var (devariable var))
  (format s "var ~A : ~A;~%" var len)
  (dolist (c constraints)
    (cond ((eq (car c) '!=) 
	   (ss-invoke-hampi-writefile-notin 
	    (second c) (format nil "or(\"~A\")" (third c)) s))
	  ((eq (car c) 'in)
	   (ss-invoke-hampi-writefile-in 
	    (second c) (ss-invoke-hampi-writefile-reg (second (third c)) (third (third c)) s) s))
	  ((eq (car c) 'notin)
	   (ss-invoke-hampi-writefile-notin 
	    (second c) (ss-invoke-hampi-writefile-reg (second (third c)) (third (third c)) s) s))
	  ((eq (car c) 'contains)
	   (format s "assert ~A contains ~S;~%" 
		   (if (varp (second c)) (devariable (second c)) (second c))
		   (ss-cast-tostring (third c))))
	  ((eq (car c) 'notcontains)
	   (format s "assert ~A not contains ~S;~%" 
		   (if (varp (second c)) (devariable (second c)) (second c))
		   (ss-cast-tostring (third c))))
	  (t (ss-assert t nil 
		     (format nil "ss-invoke-hampi-writefile given unknown constraint ~A" c))))))

(defun ss-invoke-hampi-writefile-in (thing l s) 
  (format s "assert ~A in ~A;~%" (if (varp thing) (devariable thing) thing) l))
(defun ss-invoke-hampi-writefile-notin (thing l s) 
  (format s "assert ~A not in ~A;~%" (if (varp thing) (devariable thing) thing) l))
(defun ss-invoke-hampi-writefile-reg (thing def s)
  (princ def s)
  thing)

(defun ss-invoke-hampi-parse (str)
  "(SS-INVOKE-HAMPI-PARSE STR) parses Hampi's output to return a value."
  (let (l)
    (setq l (split-string str '(#\Newline)))
    (dolist (i l 'error)   ; walk over result and find var assign or unsat
      (when (equalp i "unsat") (return nil))
      (when (search "{VAR" i) (return (second (split-string i '(#\= #\}))))))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Unfinished XML Frontend ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; unfinished since for strings we need/want to use CDATA, but the XML parsers I found and installed
;  (as well as Mike's) don't deal with CDATA correctly
#|
<phi>...</phi>
<types>...</types>
<space>...</space>
<unique><var></var>...</unique>
<required><var></var>...</required>
<init><bl><var></var><val></val></bl>...</init>


(define-condition xml-error (error) ((comment :initarg :comment)))


(defun ss-loadxml (string)
  "(SS-LOADXML STRING) returns an ss-prob representation its xml representation in STRING."
  (let (p l)
    (setq l (parsexml (scanxml (substitute #\- #\: string))))
    (setq p (make-ss-prob))
    (when (not (eq (first (first p)) 'stringsolve))
      (error 'xml-error :comment "Toplevel item must be stringsolve"))
      (dolist (field (cdr l))
	(when (not (listp field))
	  (error 'xml-error :comment (format nil "Unexpected: ~A" field))
	  (case (first field)
	    (phi (setf (ss-prob-phi p) (unxclify (second field))))
	    (types (setf (ss-prob-types p) (mapcar #'unxclify (cdr field))))
	    (space (setf (ss-prob-space p) (unxclify (second field))))
	    (unique (setf (ss-prob-unique p) (mapcar #'second (cdr field))))
	    (required (setf (ss-prob-required p) (mapcar #'second (cdr field))))
	    (init (setf (ss-prob-init p) 
			(mapcar #'(lambda (x) (list (second (second x)) (second (third x)))) (cdr field))))
	    (otherwise (push field (ss-prob-metafields p))))))))

(defun ss-dumpxml (prob)
  "(SS-DUMPXML PROB) returns a string consisting of an XML representation of ss-prob PROB"
  (with-output-to-string (s)
    ;(format s "<?xml version=\"1.0\"?>~%~%")
    (format s "<stringsolve>~%")
    (format s "<phi>~%")
    (ss-dumpxml-xcl (ss-prob-phi prob) 1 s)
    (format s "</phi>~%")
    (format s "<space>~%")
    (ss-dumpxml-xcl (ss-prob-space prob) 1 s)
    (format s "</space>~%")
    (format s "<types>~%")
    (mapc #'(lambda (x) (ss-dumpxml-xcl x 1 s)) (drop-op (ss-prob-types prob)))
    (format s "</types>~%")
    (format s "<unique>~%")
    (cond ((listp (ss-prob-required prob))
	   (mapc #'(lambda (x) (ss-dumpxml-xcl-term x 1 s)) (ss-prob-unique prob)))
	  (t
	   (format s "~A" (ss-prob-required prob))))
    (format s "</unique>~%")
    (format s "<required>~%")
    (cond ((listp (ss-prob-required prob))
	   (mapc #'(lambda (x) (ss-dumpxml-xcl-term x 1 s)) (ss-prob-required prob)))
	  (t
	   (format s "~A" (ss-prob-required prob))))
    (format s "</required>~%")
    (format s "<init>~%")
    (dolist (v (ss-prob-init prob))
      (printspaces 1 s)
      (format s "<bl>~%")
      (printspaces 2 s)
      (ss-dumpxml-xcl-term (first v) 3 s)
      (ss-dumpxml-xcl-term (second v) 0 s)
      (format s "</bl>~%"))
    (format s "</init>~%")
    (format s "</stringsolve>~%")))

(defun ss-dumpxml-xcl (p n s)
  (cond ((atom p) 
	 (printspaces (* 2 n) s)
	 (format s "<proposition>~A</proposition>" p))
	((find (car p) '(and or not => <= <=>))
	 (setq p (kifsent2xclsent p))
	 (printspaces (* 2 n) s)
	 (format s "<~A>~%" (first p))
	 (mapc #'(lambda (x) (ss-dumpxml-xcl x (1+ n) s)) (cdr p))
	 (printspaces (* 2 n) s)
	 (format s "</~A>~%" (first p)))
	(t
	 (printspaces (* 2 n) s)
	 (format s "<relnsent>~%")
	 (printspaces (* 2 (1+ n)) s)
	 (format s "<relation>~A</relation>~%" (first p))
	 (mapc #'(lambda (x) (ss-dumpxml-xcl-term x (1+ n) s)) (cdr p))
	 (printspaces (* 2 n) s)
	 (format s "</relnsent>~%"))))

(defun ss-dumpxml-xcl-term (x n s)
  (cond ((varp x)
	 (printspaces (* 2 n) s)
	 (format s "<var>~A</var>~%" x))
	((stringp x)
	 (printspaces (* 2 n) s)
	 (format s "<string><![CDATA[~A]]></string>~%" x))
	((atom x)
	 (printspaces (* 2 n) s)
	 (format s "<object>~A</object>~%" x))
	(t
	 (printspaces (* 2 n) s)
	 (format s "<term>~%")
	 (printspaces (* 2 (1+ n)) s)
	 (format s "<function>~A</function>~%" (first x))
	 (mapc #'(lambda (x) (ss-dumpxml-xcl-term x (1+ n) s)) (cdr x))
	 (printspaces (* 2 n) s)
	 (format s "</term>~%"))))


(defun kifsent2xclsent (p) 
  (cond ((atom p) p)
	(t
	 (case (first p)
	   (and p)
	   (or p)
	   (not p)
	   (<= (list* (cons 'implies (cddr p)) (list (second p))))
	   (=> (cons 'implies (cdr p)))
	   (<=> (cons 'iff (cdr p)))))))
|#