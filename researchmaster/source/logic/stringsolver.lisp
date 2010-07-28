; all regexps are pcre (perl compatible regular expressions)
; requires Hampi as a helper, which only runs on Linux
(defun r () 
  (load "/home/notamper-local/papers/notamper/tool/solver/ss/stringsolver.lisp")
  (load "/home/notamper-local/papers/notamper/tool/solver/ss/stringsolvertests.lisp"))

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
(defvar *break-on-hampi-error* t
  "whether or not to break when hampi returns an error")
(defvar *check-dynamic-regexp* t
  "whether or not to ensure that our dynamically created regexps can be translated")
(defparameter *hampi-input-file* "/tmp/ss/b.hmp" 
  "base temp filename for hampi input")
(defparameter *hampi-max-storage* 100 
  "max number of files stored on disk for any one solution attempt")
(defparameter *notamp-raw-output* "/tmp/ss/raw.lisp"
  "location to store raw notamp results in lisp-friendly format.  Not currently used.")
(defparameter *hampi-regtest-tmp* "/tmp/hampiregtest.txt"
  "location for testing regular expressions")
(defparameter *hampi-checkreg-tmp* "/tmp/bar.hmp"
  "location for testing a given string against a regular expression")

; hampi invocation info: right now using server version 
;     (server version is faster for repeated calls than standalone version)
(defparameter *hampi* nil "deprecated, sort of")
(defparameter *hampi-reg* "hampireg" "script for converting pcre regexps to hampi regexps")
(defparameter *hampi-regtest* "hampiregtest" "script for checking regexp translator works")
(defparameter *hampi-init* "hampi_s")
(defparameter *hampi-init-args* '("4445"))
(defparameter *hampi-invoke* "hampi_c")
(defparameter *hampi-invoke-args* '("4445"))
(defparameter *hampi-kill* "hampi_c")
(defparameter *hampi-kill-args* '("4445" "SHUTDOWN"))
(defparameter *reg-check* "regcheck" "script that checks whether a given string belongs to a given regexp")

(defparameter *app-location* "./ss"
  "location for application--only used when saving application")
(defparameter *app-init-location* 
  "./ss-init.lisp"
  "location for application initialization--used for application")

; internal globals
(defparameter *reg-special-chars* (list #\\ #\^ #\$ #\( #\) #\[ #\] #\{ #\} #\. #\* #\? #\| #\+ #\-))

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
  (stringappend "[" (ss-regesc (apply #'stringappend *reg-special-chars*)) " \\w!@#%&:/=\"><;,'`~" "]"))
(defparameter *boolreg* "true | false")
(defparameter *strreg* (stringappend *charreg* "*"))
(defparameter *strreg1* (stringappend *charreg* "+"))
(defparameter *numreg* "[0-9]+")

(defvar *counter* 1)
(defvar *ss-solve-count* 0)
(defvar *ss-varmapping* nil "binding list for cleansing variables")
(defvar *ss-vars* nil "var list for cleansing variables")
(defvar *hampi-errored* nil "whether or not hampi errored")

; problem structure
; possible statuses: inconsistent, valid, contingent
(defstruct ss-prob 
  ; user-supplied 
  (name nil) phi (space 'true) (types nil)  (unique t) (required t) (init nil)
  (status :contingent) (metafields nil)
  ; internal
  (varnames nil) (variantstatus nil))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Test Routines ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; save-application call
(defun savess ()
  ;(ignore-errors (load *app-init-location*))
  (save-application *app-location* :prepend-kernel t))
; :toplevel-function #'ssx

; toplevel for application
(defun ssx (file &key (numgood 1) (unique 'unknown) (required 'unknown) (stream t))
  (ignore-errors (load *app-init-location*))
  (format stream "<testcases>~%")
  (dolist (p (read-file file))
    (ssn (eval p)
	 :numgood numgood :unique unique :required required :stream stream))
  (format stream "</testcases>~%")
  (quit))

; grab a test by name
(defun ss-findtest (name)
  "(SS-TEST NAME) finds the problem with name NAME."
  (find name (ss-deftests) :key #'ss-prob-name))

; test ssn on all tests
(defun ssnall (&key (numgood 1) (unique 'unknown) (required 'unknown) (stream t))
  (dolist (p (ss-deftests) t)
    (when (and (ss-prob-phi p) (ss-prob-types p))
      (unless *quiet* (format t "~&****** Testing ~S~%" (ss-prob-name p)))
      (ssn p :numgood numgood :unique unique :required required :stream stream))))

; input generation routine for notamper tool with all kinds of correctness checks
(defun ssn (prob &key (numgood 1) (unique 'unknown) (required 'unknown) (stream t) (outputfun #'notamper2xml))
  "(SSN PROB &key NUMGOOD UNIQUE REQUIRED STREAM) 
   Run a single notamper problem and output result to STREAM.
   PROB can either be a ss-prob or the name of an ss-prob.
   The unique and required fields of the given problem can be overridden with
   UNIQUE and REQUIRED."
  (when (not (ss-prob-p prob)) 
    (setq prob (ss-findtest prob)) (when (not prob) (return-from ssn nil)))
  (setq prob (ss-cleanse prob))

  (when (eq unique 'unknown) (setq unique (ss-prob-unique prob)))
  (when (eq required 'unknown) (setq required (ss-prob-required prob)))

  (cond ((not (ss-test-all-regexps (ss-prob-phi prob)))
	 (format t "Regexp failure in formula~%"))
	((not (ss-test-all-regexps (ss-prob-types prob)))
	 (format t "Regexp failure in type~%"))
	(t
	 (let ((start (get-universal-time)) val (*ss-solve-count* 0))
	   (format t "~&")
	   
	   (setq val
		 (notamp (ss-prob-phi prob) :numgood numgood :unique unique :required required 
			 :types (ss-prob-types prob) :space (ss-prob-space prob)))
	   (when *ss-debug* (format t "~&Solution:~%") (pprint val))

	   (funcall outputfun (ss-uncleanse val prob) prob 
			 :time (- (get-universal-time) start) :stream stream :count *ss-solve-count*)))))    

; test stringsolver on all variants of all test cases  
(defun sst (&optional (name nil) &key (variant nil) (stopearly t) (s t) (extratests t) (required 'unknown))
  "(SST &optional NAME STOPEARLY S) 
    Toplevel testing routine for string solver.  NAME limits the testing to the
    specified test case NAME.  If NAME is not supplied, runs through all test cases.
    STOPEARLY tells it to stop once a failure occurs (default is T), and 
    S is the stream to which results are written.
    If EXTRATESTS is T (the default), also runs solver tests on the negation of each
    problem, and each disjunct of the DNF of the problem and its negation."
  (ss-hampi-init)
  (let (variants phi res status i l failures *hampi-errored*)
    (setq failures 0)
    (dolist (p (ss-deftests) t)
      (when (or (not name) (eq (ss-prob-name p) name))
	(format s "~&Beginning tests of ~A~%" (ss-prob-name p))
	(setq p (ss-cleanse p))
	(when (eq required 'unknown) (setq required (ss-prob-required p)))
	(setq phi (ss-prob-phi p))
	(setq status (ss-prob-status p))
	(setq variants nil)
	(when extratests 
	  (setq variants (append (list (list (maknot phi) (sst-invert-status status))) 
				 (mapcar #'(lambda (x) (list x status)) (or2set (dnf phi)))
				 (mapcar #'(lambda (x) (list x (sst-invert-status status))) 
					 (or2set (dnf (maknot phi)))))))
	(push (list phi status) variants)
	(setq l (length variants))
	(setq i 1)
	(dolist (v variants)
	  (when (or (not variant) (= i variant))
	    (format s "~&****** Testing ~S (variant ~A of ~A)~%" (ss-prob-name p) i l)
	    (setq status (second (assoc i (ss-prob-variantstatus p))))
	    (unless status (setq status (second v)))
	    (setq res (sst-aux (first v) status (ss-prob-types p) required (ss-prob-space p) s))
	    (when (not res) (setq failures (1+ failures)))
	    (when (and stopearly (not res)) (return-from sst nil)))
	  (setq i (1+ i)))))
    (if (= failures 0) 
	(format s "~&Complete Success~%")
	(format s "~&Failure on ~A test~A~%" failures (if (> failures 1) "s" "")))
    (ss-hampi-kill)
    (= failures 0)))

(defun sst-invert-status (x) (case x (:inconsistent :valid) (:contingent :contingent) (:valid :inconsistent)))

(defun sst-aux (phi status types required space s)
  (when (and required (atom required)) (setq required (ss-guess-required phi types)))
  (let (result (tme (get-universal-time)))
    (setq *hampi-errored* nil)
    (setq result (ss-test-solve phi status types required space))
    (setq tme (- (get-universal-time) tme))
    (cond (result
	   (format s "~&****** Success in ~A seconds~%" tme))
	  (t
	   (format s "~&****** Failure in ~A seconds on ~A problem~%" tme status)
	   (format s "HAMPI ERROR: ~A~%" *hampi-errored*)
	   (format s "REQUIRED: ~A~%" required)
	   (format s "PHI: ~S~%TYPES: ~%" phi)
	   (pretty-print types s)
	   (format s "~&SPACE: ~S~%" space)))
    result))

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
    (setq bl (hash2bl (ss-solve p :types types :required (union required (vars p)))))
    (unless *quiet* (format t "~&Solution: ~S~%" bl))
    (cond ((eq status :inconsistent) (not bl))
	  ((not bl) nil)
	  (t (ss-checksat p types (append bl '((t . t))))))))

(defun ss-checksat (p types bl)
  (setq p (plug p bl))
  (and (ss-checksat-types bl types)
       (ss-checksat-aux p)))

(defun ss-checksat-types (bl types)
  (let (y)
    (dolist (b bl t)
      (when (varp (car b))
	(setq y (gethash (car b) types))
	(when y
	  (unless (ss-checkreg (ss-cast-tostring (cdr b)) (second (third y)))
	    (when *ss-debug* (format t "~&Checksat type failure: ~A~%" b))
	    (return nil)))))))

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
	(t (ss-checksat-aux-simplify p))))

(defun ss-checksat-aux-simplify (p)
  "(SS-CHECKSAT-AUX-SIMPLIFY P) returns T/NIL for sentences or a value
   for a term, assuming P is ground."
  (cond ((atom p) p)
	((eq (car p) 'len) (length (second p)))
	((find (car p) '(contains notcontains))
	 (setq p (ss-cast p))
	 (let (v)
	   (setq v (search (third p) (second p)))
	   (if (eq (car p) 'contains) v (not v))))
	((find (car p) '(in notin)) 
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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Problem Cleansing ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun ss-cleanse (prob)
  "(SS-CLEANSE PROBLEM) takes possibly screwy outside problem definitions and cleanses
   it for use in included routines.  Destructively modifies PROB."
  (setq *ss-varmapping* nil)

  ; remove (var "myCaseSensitiveVar") and augment ss-prob-varnames as appropriate
  (ss-cleanse-varcases prob)

  ; wrap reg around regexps, as necessary
  (setf (ss-prob-phi prob) (ss-fix-innotin (ss-prob-phi prob)))
  (setf (ss-prob-space prob) (ss-fix-innotin (ss-prob-space prob)))
  (setf (ss-prob-types prob) (mapcar #'ss-fix-innotin (drop-op (ss-prob-types prob))))

  ; variable name fixes (for Hampi, not us)
  (setf (ss-prob-phi prob) (maptree #'make-nice-variable (ss-prob-phi prob)))
  (setf (ss-prob-space prob) (maptree #'make-nice-variable (ss-prob-space prob)))
  (setf (ss-prob-types prob) (maptree #'make-nice-variable (ss-prob-types prob)))

  (setf (ss-prob-varnames prob) (butlast (compose-mgus (list (append (ss-prob-varnames prob) truth)
							     (append *ss-varmapping* truth)))))

  ; turn types into a hash table
  (let ((e (make-hash-table)))
    (mapc #'(lambda (x) (setf (gethash (second x) e) x)) (ss-prob-types prob))
    (setf (ss-prob-types prob) e))

  prob)

(defun ss-uncleanse (thing prob)
  "(SS-UNCLEANSE THING PROB) undoes the externally important changes made by ss-cleanse.
   Destructive."
  (cond ((atom thing) (nsublis (ss-prob-varnames prob) thing))  
	((listp thing) (nsublis (ss-prob-varnames prob) thing))
	((ss-prob-p thing)
	 (setf (ss-prob-phi thing) (nsublis (ss-prob-varnames prob) (ss-prob-phi thing)))
	 (setf (ss-prob-space thing) (nsublis (ss-prob-varnames prob) (ss-prob-space thing)))
	 (setf (ss-prob-types thing) (nsublis (ss-prob-varnames prob) (ss-prob-types thing)))
	 thing)
	(t thing)))

(defun ss-cleanse-varcases (prob)
  (let (*ss-varmapping* *ss-vars*)
    (setq *ss-varmapping* nil)
    (setq *ss-vars* (union* (vars (ss-prob-phi prob))
			    (vars (ss-prob-types prob))
			    (vars (ss-prob-space prob))))
    (setf (ss-prob-phi prob) (ss-cleanse-varcases-aux (ss-prob-phi prob)))
    (setf (ss-prob-space prob) (ss-cleanse-varcases-aux (ss-prob-space prob)))
    (setf (ss-prob-types prob) (ss-cleanse-varcases-aux (ss-prob-types prob)))
    (setf (ss-prob-varnames prob) *ss-varmapping*)))

(defun ss-cleanse-varcases-aux (p)
  "(SS-CLEANSE-VARCASES-AUX P VARS) replaces all (var 'varName') in P with a new variable name ?v
  and returns a new P, modifying *ss-varmapping* and *ss-vars*."
  (cond ((atom p) p)
	((eq (first p) 'var) 
	 (let (v p2)
	   (cond ((setq p2 (rassoc (second p) *ss-varmapping* :test #'equal))
		  (car p2))
		 (t
		  (setq p2 (tostring (list "?" (second p))))
		  (setq v (read-from-string p2))
		  (do () ((not (member v *ss-vars*))) 
		    (setq v (read-from-string (tostring (gentemp p2)))))
		  (push (cons v (second p)) *ss-varmapping*)
		  (push v *ss-vars*)
		  v))))
	(t (cons (car p) (mapcar #'ss-cleanse-varcases-aux (cdr p))))))

(defun ss-fix-innotin (p)
  (cond ((atom p) p)
	((find (car p) '(and or not <= => <=> forall exists))
	 (cons (car p) (mapcar #'ss-fix-innotin (cdr p))))
	((member (car p) '(in notin))
	 (if (stringp (third p))
	     (list (car p) (second p) `(reg ,(ss-fix-reg (third p))))
	     (list (car p) (second p) `(reg ,(ss-fix-reg (second (third p)))))))
	(t p)))

(defun ss-fix-reg (str)
  (let ((l (length str)))
    (cond ((= l 0) str)
	  (t
	   (if (and (char= (aref str 0) #\/)
		    (char= (aref str (1- l)) #\/))
	       (subseq str 1 (1- l))
	       str)))))    

(defun drop-op (l)
  (cond ((atom l) l)
	((atom (car l)) (cdr l))
	(t l)))

(defun make-nice-variable (a)
  (cond ((not (varp a)) a)
	(t
	 (push (cons nil a) *ss-varmapping*)  ; storing for easy reversal: (newvar . oldvar)
	 (setq a (tostring a))
	 (dotimes (i (length a))
	   (dolist (s '((#\[ . #\_) (#\] . #\_) (#\- . #\_)))
	     (if (char= (aref a i) (car s))
		 (setf (aref a i) (cdr s)))))
	 (setq a (read-from-string a))
	 (if (eq a (cdr (first *ss-varmapping*)))
	     (pop *ss-varmapping*)
	     (setf (car (first *ss-varmapping*)) a))
	 a)))
	 
(defun ss-replace-contains (p)
  (cond ((atom p) p)
	((find (car p) '(and or not <= => <=> forall exists)) 
	 (cons (car p) (mapcar #'ss-replace-contains (cdr p))))
	((eq (car p) 'contains) 
	 (list 'in (second p) (list 'reg (stringappend *strreg* (ss-regesc (third p)) *strreg*))))
	(t p)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Toplevel Invocations ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; These wrappers are necessary to ensure Hampi is initialized and terminated correctly.
(defun main (f)
  (ss-hampi-init)
  (multiple-value-prog1 (funcall f)
    (ss-hampi-kill)))

(defun notamp (p &key (numgood 1) (unique nil) (required nil) (types nil) (space 'true)) 
  (main #'(lambda () (notamper p :numgood numgood :unique unique :required required :types types
			       :space space))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Testcase generation ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun notamper (p &key (numgood 1) (unique nil) (required nil) (types nil) (space 'true))
  "(NOTAMPER P NUMGOOD UNIQUE) 
   takes a KIF formula P (in the notamper constraint language defined below)
   and generates NUMGOOD distinct variable assignments for P and multiple variable assignments
   satisfying (NOT P).  Notamper returns a list of two elements 
         ((good P bl1 ... bln) (bad (Pvariation1 bl1) ... (Pvariationm blm))).  
   Each bli is a binding list, i.e. a list of (var val).
   For all variables included in the UNIQUE list, the values assigned to those variables
   are unique across good and bad.
   The REQUIRED list is the set of variables required to have values.
   The SPACE argument is a set of constraints that all variable assignments must satisfy.
   The TYPES argument is a list of IN statements."
  (let ((history nil) bl (good nil) (bad nil) firstgood orlist tmp)
    (assert (and (integerp numgood) (>= numgood 0)) nil 
	    "NOTAMPER requires NUMGOOD to be a non-negative integer.")

    ; compute required and unique if set to T
    (when (and (atom required) required) (setq required (ss-guess-required p types)))
    (when (and (atom unique) unique) (setq unique (ss-guess-unique p required)))

    ; compute NUMGOOD unique good answers, distributed across DNF pieces of P
    (unless *quiet* (format t "~&~%~%**** Generating Good Answers ****~%~%"))
    (setq firstgood nil)
    (setq orlist (or2set (dnf p)))
    (do ((i 0))
	((or (>= i numgood) (null orlist)))
      (dolist (d orlist)
	(when (>= i numgood) (return))
	(unless *quiet* 
	  (format t "~&**Solving ~S (~A of ~A good)~%" p (1+ i) numgood))
	(setq bl (notamper-solve-good d required types space history))
	(cond (bl
	       (unless firstgood (setq firstgood (remove-if #'(lambda (x) (member (first x) unique)) bl)))
	       (push bl good)
	       (setq history (notamper-newhist history bl unique))
	       (setq i (1+ i)))
	      (t ; this disjunct has no more solutions
	       (setq orlist (delete d orlist :test #'equal))))
	(when *ss-debug* (format t "~&  Solution: ~S~%" bl))))
    (setq good (remove-duplicates good :test #'equal))
    (when (not good) 
      (unless *quiet* (format t "~&~%**** No good answers found.  Aborting.****~%~%"))
      (return-from notamper nil))

    ; compute one bad answer for each disjunct in DNF
    (unless *quiet* (format t "~&~%~%**** Generating Bad Answers ****~%~%"))
    (dolist (v (or2set (dnf (maknot p))))
      (unless *quiet* (format t "~&**Solving ~S~%" v))
      (setq bl (notamper-solve-bad v required types space history p firstgood))
      (when *ss-debug* (format t "~&  Solution: ~S~%" bl))
      (when bl (push (list v bl) bad))
      (setq history (notamper-newhist history bl unique)))
    (setq bad (remove-duplicates bad :test #'blequal :key #'second))

    ; check if an element of good is an element of bad. If so, warn and then throw out. 
    (setq tmp nil)
    (dolist (b bad)
      (when (member (second b) good :test #'blequal)
	(unless *quiet* 
	  (format t "~&!! Warning: bad solution also a good solution.  Throwing out bad. !!~%")
	  (format t "~S~%" b))
	(push b tmp)))
    (setq bad (delete-if #'(lambda (x) (member x tmp :test #'equal)) bad))

    ; return two lists
    (list (list* 'good p (nreverse good))
	  (cons 'bad (nreverse bad)))))

(defun notamper-solve-good (p required types space history)
  (let (bl)
    (setq p (makand (notamper-addhist p history (union (vars p) required)) space))
    ; asking for solution including all vars in P plus required so we can checksat
    (setq bl (ss-solve p :required (union (vars p) required) :types types))
    (when bl
      (assert (ss-checksat p types (append (hash2bl bl) '((t . t)))) nil 
	      (format nil "In notamper-solve-good, ss-solve failed on ~%~S~%producing ~S" p (hash2bl bl)))
      (setq bl (hash2list bl)))
    bl))

(defun notamper-solve-bad (p required types space history origp goodbl)
  "(NOTAMPER-SOLVE-BAD P HISTORY REQUIRED TYPES SPACE ORIGP GOODBL) finds a solution to
   (and P SPACE) with at least REQUIRED variables having assignments.  TYPES includes
   the type for each variable; HISTORY includes the history of assignments up
   to this point for those variables that must be unique.  ORIGP is a sentence such that
   P implies (not ORIGP).  GOODBL includes a binding for all required, non-unique variables
   that jointly satisfy ORIGP.  "
  (let (bl unassigned vs p2 bl2)
    (setq vs (vars p))
    (setq p2 (makand (notamper-addhist p history vs) space))
    (setq bl (ss-solve p2 :required vs :types types)) ; required are just those in p
    (when bl
      (assert (ss-checksat p2 types (append (hash2bl bl) truth)) nil 
	      (format nil "In notamper-solve-bad, ss-solve failed initially on ~%~S~%producing ~S" p2 (hash2bl bl)))
      (setq bl (hash2list bl))
      (setq bl (delete-if-not #'(lambda (x) (member (first x) vs)) bl))  ; reduce var assign to original vs
      ; add variables that were required from good solution
      (setq unassigned (set-difference required (mapcar #'first bl)))
      (when *ss-debug* (format t "~&BL: ~S, unassigned: ~A~%" bl unassigned))
      (when unassigned
	; augment bl with goodbl (which includes assignments for all REQUIRED but no UNIQUE variables)
	(setq goodbl (remove-if-not #'(lambda (x) (member (first x) unassigned)) goodbl))
	(setq unassigned (set-difference unassigned (mapcar #'first goodbl)))
	(setq bl (nconc goodbl bl))
	(when *ss-debug* (format t "~&BL: ~S, unassigned: ~A~%" bl unassigned))
	(when unassigned
	  ; augment bl with remaining variables (which must be UNIQUE) by invoking solver on origp
	  ; since we only care about unassigned vars, there is no need to update history because previous
	  ;    BL left the unassigned variables unassigned.
	  (setq p2 (makand (notamper-addhist origp history unassigned) space))
	  (setq bl2 (ss-solve p2 :types types :required (union unassigned (vars p2))))
	  (cond ((not bl2) (setq bl nil))
		(t
		 (assert (ss-checksat p2 types (append (hash2bl bl2) truth)) nil 
			 (format nil "In notamper-solve-bad, ss-solve failed secondarily on ~%~S~%producing ~S" 
				 p2 (hash2bl bl)))
		 (setq bl2 (hash2list bl2))
		 (setq bl2 (remove-if-not #'(lambda (x) (member (first x) unassigned)) bl2))
		 (setq bl (nconc bl2 bl)))))))
    bl))

(defun notamper-newhist (history bl unique) 
  "(NOTAMPER-NEWHIST HISTORY BL UNIQUE) augments HISTORY to reflect most recent
   assignment BL, where we only need store variables occurring in UNIQUE list."
  (when (not (hash-table-p history)) (setq history (make-hash-table)))
  (dolist (b bl history)
    (when (member (first b) unique)
      (push (second b) (gethash (first b) history)))))
;  (nconc (remove-if-not #'(lambda (x) (member (first x) unique)) bl) history))

(defun notamper-addhist (p history vs)
  "(NOTAMPER-ADDHIST P HISTORY) augments P to include constraints from HISTORY
   on variables VS."
  (makand (maksand (mapcarnot #'(lambda (x) (if (member (car x) vs)
						(list 'notin (car x) (ss-set2reg (cdr x)))
						nil)) 
			     (hash2bl history))) 
	  p))

(defun ss-set2reg (s)
  (setq s (mapcar #'ss-regesc (mapcar #'tostring s)))
  (setq s (tostring (cons (first s) (mapcan #'(lambda (x) (list "|" x)) (cdr s)))))
  (makreg s))

(defun blequal (l1 l2) (setequal l1 l2 :test #'equal))

(defun setequal (l1 l2 &key (test #'eq) (key #'identity))
  (and (null (set-difference l1 l2 :test test :key key))
       (null (set-difference l2 l1 :test test :key key))))

(defun ss-guess-required (p types)
  (when (hash-table-p types) (setq types (hash2list types)))
  (union (vars p) (vars types)))

(defun ss-guess-unique (p required)
  "(SS-GUESS-UNIQUE P) returns all variables not appearing in a positive = statement."
  (let (v)
    (setq p (nnf p))
    (setq v (find-pos-eq-vars p))
    (setq v (union v (find-pos-in-vars p)))
    (set-difference required v)))

(defun ss-guess-maxlen (p)
  "(SS-GUESS-MAXLEN P) guesses a maxlength for the constraints in P."
  (let ((m (find-terms #'numberp p)))
    (if m (+ 10 (apply #'max m)) 20)))

(defun find-pos-eq-vars (p)
  "(FIND-POS-EQ-VARS P) finds all variables appearing in positive = literals.
   Assumes P is in NNF."
  (cond ((atom p) nil)
	((find (car p) '(and or => <= <=> forall exists))
	 (mapcan #'find-pos-eq-vars (cdr p)))
	((eq (car p) 'not) nil)
	((eq (car p) '=) (cond ((varp (second p)) 
				(if (varp (third p)) (list (second p) (third p)) (list (second p))))
			       ((varp (third p)) (list (third p)))
			       (t nil)))
	(t nil)))

(defun find-pos-in-vars (p)
  "(FIND-POS-IN-VARS P) finds all variables appearing in positive in literals.
   Assumes P is in NNF."
  (cond ((atom p) nil)
	((find (car p) '(and or => <= <=> forall exists))
	 (mapcan #'find-pos-in-vars (cdr p)))
	((eq (car p) 'not) nil)
	((eq (car p) 'in) (if (varp (second p)) (list (second p)) nil))
	(t nil)))

;;;;;;;;;;;;;;; Testcase Output ;;;;;;;;;;;;;;;

(defun notamper2xml (notamperout prob &key (stream t) (time nil) (count nil)) 
  "(NOTAMPER2XML NOTAMPEROUT S) takes the output of notamper and prints to stream S
   an XML representation of it."
  (format stream "<form name=\"~A\">~%" (ss-prob-name prob))
  (printspaces 2 stream) (format stream "<attributes>~%")
  (dolist (m (ss-prob-metafields prob))
    (printspaces 4 stream)
    (format stream "<~(~A~)>~A</~(~A~)>~%" (first m) (second m) (first m)))
  (printspaces 4 stream) (format stream "<time>~A</time>~%" time)
  (printspaces 4 stream) (format stream "<count>~A</count>~%" count)
  (printspaces 2 stream) (format stream "</attributes>~%")
  (printspaces 2 stream) (format stream "<good>~%")
  (dolist (bl (cddr (first notamperout)))
    (when bl
      (printspaces 4 stream)
      (format stream "<case>~%")
      (notamper2xml-varassign nil bl 6 stream)
      (printspaces 4 stream)
      (format stream "</case>~%")))
  (printspaces 2 stream) (format stream "</good>~%")
  (printspaces 2 stream) (format stream "<bad>~%")
  (dolist (bl (cdr (second notamperout)))
    (when (second bl)
      (printspaces 4 stream)
      (format stream "<case>~%")
      (notamper2xml-varassign (vars (first bl)) (second bl) 6 stream)
      (printspaces 4 stream)
      (format stream "</case>~%")))
  (printspaces 2 stream) (format stream "</bad>~%")
  (format stream "</form>~%"))

(defun notamper2xml-varassign (tamperedvars bl n s)
  (dolist (x bl)
    (printspaces n s) 
    (format s "<bl><var tamper=\"~A\">~A</var><val>~A</val></bl>~%" 
	    (if (member (first x) tamperedvars) "true" "false")
	    (urlify (notamper2web-varspelling (first x))) 
	    (urlify (second x)))))
#|
(defun notamper2web (notamperout prob &key (stream t) (time nil) (count nil)) 
  "(NOTAMPER2WEB NOTAMPEROUT PROB &KEY STREAM TIME) takes the output of notamper and prints to stream 
   STREAM an http representation of it with headers described in PROB."
  (let (cnt)
    (setq cnt (+ (length (cddr (first notamperout)))
		 (length (cdr (second notamperout)))))
    (format stream "~&")
    (dolist (m (ss-prob-metafields prob))
      (format stream "~A: ~A~%" (first m) (second m)))
    (when count (format stream "Number of string problems solved: ~A~%" count))
    (when time (format stream "Time: ~A seconds~%" time))
    (format stream "Number of testcases generated: ~A~%" cnt)
    (format stream "~%")

    (if (ss-prob-multipart prob)
	(notamper2web-multi notamperout stream)
	(notamper2web-std notamperout stream))))

(defun notamper2web-std (notamperout stream)
    ; good
    (dolist (bl (cddr (first notamperout)))
      (notamper2web-std-bl bl stream "&"))
    (format stream "~%")
    
    ; bad
    (dolist (entry (cdr (second notamperout)))
      (notamper2web-std-bl (second entry) stream "&"))
    (format stream "~%"))

(defun notamper2web-std-bl (bl s sep)
  (when bl
    (format s "~A=~A" 
	    (urlify (notamper2web-varspelling (first (car bl)))) 
	    (urlify (second (car bl))))
    (dolist (b (cdr bl))
      (format s "~A~A=~A" sep 
	      (urlify (notamper2web-varspelling (first b))) 
	      (urlify (second b))))
    (format s "~%")))
	 
(defun notamper2web-multi (notamperout stream)
  ; good
  (format stream "~&**good~%~%")
  (dolist (bl (cddr (first notamperout)))
    (notamper2web-multi-bl bl stream))
    
  ; bad
  (format stream "~&**bad~%~%")
  (dolist (entry (cdr (second notamperout)))
    (notamper2web-multi-bl (second entry) stream)))

(defun notamper2web-multi-bl (bl s &optional (boundary "--AaB03x"))
  (when bl
    (format s "Content-Type: multipart/form-data; boundary=~A~%~%" boundary)
    (dolist (b bl)
      (format s "~A~%" boundary)
      (format s "Content-Disposition: form-data; name=\"~(~A~)\"~%~%"
	       (notamper2web-varspelling (first b)))
      (format s "~A~%"  (second b)))
    (format s "~&**nazari~%~%")))
|#

(defun notamper2web-varspelling (v)
  (cond ((varp v) (format nil "~(~A~)" (droparrayindex (devariable v))))
	((atom v) v)
	((eq (car v) 'var) (second v))
	(t v)))

(defun droparrayindex (s)
  (setq s (split-string (tostring s) '(#\[ #\])))
  (when (second s)
    (setf (second s) "[]"))
  (apply #'stringappend s))

; borrowed from Mike
(defun urlify (s)
  (unless (stringp s) (setq s (princ-to-string s)))
  (with-output-to-string (o)
    (do ((i 0 (1+ i)) (c) (n (length s)))
        ((= i n) o)
        (setq c (elt s i))
        (cond ((alphanumericp c) (write-char c o))
              ((find c '(#\$ #\- #\_ #\. #\+) :test #'char=) (write-char c o))
              (t (format o "%~:@(~2,'0x~)" (char-code c)))))))

;(notamper-build-negative-variations '(and (eq ?email1 ?email2) (eq ?pwd1 ?pwd2) (not (eq ?userid ""))))
(defun notamper-build-negative-variations (p)
  "(SS-BUILD-VARIATIONS P) takes a conjunction of literals P and constructs
   a set of conjunctions of literals--one for each conjunct in P but negated."
  (assert (or (literalp p)
	      (and (listp p) (eq (car p) 'and) (every #'literalp (cdr p))))
	  nil "SS-BUILD-VARIATIONS requires a conjunction of literals as input")
  (setq p (and2set p))
  (do ((s p (cdr s))
       (res nil) (sofar nil))
      ((null s) res)
    (push (maksand (revappend sofar (cons (maknot (car s)) (cdr s)))) res)
    (push (car s) sofar)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; String Solving ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun stringfindx (thing p th)
  (let (h)
    (setq h (ss-solve (makand p (maksand (contents th))) :required (vars thing)))
    (if h (plug thing (nconc (hash2bl h) truth)) nil))) 
  
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
      (return-from ss-solve 'error))

    ; convert to dnf and solve independently
    (dolist (c (or2set (dnf p)))
      (setq c (copy-tree c))   ; so that we can use destructive ops below
      (setq c (ss-solve-atoms (mapcar #'ss-simplify-not (and2set c)) types required))
      (when c (return c)))))

(defun or2set (p)
  (cond ((atom p) (list p))
	((eq (car p) 'or) (cdr p))
	(t (list p))))

(defun and2set (p)
  (cond ((atom p) (list p))
	((eq (car p) 'and) (cdr p))
	(t (list p))))

;;;;;;;;;;;; Vocabulary checking ;;;;;;;;;;;;
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
			      (make-parameter :symbol 'reg :arity 1 :type 'function)
			      (make-parameter :symbol 'len :arity 1 :type 'function)
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
;(define-condition ss-type-error (error))

(defun ss-solve-atoms (ps types required)
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

(defun ss-solve-atoms-simplify (ps)
  "(SS-SOLVE-ATOMS SIMPLIFY PS) takes a list of atoms and simplifies them.  Returns 'error
   if an error was found.  Returns 'unsat if unsatisfiability discovered.  Otherwise, returns
   simplification of PS."
  (let (simps)
    (setq simps (mapcar #'ss-simplify (mapcar #'ss-orient ps)))
    (when (member 'error simps)
      (unless *quiet*
	(format t "~&Type errors found during simplification.  Aborting.~%")
	(do ((e simps (cdr e))
	     (p ps (cdr p)))
	    ((null e))
	 (when (eq (car e) 'error) (format t "  ~S~%" (car p)))))
      (return-from ss-solve-atoms-simplify 'error))
    (when (member 'false simps)
      (unless *quiet*
	(format t "~%Unsatisfiability found during simplification.  Simplified result: ~&  ~S~%" simps))
      (return-from ss-solve-atoms-simplify 'unsat))
    (delete 'true simps)))

(defun ss-tr-noteq (p)
  "(SS-TR-NOTEQ P) turns (!= ?x <atom>) into a regular expression constraint.  
   Assumes p has been oriented and that <atom> is not the empty string."
  (cond ((atom p) p)
	((and (eq (car p) '!=) (varp (second p)) (atom (third p)) (not (varp (third p))))
	 (assert (not (equal (third p) "")) nil 
		 (format nil "~&ss-tr-noteq encountered the empty string in: ~S~%" p))
	 `(notin ,(second p) ,(makreg (ss-regesc (ss-cast-tostring (third p))))))
	(t p)))

(defun makreg (s)
  (setq s (list 'reg s))
  (when *check-dynamic-regexp*
    (assert (ss-test-all-regexps `(in ?x ,s)) nil 
	    (format nil "makreg constructed untranslatable regexp: ~S~%run ~A on ~A~%" 
		    s *hampi-regtest* *hampi-regtest-tmp*)))
  s)

(defun ss-tr-noteq-empty (p)
  "(SS-TR-NOTEQ-EMPTY P) turns (!= ?x "") into (gt (len ?x) 0).  
   Assumes P has been oriented."
  (cond ((atom p) p)
	((and (eq (car p) '!=) (varp (second p)) (equal (third p) ""))
	 (ss-orient `(gt (len ,(second p)) 0)))
	(t p)))

(defun delete!=bool (lits)
  "(DELETE!=BOOL LITS) takes a list of literals and returns a logically equivalent list of literals
   without any literals of the form x != <bool> where x is a variable and <bool> is either true or false
   (and the binding list as a second value).  Destructive."
  (let (l var val)
    ; find a substitution to perform
    (setq l (find-if #'(lambda (x) (and (positive-literalp x) (eq (relation x) '!=) 
					(or (and (varp (second x)) (boolp (third x))) 
					    (and (varp (third x)) (boolp (second x))))))
		     lits))
    (cond ((not l) (values lits '((t . t))))
	  (t 
	   (if (varp (second l)) 
	       (setq var (second l) val (maknot (third l))) 
	       (setq var (third l) val (maknot (second l))))
	   (multiple-value-bind (newlits bl) 
	       (delete!=bool (nsubst val var (delete l lits :test #'equal)))
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
    (assert (and (not (search "error" out))
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
	((find (car p) '(in notin contains notcontains)) p)
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
(defun regp (x) (and (listp x) (eq (first x) 'reg)))

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

  
(defun ss-simplify (p)
  "(SS-SIMPLIFY P) Evaluates p if it is a ground atom to either true or false.
   Returns new p or 'error.  Also casts where necessary."
  ;(declare (notinline ss-simplify))
  (cond ((atom p) p)
	; in case non-atom given
	((find (car p) '(<= => or and not <=> forall exists))
	 (cons (car p) (mapcar #'ss-simplify (cdr p))))

	; len
	((lenp p)
	 (setq p (ss-cast-tostring (ss-simplify (second p))))
	 (cond ((stringp p) (length p))
	       ((varp p) `(len ,p))
	       (t 'error))) 
	 
	; relational operators
	((member (car p) '(= != lt lte gt gte in notin contains notcontains))
	 (setq p (ss-toerr (cons (car p) (mapcar #'ss-simplify (cdr p)))))
	 (when (eq p 'error) (return-from ss-simplify 'error))
	 (setq p (ss-cast p))  ; get types right to the extent that we can
	 (when (eq p 'error) (return-from ss-simplify 'error))
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
			     (t p)))
		      ((and (atom (second p)) (regp (third p)) (not (varp (second (third p)))))
		       (cond ((eq (car p) 'in) (if (ss-checkreg (second p) (second (third p))) 'true 'false))
			     ((eq (car p) 'notin) (if (ss-checkreg (second p) (second (third p))) 'false 'true))
			     (t p)))
		      (t p)))
	       (t p)))
	(t (ss-cast p))))

(defun ss-cast (p)
  "(SS-CAST P) takes an atom and casts its arguments to the correct type
   and returns the result or ERROR."
  (cond ((find (car p) '(lt lte gt gte))
	 (ss-toerr (list (car p) (ss-cast-tonum (second p)) (ss-cast-tonum (third p)))))
	((find (car p) '(= !=))
	 (cond ((and (listp (second p)) (eq (car (second p)) 'reg)) 'error)
	       ((and (listp (third p)) (eq (car (third p)) 'reg)) 'error)
	       ((stringp (second p)) (ss-toerr (list (car p) (second p) (ss-cast-tostring (third p)))))
	       ((stringp (third p)) (ss-toerr (list (car p) (ss-cast-tostring (second p)) (third p))))
	       ((numberp (second p)) (ss-toerr (list (car p) (second p) (ss-cast-tonum (third p)))))
	       ((numberp (third p)) (ss-toerr (list (car p) (ss-cast-tonum (second p)) (third p))))
	       ((boolp (second p)) (ss-toerr (list (car p) (second p) (ss-cast-tobool (third p)))))
	       ((boolp (third p)) (ss-toerr (list (car p) (ss-cast-tobool (second p)) (third p))))
	       ((lenp (second p)) (ss-toerr (list (car p) (second p) (ss-cast-tonum (third p)))))
	       ((lenp (third p)) (ss-toerr (list (car p) (ss-cast-tonum (second p)) (third p))))
	       (t p)))
	((find (car p) '(in notin))
	 (ss-toerr (list (car p) (ss-cast-tostring (second p)) (ss-cast-toreg (third p)))))
	((find (car p) '(contains notcontains))
	 (ss-toerr (list (car p) (ss-cast-tostring (second p)) (ss-cast-tostring (third p)))))
	(t p)))

(defun ss-toerr (x) 
  (cond ((atom x) (if (eq x 'error) 'error x))
	((listp x) (if (member 'error x) 'error x))
	(t x)))

(defun ss-cast-tonum (x) 
  (cond ((boolp x) (if (eq x 'true) 1 0))
	((numberp x) x)
	((stringp x) 
	 (cond ((equal x "") 'error) 
	       (t (setq x (ignore-errors (read-from-string x nil nil))) 
		  (if (numberp x) x 'error))))
	((varp x) x)
	((lenp x) x)
	(t 'error)))

(defun ss-cast-tostring (x)
  (cond ((boolp x) (tostring x))
	((numberp x) (tostring x))
	((stringp x) x)
	((varp x) x)
	((lenp x) x)
	(t 'error)))

(defun ss-cast-tobool (x)
  (cond ((boolp x) x)
	((numberp x) (if (= x 0) 'false 'true))
	((stringp x) (cond ((or (equal x "1") (equal x "true")) 'true)
			   ((or (equal x "0") (equal x "false")) 'false)
			   (t 'error)))
	((varp x) x)
	((lenp x) x)
	(t 'error)))

(defun ss-cast-toreg (x)
  (cond ((varp x) 'error)
	((atom x) (list 'reg (format nil "~A" x)))
	((and (eq (car x) 'reg) (stringp (second x))) x)
	(t 'error)))
    
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
	       ((eq (car p) 'in) (cons 'notin (cdr p)))
	       ((eq (car p) 'notin) (cons 'in (cdr p)))
	       ((eq (car p) 'contains) (cons 'notcontains (cdr p)))
	       ((eq (car p) 'notcontains) (cons 'contains (cdr p)))))	       
	(t p)))    

;;;;;;;;;;;; Type inference/checking ;;;;;;;;;;;;
; kinds of terms: str num bool reg  (variables have computed types)

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
  (atype num)
  (atype str)
  (atype reg)
  (atype bool)

  (numericop lt)
  (numericop lte)
  (numericop gt)
  (numericop gte)
  (equality =)
  (equality !=)
  (regexp in)
  (regexp notin)
  (contop contains)
  (contop notcontains)

  (bool true)
  (bool false)

  ; type checking
  (basic num)
  (basic str)
  (<= (type ?op num num) (numericop ?op))
  (<= (type ?op ?y ?z) (equality ?op) (basic ?y) (basic ?z))
  (<= (type ?op str reg) (regexp ?op))

  ; type inference: assumes atoms are oriented
  (<= (argtype ?arg num) (c = ?arg (len ?x)))
  
  (<= (argtype ?arg num) (c ?p ?arg ?x) (numericop ?p))
  (<= (argtype ?arg num) (c ?p ?x ?arg) (numericop ?p))
  (<= (argtype ?arg str) (c ?p ?arg ?x) (regexp ?p))
  (<= (argtype ?arg str) (c ?p ?x (len ?arg)))
  (<= (argtype ?arg bool) (c = ?arg ?y) (bool ?y))

  (<= (argtype ?arg str) (c ?p ?arg ?x) (contop ?p))
  (<= (argtype ?arg str) (c ?p ?x ?arg) (contop ?p))

)

;;;;;;;;;;;; Len and range bounds ;;;;;;;;;;;;

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
      (cond ((and (numberp (second p)) (lenp (third p)))
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
	   
(defun ss-solve-iterate (ps len)
  "(SS-SOLVE-ITERATE PS) uses Hampi to find a solution to the atoms given as input.
   Assumes all atoms are one of 
        (= (len var) (len var))            us  Easy to check
        (!= var var)                       us  Easy to check
        (!= var (len var))                 us  Easy to check
        (!= (len var) (len var))           us  Easy to check
        (numop var var)                    us  Fold into search
        (numop var (len var))              us  Fold into search
        (numop (len var) (len var))        us  Easy to check
        (regop var (reg str))              hampi"

  (let (vs top bot ham h)
    (setq vs (ss-varordering (vars ps) ps len))
    (when *ss-debug* (print vs))
    (multiple-value-setq (top bot ham) (ss-index-constraints vs ps))
    (when *ss-debug*
      (format t "~&Top:~%")
      (pretty-print top)
      (format t "Bottom:~%")
      (pretty-print bot)
      (format t "Hampi:~%")
      (pretty-print ham))
    (setq *hampi* (create-hampi-simulator))  ; deprecated
    (setq h (ss-solve-iterate-aux vs (make-hash-table) len top bot ham))
    ; hash values returned are (len val), so here we simplify to just val.
    (when h (maphash #'(lambda (key val) (setf (gethash key h) (second val))) h))
    h))

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

(defun ss-index-constraints (vars ps)
  "(SS-INDEX-CONSTRAINTS VARS PS LEN) returns three hash tables, all keyed on VARS.
   All assume variable ordering is VARS.
   The first table contains all (len(var) != len(var)) constraints that can be checked 
   before calling HAMPI.  The second table contains all (var != var) constraints that 
   must be checked after calling HAMPI.  The third contains all constraints that must 
   be communicated to HAMPI (marked with hampi above)."
  (let (top bottom hampi order)
    ; first make it fast/easy to check if v1 occurs after v2 in ordering
    (setq order (make-hash-table))
    (do ((vs vars (cdr vs)))
	((null vs))
      (setf (gethash (car vs) order) (cdr vs)))

    ; now build the indexes
    (setq top (make-hash-table))
    (setq bottom (make-hash-table))
    (setq hampi (make-hash-table))
    (dolist (p ps (values top bottom hampi))
      (cond ((and (eq (car p) '!=) (varp (second p)) (varp (third p)))  ; bottom
	     (if (member (third p) (gethash (second p) order))
		 (push p (gethash (third p) bottom))
		 (push p (gethash (second p) bottom))))
	    ((and (eq (car p) '!=) (lenp (second p)) (lenp (third p)))
	     (if (member (second (third p)) (gethash (second (second p)) order))
		 (push p (gethash (second (third p)) top))
		 (push p (gethash (second (second p)) top))))
	    (t (push p (gethash (car (vars p)) hampi)))))))

(defun ss-solve-iterate-aux (vs state len top bottom hampi)
  ;(declare (notinline ss-solve-iterate-aux))
  (cond ((null vs) state)
	(t
	 (let (sofar interval forbids before after h al val)
	   (setq interval (gethash (car vs) len))
	   (setq forbids (cdr interval))
	   (setq interval (car interval))
	   (setq before (gethash (car vs) top))
	   (setq after (gethash (car vs) bottom))
	   (setq h (gethash (car vs) hampi))
	   (setq sofar nil)
           (unless *quiet* (format t "~&Solving var ~A for size" (car vs)))
	   (do ((i (first interval) (1+ i)))
	       ((> i (second interval)) nil)
	     (unless *quiet* (format t " ~A" i))
	     (setf (gethash (car vs) *hampi*) (gethash 'univ *hampi*))
	     (unless (member i forbids)          ; len(var) != int
	       (when (ss-sat-len (car vs) i state before)   ; len(var) != len(var)
		 (setq sofar nil)
		 (setq val t)
		 (do () ((eq val nil))
		   (setq val (ss-invoke-hampi (car vs) i (ss-construct-hinput (car vs) sofar h)))
		   (unless *quiet* (format t " (~A)" val)) 
		   (unless val (return))
		   (push val sofar)
		   (when (ss-sat-val (car vs) val state after)  ; var != var
		     (setf (gethash (car vs) state) (list i val))
		     (setq al (ss-solve-iterate-aux (cdr vs) 
						    state
						    len top bottom hampi))
		     ; don't need to undo setf to state since constraints only include
		     ;   the appropriate variables
		     (when al (return-from ss-solve-iterate-aux al)))))))))))

; (trace ss-sat-len ss-sat-val ss-invoke-hampi ss-construct-hinput ss-solve-iterate-aux)

(defun ss-sat-val (var val state ps)
  "(SS-SAT-VAL VAR VAL STATE PS) checks if VAR/VAL together with the other assignments in STATE
   satisfies all the constraints in PS.  All constraints in PS are of the form var != var."
  (let (var2)
    (dolist (p ps t)
      (if (eq (second p) var) (setq var2 (third p)) (setq var2 (second p)))
      (when (equal val (second (gethash var2 state))) (return nil)))))

(defun ss-sat-len (var len state ps)
  "(SS-SAT-LEN VAR LEN STATE PS) checks if VAL together with the other lengths in STATE
   satisfies all the constraints in PS.  All constraints in PS are of the form 
   len(var) != len(var)."
  (let (var2)
    (dolist (p ps t)
      (setq var2 (cadaddr p))
      (when (eq var2 var) (setq var2 (cadadr p)))
      (when (= len (first (gethash var2 state))) (return nil)))))

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
	       (otherwise (assert t nil (format nil "Unknown operator in ss-sat-constraints: ~S" p)))))

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

(defun ss-invoke-hampi (var len constraints) (ss-invoke-hampi-real var len constraints))

(defun ss-invoke-hampi-simulator (var len constraints) 
  (declare (ignore len constraints)) 
  (let (vals)
    (setq vals (gethash var *hampi*))
    (when (listp vals) (setf (gethash var *hampi*) (cdr vals)))
    (if (eq vals t) nil (car vals))))

(defun ss-invoke-hampi-real (var len constraints) 
  "(SS-INVOKE-HAMPI2 VAR LEN CONSTRAINTS) runs Hampi to find a string of length LEN
   that when assigned to VAR satisfies CONSTRAINTS."
  (cond ((null constraints) (if (= len 0) "" (tostring (maptimes #'(lambda () "a") len)))) 
	(t
	 (let ((f (tostring (list *hampi-input-file* *counter*))) out)
	   (if (< *counter* *hampi-max-storage*) (setq *counter* (1+ *counter*)) (setq *counter* 0))
	   (with-open-file (out f :direction :output :if-does-not-exist :create :if-exists :supersede)
	     (ss-invoke-hampi-writefile var len constraints out))
	   (setq out (with-output-to-string (s) 
		       (run-program *hampi-invoke* (append *hampi-invoke-args* (list f)) :output s)))
	   (setq out (ss-invoke-hampi-parse out))
	   (cond ((eq out 'error)
		  (setq *hampi-errored* t)
		  (if *break-on-hampi-error*
		      (break (format nil "Hampi errored for var ~A with length ~A found in file ~A~%"
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
	  (t (assert t nil 
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

(defun create-hampi-simulator ()
  (let (h)
    (setq h (make-hash-table))
    (setf (gethash 'univ h) '("aaa" "aab" "aba" "abb" "baa" "bab" "bba" "bbb"))
    h))

(defun ss-hampi-init () (run-program *hampi-init* *hampi-init-args*) (sleep 1))
(defun ss-hampi-kill () (run-program *hampi-kill* *hampi-kill-args*))


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