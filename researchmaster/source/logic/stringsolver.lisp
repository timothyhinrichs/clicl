; all regexps are pcre (perl compatible regular expressions)
; requires Kaluza as a helper, which only runs on Linux

(defVar *ss-max-length* 300 
  "maximum length of a string")
(defVar *ss-max-val* 999 
  "maximum value for a numeric variable")
(defVar *ss-guess-maxlen* t
  "whether or not to guess the value for *ss-max-length* by examining constraints")

(defVar *quiet* t
  "quiet basic progress and error printouts")
(defVar *ss-debug* nil
  "print out debugging info if t")
(defVar *break-on-external-solver-error* t
  "whether or not to break when hampi returns an error")
(defVar *check-dynamic-regexp* t
  "whether or not to ensure that our dynamically created regexps can be translated")
(defVar *command-line* nil
  "whether being executed from the command line.  If NIL, errors result in CCL breaks; otherwise, errors quit.")
(defParameter *ss-ignore-variable-source* nil
  "whether or not to treat the same variable name from get/post/cookie as the same variable")
(defParameter *ss-type-inference-full-depth* t
  "whether or not to ensure type inference is complete by searching to full depth of space")

; external helper routines
(defParameter *hampi-regtest-tmp* "/tmp/hampiregtest.txt"
  "location for testing regular expressions")
(defParameter *hampi-checkreg-tmp* "/tmp/bar.hmp"
  "location for testing a given string against a regular expression")
(defParameter *hampi-reg* "hampireg" "script for converting pcre regexps to hampi regexps")
(defParameter *hampi-regtest* "hampiregtest" "script for checking regexp translator works")
(defParameter *reg-check* "regcheck" "script that checks whether a given string belongs to a given regexp")

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
(defVar *solver-input-file* "/tmp/ss/b.kaluza" 
  "base temp filename for Kaluza input")
(defVar *solver-max-storage* 100 
  "max number of files stored on disk for any one solution attempt")
(defVar *store-external-solver-files* nil
  "whether to store all external solver attempts (up to *solver-max-storage*)")

(defParameter *solver-init* "echo")
(defParameter *solver-init-args* '("noop"))
(defParameter *solver-invoke* "kaluza")
(defParameter *solver-invoke-args* nil)
(defParameter *solver-kill* "echo")
(defParameter *solver-kill-args* '("noop"))


; internal globals
(defParameter *ss-canonical* "Used to store canonical representations of sentences.")
(defParameter *reg-special-chars* (list #\\ #\^ #\$ #\( #\) #\[ #\] #\{ #\} #\. #\* #\? #\| #\+ #\- #\/))
(defParameter *ss-cast-types* (make-hash-table))  ; to avoid passing types everywhere
(defIne-condition ss-type-error (error) ((text :initarg :text :reader text)))
(defIne-condition ss-symbol-error (error) ((text :initarg :text :reader text)))
(defIne-condition ss-boolean-error (error) ((text :initarg :text :reader text)))
(defIne-condition ss-syntax-error (error) ((text :initarg :text :reader text)))

(defun ss-regesc (s &optional (charlist *reg-special-chars*))
  "(SS-REGESC S) translates s so that all special characters are escaped for regular expressions."
  (let ((res nil) char)
    (dotimes (i (length s))
      (setq char (aref s i))
      (if (member char charlist)
	  (push (tostring (list #\\ char)) res)
	  (push char res)))
    (apply #'stringappend (nreverse res))))

(defParameter *charreg* 
  (stringappend "[" (ss-regesc (apply #'stringappend *reg-special-chars*)) " \\w!@#%&:=\"><;,'`~" "]"))
(defParameter *boolreg* "true | false")
(defParameter *strreg* (stringappend *charreg* "*"))
(defParameter *strreg1* (stringappend *charreg* "+"))
(defParameter *numreg* "[0-9]+")

(defVar *counter* 1)
(defVar *ss-solve-count* 0)
(defVar *ss-varmapping* nil "binding list for cleansing variables")
(defVar *ss-vars* nil "var list for cleansing variables")
(defVar *external-solver-errored* nil "whether or not the external solver errored")

; input problem structure
; possible statuses: inconsistent, valid, contingent
(defStruct SS-PROB 
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
       (ss-sat p bl)))

(defun ss-sat (p bl)
  (eq (handler-case (ss-evaluate (plug p bl))
	(ss-type-error () 'false))
      'true))
 
(defun ss-checksat-types (bl types)
  (let (y)
    (dolist (b bl t)
      (when (varp (car b))
	(setq y (gethash (car b) types))
	(when y
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
		 (unless (ss-checkreg (ss-cast-tostring (cdr b)) (ss-makreg (third y)))
		   (when *ss-debug* (format t "~&Checksat type failure: ~A~%" b))
		   (return nil)))))))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Input Cleansing ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
#|
(defun ss-canon (p)
  "(SS-CANON P) takes a sentence in the external format and transforms it into a canonical form."
  (setq p (ss-fix-php-ops p))
  (setq p (ss-fix-boolean-funcs p))
  (setq p (mapbool #'(lambda (x) (flatten-operator (delete-duplicates x :test #'sentequal))) p))
  (setq p (mapbool #'ss-eliminate-idempotent p))
  p)

(defun ss-eliminate-idempotent (p)
  (cond ((member (car p) '(tobool bool boolean)) 
	 (if (member (relation (second p)) '(and not = != tobool bool boolean < > lt gt lte gte >=
|#

(defun ss-cleanse (prob)
  "(SS-CLEANSE PROBLEM) takes input languages and reduces to internal, simplified
   language.  Most other routines assume cleansing has already occurred. Destructively modifies PROB."
  ; ordering of cleansing operations below is important

  ; drop initial operator from list of type specs
  (setf (ss-prob-types prob) (drop-op (ss-prob-types prob)))

  ; substitute PHP operators for internal operators
  (setf (ss-prob-phi prob) (ss-fix-php-ops (ss-prob-phi prob)))
  (setf (ss-prob-space prob) (ss-fix-php-ops (ss-prob-space prob)))

  ; handle boolean operators being used as functions
  (setf (ss-prob-phi prob) (ss-fix-boolean-funcs (ss-prob-phi prob)))
  (setf (ss-prob-space prob) (ss-fix-boolean-funcs (ss-prob-space prob)))

  ; reduce boolean operators to KIF operators
  (setf (ss-prob-phi prob) (boolops2kif (ss-prob-phi prob)))
  (setf (ss-prob-space prob) (boolops2kif (ss-prob-space prob)))

  ; eliminate duplicates inside boolean ops
  (setf (ss-prob-phi prob) (mapbool #'(lambda (x) (delete-duplicates x :test #'sentequal)) (ss-prob-phi prob)))
  (setf (ss-prob-space prob) (mapbool #'(lambda (x) (delete-duplicates x :test #'sentequal)) (ss-prob-space prob)))

  ; remove (var "myCaseSensitiveVar") and augment ss-prob-varnames as appropriate
  (setq *ss-varmapping* nil)
  (ss-cleanse-varcases prob)
  
  ; tweak regular expressions
  (setf (ss-prob-phi prob) (ss-fix-innotin (ss-prob-phi prob)))
  (setf (ss-prob-space prob) (ss-fix-innotin (ss-prob-space prob)))
  (setf (ss-prob-types prob) (mapcar #'ss-in2type (ss-prob-types prob)))

  ; remove syntactic sugar
  (setf (ss-prob-phi prob) (ss-drop-syntactic-sugar (ss-prob-phi prob)))
  (setf (ss-prob-space prob) (ss-drop-syntactic-sugar (ss-prob-space prob)))
  (setf (ss-prob-types prob) (mapcar #'ss-drop-syntactic-sugar (ss-prob-types prob)))

  ; miscellaneous hacks
  (setf (ss-prob-phi prob) (ss-fix-misc (ss-prob-phi prob)))
  (setf (ss-prob-space prob) (ss-fix-misc (ss-prob-space prob)))
  (setf (ss-prob-types prob) (mapcar #'ss-fix-misc (ss-prob-types prob)))

  ; do type inference and cast objs in constraints to satisfy types.
  (ss-cleanse-typeinference prob)

  ; simplify where possible
  (setf (ss-prob-phi prob) (ss-simplify (ss-prob-phi prob)))
  (setf (ss-prob-space prob) (ss-simplify (ss-prob-space prob)))
  (setf (ss-prob-types prob) (mapcar #'ss-simplify (ss-prob-types prob)))

  (setf (ss-prob-varnames prob)
;	(mapcar #'(lambda (x) (cons (car x) (cdr x)))
		(butlast (compose-mgus (list (append (ss-prob-varnames prob) truth)
					     (append *ss-varmapping* truth)))))

  ; turn types into a hash table
  (let ((e (make-hash-table)))
    (mapc #'(lambda (x) (setf (gethash (second x) e) x)) (ss-prob-types prob))
    (setf (ss-prob-types prob) e))

  prob)

(defun ss-cleanse-more* (thing prob &optional (justvars nil))
;(print (hash2bl (ss-prob-types prob))) 
 (let (*ss-varmapping* *ss-vars*)
    (unless justvars
      (setq thing (ss-fix-php-ops thing))
      (setq thing (ss-fix-boolean-funcs thing))
      (setq thing (boolops2kif thing))

      (setq thing (mapbool #'(lambda (x) (delete-duplicates x :test #'sentequal)) thing)))
    (setq *ss-varmapping* (ss-prob-varnames prob))
    (setq *ss-vars* (union* (vars (ss-prob-phi prob))
			    (vars (ss-prob-types prob))
			    (vars (ss-prob-space prob))))
    (setq thing (ss-cleanse-varcases-aux thing))
    (setf (ss-prob-varnames prob) *ss-varmapping*)
    (unless justvars
      (setq thing (ss-fix-innotin thing))
      (setq thing (ss-drop-syntactic-sugar* thing))
      (setq thing (ss-fix-misc thing)))
    thing))

(defun ss-cleanse-more (thing prob)
  "(SS-CLEANSE-MORE THING PROB) applies the full ss-cleanse to a new sentence THING
   in the context of PROB. "
  ; do lightweight cleansing

  (setq thing (ss-cleanse-more* thing prob))
  ; remove syntactic sugar completely
  (setq thing (ss-drop-syntactic-sugar thing)) 
  ; do type inference and cast objs in constraints to satisfy types.
  (setq thing (ss-cleanse-typeinference-more thing prob))
  ; simplify where possible
  (setq thing (ss-simplify thing))
  thing)

(defun ss-uncleanse (thing prob &optional (alist-builder #'ss-build-prob-alist))
  "(SS-UNCLEANSE THING PROB) undoes the externally important changes made by ss-cleanse.
   Destructive."
  (let (alist)
    (setq alist (funcall alist-builder prob))
    (cond ((atom thing) (nsublis alist thing))  
	  ((listp thing) (nsublis alist thing))
	  ((ss-prob-p thing)
	   (setf (ss-prob-phi thing) (nsublis alist (ss-prob-phi thing)))
	   (setf (ss-prob-space thing) (nsublis alist (ss-prob-space thing)))
	   (setf (ss-prob-types thing) (nsublis alist (ss-prob-types thing)))
	   thing)
	  (t thing))))

(defun ss-build-prob-alist (prob)
  (mapcar #'(lambda (x) (cons (first x) (list (third x) (second x)))) (ss-prob-varnames prob)))

(defun ss-fix-php-ops (p)
  (setq p (sublis '((&& . and)
		    (! . not))
		  p)))

(defun ss-fix-boolean-funcs (p)
  "(SS-FIX-BOOLEAN-FUNCS P) tries to simplify P so that all the boolean ops AND/OR/NOT appear
   at the top-level of the sentences, as prescribed in FOL.  When this cannot be accomplished,
   throws ss-boolean-error.  Note that throwing the error is necessary whenever 
   the usual conversion to DNF is inadequate, i.e. even if we flatten, we couldn't convert to
   DNF and give the underlying string solver a conjunction to solve." 
  (cond ((atom p) p)
	((member (car p) '(and or not => <= <=>)) (cons (car p) (mapcar #'ss-fix-boolean-funcs (cdr p))))
	((member (car p) '(forall exists)) (list (first p) (second p) (ss-fix-boolean-funcs (third p))))
	((member (car p) '(tobool bool boolean)) (ss-fix-boolean-funcs (second p)))
	(t (if (intersectionp (get-vocabulary p)
			      (list (make-parameter :symbol 'not)
				    (make-parameter :symbol 'and)
				    (make-parameter :symbol 'or))
			      :key #'parameter-symbol)
	       (error 'ss-boolean-error)
	       p))))

; basically hack central
; Here we handle a bunch of special cases we've run into that we can't really implement.
(defun ss-istrue (x)
  (setq x (ss-orient x))
  (cond ((atom x) nil)
	((eq (car x) '=) (sentequal (second x) (third x)))
	((eq (car x) '!=)
	 (cond ((atom (third x)) nil)
	       ((atom (second x))
		(and (eq (relation (third x)) 'concat) (stringp (second x)) (= (length (second x)) 0)))
	       (t nil)))
	((member (car x) '(gt gte lt lte))
	 (handler-case (eq 'true (ss-simplify x))
	   (condition () nil)))
	((eq (car x) 'require) (not (varp (second x))))
	(t nil)))

; bunch of hacks to handle language elements we can't handle in general
(defun ss-fix-misc (x)
  (setq x (ss-orient x))
  (cond ((atom x) x)
	((and (eq (second x) 'false) (eq (car x) '=))
	 (if (eq (signifier (third x)) 'strpos)
	     (cons 'notstrpos (cdr (third x)))
	     x))
	((equal (second x) "")
	 (cond ((eq (car x) 'lt) `(!= "" ,(third x)))
	       ((eq (car x) 'gt) 'false)
	       ((eq (car x) 'lte) 'true)
	       ((eq (car x) 'gte) `(= "" ,(third x)))
	       (t x)))
	(t x)))

(defun ss-cleanse-typeinference (prob)
  (let (types p)
    (setq p (list* (ss-prob-phi prob) 
		   (ss-prob-space prob) 
		   (ss-prob-types prob)))
    ; need to extract all of p's atoms and then flatten them.
    (setq p (find-atoms (maksand p)))
    (setq p (flatten-operator (maksand (mapcar #'(lambda (x) (flatten-functions x :universal nil)) p))))
    ; grab types
    (setq types (ss-type-inference (and2list p)))  ; list of (var . <list of types>)
    ; if ambiguity, don't include type in *ss-cast-types*
    (setq types (remove-if #'third types))  
    (setq types (mapcar #'(lambda (x) (cons (first x) (second x))) types))
    ; resolve conflicts
    ;(setq types (mapcar #'(lambda (x) (cons (car x) (ss-resolve-types (cdr x)))) types))
    (setq *ss-cast-types* (bl2hash types))
    ; fix constraints
    (setf (ss-prob-phi prob) (ss-cast (ss-prob-phi prob) types))
    ; add non-string types to prob-types
    (dolist (y types)
      (when (member (cdr y) '(num bool)) 
	(push (list 'type (car y) (cdr y)) (ss-prob-types prob))))
    ; return result
    prob))

(defun ss-cleanse-typeinference-more (thing prob)
  (let (types p)
    (setq p (list* thing
		   (ss-prob-phi prob) 
		   (ss-prob-space prob)
		   (if (hash-table-p (ss-prob-types prob)) 
		       (mapcar #'cdr (hash2bl (ss-prob-types prob)))
		       (ss-prob-types prob))))
    ; need to extract all of p's atoms and then flatten them.
    (setq p (find-atoms (maksand p)))
    (setq p (flatten-operator (maksand (mapcar #'(lambda (x) (flatten-functions x :universal nil)) p))))
    ; grab types
    (setq types (ss-type-inference (and2list p)))
    ; if ambiguity, don't include type in *ss-cast-types*
    (setq types (remove-if #'third types))  
    (setq types (mapcar #'(lambda (x) (cons (first x) (second x))) types))
    ; resolve conflicts
    ;(setq types (mapcar #'(lambda (x) (cons (car x) (ss-resolve-types (cdr x)))) types))
    (setq *ss-cast-types* (bl2hash types))
    ; fix constraints
    (setq thing (ss-cast thing types))
    ; skip adding non-string types to prob-types (since may not have sufficient info??)
    ;(dolist (y types)
    ;  (when (member (cdr y) '(num bool)) 
    ;	(push (list 'typedecl (car y) (cdr y)) (ss-prob-types prob))))
    ; return result
    thing))

(defun ss-cleanse-varcases (prob)
  (let (*ss-varmapping* *ss-vars*)
    (setq *ss-varmapping* nil)
    (setq *ss-vars* (union* (vars (ss-prob-phi prob))
			    (vars (ss-prob-types prob))
			    (vars (ss-prob-space prob))))
    (setf (ss-prob-phi prob) (ss-cleanse-varcases-aux (ss-prob-phi prob)))
    (setf (ss-prob-space prob) (ss-cleanse-varcases-aux (ss-prob-space prob)))
    (setf (ss-prob-types prob) (mapcar #'ss-cleanse-varcases-aux (ss-prob-types prob)))
    (setf (ss-prob-varnames prob) *ss-varmapping*)))

(defun ss-cleanse-varcases-aux (p)
  "(SS-CLEANSE-VARCASES-AUX P VARS) replaces all (var 'varName') in P with a new variable name ?v
  and returns a new P, modifying *ss-varmapping* and *ss-vars*. *"
  ; *ss-varmapping* is a list of (kifvar stringspelling [get/post/cookie/var])
  (flet ((uniquevar (strvar class)
	   (let (p v)
	     (setq p (tostring (list "?" strvar)))
	     (setq v (read-from-string p))
	     (do () ((not (member v *ss-vars*))) 
	       (setq v (read-from-string (tostring (gentemp p)))))
	     (push (list v strvar class) *ss-varmapping*)
	     (push v *ss-vars*)
	     v))
	 (varspelling (strvar)
	   (dotimes (i (length strvar))
	     (dolist (s '((#\[ . #\_) (#\] . #\_) (#\- . #\_)))
	       (if (char= (aref strvar i) (car s))
		   (setf (aref strvar i) (cdr s)))))
	   strvar))
    (cond ((varp p)   
	   (cond ((find p *ss-varmapping* :key #'first) p)
		 (t
		  (let (newp)
		    (setq newp (tosymbol (varspelling (subseq (tostring p) 1))))
		    (setq p (tosymbol (list "?" newp)))	   
		    (push (list p newp 'var) *ss-varmapping*)
		    (setq *ss-vars* (adjoin p *ss-vars*))
		    p))))	      
	  ((atom p) p)   
	  ((and (listp p) (listp (first p))) (ss-cleanse-varcases-aux (first p))) ; allows ((var ?x)) in place of (var ?x)
	  ((member (first p) '(var get post cookie request))
	   (cond ((null (cdr p)) (case (first p)
				   (get '$GET_ARRAY)
				   (post '$POST_ARRAY)
				   (cookie '$COOKIE_ARRAY)
				   (request '$REQUEST_ARRAY)
				   (otherwise 'UNKNOWN_VAR)))
		 (*ss-ignore-variable-source*
		  (let (varentry)
		    (setq varentry (find (second p) *ss-varmapping* :test #'equal :key #'second))
		    (if varentry (first varentry) (uniquevar (varspelling (tostring (second p))) 'var))))
		 (t
		  (let (src strvar varentry)
		    (setq src (first p))
		    (when (eq src 'request) (setq src 'var))   ; use VAR if src is unknown
		    (setq strvar (varspelling (tostring (second p))))
		    (if (eq src 'var)
 			(setq varentry (find strvar *ss-varmapping* :key #'second :test #'equal))
			(setq varentry (find-if #'(lambda (x) (and (or (eq (third x) 'var) (eq (third x) src)) (equal (second x) strvar))) 
						*ss-varmapping*)))
		    (cond (varentry 
			   (cond ((eq src (third varentry))
				  (first varentry))
				 ((eq src 'var)
				  (first varentry))
				 (t ; found entry with unknown src that is now known; changing to get/post/cookie
				  (setf (third varentry) src)
				  (first varentry))))
			  (t
			   (uniquevar (varspelling strvar) src)))))))
	(t (cons (car p) (mapcar #'ss-cleanse-varcases-aux (cdr p)))))))

(defun ss-in2type (p)
  (cond ((atom p) p)
	((find (car p) '(and or not <= => <=> forall exists))
	 (cons (car p) (mapcar #'ss-in2type (cdr p))))
	((member (car p) '(in typedecl)) (list 'type (second p) (ss-makreg (third p))))
	(t p)))

(defun ss-fix-innotin (p)
  (cond ((atom p) p)
	((find (car p) '(and or not <= => <=> forall exists))
	 (cons (car p) (mapcar #'ss-fix-innotin (cdr p))))
	((member (car p) '(in notin nin))
	 (list (car p) (second p) (ss-makreg (third p))))
	(t p)))

(defun ss-makreg (thing)
  "(MAKREG THING) attempts to coerce thing to a regular expression string."
  (cond ((stringp thing)
	 (setq thing (ss-fix-reg thing)))
#|
	 (when *check-dynamic-regexp*
	   (assert (ss-test-all-regexps `(in ?x ,thing)) nil 
		   (format nil "ss-makreg constructed untranslatable regexp: ~S~%run ~A on ~A~%" 
			   thing *hampi-regtest* *hampi-regtest-tmp*)))
|#
	((atom thing) (ss-break (format nil "Couldn't make ~S into a regexp" thing)))
	((eq (car thing) 'reg) (ss-makreg (second thing)))
	(t (setq thing (ss-simplify thing))
	   (if (stringp thing) (ss-makreg thing) (ss-break (format nil "Couldn't make ~S into a regexp" thing))))))
  
(defun ss-fix-reg (str)
  "(SS-FIX-REG STR) drops surrounding / and / off of string."
  (setq str (string-trim '(#\Space #\Return #\Linefeed) str))
  (let ((l (length str)))
    (cond ((= l 0) str)
	  ((= l 1) str)
	  (t
	   (if (and (char= (aref str 0) #\/) (char= (aref str (1- l)) #\/))
	       (subseq str 1 (1- l))
	       str)))))
#|  Adding / / around reg
	   (setq pre (if (char= (aref str 0) #\/) "" "/"))
	   (setq post (if (char= (aref str (1- l)) #\/) "" "/"))
	   (stringappend pre str post)))))
|#


;;;;;;;;;;;;;;;;;; PHP syntactic sugar ;;;;;;;;;;;;;;;;;;

(defun ss-drop-syntactic-sugar (p) (ss-drop-sugar-sent p))
(defun ss-drop-syntactic-sugar* (p)
  "(SS-DROP-SYNTACTIC-SUGAR* P) is a lightweight version of ss-drop-syntactic-sugar*.
   It does translations that don't change the boolean structure of the sentence."
  (mapopands #'ss-drop-sugar-toplevel (mapatomterm #'(lambda (x) (ss-drop-sugar1* x)) p)))

(defun ss-drop-sugar-sent (p) (mapopands #'ss-drop-sugar-atom p))
(defun ss-drop-sugar-atom (p)  
  (mapopands #'ss-drop-sugar-toplevel (ss-drop-sugar-atom-core p)))


(defun ss-drop-sugar-toplevel (p)
  (let (rettype)
    (cond ((varp p) `(istrue ,p))
	  ((atom p) p)
	  ((equal p '(true)) 'true)
	  ((equal p '(false)) 'false)
	  (t
	   (setq rettype (viewfindx '?x `(returntype ,(car p) ?x) 'sstypes))
	   (case rettype
	     (num `(!= ,p 0))
	     (bool `(= ,p true))
	     (str `(and (!= ,p "") (!= ,p "0")))
	     (otherwise p))))))

(defun ss-drop-sugar-atom-core (p)
  (cond ((atom p) p)
	(t (multiple-value-bind (args extra) (mapcaraccum #'ss-drop-sugar-term (cdr p))
	     (setq p (cons (car p) args))
	     ; ideally, P would be a legitimate atom, but might be a term
	     (multiple-value-bind (newp newextra) (ss-drop-sugar p (car p))
	       (setq extra (mapcar #'ss-drop-sugar-sent (append newextra extra)))
	       (maksand (cons newp extra)))))))

(defun ss-drop-sugar-term (term)
  (cond ((atom term) (ss-drop-sugar term term))
	(t (multiple-value-bind (args extra) (mapcaraccum #'ss-drop-sugar-term (cdr term))
	     (setq term (cons (car term) args))
	     (multiple-value-bind (newterm extra2) (ss-drop-sugar term (signifier term))
	       (values newterm (nconc extra extra2)))))))

(defun ss-drop-sugar (term type)
  (setq term (ss-drop-sugar1 term type))
  (ss-drop-sugar2 term (signifier term)))

(defun ss-drop-sugar1* (term)
  (setq term (ss-drop-sugar1 term (signifier term)))
  (cond ((atom term) term)
	((eq (car term) '!=)  ; change (!= NULL <var>) to (require <var>)
	 (cond ((and (eq (second term) 'null) (varp (third term)))
		`(require ,(third term)))
	       ((and (eq (third term) 'null) (varp (second term)))
		`(require ,(second term)))
	       (t term)))
	(t term)))
	       

(defGeneric ss-drop-sugar1 (term type))
;  (SS-DROP-SUGAR1 TERM TYPE) handles the lightweight transformations of ss-drop-sugar.
;  In particular, doesn't change structure of formulas--no new vars, no new boolean ops.
;  Useful to split the two apart so that we can translate most of external language into
;  language that we can manipulate to some extent.

(defMethod ss-drop-sugar1 (term (type (eql 'strlen))) (list 'len (second term)))
(defMethod ss-drop-sugar1 (term (type (eql 'isset))) `(require ,(second term)))
(defMethod ss-drop-sugar1 (term (type (eql '==))) (cons '= (cdr term)))
(defMethod ss-drop-sugar1 (term (type (eql '===))) (cons '= (cdr term)))
(defMethod ss-drop-sugar1 (term (type (eql '<>))) (cons '!= (cdr term)))
(defMethod ss-drop-sugar1 (term (type (eql '>))) (cons 'gt (cdr term)))
(defMethod ss-drop-sugar1 (term (type (eql '<))) (cons 'lt (cdr term)))
(defMethod ss-drop-sugar1 (term (type (eql '>=))) (cons 'gte (cdr term)))
; can't translate <= as lte since <= is obviously implication
(defMethod ss-drop-sugar1 (term (type (eql '++))) `(+ ,(second term) 1))
(defMethod ss-drop-sugar1 (term (type (eql '--))) `(- ,(second term) 1))
; can't handle b/c of side effects: += -= *= /= %= .=  
(defMethod ss-drop-sugar1 (term (type (eql 'int))) `(tonum ,(second term)))
(defMethod ss-drop-sugar1 (term (type (eql 'integer))) `(tonum ,(second term)))
(defMethod ss-drop-sugar1 (term (type (eql 'intval))) `(tonum ,(second term)))
(defMethod ss-drop-sugar1 (term (type (eql 'bool))) `(tobool ,(second term)))
(defMethod ss-drop-sugar1 (term (type (eql 'boolean))) `(tobool ,(second term)))
(defMethod ss-drop-sugar1 (term (type (eql 'float))) `(tonum ,(second term)))
(defMethod ss-drop-sugar1 (term (type (eql 'double))) `(tonum ,(second term)))
(defMethod ss-drop-sugar1 (term (type (eql 'real))) `(tonum ,(second term)))
(defMethod ss-drop-sugar1 (term (type (eql 'string))) `(tostring ,(second term)))
(defMethod ss-drop-sugar1 (term (type (eql 'strval))) `(tostring ,(second term)))
(defMethod ss-drop-sugar1 (term (type (eql 'eregi))) `(in ,(third term) ,(second term)))
(defMethod ss-drop-sugar1 (term (type (eql 'ereg))) `(in ,(third term) ,(second term)))
(defMethod ss-drop-sugar1 (term (type (eql 'stripos))) (cons 'strpos (cdr term)))
; ignoring some functions
(defMethod ss-drop-sugar1 (term (type (eql 'htmlspecialchars))) (second term))
(defMethod ss-drop-sugar1 (term (type (eql 'stripslashes))) (second term))
(defMethod ss-drop-sugar1 (term (type (eql 'base64_decode))) (second term))
(defMethod ss-drop-sugar1 (term (type (eql 'addslashes))) (second term))
; default to no rewrite
(defMethod ss-drop-sugar1 (term type) (declare (ignore type)) term)

(defGeneric ss-drop-sugar2 (term type))
;  (SS-DROP-SUGAR2 TERM TYPE) removes the sugar from TERM, which is either
;   an atom or a functional term whose arguments have already been desugared.
;   Returns 2 values: the new term and a list of supporting statements.
;   If can't handle translation, returns (list* 'untyped term) so that 
;   type checker doesn't barf.  This ensures that a large constraint is
;   not deemed unsat just because we can't address some small portion of it.
;   Kaluza will return unsat if a given conjunction of atoms contains
;   an unknown symbol.

(defun ss-php-true (thing)
  (let (type)
    ; grab the type of thing
    (cond ((varp thing)
	   (cond ((hash-table-p *ss-cast-types*) (setq type (gethash thing *ss-cast-types*)))
		 ((listp *ss-cast-types*) (setq type (cdr (assoc thing *ss-cast-types*))))
		 (t (setq type nil))))
	  ((atom thing) 
	   (if (and (not (= thing 0)) (not (= thing "")) (not (= thing "0")) (not (= thing 'false)))
	       (setq type 'true) (setq type 'false)))
	  (t (setq type (viewfindx '?x `(returntype ,(car thing) ?x) 'sstypes))))

    ; construct expression
    (cond ((boolp type) type)
	  (t
	   (let (issetvar)
	     (setq issetvar (if (varp thing) (ss-drop-sugar-atom `(isset ,thing)) 'true))
	     (case type
	       (str (makand `(and (not (= ,thing "")) (not (= ,thing "0"))) issetvar))
	       (num (makand `(not (= ,thing 0)) issetvar))
	       (bool (makand `(not (= ,thing false)) issetvar))
	       (otherwise (makand `(and (not (= ,thing false)) (not (= ,thing 0)) (not (= ,thing "")) (not (= ,thing "0")))
				  issetvar))))))))



(defMethod ss-drop-sugar2 (term (type (eql 'istrue)))
  (ss-php-true (second term)))

(defMethod ss-drop-sugar2 (term (type (eql 'ltrim)))
  (let ((thing (second term))
	(chars (if (cddr term) (third term) '("\\t" "\\n" "\\r" " "))))
    (let ((y (newindvar)) 
	  (z (newindvar)) 
	  (white (stringappend "[" (apply #'stringappend chars) "]"))) 
      (values y `((= ,thing (concat ,z ,y))
		  (in ,z ,(stringappend white "*"))
		  (nin ,y ,(stringappend "^" white "+")))))))

(defMethod ss-drop-sugar2 (term (type (eql 'rtrim)))
  (let ((thing (second term))
	(chars (if (cddr term) (third term) '("\\t" "\\n" "\\r" " "))))
    (let ((y (newindvar)) 
	  (z (newindvar)) 
	  (white (stringappend "[" (apply #'stringappend chars) "]"))) 
      (values y `((= ,thing (concat ,y ,z))
		  (in ,z ,(stringappend white "*"))
		  (nin ,y ,(stringappend white "+$")))))))

(defMethod ss-drop-sugar2 (term (type (eql 'trim)))
  (if (cddr term)
      (ss-drop-sugar-term (list 'ltrim (cons 'rtrim (third term)) (third term)))
      (ss-drop-sugar-term `(ltrim (rtrim ,(second term))))))

(defMethod ss-drop-sugar2 (term (type (eql 'chop)))
  (ss-drop-sugar2 term 'rtrim))

(defMethod ss-drop-sugar2 (term (type (eql 'notstrpos)))
  ; bool notstrpos ( string $haystack , mixed $needle [, int $offset = 0 ] )
  (let* ((leftvar (newindvar))
	 (remainvar (newindvar)))
    (cond ((fourth term)
	   (values `(not (in ,remainvar ,(third term)))
		   `((= ,(second term) (concat ,leftvar ,remainvar))
		     (= (len ,leftvar) ,(fourth term)))))
	  (t
	   `(not (in ,(second term) ,(third term)))))))

(defMethod ss-drop-sugar2 (term (type (eql 'strpos)))
  ; int strpos ( string $haystack , mixed $needle [, int $offset = 0 ] )
  ; NOT CORRECTLY HANDLING WITH WHEN NEEDLE DOESN'T EXIST B/C STRPOS RETURNS FALSE, which is of the wrong type.
  ;  Instead, use NOTSTRPOS.
  (let* ((leftvar (newindvar))
	 (remainvar (newindvar))
	 (sents))
    (setq sents
	  `((= ,(second term) (concat (concat ,leftvar ,(third term)) ,remainvar))))
    (when (cdddr term)
      (push `(gte (len ,leftvar) ,(fourth term)) sents))
    (values `(len ,leftvar) sents)))

(defMethod ss-drop-sugar2 (term (type (eql 'strstr)))
  ; string strstr ( string haystack , string needle [, bool before_needle = false ] )
  ;  HANDLING WHEN NEEDLE CAN'T BE FOUND WEIRDLY.  RETURNING "" INSTEAD OF FALS.
  ;     NECESSARY SINCE FALSE IS OF THE WRONG TYPE.
  (let* ((leftvar (newindvar)) (rightvar (newindvar)) (outvar (newindvar))
	 (haystack (second term)) (needle (third term)) (beforeneedle (fourth term))
	 sents reg)
    (cond ((not (stringp needle)) term)  ; can't seem to do translation with variable for needle
	  (t
	   (setq reg (stringappend ".*" (ss-regesc needle) ".*"))
	   ; case when needle doesn't appear
	   (setq sents `((=> (not (in ,haystack ,reg))
			 (= ,outvar ""))))
	   ; case when needle does appear -- either return stuff after needle or before needle
	   (if (or (not beforeneedle) (eq beforeneedle 'false))
	       (push `(=> (in ,haystack ,reg)
			  (and (= ,haystack (concat ,leftvar (concat ,needle ,rightvar)))
			       (= ,outvar (concat ,needle ,rightvar))))
		     sents)
	       (push `(=> (in ,haystack ,reg)
			  (= ,haystack (concat ,outvar (concat ,needle ,rightvar))))
		     sents))
	   (values outvar sents)))))

(defMethod ss-drop-sugar2 (term (type (eql 'substr)))
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

(defMethod ss-drop-sugar2 (term (type (eql 'substr_replace)))
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

(defMethod ss-drop-sugar2 (term (type (eql 'preg_match)))
  ; preg_match($pattern, $subject, [$matches [, $flags [, $offset]]]);
  ; ignoring extra arguments
  (if (fourth term)
      (list* 'untyped term)
      `(in ,(third term) ,(second term))))

(defMethod ss-drop-sugar2 (term (type (eql 'preg_match_all)))
  ; int preg_match_all(string $pattern,string $subject,array &$matches  [, int $flags [, int $offset = 0  ]] )
  ; the purpose of this is to return the array of matches, and we can't do that.
  (list* 'untyped term))

(defMethod ss-drop-sugar2 (term (type (eql 'preg_replace)))
  ; preg_replace ($pattern , $replacement , $subject [, int $limit = -1 [, int &$count ]] )
  ; $pattern/$replacement can either be strings or arrays.
  ; implement an approximation: replace only one instance
  (cond ((fifth term) (list* 'untyped term))
	(t (ss-drop-sugar2-array-replace term 'preg_replace1))))

(defMethod ss-drop-sugar2 (term (type (eql 'str_replace)))
  ; str_replace ( mixed $search , mixed $replace , mixed $subject [, int &$count ] )
  ; replace only 1 instance
  (cond ((fifth term) term)
	(t (ss-drop-sugar2-array-replace term 'str_replace1))))

(defun ss-drop-sugar2-array-replace (term subreplace)
  (labels ((build (pattrepl subject)
	     (cond ((null pattrepl) subject)
		   (t `(,subreplace ,(car (first pattrepl))
				    ,(cdr (first pattrepl))
				    ,(build (cdr pattrepl) subject))))))
    (let (patterns replacements zip new)
      (setq patterns (if (arrp (second term)) (cdr (second term)) (list (second term))))
      (setq replacements (if (arrp (third term)) (cdr (third term)) (list (third term))))
      (do ((ps patterns (cdr ps))
	   (rs replacements (cdr rs)))
	  ((null ps) zip)
	(push (cons (car ps) (if (car rs) (car rs) "")) zip))
      (setq new (build zip (fourth term)))
      (ss-drop-sugar-term new))))

(defMethod ss-drop-sugar2 (term (type (eql 'str_replace1)))
  ; str_replace ($search , $replacement , $subject [, $limit [, $count]])
  ; CAUTION: only performs 1 replacement; ignores $limit and $count
  (cond ((stringp (second term))
	 (let* ((leftvar (newindvar))
		(rightvar (newindvar))
		(result (newindvar)))
	   (values result
		   `((=> (in ,(fourth term) ,(second term))
			 (and (= ,(fourth term) (concat ,leftvar (concat ,(second term) ,rightvar)))
			      (= ,result (concat ,leftvar (concat ,(third term) ,rightvar)))))
		     (=> (not (in ,(fourth term) ,(second term)))
			 (= ,result ,(fourth term)))))))
	  (t term)))

; Below is broken b/c there may not be a match to replace, and we're not handling that case.
; Problem may be pervasive throughout the above.
;    ?x
;    (substr ?y subject)
;    (in ?y pattern)
;    (=> (in ?y pattern)(= ?x (substr_replace ...)))
;    (=> (notin pattern subject) (= ?x subject))
(defMethod ss-drop-sugar2 (term (type (eql 'preg_replace1)))
  ; preg_replace ($pattern , $replacement , $subject [, $limit [, $count]])
  ; CAUTION: only performs 1 replacement; ignores $limit and $count
  (let* ((leftvar (newindvar)) (rightvar (newindvar)) (outvar (newindvar)) (matchvar (newindvar))
	 (haystack (fourth term)) (needle (second term)) (repl (third term)))
    (cond ((not (stringp needle)) term)  ; can't seem to do translation with variable for needle
	  (t
	   ;(setq reg (stringappend ".*" needle ".*"))
	   ; case when needle doesn't appear
	   (values outvar `((=> (not (in ,haystack ,needle))
				(= ,outvar ,haystack))
			    (=> (in ,haystack ,needle)
				(and (in ,matchvar ,needle)
				     (= ,haystack (concat ,leftvar (concat ,matchvar ,rightvar)))
				     (= ,outvar (concat ,leftvar (concat ,repl ,rightvar)))))))))))

(defMethod ss-drop-sugar2 (term (type (eql 'empty)))
   ; PHP 5 notion of empty (skipping NULL and objects)
  `(or (= ,(second term) "")
       (= ,(second term) 0)
       (= ,(second term) "0")
       (= ,(second term) false)
       (forbid ,(second term))))

(defMethod ss-drop-sugar2 (term type)
  (declare (ignore type))
  term)

#|
  (cond ((atom term) term)
	(t
	 (multiple-value-bind (args extra) 
	     (mapcaraccum #'(lambda (x) (ss-drop-sugar x (signifier x))) (cdr term))
	   (values (cons (car term) args) extra)))))
|#

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; String Solving ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; assumes inputs have already been cleansed via ss-cleanse

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
  (let (e q (*ss-max-length* *ss-max-length*))
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
      (cond ((eq (third y) 'num)
	     (setq p (makand `(or (gte ,(second y) 0) (lt ,(second y) 0)) p)))
	    ((eq (third y) 'bool)
	     (setq p (makand `(or (= ,(second y) true) (= ,(second y) false)) p)))))

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
	(setq q (mapcar #'ss-simplify-not (and2list c)))
	(handler-case (setq c (ss-solve-atoms q types required forbidden))
	  (ss-type-error ()       
	    (when *ss-debug* 
	      (format t "String solver returning UNSAT: type error:")
	      (pprint q)) 
	    (setq c nil))
	  (ss-symbol-error () (setq c nil)))
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


;;;;;;;;;;;; Conjunctive Constraint solving ;;;;;;;;;;;;
; actual solving.
(defVar *ss-tmp*)
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
  ;(setq *ss-tmp* types)
  (let (bl blvars psvars solvervars boundvars h vs r f basictypes *ss-cast-types*)  ; bl2

    ; rip out require/forbid atoms and add to required/forbidden lists
    (multiple-value-setq (r ps) (split #'(lambda (x) (member (reln x) '(require forbid))) ps))
    (multiple-value-setq (r f) (split #'(lambda (x) (eq (reln x) 'require)) r))
    (setq required (union (mapcarnot #'(lambda (x) (if (varp (second x)) (second x) nil)) r) required))
    (setq forbidden (union (mapcarnot #'(lambda (x) (if (varp (second x)) (second x) nil)) f) forbidden))
    (when (intersectionp required forbidden) (return-from ss-solve-atoms nil))
    
    ; check for degenerate true/false cases
    (setq ps (remove-truth1 (maksand ps)))
    (when (eq ps 'false) (return-from ss-solve-atoms nil))  
    (when (eq ps 'true) (return-from ss-solve-atoms (ss-augment-result3 (make-hash-table) nil required)))
    (when (tautp ps *ss-internal-complements*) (return-from ss-solve-atoms nil))
    (setq ps (and2list ps))

    ; infer types, resolve ambiguities, store so simplifier can operate
    (setq basictypes (ss-type-inference (append ps (mapcar #'cdr (hash2bl ps)))))
    (setq basictypes (mapcar #'(lambda (x) (cons (car x) (ss-resolve-types (cdr x)))) basictypes))
    (setq *ss-cast-types* (bl2hash basictypes))

    ; remove != constraints that are of the wrong type, since they are satisfied anyway.  (Kaluza gives type error.)
    ;   but ensure any var included in such a constraint is added to required var list.
    (multiple-value-bind (newps newreq) (ss-whitebox-remove-ne ps types)
      (setq ps newps)
      (setq required (union required newreq)))

    ; simplify constraints, evaluating what we can
    (setq ps (ss-solve-atoms-simplify ps))
    (when (not (listp ps)) (return-from ss-solve-atoms nil))
    (setq psvars (vars ps))
    (when (intersectionp psvars forbidden) (return-from ss-solve-atoms nil))  ; if var shows up in PS, we're finding value
    (setq ps (and2list (flatten-operator (flatten-functions (maksand ps) :universal nil))))

    ; remove equality for (= var ground); leave (= var unground) since we need flat terms for Kaluza.
    ;   REMOVED BECAUSE WE'RE MISSING UNSAT CASES
    ;(multiple-value-setq (ps bl) (delete-simple= ps :test #'equal))
    ;(when (eq ps :unsat) (return-from ss-solve-atoms nil))
    ; remove != for boolean variables with ground arg    REMOVED FOR SPECIFIC CASE WHERE TYPE OF VAR IS UNKNOWN
    ;(multiple-value-setq (ps bl2) (delete!=bool ps :test #'equal))
    ;(when (eq ps :unsat) (return-from ss-solve-atoms nil))
    ; remove duplicates
    (setq ps (remove-duplicates ps :test #'equal))
    ; combine variable bindings
    ;(setq bl (compose-mgus (list bl bl2)))

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
    (setq boundvars (instantiated-vars bl))
    (setq solvervars (set-difference (union* psvars required) blvars))
    (when (or (intersectionp forbidden blvars) (intersectionp forbidden psvars)) (return-from ss-solve-atoms nil))
    ;(format t "required: ~A, forbidden: ~A, blvars: ~A, psvars: ~A~%" required forbidden blvars psvars)
    ;(format t "psvars: ~A, required: ~A, solvervars: ~A~%" psvars required solvervars)
    ; add user-specified typing constraints for solver variables.
    (when types
      (dolist (v solvervars)
	(when (setq v (gethash v types))
	  (unless (ss-typep (third v))   ; already taken care of by ss-solve
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
	(ss-augment-result3 (bl2hash h) bl (union required boundvars)))))

(defun ss-whitebox-remove-ne (ps types)
  (let (var term newps y req)
    (setq newps nil)
    (setq req nil)
    (dolist (p ps (values (nreverse newps) (uniquify req)))
      (cond ((and (listp p) (eq (car p) '!=))
	     (cond ((and (varp (second p)) (atom (third p)) (not (varp (third p))))
		    (setq var (second p))
		    (setq term (third p)))
		   ((and (varp (third p)) (atom (second p)) (not (varp (second p))))
		    (setq var (third p))
		    (setq term (second p)))
		   (t (setq var nil)))
	     (cond (var
		    (setq y (gethash var types))
		    (cond ((not y) (push p newps))
			  ((stringp (third y)) 
			   (if (stringp term) 
			       (push p newps) 
			       (push var req)))
			  ((and (eq (car y) 'typedecl) (eq (third y) 'bool))
			   (if (or (eq term 'true) (eq term 'false))
			       (push p newps)
			       (push var req)))
			  ((and (eq (car y) 'typedecl) (eq (third y) 'num))
			   (if (numberp term)
			       (push p newps)
			       (push var req)))
			  (t (push p newps))))
		   (t (push p newps))))
	    (t (push p newps))))))



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
	((not (ss-symmetric (car p))) p)
	; (op non-atom atom)
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
    (otherwise nil)))

(defun boolp (x) (member x '(true false)))
(defun lenp (x) (and (listp x) (eq (first x) 'len)))
(defun regp (x) (stringp x))  ;(and (listp x) (eq (first x) 'reg)))
(defun concatp (x) (and (listp x) (eq (first x) 'concat)))
(defun typecastp (x) (and (listp x) (member (first x) '(tostr tobool tonum))))
(defun arrp (x) (and (listp x) (eq (first x) 'array)))
(defun ss-typep (x) (member x '(num str bool)))

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

(defun ss-simplify (p &optional (types *ss-cast-types*))
  "(SS-SIMPLIFY P) Evaluates p if it is a ground atom to either true or false.
   Returns new p or throws an ss-type-error.  Also casts where necessary."
  ;(declare (notinline ss-simplify))
  (let ((rec #'(lambda (x) (ss-simplify x types))))
    (cond ((atom p) p)
	  ((find (car p) '(or and not))
	   (remove-truth1 (cons (car p) (mapcar rec (cdr p)))))
	  
	; concat
	  ((concatp p)
	   (setq p (cons (car p) (mapcar #'(lambda (x) (ss-cast-tostring (funcall rec x))) (cdr p))))
	   (if (every #'stringp (cdr p))
	       (apply #'stringappend (cdr p)) 
	       p))

	; len
	  ((lenp p)
	   (setq p (ss-cast-tostring (funcall rec (second p))))
	   (cond ((stringp p) (length p))
		 ((varp p) `(len ,p))
		 (t (error 'ss-type-error :text (format nil "invalid len atom: ~A" p))))) 

	; explicit type casting
	  ((typecastp p)
	   (let (inner)
	     (handler-case (progn
			     (setq inner (funcall rec (second p)))
			     (cond ((eq (car p) 'tostr) 
				    (setq inner (ss-cast-tostring inner))
				    (if (stringp inner) inner `(tostr ,inner)))
				   ((eq (car p) 'tonum) 
				    (setq inner (ss-cast-tonum inner))
				    (if (numberp inner) inner `(tonum ,inner)))
				   ((eq (car p) 'tobool) 
				    (setq inner (ss-cast-tobool inner))
				    (if (boolp inner) inner `(tobool ,inner)))))
	       (ss-type-error () p))))
	  
	; modals
	  ((eq (car p) 'require) (if (varp (second p)) p 'true))
	  ((eq (car p) 'forbid) (if (varp (second p)) p 'false))
	  
					; relational operators
	  ((member (car p) '(= != lt lte gt gte in notin nin contains notcontains))
	   (setq p (cons (car p) (mapcar rec (cdr p))))
	   (setq p (ss-cast p types))  ; get types right to the extent that we can
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
	  (t (ss-cast p types)))))
  
(defun ss-cast (p &optional (types *ss-cast-types*))
  "(SS-CAST P TYPES) takes an arbitrary sentence p and for each atom 
   casts all args as required by types contained in ss-types theory.
   Throws ss-symbol-error and ss-type-error when ss-types has not enough info
   and when casting cannot be accomplished, respectively.
   Never adds explicit type casting function calls.  Tries
   to reduce explicit typecasts."
  (flet ((add-indices (list) 
	   (do ((l list (cdr l))
		(i 0 (1+ i)))
	       ((null l) list)
	     (setf (car l) (cons i (car l))))))
    (cond ((atom p) p)
	  ((eq (car p) 'untyped) p)
	  ((member (car p) '(and or not => <= <=>)) 
	   (cons (car p) (mapcar #'(lambda (x) (ss-cast x types)) (cdr p))))
	  ((member (car p) '(forall exists))
	   (list (first p) (second p) (ss-cast (third p) types)))
	  (t (let (args current legal newp type)
	       ; cast functional terms
	       (setq args (mapcar #'(lambda (x) (ss-cast x types)) (cdr p)))
	       ; grab types of all args
	       ;  current is indexed by argument position (starting at 0)
	       ;  the value for each arg is (arg . casteabletypes)
	       (setq current (make-hash-table))
	       (do ((i 0 (1+ i))
		    (as args (cdr as)))
		   ((null as))
		 (setf (gethash i current) (cons (car as) (ss-cast-argtype (car as) types))))
	       ; find legal types for args, add indices, and group by arg sets that must be the same.
	       (setq legal (viewfindx '?x `(reltype ,(car p) ?x) 'sstypes))
	       (unless legal (error 'ss-symbol-error :text (format nil "Type undefined for ~A" (car p))))
	       ; walk over all the types, and for those that are numbers, resolve current types
	       (dolist (l (group-by (add-indices (copy-list (cdr legal))) #'cdr))
		 (setq type (car l))
		 (when (numberp type)
		   (setq type (ss-resolve-arg-types (mapcar #'(lambda (x) (cdr (gethash (first x) current)))
							    (cdr l)))))
		   ; set the types for all args to the resolved type
		 (dolist (j (cdr l))
		   (setf (cdr (gethash (car j) current)) type)))
		     
	       ; now cast 
	       (dotimes (i (length args))
		 (let ((argtype (gethash i current)))  ; (arg . type)
		   (push (ss-cast-to (car argtype) (cdr argtype)) newp)))
	       (cons (car p) (nreverse newp)))))))

(defun ss-cast-to (thing type)
  (case type
    (str (ss-cast-tostring thing))
    (num (ss-cast-tonum thing))
    (bool (ss-cast-tobool thing))
    (otherwise thing)))

(defun ss-cast-argtype (a types) 
  (cond ((varp a) (if (hash-table-p types) (list (gethash a types)) (list (cdr (assoc a types)))))
	((stringp a) (let (v)
		       (setq v (handler-case (progn (ss-cast-tonum a) '(str num bool)) 
				 (ss-type-error () '(str bool))))
		       v))
	((boolp a) '(bool num str))
	((numberp a) '(num bool str))
	((member a '(num bool str)) (list a))
	((typecastp a) (let ((v (ss-cast-argtype (second a) types)))
			 (cons (case (car a) 
				 (tonum '(num . tonum))
				 (tostr '(str . tostr))
				 (tobool '(bool . tobool)))
			       v)))
;			 (if (member 'num v) v (append v (list 'num)))))
#|
	 (let (n s b)
	   (setq n (handler-case (ss-cast-tonum (second a)) (ss-type-error () nil)))
	   (setq s (handler-case (ss-cast-tostring (second a)) (ss-type-error () nil)))
	   (setq b (handler-case (ss-cast-tobool (second a)) (ss-type-error () nil)))
	   (delete-if #'not (list n s b))))
|#
	((listp a) (list (viewfindx '?x `(argtype ,a ?x) 'sstypes)))
	(t (error 'ss-symbol-error :text (format nil "Cannot compute argtype for ~A" a)))))

(defun ss-resolve-arg-types (types)
  "(SS-RESOLVE-ARG-TYPES TYPES) takes a list of lists of typename (str, num, bool) where the
   first element of each typename list is the current type of an argument to a relation.
   Remaining elements of each typename list are the other types the arg can be cast to.
   Chooses the type to cast all args to (since all our relations take the same typed args).
   Addresses as much implicit type casting as possible."
  (let (v)
    (setq v (car types))
    (dolist (y (cdr types))
      ; update original type counts
      ;(setq m (assoc (car y) origcnt))
      ;(if m (setf (cdr m) (1+ (cdr m))) (setq origcnt (acons (car y) 1 origcnt)))
      ; keep track of intersection of types
      (setq v (intersection v y :key #'(lambda (x) (if (consp x) (car x) x)))))
    (ss-resolve-types v)))

(defun ss-resolve-types (types)
  "(SS-RESOLVE-TYPES TYPES) takes a list of types and chooses a reasonable one."
      ; find string with maximal 
  (flet ((resolve (list)
	   (cond ((atom list) list)
		 ((member 'num list) 'num)
		 ((member 'str list) 'str)
		 ((member 'bool list) 'bool)
		 (t (first list)))))  ; handles case where all types are unknown
    (cond ((atom types) types)
	  (t 
	   (multiple-value-bind (typecasts raw) (split #'consp types)
	     (setq raw (set-difference raw typecasts :key #'(lambda (x) (if (consp x) (car x) x))))
	     (cond (raw (resolve raw))
		   (t (resolve (mapcar #'(lambda (x) (if (consp x) (first x) x)) types)))))))))

#|
(defun ss-cast (p)
  "(SS-CAST P) takes an atom and casts its arguments to the correct type
   and returns the result or ERROR.  Assumes P has already been simplified via ss-simplify."
  (cond ((find (car p) '(lt lte gt gte))
	 (list (car p) (ss-cast-tonum (second p)) (ss-cast-tonum (third p))))
	((find (car p) '(= !=))
	 (cond ((and (listp (second p)) (eq (car (second p)) 'reg)) (error 'ss-type-error :text (format nil "Casting problem: ~A" p)))
	       ((and (listp (third p)) (eq (car (third p)) 'reg)) (error 'ss-type-error :text (format nil "Casting problem: ~A" p)))
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
|#
#|
(defun ss-toerr (x) 
  (cond ((atom x) (if (eq x 'error) 'error x))
	((listp x) (if (member 'error x) 'error x))
	(t x)))
|#

(defun php-false (x) (or (eq x 'false) (eq x 0) (equal x "") (equal x "0")))
(defParameter *ss-false-values* #'php-false)

; assumes all types for variables are known
(defun ss-cast-tonum (x) 
  (cond ((boolp x) (if (funcall *ss-false-values* x) 0 1))
	((numberp x) x)
	((eq x 'num) x)
	((stringp x) 
	 (cond ((equal x "") (error 'ss-type-error :text (format nil "Cannot convert string to type NUM: ~A" x))) 
	       (t (setq x (ignore-errors (read-from-string x nil nil))) 
		  (if (numberp x) x (error 'ss-type-error :text (format nil "Cannot convert string to type NUM: ~A" x))))))
	((varp x) (let ((type (gethash x *ss-cast-types*)))
		    (cond ((not type) x)
			  ((eq type 'num) x)
			  (t (error 'ss-type-error :text (format nil "Cannot convert var to type NUM: ~A" x))))))
	((lenp x) x)
	((and (listp x) (eq (car x) 'tonum)) 
	 (handler-case (ss-cast-tonum (second x))
	   (ss-type-error () x)))
	((typecastp x) (ss-cast-tonum (second x)))
	(t (error 'ss-type-error :text (format nil "Cannot convert to type NUM: ~A" x)))))

(defun ss-cast-tostring (x)
  (cond ((boolp x) (tostring x))
	((numberp x) (tostring x))
	((stringp x) x)
	((eq x 'str) x)
	((varp x) (let ((type (gethash x *ss-cast-types*)))
		    (cond ((not type) x)
			  ((eq type 'str) x)
			  (t (error 'ss-type-error :text (format nil "Cannot convert var to type STR: ~A" x))))))
	((concatp x) x)
	((and (listp x) (eq (car x) 'tostr)) 
	 (handler-case (ss-cast-tostring (second x))
	   (ss-type-error () x)))
	((typecastp x) (ss-cast-tostring (second x)))
	(t (error 'ss-type-error :text (format nil "Cannot convert to type STR: ~A" x)))))

(defun ss-cast-tobool (x)
  (cond ((or (boolp x) (numberp x) (stringp x)) (if (funcall *ss-false-values* x) 'false 'true))
	((eq x 'bool) x)
	((varp x) (let ((type (gethash x *ss-cast-types*)))
		    (cond ((not type) x)
			  ((eq type 'bool) x)
			  (t (error 'ss-type-error :text (format nil "Cannot convert var to type BOOL: ~A" x))))))
	((and (listp x) (eq (car x) 'tobool)) x)
	((typecastp x) (ss-cast-tobool (second x)))
	(t (error 'ss-type-error :text (format nil "Cannot convert to type BOOL: ~A" x)))))

(defun ss-cast-toreg (x)
  (if (or (stringp x) (numberp x) (boolp x))
      (tostring x)
      (error 'ss-type-error :text (format nil "Cannot convert to REG: ~A" x))))
    
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
  (let (vs al th val val2 depthlimit)
    ; grab all types for all variables appearing in PS, using datalog
    (setq vs (vars ps))
    (setq al (mapcar #'(lambda (x) (cons x (quotify x))) vs))
    (push '(t . t) al)
    (setq al (nreverse al))
    (setq ps (plug ps al))
    (setq th (define-theory (make-instance 'prologtheory) "" (mapcar #'(lambda (x) (list 'atom x)) ps)))
    (includes th 'sstypes)
    (setq val nil)
    (if *ss-type-inference-full-depth*
	(setq depthlimit (* 3 (length vs)))
	(setq depthlimit 3))
    (dolist (v vs)
      (let ((*depth* depthlimit) ; ) 
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

(defParameter *ss-internal-complements*
  '((= . !=) (lt . gte) (gt . lte) (lt . gt) (in . notin) (in . nin) (require . forbid))
)
(defParameter *ss-internal-vocab* 
  (list (make-parameter :symbol 'lt :arity 2 :type 'relation)
	(make-parameter :symbol 'lte :arity 2 :type 'relation)
	(make-parameter :symbol 'gt :arity 2 :type 'relation)
	(make-parameter :symbol 'gte :arity 2 :type 'relation)
	(make-parameter :symbol '= :arity 2 :type 'relation)
	(make-parameter :symbol '!= :arity 2 :type 'relation)
	(make-parameter :symbol 'in :arity 2 :type 'relation)
	(make-parameter :symbol 'notin :arity 2 :type 'relation)
	(make-parameter :symbol 'nin :arity 2 :type 'relation)
	(make-parameter :symbol 'len :arity 1 :type 'function)
	(make-parameter :symbol 'concat :arity 2 :type 'function)
	(make-parameter :symbol '+ :arity 2 :type 'function)
	(make-parameter :symbol '* :arity 2 :type 'function)
	(make-parameter :symbol '/ :arity 2 :type 'function)
	(make-parameter :symbol '- :arity 2 :type 'function)
	;(make-parameter :symbol 'contains :arity 2 :type 'relation)
	;(make-parameter :symbol 'notcontains :arity 2 :type 'relation)
	(make-parameter :symbol 'type :arity 2 :type 'relation)
	(make-parameter :symbol 'true :arity 0 :type 'relation)
	(make-parameter :symbol 'false :arity 0 :type 'relation)
	(make-parameter :symbol 'require :arity 1 :type 'relation)
	(make-parameter :symbol 'forbid :arity 1 :type 'relation)
	(make-parameter :symbol 'tobool :arity 1 :type 'relation)
	(make-parameter :symbol 'tostr :arity 1 :type 'relation)
	(make-parameter :symbol 'tonum :arity 1 :type 'relation)
))
			       
(deftheory sstypes ""
  (atype str)
  (atype num)
  (atype bool)

  (numericop lt)
  (numericop lte)
  (numericop gt)
  (numericop gte)
  (math *)
  (math +)
  (math -)
  (math /)
  (equality =)
  (equality !=)
  (regexp in)
  (regexp notin)
  (regexp nin)
  (contop contains)
  (contop notcontains)
  (modal require)
  (modal forbid)
  (bool true)
  (bool false)

  ; Implicit casting
  (<= (reltype ?x (listof 1 1)) (equality ?x))
  (<= (reltype ?x (listof num num)) (numericop ?x))
  (<= (reltype ?x (listof str str)) (regexp ?x))
  (<= (reltype ?x (listof str str)) (contop ?x))
  (<= (reltype type (listof 1 2)))
  (<= (reltype tostr (listof 1)))
  (<= (reltype tonum (listof 1)))
  (<= (reltype tobool (listof 1)))
  (<= (reltype len (listof str)))
  (<= (reltype concat (listof str str)))
  (<= (reltype ?x (listof 1)) (modal ?x))
  (<= (reltype ?x (listof num num)) (math ?x))

  (<= (returntype ?x num) (math ?x))
  (<= (returntype len num))
  (<= (returntype concat str))
  (<= (returntype tostr str))
  (<= (returntype tonum num))
  (<= (returntype tobool bool))

  ;  Inferring the types of variables  (probably have redundant info, but seems to work for now)
  ;;;; basic types
  (<= (argtype num num))
  (<= (argtype bool bool))
  (<= (argtype str str))
  (<= (argtype ?x bool) (bool ?x))
  (<= (argtype ?x bool) (atom ?x) (evaluate (varp ?x) t))
  (<= (argtype ?x num) (evaluate (numberp ?x) t))
  (<= (argtype ?x str) (evaluate (stringp ?x) t))

  ;;;;; functional term types
  ; type casting
  (<= (argtype (tostr ?x) str))
  (<= (argtype (tonum ?x) num))
  (<= (argtype (tobool ?x) bool))
  ; len
  (<= (argtype (len ?x) num))
  (<= (argtype ?x str) (term (len ?x)))
  ; concat
  (<= (argtype (concat ?x ?y) str))
  (<= (argtype ?x str) (term (concat ?x ?y)))
  (<= (argtype ?x str) (term (concat ?y ?x)))

#|
  ; shouldn't need many of the below since we're now dealing with types after dropping syntactic sugar
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
|#  
  ; basic operations
  (<= (argtype ?x str) (atom (type ?x str)))
  (<= (argtype ?x num) (atom (type ?x num)))
  (<= (argtype ?x bool) (atom (type ?x bool)))
  (<= (argtype ?x str) (atom (type ?x ?y)) (evaluate (stringp ?y) t))

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

(defParameter *ss-kaluza-vocab* 
  (list (make-parameter :symbol 'lt :arity 2 :type 'relation)
	(make-parameter :symbol 'lte :arity 2 :type 'relation)
	(make-parameter :symbol 'gt :arity 2 :type 'relation)
	(make-parameter :symbol 'gte :arity 2 :type 'relation)
	(make-parameter :symbol '= :arity 2 :type 'relation)
	(make-parameter :symbol '!= :arity 2 :type 'relation)
	(make-parameter :symbol 'in :arity 2 :type 'relation)
	(make-parameter :symbol 'notin :arity 2 :type 'relation)
	(make-parameter :symbol 'nin :arity 2 :type 'relation)
	(make-parameter :symbol 'len :arity 1 :type 'function)
	(make-parameter :symbol 'concat :arity 2 :type 'function)
	(make-parameter :symbol '+ :arity 2 :type 'function)
	(make-parameter :symbol '* :arity 2 :type 'function)
	(make-parameter :symbol '/ :arity 2 :type 'function)
	(make-parameter :symbol '- :arity 2 :type 'function)
	;(make-parameter :symbol 'contains :arity 2 :type 'relation)
	;(make-parameter :symbol 'notcontains :arity 2 :type 'relation)
	(make-parameter :symbol 'type :arity 2 :type 'relation)
	;(make-parameter :symbol 'true :arity 0 :type 'relation)
	;(make-parameter :symbol 'false :arity 0 :type 'relation)
	;(make-parameter :symbol 'require :arity 1 :type 'relation)
	;(make-parameter :symbol 'forbid :arity 1 :type 'relation)
))

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
	   (when *ss-debug* (format t "~&Kaluza ran in ~A seconds on ~A~%" (- (get-universal-time) starttime) f))
	   (cond ((eq out :error)
		  (setq *external-solver-errored* t)
		  (if *break-on-external-solver-error*
		      (ss-break (format nil "Kaluza errored in file ~A~%" f))
		      :unsat))   ; errors usually appear b/c of type conflicts, which are unsat
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
  (ss-validate-syntax-kaluza constraints)
  (let (var)
    (dolist (c constraints)
      (when (negative-literalp c) (format s "!") (setq c (second c)))  ; shouldn't need this, but just in case
      (cond ((eq (car c) '!=)  ; patch for Kaluza bug
	     (setq var (read-from-string (tostring (gentemp "temp"))))
	     (ss-invoke-kaluza-writefile-constraint `(assign ,var ,c) s)
	     (ss-invoke-kaluza-writefile-constraint `(assert ,var) s))
	    ((eq (car c) 'lt) ; patch for Kaluza parsing bug
	     (ss-invoke-kaluza-writefile-constraint (list 'gt (third c) (second c)) s))
	    (t (ss-invoke-kaluza-writefile-constraint c s))))))

(defun ss-validate-syntax-kaluza (ps)
  (unless (every #'atomicp ps)
    (error 'ss-syntax-error :text (format nil "Non-atomic sentence passed to Kaluza: ~A" ps)))
  (ss-validate-vocab-kaluza ps))

(defun ss-validate-vocab-kaluza (ps)
  (ss-validate-vocab ps *ss-kaluza-vocab*))

(defun ss-validate-vocab-internal (ps)
  (ss-validate-vocab ps *ss-internal-vocab*))

(defun ss-validate-vocab (ps vocab)
  (let (errs)
    (setq errs (set-difference (remove-if #'isobject (get-vocabulary (maksand ps))) vocab :test #'equalp))
    (when errs
      (error 'ss-symbol-error :text (format nil "Unknown vocabulary: ~A" errs)))
    ps))
      
(defun ss-invoke-kaluza-writefile-constraint (p s)
  (ss-invoke-kaluza-writefile-atom p s)
  (format s ";~%"))

(defun ss-invoke-kaluza-writefile-p2i (c s)
  (ss-invoke-kaluza-writefile-term (second c) s)
  (format s " ")
  (ss-invoke-kaluza-writefile-op (first c) s)
  (format s " ")
  (ss-invoke-kaluza-writefile-term (third c) s))

(defun ss-invoke-kaluza-capeq (term)
  ; given a term, expand as appropriate
  ; numeric and concat terms must be assigned via := before they are compared
  ;   and len terms must be assigned via == before compared.
  (let (new)
    (cond ((atom term) term)
	  ((member (car term) '(concat * / + -))
	   (setq new (gentemp "?TMP"))
	   (values new (list 'assign new term)))
	  (t (setq new (gentemp "?TMP"))
	     (values new (list '= new term))))))

(defun ss-invoke-kaluza-writefile-atom (p s)
  (flet ((p2i (c) (ss-invoke-kaluza-writefile-p2i c s))
	 (capeq (term) (ss-invoke-kaluza-capeq term)))
    
    (cond ((atom p) (ss-invoke-kaluza-writefile-term p s))
	  ((eq (car p) 'assign)
	   (p2i (list (first p) (second p) (list 'meta (third p)))))
	  ((eq (car p) 'assert)
	   (format s "ASSERT(")
	   (ss-invoke-kaluza-writefile-term (second p) s)
	   (format s ")"))
	  ((member (car p) '(= != lt gt lte gte))
	   (let (new1 new2 extra1 extra2)
	     (multiple-value-setq (new1 extra1) (capeq (second p)))
	     (multiple-value-setq (new2 extra2) (capeq (third p)))
	     (when extra1 (p2i extra1) (format s ";~%"))
	     (when extra2 (p2i extra2) (format s ";~%"))
	     (p2i (list (car p) new1 new2))))
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
	 (format s "CapturedBrack(/~A/,0)" (second term))) ;(ss-regesc (second term) '(#\/))))		 
	((eq (car term) 'reglit)
	 (ss-assert (stringp (second term)) nil (format nil "Reglit terms must be (reglit <string>); found ~A" term))
	 (format s "CapturedBrack(~S,0)" (second term)))		 
	((eq (car term) 'len)
	 (ss-assert (atom (second term)) nil (format nil "Len terms must be (len <atom>); found ~A" term))
	 (format s "Len(") 
	 (ss-invoke-kaluza-writefile-term (second term) s)
	 (format s ")"))
	((eq (car term) 'concat) 
	 (ss-assert (and (atom (second term)) (third term)) nil 
		    (format nil "Concat terms must be (concat <atom> <atom>); found ~A" term))
	 (ss-invoke-kaluza-writefile-term (second term) s)
	 (format s " . ")
	 (ss-invoke-kaluza-writefile-term (third term) s))
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
  "(SS-INVOKE-KALUZA-PARSE STR) parses Kaluza's output to return a value."
  (setq str (string-trim '(#\Space) str))
  (cond ((eq (length str) 0) :unsat)
	((every #'(lambda (i) (or (= (length i) 0) (eq (search "rm: " i) 0))) (split-string str '(#\Newline))) :unsat)
	(t
	 (let (l bl)
	   (setq l (split-string str '(#\Newline)))
	   (dolist (i l bl)   ; walk over result and find var assign or unsat
	     (cond ((= (length i) 0))  ; ignore blanks
		   ((eq (search "rm: " i) 0))  ; ignore rm warnings
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