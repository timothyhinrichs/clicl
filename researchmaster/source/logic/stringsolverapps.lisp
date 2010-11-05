;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; stringsolverapps.lisp
;;      Applications of string solver.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defparameter *app-location* "./ss"
  "location for application--only used when saving application")
(defparameter *app-init-location* 
  "./ss-init.lisp"
  "location for application initialization--used for application")

; save-application call
(defun savess ()
  ;(ignore-errors (load *app-init-location*))
  (save-application *app-location* :prepend-kernel t))
; :toplevel-function #'ssx


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;  Whitebox
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; Add set-env from notamper.pl to script calling the function: ss-whitebox

(defvar *ss-working-prefix* "/tmp/notamper/")
(defvar *ss-home* "" "read from environmental variable NOTAMPER_BASE")
(defvar *ss-component-paths* '((:narcissus . "narcissus_js/js/narcissus")
			       (:jsdist . "narcissus_js/dist/bin")
			       (:htmlext . "formula-extractor/formula-generator")
			       (:respgen . "response-generator")))
(defvar *ss-history-unique* nil
  "global storage of all the assignments made for the unique variables")
(defvar *ss-global-unique* nil
  "global storage for the set of all unique vars for an entire server")

; NEED TO ADD CALL TO SS-CLEANSE FOR TYPE INFERENCE (at least).  
;   PERHAPS CONSTRUCT SS-PROB FOR EACH FCLIENT AND THEN CALL.
(defun ss-whitebox (url &key (unique 'unknown) (required 'unknown) (stream t) (outputfun #'ss-output-whitebox))
  "(SS-WHITEBOX URL) is the toplevel function for invoking the whitebox program analysis routine
   using the stringsolver.  It takes a URL as an argument."
  (let (start hostiles *ss-solve-count*)
    (load *app-init-location*)
    (ss-assert (stringp url) nil (format nil "SS-WHITEBOX expects a string; found ~A" url))
    (ss-whitebox-init url)
    (dolist (fclient (ss-compute-fclients url))
      (setq *ss-solve-count* 0)
      (setq start (get-universal-time))
      (setq hostiles (ss-whitebox-process-fclient fclient :unique unique :required required))
      (funcall outputfun hostiles fclient :time (- (get-universal-time) start) :stream stream :count *ss-solve-count*))
    ;(ss-whitebox-cleanup)
    nil))

(defun ss-output-whitebox (hostiles prob &key (time 0) (stream t) (count 0))
  "(SS-OUTPUT-WHITEBOX HOSTILES PROB TIME STREAM COUNT) outputs the results to STREAM.
   HOSTILES is a list of (binding list . explanation)."
  (format stream "~&~A Hostiles found for ~A in ~A seconds using string-solver ~A times~%"
	  (length hostiles) (second (find 'url (ss-prob-metafields prob) :key #'car)) time count)
  (dolist (h hostiles)
    (format stream "~&~A with explanation ~A~%" (car h) (cdr h))))

(defun ss-whitebox-process-fclient (prob &key (unique 'unknown) (required 'unknown))
  (let (hostiles req uniq (*ss-history-unique* nil) (*ss-global-unique* nil) insts) 
    ; compute union of all individual uniques so we track all the right vars.
    ;  also make list of problem instances to solve.
    (dolist (phi (or2list (ss-prob-phi prob)))
      (multiple-value-setq (req uniq) (ss-get-required-unique phi prob required unique))
      (push (list phi prob req uniq) insts)
      (setq *ss-global-unique* (append uniq *ss-global-unique*)))  ; append to copy uniq
    (setq *ss-global-unique* (uniquify *ss-global-unique*))
      
    ; try to find 1 hostile per disjunct in DNF(fclient)
    (dolist (i insts)
      (multiple-value-bind (hostile explanation) 
	  (ss-whitebox-find-benignhostile (first i) (second i) :required (third i) :unique (fourth i))
	(when hostile (push (ss-uncleanse (cons hostile explanation) prob) hostiles))))
    hostiles))

(defun ss-whitebox-find-benignhostile (p prob &key (required nil) (unique nil))
  "(SS-WHITEBOX-FIND-BENIGNHOSTILE P PROB ...) explores application to find a benign input of (a refinement of)
   P and then uses it to find a hostile input.  Returns 2 values: hostile binding list and an explanation 
   for how it was generated."
  (when *ss-debug* 
    (ss-trace (with-output-to-string (s) 
		(format s "Searching for benign for ~A using " (ss-prob-name prob)) (pprint p s)) 1))
  (let (queue q newq history successp constraints benign)
    (setq history nil)   ;  a list of conjunctions of literals.
    (push 'true queue)
    (do () ((null queue) nil)
      (setq q (pop queue))
      (setq benign (ss-whitebox-solve (makand p q) prob :required required :unique unique :depth 2))
      (when benign
	(multiple-value-setq (successp constraints) (ss-whitebox-testserver prob benign))
	(setq constraints (ss-simplify-aggressive constraints))
	(ss-assert (not (eq constraints :unsat)) nil 
		   (format nil "SS-WHITEBOX-FIND-BENIGNHOSTILE: server constraint set unsatisfiable: ~A" constraints))
	(cond (successp  ; found benign
	       (multiple-value-bind (hostile expl) (ss-whitebox-construct-hostile p (maksand (cons q constraints)) prob)
		 (when hostile (return (values hostile expl)))))
	      (t ; need to keep trying benign: depth first
	       (dolist (s constraints)
		 (setq newq (makand q (maknot s)))
		 (unless (ss-whitebox-redundantp history newq)
		   (push newq queue)
		   (push newq history)))))))))
 
(defun ss-whitebox-construct-hostile (fclient constraint prob &key (required nil) (unique nil))
  "(SS-WHITEBOX-CONSTRUCT-HOSTILE FCLIENT CONSTRAINT PROB) assuming that there is a solution to 
   FCLIENT ^ CONSTRAINT that hits a success sink at the server, construct a solution to
   ~FCLIENT  ^ CONSTRAINT and test if it too hits a success sink."
  (when *ss-debug* (ss-trace "Searching for hostile" 1))
  (let (hostile success)
    ; ask each DNF(~fclient) separately so we know which variables have been tampered with
    (dolist (d (or2list (dnf (maknot fclient))) nil)
      (setq hostile (ss-whitebox-solve (makand d constraint) prob :required required :unique unique))
      (setq success (ss-whitebox-testserver prob hostile))
      (when success (return (values hostile (list fclient (vars d) constraint)))))))

(defun ss-whitebox-testserver (prob bl)
  "(SS-WHITEBOX-TESTSERVER PROB BL) throws BL at the server and returns two values:
   (i) whether or not the BL drove the server to a success sink and (ii) the set of constraints
   encountered along the way."
  (let (blfile notamperout)
    (setq blfile (stringappend *ss-working-prefix* "bl.xml"))
    ; write out binding list in XML
    (setq notamperout (ss-uncleanse (list (list 'good t bl)) prob))
    (with-open-file (f blfile :direction :output :if-does-not-exist :create :if-exists :supersede)
      (notamper2xml notamperout prob :stream f))
    ; record that we're giving these inputs to the server
    (setq *ss-history-unique* (nconc (remove-if-not #'(lambda (x) (member (car x) *ss-global-unique*)) bl)
				    *ss-history-unique*))
    ; invoke Nazari's code to send inputs to server
    (exec-commandline "cd" (ss-abspath :respgen "") 
		      "; rm -Rf responses results diffsizes.txt; mkdir responses; mkdir results;"
		      "java -jar HTTPReqGen.jar" blfile (stringappend *ss-working-prefix* "req_gen_out.htm"))
    ; read results into appropriate data structure
    (ss-whitebox-process-server (ss-abspath :respgen (stringappend "responses/" (ss-prob-name prob) "_BENIGN1.htm")))))

(defun ss-whitebox-process-server (file)
  "(SS-WHITEBOX-PROCESS-SERVER FILE) uses the server response stored in file to return two values:
   whether or not the server hit a success sink and the set of constraints encountered along the way."
  (read-any-file file))
  
(defun ss-whitebox-redundantp (history q)
  "(SS-WHITEBOX-REDUNDANT HISTORY Q) returns T if Q is logically equivalent to some element in HISTORY."
  (some #'(lambda (x) (seteq x q #'equal)) history))

(defun ss-whitebox-solve (p prob &key (required nil) (unique nil) (depth 0))
  "(SS-WHITEBOX-FIND-HOSTILE-SOLVE P ...) runs stringsolver on P and returns result"
  (let (val u)
    ; create unique constraints
    (setq u (remove-if-not #'(lambda (x) (member (car x) unique)) *ss-history-unique*))
    (setq u (mapcar #'(lambda (x) `(!= ,(car x) ,(cdr x))) u))

    ; create sentence
    (setq p (maksand (list* p (ss-prob-space prob) u)))

    (when *ss-debug* 
      (ss-trace (with-output-to-string (s) (format s "String solving:") (pprint p s))
		depth))
    (setq val (ss-solve p :required required :types (ss-prob-types prob)))
    (when val
      (setq val (hash2bl val))
      (when *ss-debug* (ss-trace (format nil "Solution: ~S" val) depth))
      (unless *quiet*
	(ss-assert (ss-checksat p (ss-prob-types prob) (append val truth)) nil 
		   (format nil "ss-solve failed on ~%~S~%producing ~%~S~%with types~%~S~%" 
			   p val (hash2bl (ss-prob-types prob))))))
    val))
    

(defun ss-whitebox-init (url)
  (when *ss-debug* (ss-trace (format nil "Running whitebox analysis on ~A" url) 0))
  ; make sure temp directory exists
  (exec-commandline "mkdir" *ss-working-prefix*)
  ; grab root
  (setq *ss-home* (string-trim '(#\Space #\Newline #\Return) (exec-commandline "echo $NOTAMPER_BASE")))
  ; clean up paths
  (setq *ss-home* (ss-path-endify *ss-home*))
  (setq *ss-working-prefix* (ss-path-endify *ss-working-prefix*))
  (dolist (v *ss-component-paths*)
    (setf (cdr v) (ss-path-endify (string-left-trim '(#\/) (cdr v))))))

(defun ss-whitebox-cleanup ()
  (exec-commandline "cd" *ss-working-prefix* "; rm -f homepage.htm *.constraints* *.scripts *.log"))
; (stringappend *ss-home* "cleanup.sh")))

(defun ss-compute-fclients (url)
  "(SS-COMPUTE-FCLIENTS URL) takes a URL and returns a list of ss-prob structs:
   one for each form at the URL.  Exactly same as blackbox version."
  (when *ss-debug* (ss-trace "Computing fclient for each form" 1))
  (ss-extract-ht-constraints (ss-get-html url) url)
  (ss-extract-js-constraints)
  (mapcar #'(lambda (x) (eval (first (read-file x)))) (ss-combine-constraints)))

(defun ss-get-html (url &optional (dest (stringappend *ss-working-prefix* "homepage.htm")))
  (when *ss-debug* (ss-trace (format nil "Downloading ~A" url) 2))
  (exec-commandline "wget" "-O" dest url)
  dest)
    
(defun ss-extract-ht-constraints (htmlfile url)
  (when *ss-debug* (ss-trace "Extracting HTML Constraints" 2))
  (exec-commandline "cd" *ss-working-prefix* ";" "java" "HTMLConstraints" "-offline" htmlfile url))

(defun ss-extract-js-constraints ()
  "(SS-EXTRACT-JS-CONSTRAINTS) for each XXXX.script file in working directory, 
   run JS extractor to produce XXXX.constraints_JS file in working directory
   that contains the constraints imposed by the JS file."
  (when *ss-debug* (ss-trace "Extracting JavaScript Constraints" 2))
  (let (scripts logfile jscfile)
    (setq scripts (mapcar #'namestring (directory (stringappend *ss-working-prefix* "*.scripts"))))
    (dolist (s scripts)
      (unless (search "DO_NOT_PROCESS_NOTAMPER" (read-any-file s))
	(setq logfile (stringappend s ".log"))
	(with-open-file (f logfile :direction :output :if-does-not-exist :create :if-exists :supersede)
	  (run-program (ss-abspath :jsdist "") (list "js.js") :output f))
	(setq jscfile (stringappend (drop-file-extension s) ".constraints_JS"))
	(with-open-file (f jscfile :direction :output :if-does-not-exist :create :if-exists :supersede)
	  (princ (exec-commandline (stringappend "grep \"JavaScript evaluator generated these constraints NTBEGIN\"" logfile " | sed -e \"s/JavaScript evaluator generated these constraints NTBEGIN//g\"")) f))))))

(defun ss-trace (msg &optional (depth 0))
  (format t "~&")
  (dotimes (i depth)
    (format t " | "))
  (princ msg t)
  (format t "~%"))

(defun ss-combine-constraints ()
  "(SS-COMBINE-CONSTRAINTS) takes constraints from X.constraints_H X.constraints_H_1 X.constraints_JS and
   combines them into a single X.constraints file, which is in the ss-prob format."
  (when *ss-debug* (ss-trace "Combining Constraints" 2))
  (let ((hcfiles (directory (stringappend *ss-working-prefix* "*.constraints_H")))
	(constraints nil) hcfile2 jscfile outfile)
    (setq hcfiles (mapcar #'namestring hcfiles))
    (dolist (h hcfiles constraints)
      (setq hcfile2 (stringappend h "_1"))
      (setq jscfile (stringappend (drop-file-extension h) ".constraints_JS"))
      (setq outfile (stringappend (drop-file-extension h) ".constraints"))
      (exec-commandline "touch" h jscfile hcfile2)
      (exec-commandline "cat" h jscfile hcfile2 ">" outfile)
      (push outfile constraints))))
       
(defun ss-abspath (component path)
  (let ((res (find component *ss-component-paths* :key #'car)))
    (ss-assert res nil (format nil "Couldn't find component ~A when computing absolute path for ~A" component path))
    (stringappend *ss-home* (cdr res) path)))

(defun ss-path-endify (string)
  "(SS-PATH-ENDIFY STRING) appends / to string if it does not exist"
  (if (char= (aref string (1- (length string))) #\/) string (stringappend string "/")))

(defun drop-file-extension (string)
  "(DROP-FILE-EXTENSION STRING) drops the trailing .<chars> from string"
  (let (i)
    (setq i (search "." string :from-end t))
    (if i 
	(subseq string 0 i) 
	string)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;  Blackbox
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar *notamp-raw-output* "/tmp/ss/raw.lisp"
  "location to store raw notamp results in lisp-friendly format.  Not currently used.")

(defun ssx (file &key (numgood 1) (unique 'unknown) (required 'unknown) (stream t))
  (load *app-init-location*)
  (format stream "<testcases>~%")
  (dolist (p (read-file file))
    (ssn (eval p)
	 :numgood numgood :unique unique :required required :stream stream))
  (format stream "</testcases>~%")
  (quit))

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

;  (cond ((not (ss-test-all-regexps (ss-prob-phi prob)))
;	 (format t "Regexp failure in formula~%"))
;	((not (ss-test-all-regexps (ss-prob-types prob)))
;	 (format t "Regexp failure in type~%"))
;	(t
	 (let ((start (get-universal-time)) val (*ss-solve-count* 0))
	   (format t "~&")
	   
	   (setq val
		 (notamp (ss-prob-phi prob) :numgood numgood :unique unique :required required 
			 :types (ss-prob-types prob) :space (ss-prob-space prob)))
	   (when *ss-debug* (format t "~&Solution:~%") (pprint val))

	   (funcall outputfun (ss-uncleanse val prob) prob 
			 :time (- (get-universal-time) start) :stream stream :count *ss-solve-count*)))

(defun notamp (p &key (numgood 1) (unique nil) (required nil) (types nil) (space 'true)) 
  (ss-hampi-init)
  (multiple-value-prog1 (notamper p :numgood numgood :unique unique :required required :types types :space space)
    (ss-hampi-kill)))

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
    (ss-assert (and (integerp numgood) (>= numgood 0)) nil 
	    "NOTAMPER requires NUMGOOD to be a non-negative integer.")

    ; compute required and unique if set to T
    (when (and (atom required) required) (setq required (ss-guess-required p types)))
    (when (and (atom unique) unique) (setq unique (ss-guess-unique p required)))

    ; compute NUMGOOD unique good answers, distributed across DNF pieces of P
    (unless *quiet* (format t "~&~%~%**** Generating Good Answers ****~%~%"))
    (setq firstgood nil)
    (setq orlist (or2list (dnf p)))
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
    (dolist (v (or2list (dnf (maknot p))))
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
      (unless *quiet*
	(unless (ss-checksat p types (append (hash2bl bl) '((t . t))))
	  (format t "In notamper-solve-good, ss-solve failed on ~%~S~%producing ~S" p (hash2bl bl))))
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
      (unless *quiet* 
	(unless (ss-checksat p2 types (append (hash2bl bl) truth)) 
	  (format t "In notamper-solve-bad, ss-solve failed initially on ~%~S~%producing ~S" p2 (hash2bl bl))))
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
		 (unless *quiet*
		   (unless (ss-checksat p2 types (append (hash2bl bl2) truth))
		     (format t "In notamper-solve-bad, ss-solve failed secondarily on ~%~S~%producing ~S" 
				 p2 (hash2bl bl))))
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
						(list 'nin (car x) (ss-set2reg (cdr x)))
						nil)) 
			     (hash2bl history))) 
	  p))

(defun ss-set2reg (s)
  (setq s (mapcar #'(lambda (x) (ss-regesc (tostring x))) s))
  (setq s (tostring (cons (first s) (mapcan #'(lambda (x) (list "|" x)) (cdr s)))))
  (ss-makreg s))

(defun blequal (l1 l2) (setequal l1 l2 :test #'equal))

(defun setequal (l1 l2 &key (test #'eq) (key #'identity))
  (and (null (set-difference l1 l2 :test test :key key))
       (null (set-difference l2 l1 :test test :key key))))

(defun ss-get-required-unique (p prob required unique)
  ; manual override
  (when (eq required 'unknown)
    (setq required (ss-prob-required prob)))
  ; all vars == our guess at required
  (unless (listp required)
    (setq required
	  (uniquify (nconc (vars p) 
			   (vars (ss-prob-types prob)) 
			   (vars (ss-prob-space prob))))))

  ; manual override
  (when (eq unique 'unknown)
    (setq unique (ss-prob-unique prob)))
  ; if unique = t, all vars
  ; if unique not a list, guess vars
  (cond ((eq unique 't) 
	 (setq unique required))  ; all variables are unique
	((not (listp unique))
	 (setq unique (ss-guess-unique (maksand (list p
						      (ss-prob-types prob)
						      (ss-prob-space prob)))
				       required))))
  (values required unique))

(defun ss-guess-required (p types)
  (when (hash-table-p types) (setq types (hash2list types)))
  (set-difference (union (vars p) (vars types))
		  (mapcarnot #'(lambda (x) (if (and (listp x) (eq (car x) 'require) (varp (second x)))
					       (second x)))
			     (find-atoms p))))

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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Input Cleansing
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun ss-cleanse (prob)
  "(SS-CLEANSE PROBLEM) takes possibly screwy outside problem definitions and cleanses
   it for use in included routines.  Destructively modifies PROB."
  ; ordering of cleansing operations below is important

  ; remove (var "myCaseSensitiveVar") and augment ss-prob-varnames as appropriate
  (setq *ss-varmapping* nil)
  (ss-cleanse-varcases prob)

  ; tweak regular expressions
  (setf (ss-prob-phi prob) (ss-fix-innotin (ss-prob-phi prob)))
  (setf (ss-prob-space prob) (ss-fix-innotin (ss-prob-space prob)))
  (setf (ss-prob-types prob) (mapcar #'ss-in2type (drop-op (ss-prob-types prob))))

  ; do type inference and cast objs in constraints to satisfy types.
  (ss-cleanse-typeinference prob)

  ; remove syntactic sugar
  (setf (ss-prob-phi prob) (ss-simplify (ss-drop-syntactic-sugar (ss-prob-phi prob))))
  (setf (ss-prob-space prob) (ss-simplify (ss-drop-syntactic-sugar (ss-prob-space prob))))
  (setf (ss-prob-types prob) (ss-simplify (ss-drop-syntactic-sugar (ss-prob-types prob))))

  ; variable name fixes (for Hampi, not us)
  (setf (ss-prob-phi prob) (maptree #'make-nice-variable (ss-prob-phi prob)))
  (setf (ss-prob-space prob) (maptree #'make-nice-variable (ss-prob-space prob)))
  (setf (ss-prob-types prob) (maptree #'make-nice-variable (ss-prob-types prob)))

  (setf (ss-prob-varnames prob)
	(mapcar #'(lambda (x) (cons (car x) (list 'var (cdr x))))
		(butlast (compose-mgus (list (append (ss-prob-varnames prob) truth)
					     (append *ss-varmapping* truth))))))

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
	
(defun ss-cleanse-typeinference (prob)
  (let (types p)
    (setq p (list* (ss-prob-phi prob) 
		   (ss-prob-space prob) 
		   (if (atom (car (ss-prob-types prob)))  (cdr (ss-prob-types prob)) (ss-prob-types prob))))
    ; need to extract all of p's atoms and then flatten them.
    (setq p (find-atoms (maksand p)))
    (setq p (flatten-operator (maksand (mapcar #'flatten-functions p))))
    ; grab types
    (setq types (ss-type-inference (and2list p)))
    ; resolve conflicts
    (setq types (mapcar #'(lambda (x) (cons (car x) (ss-resolve-types (cdr x)))) types))
    ; fix constraints
    (setf (ss-prob-phi prob) (ss-force-types (ss-prob-phi prob) types))
    ; add non-string types to prob-types
    (dolist (y types)
      (when (member (cdr y) '(num bool)) 
	(push (list 'typedecl (car y) (cdr y)) (ss-prob-types prob))))
    ; return result
    prob))
    

(defun ss-force-types (p types)
  (flet ((argtype (a) (cond ((varp a) (cdr (assoc a types)))
			    ((stringp a) 'str)
			    ((member a '(true false)) 'bool)
			    ((numberp a) 'num)
			    ((listp a) (viewfindx '?x `(argtype ,a ?x) 'sstypes))  ; all funcs return str
			    (t 'str))))
    (cond ((atom p) p)
	  ((member (car p) '(and or not => <= <=> forall exists)) 
	   (cons (car p) (mapcar #'(lambda (x) (ss-force-types x types)) (cdr p))))
	  (t (let (type rtype)
	       (setq type (ss-resolve-types (mapcar #'argtype (cdr p))))
	       (setq rtype (viewfindx '?x `(reltype ,(car p) ?x) 'sstypes))
	       (ss-assert (or (not rtype) (eq rtype type)) nil
			  (format nil "Type mismatch: one of args in ~A is of type ~A but must be of type ~A"
				  p type rtype))		 
	       (cond ((eq type 'str) (cons (car p) (mapcar #'ss-cast-tostring (cdr p))))
		     ((eq type 'num) (cons (car p) (mapcar #'ss-cast-tonum (cdr p))))
		     ((eq type 'bool) (cons (car p) (mapcar #'ss-cast-tobool (cdr p))))
		     (t p)))))))

(defun ss-resolve-types (types)
  (cond ((atom types) types)
	((member 'str types) 'str)
	((member 'num types) 'num)
	((member 'bool types) 'bool)
	((listp types) (first types))))
    
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

(defun ss-in2type (p)
  (cond ((atom p) p)
	((find (car p) '(and or not <= => <=> forall exists))
	 (cons (car p) (mapcar #'ss-in2type (cdr p))))
	((eq (car p) 'in) (list 'type (second p) (ss-makreg (third p))))
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


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Output results (XML)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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
      (notamper2xml-varassign (ss-vars (first bl)) (second bl) 6 stream)
      (printspaces 4 stream)
      (format stream "</case>~%")))
  (printspaces 2 stream) (format stream "</bad>~%")
  (format stream "</form>~%"))

(defun ss-vars (p)
  (cond ((varp p) (list p))
	((atom p) nil)
	((member (car p) '(and or not => <= <=> forall exists)) (mapcan #'ss-vars (cdr p)))
	((eq (car p) 'var) (list (second p)))
	(t (mapcan #'ss-vars (cdr p)))))

(defun notamper2xml-varassign (tamperedvars bl n s)
  (let (v)
    (dolist (x bl)
      (setq v (if (atom (first x)) (first x) (second (first x))))
      (printspaces n s) 
      (format s "<bl><var tamper=\"~A\">~A</var><val>~A</val></bl>~%" 
	      (if (member v tamperedvars) "true" "false")
		  (urlify (notamper2web-varspelling (first x)))
		  (urlify (second x))))))
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
  (ss-assert (or (literalp p)
	      (and (listp p) (eq (car p) 'and) (every #'literalp (cdr p))))
	  nil "SS-BUILD-VARIATIONS requires a conjunction of literals as input")
  (setq p (and2list p))
  (do ((s p (cdr s))
       (res nil) (sofar nil))
      ((null s) res)
    (push (maksand (revappend sofar (cons (maknot (car s)) (cdr s)))) res)
    (push (car s) sofar)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
