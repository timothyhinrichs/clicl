;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; stringsolverapps.lisp
;;      Applications of string solver.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun r () 
  (let (home)
    (setq home (string-trim '(#\Space #\Newline #\Return) (exec-commandline "echo $NOTAMPER_BASE")))
    (setq home (ss-path-endify home))
    (load (stringappend home "solver/ss/stringsolver.lisp"))
    (load (stringappend home "solver/ss/stringsolverapps.lisp"))
    (load (stringappend home "solver/ss/stringsolvertests.lisp"))))


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
;;  Whitebox NoTamper testing
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; user customizations
(defparameter *ss-node-limit* nil
  "NIL or int defining the maximum number of nodes to search")
(defparameter *ss-solve-limit* nil
  "NIL or int defining the maximum number of string solver invocations")
(defparameter *ss-benign-limit* nil
  "NIL or int defining the maximum number of benign inputs to find")
(defparameter *ss-reset-limits-per-disjunct* nil
  "boolean controlling whether or not the above limits are per disjunct or are global")

(defparameter *ss-aggressive-pruning* nil
  "whether or not to prune search space aggressively")
(defparameter *ss-interactive-trace-success* nil 
  "whether or not to ask the user for the success status of a trace")
(defparameter *ss-interactive-form-control* nil
  "whether or not to ask the user about proceeding for each new form")
(defparameter *ss-pause-after-download* nil
  "whether or not to pause after downloading and processing form")
(defparameter *ss-success-sink* 1
  "An integer that indicates the definition for a success sink")
(defparameter *ss-show-search* nil
  "similar to *ss-debug* but independently controlled; shows the search space being explored.")
(defparameter *ss-stop-now* nil
  "set this to stop entire search now and exit gracefully")
(defparameter *ss-whitebox-fclient-processor* 1
  "an integer value controlling which fclient processor to use")
(defparameter *ss-only-sink-relevant-hostiles* t
  "require that the only hostiles generated are based on disjuncts of ~fclient containing a variable
  on which the sink is dependent")
(defparameter *ss-java-home* nil "read from environmental variable JAVA_HOME")
(defparameter *ss-java-invoke* nil "$JAVA_HOME/bin/java")
(defparameter *ss-whitebox-logname* "notamper.log" "user-provided file path starting from *ss-home*")
(defparameter *ss-working-prefix* "/tmp/notamper/")
(defparameter *ss-component-paths* '((:narcissus . "narcissus_js/js/narcissus")
			       (:jsdist . "narcissus_js/dist/bin")
			       (:htmlext . "formula-extractor/formula-generator")
			       (:respgen . "response-generator")
			       (:serverformula . "server-formula-wpk")
			       (:traceanalysis . "trace-dep-analysis")))


; internal global vars
(defparameter *ss-node-count* 0 "global counter for nodes searched")
(defparameter *ss-benign-count* 0 "global counter for benign inputs discovered")
(defparameter *ss-whitebox-log* "" "internal, full path to log")
(defparameter *ss-home* "" "read from environmental variable NOTAMPER_BASE")
(defvar *ss-history-unique* nil
  "global storage of all the assignments made for the unique variables")
(defvar *ss-global-unique* nil
  "global storage for the set of all unique vars for an entire server")
(defvar *ss-white-downloads* 0)

(defstruct ss-prob 
  ; user-supplied 
  (name nil) phi (space 'true) (types nil)  (unique t) (required t) (init nil)
  (status :contingent) (metafields nil)
  ; internal
  (varnames nil) (variantstatus nil))

#|
(define-theory 'data "" (read-file "/Users/thinrich/Downloads/notamper.log.prithvi_new.mybloggie.LATEST"))
(setq a (indexps 'hostile 'data) b 0)
(remove-duplicates a :test #'ss-hostiles-equal)
|#
(defstruct ss-sink id phi vars status constraints)
(defun ss-sink-success (x) (eq (ss-sink-status x) 'success))
(defun ss-sink-failure (x) (eq (ss-sink-status x) 'failure))
(defun ss-sink-unknown (x) (eq (ss-sink-status x) 'unknown))

(defun white (&optional name (hint 'true) &rest forms) (apply #'whitet name hint forms))
(defun whitet (&optional name (hint 'true) &rest forms)
  (dolist (v (white-tests))
    (if name
	(when (eq (first v) name) (ss-whitebox (second v) :space hint :unique nil :required nil :forms forms))
	(ss-whitebox (second v) :unique nil :required nil))))

(defun ss-sentence-check (p)
  "(SS-SENTENCE-CHECK P) throws an error if can't translate P to internal format."
  (ss-whitebox-init nil)
  (ss-validate-vocab-internal (and2list (ss-cleanse-more p (make-ss-prob)))))

(defun ss-whitebox (url &key (space 'true) (unique 'unknown) (required 'unknown) (stream t) (forms nil) 
		    (outputfun #'ss-output-whitebox))
  "(SS-WHITEBOX URL) is the toplevel function for invoking the whitebox program analysis routine
   using the stringsolver.  It takes a URL as an argument."
  (let (start hostiles *ss-solve-count* (*ss-white-downloads* 0) fclientprobs url2 
	      (*ss-false-values* #'php-false) (*ss-solve-count* 0))
    (load *app-init-location*)
    (ss-assert (stringp url) nil (format nil "SS-WHITEBOX expects a string; found ~A" url))
    (ss-whitebox-init url)
    ; compute fclient for each form at URL, removing those forms pointing to different URLs
    ;   by making sure fclient URL is included in the original URL (since orig may include extra params)
    (ss-whitebox-cleanup)  ; so we don't get confused about whether or not stuff was downloaded
    (exec-commandline "mv" *ss-whitebox-log* (stringappend *ss-whitebox-log* ".bak"))
    (setq fclientprobs nil)
    (when *ss-show-search* (format t "~&Pre-processing ~A~%" url))
    (dolist (p (ss-compute-fclients url :space space))
      (setq url2 (second (assoc 'url (ss-prob-metafields p))))
      (if (equalp (ss-base-url url2) (ss-base-url url))
	  (push p fclientprobs)
	  (when *ss-debug* (ss-trace (format nil "** Ignoring form with URL ~A **" url2) 0))))
    (when *ss-pause-after-download* (format t "~&Enter anything to proceed:") (read))

    ; analyze local fclients
    (do ((fclients (nreverse fclientprobs) (cdr fclients)) (fclient))
	((null fclients))
      (setq fclient (car fclients))
      (when *ss-stop-now* (return))
      (when (or (null forms) (member (ss-prob-name fclient) forms))
	(setq *ss-solve-count* 0)
	(setq start (get-universal-time))
	(when *ss-debug* 
	  (ss-trace (format nil "** Vulnerability detection for form ~A commencing **" (ss-prob-name fclient)) 0)
	  (unless *quiet* (pprint (ss-prob-phi fclient))))	
	(setq hostiles (ss-whitebox-process-fclient fclient :unique unique :required required :depth 1))
	(funcall outputfun hostiles fclient :time (- (get-universal-time) start) :stream stream :count *ss-solve-count*)
	(when (and (cdr fclients) *ss-interactive-form-control*)
	  (format t "~&Proceed?  (No = nil) ") (let ((v (read))) (unless v (return))))))
    ;(ss-whitebox-cleanup)
    nil))

(defun ss-base-url (url) 
  (let (s)
    (setq s (position #\: url))
    (if (search "///" url :start2 s)
	(setq s (+ 4 s))
	(setq s (+ 3 s)))
    (subseq url 0 (position #\/ url :start s))))

(defun ss-output-whitebox (hostiles prob &key (time 0) (stream t) (count 0))
  "(SS-OUTPUT-WHITEBOX HOSTILES PROB TIME STREAM COUNT) outputs the results to STREAM.
   HOSTILES is a list of (binding list . explanation)."
  (declare (ignore count))
  (format stream "~&***** Processing for ~A complete in ~A seconds.~%"
	  (ss-prob-name prob) time)
  (format stream "~&***** ")
  (if hostiles
      (format stream "ALERT: hostiles recorded in ~A ." *ss-whitebox-log*)
      (format stream "No hostiles found. Logfile: ~A" *ss-whitebox-log*)))

(defun ss-log (&rest things) (mapc #'(lambda (f) (append-file *ss-whitebox-log* f)) things))

(defun ss-whitebox-process-fclient (prob &key (required nil) (unique nil) (depth 0))
  (case *ss-whitebox-fclient-processor*
    (1 (ss-whitebox-process-fclient1 prob :required required :unique unique :depth depth))
    (2 (ss-whitebox-process-fclient2 prob :required required :unique unique :depth depth))
    (3 (ss-whitebox-process-fclient3 prob :required required :unique unique :depth depth))
    (otherwise (ss-whitebox-process-fclient1 prob :required required :unique unique :depth depth))))


; in this version, we first use fclient to find a benign input and the server-side constraints C leading to a
;  success and then look for a solution to C and each of the disjuncts of DNF(~fclient).
(defun ss-whitebox-process-fclient1 (prob &key (required nil) (unique nil) (depth 0))
  (ss-whitebox-process-formula (ss-prob-phi prob) prob #'ss-whitebox-server-benign-hostile
			       :required required :unique unique :depth depth))

; in this version, we just try to find server-side constraints C that when conjoined with one of the 
;   disjuncts of DNF(~fclient), we hit a success node.
(defun ss-whitebox-process-fclient2 (prob &key (required nil) (unique nil) (depth 0))
  (ss-whitebox-process-formula (maknot (ss-prob-phi prob)) prob #'ss-whitebox-server-benign
			       :required required :unique unique :depth depth))

; in this version, we first use fclient to find a benign input and the server-side constraints C0 leading to a
;  success.  Then we perturb each of C0 individually, looking for new success nodes with constraints Ci.  
;  For each of the Ci, we look for a solution to Ci conjoined with each of the disjuncts of DNF(~fclient).
;  Combination of 1 and 2.
(defun ss-whitebox-process-fclient3 (prob &key (required nil) (unique nil) (depth 0))
  (ss-whitebox-process-formula (ss-prob-phi prob) prob #'ss-whitebox-server-benign-perturb-hostile
			       :required required :unique unique :depth depth))

(defun ss-whitebox-process-formula (formula prob searchprocessor &key (required nil) (unique nil) (depth 0))
  "(SS-WHITEBOX-PROCESS-FORMULA FORMULA PROB SEARCHPROCESSOR) Probe the server according to
   SEARCHPROCESSOR one disjunct of DNF(FORMULA) at a time."
  (setq *ss-tmp* prob)
  (let (responses lastnodecount lastbenigncount lastsolvecount hostiles 
		  (*ss-history-unique* nil) (*ss-global-unique* nil)
		  (*ss-node-count* 0) (*ss-solve-count* 0) (*ss-benign-count* 0))
    (ss-log `(start-formula ,formula))
    (ss-log `(prob-name ,(ss-prob-name prob)))
    (ss-log `(prob-phi ,(ss-prob-phi prob)))
    (ss-log `(prob-space ,(ss-prob-space prob)))
    (ss-log `(prob-types ,(hash2bl (ss-prob-types prob))))
    (ss-log `(prob-unique ,(ss-prob-unique prob)))
    (ss-log `(prob-required ,(ss-prob-required prob)))
    (ss-log `(prob-metafields ,(ss-prob-metafields prob)))
    (ss-log `(prob-varnames ,(ss-prob-varnames prob)))
    (dolist (p (or2list (dnf formula)))
      (when *ss-reset-limits-per-disjunct*
	(setq *ss-node-count* 0)
	(setq *ss-solve-count* 0)
	(setq *ss-benign-count* 0))      
      ;(handler-case (progn
      (setq lastnodecount *ss-node-count*)
      (setq lastbenigncount *ss-benign-count*)
      (setq lastsolvecount *ss-solve-count*)
      (when *ss-show-search* (format t "~&Starting new disjunct for form ~A~%" (ss-prob-name prob)))
      (when *ss-debug* 
	(ss-trace (format nil "** Starting new disjunct for form ~A **" (ss-prob-name prob)) depth)
	(pprint p))
      (when *ss-stop-now* (return))
      (ss-log `(newdisjunct ,p))
      (ss-log `(disjunct-starttime ,(get-universal-time)))
      (setq responses 
	(ss-whitebox-search-server p prob searchprocessor :required required :unique unique :depth (1+ depth)))
      (when responses (push responses hostiles))
      (ss-log `(disjunct-responses ,responses))
      (ss-log `(disjunct-nodecount ,(- *ss-node-count* lastnodecount)))
      (ss-log `(disjunct-stringsolvercount ,(- *ss-solve-count* lastsolvecount)))
      (ss-log `(disjunct-benigncount ,(- *ss-benign-count* lastbenigncount)))
      (ss-log `(disjunct-endtime ,(get-universal-time))))
      ;(condition () (when (or *ss-debug* *ss-show-search*) (format t "Found error...continuing")))))
    (ss-log `(prob-endtime ,(get-universal-time)))
    (ss-log `(end-formula ,formula))
    hostiles))

(defun ss-whitebox-search-server (p prob processor &key (required nil) (unique nil) (depth 0))
  "(SS-WHITEBOX-SEARCH-SERVER P PROB PROCESSOR) by repeatedly generating and sending inputs to the server,
   augment P so that it achieves success.  PROCESS is a function that takes
   (i) a sentence phi, (ii) a prob, keyed (iii) unsats, (iv) required, (v) unique, (vi) depth and returns multiple values:
   (a) whether or not PHI is a success, (b) the server constraints associated with PHI, and 
   (c) whether or not to stop the search.
   Returns two values: (a) the first a list of the constraints that when added to P generate server successes,
   (b) the number of nodes explored."
  (let (returnvals queue history prettyhistory successes q constraints unsats (*ss-cast-types* *ss-cast-types*))
    (multiple-value-setq (required unique) (ss-get-required-unique p prob required unique))
    (when *ss-show-search* 
      (format t "~&Starting new search for vulnerability in form named ~A" (ss-prob-name prob)) (pprint p t) (format t "~%~%"))
    (setq history nil)   ;  a list of conjunctions of literals.
    (setq prettyhistory (list nil)) ; a list of external constraints (embedded in another list for side effects).
    (setq unsats (list nil))
    (setq queue (ss-make-queue))
    (ss-enqueue queue 'true 0)
    ; use a queue to control how we search the space of server-side constraints
    (do () ((ss-empty queue))
      ; for each point in the search space, try to find a success, as defined by PROCESS
      (when *ss-stop-now* (return))
      (when (or (and (numberp *ss-node-limit*) (>= *ss-node-count* *ss-node-limit*))
		(and (numberp *ss-solve-limit*) (>= *ss-solve-count* *ss-solve-limit*))
		(and (numberp *ss-benign-limit*) (>= *ss-benign-count* *ss-benign-limit*))) 
	(return))
      (setq q (ss-pop queue))
      (when *ss-show-search* (format t "~&~%~S~%" q)) ;   Node~%~S~%" q))
      (setq *ss-node-count* (1+ *ss-node-count*))
      (push q history)
      (multiple-value-setq (returnvals constraints) 
	(multiple-value-call processor (makand p q) prob :unsats unsats :required required :unique unique :depth (1+ depth)))
      (setq successes (append returnvals successes))
      (when constraints
	(ss-whitebox-enqueue-server-constraints constraints p q history queue unsats prettyhistory :depth (1+ depth))))
    (nreverse successes)))

(defun ss-whitebox-server-benign-perturb-hostile (p prob &key (unsats nil) (required nil) (unique nil) (depth 0))
  ; check if p leads to success.  if so, find all constraints w new vars, see if they lead to successes.
  ;  For all successes, find all hostiles.
  (let (sinks hostiles fclientvars allhostiles succcons failcons)
    (setq sinks (ss-whitebox-server-benign* p prob :unsats unsats :required required :unique unique :depth depth))
    (setq allhostiles nil) ; accummulator
    (dolist (s sinks)
      (cond ((ss-sink-success s)
	     (setq *ss-benign-count* (1+ *ss-benign-count*))
	     ; find hostiles for all success sinks
	     (setq hostiles (ss-whitebox-construct-hostile s (ss-prob-phi prob) prob
							   :tamperable t :depth (1+ depth)))
	     (setq allhostiles hostiles)
	     ; store success constraints so we can perturb them once we have them all (do some redundancy elim)
	     (setq succcons (union (ss-sink-constraints s) succcons :test #'ss-sentequal)))
	    (t (setq failcons (append (ss-sink-constraints s) failcons)))))
    ; now perturb success constraints to find additional hostiles
    (setq fclientvars (vars (ss-prob-phi prob)))
    (setq succcons (remove-if #'(lambda (x) (subsetp (vars x) fclientvars)) succcons))
    (dolist (n succcons)
      (when *ss-show-search* (format t "~&Perturbing constraint~%~S~%~%" n))
      (when *ss-debug* (ss-trace (format nil "** Perturbing success sink constraint **~%~S" n) (1+ depth)))
      (setq hostiles (ss-whitebox-server-benign-hostile* (makand p (maknot n)) prob :unsats unsats 
							 :required required :unique unique :depth (1+ depth)))
      (setq allhostiles (append hostiles allhostiles)))
    ; return hostiles together with failure constraints
    (values allhostiles failcons)))

(defun ss-whitebox-server-benign-perturb-hostile_old (p prob &key (unsats nil) (required nil) (unique nil) (depth 0))
  ; check if p leads to success.  if so, find all constraints w new vars, see if they lead to successes.
  ;  For all successes, find all hostiles.
  (let (successp payload hostiles fclientvars allhostiles newconstraints)
    (multiple-value-setq (successp payload) 
      (ss-whitebox-server-benign* p prob :unsats unsats :required required :unique unique :depth depth))
    (cond (successp
	   (setq *ss-benign-count* (+ *ss-benign-count* (length payload)))
	   ; find hostiles for individual sinks
	   (dolist (s payload)
	     (setq hostiles (ss-whitebox-construct-hostile s (ss-prob-phi prob) prob
							   :tamperable t :depth (1+ depth)))
	     (setq allhostiles hostiles)
	     (when hostiles (ss-log `(vulnerability ,p ,(ss-sink-constraints s)))))
	   ; perturb each of the constraints in the union of the sinks
	   (setq newconstraints (apply #'union* (mapcar #'ss-sink-constraints payload)))
	   (setq fclientvars (vars (ss-prob-phi prob)))
	   (setq newconstraints (remove-if #'(lambda (x) (subsetp (vars x) fclientvars)) newconstraints))
	   (dolist (n newconstraints)
	     (when *ss-show-search* (format t "~&Perturbing constraint~%~S~%~%" n))
	     (when *ss-debug* (ss-trace (format nil "** Perturbing success sink constraint **~%~S" n) (1+ depth)))
	     (multiple-value-setq (successp hostiles) 
	       (ss-whitebox-server-benign-hostile* (makand p (maknot n)) prob 
						   :unsats unsats :required required :unique unique :depth (1+ depth)))
	     (setq allhostiles (nconc allhostiles hostiles)))
	   (values (not (null allhostiles)) allhostiles))
	  (t (values nil payload)))))

; version where we don't increase *ss-benign-count*
(defun ss-whitebox-server-benign-hostile* (p prob &key (unsats nil) (required nil) (unique nil) (depth 0))
  (let ((*ss-benign-count* *ss-benign-count*))
    (ss-whitebox-server-benign-hostile p prob :unsats unsats :required required :unique unique :depth depth)))

(defun ss-whitebox-server-benign-hostile (p prob &key (unsats nil) (required nil) (unique nil) (depth 0))
  "(SS-WHITEBOX-SERVER-BENIGN-HOSTILE P PROB) returns (i) hostiles generated by probing success sinks
   generated by P and (ii) if P leads to a failure, the constraints along that path."
  ; for all success nodes p generates, find hostiles.  p is benign.  if so, find all hostiles
  (let (sinks allhostiles hostiles constraints)
    (setq sinks (ss-whitebox-server-benign* p prob :unsats unsats :required required :unique unique :depth depth))
    (setq allhostiles nil)
    (dolist (s sinks)
      (cond ((ss-sink-success s)
	     (setq *ss-benign-count* (1+ *ss-benign-count*))
	     (setq hostiles (ss-whitebox-construct-hostile s (ss-prob-phi prob) prob  ; CHECK THAT HANDING S IS RIGHT
							   :tamperable t :depth (1+ depth)))
	     (when hostiles
	       (ss-log `(vulnerability ,p ,(ss-sink-constraints s)))
	       (push hostiles allhostiles)))
	    (t (setq constraints (append (ss-sink-constraints s) constraints)))))
    (values allhostiles constraints)))

(defun ss-whitebox-server-benign-hostile_old (p prob &key (unsats nil) (required nil) (unique nil) (depth 0))
  ; check if p is benign.  if so, find all hostiles
  (let (successp payload allhostiles hostiles)
    (multiple-value-setq (successp payload)  ;if successp, payload is list of sinks, else payload is list of constraints
      (ss-whitebox-server-benign* p prob :unsats unsats :required required :unique unique :depth depth))
    (cond (successp
	   (setq *ss-benign-count* (+ *ss-benign-count* (length payload)))
	   (dolist (s payload)
	     (setq hostiles (ss-whitebox-construct-hostile s (ss-prob-phi prob) prob 
							   :tamperable t :depth (1+ depth)))
	     (when hostiles
	       (ss-log `(vulnerability ,p ,(ss-sink-constraints s)))
	       (push hostiles allhostiles)))
	   (values (not (null allhostiles)) allhostiles))
	  (t (values nil payload)))))

; version where we don't increase *ss-benign-count*
(defun ss-whitebox-server-benign* (p prob &key (unsats nil) (required nil) (unique nil) (depth 0))
  (let ((*ss-benign-count* *ss-benign-count*))
    (ss-whitebox-server-benign p prob :unsats unsats :required required :unique unique :depth depth)))

(defun ss-whitebox-server-benign (p prob &key (unsats nil) (required nil) (unique nil) (depth 0))
  "(SS-WHITEBOX-SERVER-BENIGN P PROB) checks if P represents a class of inputs
   that the server accepts.  May modify *ss-cast-types*.  
   Returns (i) a list of (successp payload) (ii) solution to p used to test.
   Destructively modifies UNSATS, a list containing a single element, which is a list of unsatisfiable
   sentences."
  ;(when *ss-debug* (ss-trace "Looking for benign." depth) (pprint p))
  (let (benign tracefile sinks)
    (when *ss-debug* (ss-trace (format nil "** Attempting benign **~%~S" p) depth))
    (setq benign (ss-whitebox-solve p prob :required required :unique unique :depth depth))
    (cond (benign
	   (when *ss-debug* (ss-trace (format nil "** Found solution **~%~S" benign) depth))
	   (when *ss-show-search* (format t "~S~%~%" benign))
	   (setq tracefile (ss-whitebox-testserver prob benign :depth depth))
	   (setq sinks (ss-whitebox-trace-analysis tracefile prob :depth depth))
	   (setq sinks (delete-if #'ss-sink-unknown sinks))
	   (when *ss-debug* 
	     (ss-trace (format nil "** Found ~A success sinks **~%~S~%~S" (length sinks) benign p) depth))
	   (when (some #'ss-sink-success sinks) (ss-log `(success-sinks ,benign ,tracefile ,p)))
	   (dolist (s sinks)
	     (when (ss-sink-success s)
	       (setq *ss-benign-count* (+ *ss-benign-count* (length sinks)))
	       (ss-log `(success-sink ,(ss-sink-id s) ,(ss-sink-constraints s) ,(ss-sink-phi s) ,(ss-sink-vars s)))))
	   (values sinks benign))
	  (t
	   (when unsats
	     (setf (first unsats) (cons p (first unsats))))
	   (values nil nil)))))

(defun ss-whitebox-server-benign_old (p prob &key (unsats nil) (required nil) (unique nil) (depth 0))
  "(SS-WHITEBOX-SERVER-BENIGN P PROB) checks if P represents a class of inputs
   that the server accepts.  May modify *ss-cast-types*.  
   Returns (i) successp (ii) constraints in trace (iii) solution to p used to test.
   Destructively modifies UNSATS, a list containing a single element, which is a list of unsatisfiable
   sentences."
  ;(when *ss-debug* (ss-trace "Looking for benign." depth) (pprint p))
  (let (benign tracefile successp sinks)
    (when *ss-debug* (ss-trace (format nil "** Attempting benign **~%~S" p) depth))
    (setq benign (ss-whitebox-solve p prob :required required :unique unique :depth depth))
    (cond (benign
	   (when *ss-debug* (ss-trace (format nil "** Found solution **~%~S" benign) depth))
	   (when *ss-show-search* (format t "~S~%~%" benign))
	   (setq tracefile (ss-whitebox-testserver prob benign :depth depth))
	   (setq sinks (ss-whitebox-trace-analysis tracefile prob :depth depth))
	   (setq sinks (delete-if #'ss-sink-unknown sinks))
	   (setq successp (some #'ss-sink-success sinks))
	   (cond (successp
		  (setq *ss-benign-count* (+ *ss-benign-count* (length sinks)))
		  (setq sinks (delete-if-not #'ss-sink-success sinks))
		  (when *ss-debug* 
		    (ss-trace (format nil "** Found ~A success sink(s) **~%~S~%~S" (length sinks) benign p) depth))
		  (when *ss-show-search* (format t "~&Found success sink:~%~S~%~%" benign))
		  (ss-log `(success-sinks ,benign ,tracefile ,p))
		  (dolist (s sinks)
		    (ss-log `(success-sink ,(ss-sink-id s) ,(ss-sink-phi s) ,(ss-sink-vars s) ,(ss-sink-constraints s))))
		  (values t sinks))
		 (t (when *ss-debug* (ss-trace "** Trace failure **" depth))
		    (values nil (apply #'union* (mapcar #'ss-sink-constraints sinks))))))
	  (t
	   (when unsats
	     (setf (first unsats) (cons p (first unsats))))
	   (values nil nil)))))

(defun ss-whitebox-construct-hostile (sink fclient prob &key (tamperable t) (required nil) (unique nil) (depth 0))
  "(SS-WHITEBOX-CONSTRUCT-HOSTILE SINK FCLIENT PROB) assuming that there is a solution to 
   FCLIENT ^ (ss-sink-constraints SINK) that hits a success sink at the server, construct a solution to
   ~FCLIENT  ^ (ss-sink-constraints SINK) and test if it too hits a success sink.  TAMPERABLE is a list of variables
   we are allowed to tamper with."
  (let (hostile succsink sinks hostiles tracefile constraints)
    (setq constraints (ss-subsumption-elim (ss-sink-constraints sink)))
    ; ask each DNF(~fclient) separately so we know which variables have been tampered with
    (setq hostiles nil)
    (dolist (d (or2list (dnf (maknot fclient))) hostiles)
      (when (or (not *ss-only-sink-relevant-hostiles*) (intersectionp (vars d) (ss-sink-vars sink)))  ; only pertinent hostiles
	(when *ss-debug* (ss-trace (format nil "** Attempting hostile **~%~S" d) depth))
	(when *ss-stop-now* (return hostiles))
	(when (or (eq tamperable t) (subsetp (vars d) tamperable))
	  ;(when *ss-debug* (ss-trace (format nil "Looking for hostile along success path.") (1+ depth)))
	  ;(when *ss-debug* (unless *quiet* (ss-trace (with-output-to-string (s) (pprint d s)) (1+ depth))))
	  (setq hostile (ss-whitebox-solve (makand d (maksand constraints)) prob 
					   :required required :unique unique :depth (1+ depth)))
	  (cond (hostile
		 (setq tracefile (ss-whitebox-testserver prob hostile :depth (1+ depth)))
		 (setq sinks (ss-whitebox-trace-analysis tracefile prob :depth (1+ depth)))
		 (setq succsink (ss-whitebox-hostile-success-sinkp sinks d sink))
		 (cond (succsink 
			(when *ss-show-search* (format t "~&Hostile attempt succeeded:~%~S~%~A~%~S~%~%" d tracefile hostile))
			(when *ss-debug* (ss-trace (format nil "** Hostile found **~%~S~%~A~%~S" hostile tracefile d) depth))
			(push hostile hostiles)
			(ss-log `(hostile ,hostile ,d ,constraints ,(ss-sink-id succsink) ,(ss-sink-constraints succsink) 
					  ,(ss-sink-phi succsink) ,(ss-sink-vars succsink))))
		       (t (when *ss-debug* 
			    (ss-trace (format nil "** Hostile attempt failed **~%server rejection~%~S~%~A~%~S" 
					      hostile tracefile d) depth)))))
		(t (when *ss-debug* 
		     (ss-trace (format nil "** Hostile attempt failed: no string solution **~%~A~%~S" tracefile d) depth)))))))))

(defun ss-hostiles-equal (h1 h2)
  "(SS-HOSTILES-EQUAL H1 H2) returns T iff hostile log entry h1 is the same as h2, for a reasonable definition of same."
  (and 
   ; ~fclient disjuncts equal
   (ss-sentequal (third h1) (third h2))
   ; sink ids equal
   (eq (fifth h1) (fifth h2))
   ; constraints leading to sink equal
   (setequal (sixth h1) (sixth h2) :test #'ss-sentequal)
   ; variables that the sink depends on equal
   (setequal (eighth h1) (eighth h2) :test #'equal)))


(defun ss-whitebox-hostile-success-sinkp (sinks fclientdisj benignsink)
  "(SS-WHITEBOX-HOSTILE-SUCCESS-SINKP SINKS FCLIENTDISJ BENIGNSINK) finds the sink in SINKS (if it exists)
   that witnesses a successful hostile attack for a solution to the negated fclient disjunct FCLIENTDISJ
   wrt the benign sink BENIGNSINK."
  ; for now, if there is a sink that depends on some of the variables in fclientdisj, we call it a success
  (declare (ignore benignsink))
  (setq fclientdisj (vars fclientdisj))
  (find-if #'(lambda (x) (and (ss-sink-success x) (intersectionp (ss-sink-vars x) fclientdisj))) sinks))

(defun ss-whitebox-construct-hostile_old (sink fclient prob &key (tamperable t) (required nil) (unique nil) (depth 0))
  "(SS-WHITEBOX-CONSTRUCT-HOSTILE SINK FCLIENT PROB) assuming that there is a solution to 
   FCLIENT ^ (ss-sink-constraints SINK) that hits a success sink at the server, construct a solution to
   ~FCLIENT  ^ (ss-sink-constraints SINK) and test if it too hits a success sink.  TAMPERABLE is a list of variables
   we are allowed to tamper with."
  (let (hostile success hostiles tracefile constraints)
    (setq constraints (ss-subsumption-elim (ss-sink-constraints sink)))
    ; ask each DNF(~fclient) separately so we know which variables have been tampered with
    (setq hostiles nil)
    (dolist (d (or2list (dnf (maknot fclient))) hostiles)
      (when (or (not *ss-only-sink-relevant-hostiles*) (intersectionp (vars d) (ss-sink-vars sink)))  ; only pertinent hostiles
	(when *ss-debug* (ss-trace (format nil "** Attempting hostile **~%~S" d) depth))
	(when *ss-stop-now* (return hostiles))
	(when (or (eq tamperable t) (subsetp (vars d) tamperable))
	  ;(when *ss-debug* (ss-trace (format nil "Looking for hostile along success path.") (1+ depth)))
	  ;(when *ss-debug* (unless *quiet* (ss-trace (with-output-to-string (s) (pprint d s)) (1+ depth))))
	  (setq hostile (ss-whitebox-solve (makand d (maksand constraints)) prob 
					   :required required :unique unique :depth (1+ depth)))
	  (cond (hostile
		 (setq tracefile (ss-whitebox-testserver prob hostile :depth (1+ depth)))
		 (setq success (ss-whitebox-trace-successp tracefile))
		 (cond (success 
			(when *ss-show-search* (format t "~&Hostile attempt succeeded:~%~S~%~A~%~S~%~%" d tracefile hostile))
			(when *ss-debug* (ss-trace (format nil "** Hostile found **~%~S~%~A~%~S" hostile tracefile d) depth))
			(push hostile hostiles)
			(ss-log `(hostile ,hostile ,d ,constraints)))
		       (t (when *ss-debug* 
			    (ss-trace (format nil "** Hostile attempt failed **~%server rejection~%~S~%~A~%~S" 
					      hostile tracefile d) depth)))))
		(t (when *ss-debug* 
		     (ss-trace (format nil "** Hostile attempt failed: no string solution **~%~A~%~S" tracefile d) depth)))))))))

(defun ss-whitebox-server-benign_orig (p prob &key (unsats nil) (required nil) (unique nil) (depth 0))
  "(SS-WHITEBOX-SERVER-BENIGN P PROB) checks if P represents a class of inputs
   that the server accepts.  May modify *ss-cast-types*.  
   Returns (i) successp (ii) constraints in trace (iii) solution to p used to test.
   Destructively modifies UNSATS, a list containing a single element, which is a list of unsatisfiable
   sentences."
  ;(when *ss-debug* (ss-trace "Looking for benign." depth) (pprint p))
  (let (benign tracefile constraints successp)
    (when *ss-debug* (ss-trace (format nil "** Attempting benign **~%~S" p) depth))
    (setq benign (ss-whitebox-solve p prob :required required :unique unique :depth depth))
    (cond (benign
	   (when *ss-debug* (ss-trace (format nil "** Found solution **~%~S" benign) depth))
	   (when *ss-show-search* (format t "~S~%~%" benign))
	   (setq tracefile (ss-whitebox-testserver prob benign :depth depth))
	   (setq constraints (ss-whitebox-trace-extract-formulas tracefile prob :depth depth))
	   (setq successp (ss-whitebox-trace-successp tracefile))
	   (cond (successp  
		  (when *ss-debug* 
		    (ss-trace (format nil "** Found success sink **~%~S~%~S" benign p) depth))
		  (when *ss-show-search* (format t "~&Found success sink:~%~S~%~%" benign))
		  (ss-log `(success-sink ,benign ,tracefile ,p ,constraints)))
		 (t (when *ss-debug* (ss-trace "** Trace failure **" depth))))
	   (values successp constraints))
	  (t
	   (when unsats
	     (setf (first unsats) (cons p (first unsats))))
	   (values nil constraints)))))



; in this version, we first use fclient to find a benign input and the server-side constraints C leading to a
;  success and then look for a solution to C and each of the disjuncts of DNF(~fclient).
;  Turns out that C can be too complex to solve reasonably, so this is slow.
#|
(defun ss-whitebox-process-fclient1-orig (prob &key (required nil) (unique nil) (depth 0))
  ;(setq *ss-tmp* prob)
  (let (queue q history prettyhistory successp constraints benign tracefile fclient hostiles newhostiles unsats
	      (*ss-history-unique* nil) (*ss-global-unique* nil) nodecount)
    (setq fclient (ss-prob-phi prob))
    ; try to find 1 hostile for each of the DNF of fclient
    (ss-log `(prob-name ,(ss-prob-name prob)))
    (ss-log `(prob-phi ,(ss-prob-phi prob)))
    (ss-log `(prob-space ,(ss-prob-space prob)))
    (ss-log `(prob-types ,(hash2bl (ss-prob-types prob))))
    (ss-log `(prob-unique ,(ss-prob-unique prob)))
    (ss-log `(prob-required ,(ss-prob-required prob)))
    (ss-log `(prob-metafields ,(ss-prob-metafields prob)))
    (ss-log `(prob-varnames ,(ss-prob-varnames prob)))
    (ss-log `(prob-starttime ,(ss-prob-name prob) ,(get-universal-time)))
    (when *ss-show-search* 
	(format t "~&Fclient for form named ~A:" (ss-prob-name prob))
	(pprint fclient) (format t "~%~%"))
    (dolist (p (or2list (dnf fclient)) hostiles)
      (when *ss-stop-now* (return))
      (ss-log `(newdisjunct ,p))
      (ss-log `(disjunct-starttime ,(get-universal-time)))
      (setq nodecount 0)
      (multiple-value-setq (required unique) (ss-get-required-unique p prob required unique))
      (when *ss-show-search* (format t "~&Fclient disjunct:") (pprint p) (format t "~%~%"))
      (when *ss-debug* 
	(ss-trace (format nil "Starting new search for vulnerability in form named ~A" (ss-prob-name prob)) depth) 
	(unless *quiet* (ss-trace (with-output-to-string (s) (pprint p s)) depth)))
      (setq history nil)   ;  a list of conjunctions of literals.
      (setq prettyhistory (list nil)) ; a list of external constraints (embedded in another list for side effects).
      (setq queue (ss-make-queue))
      (ss-enqueue queue 'true 0)
      ; use a queue to control how we search the space of server-side constraints
      (do () ((ss-empty queue) nil)
	; for each point in the search space, try to find a solution to fclient that leads to a success sink.
	(when *ss-stop-now* (return))
	(setq q (ss-pop queue))
	(when *ss-show-search* (format t "~&~%~S~%" q)) ;   Node~%~S~%" q))
	(setq nodecount (1+ nodecount))
	(push q history)
	(when *ss-debug*
	  (ss-trace "Popping sentence off queue." (1+ depth))
	  (unless *quiet* (ss-trace (with-output-to-string (s) (pprint q s)) (1+ depth))))
	; start by finding a benign input, i.e. a solution to fclient
	(when *ss-debug* (ss-trace (format nil "Looking for candidate benign.") (1+ depth)))
	(setq benign (ss-whitebox-solve (makand p q) prob :required required :unique unique :depth (1+ depth)))
	(cond (benign
	       ; then check if that benign input leads to a success sink.
	       ;(when *ss-debug* (ss-trace (format nil "Found candidate benign.") (1+ depth)))
	       ;(when *ss-debug* (unless *quiet* (ss-trace (format nil "~S" benign) (1+ depth))))
	       (when *ss-show-search* (format t "~&~S" benign))
	       (setq tracefile (ss-whitebox-testserver prob benign :depth (1+ depth)))
	       (let (*ss-cast-types*)
		 (setq constraints (ss-whitebox-trace-extract-formulas tracefile prob :depth (1+ depth)))
		 (setq successp (ss-whitebox-trace-successp tracefile))
		 (when *ss-debug* 
		   (unless *quiet* (ss-trace (format nil "Trace status: ~A" (if successp "success" "failure")) (1+ depth))))
		 (cond (successp
			(when *ss-show-search* (format t "~&Found success sink.~%"))
			(ss-log `(success-sink ,benign ,tracefile ,(makand p q) ,constraints))
					; try to find a hostile input: one satisfying ~fclient and ending in the same sink we just found
			(when *ss-debug* (ss-trace (format nil "Constructing multiple hostiles and patch.") (1+ depth)))
			(setq newhostiles (ss-whitebox-construct-hostile constraints fclient prob :tamperable t :depth (1+ depth)))
			(cond (newhostiles
			       (when *ss-show-search* (format t "~&Found and logged hostiles.~%"))
			       (setq hostiles t)
			       (ss-log `(vulnerability ,(makand p q) ,constraints))			       
			       (when *ss-stop-early* (return))) ; go on to next class of benign inputs
			      (t
			       (when *ss-show-search* 
				 (format t "~&Failed to find hostile.  Continuing search for different success sink.~%"))
			       (when *ss-debug*
				 (ss-trace "Hostile construction failed.  Continuing search for benign." (1+ depth))))))
		       (t ; add constraints to queue
					;(when *ss-show-search* (format t "~&No success sink.  Expanding search space~%"))
			(when *ss-debug* (ss-trace (format nil "Candidate benign not benign.  Continuing search.") (1+ depth)))
			(ss-whitebox-enqueue-server-constraints constraints p q history queue unsats prettyhistory :depth depth)
			(when *ss-debug*
			  (unless *quiet*
			    (ss-trace "New queue." (1+ depth))
			    (ss-trace (with-output-to-string (s) (pprint (ss-raw-queue queue) s)) (1+ depth))))))))	      
	      (t
	       ; unsats are useful so that we avoid calls to the string solver, which is expensive
	       (push q unsats)
	       (when *ss-debug* (ss-trace (format nil "No candidate benign found.") (1+ depth))))))
      (ss-log `(disjunct-nodecount ,nodecount))
      (ss-log `(disjunct-endtime ,(get-universal-time))))
    (ss-log `(prob-solver-count ,*ss-solve-count*))
    (ss-log `(prob-endtime ,(get-universal-time)))
    hostiles))
|#
 
(defun ss-whitebox-construct-patch (tracefile fclient &key (tamperable t) (depth 0))
  "(SS-WHITEBOX-CONSTRUCT-PATCH ...) takes a trace and fclient and constructs a conditional that when placed at the
   top of the server code that generated the trace ensures fclient(inputs) | fclient(sanitize(inputs)) 
   is satisfied before any of the sinks on the server are executed."
    ; interpolate fclient to just the tamperable variables
  (setq fclient (ss-interpolate fclient tamperable))
  `(if (and ,(maknot fclient) ,(ss-whitebox-trace-wpk tracefile (maknot fclient) :depth depth)) exit))

(defun ss-interpolate (p vars)
  "(SS-INTERPOLATE P VARS) reduce P so that the only variables that appear are in VARS."
  (setq p (maksor (remove-if-not #'(lambda (x) (subsetp (vars (ss-simplify-aggressive (and2list x))) vars)) 
				 (or2list (dnf p)))))
  (when (eq p 'false) (format t "Could not interpolate the following to vars ~A~%~A~%" vars p))
  p)

#|
(defun ss-inconsistentp (p q)
  "(SS-INCONSISTENTP P Q) if returns T then P and Q are inconsistent."
  ; canonicalize: (i) nnf, (ii) make p a literal when possible, (iii) make p a conjunction instead of a disjunction.
  (when (or (and (not (literalp p)) (literalp q))
	    (and (not (eq (relation p) 'and)) (eq (relation q) 'and)))
    (let ((tmp p)) (setq p q) (setq q tmp)))

  (cond ((and (literalp p) (literalp q)) 
	 (cond ((and (positive-literalp p) (positive-literalp q))
		
	((and (literalp p) (eq (car q) 'and))
	 (some #'(lambda (x) (ss-inconsistentp p x)) (cdr q)))
	((and (literalp p) (eq (car q) 'or))
	 (every #'(lambda (x) (ss-inconsistentp p x)) (cdr q)))
	((and (eq (car p) 'and) (eq (car q) 'and))
	 (some #'(lambda (x) (some #'(lambda (y) (ss-inconsistentp x y)) (cdr q))) (cdr p)))
	((and (eq (car p) 'and) (eq (car q) 'or))
	 (every #'(lambda (x) (some #'(lambda (y) (ss-inconsistentp x y)) (cdr p))) (cdr q)))
	(t nil)))
|#

(defparameter *ss-pretty-names* (make-hash-table :test #'equal))
(defun ss-set-prettyname (x name) (setf (gethash x *ss-pretty-names*) name))
(defun ss-get-prettyname (x) (let (y) (setq y (gethash x *ss-pretty-names*)) (if y y x)))
(defun ss-negate-pretty-constraint (x)
  (ss-set-prettyname (maknot x) (maknot (ss-get-prettyname x))))

(defun ss-whitebox-enqueue-server-constraints (constraints p q history queue unsats prettyhistory &key (depth 0))
  (declare (ignore depth))
  ;(setq constraints (ss-whitebox-order-constraints constraints))
  ;(when *ss-debug*
  ;  (unless *quiet* 
  ;    (ss-trace (format nil "Trace formula:" ) (1+ depth))
  ;    (pprint constraints)))
  (setq constraints (ss-subsumption-elim constraints))
  (setq constraints (ss-subsumption-elim constraints :key #'ss-get-prettyname))
  ;(setq servervars (vars constraints))
  ;(setq constraints (ss-simplify-aggressive constraints))
  (ss-assert (not (eq constraints :unsat)) nil 
	     (format nil "SS-WHITEBOX-FIND-BENIGNHOSTILE: server constraint set unsatisfiable: ~A" constraints))
					; each constraint can be complex, so convert negation to DNF for each.
  (cond (*ss-aggressive-pruning*
         ; only conjoin q to those constraints dependent on q: detect-dependencies just divides constraints list into 2 lists
	 (multiple-value-bind (indepq depq) (ss-whitebox-enqueue-dependency-split constraints q)
	   (setq depq (mapcar #'(lambda (x) (makand q (maknot x))) depq))
	    ;if pretty names of indep sentences already seen, delete.  Should apply to dep as well but don't have pretty names for them.
	   (setq depq (delete-if #'(lambda (x) (ss-some-similarp1 (ss-get-prettyname x) (first prettyhistory))) depq))
           ; pretty history is a list wrapped in a list so we can modify it in this function
	   (setf (first prettyhistory) (union (mapcar #'ss-get-prettyname depq) (first prettyhistory) :test #'sentequal))
	   (setq constraints (nconc (mapcar #'maknot indepq) depq))))
	(t (setq constraints (mapcar #'(lambda (x) (makand q (maknot x))) constraints))))
  ; eliminate redundancies in search space
  (setq constraints (mapcan #'(lambda (x) (or2list (dnf x))) constraints))
  (setq constraints (delete 'false (mapcar #'(lambda (x) (ss-simplify-constraint-wrt x p)) constraints)))
  (setq constraints (delete-if #'(lambda (x) (tautp x *ss-internal-complements*)) constraints))
  (setq constraints (delete-if #'(lambda (x) 
				   (or (ss-some-similarp1 x history)
				       (ss-some-similarp1 x queue)
				       (ss-some-similarp1 x (list p))
				       (ss-some-similarp2 x unsats)))
			       constraints))
  ; enqueue so that constraints dependent on q are highest in queue  ("top" of queue on left)
  ;(when *ss-debug* 
  ;  (ss-trace "Adding constraints at front of queue." (1+ depth))
  ;  (unless *quiet* (pprint constraints)))
  (ss-batch-enqueue queue (reverse constraints) 0 :test #'sentequal))

(defun ss-sentequal (p q) (sentequal p q :test #'ss-atomequal))
(defun ss-atomequal (p q)
  (cond ((atom p) (eq p q))
	((atom q) nil)
	((equal p q) t)
	((and (eq (car p) '=) (eq (car q) '=))
	 (and (equal (second p) (third q)) (equal (third p) (second q))))
	(t nil)))

(defun ss-simplify-constraint-wrt (c p)
  "(SS-SIMPLIFY-CONSTRAINT-WRT C P) takes two conjunctions of literals C and P and simplifies C with
   respect to P, e.g. removes conjuncts from C implied by P."
  (setq c (copy-list (and2list c)))
  (setq p (and2list p))
  (cond ((atom c) c)
	((atom p) (maksand c))
	(t
	 (let (v)
	   ; delete any (require ?v) or (not (forbid ?v)) where ?v appears in p
	   (setq v (vars p))
	   (setq c (delete-if #'(lambda (x) 
				  (and (listp x) (or (and (eq (first x) 'require) (member (second x) v))
						     (and (negative-literalp x) (eq (car (second x)) 'forbid) 
							  (member (second (second x)) v)))))
			      c))
	   (setq c (delete-if #'(lambda (x) (member (maknot x) p :test #'ss-sentequal)) c))
	   (maksand c)))))

(defun ss-whitebox-enqueue-dependency-split (constraints q)
  "(SS-WHITEBOX-ENQUEUE-DEPENDENCY-SPLIT CONSTRAINTS Q).  Returns CONSTRAINTS split into two
   pieces as two values, where the split is determined by the first occurrence of any variable
   from P that appears. Preserves ordering, e.g. (append v1 v2) gives back CONSTRAINTS."
  (do ((cs constraints (cdr cs))
       (indep)
       (v (vars q)))
      ((null cs) (values (nreverse indep) nil))
    (if (intersectionp v (vars (car cs)))
	(return (values (nreverse indep) cs))
	(push (car cs) indep))))

; poor man's priority queue (generalizes DFS)
(defun ss-make-queue () (list nil))
(defun ss-empty (q) (null (car q)))
(defun ss-raw-queue (q) (car q))

(defun ss-batch-enqueue (q things prioritychange &key (test #'eq))
  (mapc #'(lambda (x) (ss-enqueue q x prioritychange :test test)) (reverse things))
  q)

(defun ss-enqueue (q val prioritychange &key (test #'eq))
  (let (e)
    (setq e (find val (car q) :key #'first :test test))
    (cond (e
	   (setf (second e) (+ (second e) prioritychange)))
	  (t
	   (push (list val prioritychange) (car q))))))

(defun ss-pop (q)
  ; break ties by choosing most recent in queue
  ; to simulate DFS, set all priorities to the same and add items in proper order (so that deepest items are added last)
  (let ((e (caar q)))
    (dolist (v (cdr (car q)))
      (when (< (second v) (second e))
	(setq e v)))
    (setf (car q) (delete e (car q)))
    (car e)))
		   
(defun ss-subsumption-elim (th &key (key #'identity)) (subsumption-elimination th :test #'ss-similarp :key key))
(defun ss-some-similarp1 (p list) (some #'(lambda (x) (ss-similarp p x)) list))
(defun ss-some-similarp2 (p list) (some #'(lambda (x) (ss-similarp x p)) list))
(defun ss-similarp (p q)
  "(SS-SIMILARP P Q) if returns T then P is subsumed by Q."
  (when (literalp p) (setq p (list 'and p)))
  (when (literalp q) (setq q (list 'and q)))
  (cond ((and (eq (car p) 'and) (eq (car q) 'and))
	 (subsetp (cdr p) (cdr q) :test #'sentequal))
	((and (eq (car p) 'or) (eq (car q) 'or))
	 (subsetp (cdr q) (cdr p) :test #'sentequal))
	(t nil)))

(defun ss-whitebox-order-constraints (ps)
  "(SS-WHITEBOX-ORDER CONSTRAINTS) reorders PS so that the most promising constraints to explore come first.
   PS are assumed to be given in the order they appear in the trace."
  (reverse ps))

(defun ss-whitebox-trace-analysis (tracefile prob &key (depth 0))
  "(SS-WHITEBOX-TRACE-ANALYSIS TRACEFILE PROB) extracts sinks from tracefile and cleans them.  Ignores
   sinks with status 'unknown'."
  (declare (ignore depth))
  (flet ((clean (x)
	   (let ((newx (ss-cleanse-more* x prob)))
	     (cond ((ss-istrue newx) 'true)
		   (t (ss-set-prettyname x newx) newx))))
	 (handleerr (e c)
	   (when *ss-debug* (ss-trace (format nil "~A; dropping constraint: ~S" (text e) c)))))
    (let (raw constraints final tracefilec sinkid status sink sinkvars sinks result)
      ; invoke trace analyzer, which produces sinkid, succfailunk, constraints, sink, sinkvars for each sink
      (setq tracefilec (stringappend tracefile ".constraints"))
      (exec-commandline "touch" tracefilec)
      (setq result (exec-commandline "cd" (ss-abspath :traceanalysis "") "; ./run-trace-da.pl" tracefile))
      (write-any-file tracefilec result)
      (setq sinks nil)
      (dolist (s (read-lines result))
	(setq sinkid (first s))
	(setq status (second s))
	(setq raw (third s))
	(setq sink (fourth s))
	(setq sinkvars (fifth s))
	(unless (eq status 'unknown)  ; cleanse unless we're going to ignore anyway
          ; cleanse sinkvars (note call ss-cleanse-more* individually so that ((post "x")) does not become ?x)
	  (setq sinkvars (mapcar #'(lambda (x) (ss-cleanse-more* x prob t)) sinkvars))
          ; cleanse sink
	  (setq sink (ss-cleanse-more* sink prob)) 
          ; cleanse constraints
	  (setq raw (delete-duplicates raw :test #'equal))
	  (setq constraints (delete 'true (mapcar #'clean raw)))
	  (setq raw (mapcar #'ss-get-prettyname constraints))
	  (setq final nil)
	  (dolist (c constraints)
	  ; go ahead and cleanse to make sure we can, but then return the semi-cleansed version.  
	  ;   String-solver will then recleanse.  Necessary to avoid search space with fake variables.
	    (handler-case 
		(progn 
		  (ss-validate-vocab-internal (list (ss-cleanse-more (ss-drop-syntactic-sugar c) prob))) 
		  (push c final))
	      (ss-type-error (e) (handleerr e c))
	      (ss-symbol-error (e) (handleerr e c))
	      (ss-boolean-error (e) (handleerr e c))
	      (ss-syntax-error (e) (handleerr e c))
	      (condition () (when *ss-debug* (ss-trace (format nil "Unknown error; dropping constraint ~S" c))))))
	  (push (make-ss-sink :id sinkid :status status :constraints (nreverse final) :phi sink :vars sinkvars)
		sinks)))
      sinks)))

(defun ss-whitebox-trace-extract-formulas (tracefile prob &key (depth 0))
  (declare (ignore depth))
  (flet ((clean (x)
	   (let ((newx (ss-cleanse-more* x prob)))
	     (cond ((ss-istrue newx) 'true)
		   (t (ss-set-prettyname x newx) newx))))
	 (handleerr (e c)
	   (when *ss-debug* (ss-trace (format nil "~A; dropping constraint: ~S" (text e) c)))))
    (let (raw constraints final tracefilec)
      (setq tracefilec (stringappend tracefile ".constraints"))
      (exec-commandline "touch" tracefilec)
      (exec-commandline *ss-java-invoke* "-Xms512m -Xmx512m edu.uic.rites.sisl.formula.ServerFormulaNazari"
			tracefile tracefilec)
      (setq raw (read-file tracefilec))
      (setq raw (delete-duplicates raw :test #'equal))
      (write-file (stringappend *ss-working-prefix* "formulaext") raw)
      (setq constraints (delete 'true (mapcar #'clean raw)))
      (setq raw (mapcar #'ss-get-prettyname constraints))
      ;(when *ss-debug* 
	;(unless *quiet* (ss-trace "Raw constraints extracted:" depth) (pprint raw)))
      (setq final nil)
      (dolist (c constraints)
	; go ahead and cleanse to make sure we can, but then return the semi-cleansed version.  
	;   String-solver will then recleanse.  Necessary to avoid search space with fake variables.
	(handler-case 
	    (progn 
	      (ss-validate-vocab-internal (list (ss-cleanse-more (ss-drop-syntactic-sugar c) prob))) 
	      (push c final))
	  (ss-type-error (e) (handleerr e c))
	  (ss-symbol-error (e) (handleerr e c))
	  (ss-boolean-error (e) (handleerr e c))
	  (ss-syntax-error (e) (handleerr e c))
	  (condition () (when *ss-debug* (ss-trace (format nil "Unknown error; dropping constraint ~S" c))))))
      (values (nreverse final) raw))))
	
(defun ss-whitebox-trace-truncate (tracefile)
  "(SS-WHITEBOX-TRUNCATE-TRACE TRACE) returns a shortened version of trace where
   the last line is either exit/die or the sink just before the first sink that uses
   its result.  Just a wrapper around a shell script."
    tracefile)
#|  (let (newfile end)
    ; want to do this through files b/c tracefiles are large
    ; Michelle's code -- need to figure out what to do about f(break)
    (setq newfile (stringappend tracefile ".short"))
    (setq tracefile (read-any-file tracefile))
    (setq end (search "mysql_query" tracefile))
    (setq end (position #\Newline tracefile :from-end t :end end))
    (write-any-file newfile (subseq tracefile 0 end))
    newfile))
|#
#|
    (exec-commandline "cd" (ss-abspath :serverformula "formula")
		      "; java ServerFormula " tracefile " -trunk " newfile " \"f(break)\"")
    newfile))
|#

(defun ss-whitebox-trace-successp (tracefile)
  "(SS-WHITEBOX-TRACE-SUCCESSP TRACEFILE) Checks if tracefile resulted in 
   a failure or a success.  Just a wrapper around a shell script."
  (flet ((posindicatorp ()
	   (let (v)
	     (setq v (exec-commandline "grep 'mysql_query\\|fputs\\|file_put_contents'" tracefile))
	     (not (= (length v) 0))))
	 (negindicatorp ()
	   (let (v)
	     (setq v (exec-commandline "grep '^exit()\\|die()'" tracefile))
	     (not (= (length v) 0)))))
    (let (v)
      (case *ss-success-sink*
	(1 (setq v (posindicatorp)))
	(2 (setq v (if (negindicatorp) nil (posindicatorp))))
	(otherwise (setq v (posindicatorp))))
      (cond (*ss-interactive-trace-success*
	     (format t "~&Is the trace contained in the following file a success? [~A]" v)
	     (print tracefile)
	     (if (read) t nil))
	    (t v)))))

;  (if (search "Success"
;	      (exec-commandline "cd" (ss-abspath :serverformula "formula")
;				"; java ServerFormula " tracefile " -check"))
;      t nil))

(defun ss-whitebox-trace-wpk (tracefile p &key (depth 0))
  (declare (ignore tracefile p depth))
  'true)

  
(defun ss-whitebox-testserver (prob bl &key (depth 0))
  "(SS-WHITEBOX-TESTSERVER PROB BL) throws BL at the server and returns the name of a file containing the
   trace produced by the server."
  (let (blfile)
    (setq blfile (stringappend *ss-working-prefix* "bl" (tostring *ss-white-downloads*) ".xml"))
    ; write out binding list in XML
    (with-open-file (f blfile :direction :output :if-does-not-exist :create :if-exists :supersede)      
      (ss-bl2xml (ss-uncleanse (mapcar #'(lambda (x) (list (car x) (cdr x))) bl) prob) prob :stream f))
    ; record that we're giving these inputs to the server
    (setq *ss-history-unique* (nconc (remove-if-not #'(lambda (x) (member (car x) *ss-global-unique*)) bl)
				    *ss-history-unique*))
    ; invoke Nazari's code to send inputs to server
    (ss-whitebox-process-server (ss-whitebox-get-response blfile :depth depth))))

(defun ss-get-html (url &key (dest (stringappend *ss-working-prefix* "homepage.htm")) (depth 0))
  (let ((xmlfile (stringappend *ss-working-prefix* "get_html.xml")))
    (with-open-file (f xmlfile :direction :output :if-does-not-exist :create :if-exists :supersede)
      (ss-bl2xml nil (make-ss-prob :metafields `((url ,url) (method "GET") (enctype "application/x-www-form-urlencoded")))
		 :stream f))
    (setq dest (ss-whitebox-get-response xmlfile :dest dest :url url :depth depth))
    (ss-assert (probe-file dest) nil (format nil "Downloading ~A failed" url))
    (setq dest (ss-whitebox-drop-trace dest))
    (ss-assert (probe-file dest) nil (format nil "Trace deletion ~A failed" url))
    dest))

(defun ss-whitebox-drop-trace (file)
  "(SS-WHITEBOX-PROCESS-SERVER FILE) processes the server response contained in FILE to extract the
   appropriate trace and store it in a new file, whose name is returned."
  (let (newfile openstr)
    (setq openstr "<NOTAMPER_TRACE_START>")
    (setq newfile (stringappend file ".notrace"))
    (exec-commandline (format nil "awk '{if (match($0,\"~A\")) exit; print}' ~A > ~A" openstr file newfile))
    newfile))

(defun ss-whitebox-get-response (xmlfile &key (dest (stringappend *ss-working-prefix* "homepage.htm")) (url nil) (depth 0)) 
  "(SS-WHITEBOX-GET-RESPONSE XMLFILE) invokes the response generator on the given XMLFILE 
   and returns the file storing the result.  Not using wget b/c response generator 
   handles weird javascript redirects.  Also, response generator allows for cookies."
  (setq dest (stringappend dest "." *ss-white-downloads*))
  (setq *ss-white-downloads* (1+ *ss-white-downloads*))
  (when *ss-debug* (if url
		       (ss-trace (format nil "** Downloading ~A to ~A **" url dest) depth)))
		       ;(ss-trace (format nil "Downloading server response to ~A" dest) depth)))
  (exec-commandline "cd" (ss-abspath :respgen "") 
		     ;"; rm -Rf responses results diffsizes.txt; mkdir responses; mkdir results;"
		    ";" *ss-java-invoke* "-jar HTTPReqGen.jar" xmlfile "-d" dest)
  dest)

(defun ss-whitebox-process-server (file)
  "(SS-WHITEBOX-PROCESS-SERVER FILE) processes the server response contained in FILE to extract the
   appropriate trace and store it in a new file, whose name is returned."
  (let (newfile)
    (setq newfile (stringappend file ".trace"))
    (exec-commandline "awk '/NOTAMPER_TRACE_START/,/NOTAMPER_TRACE_END/'" file "| sed '1d;$d'" ">" newfile)
    (ss-whitebox-trace-truncate newfile)))
  
(defun ss-whitebox-solve (p prob &key (required nil) (unique nil) (depth 0))
  "(SS-WHITEBOX-SOLVE P ...) runs stringsolver on P and returns result"
  (let (val u (*ss-whitebox-log* *ss-whitebox-log*) blokay)
    ; make sure to reduce to internal vocab
    (setq p (car (ss-validate-vocab-internal (list (ss-cleanse-more (ss-drop-syntactic-sugar p) prob)))))

    ; create unique constraints
    (setq u (remove-if-not #'(lambda (x) (member (car x) unique)) *ss-history-unique*))
    (setq u (mapcar #'(lambda (x) `(!= ,(car x) ,(cdr x))) u))

    ; create sentence
    (setq p (maksand (list* p (ss-prob-space prob) u)))

    (when *ss-debug* 
      (ss-trace (format nil "** String solving **~%~S" p) depth))
;      (unless *quiet* (pprint p)))

    ;(setq p (ss-drop-syntactic-sugar p))
    (setq val (ss-solve p :required required :types (ss-prob-types prob)))
    (cond (val
	   (setq val (hash2bl val))
	   (when *ss-debug* 
	     ;(ss-trace (format nil "Found string solution.") depth)
	     ;(unless *quiet* (ss-trace (format nil "~S" val) depth))
	     (setq blokay (ss-checksat p (ss-prob-types prob) (append val truth)))
	     (unless blokay
	       (if *break-on-external-solver-error*
		   (ss-error (format nil "ss-solve failed on ~%~S~%producing ~%~S~%with types~%~S~%" 
				     p val (hash2bl (ss-prob-types prob)))) 
		   (format t "ss-solve failed on ~%~S~%producing ~%~S~%with types~%~S~%" 
				     p val (hash2bl (ss-prob-types prob)))))))
	  (t ));(when *ss-debug* (ss-trace "No string solution." depth))))
    val))
    
(defun ss-whitebox-init (url)
  (declare (ignore url))
  ;(when *ss-debug* (ss-trace (format nil "Running whitebox analysis on ~A" url) 0))
  ; make sure temp directory exists
  (exec-commandline "mkdir" *ss-working-prefix*)
  ; grab env variables
  (setq *ss-home* (string-trim '(#\Space #\Newline #\Return) (exec-commandline "echo $NOTAMPER_BASE")))
  (setq *ss-java-home* (string-trim '(#\Space #\Newline #\Return) (exec-commandline "echo $JAVA_HOME")))
  ; clean up paths
  (setq *ss-home* (ss-path-endify *ss-home*))
  (setq *ss-working-prefix* (ss-path-endify *ss-working-prefix*))
  (setq *ss-java-home* (ss-path-endify *ss-java-home*))
  (dolist (v *ss-component-paths*)
    (setf (cdr v) (ss-path-endify (string-left-trim '(#\/) (cdr v)))))

  ; other globals
  (setq *ss-whitebox-log* (stringappend *ss-home* *ss-whitebox-logname*))
  (setq *ss-java-invoke* (stringappend *ss-java-home* "bin/java")))

(defun ss-whitebox-cleanup ()
  (exec-commandline "rm -f" (stringappend *ss-working-prefix* "*")))  ;-f F* homepage.htm *.constraints* *.scripts *.log"))
; (stringappend *ss-home* "cleanup.sh")))

(defun ss-compute-fclients (url &key (space 'true) (depth 0))
  "(SS-COMPUTE-FCLIENTS URL) takes a URL and returns a list of ss-prob structs:
   one for each form at the URL.  Exactly same as blackbox version."
  (exec-commandline "rm" (stringappend *ss-working-prefix* "F*"))
  (ss-extract-ht-constraints (ss-get-html url :depth depth) url :depth depth)
  (ss-extract-js-constraints :depth depth)
  (mapcar #'(lambda (x) 
	      (let (p)
		(setq p (eval (first (read-file x))))
		(setf (ss-prob-space p) (makand (ss-prob-space p) space))
		(ss-cleanse p))) 
	  (ss-combine-constraints :depth depth)))
    
(defun ss-extract-ht-constraints (htmlfile url &key (depth 0))
  (when *ss-debug* (ss-trace "** Extracting HTML Constraints **" depth))
  (exec-commandline "cd" *ss-working-prefix* ";" *ss-java-invoke* "HTMLConstraints" "-offline" htmlfile url))

(defun ss-extract-js-constraints (&key (depth 0))
  "(SS-EXTRACT-JS-CONSTRAINTS) for each XXXX.script file in working directory, 
   run JS extractor to produce XXXX.constraints_JS file in working directory
   that contains the constraints imposed by the JS file."
  (when *ss-debug* (ss-trace "** Extracting JavaScript Constraints **" depth))
  (let (scripts logfile jscfile)
    (setq scripts (mapcar #'namestring (directory (stringappend *ss-working-prefix* "*.scripts"))))
    (dolist (s scripts)
      (unless (search "DO_NOT_PROCESS_NOTAMPER" (read-any-file s))
	(exec-commandline "cp" s (ss-abspath :narcissus "/test.js"))
	(setq logfile (stringappend s ".log"))
	(exec-commandline (ss-abspath :jsdist "js") (ss-abspath :narcissus "js.js") ">" logfile)
;	(with-open-file (f logfile :direction :output :if-does-not-exist :create :if-exists :supersede)
;	  (run-program (ss-abspath :jsdist "") (list "js.js") :output f))
	(setq jscfile (stringappend (drop-file-extension s) ".constraints_JS"))
	(exec-commandline "grep \"JavaScript evaluator generated these constraints NTBEGIN\"" logfile "| sed -e \"s/JavaScript evaluator generated these constraints NTBEGIN//g\"" ">" jscfile)))))

;	(with-open-file (f jscfile :direction :output :if-does-not-exist :create :if-exists :supersede)
;	  (princ (exec-commandline (stringappend "grep \"JavaScript evaluator generated these constraints NTBEGIN\"" logfile " | sed -e \"s/JavaScript evaluator generated these constraints NTBEGIN//g\"")) f))))))

(defun ss-trace (msg &optional (depth 0))
  (declare (ignore depth))
  (format t "~&")
;  (dotimes (i depth)
;    (format t " | "))
  (princ msg t)
  (format t "~%"))

(defun ss-combine-constraints (&key (depth 0))
  "(SS-COMBINE-CONSTRAINTS) takes constraints from X.constraints_H X.constraints_H_1 X.constraints_JS and
   combines them into a single X.constraints file, which is in the ss-prob format."
  (when *ss-debug* (ss-trace "** Combining Constraints **" depth))
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
;;  Blackbox NoTamper
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
  (let ((history nil) bl (good nil) (bad nil) firstgood orlist tmp (*ss-false-values* #'php-false))
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
;; Output results (XML)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun notamper2xml (notamperout prob &key (stream t) (time nil) (count nil)) 
  "(NOTAMPER2XML NOTAMPEROUT S) takes the output of notamper and prints to stream S
   an XML representation of it.  Used for blackbox version of NoTamper."
  (flet ((f (x) (ss-xml-esc x)))
    (format stream "<form name=\"~A\">~%" (f (ss-prob-name prob)))
    (printspaces 2 stream) (format stream "<attributes>~%")
    (dolist (m (ss-prob-metafields prob))
      (printspaces 4 stream)
      (format stream "<~(~A~)>~A</~(~A~)>~%" (f (first m)) (f (second m)) (f (first m))))
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
    (format stream "</form>~%")))

(defun ss-xml-esc (x) 
  (setq x (tostring x)) 
  (replace-all x "&" "&amp;"))

(defun ss-bl2xml (bl prob &key (stream t))
  (flet ((f (x) (ss-xml-esc x)))
    (format stream "<testcases>~%")
    (format stream "<form name=\"~A\">~%" (f (ss-prob-name prob)))
    (printspaces 2 stream) (format stream "<attributes>~%")
    (dolist (m (ss-prob-metafields prob))
      (printspaces 4 stream)
      (format stream "<~(~A~)>~A</~(~A~)>~%" (f (first m)) (f (second m)) (f (first m))))
    (printspaces 2 stream) (format stream "</attributes>~%")
    (printspaces 2 stream) (format stream "<good>~%")
    (printspaces 4 stream) (format stream "<case>~%")
    (when bl (notamper2xml-varassign nil bl 6 stream 
				     (mapcar #'(lambda (x) (list (third x) (second x))) (ss-prob-varnames prob))))
    (printspaces 4 stream) (format stream "</case>~%")
    (printspaces 2 stream) (format stream "</good>~%")
    (format stream "</form>~%")
    (format stream "</testcases>")))

(defun ss-vars (p)
  (cond ((varp p) (list p))
	((atom p) nil)
	((member (car p) '(and or not => <= <=> forall exists)) (mapcan #'ss-vars (cdr p)))
	((eq (car p) 'var) (list (second p)))
	(t (mapcan #'ss-vars (cdr p)))))

(defun notamper2xml-varassign (tamperedvars bl n s &optional (varstoshow t))
  (flet ((f (x) (ss-xml-esc x)))
    (let (var loc)
      (dolist (x bl)
	(when (or (eq varstoshow t) (member (car x) varstoshow :test #'equal))
	   ; grab val and location get/put/cookie 
	  (cond ((atom (first x))
		 (setq loc nil)
		 (setq var (first x)))
		((eq (first (first x)) 'var)
		 (setq loc nil)
		 (setq var (second (first x))))
		(t
		 (setq loc (first (first x)))
		 (setq var (second (first x)))))
           ; output
	  (printspaces n s) 
	  (format s "<bl><var tamper=\"~A\"~A>~A</var><val>~A</val></bl>~%" 
		  (if (member var tamperedvars) "true" "false")
		  (if loc (format nil " location=\"~A\"" loc) "")
		  (f (urlify (notamper2web-varspelling (first x))))
		  (f (urlify (second x)))))))))
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
	((member (car v) '(var get post cookie)) (second v))
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

(defun replace-all (string part replacement &key (test #'char=))
"Returns a new string in which all the occurences of the part 
is replaced with replacement."
    (with-output-to-string (out)
      (loop with part-length = (length part)
            for old-pos = 0 then (+ pos part-length)
            for pos = (search part string
                              :start2 old-pos
                              :test test)
            do (write-string string out
                             :start old-pos
                             :end (or pos (length string)))
            when pos do (write-string replacement out)
            while pos))) 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
