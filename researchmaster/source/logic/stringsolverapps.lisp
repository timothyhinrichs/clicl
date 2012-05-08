;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; stringsolverapps.lisp
;;      Applications of string solver.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun r () 
  (let (home)
    (setq home (string-trim '(#\Space #\Newline #\Return) (exec-commandline "echo $NOTAMPER_BASE")))
    (setq home (ss-path-endify home))
    (load (stringappend home "solver/clicl/researchmaster/source/logic/stringsolver.lisp"))
    (load (stringappend home "solver/clicl/researchmaster/source/logic/stringsolverapps.lisp"))
    (load (stringappend home "solver/clicl/researchmaster/source/logic/stringsolvertests.lisp"))))

; save-application call (deprecated)
(defparameter *app-location* "./ss"
  "location for application--only used when saving application")
(defun savess ()
  ;(ignore-errors (load *app-init-location*))
  (save-application *app-location* :prepend-kernel t))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Common
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defparameter *app-init-location* 
  "./ss-init.lisp"
  "location for application initialization--used for application")

; user customizations
(defparameter *ss-node-limit* nil
  "NIL or int defining the maximum number of nodes to search")
(defparameter *ss-solve-limit* nil
  "NIL or int defining the maximum number of string solver invocations")
(defparameter *ss-benign-limit* nil
  "NIL or int defining the maximum number of benign inputs to find")
(defparameter *ss-reset-limits-per-disjunct* nil
  "boolean controlling whether or not the above limits are per disjunct or are global")

(defparameter *ss-obey-unique* nil
  "whether to enforce the uniqueness constraints---problematic b/c of Kaluza bug.")
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
(defparameter *ss-run-blackbox* t
  "whether or not to run the blackbox version of the system (without rank analysis)")
(defparameter *ss-run-whitebox* t
  "whether or not to run the whitebox version of the system")
(defparameter *ss-delete-duplicate-benign-sinks* t
  "whether to delete benign sinks that were visited previously")
(defparameter *ss-blackbox-analyze-sinks* t
  "whether to use trace from instrumented app to check correctness of blackbox approach")

(defparameter *ss-java-home* nil "read from environmental variable JAVA_HOME")
(defparameter *ss-java-invoke* nil "$JAVA_HOME/bin/java")
(defparameter *ss-whitebox-logname* "notamper.log" "user-provided file path starting from *ss-home*")
(defparameter *ss-working-prefix* "/tmp/notamper/")
(defparameter *ss-component-paths* '((:narcissus . "narcissus_js/js/narcissus")
			       (:jsdist . "narcissus_js/dist/bin")
			       (:htmlext . "formula-extractor/formula-generator")
			       (:respgen . "response-generator")
			       (:serverformula . "server-formula-wpk")
			       (:rankgen . "rank-generator")
			       (:traceanalysis . "trace-dep-analysis")
			       (:jsintegrator . "jsintegrator")))
(defparameter *ss-db-queries* '(mysql_query))

(defparameter *ss-wavs-final* '("wavsvalidation.js" "scripts"))
 
; internal global vars
(defvar *ss-benign-history* nil "a list of benigns we already found--no need to re-check")
(defparameter *ss-node-count* 0 "global counter for nodes searched")
(defparameter *ss-benign-count* 0 "global counter for benign inputs discovered")
(defparameter *ss-whitebox-log* "" "internal, full path to log")
(defparameter *ss-home* "" "read from environmental variable NOTAMPER_BASE")
(defvar *ss-history-unique* nil
  "global storage of all the assignments made for the unique variables")
(defvar *ss-global-unique* nil
  "global storage for the set of all unique vars for an entire server")
(defvar *ss-white-downloads* 0)
(defvar *ss-prob* nil)




(defstruct ss-prob 
  ; user-supplied 
  (name nil) phi (space 'true) (types nil)  (unique t) (required t) (init nil)
  (status :contingent) (metafields nil) 
  ; internal
  (varnames nil) (variantstatus nil) (dbfile nil) (stuburl nil) (indep nil))

; constraints should be the union of staticconstraints and dynamicconstraints
(defstruct ss-sink id phi vars status constraints dbconstraints dynamicconstraints staticconstraints)
(defun ss-sink-success (x) (eq (ss-sink-status x) 'success))
(defun ss-sink-failure (x) (eq (ss-sink-status x) 'failure))
(defun ss-sink-unknown (x) (eq (ss-sink-status x) 'unknown))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Proxy patching
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun ss-proxypatch (url &key space)
  (let (files counter filename fclient)
    ; initialize environment
    (load *app-init-location*)
    (ss-assert (stringp url) nil (format nil "SS-WHITEBOX expects a string; found ~A" url))
    (ss-whitebox-init url)
    (ss-whitebox-cleanup)  ; so users don't get confused about whether or not stuff was downloaded
    (exec-commandline "mv" *ss-whitebox-log* (stringappend *ss-whitebox-log* ".bak"))
    ; generate patches
    (setq counter 0)
    (dolist (prob (ss-compute-fclients url :space space))
      (setq filename (stringappend *ss-working-prefix* (format nil "proxypatch~A.pl" counter)))
      (with-open-file (f filename :direction :output :if-exists :supersede :if-does-not-exist :create)
	(setq fclient (makand (ss-prob-phi prob) (maksand (mapcar #'cdr (hash2bl (ss-prob-types prob))))))
	(push (list filename (jskifs2perl (drop-ands fclient) f) fclient) files))
      (setq counter (1+ counter)))
    (nreverse files)))
	
(defun jskifs2perl (ps s)
  "(JSKIFS2PERL PS S) takes a list of constraints PS and a stream S and writes out Perl
   to S so that when executed returns 0 if there is an error and 1 otherwise."
  (let (vars)
    (format s "#!/usr/bin/perl -w~%")
    ; grab variables from command line
    (setq vars (vars ps))
    (do ((vs vars (cdr vs))
	 (i 0 (1+ i)))
	((null vs))
      (format s "my ")
      (jskifterm2perl (car vs) s)
      (format s " = $ARGV[~A];~%" i))
	; output the test for each formula in fclient; if fails, exit early.
    (dolist (formula ps)
      (format s "if (")
      (jskif2perl (maknot formula) s)
      (format s ") { print \"constraint violated: \", __LINE__, \"\\n\"; exit 0; }~%"))
	; footer info
    (format s "print \"passed ALL tests\\n\";~%")
    (format s "exit 1;~%")
    vars))
 
(define-condition unknown-symbol (error) ((comment :initarg :comment :accessor comment)))
(define-condition syntax (error) ((comment :initarg :comment :accessor comment)))
(define-condition unimplemented (error) ((comment :initarg :comment :accessor comment)))

(defun jskif2perl (p s)
  "(JSKIF2PERL P) outputs quantifier-free KIF formula P, assumed to be implicitly existentially quantified, 
   in Perl syntax.  Good for quickly checking if P is satisfied given values for all variables.  
   Also assumes that all object constants are strings or numbers."
  (flet ((printreg (x s) 
	   (setq x (ss-makreg x))
	   (format s "/^(~A)$/" x)))
    (cond ((eq p 'true) (format s "0 == 0"))
	  ((eq p 'false) (format s "0 == 1"))
	  ((atom p) (error 'syntax :comment (format nil "~A" p)))
	  ((eq (car p) 'and) (format s "(") (list2infix (cdr p) "&&" #'jskif2perl s) (format s ")"))
	  ((eq (car p) 'or) (format s "(") (list2infix (cdr p) "||" #'jskif2perl s) (format s ")"))
	  ((eq (car p) 'not) (format s "!(") (jskif2perl (second p) s) (format s ")"))
	  ((eq (car p) 'require) (jskif2perl `(!= (len ,(second p)) 0) s))
	  ((eq (car p) 'forbid) (jskif2perl `(= (len ,(second p)) 0) s))
	  (t (case (first p)
	       (in (jskifterm2perl (second p) s) (format s " =~~ ") (printreg (third p) s))
	       (notin (jskifterm2perl (second p) s) (format s " !~~ ") (printreg (third p) s))
	       (nin (jskifterm2perl (second p) s) (format s " !~~ ") (printreg (third p) s))
	       (lt (jskifterm2perl (second p) s) (format s " < ") (jskifterm2perl (third p) s))
	       (lte (jskifterm2perl (second p) s) (format s " <= ") (jskifterm2perl (third p) s))
	       (gt (jskifterm2perl (second p) s) (format s " > ") (jskifterm2perl (third p) s))
	       (gte (jskifterm2perl (second p) s) (format s " >= ") (jskifterm2perl (third p) s))
	       (= (jskifterm2perl (second p) s) (format s " eq ") (jskifterm2perl (third p) s))
	       (!= (jskifterm2perl (second p) s) (format s " ne ") (jskifterm2perl (third p) s))
	       (type (if (stringp (third p)) (jskif2perl `(in ,(second p) ,(third p)) s) (error 'unimplemented :comment (format nil "~A" p))))
	       (tobool (error 'unimplemented :comment (format nil "~A" p)))
	       (tostr (error 'unimplemented :comment (format nil "~A" p)))
	       (tonum (error 'unimplemented :comment (format nil "~A" p)))
	       (otherwise (error 'syntax :comment (format nil "~A" p))))))))

(defun jskifterm2perl (e s)
  (cond ((varp e) (format s "$~(~A~)" (devariable e)))
	((stringp e) (prin1 e s))
	((numberp e) (princ e s))
	((atom e) (error 'syntax :comment (format nil "~A" e)))
	((eq (car e) '+) (format s "(") (list2infix (cdr e) "+" #'jskifterm2perl s) (format s ")"))
	((eq (car e) '-) (format s "(") (list2infix (cdr e) "-" #'jskifterm2perl s) (format s ")"))
	((eq (car e) '*) (format s "(") (list2infix (cdr e) "*" #'jskifterm2perl s) (format s ")"))
	((eq (car e) '/) (format s "(") (list2infix (cdr e) "/" #'jskifterm2perl s) (format s ")"))
	((eq (car e) 'len) (format s "length(") (jskifterm2perl (second e) s) (format s ")"))
 	((eq (car e) 'concat) (format s "(") (list2infix (cdr e) "." #'jskifterm2perl s) (format s ")"))
	(t (error 'syntax :comment (format nil "~A" e)))))
       


	     
 

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;  Web Application Validation Extraction and Synthesis (WAVES)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; client validation from whitebox server analysis
;  govwa, cvwsa .... wavs  (whitebox analysis validation synthesis)

(defun wt (name)
  (dolist (e (wavs-tests))
    (when (eq (wavstest-name e) name)
      (wavs (wavstest-url e) (wavstest-bl e) (wavstest-stuburl e) :unique (wavstest-unique e) 
	    :required (wavstest-required e) :multipart (wavstest-multipart e) :forms (list (wavstest-formid e))
	    :indep (wavstest-indep e)))))

(defun wavs (url bl stuburl &key (unique 'unknown) (required 'unknown) (multipart nil) (forms nil) (indep nil))
  (ss-synth url bl :stuburl stuburl :unique unique :required required :multipart multipart :forms forms :indep indep))

(defun ss-output-wavs (results prob &key (time 0) (stream t) (count 0))
  "(SS-OUTPUT-WAVS RESULTS PROB TIME STREAM COUNT) outputs the results to STREAM."
  (declare (ignore count))
  (format stream "~A" results)
  (when *ss-debug* 
    (format stream "~&***** Processing for ~A complete in ~A seconds. *****~%"
	    (ss-prob-name prob) time)
    (format stream "~&***** Logfile available in ~A *****~%" *ss-whitebox-log*)
    (format stream "~&***** Final JS files found in ~A: ~A *****~%" *ss-working-prefix* *ss-wavs-final*) 
    (format stream "~&***** ")))
  
;  (if results
;      (format stream "ALERT: hostiles recorded in ~A ." *ss-whitebox-log*)
;      (format stream "No hostiles found. Logfile: ~A" *ss-whitebox-log*)))

(defun ss-synth (url bl &key space (unique 'unknown) (required 'unknown) (stream t) (forms nil) (dbfile "harness.db")
		    (stuburl nil) (multipart nil) (indep nil) (outputfun #'ss-output-wavs))
  "(SS-SYNTH URL INIT) takes a url and an initial variable assignment.  Returns JavaScript
   code and PHP code.  When JavaScript is added to the client, it detects user errors without
   hitting the server, except when required by AJAX."
  (let (fclientprobs url2 start results (*ss-white-downloads* 0) (*ss-ignore-variable-source* nil))
    (load *app-init-location*)
    (ss-assert (stringp url) nil (format nil "SS-WHITEBOX expects a string; found ~A" url))
    (ss-whitebox-init url)
    (ss-whitebox-cleanup)  ; so users don't get confused about whether or not stuff was downloaded
    (exec-commandline "mv" *ss-whitebox-log* (stringappend *ss-whitebox-log* ".bak"))
    ; grab one fclient for each form
    (setq fclientprobs nil)
    (ss-log `(wavs url ,url))
    (ss-log `(wavs bl ,bl))
    (when *ss-show-search* (format t "~&Pre-processing ~A~%" url))
    (dolist (p (ss-compute-fclients url :space space))
      (setf (ss-prob-dbfile p) dbfile)
      (setf (ss-prob-stuburl p) stuburl)
      (when multipart (setf (ss-prob-metafields p) (cons `(multipart t) (ss-prob-metafields p))))
      (setq url2 (second (assoc 'url (ss-prob-metafields p))))
      (if (equalp (ss-base-url url2) (ss-base-url url))
	  (push p fclientprobs)
	  (when *ss-debug* (ss-trace (format nil "** Ignoring form with URL ~A **" url2) 0))))
    (when *ss-pause-after-download* (format t "~&Enter anything to proceed:") (read))
   
    ; analyze each (local) fclients
    (do ((fclients (nreverse fclientprobs) (cdr fclients)) (fclient))
	((null fclients))
      (setq fclient (car fclients))
      (when *ss-stop-now* (return))
      (setq url2 (second (assoc 'url (ss-prob-metafields fclient))))
      (when (or (null forms) (member (ss-prob-name fclient) forms) (member url2 forms :test #'equalp))
	(setq *ss-solve-count* 0)
	(setq start (get-universal-time))
	(when *ss-debug* 
	  (ss-trace (format nil "** Client validation synthesis for form ~A commencing **" (ss-prob-name fclient)) 0)
	  (unless *quiet* (pprint (ss-prob-phi fclient))))
	(setq bl (ss-cleanse-more* bl fclient t))
	(setq unique (ss-cleanse-more* unique fclient t))
	(setq required (ss-cleanse-more* required fclient t))
	(setf (ss-prob-indep fclient) (ss-cleanse-more* indep fclient t))
	(setq results (ss-synth-process-fclient bl fclient :unique unique :required required :depth 1))
	(funcall outputfun results fclient :time (- (get-universal-time) start) :stream stream :count *ss-solve-count*)
	(when (and (cdr fclients) *ss-interactive-form-control*)
	  (format t "~&Proceed?  (No = nil) ") (let ((v (read))) (unless v (return))))))
    ;(ss-whitebox-cleanup)
    nil))

; Assume routines that
; (i) take a trace and extract formulas and identify sinks (each formula: KIF), as in NoTamper/WAPTEC
; (ii) take a trace and split static and dynamic constraints.  Static: KIF, dynamic <KIF, line number>
; (iii) given a line number for a constraint and a trace, generate a stub.

; nazari's script will accept n+1 filenames: file1 is the output of plato; the reamining files are info about the dynamic constraints for each trace.  
; Nazari needs (i) Plato's output (in a file) (ii) stuburl (iii) formid (iv) list of fields with static constraints
;    (v) list of fields with dynamic constraints.  Note (iv) and (v) are a partition of the fields.
; prithvi will make each sink include an additional field: the list of dynamic constraints. 

(defun ss-synth-process-fclient (benignbl prob &key (required nil) (unique nil) (depth 0)) 
  "(SS-SYNTH-PROCESS-FCLIENT) returns a client filename and a list of server stub file names."
  (let (sinks tracefile constraints fstatic fdynamic code safe newstatic newdynamic allvars)
    (labels ((quick-clean (constraints)
	       (setq constraints (and2list (flatten-operator (nnf (maksand constraints)))))
	       (setq constraints (ss-drop-nonmethod constraints prob))
	       (setq constraints (ss-cleanse-more* constraints prob t))
	       constraints)
	     (add-constraints-to-safe (constraints)
	       (setq constraints (quick-clean constraints))
	       (setq safe (union (ss-synth-partition constraints) safe :test #'ss-sentequal)))
	     (add-sink-to-safe (sink)
	       (when (ss-sink-success sink)
		 (let ((old safe))
		   (add-constraints-to-safe (ss-sink-staticconstraints sink))
		   (add-constraints-to-safe (ss-sink-dynamicconstraints sink))
		   (when *ss-debug*
		     (let ((new (set-difference safe old :test #'ss-sentequal))) 
		       (when new
			 (ss-trace (format nil "** Found new safe constraints **") depth)
			 (unless *quiet* (pprint new))))))))
	     (simplify-failure (constraints)
	       (setq constraints (quick-clean constraints))
	       ; partition depending on indep info (yielding a list of sentences, some of which are conjunctions)
	       (if (ss-prob-indep prob)
		   (setq constraints (ss-synth-partition constraints :indep (ss-prob-indep prob)))
		   (setq constraints (list (maksand constraints))))
	       ; then remove those partitions that are safe.
	       (remove-if #'(lambda (x) (member x safe :test #'ss-intersectsp)) constraints)))
    (setq *ss-tmp* prob)
    (ss-log `(prob name ,(ss-prob-name prob)))
    (ss-log `(prob phi ,(ss-prob-phi prob)))
    (ss-log `(prob space ,(ss-prob-space prob)))
    (ss-log `(prob types ,(hash2bl (ss-prob-types prob))))
    (ss-log `(prob unique ,(ss-prob-unique prob)))
    (ss-log `(prob required ,(ss-prob-required prob)))
    (ss-log `(prob metafields ,(ss-prob-metafields prob)))
    (ss-log `(prob varnames ,(ss-prob-varnames prob)))
    (ss-log `(prob indep ,(ss-prob-indep prob)))
    (ss-log `(prob starttime ,(get-universal-time)))

    ; Use BL to take us to a success sink
    (setq tracefile (ss-wavs-server-trace prob benignbl :depth depth))
    ;(setq sinks (ss-whitebox-trace-analysis tracefile prob :depth depth))
    (setq sinks (ss-trace-analysis-extract tracefile prob :depth depth))
    (mapc #'add-sink-to-safe sinks)  ; safe uses uncleansed constraints, since errors use uncleansed constraints
    (setq sinks (mapcar #'(lambda (x) (ss-clean-sink x prob :depth depth :drop-nonmethod t)) sinks))
    (setq sinks (delete-if #'ss-sink-unknown sinks))
    ; union (static) constraints if multiple sinks and cross fingers
    (setq constraints (mapcan #'ss-sink-staticconstraints sinks)) 
    (setq constraints (remove-duplicates constraints :test #'ss-sentequal))
    (ss-log `(benign constraints ,constraints))
    (when *ss-debug*
      (ss-trace (format nil "** Found constraints with complexity ~A **" (complexity (maksand constraints))) depth)
      (unless *quiet* (pprint (maksand constraints))))

    (cond ((null constraints) "/dev/null")
	  (t
           ; find failure sinks (by negating one constraint at a time)
	   (setq allvars (apply #'append (ss-prob-indep prob)))
	   (do ((cs constraints (cdr cs))
		(hostilebl) (depconstraints) (depvars))
	       ((null cs))
	     (when (subsetp (vars (car cs)) allvars)   ; drop any constraint including non allvars
	       (restart-case 
		   (progn
		     (setq depconstraints (list (maknot (car cs))))
		     (setq depvars (vars (car cs)))
		     (when *ss-debug*
		       (ss-trace (format nil "** Looking for hostile trace using constraints ~S **" depconstraints)
				 depth))
	             ; build hostile bl out of (i) depconstraints solution and 
		     ;    (ii) assignments in benignbl that are indep
		     (setq hostilebl (ss-whitebox-solve (maksand depconstraints) prob 
							:required required :unique unique :depth depth))
		     (cond ((eq hostilebl :unsat)
			    (when *ss-debug* (ss-trace "** Hostile input construction failed **" depth)))
			   (t
			    (setq hostilebl (append hostilebl (remove-if #'(lambda (x) (member (first x) depvars)) benignbl)))
			    (when *ss-debug*
			      (ss-trace (format nil "** Found hostile binding list ~S **" hostilebl) depth))
			    (setq tracefile (ss-wavs-server-trace prob hostilebl :depth depth))
			    (setq sinks (ss-trace-analysis-extract tracefile prob :depth depth))
			    (setq sinks (delete-if #'ss-sink-unknown sinks))
			    (dolist (s sinks)
			      (cond ((ss-sink-failure s)
				     (setq newstatic (maksand (simplify-failure (ss-sink-staticconstraints s))))
				     (when (subsetp (vars newstatic) allvars)
				       (setq fstatic (adjoin newstatic fstatic :test #'ss-sentequal))
				       (when *ss-debug*
					 (when (not (eq newstatic 'true))
					   (ss-trace "** Found static error conditions **")
					   (unless *quiet* (pprint newstatic)))))
				     (setq newdynamic (maksand (simplify-failure (ss-sink-dynamicconstraints s))))
				     (when (subsetp (vars newdynamic) allvars)
				       (setq fdynamic (adjoin newdynamic fdynamic :test #'ss-sentequal))
				       (when *ss-debug*
					 (when (not (eq newdynamic 'true))
					   (ss-trace "** Found dynamic error conditions **")
					   (unless *quiet* (pprint newdynamic))))))
				    ((ss-sink-success s) (add-sink-to-safe s)))))))
		 (skip-constraint () 
		   (when *ss-debug* 
		     (ss-trace (format nil "Continuing after error when negating ~A " (car cs))))))))
      
	   ; generate actual code
	   (setq fstatic (maknot (maksor (remove-duplicates fstatic :test #'ss-sentequal))))
	   (setq fdynamic (maknot (maksor (remove-duplicates fdynamic :test #'ss-sentequal))))
	   (setq fstatic (ss-uncleanse fstatic prob))
	   (setq fdynamic (ss-uncleanse fdynamic prob))
	   (ss-log `(fstatic constraints ,fstatic))
	   (ss-log `(fstatic complexity ,(complexity fstatic)))
	   (ss-log `(fdynamic constraints ,fdynamic))
	   (ss-log `(fdynamic complexity ,(complexity fdynamic)))
	   (setq code (ss-synth-code fstatic fdynamic prob :depth depth))
	   (ss-log `(prob endtime ,(get-universal-time)))
	   code)))))

; grab the trace the normal way, but then roll back the database
(defun ss-wavs-server-trace (prob bl &key (depth 0))
  (let (res)
    (setq res (ss-whitebox-server-trace prob bl :depth depth))
    (exec-commandline "rollbackwavs")
    res))

(defun ss-intersectsp (p q)
  "(SS-INTERSECTSP P Q) returns T if P and Q share at least one variable and E*.P^Q is satisfiable,
   assuming that both P and Q are satisfiable.  Only approximate, but fast."
  (flet ((reduce (r)
	   (cond ((atom r) (list r))
		 ((eq (car r) 'and) (drop-ands r))
		 (t (list r)))))
    (setq p (reduce p))
    (setq q (reduce q))
    (or (subsetp p q :test #'ss-sentequal)
	(subsetp q p :test #'ss-sentequal))))

(defun ss-synth-constraints-influencing (vars ps)
  "(SS-SYNTH-CONSTRAINTS-INFLUENCING VARS PS) takes a list of variables VARS and 
   a list of constraints PS and returns two values: the subset of PS influencing VARS, assuming
   PS is consistent, and the set of variables occurring in that subset."
  ; expand VARS to include all those dependent on VARS
  (let (vs)
    (setq vs (mapcar #'vars ps))
    (do ((changed t nil))
	((not changed))
      (dolist (v vs)
	(when (intersectionp v vars)
	  (setq vars (union v vars))
	  (setq vs (delete v vs))
	  (setq changed t))))
    ; grab all constraints with VARS
    (values (remove-if-not #'(lambda (x) (intersectionp vars (vars x))) ps)
	    vars)))

(defun ss-synth-partition (ps &key (indep nil) (test #'eq))
  "(SS-SYNTH-PARTITION PS INDEP) partitions PS so that only those constraints in each partition
   influence one another, using the partitioning of variables INDEP into dependency classes if available.
   Returns that partitioning but where each partition is maksanded, i.e. returns a list of sentences."
  (let (parts h part g f vs)
    ; hash constraints on individual variables.
    (setq h (make-hash-table :test test))
    (dolist (p ps)
      (dolist (v (vars p))
	(setf (gethash v h) (cons p (gethash v h)))))

    ; build dependency graph
    ; add edges from PS
    (setq g (make-agraph))
    (dolist (p ps)
      (setq vs (vars p))
      (setq f (first vs))
      (agraph-adjoin-noded f g :test test)
      (dolist (v (cdr vs))
	(agraph-adjoin-edged f v nil g :test test)))
    ; add edges from indep
    (dolist (vs indep)
      (setq f (first vs))
      (agraph-adjoin-noded f g :test test)
      (dolist (v (cdr vs))
	(agraph-adjoin-edged f v nil g :test test)))
    ; grab dependency info
    (setq indep (agraph-connected-components g :test test))

    ; walk over new dependencies and union all constraints
    (setq parts nil)
    (dolist (d indep)
      (setq part nil)
      (dolist (v d)
	(setq part (union part (gethash v h) :test #'ss-sentequal)))
      (push part parts))
    (mapcar #'maksand parts)))


    

(defun ss-synth-code (static dynamic prob &key (depth 0))
  (let (platofile extrafile staticvars dynamicvars cpcmd)
    (setq platofile (stringappend *ss-working-prefix* "wavs-platofile.js"))
    (setq extrafile (stringappend *ss-working-prefix* "wavs-extra.txt"))

    (setq staticvars (vars (ss-cleanse-more* static prob t)))
    (setq dynamicvars (vars (ss-cleanse-more* dynamic prob t)))
    ;(setq staticvars (set-difference staticvars dynamicvars))
    (setq staticvars (ss-uncleanse staticvars prob))
    (setq dynamicvars (ss-uncleanse dynamicvars prob))

    (write-any-file platofile (ss-synth-javascript static depth))
    (with-open-file (e extrafile :direction :output :if-does-not-exist :create :if-exists :supersede)
      (format e "~A~%~A~%" (ss-prob-stuburl prob) (ss-prob-name prob))
      (dolist (v staticvars)
	(format e "~A " (if (atom v) v (second v))))
      (format e "~%")
      (dolist (v dynamicvars)
	(format e "~A " (if (atom v) v (second v))))
      (format e "~%"))
    (setq cpcmd (mapcar #'(lambda (x) (format nil "; cp ~A ~A" x *ss-working-prefix*)) *ss-wavs-final*))
    (setq cpcmd (apply #'stringappend cpcmd))
    (exec-commandline "cd" (ss-abspath :jsintegrator "bin") "; java JSIntegrator" platofile extrafile cpcmd)))

#|
(defun ss-synth-split-constraints (tracefile prob)
  "(SS-SYNTH-PROCESS-FAILURESINK TRACEFILE PROB) analyzes the trace and splits into static and dynamic
   constraints.  Returns 2 values: the KIF static constraints and a list of (KIF linenumber) for each
   dynamic constraint."
  (declare (ignore prob))
  (let (result static dynamic)
    ; given a tracefile, the script returns staticfile dynfile1 linenum1 dynfile2 linenum2  ...
    ; The contents of staticfile is the KIF formulas for the static constraints
    ; The contents of dynfilej are the KIF formulas for the ith dynamic constraint
    ; linenumj dictates the line number in the tracefile where dynamic constraint i was located 
    (setq result (exec-commandline "cd" (ss-abspath :traceanalysis "") "; ./wavs-split-constraints.pl" tracefile))
    (setq result (split-string result '(#\Newline #\Space)))
    (handler-case (setq static (read-file (first result)))
      (condition () 
	(setq static 'false)
	(when *ss-debug* (ss-trace (format nil "Cannot read static constraint file ~A" (first result))))))
    (do ((rs (cdr result) (cddr rs)) (d))
	((null rs))
      (handler-case (setq d (read-file (first rs)))
	(condition () 
	  (setq d 'false)
	  (when *ss-debug* (ss-trace (format nil "Cannot read dynamic constraint file ~A " (car rs))))))
      (push (list d (second rs)) dynamic))
    (values static dynamic)))

(defun ss-synth-ajax-calls (x) x)

(defun ss-synth-serverstub (linenum tracefile)
  ; result is a single filename that includes the stub
  (exec-commandline "cd" (ss-abspath :traceanalysis "") "; ./wavs-server-stub.pl" linenum tracefile))
|#

; JavaScript code generation
(defun ss-synth-javascript (fserver depth)
  "(SS-SYNTH-JAVASCRIPT FSERVER) takes an fserver formula (or formulas) and returns a string
   comprised of JavaScript that computes errors.
   Assumes always evaluating over a completely filled out form.
   Additional JS support files available through *corejs*.  Need to replace
   cellvalue (a function that returns the *set* of values for a given cellname 
   as a string) if any constraint involves more than one field."
  (cond ((eq fserver 'true) "")
	((eq fserver 'false) "// Error: Constraints were contradictory.")
	(t
	 (let (th html)
	   (setq fserver (list2p fserver))
	   (setq fserver (csvs-cleanse fserver))
	   (setq th (fhlc2web-theory fserver :completep t :casesensitive t :allowconflicts t :debug nil :unique t))
	   (setq html (compile-websheet th))
	   (when (htmlform-errors html) 
	     (ss-log `(platoerrors ,(htmlform-errors html)))
	     (when *ss-debug*
	       (ss-trace (format nil "Errors discovered by Plato: ~A" (htmlform-errors html)) depth)))
	   (htmlform-javascript html)))))

(defun csvs-cleanse (fserver)
  (let (*ss-varmapping* *ss-vars*)
    ; fix php operators
    (setq fserver (ss-fix-php-ops fserver))
    ; fix php boolean connectives
    (setq fserver (ss-fix-boolean-funcs fserver))
    ; change non-kif boolean connectives into KIF boolean connectives
    (setq fserver (boolops2kif fserver))
    ; eliminate duplicates inside boolean ops
    (setq fserver (mapbool #'(lambda (x) (delete-duplicates x :test #'sentequal)) fserver))
    ; translate (var "myCaseSensitiveVar") to ?var and record mappings
    (setq *ss-varmapping* nil)
    (setq *ss-vars* (vars fserver))
    (setq fserver (ss-cleanse-varcases-aux fserver))
    ; tweak regular expressions
    (setq fserver (ss-fix-innotin fserver))
    ; miscellaneous hacks
    (setq fserver (ss-fix-misc fserver))
    ; Skipping: do type inference and cast objs in constraints to satisfy types.
    ; (ss-cleanse-typeinference prob)
    ; Skipping: assumes only WAPTEC internal language simplify where possible
    ; (setq fserver (ss-simplify fserver))
    ; turn functional sentences into proper relational sentences
    (setq fserver (csvs-funcs2relns fserver))
    ; translate a few relations
    (setq fserver (csvs-drop-sugar fserver))
    ; turn variables into monadics: (p ?x) becomes (=> (x ?x) (p ?x))
    (setq fserver (csvs-vars2monadics fserver))
    fserver))

(defun csvs-vars2monadics (p)
  (maksand (mapcar #'(lambda (x) (append (cons '=> (mapcar #'(lambda (v) (list (devariable v) v)) (vars x))) (list x)))
		   (clauses p))))

;  (let (prefix)
;    (setq p (and2list p))
;    (setq prefix )
;    (maksand (mapcar #'(lambda (x) (nconc (cons '=> prefix) (list x))) p))))

(defun csvs-drop-sugar (p)
  (mapatoms #'(lambda (x) 
		(cond ((atom x) x)
		      ((eq (car x) '<) (cons 'lt (cdr x)))
		      ((eq (car x) '>) (cons 'gt (cdr x)))
		      ((member (car x) '(=== ==)) (cons '= (cdr x)))
		      ((eq (car x) '<=) (cons 'lte (cdr x)))
		      ((eq (car x) '>=) (cons 'gte (cdr x)))
		      (t x)))
	    p))

(defun csvs-funcs2relns (p)
  (let ((builts (ws-builtins)) isfunc)
    (setq isfunc #'(lambda (x y) (and (eq x (pred-name y)) (isfunction (pred-parameter y)))))
    (mapatoms #'(lambda (q) (let (b)
			      (setq b (find (relation q) builts :test isfunc))
			      (if b `(= true (tobool ,q)) q)))
	      p)))
					    

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;  Whitebox NoTamper testing (WAPTEC)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defun ss-whitebox-summarize (logfile outdir)
  "(SS-WHITEBOX-SUMMARIZE LOGFILE HTMLFILE) takes a logfile (the input) and an html file name.
   Outputs a textual summary to the screen and dumps and HTML version to HTML file."
  (setq outdir (ss-path-endify outdir)) ; ensures ends with /
  (exec-commandline "mkdir" outdir)  
  (with-open-file (h (stringappend outdir "index.html") :direction :output :if-exists :supersede :if-does-not-exist :create)
    (format h "<html><head><title>WAPTEC Analysis Summary</title>~%")
    (format h "<style type='text/css'>
     table, th, td {border: 2px solid green; padding: 5px; }
     table {border-collapse: collapse; }
     th {background-color: green; color: white;}
     .formtitle {font-size: 45px; width: 100%; text-align: center;}
     .subtitle {font-size: 35px; width: 100%; text-align: center;}
     .tabletitle {font-size: 30px; text-align: left;}
     .tamperedvar {color: red; } 

     </style>")
    (format h "</head><body>~%")
    (let (cum)
      (dolist (g (read-file logfile))
	(cond ((eq (car g) 'fclient) ; new form starting
	       (setq cum nil)
	       (push g cum))
	      ((eq (car g) 'end-formula) ; form ending
	       (push g cum)
	       (ss-whitebox-summarize-form h (nreverse cum) outdir)
	       (format h "~&<hr width='100%' />~%")
	       (setq cum nil))
	      (t (push g cum))))
      ; in case log is incomplete
      (when cum (ss-whitebox-summarize-form h (nreverse cum) outdir)))
    (format h "</body></html>")))

(defun ss-whitebox-summarize-form (s l outdir)
  "(SS-WHITEBOX-SUMMARIZE-FORM L) takes a stream S and a list of sentences L of logfile data pertinent to a form.
   Writes textual summary to T and HTML summary to S.  Copies auxiliary html files to OUTDIR."
  (labels ((complexity (name)
	     (let (comp)
	       (setq comp (indexps name 'd))
	       (setq comp (mapcar #'second comp))
	       (cond (comp
		      (format t "~&~%~A: min ~A, max ~A, avg ~A, count ~A~%"
			      name (apply #'min comp) (apply #'max comp) (* 1.0 (/ (reduce #'+ comp) (length comp))) (length comp))
		      (format s "<tr><td>~A</td><td>min ~A, max ~A, avg ~A, count ~A</td></tr>~%"
			      name (apply #'min comp) (apply #'max comp) (* 1.0 (/ (reduce #'+ comp) (length comp))) (length comp)))
		     (t
		      (format t "~&~%~A: none found~%" name)
		      (format s "<tr><td>~A</td><td>none found</td></tr>~%" name)))))
	 (auxhtml (path outdir)
	   "Copies pathname to outdir and returns filename portion of pathname"
	   (let (fn)
	     (setq fn (pathname path))
	     (setq fn (stringappend (pathname-name fn) "." (pathname-type fn)))
	     (exec-commandline "cp" path (stringappend outdir fn))
	     fn))
	 (probfind (key d)
	   (third (find-if #'(lambda (x) (and (listp x) (eq (first x) 'prob) (eq (second x) key))) (contents d))))
	 (fixvars (thing tamperedvars varnames)
	   (let (p)
	     (setq p (make-ss-prob :varnames varnames))
	     (sublis (mapcar #'(lambda (x) (cons (first x) (varspel (first x) p tamperedvars))) varnames) thing)))
	 (htmlhostile* (s h tamperedvars varnames)
	    (format s "<td>")
	     (output-var-assign (second h) tamperedvars varnames)
	     (format s "</td><td>~A</td><td>~A</td>" (fifth h) (fixvars (seventh h) tamperedvars varnames)))
	 (htmlhostile (s h tamperedvars varnames)
	   (format s "<tr>")
	   (htmlhostile* s h tamperedvars varnames)
	   (format s "</tr>~%"))
	 (varspel (v p tampered)
	   (let (oldv)
	     (setq oldv v)
	     (setq v (ss-uncleanse v p))
	     (if (listp v)
		 (setq v (format nil "~A[~A]" (first v) (second v)))
		 (setq v (format nil "~A" v)))
	     (setq v (format nil "<span class='~A'>~A</span>" (if (member oldv tampered) "tamperedvar" "var") v))
	     v))
	 (output-var-assign (alist tamperedvars varnames)
	   (let (p)
	     (setq p (make-ss-prob :varnames varnames)) 
	     (dolist (a alist)
	       (format s "~A = ~S; " (varspel (car a) p tamperedvars) (cdr a))))))
    (let (black benign hostile hostiles fn fp now prob start end totalhostiles uniquehostiles val varnames)
      (define-theory 'd "" l)
      (setq prob (indexps 'prob 'd))

      (setq val (probfind 'name prob))
      (format t "~&SUMMARY FOR FORM: ~A~%" val)
      (format s "<div class='formtitle'>Form ~A</div>~%" val)
      ; time
      (format s "<p><div class='tabletitle'>WAPTEC Statistics</div><table>~%")
      (setq now (get-universal-time))
      (setq start (probfind 'starttime prob))
      (setq end (probfind 'endtime prob))
      (when start
	(cond (end 
	       (setq val (- end start))
	       (format t "~&~%TIME: ~A seconds~%" val)
	       (format s "<tr><td>Time</td><td>~A seconds</td></tr>~%" val))	      
	      (t
	       (setq val (- now start))
	       (format t "~&~%TIME: ~A seconds since started at ~A~%" val now)
	       (format s "<tr><td>Time</td><td>~A seconds since started at ~A</td></tr>~%" val now))))

      ; COMPLEXITIES
      (complexity 'fclient-complexity)
      (complexity 'fserver-complexity)
      (complexity 'fdb-complexity)	      
      (format s "</table>")
      
      ; BLACKBOX 
      (setq varnames (probfind 'varnames prob))
      (setq black (indexps 'blackbox 'd))
      (multiple-value-setq (benign hostile) (split #'(lambda (x) (eq (second x) 'benign)) black))
      ; grab first two benigns
      (setq benign (list (first benign) (second benign)))
      ; compute which hostiles were reported as legit
      ; compute true positives, false positives, and false negatives
      ; for now just print benign and hostiles
      (format t "~&~%BLACKBOX RESULTS~%")
      (format s "<p><div class='subtitle'>Blackbox Results</div>~%")
      ; (blackbox benign ,assign ,htmlfile ,(sinksucc sinks))
      (setq val (length (remove nil benign)))
      (format t "Benigns (~A found):~%" val)
      (format s "<p><div class='tabletitle'>Benigns (~A)</div><table border='1'>~%" val)
      (format s "<tr><th>Assignment</th><th>Link</th><th>Sink status</th></tr>~%")
      (dolist (x benign)
	(when x 
	  (format t "~S~%" x)
	  (format s "<tr><td>")
	  (output-var-assign (third x) nil varnames)
	  (format s "</td><td><a href='~A'>HTML</a></td><td>~A</td></tr>~%" (auxhtml (fourth x) outdir) (fifth x))))
      (format s "</table>~%")
      ; (blackbox hostile ,assign ,htmlfile ,(sinksucc sinks) rank)
      (setq val (length hostile))
      (format t "Hostiles (~A found):~%" val)
      (format s "<p><div class='tabletitle'>Hostiles (~A)</div><table border='1'>~%" val)
      (format s "<tr><th>Assignment</th><th>Link</th><th>Sink status</th><th>Rank</th></tr>~%")
      (dolist (x (sort hostile #'(lambda (x y) (if (and (numberp x) (numberp y)) (< x y) nil)) :key #'sixth))
	(when x
	  (format t "~S~%" x)
	  (format s "<tr><td>")
	  (output-var-assign (second (third x)) (vars (first (third x))) varnames)
	  (format s "<br><br>~S" (first (third x)))
	  (format s "</td><td><a href='~A'>HTML</a></td><td>~A</td><td>~A</td></tr>~%" 
		   (auxhtml (fourth x) outdir) (fifth x) (sixth x) )))
      (format s "</table>")
      
      ; WHITEBOX 
      (format t "~&~%WHITEBOX RESULTS~%")
      (format s "<p><div class='subtitle'>Whitebox Results</div>~%")
      ; hostiles: remove duplicates, group by disjunct of ~fclient, report num instances of each disjunct
      (setq hostiles (remove-if-not #'(lambda (x) (and (listp x) (eq (first x) 'hostile))) (indexps 'hostile 'd)))
      (setq totalhostiles (length hostiles))
      (setq hostiles (remove-duplicates hostiles :test #'ss-hostiles-equal))
      (setq uniquehostiles (length hostiles))
      (setq hostiles (group-by hostiles #'third :test #'ss-sentequal))
      ; for now just print
      ; (hostile ,varassign ,disj ,constraints ,(ss-sink-id succsink) ,(ss-sink-constraints succsink) 
      ;        ,(ss-sink-phi succsink) ,(ss-sink-vars succsink))      
      (format t "Hostiles for WB+DB (~A total, ~A unique, ~A classes):~%" totalhostiles uniquehostiles (length hostiles))
      (format s "<p><div class='tabletitle'>Hostiles for WB+DB (~A total, ~A unique, ~A classes)</div>~%" 
	      totalhostiles uniquehostiles (length hostiles))
      (format s "<table>")
      (mapc #'(lambda (x) (format t "~&~%o  -fclient disjunct ~S (~A instances)~%" (car x) (length (cdr x)))
		      (dolist (h (cdr x)) (format t "~S~%" h))) hostiles)
      (dolist (x hostiles)
	(format s "<tr><td>&#x00AC;F<sub>client</sub> disjunct ~S (~A instances)" (fixvars (car x) nil varnames) (length (cdr x)))
	(format s "<table>~%")
	(format s "<tr><th>Variable assignment</th><th>Sink ID</th><th>Sink expression</th></tr>~%")
	(dolist (h (cdr x)) (htmlhostile s h (vars (car x)) varnames))
	(format s "</table>")
	(format s "</td></tr>"))
      (format s "</table>")
	      
      ; FP
      (setq fp (indexps 'falsepositive 'd))
      ;(setq fp (mapcar #'third fp))
      (remove-duplicates fp :key #'third :test #'ss-hostiles-equal)
      ; for now just print
      (format t "~&~%False Positives for WB-DB (~A found):~%" (length fp))
      (format s "<p><div class='tabletitle'>False Positives for WB-DB (~A found)</div>~%" (length fp))
      (mapc #'(lambda (x) (format t "~S~%" x)) fp)
      (format s "<table>")
      (format s "<tr><th>Cause</th><th>Variable assignment</th></tr>~%")
      (dolist (x fp) 
	(format s "<tr><td>~A</td><td>" (second x))
	(output-var-assign (second (third x)) (vars (third (third x))) varnames)
	(format s "</td></tr>~%"))
      (format s "</table>")
	      
      ; FN
      (setq fn (indexps 'falsenegative 'd))
      (remove-duplicates fn :key #'third :test #'ss-hostiles-equal)
      ; for now just print
      (format t "~&~%False Negatives for WB-DB (~A found):~%" (length fn))
      (format s "<p><div class='tabletitle'>False Negatives for WB-DB (~A found)</div>~%" (length fn))
      (mapc #'(lambda (x) (format t "~S~%" x)) fn)
      (format s "<table>")
      (format s "<tr><th>Cause</th><th>Variable assignment</th><th>Sink ID</th><th>Sink expression</th></tr>~%")
      (dolist (x fn) 
	(format s "<tr><td>~A</td>" (second x))
	(htmlhostile* s (third x) (vars (third (third x))) varnames)
	(format s "</tr>"))
      (format s "</table>")

      ; FCLIENT
      (format s "<p><div class='subtitle'>F<sub>client</sub></div>~%")
      (format s "~S~%" (fixvars (probfind 'phi prob) nil varnames))
)))

(defun ss-commandline-sat (bl)
  (let ((*ss-debug* nil))
    (setf (cdr (last bl)) '((t . t)))
    (ss-sat *ss-dcpportal-fclient* bl)))

(defun white (&optional name &rest forms )
  (let (thehint thedbfile theunique)
    (dolist (v (white-tests))
      (setq thehint (third v))
      (unless thehint (setq thehint 'true))
      (setq thedbfile (fourth v))
      (setq theunique (fifth v))
      (if name
	  (when (eq (first v) name) (ss-whitebox (second v) :space thehint :unique theunique :required nil :forms forms :dbfile thedbfile))
	  (ss-whitebox (second v) :unique nil :required nil :dbfile thedbfile)))))

(defun ss-sentence-check (p)
  "(SS-SENTENCE-CHECK P) throws an error if can't translate P to internal format."
  (ss-whitebox-init nil)
  (ss-validate-vocab-internal (and2list (ss-cleanse-more p (make-ss-prob)))))

(defun ss-whitebox (url &key space (unique 'unknown) (required 'unknown) (stream t) (forms nil) (dbfile nil)
		    (outputfun #'ss-output-whitebox))
  "(SS-WHITEBOX URL) is the toplevel function for invoking the whitebox program analysis routine
   using the stringsolver.  It takes a URL as an argument."
  (let (start hostiles *ss-solve-count* (*ss-white-downloads* 0) fclientprobs url2 
	      (*ss-false-values* #'php-false) (*ss-solve-count* 0) (*ss-benign-history* nil))
    (unless space (setq space 'true))
    (load *app-init-location*)
    (ss-assert (stringp url) nil (format nil "SS-WHITEBOX expects a string; found ~A" url))
    (ss-whitebox-init url)
    ; compute fclient for each form at URL, removing those forms pointing to different URLs
    ;   by making sure fclient URL is included in the original URL (since orig may include extra params)
    (ss-whitebox-cleanup)  ; so users don't get confused about whether or not stuff was downloaded
    (exec-commandline "mv" *ss-whitebox-log* (stringappend *ss-whitebox-log* ".bak"))
    (setq fclientprobs nil)
    (when *ss-show-search* (format t "~&Pre-processing ~A~%" url))
    (dolist (p (ss-compute-fclients url :space space))
      (setf (ss-prob-dbfile p) dbfile)
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
  (ss-whitebox-summarize *ss-whitebox-log* (stringappend *ss-working-prefix* "waptec-results"))
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
		  (*ss-history-unique* nil) (*ss-global-unique* unique)
		  (*ss-node-count* 0) (*ss-solve-count* 0) (*ss-benign-count* 0))
    (ss-log `(fclient ,formula))
    (ss-log `(fclient-complexity ,(complexity (ss-prob-phi prob))))
    (ss-log `(prob name ,(ss-prob-name prob)))
    (ss-log `(prob phi ,(ss-prob-phi prob)))
    (ss-log `(prob space ,(ss-prob-space prob)))
    (ss-log `(prob types ,(hash2bl (ss-prob-types prob))))
    (ss-log `(prob unique ,(ss-prob-unique prob)))
    (ss-log `(prob required ,(ss-prob-required prob)))
    (ss-log `(prob metafields ,(ss-prob-metafields prob)))
    (ss-log `(prob varnames ,(ss-prob-varnames prob)))
    (ss-log `(prob starttime ,(get-universal-time)))    
    (when *ss-run-blackbox* (ss-whitebox-blackbox formula prob))
    (when *ss-run-whitebox*
      (when *ss-debug* (format t "~&~%***** WHITEBOX ANALYSIS *****~%~%"))
      (dolist (p (or2list (ss-fclient-dnf formula)))
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
	  (ss-trace (format nil "** Starting whitebox analysis for new disjunct for form ~A **" (ss-prob-name prob)) depth)
	  (pprint p))
	(when *ss-stop-now* (return))
	(ss-log `(newdisjunct ,p))
	(ss-log `(disjunct-starttime ,(get-universal-time)))
	(setq responses 
	      (ss-whitebox-search-server p prob searchprocessor :required required :unique unique :depth (1+ depth)))
	(when responses (push responses hostiles))
	(ss-log `(disjunct responses ,responses))
	(ss-log `(disjunct nodecount ,(- *ss-node-count* lastnodecount)))
	(ss-log `(disjunct stringsolvercount ,(- *ss-solve-count* lastsolvecount)))
	(ss-log `(disjunct benigncount ,(- *ss-benign-count* lastbenigncount)))
	(ss-log `(disjunct endtime ,(get-universal-time)))))
      ;(condition () (when (or *ss-debug* *ss-show-search*) (format t "Found error...continuing")))))
    (ss-log `(prob endtime ,(get-universal-time)))
    (ss-log `(end-formula ,formula))
    hostiles))

(defun ss-whitebox-blackbox (formula prob &key (required t) (unique nil) (depth 0))
  (flet ((sinksucc (sinks)
	   (cond ((null sinks) 'nosinks)
		 ((every #'ss-sink-success sinks) 'success)
		 ((every #'ss-sink-failure sinks) 'failure)
		 (t 'successfailure)))
	 (thro (bl)
	   (let (htmlfile tracefile sinks)
	     (multiple-value-setq (htmlfile tracefile) (ss-whitebox-server-html-trace prob bl :depth depth))
	     (when *ss-blackbox-analyze-sinks*
	       (setq sinks (ss-whitebox-trace-analysis-log tracefile prob :depth depth))
	       (setq sinks (delete-if #'ss-sink-unknown sinks))
	       (when *ss-debug* 
		 (ss-trace (format nil "** Found ~A sinks **" (length sinks)) depth)))
	     (values sinks htmlfile))))
    (let (assigns benigns hostiles sinks htmlfile benign1 benign2 rank)
      (when *ss-debug* (format t "~&~%***** BLACKBOX ANALYSIS *****~%~%"))
      ; invoke blackbox routines to generate variable assignments
      ;    notamper-blackbox returns:      ((good P bl1 ... bln) (bad (Pvariation1 bl1) ... (Pvariationm blm)))
      (setq assigns (notamper-blackbox formula prob :numgood 2 :required required :unique unique))
      (setq benigns (cddr (first assigns)))
      (setq hostiles (cdr (second assigns)))  ;(mapcar #'second (cdr (second assigns))))
      ; throw inputs at server and record results
      (when *ss-debug* (format t "~&Throwing variable assignments at server~%"))
      (ss-log `(blackbox-begin))
      (setq benign1 nil benign2 nil)
      (when *ss-debug* (format t "Benign "))
      (dolist (b benigns)
	(multiple-value-setq (sinks htmlfile) (thro b))
	(when (not benign1) (setq benign1 htmlfile))
	(when (and benign1 (not benign2)) (setq benign2 htmlfile))
	(ss-log `(blackbox benign ,b ,htmlfile ,(sinksucc sinks)))
	(when *ss-debug* (format t ".")))
      (when *ss-debug* (format t "~&Hostile "))
      (when (and benign1 (not benign2)) (setq benign2 benign1))
      (dolist (b hostiles)
	(multiple-value-setq (sinks htmlfile) (thro (second b)))
	(setq rank nil)
	(when benign1
	  (setq rank (read-user-string (exec-commandline "cd" (ss-abspath :rankgen "") ";" 
							 (ss-abspath :rankgen "domcomp") benign1 benign2 htmlfile))))
	(ss-log `(blackbox hostile ,b ,htmlfile ,(sinksucc sinks) ,rank ,(vars (first b))))
	(when *ss-debug* (format t ".")))
      (when *ss-debug* (format t "~%"))
      (ss-log `(blackbox-end)))))

(defun ss-whitebox-blackbox_old (formula prob &key (maxbenign 2) (required nil) (unique nil) (depth 0))
  (labels ((slog (type assign sinks htmlfile)
	     (ss-log `(blackbox ,type ,assign ,htmlfile ,(sinksucc sinks))))
	   (sinksucc (sinks)
	     (cond ((null sinks) 'nosinks)
		   ((every #'ss-sink-success sinks) 'success)
		   ((every #'ss-sink-failure sinks) 'failure)
		   (t 'successfailure))))
    (let (assign sinks htmlfile benigncnt)
       ; try to find at least two different solutions to fclient that the server accepts--log all results
    (when *ss-debug* (format t "~&~%***** BLACKBOX ANALYSIS *****~%~%"))
      (ss-log `(blackbox-begin))
      (unless (and (numberp maxbenign) (< maxbenign 1))
	(multiple-value-setq (sinks assign htmlfile) 
	  (ss-whitebox-server-benign* formula prob :required required :unique unique :depth (1+ depth)))
	(when (and assign (numberp maxbenign) (> maxbenign 1))
	  (when *ss-debug* (ss-trace "** Found benign #1 **"))
	  (slog 'benign assign sinks htmlfile)
	  (setq benigncnt 1)
	  (dolist (a assign)
	    (multiple-value-setq (sinks assign htmlfile)
	      (ss-whitebox-server-benign* (makand formula `(not (= ,(car a) ,(cdr a))))
					  prob :required required :unique unique :depth (1+ depth)))
	    (when assign
	      (slog 'benign assign sinks htmlfile)
	      (setq benigncnt (1+ benigncnt))
	      (when *ss-debug* (ss-trace (format nil "** Found benign #~A **" benigncnt)))
	      (when (and (numberp maxbenign) (>= benigncnt maxbenign)) (return))))))
 
      ; for each disjunct in the negation of fclient, report hostile attempts
      (when *ss-debug* (ss-trace "** Running blackbox hostile **"))
      (dolist (d (or2list (ss-notfclient-dnf (maknot formula))))
	(multiple-value-setq (sinks assign htmlfile) 
	  (ss-whitebox-server-benign* d prob :required required :unique unique :depth (1+ depth)))
	(when assign
	  (when *ss-debug* (ss-trace "Found hostile"))
	  (slog 'hostile assign sinks htmlfile)))

      (ss-log `(blackbox-end)))))

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

(defun ss-sink-priority (sink fclientvars)
  (if (set-difference (vars (ss-sink-constraints sink)) fclientvars) 0 1))

(defun ss-whitebox-server-benign-perturb-hostile (p prob &key (unsats nil) (required nil) (unique nil) (depth 0))
  ; check if p leads to success.  if so, find all constraints w new vars, see if they lead to successes.
  ;  For all successes, find all hostiles.
  (let (sinks hostiles fclientvars allhostiles (succcons nil) failcons)
    ; find success sink, grab all success/failure constraints, 
    (setq sinks (ss-whitebox-server-benign* p prob :unsats unsats :required required :unique unique :depth depth))
    (dolist (s sinks)
      (when (ss-sink-success s) (setq succcons (union (ss-sink-constraints s) succcons :test #'ss-sentequal)))
      (when (ss-sink-failure s) (setq failcons (append (ss-sink-constraints s) failcons))))

    ; find new sinks by perturbing those constraints
    (setq fclientvars (vars (ss-prob-phi prob)))
    (setq succcons (remove-if #'(lambda (x) (subsetp (vars x) fclientvars)) succcons))
    (dolist (n succcons)
      (when *ss-show-search* (format t "~&Perturbing constraint~%~S~%~%" n))
      (when *ss-debug* (ss-trace (format nil "** Perturbing success sink constraint **~%~S" n) (1+ depth)))
      (setq sinks (union sinks (ss-whitebox-server-benign* (makand p (maknot n)) prob :unsats unsats 
							   :required required :unique unique :depth (1+ depth))			 
			 :test #'ss-sinkequal)))
    (setq sinks (sort (nreverse sinks) #'< :key #'(lambda (x) (ss-sink-priority x fclientvars))))

    (when *ss-debug* (ss-trace (format nil "** Processing benign sinks **~%~S" sinks) depth))
    ; find hostiles
    (setq allhostiles nil) ; accummulator
    (dolist (s sinks)
      (when (ss-sink-success s)
	(when *ss-debug* (ss-trace (format nil "** Processing benign sink **~%~S" s) depth))
	(setq *ss-benign-count* (1+ *ss-benign-count*))
	(push s *ss-benign-history*)
	(setq hostiles (ss-whitebox-construct-hostile s (ss-prob-phi prob) prob
						      :tamperable t :depth (1+ depth)))
	(setq allhostiles (append hostiles allhostiles))))

    ; return hostiles together with failure constraints
    (values allhostiles failcons)))

; version where we don't increase *ss-benign-count*
(defun ss-whitebox-server-benign-hostile* (p prob &key (unsats nil) (required nil) (unique nil) (depth 0))
  (let ((*ss-benign-count* *ss-benign-count*) (*ss-benign-history* *ss-benign-history*))
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
	     (push s *ss-benign-history*)
	     (setq hostiles (ss-whitebox-construct-hostile s (ss-prob-phi prob) prob  ; CHECK THAT HANDING S IS RIGHT
							   :tamperable t :depth (1+ depth)))
	     (when hostiles
	       (ss-log `(vulnerability ,p ,(ss-sink-constraints s)))
	       (push hostiles allhostiles)))
	    (t (setq constraints (append (ss-sink-constraints s) (ss-sink-dbconstraints s) constraints)))))
    (values allhostiles constraints)))

; version where we don't increase *ss-benign-count*
(defun ss-whitebox-server-benign* (p prob &key (unsats nil) (required nil) (unique nil) (depth 0))
  (let ((*ss-benign-count* *ss-benign-count*) (*ss-benign-history* *ss-benign-history*))
    (ss-whitebox-server-benign p prob :unsats unsats :required required :unique unique :depth depth)))

(defun ss-whitebox-server-benign (p prob &key (unsats nil) (required nil) (unique nil) (depth 0))
  "(SS-WHITEBOX-SERVER-BENIGN P PROB) checks if P represents a class of inputs
   that the server accepts.  May modify *ss-cast-types*.  
   Returns (i) a list of success/failure sinks and (ii) solution to p used to generate sinks and
   (iii) the file containing the HTML for the resulting page.
   Destructively modifies UNSATS, a list containing a single element, which is a list of unsatisfiable
   sentences."
  ;(when *ss-debug* (ss-trace "Looking for benign." depth) (pprint p))
  (let (bl tracefile htmlfile sinks)
    (when *ss-debug* (ss-trace (format nil "** Attempting benign **~%~S" p) depth))
    (setq bl (ss-whitebox-solve p prob :required required :unique unique :depth depth))
    (cond ((not (eq bl :unsat))
	   (when *ss-debug* (ss-trace (format nil "** Found solution **~%~S" bl) depth))
	   (when *ss-show-search* (format t "~S~%~%" bl))
	   (multiple-value-setq (htmlfile tracefile) (ss-whitebox-server-html-trace prob bl :depth depth))
	   (setq sinks (ss-whitebox-trace-analysis-log tracefile prob :depth depth))
	   (setq sinks (delete-if #'ss-sink-unknown sinks))
	   (setq sinks (ss-whitebox-server-benign-process-history sinks))  ; eliminates some of the sinks, adds remainder to history
	   (when *ss-debug* 
	     (ss-trace (format nil "** Found ~A success and ~A failure sinks **" 
			       (count-if #'ss-sink-success sinks) (count-if #'ss-sink-failure sinks)) depth)
	     (ss-trace (format nil "~A" sinks) depth))
	   (when (some #'ss-sink-success sinks) (ss-log `(success-sinks ,bl ,tracefile ,p)))
	   (dolist (s sinks)
	     (when (ss-sink-success s)
	       (setq *ss-benign-count* (1+ *ss-benign-count*))
	       (push s *ss-benign-history*)
	       (ss-log `(success-sink ,(ss-sink-id s) ,(ss-sink-constraints s) ,(ss-sink-phi s) ,(ss-sink-vars s)))))
	   (values sinks bl htmlfile))
	  (t
	   (when unsats
	     (setf (first unsats) (cons p (first unsats))))
	   (values nil nil nil)))))

(defun ss-whitebox-server-benign-process-history (sinks)
  (when *ss-delete-duplicate-benign-sinks*
    (setq sinks
	  (remove-if #'(lambda (x) (and (ss-sink-success x) (some #'(lambda (y) (ss-sinkequal x y)) *ss-benign-history*)))
		     sinks)))
  sinks)

(defun ss-sinkequal (sink1 sink2)
  (and (ss-sink-p sink1) 
       (ss-sink-p sink2)
       (eq (ss-sink-id sink1) (ss-sink-id sink2))
       (ss-sentequal (maksand (ss-sink-constraints sink1))
		     (maksand (ss-sink-constraints sink2)))
       (ss-sentequal (maksand (ss-sink-dbconstraints sink1))
		     (maksand (ss-sink-dbconstraints sink2)))))

(defun ss-whitebox-construct-hostile (sink fclient prob &key (tamperable t) (required nil) (unique nil) (depth 0))
  "(SS-WHITEBOX-CONSTRUCT-HOSTILE SINK FCLIENT PROB) assuming that there is a solution to 
   FCLIENT ^ (ss-sink-constraints SINK) that hits a success sink at the server, construct a solution to
   ~FCLIENT  ^ (ss-sink-constraints SINK) and test if it too hits a success sink.  TAMPERABLE is a list of variables
   we are allowed to tamper with."
  (let (hostile successp hostiles constraints allconstraints hiddenvaronly)
    (setq constraints (ss-subsumption-elim (ss-sink-constraints sink)))
    (setq allconstraints (ss-subsumption-elim (append constraints (ss-sink-dbconstraints sink))))
    ; ask each DNF(~fclient) separately so we know which variables have been tampered with
    (setq hostiles nil)
    (setq hiddenvaronly (null (intersectionp (vars fclient) (ss-sink-vars sink))))
    (dolist (d (or2list (ss-notfclient-dnf (maknot fclient))) hostiles)
      (when (or hiddenvaronly (not *ss-only-sink-relevant-hostiles*) (intersectionp (vars d) (ss-sink-vars sink)))  ; only pertinent hostiles
	(when *ss-debug* (ss-trace (format nil "** Attempting hostile **~%~S" d) depth))
	(when *ss-stop-now* (return hostiles))
	(when (or (eq tamperable t) (subsetp (vars d) tamperable))
	  ;(when *ss-debug* (ss-trace (format nil "Looking for hostile along success path.") (1+ depth)))
	  ;(when *ss-debug* (unless *quiet* (ss-trace (with-output-to-string (s) (pprint d s)) (1+ depth))))

	  ; try without db constraints
	  (multiple-value-setq (hostile successp) 
	    (ss-whitebox-construct-hostile-process d constraints prob sink hiddenvaronly
						   :required required :unique unique :depth (1+ depth)))
	  ; when found some solution
	  (when hostile
	    ; if failure, try with db constraints
	    (unless successp
	      (when (member (relation (ss-sink-phi sink)) *ss-db-queries*)
		(ss-log `(falsepositive nodbreturnval ,hostile))
		(when *ss-debug* (ss-trace "** False positive **" depth))
		(multiple-value-setq (hostile successp) 
		  (ss-whitebox-construct-hostile-process d allconstraints prob sink hiddenvaronly
							 :required required :unique unique :depth (1+ depth)))
		(when successp
		  (ss-log `(falsenegative nodbconstraints ,hostile))
		  (when *ss-debug* (ss-trace "** False negative **" depth))))))

	  ; when success, add to hostile list
	  (when (and hostile successp) (push hostile hostiles)))))))

(defun ss-whitebox-construct-hostile-process (d constraints prob sink hiddenvaronly &key (required nil) (unique nil) (depth 0))
  (let (hostile tracefile sinks succsink)
    (setq hostile (ss-whitebox-solve (makand d (maksand constraints)) prob 
				     :required required :unique unique :depth (1+ depth)))
    (cond ((not (eq hostile :unsat))
	   (setq tracefile (ss-whitebox-testserver prob hostile :depth (1+ depth)))
	   (setq sinks (ss-whitebox-trace-analysis-log tracefile prob :depth (1+ depth)))
	   (setq succsink (ss-whitebox-hostile-success-sinkp sinks d sink hiddenvaronly))
	   (cond (succsink 
		  (when *ss-show-search* (format t "~&Hostile attempt succeeded:~%~S~%~A~%~S~%~%" d tracefile hostile))
		  (when *ss-debug* (ss-trace (format nil "** Hostile found **~%~S~%~A~%~S" hostile tracefile d) depth))
		  (setq hostile `(hostile ,hostile ,d ,constraints ,(ss-sink-id succsink) ,(ss-sink-constraints succsink) 
				    ,(ss-sink-phi succsink) ,(ss-sink-vars succsink)))
		  (ss-log hostile)
		  (values hostile t))
		 (t (when *ss-debug* 
		      (ss-trace (format nil "** Hostile attempt failed **~%server rejection~%~S~%~A~%~S" 
					hostile tracefile d) depth))
		    (values `(hostile ,hostile ,d ,constraints) nil))))
	  (t (when *ss-debug* 
	       (ss-trace (format nil "** Hostile attempt failed: no string solution **~%~A~%~S" tracefile d) depth))
	     (values nil nil)))))

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


(defun ss-whitebox-hostile-success-sinkp (sinks fclientdisj benignsink hiddenvaronly)
  "(SS-WHITEBOX-HOSTILE-SUCCESS-SINKP SINKS FCLIENTDISJ BENIGNSINK) finds the sink in SINKS (if it exists)
   that witnesses a successful hostile attack for a solution to the negated fclient disjunct FCLIENTDISJ
   wrt the benign sink BENIGNSINK."
  ; for now, if there is a sink that depends on some of the variables in fclientdisj, we call it a success
  (declare (ignore benignsink))
  (cond (hiddenvaronly (find-if #'ss-sink-success sinks))
	(t
	 (setq fclientdisj (vars fclientdisj))
	 (find-if #'(lambda (x) (and (ss-sink-success x) (intersectionp (ss-sink-vars x) fclientdisj))) sinks))))

 
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

(defun ss-notfclient-dnf (p) (dnf p))
(defun ss-fclient-dnf (p)
  (if (> (ss-approx-dnf-size (nnf p)) 10)
      (ss-approx-dnf p)
      (dnf p)))

(defun ss-approx-dnf-size (p)
  "(SS-APPROX-DNF-SIZE P) computes the worst-case number of disjuncts in the DNF of 
   the formula P, assuming P is in NNF, employs only AND/OR/NOT connectives, and
   contains no quantifiers."
  (cond ((atom p) 1)
	((eq (car p) 'and) (reduce #'* (mapcar #'ss-approx-dnf-size (cdr p))))
	((eq (car p) 'or) (reduce #'+ (mapcar #'ss-approx-dnf-size (cdr p))))
	(t 1)))

(defun ss-approx-dnf (p)
  ; replace the flat, equality disjuncts (or (= ?x a) (= ?x b) ...) with a single value.
  ; then compute dnf
  (flet ((orp (x) (and (listp x) (eq (car x) 'or)))
	 (flateq (list)
	   (let (v l var val)
	     (setq v nil l nil)
	     (dolist (x list (cons v l))
	       (unless (and (listp x) (eq (car x) '=)) (return nil))
	       ; grab var and val
	       (cond ((and (varp (second x)) (and (atom (third x)) (not (varp (third x)))))
		      (setq var (second x))
		      (setq val (third x)))
		     ((and (varp (third x)) (and (atom (second x)) (not (varp (second x)))))
		      (setq var (third x))
		      (setq val (second x)))
		     (t (return nil)))
	       ; check if same var as all previous
	       (cond ((not v) (setq v var) (setq l val))
		     ((not (eq v var)) (return nil)))))))		     
  (let (bl rest b)
    ; for each flat equality disjunction, pick a binding for that disjunction and apply it to the remaining sentences.
    (dolist (q (and2list (flatten-operator (nnf p))))
      (cond ((orp q)
	     (setq b (flateq (or2list q)))
	     (if b
		 (push b bl)
		 (push q rest)))
	    (t
	     (push q rest))))
   ; (when bl (setq rest (sublis bl rest)))
    (setq rest (nconc (mapcar #'(lambda (x) `(= ,(car x) ,(cdr x))) bl) rest))
;    (pprint (flatten-operator (maksand rest))))))
    (dnf (maksand rest)))))


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

(defun ss-whitebox-trace-analysis-log (tracefile prob &key (depth 0))
  (mapc #'ss-log-sink (ss-whitebox-trace-analysis tracefile prob :depth depth)))

(defun ss-whitebox-trace-analysis (tracefile prob &key (depth 0) (drop-nonmethod nil) (save-prettyname t))
  "(SS-WHITEBOX-TRACE-ANALYSIS TRACEFILE PROB) extracts sinks from tracefile and cleans them.  Ignores
   sinks with status 'unknown'."
  ;(declare (ignore depth))
  (mapcar #'(lambda (x) (ss-clean-sink x prob :depth depth :drop-nonmethod drop-nonmethod 
						:save-prettyname save-prettyname))
	  (ss-trace-analysis-extract tracefile prob :depth depth)))

(defun ss-log-sink (sink)
  (ss-log `(fserver ,(maksand (ss-sink-constraints sink))))
  (ss-log `(fserver-complexity ,(complexity (ss-sink-constraints sink))))
  (ss-log `(fdb ,(ss-sink-dbconstraints sink)))
  (ss-log `(fdb-complexity ,(complexity (ss-sink-dbconstraints sink))))
  (ss-log `(fstatic ,(ss-sink-staticconstraints sink)))
  (ss-log `(fstatic-complexity ,(complexity (ss-sink-staticconstraints sink))))
  (ss-log `(fdynamic ,(ss-sink-dynamicconstraints sink)))
  (ss-log `(fdynamic-complexity ,(complexity (ss-sink-dynamicconstraints sink))))
  sink)

(defun ss-drop-nonmethod (constraints prob)
  "(SS-DROP-NONMETHOD CONSTRAINTS PROB) returns the subset of the raw PHP CONSTRAINTS
   where all variables are of the form (method \"blah\") where METHOD is the 
   form's method according to PROB."
  (flet ((nonmethodvar (x types method) (and (listp x) (member (car x) types) (not (eq (car x) method))))) 
    (let (method vartypes)
      (setq method (tosymbol (second (assoc 'method (ss-prob-metafields prob)))))
      (setq vartypes *ss-vartypes*)
      (remove-if #'(lambda (x) (someterm #'(lambda (y) (nonmethodvar y vartypes method)) x))
		 constraints))))

(defun ss-clean-sink (sink prob &key (depth 0) (drop-nonmethod nil) (save-prettyname nil))
  (declare (ignore depth))
  (labels ((clean (x)
	   (let ((newx (ss-cleanse-more* x prob)))
	     (cond ((ss-istrue newx) 'true)
		   (t (when save-prettyname (ss-set-prettyname x newx)) newx))))
	 (handleerr (e c)
	   (when *ss-debug* (ss-trace (format nil "~A; dropping constraint: ~S" (text e) c))))
	 (cleanc (raw)
	   (let (constraints final)
	     (setq raw (delete-duplicates raw :test #'equal))
	     (when drop-nonmethod (setq raw (ss-drop-nonmethod raw prob)))
	     (setq constraints (delete 'true (mapcar #'clean raw)))
	     (setq final nil)
	     (dolist (c constraints (nreverse final))
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
		 (condition () (when *ss-debug* (ss-trace (format nil "Unknown error; dropping constraint ~S" c)))))))))
    	(unless (eq (ss-sink-status sink)  'unknown)  ; cleanse unless we're going to ignore anyway
          ; cleanse sinkvars (note we call ss-cleanse-more* individually so that ((post "x")) does not become ?x)
	  (setf (ss-sink-vars sink) (mapcar #'(lambda (x) (ss-cleanse-more* x prob t)) (ss-sink-vars sink)))
          ; cleanse sink
	  (setf (ss-sink-phi sink)  (ss-cleanse-more* (ss-sink-phi sink) prob)) 
          ; cleanse constraints
	  (setf (ss-sink-constraints sink) (cleanc (ss-sink-constraints sink)))
	  (setf (ss-sink-dbconstraints sink) (cleanc (ss-sink-dbconstraints sink)))
	  (setf (ss-sink-dynamicconstraints sink) (cleanc (ss-sink-dynamicconstraints sink)))
	  (setf (ss-sink-staticconstraints sink) (cleanc (ss-sink-staticconstraints sink))))
	sink))

(defun ss-trace-analysis-extract (tracefile prob &key (depth 0))
  (flet ((grabconstraints (v)
	   (if (atom v) v (if (atom (first v)) (and2list (nnf v)) v))))
    (let (tracefilec sinks result lines)
      (when *ss-debug* (ss-trace (format nil "** Analyzing trace: ~A **" tracefile) depth))
      (setq tracefilec (stringappend tracefile ".constraints"))
      (exec-commandline "touch" tracefilec)
      (setq result (exec-commandline "cd" (ss-abspath :traceanalysis "") "; ./run-trace-da.pl" tracefile (ss-prob-dbfile prob)))
      (write-any-file tracefilec result)
      (setq sinks nil)
      (handler-case (setq lines (read-lines* result))
	(condition () (when *ss-debug* (ss-trace (format nil "ERROR reading trace analysis: ~%~S~%" result)))))
      (dolist (s lines)
	(handler-case (push (eval s) sinks)
	  (condition () (when *ss-debug* (ss-trace (format nil "Error evaluating sink: ~A;" s) depth)))))

;	(push (make-ss-sink :id (first s) :status (second s) :constraints (grabconstraints (third s)) 
;			    :phi (fourth s) :vars (fifth s) :dbconstraints (grabconstraints (sixth s)) 
;			    :staticconstraints (grabconstraints (seventh s))
;			    :dynamicconstraints (grabconstraints (eighth s)))
;	      sinks))
      sinks)))

#|
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
      (exec-commandline *ss-java-invoke* "-Xms2048m -Xmx2048m edu.uic.rites.sisl.formula.ServerFormulaNazari"
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

|#

(defun ss-whitebox-trace-wpk (tracefile p &key (depth 0))
  (declare (ignore tracefile p depth))
  'true)

  
(defun ss-whitebox-testserver (prob bl &key (depth 0)) (ss-whitebox-server-trace prob bl :depth depth))
(defun ss-whitebox-server-trace (prob bl &key (depth 0))
  (ss-whitebox-drop-html (ss-whitebox-server-submit prob bl :depth depth)))
(defun ss-whitebox-server-html (prob bl &key (depth 0))
  (ss-whitebox-drop-trace (ss-whitebox-server-submit prob bl :depth depth)))
(defun ss-whitebox-server-html-trace (prob bl &key (depth 0))
  (let (both html trace)
    (setq both (ss-whitebox-server-submit prob bl :depth depth))
    (setq html (ss-whitebox-drop-trace both))
    (setq trace (ss-whitebox-drop-html both))
    (values html trace)))

(defun ss-whitebox-server-submit (prob bl &key (depth 0))
  "(SS-WHITEBOX-TESTSERVER PROB BL) throws BL at the server and returns the name of a file containing the result."
  (let (blfile)
    ; adjust BL so that vars supposed to be unique are assigned unique values via gentemp.
    ;(format t "Old binding list: ~S~%" bl)
    (setq bl (mapcar #'(lambda (x) (if (member (car x) *ss-global-unique*) (cons (car x) (gentemp (tostring (cdr x)))) x)) bl))
    ;(format t "New binding list: ~S~%" bl)
    ; write out binding list in XML
    (setq blfile (stringappend *ss-working-prefix* "bl" (tostring *ss-white-downloads*) ".xml"))
    (with-open-file (f blfile :direction :output :if-does-not-exist :create :if-exists :supersede)      
      (ss-bl2xml (ss-whitebox-prep-bl bl prob) prob :stream f))
    ; record that we're giving these inputs to the server
    (setq *ss-history-unique* (nconc (remove-if-not #'(lambda (x) (member (car x) *ss-global-unique*)) bl)
				    *ss-history-unique*))
    ;(format t "*ss-history-unique* (updated in server-submit): ~S~%" *ss-history-unique*)
    ; invoke Nazari's code to send inputs to server
    (ss-whitebox-get-response blfile :depth depth)))

(defun ss-whitebox-prep-bl (bl prob) (ss-uncleanse (mapcar #'(lambda (x) (list (car x) (cdr x))) bl) prob))

(defun ss-whitebox-get-response (xmlfile &key (dest (stringappend *ss-working-prefix* "homepage.htm")) (url nil) (depth 0)) 
  "(SS-WHITEBOX-GET-RESPONSE XMLFILE) invokes the response generator on the given XMLFILE 
   and returns the file storing the result.  Not using wget b/c response generator 
   handles weird javascript redirects.  Also, response generator allows for cookies."
  (let (start)
    (setq dest (stringappend dest "." *ss-white-downloads*))
    (setq *ss-white-downloads* (1+ *ss-white-downloads*))
    (setq start (get-universal-time))
    (when (and *ss-debug* url)
      (ss-trace (format nil "** Downloading ~A to ~A at time ~A**" url dest start) depth))
    ; note that the -d flag will only send the first CASE of inputs in the XML file.
    ; to submit multiple CASEs, but skip the rank generator, use the -s flag
    (exec-commandline "cd" (ss-abspath :respgen "") 
		      ;"; rm -Rf responses results diffsizes.txt; mkdir responses; mkdir results;"
		      ";" *ss-java-invoke* "-jar HTTPReqGen.jar" xmlfile "-d" dest)
    (when (and *ss-debug* url)
      (ss-trace (format nil "** Downloading finished in ~A seconds **" (- (get-universal-time) start))))
    dest))

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
    (setq openstr "<WAPTEC_TRACE_START>")
    (setq newfile (stringappend file ".notrace"))
    (exec-commandline (format nil "awk '{if (match($0,\"~A\")) exit; print}' ~A > ~A" openstr file newfile))
    newfile))

(defun ss-whitebox-drop-html (file)
  "(SS-WHITEBOX-PROCESS-SERVER FILE) processes the server response contained in FILE to extract the
   appropriate trace and store it in a new file, whose name is returned."
  (let (newfile)
    (setq newfile (stringappend file ".trace"))
    (exec-commandline "awk '/WAPTEC_TRACE_START/,/WAPTEC_TRACE_END/'" file "| sed '1d;$d'" ">" newfile)
    newfile))
  
(defun ss-whitebox-solve (p prob &key (required nil) (unique nil) (depth 0))
  "(SS-WHITEBOX-SOLVE P ...) runs stringsolver on P and returns result"
  (let (val u (*ss-whitebox-log* *ss-whitebox-log*) blokay)
    ; make sure to reduce to internal vocab
    ;(setq p (car (ss-validate-vocab-internal (list (ss-cleanse-more (ss-drop-syntactic-sugar p) prob)))))

    ; create unique constraints
    (setq unique (union unique *ss-global-unique*))
    (setq u nil)
    ; may not work b/c of bug in Kaluza
    (when *ss-obey-unique*
      (setq u (remove-if-not #'(lambda (x) (member (car x) unique)) *ss-history-unique*))
      (setq u (mapcar #'(lambda (x) `(!= ,(car x) ,(cdr x))) u)))

    ; create sentence
    (setq p (maksand (list* p (ss-prob-space prob) u)))

    (when *ss-debug* 
      (ss-trace (format nil "** String solving **~%~S" p) depth))
;      (unless *quiet* (pprint p)))

    ;(setq p (ss-drop-syntactic-sugar p))
    (setq val (ss-solve p :required required :types (ss-prob-types prob)))
    (unless (eq val :unsat)
      (setq val (hash2bl val))
      (when *ss-debug* 
	     ;(ss-trace (format nil "Found string solution.") depth)
	     ;(unless *quiet* (ss-trace (format nil "~S" val) depth))
	(setq p (car (ss-validate-vocab-internal (list (ss-cleanse-more (ss-drop-syntactic-sugar p) prob)))))
	(setq blokay (ss-checksat p (ss-prob-types prob) (append val truth)))
	(unless blokay
	  (if *break-on-external-solver-error*
	      (ss-error (format nil "ss-solve failed on ~%~S~%producing ~%~S~%with types~%~S~%" 
				p val (hash2bl (ss-prob-types prob)))) 
	      (format t "ss-solve failed on ~%~S~%producing ~%~S~%with types~%~S~%" 
		      p val (hash2bl (ss-prob-types prob)))))))
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

(defun ss-compute-fclients (url &key space (depth 0))
  "(SS-COMPUTE-FCLIENTS URL) takes a URL and returns a list of ss-prob structs:
   one for each form at the URL.  Exactly same as blackbox version."
  (let (fclients prob)
    (unless space (setq space 'true))
    (exec-commandline "rm" (stringappend *ss-working-prefix* "F*"))
    (ss-extract-ht-constraints (ss-get-html url :depth depth) url :depth depth)
    (ss-extract-js-constraints :depth depth)
    (setq fclients nil)
    (dolist (f (ss-combine-constraints :depth depth))
      (handler-case 
	  (progn
	    (setq prob (eval (first (read-file f))))
	    (setf (ss-prob-space prob) (makand (ss-prob-space prob) space))
	    (ss-cleanse prob)
	    (push prob fclients))
	(condition () (when *ss-debug* (ss-trace (format nil "Error reading fclient file: ~A; progress made: ~S" f prob) depth)))))
    (nreverse fclients)))
   
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
#|
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
  (multiple-value-prog1 (notamper-blackbox p :numgood numgood :unique unique :required required :types types :space space)
    (ss-hampi-kill)))
|#

(defun notamper-blackbox (p prob &key (numgood 1) (unique nil) (required nil))
  "(NOTAMPER-BLACKBOX P NUMGOOD UNIQUE) 
   takes a KIF formula P (in the notamper constraint language defined below)
   and generates NUMGOOD distinct variable assignments for P and multiple variable assignments
   satisfying (NOT P).  Notamper returns a list of two elements 
         ((good P bl1 ... bln) (bad (Pvariation1 bl1) ... (Pvariationm blm))).  
   Each bli is a binding list, i.e. a list of (var val).
   For all variables included in the UNIQUE list, the values assigned to those variables
   are unique across good and bad.
   The REQUIRED list is the set of variables required to have values.
   The SPACE argument is a set of constraints that all variable assignments must satisfy.
   The TYPES argument is a hash of var to IN statements."
  (let ((history nil) bl (good nil) (bad nil) firstgood orlist tmp (*ss-false-values* #'php-false))
    (ss-assert (and (integerp numgood) (>= numgood 0)) nil 
	    "NOTAMPER requires NUMGOOD to be a non-negative integer.")

    ; compute required and unique if set to T
    (when (and (atom required) required) (setq required (ss-guess-required p (mapcar #'cdr (hash2bl (ss-prob-types prob))))))
    (when (and (atom unique) unique) (setq unique (ss-guess-unique p required)))

    ; compute NUMGOOD unique good answers, distributed across DNF pieces of P
    (unless *quiet* (format t "~&~%~%**** Generating Good Answers ****~%~%"))
    (setq firstgood nil)
    (setq orlist (or2list (ss-fclient-dnf p)))
    (do ((i 0))
	((or (>= i numgood) (null orlist)))
      (dolist (d orlist)
	(when (>= i numgood) (return))
	(when *ss-debug* 
	  (format t "~&**Solving ~S (~A of ~A good)~%" p (1+ i) numgood))
	(setq bl (notamper-solve-good d prob required history))
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
      (when *ss-debug* (format t "~&~%**** No good answers found.  Aborting.****~%~%"))
      (return-from notamper-blackbox nil))

    ; compute one bad answer for each disjunct in DNF
    (when *ss-debug* (format t "~&~%~%**** Generating Bad Answers ****~%~%"))
    (dolist (v (or2list (ss-notfclient-dnf (maknot p))))
      (when *ss-debug* (format t "~&**Solving ~S~%" v))
      (setq bl (notamper-solve-bad v prob required history p firstgood))
      (when *ss-debug* (format t "~&  Solution: ~S~%" bl))
      (when bl (push (list v bl) bad))
      (setq history (notamper-newhist history bl unique)))
    (setq bad (remove-duplicates bad :test #'blequal :key #'second))

    ; check if an element of good is an element of bad. If so, warn and then throw out. 
    (setq tmp nil)
    (dolist (b bad)
      (when (member (second b) good :test #'blequal)
	(when *ss-debug*
	  (format t "~&!! Warning: bad solution also a good solution.  Throwing out bad. !!~%")
	  (format t "~S~%" b))
	(push b tmp)))
    (setq bad (delete-if #'(lambda (x) (member x tmp :test #'equal)) bad))

    ; return two lists
    (list (list* 'good p (nreverse good))
	  (cons 'bad (nreverse bad)))))

(defun notamper-solve-good (p prob required history)
  (let (bl)
    (setq p (notamper-addhist p history (union (vars p) required)))
    ; asking for solution including all vars in P plus required so we can checksat
    (if required
	(setq bl (ss-whitebox-solve p prob :required required))
	(setq bl (ss-whitebox-solve p prob)))
    (when (eq bl :unsat) (setq bl nil))
    (when bl
      (unless *quiet*
	(unless (ss-checksat p (ss-prob-types prob) (append (hash2bl bl) '((t . t))))
	  (format t "In notamper-solve-good, ss-solve failed on ~%~S~%producing ~S" p (hash2bl bl))))
      (setq bl (hash2list bl)))
    bl))

(defun notamper-solve-bad (p prob required history origp goodbl)
  "(NOTAMPER-SOLVE-BAD P HISTORY REQUIRED TYPES SPACE ORIGP GOODBL) finds a solution to
   (and P SPACE) with at least REQUIRED variables having assignments.  TYPES includes
   the type for each variable; HISTORY includes the history of assignments up
   to this point for those variables that must be unique.  ORIGP is a sentence such that
   P implies (not ORIGP).  GOODBL includes a binding for all required, non-unique variables
   that jointly satisfy ORIGP.  "
  (let (bl unassigned vs p2 bl2)
    (setq vs (union (vars p) (vars (ss-prob-space prob))))   ; if space is disjunctive, may be problematic
    (setq p2 (notamper-addhist p history vs))
    (setq bl (ss-whitebox-solve p2 prob)) ; :required vs)) ; required are just those in p
    (when (eq bl :unsat) (setq bl nil))
    (when bl
      (unless *quiet* 
	(unless (ss-checksat p2 (ss-prob-types prob) (append (hash2bl bl) truth)) 
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
	  (setq p2 (notamper-addhist origp history unassigned))
	  (setq bl2 (ss-whitebox-solve p2 prob :required (union unassigned (vars p2))))
	  (cond ((not bl2) (setq bl nil))
		(t
		 (unless *quiet*
		   (unless (ss-checksat p2 (ss-prob-types prob) (append (hash2bl bl2) truth))
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
	(notamper2xml-varassign nil bl 6 stream )
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

(defun ss-xml-esc (x) (xmlify x))

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
	((member (car p) '(var get put cookie request)) (list (second p)))
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
		  (f (xmlify (notamper2web-varspelling (first x))))
		  (f (xmlify (second x)))))))))
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

(defun xmlify (s)
  "(XMLIFY S) HTML escapes the characters #\" #\& #\' #\< #\>"
  (unless (stringp s) (setq s (princ-to-string s)))
  (with-output-to-string (o)
    (do ((i 0 (1+ i)) (c) (n (length s)))
        ((= i n) o)
      (setq c (elt s i))
      (if (find c '(#\" #\& #\' #\< #\>) :test #'char=)
	  (format o "%~:@(~2,'0x~)" (char-code c))
	  (write-char c o)))))

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
