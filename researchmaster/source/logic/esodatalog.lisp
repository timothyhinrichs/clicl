;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Existential Second Order Datalog
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun esofinds (thing p th &optional (esodb nil))
  "(ESOFINDS THING P TH) executes the usual database query (viewfinds thing p th*) where
   th* is th augmented with base tables for the predicates ESODBS."
  (let (mat)
    (setq mat (eso-materialize esodb th))
    (cond ((not mat) :unsat)
	  (t
	   (setq mat (define-theory (make-instance 'prologtheory) "" mat))
	   (includes mat th)
	   (viewfinds thing p mat)))))

(defun eso-materialize (esodb th &rest theories)
  "(ESOMATERIALIZE ESODBS TH) computes extensions for the ESODB predicates in theory or file TH.
   TH is an existential second order stratified datalog theory."
  (setq esodb (tolist esodb))
  (setq th (mapcan #'contents (cons th theories)))
  (let (rules constraints objs)
    (setq objs (compute-dca (maksand th)))
    (multiple-value-setq (rules constraints) 
      (split #'(lambda (x) (or (atomicp x) (eq (signifier x) '<=))) th))
    (setq rules (definemore rules (mapcar #'(lambda (x) (list '= x x)) objs)))
    ;(setq rules (define-theory (make-instance 'prologtheory) "" rules))
    (eso-materialize-csp esodb constraints rules)))

; (eso-materialize '(assign-col assign-row) "/Users/thinrich/Research/code/clicl/examples/esodatalog/gantt2")

(defun eso-materialize-csp (esodb constraints rules)
  "(ESO-MATERIALIZE-CSP ESODB CONSTRAINTS RULES) takes a theory of RULES, a theory of CONSTRAINTS, 
   and a set of ESODB predicates.  Constructs extensions for those predicates such that when 
   added to RULES, the resulting stratified model satisfies CONSTRAINTS."
  (let (types g esodeps funcs knowns rec solvingfor recrules result)
    ; turn functions into relations
    (setq funcs (funcs (makand (maksand constraints) (maksand rules))))
    (setq constraints (mapcar #'functions-to-relations constraints))
    (setq rules (mapcan #'to-canonical-datalog (mapcar #'functions-to-relations rules)))

    ; Need to ground: (1) constraints relevant to unknowns and (2) rules with heads defining recursive preds
    ;   Grounding should mention (a) esodb preds, (b) recursive preds.
    ; Constraints might include references to known preds.  Will happen during grounding.
    ; Constraints might include references to preds known once given the esodbs and recursive preds.  Eliminate via interpolation.
    ; Rules might include references to known preds; thus, after grounding, these rules should be simplified
    ;   with respect to known preds.
    ; Rules might include references to preds known once given the esodbs and recursive preds.  Eliminate via interpolation.

    (setq g (dependency-graph rules))
    (mapc #'(lambda (x) (agraph-adjoin-noded x g)) esodb) 
    (setq esodeps (mapunion #'(lambda (x) (agraph-find-connected x g)) esodb))  ; dependent on esodbs
    (setq knowns (set-difference (union (relns (maksand constraints)) (relns (maksand rules))) esodeps)) ; basic datalog
    (setq rec (intersection (apply #'nconc (agraph-strongly-connected-components* g)) esodeps)) ; recursives dependent on esodbs
    ;(print rec)
    (setq solvingfor (union esodb rec)) ; what we need to solve for

    ; grab the types
    (multiple-value-setq (types constraints rules) (eso-materialize-csp-extracttypes constraints rules esodeps))

    ; ensure function-to-relation translation preserves equivalence (note we need types)
    (setq constraints (nconc (mapcan #'(lambda (x) (functional-axioms x types)) funcs) constraints))

    ; grab just the constraints/rules that will impact the esodbs, 
    ;   i.e. those constraints including a pred dependent on esodbs
    ;    and those rules defining recursive, unknown predicates.
    (setq constraints (remove-if-not #'(lambda (c) (intersection (relns c) esodeps)) constraints))
    (setq recrules (remove-if-not #'(lambda (r) (member (relation (head r)) rec)) rules))

    ; rewrite each constraint/rule so that the only preds dependent on esodbs that appear are the solvingfor + knowns.
    ;  i.e. eliminate preds that are known once we have definitions for esodbs and rec.
    (setq constraints (interpolate-datalog (set-difference esodeps solvingfor) constraints (append solvingfor knowns) rules))
    (setq recrules (interpolate-datalog (set-difference esodeps solvingfor) recrules (append solvingfor knowns) rules))

    ; construct a solution to the given constraints/rules, where only appearing preds are solvingfor + knowns
    (cond ((null constraints) nil)
	  ((eq (setq result (car (last (eso-materialize-csp-strata solvingfor knowns constraints recrules rules types)))) :unsat) :unsat)
	  (t (remove-if-not #'(lambda (x) (member (relation x) esodb)) result)))))

(defun eso-materialize-csp-extracttypes (constraints rules &optional (nontypes nil))
  "(ESO-MATERIALIZE-CSP-EXTRACTTYPES CONSTRAINTS RULES) computes a hash table of (pred num) keys
   mapped to a unarypred indicating the type for pred's numth argument.  May modify rules
   to construct new types that are the intersection of old types for when we find multiple
   types.  Never assumes a NONTYPE is a type.  
   Removes any rules used to infer type info.  Returns three values: the hash table,
   the remaining constraints, and the new rules."
  (let ((h (make-hash-table :test #'equal)) pred index typ (constraintsused nil))
    (dolist (c constraints)
      (when (eq (signifier c) '=>)
	(when (and (third c) (null (fourth c)) (atomicp (second c)))  ; of the form (=> atom b)
	  (setq pred (relation (second c)))
	  (dolist (r (and2list (third c)))
	    (when (and (monadic-atom r) (varp (second r)) (not (member (relation r) nontypes)))
	      (setq index (indexof (second r) (second c)))
	      (when index
		(setq typ (relation r))
		(when (gethash (list pred index) h)  ; found 2nd type
		  (setq typ (tosymbol (gentemp "reln")))
		  (setq rules (save `(<= (,typ ?x) 
					 (,(gethash (list pred index) h) ?x)
					 (,(relation r) ?x)) rules)))
		(setf (gethash (list pred index) h) typ)
		(push c constraintsused)))))))
    (values h (set-difference constraints constraintsused :test #'equal) rules)))

#|

(test-on-graphcoloring #'(lambda (data descriptor) (print (eso-materialize-csp-strata '(color) '((=> (color ?x ?y) (color ?z ?y) (not (adj ?x ?z))) (forall ?x (=> (region ?x) (exists ?y (color ?x ?y))))) (define-theory (make-instance 'prologtheory) "" data) h descriptor))) 3 5 10 2)

(time (progn (setq g (gen-random-blocksworld 5)) 
       (setq k (max-incident-edges g)) 
       (setq g (nconc (maptimes #'(lambda () (list 'hue (tosymbol (gentemp "hue")))) (1+ k)) g))
       (setq d (dbgrounds '(color) '((=> (color ?x ?y) (color ?z ?y) (not (adj ?x ?z))) (forall ?x (=> (region ?x) (exists ?y (color ?x ?y))))) (define-theory (make-instance 'prologtheory) "" g) h))
       (format t "~%Complexity of grounded formula: ~A~%" (complexity d))
       (setq a (cnf d) b 0)))

(test-on-taskscheduling 
 #'(lambda (data) 
     (eso-materialize-csp-strata '(does) 
				 '((=> (does ?t ?a ?d) (does ?t2 ?a ?d) (= ?t ?t2)) (=> (depends ?t1 ?t2) (does ?t1 ?a1 ?d1) (does ?t2 ?a2 ?d2) (lt ?d1 ?d2)) (forall ?t (=> (task ?t) (exists (?a ?d) (does ?t ?a ?d))))) 
				 data ;(define-theory (make-instance 'prologtheory) "" data)
				 tsktypes))
 1 5 5 1)
|#

(defun eso-materialize-csp-strata (solvingfor dbpreds constraints recrules rules types)
  "(ESO-MATERIALIZE-CSP-STRATA ESODB CONSTRAINTS RULES TYPES) given a theory of constraints, a theory
   of rules, a set of ESODB predicates, and a hash table of types,
   construct extensions for all those predicates by first grounding the constraints, invoking a SAT 
   solver, and extracting the extensions from the model returned by the SAT solver.  The hash table
   maps (pred position) to unary predicates, where position is in {1,...,arity(p)}."
  (let (p groundtime cnftime sattime (*timesofar* 0) groundcomp cnfcomp loops missingheads)
    ; ground constraints, remove remaining DBpredicates, and add loop formulas
    (when recrules
      (add-time
       (setq recrules (dbgrounds solvingfor recrules rules types t))
       (setq missingheads (preds (maksand (mapcar #'head recrules))))
       (setq missingheads (mapcar #'(lambda (x) (cons (parameter-symbol x) (maptimes #'newindvar (parameter-arity x)))) missingheads))
       (setq missingheads (dbgrounds solvingfor missingheads rules types t))
       (setq missingheads (mapcar #'head missingheads))
       (setq missingheads (set-difference missingheads (mapcar #'head recrules) :test #'equal))
       (setq missingheads (mapcar #'maknot missingheads))
       (setq recrules (brfs (simplify-complete (maksand recrules) dbpreds rules)))
       (setq loops (loop-formulas* recrules))))
    (add-time 
     (setq constraints (dbgrounds solvingfor constraints rules types))
     (setq constraints (and2list (simplify-complete (maksand constraints) dbpreds rules))))
    ;(pprint recrules)
    ;(pprint loops)
    ;(pprint constraints)
    (setq p (nconc constraints recrules loops missingheads))
    (setq groundtime (/ *timesofar* internal-time-units-per-second))
    (setq groundcomp (complexity (maksand p)))

    ; convert to CNF
    (setq *timesofar* 0)
    (add-time (setq p (mapcan #'(lambda (x) (and2list (cnf x))) p)))
    (setq cnftime (/ *timesofar* internal-time-units-per-second))
    (setq cnfcomp (complexity (maksand p)))

    ; invoke SAT solver
    (setq *timesofar* 0)
    (if (eq p 'false)
	(setq p :unsat)
	(add-time (setq p (sat-solve p))))
    (setq sattime (/ *timesofar* internal-time-units-per-second))

    ; return results and times
    (if (eq p :unsat)
	`(results ,groundtime ,groundcomp ,cnftime ,cnfcomp ,sattime :unsat)
	`(results ,groundtime ,groundcomp ,cnftime ,cnfcomp ,sattime ,p))))



(defun test-all () (test-eso-materialize))

(defun test-eso-filename (dir name extension) (namestring (loadfn name :root *localrootdir* :dir (list "examples" dir) :type extension)))
(defun test-eso-materialize ()
  (let (result errors preds fth fdata fout)
    (setq preds '(("mapcoloring" color)
		  ("taskscheduling" (task-done-by task-done-on))
		  ;("gantt" assign)  older, slower version
		  ("gantt2" (assign-row assign-col))
		  ("blocksworld" (to_stack to_table))))

    (dolist (p preds)
      ; grab input/output files
      (setq fth (test-eso-filename "esodatalog" (first p) "th"))
      (unless (probe-file fth) 
	(push (format nil "Missing test theory: ~A" fth) errors)
	(setq fth nil))
      (setq fdata (test-eso-filename "esodatalog" (first p) "data"))
      (unless (probe-file fdata) 
	(push (format nil "Missing test data: ~A" fdata) errors)
	(setq fdata nil))
      (setq fout (test-eso-filename "esodatalog" (first p) "out"))
      (unless (probe-file fout) 
	(push (format nil "Missing answer: ~A" fout) errors)
	(setq fout nil))

      ; test those files
      (when (and (or fth fdata) fout)
	(setq result (eso-materialize (second p) fth fdata))
	(unless (setequal result (read-file fout) :test #'equal)
	  (push (format nil "Incorrect output for theory ~A and data ~A" fth fdata) errors))))
    errors))





;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
