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

; map coloring
;(eso-materialize '(color) '((=> (color ?x ?y) (color ?z ?y) (not (adj ?x ?z))) (forall ?x (=> (region ?x) (exists ?y (color ?x ?y)))) (region r1) (region r2) (region r3) (hue red) (hue blue) (adj r1 r2) (adj r2 r3) (=> (color ?x ?y) (and (region ?x) (hue ?y)))))

; task scheduling
;(eso-materialize '(does) '((=> (does ?t ?a ?d) (and (task ?t) (actor ?a) (day ?d))) (=> (depends ?x ?y) (and (task ?x) (task ?y))) (=> (does ?t ?a ?d) (does ?t2 ?a ?d) (= ?t ?t2)) (forall ?t (=> (task ?t) (exists (?a ?d) (does ?t ?a ?d)))) (=> (depends ?t1 ?t2) (does ?t1 ?a1 ?d1) (does ?t2 ?a2 ?d2) (lt ?d1 ?d2)) (=> (does ?t ?a1 ?d1) (does ?t ?a2 ?d2) (and (= ?a1 ?a2) (= ?d1 ?d2))) (day mon) (day tues) (day wed) (actor tim) (actor bob) (task a) (task b) (task c) (task d) (depends a b) (depends a c) (depends b d) (depends c d) (lt mon tues) (lt mon wed) (lt tues wed)))

(defmethod eso-materialize (esodb (th symbol))
  (esomaterialize esodb th))
(defmethod eso-materialize (esodb (th list))
  (esomaterialize esodb th))
(defmethod eso-materialize (esodb (th string))
  (if (probe-file th)
      (esomaterialize esodb (read-file th))
      (assert nil nil (format nil "Error: eso-materialize could not find file ~A." th))))
(defmethod eso-materialize (esodb (th theory))
  (esomaterialize esodb th))
(defmethod eso-materialize (esodb th)
  (declare (ignore esodb))
  (assert nil nil (format nil "Error: theory represented by unknown type: ~A." th)))

(defun esomaterialize (esodb th)
  "(ESOMATERIALIZE ESODBS TH) computes extensions for the ESODB predicates in theory or file TH.
   TH is an existential second order stratified datalog theory."
  (setq esodb (tolist esodb))
  (let (rules constraints)
    (multiple-value-setq (rules constraints) 
      (split #'(lambda (x) (or (atomicp x) (eq (signifier x) '<=))) (contents th)))
    ;(setq rules (define-theory (make-instance 'prologtheory) "" rules))
    (eso-materialize-csp esodb constraints rules)))

; (eso-materialize '(assign-col assign-row) "/Users/thinrich/Research/code/clicl/examples/esodatalog/gantt2")

(defun eso-materialize-csp (esodb constraints rules)
  "(ESO-MATERIALIZE-CSP ESODB CONSTRAINTS RULES) takes a theory of RULES, a theory of CONSTRAINTS, 
   and a set of ESODB predicates.  Constructs extensions for those predicates such that when 
   added to RULES, the resulting stratified model satisfies CONSTRAINTS."
  (let (types g esodeps funcs)
    ; turn functions into relations
    (setq funcs (funcs (makand (maksand constraints) (maksand rules))))
    (setq constraints (mapcar #'functions-to-relations constraints))
    (setq rules (mapcan #'to-canonical-datalog (mapcar #'functions-to-relations rules)))

    ; using the dependency graph, grab those preds dependent on esodbs (including esodbs)
    (setq g (dependency-graph rules))
    (mapc #'(lambda (x) (agraph-adjoin-noded x g)) esodb) 
    (setq esodeps (mapunion #'(lambda (x) (agraph-find-connected x g)) esodb))

    ; grab the types
    (multiple-value-setq (types constraints rules) (eso-materialize-csp-extracttypes constraints rules esodeps))

    ; ensure function-to-relation translation preserves equivalence (note we need types)
    (setq constraints (nconc (mapcan #'(lambda (x) (functional-axioms x types)) funcs) constraints))

    ; grab just the constraints that will impact the esodbs, i.e. those constraints including a pred dependent on esodbs
    (setq constraints (remove-if-not #'(lambda (c) (intersection (relns c) esodeps)) constraints))

    ; rewrite each constraint so that the only preds dependent on esodbs that appear are the esodbs.
    ;  NOTE: For now we're assuming the rules dependent on esodbs are non-recursive
    (setq constraints (interpolate-datalog (set-difference esodeps esodb) constraints esodb rules))
    
    ; construct a solution to the given constraints 
    (cond ((null constraints) nil)
	  (t (car (last (eso-materialize-csp-strata esodb constraints rules types)))))))

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

(defun eso-materialize-csp-strata (esodb constraints rules types)
  "(ESO-MATERIALIZE-CSP-STRATA ESODB CONSTRAINTS RULES TYPES) given a theory of constraints, a theory
   of rules, a set of ESODB predicates, and a hash table of types,
   construct extensions for all those predicates by first grounding the constraints, invoking a SAT 
   solver, and extracting the extensions from the model returned by the SAT solver.  The hash table
   maps (pred position) to unary predicates, where position is in {1,...,arity(p)}."
  (let (p groundtime cnftime sattime (*timesofar* 0) groundcomp cnfcomp)
    ; ground constraints
    (add-time (setq p (delete 'true (dbgrounds esodb constraints rules types))))
    (setq groundtime (/ *timesofar* internal-time-units-per-second))
    (setq groundcomp (complexity (maksand p)))

    ; convert to CNF
    (setq *timesofar* 0)
    (add-time (setq p (mapcan #'(lambda (x) (and2list (cnf x))) p)))
    (setq cnftime (/ *timesofar* internal-time-units-per-second))
    (setq cnfcomp (complexity (maksand p)))

    ; invoke SAT solver
    (setq *timesofar* 0)
    (add-time (setq p (sat-solve p)))
    (setq sattime (/ *timesofar* internal-time-units-per-second))

    ; return results and times
    (if (eq p :unsat)
	`(results ,groundtime ,groundcomp ,cnftime ,cnfcomp ,sattime :unsat)
	`(results ,groundtime ,groundcomp ,cnftime ,cnfcomp ,sattime ,p))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
