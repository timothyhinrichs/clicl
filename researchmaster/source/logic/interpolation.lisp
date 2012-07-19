;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; interpolation.lisp
;;;     routines for computing interpolants
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(eval-when (compile load eval)
  (proclaim '(special *SUBSUMPTION-CLOSURE* *RESOLUTION-CLOSURE* *tmp*)))


(defun interpolate-datalog (sourcepreds constraints targetpreds rules)
  "(INTERPOLATE-DATALOG SOURCEPREDS CONSTRAINTS TARGETPREDS RULES) rewrites all of the FOL CONSTRAINTS
   so that all occurrences of SOURCEPREDS are replaced with occurrences of TARGETPREDS
   while preserving logical equivalence.  Relationships between source and target preds
   are given by datalog/prolog RULES."
  (let (h)
    (setq h (make-hash-table))   ; memoizing rewritings
    (mapcar #'(lambda (x) (interpolate-datalog-aux sourcepreds x targetpreds (contents rules) h)) (contents constraints))))

(defun interpolate-datalog-aux (source p target rules defns)
  (cond ((atom p) (if (member p source) (third (interpolate-datalog-pred p rules target defns)) p))
	((member (car p) '(and or not => <= <=>)) 
	 (cons (car p) (mapcar #'(lambda (x) (interpolate-datalog-aux source x target rules defns)) (cdr p))))
	((member (car p) '(forall exists))
	 (list (first p) (second p) (interpolate-datalog-aux source (third p) target rules defns)))
	(t (cond ((member (car p) source)
		  (let (pdef m)
		    (setq pdef (interpolate-datalog-pred (relation p) rules target defns))
		    (setq m (mgu (second pdef) p))
		    (if m (plug (third pdef) m) `(err :orig ,p :new ,(second pdef)))))
		 (t p)))))
      
(defun interpolate-datalog-pred (r rules preds defns)
  "(PREDICATE-COMPLETION-INLINE R RULES DEFNS) finds a definition for predicate R and
   definitions for all predicates influencing R in terms
   of PREDS using RULES, storing the results in DEFNS, which must be a hash table.
   Returns the definition for R."
  (cond ((gethash r defns) (gethash r defns))
	(t
	 (let (rrules targetpreds rdef)
	   (multiple-value-setq (rrules rules) (split #'(lambda (x) (eq (reln (head x)) r)) rules))
	   (setq targetpreds (set-difference (uniquify (mapcan #'(lambda (x) (relns (maksand (body x)))) rrules)) preds))
	   (mapc #'(lambda (x) (interpolate-datalog-pred x rules preds defns)) targetpreds)
	   (setq rrules (mapcar #'(lambda (x) `(<= ,(head x) ,(logical-inline (maksand (body x)) targetpreds defns))) rrules))
	   (setq rdef (predicate-completion-reln r rrules))
	   (setf (gethash r defns) rdef)
	   rdef))))
	   
(defun logical-inline (p targets hdefs)
  "(LOGICAL-INLINE P TARGETS HDEFS) inlines all occurrences of predicates occurring in list TARGETS
   within sentence P using the hash table of definitions HDEFS.  
   Example.  (and p q) (p) (p -> (or r s))  becomes (and (or r s) q)."
  (cond ((atom p) (if (member p targets) (third (gethash p hdefs)) p))
	((member (car p) '(and or not => <= <=>)) 
	 (cons (car p) (mapcar #'(lambda (x) (logical-inline x targets hdefs)) (cdr p))))
	((member (car p) '(forall exists))
	 (list (first p) (second p) (logical-inline (third p) targets hdefs)))
	(t
	 (cond ((member (relation p) targets)
		(let (m pdef)
		  (setq pdef (stdize (gethash (relation p) hdefs)))
		  (setq m (mgu (second pdef) p))  ; make sure to put p last here
		  (if m
		      (plug (third pdef) m)
		      `(err :orig ,p :defn ,(second pdef)))))
	       (t p)))))
	   



; For now, I've just copied the seemingly right portions out of ER.lisp.  Still need to go through
;  and construct functions called 'interpolation' or something similar and move non-interpolation
;  routines elsewhere.  Think through how this might augment Epilog's routines.

(eval-when (compile load eval)
  (proclaim '(special *check-subsumption*)))

(defvar *trace-reform2iffs* nil "whether to trace algorithm for reformulating sentences to biconditionals")
(defvar *check-subsumption* nil)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; FHL to Datalog ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; (rewrite-query '(and (row q1 ?x) (col q1 ?y) (row q2 ?z) (col q2 ?w)) '4queenstight '(add sub))
;(rewrite-query '(not (and (color r1 ?x) (color r2 ?y) (color r3 ?z))) 'mapcoloring nil)

; why aren't we just calling residues on the matrix and then adding the quantifiers back on?
; June 28, 2010: Recently generalized ISOLATE-RELATIONS but haven't updated REWRITE-QUERY to reflect
;    change in interface.  Need to revisit all of these interpolation algorithms anyhow.
(defun rewrite-query (p th relns)
  "(REWRITEQUERY P TH RELNS) rewrites the query p in theory TH in terms of
   the relation constants RELNS."
  (assert (prenexp p) nil "REWRITE-QUERY takes a query in prenex form.")
  (let (newth head pshort isolated)
    ; only rewrite the portion that is not in the complete theory
    ; p is equivalent to pshort V isolated
    (multiple-value-setq (pshort isolated) (isolate-relations (strip-quantifiers p) relns))

    (setq head (cons 'tlhgoal (vars pshort)))
    (setq newth (define-theory (make-instance 'theory) "" (contras-wo= (maksand (contents th)))))
    (definemore newth (contras-wo= `(<=> ,head ,pshort)))
    ;(pcontents newth)
    (replace-matrix p (makor (reduce-to-vocab head newth relns) isolated))))

(defun orient-iffs (th)
  "(ORIENT-IFFS TH) if there is a biconditional in TH of the form p(xbar) <=> q(xbar),
   it computes whether it is a definition for p or q and orients all such rules so that
   the predicate on the left is being defined by the predicate on the right."
  (contents th))

(defun iffs2datalog (th)
  (let (data views)
    (multiple-value-setq (data views) (split #'base-defn (contents th)))
    (append (iffs2views views)
            (iffs2db data (compute-dca th)))))

(defvar *makesafe* t "whether to ensure safety in the Datalog rules.")
(defun iffs2views (ps)
  (if *makesafe*
    (makssafe (mapcan #'view2datalog-validate ps))
    (mapcan #'view2datalog-validate ps)))

(defun iffs2db (ps dca)
  (if (every #'base-defn ps)
    (mapcan #'(lambda (x) (ground-table x dca nil)) ps)
    (completebase2db ps dca)))

(defun query2datalog (p)
  (let (query entry)
    (setq entry (maksafe (cq2d-v p (freevars p))))
    (setq query (makssafe *globaltmp*))
    (values entry query)))

(defun completebase2db (th dca)
  "(COMPLETEBASE2DB TH) turns the complete, satisfiable theory TH into a database.  Returns extensions.
   Current implementation just uses fullfinds.  Maybe we should fork 2 processes for each ground atom?"

  (let (v db query newth)
    (setq v (get-vocabulary (maksand (contents th))))
    (setq db nil)
    (setq newth (make-instance 'theory))
    (define-theory newth "" (nconc (cons '(= ?x ?x) (unique-names-full dca)) (contents th)))
    (dolist (p v)
      (when (isrelation p)
        (setq query (cons (parameter-symbol p) (maptimes #'newindvar (parameter-arity p))))
        (setq db (nconc (fullfinds query query newth) db))))
    (empty newth)
    db))

(defun makssafe (ps &optional (guard 'univ))
  (mapcar #'(lambda (x) (maksafe x guard)) ps))

(defun maksafe (p &optional (guard 'univ) (atbegin t))
  "(MAKSAFE P) takes a datalog implication p and guards unsafe variables in it with GUARD."
  (let (newlits posbodvars)

    ; construct list of all variables in positive literals in the body
    (dolist (l (cddr p))
      (when (and (not (atom l)) (positive-literalp l) (not (eq (relation l) '=)))
	(setq posbodvars (union (vars l) posbodvars))))

    ; construct univ(x) for each variable x in the rule not in posbodvars
    (setq newlits (mapcar #'(lambda (x) (list guard x)) (set-difference (vars p) posbodvars)))
    (placeguards p newlits atbegin)))

(defun placeguards (p lits atbegin)
  (cond ((and (not (atom p)) (eq (car p) '<=))
	 (if atbegin
	     (list* '<= (second p) (nconc lits (cddr p)))
	     (list* '<= (second p) (append (cddr p) lits))))
	(t 
	 (if atbegin
	     (maksand (nconc lits (list p)))
	     (maksand (cons p lits))))))
  
#|
(defun compress-rule-bottlenecks (rules)
  "(COMPRESS-RULE-BOTTLENECKS RULES) returns a logically equivalent set of rules
   where every rule h <= b1 ^ ... ^ bn with only one rule that unifies with the predicate of h
   is removed and everywhere h is called, b1^...^bn replaces the call for h, 
   under the appropriate substitution."
s  (let (bottles remaining)
    (setq bottles (split #'(lambda (x) (= (length (cdr x)) 1)) (group-by-head rules)))
    (setq remaining (mapcan #'cdr (cdr bottles)) bottles (mapcan #'cdr (car bottles)))
    (mapcar #'(lambda (x) (rewrite-conjs x bottles)) remaining)))

(defun group-by-head (rules)
  "(GROUND-BY-HEAD RULES) returns an association list of RULES, grouped by the head of each rule."
  (do ((r rules (cdr r))
       (alist nil) (tmp))
      ((null r) alist)
    (setq tmp (assoc (relation (car r)) alist))
    (if (not tmp)
      (setq alist (acons (relation (car r)) (list (car r)) alist))
      (setf (cdr tmp) (cons (car r) (cdr tmp)))))) 

(defun rewrite-conjs (rule rewrites)
  "(REWRITE-CONJS RULE REWRITES) applies all the rewrites to conjuncts in rule.
   Doesn't work: need to also bind variables in the rule as we go."
  (do ((cs (body rule) (cdr cs))
       (newbody nil) (rewrite) ol)
      ((null cs) (if newbody (list* '<= (head rule) newbody) rule))
    (setq rewrite (member (relation (car cs)) rewrites :key #'relation))
    (cond (rewrite
           (setq ol (mgu (car cs) (head rewrite)))
           (when ol
             (setq newbody (append (nreverse (body (plug rewrite ol))) newbody))))
          (t
           (setq newbody (cons (car cs) newbody))))))
|#

(defun view2datalog-validate (p)
  "(VIEW2DATALOG-VALIDATE P) constructs a view definition for P in datalog.  Caution: assumes
   that if p is of the form q(xbar) <=> r(xbar) that q is the head and r is the body.  Assumes
   variables are not shadowed, e.g. Ax.(p(x) => Ex.q(x))"
  (setq *globaltmp* nil)
  (cond ((atom p) nil)
        ((eq (car p) '<=>)
	 (let (head body new)
	   (cond ((not (complex-sentencep (second p)))
		  (setq head (second p) body (third p)))
		 ((not (complex-sentencep (third p)))
		  (setq head (third p) body (second p)))
		 (t (assert nil 
			    nil 
			    (format nil "One side of <=> must be a simple sentence.  Couldn't convert ~A."
				    p))))
	   ; handle some common special cases
	   (cond ((not (listp body)) `(<= ,head ,body))
		 ((eq (car body) 'exists) 
		  (setq new (list* '<= head (completequery2datalog-validate 
					     (third body) 
					     (union (vars head) (vars (second body)))))))
		 (t (setq new (list* '<= head (completequery2datalog-validate body (vars head))))))
	   ; add new sentence to the supporting sentences
	   (cons new *globaltmp*)))))


(defun cq2d-v (p &optional (vars nil))
  (setq *globaltmp* nil)
  (completequery2datalog-validate p vars))

; NOTICE: CAN SPEED UP BY CACHING FREE VARIABLE COMPUTATION.  CURRENTLY WE'RE
;   WALKING THE SENTENCE REPEATEDLY TO COMPUTE THE FREE VARS AT VARIOUS POINTS
;   THROUGHOUT THE SENTENCE.
(defun completequery2datalog-validate (p vars &optional (forcenew nil))
  "(COMPLETEQUERY2DATALOG-VALIDATE P VARS) assumes P is a query over a complete theory 
   using vocabulary only from that theory.  Assumes p has free variables VARS. Returns
   a list of datalog entry points, implicitly conjoined, and sets *globaltmp* to the datalog query."
  (completequery2datalog-validate-aux p vars forcenew))

(defun completequery2datalog-validate-aux (p vars forcenew)
  (declare (notinline completequery2datalog-validate-aux))
  (cond ((atom p) p)
        ((eq (car p) 'exists) (exists2datalog p vars forcenew)) 
        ((eq (car p) 'forall) 
	 (completequery2datalog-validate-aux 
	  (maknot (list 'exists (second p) (maknot (third p)))) vars forcenew))
        ((eq (car p) 'not) (negation2datalog (second p) vars))
        ((find (car p) '(or and)) (andor2datalog p vars forcenew))
        ((eq (car p) '<=)
         (if (cddr p)
           (completequery2datalog-validate-aux 
	    `(or ,(second p) ,(nnf (maknot (maksand (cddr p))))) vars forcenew)
           (completequery2datalog-validate-aux (second p) vars forcenew)))
        ((eq (car p) '=>)
         (if (cddr p)
           (completequery2datalog-validate-aux 
	    `(or ,(nnf (maknot (maksand (cdr (butlast p))))) ,(car (last p))) vars forcenew)
           (completequery2datalog-validate-aux (second p) vars forcenew)))
        ((eq (car p) '<=>)
         (completequery2datalog-validate-aux `(and (<= ,(second p) ,(third p))
						   (=> ,(second p) ,(third p)))
					     vars
					     forcenew))
        (t (list p))))

(defun andor2datalog (p vars forcenew)
  "(ANDOR2DATALOG P VARS) converts P, which is either an and or an or, with free vars vars 
   into a datalog query.  Puts extra sentences in *globaltmp*."
  (declare (notinline andor2datalog))
  (let (newlit conj)
    (setq newlit (cons (gentemp "reln") (intersect vars (freevars p))))
    (cond ((eq (car p) 'and)
	   (setq conj (mapcan #'(lambda (x) (completequery2datalog-validate-aux x vars t)) 
			      (cdr p)))
	   (cond (forcenew
		  (push (list* '<= newlit conj) *globaltmp*)
		  (setq newlit (list newlit)))
		 (t
		  (setq newlit conj))))
          ((eq (car p) 'or)
           (dolist (l (cdr p))
             (push (list* '<= newlit (completequery2datalog-validate-aux l vars nil)) *globaltmp*))
	   (setq newlit (list newlit)))
          (t (setq newlit (list p))))
    newlit))

(defun negation2datalog (p vars)
  (declare (notinline negation2datalog))
  (cond ((atom p) (list `(not ,p)))
        ((eq (car p) 'not) ; double negation
	 (completequery2datalog-validate-aux (second p) vars nil))  
        ((member (car p) '(and or => <= <=>)) ; push negations in 
	 (completequery2datalog-validate-aux (nnf (maknot p)) vars nil))
        ((member (car p) '(exists forall)) ; forcing new preds--cq2d-v should return a list of 1 sent
         (list `(not ,(maksand (completequery2datalog-validate-aux p vars t)))))
        (t (list `(not ,p)))))

(defun exists2datalog (p vars forcenew)
  (declare (notinline exists2datalog))
  (let (newp newlit newvars pertvars)
    (setq newvars (vars (second p)))
    (setq pertvars (intersection vars (freevars (third p))))
    (setq newp (completequery2datalog-validate-aux (third p) (nconc newvars pertvars) nil))
    (cond (forcenew
           (setq newlit (cons (gentemp "reln") pertvars))
           (push (list* '<= newlit newp) *globaltmp*)
           (list newlit))
          (t
           newp))))
  


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; C2D ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



(defun qc2d (vars p th relns &optional (contras t))
  "(QC2D P TH BILEVEL CONTRAS) rewrites P in TH in terms of the relation constants RELNS.
   P must be quantifier-free. CONTRAS controls whether to include all the contrapositives 
   of the query: default is t, and turning it off may lead to incompleteness."

  (setq *matches* 0)
  (when (atom vars) (setq vars (list vars)))

  ; construct query
  (let* ((glit (cons (gentemp "goal") vars))
         (qvars (set-difference (freevars p) vars))
         (grule (if qvars 
                  `(<=> ,glit (exists ,qvars ,p))
                  `(<=> ,glit ,p)))
         (newrules (if contras (contrapositives grule) (brfs grule)))
         (newth (define-theory (make-instance 'theory) "" newrules)))
    (includes newth th)

    (setq *matches* 0)
    ;(let ((*goal* (car glit)) (*ignore-goal-rules* t)
    ;      (*equality* '=) (*una* t) (*ifreductionnoextension* t))
      (prog1 
        `(<=  ,glit ,(nnf (maknot (reduce-to-vocab (maknot glit) newth relns))))
        (empty newth)
        (unincludes newth th))))

(defun reduce-to-vocab (lit th relns)
  "(REDUCE-TO-VOCAB LIT TH RELNS) rewrites LIT in terms of RELNS using theory TH."
  (let ((*ancestry* t) 
        (*tautology-elim* t) 
        (*goal* (maknot (signed-relation lit)))
        (*ignore-goal-rules* t) 
        (*equality* '=) 
        (*una* t) 
        ;(*ifreductionnoextension* t)
	(*check-subsumption* t)
        (*check-answers* t)
        (allans) (binding)
	(vars (freevars lit)))

    ; make sure to include =
    (setq relns (adjoin '= relns))

    ; compute an expression for lit
    ;(format t "Reducing ~A to the vocabulary ~A ...~%" lit relns)
    ;(pcontents th)
    (setq allans (fullresidues lit th #'(lambda (x) (find x relns))))

    ;(print allans)
    ; yank out the goal part of each residue and replace with equality statements
;    (setq allans (mapcar #'(lambda (x) 
;                             (cond ((eq (car x) 'and) 
;                                    (maksand (plug (append (find-equalities (cdr (drop-not (second x))) (cdr (drop-not lit))) (cddr x))
;                                                   (append (delete-if #'(lambda (x) (eq (car x) (cdr x))) 
;                                                                      (mapcar #'cons (vars (second x)) (vars lit)))
;                                                           '((t . t))))))
;                                   (t 
;                                    (maksand (find-equalities (cdr (drop-not x)) (cdr (drop-not lit)))))))
;                         allans))

    ; clean up the answers
    (setq binding (mapcar #'cons vars (maptimes #'newskolem2 (length vars))))
    (setq allans (mapcar #'maksand 
			 (subsumption-elimination (mapcar #'(lambda (x) 
							      (if (and (listp x) (eq (first x) 'and)) 
								  (cdr x) 
								  (list x))) 
							  (nsublis binding allans)))))
    (setq allans (nsublis (mapcar #'(lambda (x) (cons (cdr x) (car x))) binding) allans))
    (setq allans (mapcar #'(lambda (x) (equals-for-equals x vars)) allans))
    (herbrand-simplify 
     (maksor (mapcar #'(lambda (x) (quantify-exists-but x (vars lit))) (nreverse allans))))))


;;; utilities ;;;

(defun to-boolean (p)
  (cond ((atom p) p)
        ((member (car p) '(and or not)) (cons (car p) (mapcar #'to-boolean (cdr p))))
        ((eq (car p) '=>) 
	 (makor (maknot (maksand (mapcar #'to-boolean (cdr (butlast p))))) (to-boolean (car (last p)))))
        ((eq (car p) '<=) 
	 (makor (to-boolean (second p)) (maknot (maksand (mapcar #'to-boolean (cddr p))))))
        ((eq (car p) '<=>) 
	 (makand (to-boolean `(<= (second p) (third p))) (to-boolean `(=> (second p) (third p)))))
        (t p)))

(defun quantify-exists-but (p vars)
  "(QUANTIFY-EXISTS-BUT P VARS) existentially quantifies the free variables in P except for VARS."
  (let ((vs (set-difference (freevars p) vars)))
    (if vs
      (list 'exists (set-difference (freevars p) vars) p)
      p)))

(defun herbrand-simplify (p &key (eq #'eq) (key '=))
  "(HERBRAND-SIMPLIFY P) simplifies p wrt ground equalities, i.e. replaces a=a with true and
   a != a with false."
  (herbrand-simplify-aux p eq key))

(defun herbrand-simplify-aux (p eq key)
  (cond ((atom p) p)
        ((eq (car p) 'and) (hsand p eq key))
        ((eq (car p) 'or) (hsor p eq key))
        ((eq (car p) 'not) (hsnot p eq key))
        ((eq (car p) 'forall) (hsquant p eq key))
        ((eq (car p) 'exists) (hsquant p eq key))
        ((funcall eq (car p) key)
         (cond ((equal (second p) (third p)) 'true)
               ((or (freevars (second p)) (freevars (third p))) p)
               (t 'false)))
        (t p)))

(defun hsand (p eq key)
  (let ((cs (mapcar #'(lambda (x) (herbrand-simplify-aux x eq key)) (cdr p))))
    (cond ((find 'false cs) 'false)
          (t
           (maksand (delete 'true cs))))))

(defun hsor (p eq key)
  (let ((ds (mapcar #'(lambda (x) (herbrand-simplify-aux x eq key)) (cdr p))))
    (cond ((find 'true ds) 'true)
          (t
           (maksor (delete 'false ds))))))

(defun hsnot (p eq key)
  (let ((p2 (herbrand-simplify-aux (cadr p) eq key)))
    (cond ((eq p2 'false) 'true)
          ((eq p2 'true) 'false)
          (t (maknot p2)))))

(defun hsquant (p eq key)
  (list (car p) (cadr p) (herbrand-simplify-aux (caddr p) eq key)))


(defun find-equalities (data names)
  (let ((ht (make-hash-table :test #'eq)) )

    ; fill up the hash table
    (mapc #'(lambda (key val) (setf (gethash key ht) (cons val (gethash key ht)))) data names)
    ;(pretty-print ht)

    ; extract equalities by iterating over the hash table
    (with-hash-table-iterator (get-element ht)
      (do ((result (multiple-value-list (get-element)) (multiple-value-list (get-element)))
           (vals nil) (names) (key))
          ((null (cdr result)) (delete-if #'(lambda (x) (eq (second x) (third x))) vals))
        (setq key (second result))
        (setq names (third result))
        (cond ((cdr names)
               (setq vals (nconc (do ((last (car names))
                                      (phi nil)
                                      (ns (cdr names) (cdr ns)))
                                     ((null ns) (if (not (varp key)) 
                                                  (cons `(= ,key ,(car names)) phi)
                                                  phi))
                                   (setq phi (cons `(= ,(car ns) ,last) phi))
                                   (setq last (car ns)))
                                 vals)))
              ((not (varp key)) (setq vals (nconc (list `(= ,(car names) ,key)) vals))))))))

(defun set-some-equal (xs ys ylist)
  "(SET-SOME-EQUAL XS YS XLIST) sets every x in XS equal to the corresponding y in YS
   as long as y is in YLIST."
  (do ((x xs (cdr x))
       (y ys (cdr y))
       (pairing))
      ((or (null x) (null y)) pairing)
    (when (and (not (eq (car x) (car y))) (member (car y) ylist))
      (setq pairing (cons `(= ,(car x) ,(car y)) pairing)))))

(defun manipulate-equality-dnf (dnf)
  "(MANIPULATE-EQUALITY-DNF DNF) runs a greedy algorithm trying to minimize the size of the CNF 
   version of DNF."
  dnf)


;;; Theory massaging
;(defun define-c2d-theory (th doc facts)
;  (let ((exts (extensionally-defined facts)))
;    (define-theory th doc (mapcar #'(lambda (x) (order-conjuncts x #'order-conjuncts-first exts)) facts))))

(defun vocabulary-tree (p)
  "(VOCABULARY-TREE P) walks P, which is assumed to be in nnf without any implications and returns
   a tree where each node is (q . vocab), where q is a connective or a literal and vocab is the vocabulary
   contained in P."
  (cond ((atom p) (cons p (get-vocabulary p)))
        ((member (car p) '(or and)) 
         (let ((rec (mapcar #'vocabulary-tree (cdr p))))
           (cons (cons (car p) 
		       (reduce #'(lambda (x y) 
				   (union x (if (and (listp (car y)) (member (caar y) '(and or)))
						(cdr (car y)) 
						(cdr y))
					  :test #'equalp)) 
			       rec :initial-value nil))
                 rec)))
        (t (cons p (get-vocabulary p)))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Simultaneous Implication ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun simultaneous-er-all-predicates (th cpreds fullyfactored transform)
  (setq th (funcall *resolution-closure* (clauses (maksand (contents th))) *limit*))
  (when (functionp transform) (setq th (mapcarnot transform th)))
  (when (member '(or) th :test #'equal)
    (return-from simultaneous-er-all-predicates '((or))))
  (setq th (mapcar #'herbrand-simplify th))
  (when (member 'false th)
    (return-from simultaneous-er-all-predicates '((or))))
  (setq th (remove-if #'(lambda (x) (eq x 'true)) th))
  (setq th (compute-impl-queries th 
				 (mapcar #'parameter-symbol
					 (set-difference (preds (maksand th)) cpreds :test #'equalp))
				 fullyfactored)))

(defun compute-impl-queries (clauses preds fullyfactored)
  (if fullyfactored
      (compute-impl-queries-fullyfactored clauses preds)
      (compute-impl-queries-unfactored clauses preds)))

(defun compute-impl-queries-fullyfactored (clauses preds)
  "(COMPUTE-IMPL-QUERIES-FULLYFACTORED CLAUSES PREDS) returns a list of sentences of the form
   (<=> (impl '(p @x) @x) (psi @x)), where '(p @x) is possibly negated.  The list consists of
   up to two sentences for each predicate in PREDS."
  (let ((h (make-hash-table)))
    (dolist (clause clauses)
      (group-by-literals (to-orless-list clause) h preds))
    (compute-impl-queries-factored preds h)))

(defun group-by-literals (clauseset hash &optional (preds '?))
  "(GROUP-BY-LITERALS CLAUSESET HASH PREDS) augments hash table HASH so that
   clauseset is indexed on all of those hash entries (which are a subset of PREDS)
   that correspond to a signed literal in CLAUSESET."
  (let (pentry)
    (dolist (c clauseset)
      (when (or (eq preds '?) (member (relation c) preds))
	(setq pentry (gethash (relation c) hash))
	(unless pentry (setq pentry (cons nil nil)) (setf (gethash (relation c) hash) pentry))
	(if (negative-literalp c)
	    (push clauseset (cdr pentry))
	    (push clauseset (car pentry)))))))

(defun compute-impl-queries-factored (preds hash)
  (let ((result nil) tmp)
    (dolist (p preds result)
      ; positive
      (when (car (gethash p hash))
	(setq tmp (compute-impl-queries-factored-predicate p (car (gethash p hash))))
	(when tmp (push tmp result)))
      ; negative
      (when (cdr (gethash p hash))
	(setq tmp (compute-impl-queries-factored-predicate p (cdr (gethash p hash)))) 
	(when tmp (push tmp result))))))
            
(defun compute-impl-queries-factored-predicate (p clausesets) 
  (let (vars)
    (setq clausesets (mapcar #'(lambda (x) (fully-factor p x)) clausesets))
    (setq clausesets (delete nil clausesets))
    (when (null clausesets) (return-from compute-impl-queries-factored-predicate nil))
    (setq clausesets (funcall *subsumption-closure* clausesets))
    (setq vars (head-args (length (args (first (first clausesets))))))
    (setq clausesets (mapcar #'(lambda (lits)
				 (similarize-head (list* '<= (first lits) (mapcar #'maknot (cdr lits))) 
						  vars))
			     clausesets))
    (list '<=> 
	  (makimpl (second (first clausesets)) (vars (second (first clausesets)))) 
	  (equantify-except (maksor (mapcar #'(lambda (imp) (maksand (body imp))) clausesets))
			    (vars (second (first clausesets)))))))

(defun compute-impl-queries-unfactored (clauses preds)
  (let ((results nil))
    (dolist (c clauses results)
      (setq results (nconc (compute-impl-queries-unfactored-clauseset (to-orless-list c) preds) 
			   results)))))

(defun compute-impl-queries-unfactored-clauseset (clauseset preds)
  (do ((c clauseset (cdr c)) (p) (new) (results nil))
      ((null c) results)
    (setq p (relation (car c)))
    (when (member p preds)
      ; reorder clause so new lit at start
      (setq new (copy-prefix clauseset c))
      (setq new (nconc new (cdr c)))
      (setq new (cons (car c) new))
      ; create impl query
      (setq new (list '<= (makimpl (car new) (vars (car new))) (nnf (maknot (maksor (cdr new))))))
      (push new results))))

(defun fully-factor (pred lits)
  "(FULLY-FACTOR PRED LITS) takes a predicate PRED and a list of
   literals LITS.  If pred occurs both positively and negatively, returns nil.
   If all occurrences of PRED have no unifier, returns nil.  Otherwise, 
   some mgu sigma unifies all PRED literals and the return value is LITS after having applied
   sigma.  Consequently, the return value has at most one occcurrence of PRED."
 (let (predlits rest mgu)
    (multiple-value-setq (predlits rest) (split #'(lambda (x) (eq (relation x) pred)) lits))
    ; check if both positive and negative 
    (do ((ps predlits (cdr ps)) (pos nil) (neg nil))
	((null ps))
      (if (positive-literalp (car ps)) (setq pos t) (setq neg t))
      (when (and pos neg) (return-from fully-factor nil)))

    ; try to find an mgu
    (setq mgu (mgun predlits))
    (if mgu    
	(cons (plug (car predlits) mgu) (plug rest mgu))
	nil)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
