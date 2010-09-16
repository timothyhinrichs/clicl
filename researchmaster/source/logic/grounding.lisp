;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Grounding
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;; Grounding in the context of a Database  ;;;;;;;;;;;;;;;;;;;;

(defun dbgrounds (preds constraints rules types &optional (rulesp nil)) 
  "(DBGROUND PREDS CONSTRAINTS RULES TYPES) takes a list of PREDS,
   a theory of CONSTRAINTS, a theory of RULES, a hash table of
   TYPES mapping (pred int) to unary preds, and a boolean RULESP. 
   RULESP controls whether or not
   CONSTRAINTS are treated as rules, i.e. where all constraints are of the form
   (<= atom body) and the results must be rules where all heads are instantiations of atom.
   Returns a list of ground clauses where the only occurring predicates 
   are PREDS.  Reserves UNIV for the universal type and = as in Herbrand Logic."
  (let ((objs (compute-dca rules)))
    ; quantify constraints
    (setq constraints (mapcar #'(lambda (x) (uniquify-vars (quantify x))) (contents constraints)))
    ; add univ(x) and x=x for all objs x to rules
    (setq rules (definemore rules (mapcar #'(lambda (x) (list 'univ x)) objs)))
    (setq rules (definemore rules (mapcar #'(lambda (x) (list '= x x)) objs)))
    (and2list (flatten-operator (maksand (mapcar #'(lambda (x) (dbground preds x rules types rulesp)) constraints))))))

(defun dbground (preds constraint rules types rulep)
  "(DBGROUND PREDS CONSTRAINT RULES TYPES) Same as dbgrounds but
   accepts a single, closed CONSTRAINT."
  (cond ((groundp constraint) constraint)
	(rulep  ; constraint is (forall * (<= atom sent1 ... sentn)) and 
	        ;   the result needs to be a bunch of reductions with instances of atom in the head
	 (when (and (listp constraint) (member (car constraint) '(forall exists))) (setq constraint (third constraint)))
	 (let (dbquery rule dbpreds grules)
	   (setq dbpreds (set-difference (relns constraint) preds))
	   (multiple-value-setq (dbquery rule) (split #'(lambda (x) (subsetp (relns x) dbpreds)) (body constraint)))
	   (setq rule (list* '<= (head constraint) rule))
	   (setq dbquery (dbground-prep-dbquery (maksand dbquery) rule types))
	   (setq grules (dbground-ground dbquery rule rules))
	   (maksand (mapcar #'(lambda (arule) 
				      (if (body arule)
					  (list* '<= (second arule) (mapcar #'(lambda (bodysent) 
										(dbground preds (nnf bodysent) rules types nil))
									    (body arule)))
					  arule))
			    grules))))
	((literalp constraint) 
	 (assert nil nil (format nil "DBGROUND must be passed closed sentences (or rules with RULEP true).  Found ~A" constraint)))
	((member (car constraint) '(or and not => <= <=>))
	       (cons (car constraint) 
		     (mapcar #'(lambda (x) (dbground preds x rules types rulep)) (cdr constraint))))
	(t  ; quantified
	 (let (dbquery remainder q dbpreds)
	   ; rewrite query so that as many of the free vars in qf sentence are bound by db preds as possible
	   (setq dbpreds (set-difference (relns constraint) preds))
	   (setq q (isolate-relations constraint dbpreds))
	   (if (eq (car q) 'forall)
	       (setq q (list (first q) (second q) (toimp (third q))))
	       (setq q (list (first q) (second q) (toand (third q)))))
	   ; construct database query that guards all free vars in qf fragment
	   (setq dbquery (second (third q)))
	   (setq remainder (third (third q)))
	   (setq dbquery (dbground-prep-dbquery dbquery remainder types))
	   ; ground all free variables in qf fragment and throw away db fragment
	   (setq remainder (dbground-ground dbquery remainder rules))
	   ; disjoin or conjoin results
	   (if (eq (car constraint) 'forall) 
	       (setq remainder (maksand remainder)) 
	       (setq remainder (maksor remainder)))
	   ; recurse to address remaining variables in qf fragment
	   (dbground preds remainder rules types rulep)))))

(defun dbground-prep-dbquery (dbquery remainder types)
  "(DBGROUND-PREP-DBQUERY DBQUERY REMAINDER TYPES) takes the database query DBQUERY being used to 
   ground REMAINDER and a hash table of types.  Returns DBQUERY prepared for materialization."
  (let (guards te)
    ; compute type guards (even if redundant--let DB simplify)
    (setq guards (uniquify (nconc (find-freevar-types dbquery types)
				  (find-freevar-types remainder types))))
    ; ensure no tautologies are generated
    (setq te (gen-tautologyelim-constraints remainder))
    (maksand (list dbquery (maksand guards) (maksand te)))))

(defun dbground-ground (dbquery remainder rules)
  "(DBGROUND-GROUND DBQUERY REMAINDER RULES) uses a database to find all instances
   of DBQUERY true of RULES pertinent to REMAINDER, and plugs those instances
   in for remainder, returning a set of instances of remainder."
  (let (fvars answers (h (make-hash-table)))
    (setq fvars (freevars remainder))
    (setq fvars (nreverse (cons t fvars)))  ; so that all BL end with (t . t)
    (setq dbquery (or2list (dnf dbquery)))
    (setq dbquery (maksor (mapcar #'(lambda (x) (maksand (order-datalog-conjuncts (and2list x) h 'univ))) 
				  dbquery)))
    (setq answers (viewfinds fvars dbquery rules))
    (mapcar #'(lambda (a) (plug remainder (mapcar #'cons fvars a))) answers)))

; Formula renaming seems to be much slower than the naive dbgrounds, at least on map coloring.
;  Maybe look into specifying the variable ordering for the SAT solver (if the SAT solving time is slow).  
;   I could imagine that starting with
;   the "real" propositions would be good, as it would simulate the search without
;   the clever propositional encoding; however, starting with the high-level propositions
;   might simulate an abstraction/refinement approach.
(defun dbgrounds-formularenaming (esodb constraints rules types)
  "(DBGROUNDS-FORMULARENAMING ESODB CONSTRAINTS RULES TYPES)
   Same as dbgrounds, except return value is a list of sentences that
   are implicitly conjoined."
  (let (entry defs result objs)
    (setq objs (compute-dca rules))
    (setq rules (definemore rules (mapcar #'(lambda (x) (list 'univ x)) objs)))
    (setq rules (definemore rules (mapcar #'(lambda (x) (list '= x x)) objs)))
    (setq constraints (mapcar #'quantify (mapcar #'nnf constraints)))
    (dolist (c constraints result)
      (multiple-value-setq (entry defs)
	(dbground-formularenaming esodb c rules types))
      (setq result (nconc (cons entry defs) result)))))

(defun dbground-formularenaming (esodb constraint rules types)
  "(DBGROUND-FORMULARENAMING RULES CONSTRAINT ESODB UNIV) Same
   as dbground, except utilize formula renaming to simplify clausal
   form conversion post-grounding.  Also, there are two return values:
   a sentence and a set of supporting definitions."
  (declare (notinline dbground-formularenaming))
  (setq constraint (quantify constraint))
  (cond ((literalp constraint) (values constraint nil))
	((member (car constraint) '(or and not => <= <=>))
	 (let (newreln body bodynew aux auxnew)
	   (setq newreln (list (gentemp "reln")))
	   (dolist (c (cdr constraint))
	     (multiple-value-setq (bodynew auxnew)
	       (dbground-formularenaming esodb c rules types))
	     (push bodynew body)
	     (setq aux (nconc auxnew aux)))
	   (push `(<=> ,newreln ,(cons (car constraint) (nreverse body))) aux)
	   (values newreln aux)))
	(t  ; quantified
	 (let (dbquery remainder q dbpreds)
	   ; isolate the predicates defined in the database
	   (setq dbpreds (set-difference (relns constraint) esodb))
	   (setq q (isolate-relations constraint dbpreds))
	   (if (eq (car q) 'forall)
	       (setq q (list (first q) (second q) (toimp (third q))))
	       (setq q (list (first q) (second q) (toand (third q)))))
	   (setq dbquery (second (third q)))
	   (setq remainder (third (third q)))
	   (setq dbquery (dbground-prep-dbquery dbquery remainder types))
	   (setq remainder (dbground-ground dbquery remainder rules))
	   (if (eq (car constraint) 'exists) 
	       (setq remainder (maksor remainder)) 
	       (setq remainder (maksand remainder)))
	   (dbground-formularenaming esodb remainder rules types)))))

(defun find-freevar-types (p types)
  "(FIND-FREEVAR-TYPES P TYPES) takes a formula P and a hash table indexed on (pred i)
   that maps to a unary predicate capturing the type of each pred's ith argument.
   Returns a list of type atoms, e.g. ((p ?x) (q ?y)), for all the free variables."
  (uniquify (find-freevar-types-aux p types nil)))

(defun find-freevar-types-aux (p types quantifiedvars &optional (univ 'univ))
  (cond ((atom p) nil)
	((member (car p) '(forall exists)) 
	 (find-freevar-types-aux (third p) types (union quantifiedvars (tolist (second p)))))
	((member (car p) '(and or not => <= <=>))
	 (mapcan #'(lambda (x) (find-freevar-types-aux x types quantifiedvars)) (cdr p)))
	(t
	 (do ((ps (cdr p) (cdr ps))
	      (i 1 (1+ i))
	      (result nil) (pred))
	     ((null ps) result)
	   (when (and (varp (car ps)) (not (member (car ps) quantifiedvars)))
	     (setq pred (gethash (list (car p) i) types))
	     (if pred
		 (push (list pred (car ps)) result)
		 (push (list univ (car ps)) result)))))))

;;;;;;;;;;;;;;;;;;;; Naive Grounding ;;;;;;;;;;;;;;;;;;;;

(defun ground-herbrands (ps &optional (smallcnf t))
  "(GROUND-HERBRANDS PS) grounds the sentences PS in finite herbrand logic."
  (let ((result nil)
        (dca (compute-dca ps)))
    (dolist (p (contents ps) result)
      (setq result (nconc result (ground-herbrand p dca smallcnf))))))

(defun ground-herbrand (p dca &optional (smallcnf t))
  "(GROUND-HERBRAND P DCA) grounds P with DCA, treating tables specially.  
   Assumes UNA."
  (if (base-defn p) 
    (ground-table p dca)
    (ground (quantify p) dca smallcnf))) 

(defun ground (p dca &optional (smallcnf t))
  "(GROUND P DCA) returns a list of sentences that result from grounding p,
   where if smallcnf is true, we use the algorithm that invents new 
   propositional constants for subformulas to avoid an explosion in the
   clausal form conversion."
  (cond (smallcnf
         (let ((newp) (aux))
           (multiple-value-setq (newp aux) 
	     (grounding-with-full-formula-renaming p dca :toplevel t))
           (cons newp aux)))
         (t
          (list (grounding-without-formula-renaming-iterative p dca)))))

(defun grounding-without-formula-renaming (p dca)
  "(GROUNDING-WITHOUT-FORMULA-RENAMING P DCA) grounds the formula P without 
   using formula renaming. Always returns a single sentence p."
  (cond ((atom p) p)
        ((find (car p) '(and or => <= <=>)) 
	 (cons (car p) (mapcar #'(lambda (x) 
				   (grounding-without-formula-renaming x dca)) 
			       (cdr p))))
        ((eq (car p) 'not) 
	 (maknot (grounding-without-formula-renaming (cadr p) dca)))
        ((eq (car p) 'forall) 
	 (grounding-without-formula-renaming 
	  (maksand (ground-x (second p) (third p) dca)) dca))
        ((eq (car p) 'exists) 
	 (grounding-without-formula-renaming 
	  (maksor (ground-x (second p) (third p) dca)) dca))
        (t p)))

(defun grounding-without-formula-renaming-iterative (p dca)
  "(GROUNDING-WITHOUT-FORMULA-RENAMING-ITERATIVE P DCA) grounds the closed sentence P without using formula renaming.
   Always returns a single sentence p.  Does so without recursion."
  (do ((change t))
      ((not change) p)
    (multiple-value-setq (p change) (ground-something p dca))))

(defun ground-something (p dca)
  "(GROUND-SOMETHING P DCA) grounds one level of quantified sentences if there are any.  Returns
   the new sentence and T if something was grounded (NIL otherwise)."
  (cond ((atom p) (values p nil))
        ((member (car p) '(and or => <= <=> not)) 
         (do ((ps (cdr p) (cdr ps))
              (newp) (change nil) (tmpp) (tmpc))
             ((null ps) (values (cons (car p) (nreverse newp)) change))
           (multiple-value-setq (tmpp tmpc) (ground-something (car ps) dca))
           (when tmpc (setq change t))
           (setq newp (cons tmpp newp))))
        ((eq (car p) 'forall) 
	 (values (maksand (ground-x (second p) (third p) dca)) t))
        ((eq (car p) 'exists) 
	 (values (maksor (ground-x (second p) (third p) dca)) t))
        (t (values p nil))))

(defun ground-x (vs p dca)
  "(GROUND-X VS P DCA) produces a new p for each element in the 
    n-cross-product of dca, where n is the length of VS, the variables, 
    where each one's variables VS have been plugged in for a different element."
  (when (atom vs) (setq vs (list vs)))
  (let (cp result)
    (setq cp (cross-product dca (length vs))) ; universe
    (dolist (c cp)
      (setq result (cons (plug p (nconc (mapcar #'cons vs c) (list '(t . t)))) 
			 result)))
    (nreverse result)))

(defun ground-table (p dca &optional (negs t))
  "(GROUND-TABLE P) takes a biconditional p <=> phi and grounds it: 
   both the positive and negatives."
  ; orient so that the simple relation is on the left.
  (when (complex-sentencep (cadr p)) (setq p `(<=> ,(caddr p) ,(cadr p))))
  
  (let (th pos neg) 
    (cond ((all-positivep (third p)) 
           (setq th (define-theory (make-instance 'theory) "" 
		      (list '(= ?x ?x) `(<= ,(cadr p) ,(caddr p)))))
           (setq pos (viewfinds (second p) (second p) th))
           (empty th))
          (t
           (setq th (define-theory (make-instance 'theory) ""
		      (list* '(= ?x ?x) 
			     `(<= ,(cadr p) ,(caddr p)) 
			     (unique-names-full dca))))
           ; compute all the positive sentences entailed
           (setq pos (viewfinds (cadr p) (cadr p) th))
           (empty th)))
    
    ; compute all the remaining sentences
    (setq neg nil)
    (when negs
      (setq neg (mapcar #'maknot (find-complement (cadr p) dca pos))))

    ; return a single list of the pos and neg atoms
    (setq pos (nconc pos neg))
    pos))

(defun all-positivep (p)
  "(ALL-POSITIVEP P) returns T iff P contains only ands and ors of atomic 
   sentences."
  (cond ((atom p) t)
        ((member (car p) '(and or forall exists)) 
	 (every #'all-positivep (cdr p)))
        ((member (car p) '(=> <= <=> not)) nil)
        (t t)))

(defun grounding-with-full-formula-renaming (p dca &key (toplevel nil) (pol 1))
  "(GROUNDING-WITH-FORMULA-RENAMING-WITH-TEST P DCA) grounds p using DCA
    but uses formula renaming to avoid introducing large disjunctions for 
    existentials inside of universals.  Returns two values: the grounded p 
    and the set of new formula renamings."
  ;(declare (notinline grounding-with-full-formula-renaming))
  (let ((tmpp) (tmpaux))
    (cond ((atom p) (values p nil))
          ((eq (car p) 'forall) 
	   (grounding-with-full-formula-renaming 
	    (maksand (ground-x (second p) (third p) dca)) dca 
	    :toplevel toplevel :pol pol))
          ((eq (car p) 'exists) 
	   (grounding-with-full-formula-renaming 
	    (maksor (ground-x (second p) (third p) dca)) dca 
	    :toplevel toplevel :pol pol))
          ((eq (car p) 'not) 
           (multiple-value-setq (tmpp tmpaux) 
	     (grounding-with-full-formula-renaming 
	      (cadr p) dca 
	      :pol (new-polarity 'not 1 pol)))
           (values (maknot tmpp) tmpaux))
          ((find (car p) '(and or <= => <=>))
           (let ((r (list (gentemp "reln"))) (extra nil) (aux nil) newp)

             ; new relation: no free variables, ever
             ;(setq r (cons r (freevars (cdr p))))   

	     ; for all the subpieces of p, recurse: produces newp, the 
	     ;  replacement for p and aux, a set of new formulas, 
	     ;  already reduced
             (do ((ps (cdr p) (cdr ps))
                  (cnt 1 (1+ cnt))
                  (newpol nil))
                 ((null ps) (setq newp (cons (car p) (nreverse newp)))) 
               (setq newpol 
		     (new-polarity (car p) 
				   (if (and (eq (car p) '=>) (null (cdr ps))) 
				       '* cnt) pol))
               (multiple-value-setq (tmpp tmpaux) 
		 (grounding-with-full-formula-renaming (car ps) dca 
						       :pol newpol))
               (setq newp (cons tmpp newp))
               (setq aux (nconc tmpaux aux)))
 
             ; compute the (partial) definition for that relation
             (cond ((= pol 1) (setq extra `(<= ,newp ,r)))
                   ((= pol -1) (setq extra `(<= ,r ,newp)))
                   ((= pol 0) (setq extra `(<=> ,r ,newp)))
                   (t (setq extra `(uhoh ,p))))
             ;(when (cdr r) (setq extra `(forall ,(cdr r) ,extra)))
             
             ; return the new relation and the extra sentences
             (if toplevel (values newp aux) (values r (cons extra aux)))))
          (t
           (values p nil)))))


;;;;;;;;;;;; utility

(defun new-polarity (conn pos pol)
  "(NEW-POLARITY CONN POS POL) given a logical connective, the position of a literal in the sentence with that connective, and the polarity of the sentence,
   return the polarity of the literal at that position."
  (cond ((find conn '(and or)) pol)
        ((eq conn '=>) (if (eq pos '*) pol (* -1 pol)))
        ((eq conn '<=) (if (eq pos 1) pol (* -1 pol)))
        ((eq conn '<=>) 0)
        ((eq conn 'not) (* -1 pol))
        (t pol)))

(defun cnf-bound (p)
  "(CNF-BOUND P) computes the upper bound on the conjunctive normal form of 
   P, using the obvious algorithm."
  (cond ((atom p) 1)
        ((eq (car p) 'and) (reduce #'+ (mapcar #'cnf-bound (cdr p))))
        ((eq (car p) 'or) (reduce #'* (mapcar #'cnf-bound (cdr p))))
        ((eq (car p) '=>) 
	 (if (cddr p) 
	     (* (cnf-bound (maknot (maksand (cdr (butlast p)))))
		(cnf-bound (car (last p))))
	     (cnf-bound (cadr p))))
        ((eq (car p) '<=) 
	 (if (cddr p)
	     (* (cnf-bound (cadr p)) (cnf-bound (maknot (maksand (cddr p)))))
	     (cnf-bound (cadr p))))
        ((eq (car p) '<=>) 
	 (+ (* (cnf-bound (second p)) (cnf-bound (maknot (third p))))
	    (* (cnf-bound (maknot (second p))) (cnf-bound (third p)))))
        ((find (car p) '(forall exists)) (cnf-bound (third p)))
        ((eq (car p) 'not) (dnf-bound (cadr p)))
        (t 1)))

(defun dnf-bound (p)
  "(DNF-BOUND P) computes the upper bound on the conjunctive normal form of 
   the negation of P, which gives an upper bound on the disjunctive normal 
   form of p."
  (cond ((atom p) 1)
        ((eq (car p) 'and) (reduce #'* (mapcar #'dnf-bound (cdr p))))
        ((eq (car p) 'or) (reduce #'+ (mapcar #'dnf-bound (cdr p))))
        ((eq (car p) '=>) 
	 (if (cddr p)
	     (+ (dnf-bound (maknot (maksand (cdr (butlast p)))))
		(dnf-bound (car (last p))))
	     (dnf-bound (cadr p))))
        ((eq (car p) '<=) 
	 (if (cddr p)
	     (+ (dnf-bound (cadr p)) (dnf-bound (maknot (maksand (cddr p)))))
	     (dnf-bound (cadr p))))
        ((eq (car p) '<=>) 
	 (* (+ (dnf-bound (second p)) (dnf-bound (maknot (third p))))
	    (+ (dnf-bound (maknot (second p))) (dnf-bound (third p)))))
        ((find (car p) '(forall exists)) (dnf-bound (third p)))
        ((eq (car p) 'not) (cnf-bound (cadr p)))
        (t 1)))

(defun find-complement (atom dca set)
  "(FIND-COMPLEMENT ATOM DCA SET) finds all the ground atoms of ATOM using
   DCA that are not in SET."
  (let (complement)
    (dolist (g (cdar (ground-herbrand (quantify atom) dca nil)) complement)
      (when (not (member g set :test #'equal))
        (setq complement (cons g complement))))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
