;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; reformulation.lisp: hodgepodge logical reformulation routines
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun makevariable (sym)
  "(MAKEVARIABLE SYM) returns ?SYM as a symbol."
  (read-from-string (format nil "?~A" sym)))

(defun devariable (var)
  "(DEVARIABLE VAR) returns VAR after removing ?"
  (read-from-string (subseq (symbol-name var) 1)))

(defun variable (sym)
  "(VARIABLE SYM) makes SYM into a variable by prepending ?"
  (tosymbol (list "?" sym)))

(defun varn (n) (intern (format nil "?~A" n)))
(defun varnm (n m) (intern (format nil "?~A-~A" n m)))

(defun new-vars (p)
  (plug p (nconc (mapcar #'(lambda (x) (cons x (newindvar))) (vars p)) '((t . t)))))

(defun lit-to-obj (lit) (read-from-string (lit-to-string lit)))
(defun lit-to-string (lit)
  (cond ((atom lit) (stringappend "+" (atom-to-string lit)))
        ((eq (car lit) 'not) (stringappend "-" (atom-to-string (second lit))))
        (t (stringappend "+" (atom-to-string lit)))))

(defun atom-to-obj (a) (read-from-string (atom-to-string a)))
(defun atom-to-string (a)
  (cond ((atom a) (string a))
        (t (stringappend "_" 
                         (reduce #'(lambda (x y) (stringappend x "_" (atom-to-string y))) 
                                 (cdr a) 
                                 :initial-value (atom-to-string (first a)))
                         "_"))))

(defun saturate (r arity terms)
  "(SATURATE R ARITY TERMS) computes all the possible atoms of arity ARITY with relation
   constant R using TERMS, of which there are TERMS^ARITY."
  (cond ((or (not (numberp arity)) (null terms)) nil)
        ((= arity 0) (list '(r)))
        (t
         (mapcar #'(lambda (x) (cons r x)) (cross-product terms arity)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;; Basic sentence manipulation ;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun equantify (fact)
  "(EQUANTIFY FACT) existentially quantifies all free variables in fact."
  (equantify-except fact nil))

(defun equantify-except (fact vars)
  (let ((vs (set-difference (freevars fact) vars)))
    (if vs
      `(exists ,vs ,fact)
      fact)))
    
(defun quantify (fact)
  "(QUANTIFY FACT) universally quantifies all free variables in fact."
  (let ((vs (freevars fact)))
    (if vs
      `(forall ,vs ,fact)
      fact)))

(defun maksamesign (lit as)
  "(MAKESAMESIGN LIT AS) returns LIT after ensuring it has the same sign as AS."
  (setq lit (drop-not lit))
  (if (negative-literalp as) (maknot lit) lit))

(defun stripnot (p)
  (cond ((atom p) p)
        ((eq (car p) 'not) (cadr p))
        (t p)))

(defun strip-quantifiers (p)
  "(STRIP-QUANTIFIERS P) removes the leading quantifiers from p."
  (cond ((atom p) p)
        ((member (car p) '(forall exists)) (strip-quantifiers (third p)))
        (t p)))

(defun drop-not (p)
  (cond ((atom p) p)
        ((eq (car p) 'not) (second p))
        (t p)))

(defun drop-or (p)
  (cond ((atom p) p)
        ((eq (car p) 'or) (cdr p))
        (t p)))

(defun drop-and (p)
  (cond ((atom p) p)
        ((eq (car p) 'and) (cdr p))
        (t p)))

(defun drop-ands (p)
  "(DROP-ANDS P) turns p into a list of sentences by dropping the outermost 
   ands."
  (cond ((atom p) (list p))
        ((eq (car p) 'and) (mapcan #'drop-ands (cdr p)))
        (t (list p))))

(defun drop-things (p thing &key (test #'eq))
  (cond ((atom p) (list p))
	((funcall test (car p) thing) 
	 (mapcan #'(lambda (x) (drop-things x thing :test test)) (cdr p)))
	(t (list p))))

(defun drop-exists (p)
  (cond ((atom p) p)
	((eq (car p) 'exists) (third p))
	(t p)))

(defun to-orless-list (p) (if (and (listp p) (eq (car p) 'or)) (cdr p) (list p)))
(defun or2list (p) (to-orless-list p))
(defun and2list (p) (if (and (listp p) (eq (car p) 'and)) (cdr p) (list p)))

(defun flatten-operator (p)
  (cond ((atom p) p)
        (t
         (do ((bs (cdr p) (cdr bs))
              (result nil) (again nil))
             ((null bs) (if again (flatten-operator (cons (car p) result)) (cons (car p) result)))
           (cond ((and (listp (car bs)) (eq (caar bs) (car p)))
                  (setq result (nconc result (cdr (car bs))))
                  (when (some #'(lambda (x) (and (listp x) (eq (car x) (car p)))) (cdr (car bs)))
                    (setq again t)))
                 (t
                  (setq result (nconc result (list (car bs))))))))))

(defun orientimp (r p)
  "(ORIENTIMP R P) orients the implication p wrt relation constant r so that the result
   is r => ... or r <= ..."
  (cond ((atom p) p)
        ((not (member (car p) '(<= =>))) p)
        (t
         (let (head body tmp)
           (if (eq (car p) '=>)
             (setq head (car (last p)) body (butlast (cdr p)))
             (setq head (second p) body (cddr p)))
           
           ; ensure both head and body are FOL sentences
           (setq body (maksand body))
           
           ; swap head and body, after negating, if relation of interest is in the body
           (when (eq (relation body) r)
             (setq head (maknot head) body (maknot body))
             (setq tmp body)
             (setq body head)
             (setq head tmp))
           
           ; construct the sentence
           (if (negative-literalp head)
             `(=> ,(maknot head) ,(maknot body))
             `(<= ,head ,body))))))

(defun contras-wo= (p)
  (setq p (brfs p))
  (mapcan #'contras-wo=exp p))

(defun contras-wo=exp (p)
  (let ((res (if (not (eq (relation (head p)) '=)) (list p) nil)))
    ; walk over body
    (dolist (a (cddr p))
      (when (not (eq (relation a) '=))
        (setq res (cons (list* '<= (maknot a) 
			       (cons (maknot (second p)) 
				     (cddr (remove a p :test #'equal)))) res))))
    (if (null res) (list p) res)))


(defun replace-matrix (p matrix)
  "(REPLACE-MATRIX P MATRIX) assumes P is in prenex form and replaces the matrix
   of P with MATRIX without copying MATRIX."
  (cond ((atom p) matrix)
        ((eq (car p) 'forall) 
	 `(forall ,(second p) ,(replace-matrix (third p) matrix)))
        ((eq (car p) 'exists) 
	 `(exists ,(second p) ,(replace-matrix (third p) matrix)))
        (t matrix)))

(defun replace-numbers (p &optional (prefix "tlh"))
  (maptree #'(lambda (x) (if (numberp x)
                             (tosymbol (format nil "~A~A" prefix x))
                             x))
	   p))

(defun propositional-clauses (p)
  "(PROPOSITIONAL-CLAUSES P) converts P to clausal form and converts each
   literal to a propositional literal by dropping all arguments."
  (mapcar #'(lambda (x) (if (atom x) 
			    (signed-relation x)
			    (mapcar #'signed-relation x))) (clauses p)))

(defun mapopands (func p)
  "(MAPOPANDS FUNC P) applies function FUNC to all of the operands of the sentence P and
   returns the result."
  (cond ((atomicp p) (funcall func p))
	((member (car p) '(forall exists)) (list (first p) (second p) (funcall func (third p))))
	(t (cons (car p) (mapcar func (cdr p))))))

(defun transform-theory (f th &optional (saver #'save))
  "(TRANSFORM-THEORY F TH) applies F to all the sentences in TH, putting the
   results in a new theory and returning that theory."
  (let ((newth (make-instance (type-of th))))
    (mapc #'(lambda (y) (let ((newrule (funcall f y))) 
                          (when newrule (funcall saver newrule newth))
                          (if (not newrule) (format t "broken~%"))))
          (sentences '? th))
    newth))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Equality
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun add-equality (th &key (contras nil) (unique-names nil) (dca nil))
  "(ADD-EQUALITY TH) produces the standard equality axioms for th:
   reflexivity, symmetry, transitivity.  Also adds substitution 
   axioms for all the predicates and functions.  Creates a new
   theory and includes it in the original."
  (let ((eqtheory (make-instance 'theory)))
    (define-theory eqtheory "equality axioms" (equality-axioms (contents th) 
                                                               :contras contras
                                                               :unique-names unique-names
                                                               :dca dca))
    (includes eqtheory th)
    eqtheory))

(defun equality-axioms (sents &key (reflex t) (sym t) (trans t) (subst t) (contras nil) 
			(unique-names nil) (dca nil) (univ nil))
  "(EQUALITY-AXIOMS SENTS) returns a list of the equality axioms for 
   the given sentences.  Start with reflexivity, symmetry, and
   transitivity.  Then find the relational substitution axioms.
   Finally find the functional substitution axioms.  Compute the
   contrapositives of the substitution axioms and unique names if required."

  (let* ((vocab (get-vocabulary (maksand sents)))
         (relations (remove-if-not #'isrelation vocab))
         (functions (remove-if-not #'isfunction vocab))  ;get-functions sents)
         (objects (mapcar #'parameter-symbol (remove-if-not #'isobject vocab))) 

         (basic-eq nil) (reln-eq nil) (func-eq nil) (uniq nil) (dcaa nil)
	 (r nil) (s nil) (tr nil))

    (when univ (setq objects (union univ objects)))
    (when reflex (setq r (list '(= ?x ?x))))
    (when sym (setq s (list '(<= (= ?y ?x) (= ?x ?y)))))
    (when trans (setq tr (list '(<= (= ?x ?z) (= ?x ?y) (= ?y ?z)))))
    (setq basic-eq (nconc r s tr))

    (when subst
      (setq reln-eq (mapcan #'relational-substitution-axioms (drop-equality relations)))
      (setq func-eq (mapcan #'functional-substitution-axioms functions)))
    (when unique-names
      (setq uniq (unique-names objects)))
    (when dca 
      (setq dcaa (list (dca objects))))
    (if contras 
      (contrapositives (maksand (nconc basic-eq reln-eq func-eq uniq dcaa)))
      (nconc basic-eq reln-eq func-eq uniq dcaa))))

(defun drop-equality (relations)
  "(DROP-EQUALITY RELATIONS) returns all those relations that are not ="
  (remove '= relations :key #'parameter-symbol))

(defun relational-substitution-axioms (relation)
  "(RELATIONAL-SUBSTITUTION-AXIOMS RELATION) returns the relational substitution
   axioms for the relation object RELATION.  A relation object has a name and an
   arity.  Thus (p 1) will give (<= (p ?y) (p ?x) (= ?x ?y))."

  ; loop over each argument, producing one substitution axiom per iteration.
  (do ((a 0 (+ a 1))
       (axioms nil)  ; axioms is the set of all substitution axioms
       (axiom nil)   ; axiom will be one subst axiom
       (head (build-reln relation))     ; head of the axiom
       (var (newindvar)) ; new variable
       (diffhead nil)) ; slightly altered head
      ((= a (parameter-arity relation)) axioms)
    (setq diffhead (substitute var (nth (+ a 1) head) head))
    (setq axiom `(<= ,head ,diffhead (= ,var ,(nth (+ a 1) head))))
    (setq axioms (cons axiom axioms))))

(defun build-reln (reln)
  "(BUILD-RELN RELN) takes a relation with name p and arity n and produces
   an atom with n distinct variables for relation p."
  (build-named-var-list (parameter-symbol reln) (parameter-arity reln)))

(defun functional-substitution-axioms (function)
  "(FUNCTIONAL-SUBSTITUTION-AXIOMS FUNCTION) returns the functional substitution
   axioms for the functional object.  A functional object has a name and an
   arity.  Thus (f 1) will give (<= (= (f ?y) (f ?x)) (= ?x ?y))."

  ; loop over each argument, producing one substitution axiom per iteration.
  (do ((a 0 (+ a 1))
       (axioms nil)  ; axioms is the set of all substitution axioms
       (axiom nil)   ; axiom will be one subst axiom
       (head (build-func function)) 
       (var (newindvari)) ; new variable
       (diffhead nil)) ; slightly altered head
      ((= a (parameter-arity function)) axioms)
    (setq diffhead (substitute var (nth (+ a 1) head) head))
    (setq axiom `(<= (= ,head ,diffhead) (= ,var ,(nth (+ a 1) head))))
    (setq axioms (cons axiom axioms))))

(defun build-func (func)
  "(BUILD-FUNC FUNC) takes a function with name p and arity n and produces
   an term with n distinct variables for function p."
  (build-named-var-list (parameter-symbol func) (parameter-arity func)))

(defun build-named-var-list (name arity)
  "(BUILD-NAMED-VAR-LIST NAME ARITY) returns a list whose head is NAME and
  whose body contains ARITY distinct new variables."
  (do ((i 0 (+ i 1))
       (l nil))
      ((= i arity) (cons name l))   ; no need to reverse--body is a list of new variables.
    (setf l (cons (newindvari) l))))

(defun newindvari () (gentemp "?"))

(defun dca (names)
  "(DCA names) returns the domain closure axiom in kif for all the NAMES."
  (list 'forall '?x (cons 'or (mapcar #'(lambda (y) `(= ?x ,y)) names))))

(defun compute-dca (th)
  "(COMPUTE-DCA THEORY) finds the list of all object constants in THEORY."
  (objs (maksand (contents th))))

(defun unique-names (names)
  "(UNIQUE-NAMES NAMES) returns unique names axioms in kif for all the names."
  (do ((n names (cdr n))
       (result nil))
      ((null n) (reverse result))
    (do ((nn (cdr n) (cdr nn)))
        ((null nn))
      (setq result (cons `(not (= ,(car n) ,(car nn))) result)))))

(defun unique-names-full (names)
  "(UNIQUE-NAMES-FULL NAMES) returns unique names axioms in kif for all the names
   for all pairs of names.  The non-full version only produces the triangularization."
  (do ((n names (cdr n))
       (result nil))
      ((null n) (reverse result))
    (do ((nn (cdr n) (cdr nn)))
        ((null nn))
      (setq result (cons `(not (= ,(car n) ,(car nn))) result))
      (setq result (cons `(not (= ,(car nn) ,(car n))) result)) 
      )))

(defun one-less (names)
  "(ONE-LESS NAMES) picks off the first of NAMES and returns the 
   sentence that says it must equal one of the remaining names."
  (maksor (mapcar #'(lambda (x) `(= ,(car names) ,x)) (cdr names))))

(defun propagate-equality (l vars)
  "(PROPAGATE-EQUALITY L VARS) takes a list of literals L and a list of variables VARS.
   It replaces equals for equals in the usual way until a fixed point is reached,
   while never removing a variable in VARS."
  (assert (and (listp l) (listp vars)) nil "propagate-equality takes two lists")

  ; first copy l so we can destructively modify it
  (propagate-equality-aux (copy-tree l) vars))

(defun propagate-equality-aux (l vars)
  ; find the first substitution: (= x y) means replace x with y (modulo VARS)
  (let (tgt)
    (setq tgt (first-result #'(lambda (x) (orientedrewrite x vars)) l))
    (if tgt
      (propagate-equality-aux (remove-if #'(lambda (x) (and (listp x) (eq (car x) '=) (equal (second x) (third x))))
                                         (nsubst (third tgt) (second tgt) l))
                              vars)
      l)))

(defun orientedrewrite (lit vars)
  (cond ((atom lit) nil)
        ((not (eq (car lit) '=)) nil)
        ((and (varp (second lit)) (not (member (second lit) vars))) lit)
        ((and (varp (third lit)) (not (member (third lit) vars))) `(= ,(third lit) ,(second lit)))
        (t nil)))

(defun delete= (lits)
  "(DELETE= LITS) takes a list of literals and returns a logically equivalent list of literals
   without any literals of the form t = u where either t or u is a variable 
   (and the binding list as a second value).  Destructive."
  (let (l var val)
    ; find a substitution to perform
    (setq l (find-if #'(lambda (x) (and (positive-literalp x) (eq (relation x) '=) 
					(or (varp (second x)) (varp (third x)))))
		     lits))
    (cond ((not l) (values lits '((t . t))))
	  (t 
	   (if (varp (second l)) 
	       (setq var (second l) val (third l)) 
	       (setq var (third l) val (second l)))
	   (multiple-value-bind (newlits bl) (delete= (nsubst val var (delete l lits :test #'equal)))
	     (values newlits (acons var val bl)))))))

(defun equals-for-equals (lits &optional (vars nil))
  "(EQUALS-FOR-EQUALS LITS VARS) removes all x=y in the literals LITS by substituting y for x.
   Ensures never to replace any of VARS.  Destructive."
  ; build substitution list and plug that list in.
  (let ((bl nil))
    (dolist (l lits)
      (when (and (positive-literalp l) (eq (relation l) '=))
	(cond ((member (second l) vars) 
	       (unless (or (member (third l) vars)
			   (not (varp (third l))))
		 (push (cons (third l) (second l)) bl)))
	      ((member (third l) vars) 
	       (unless (not (varp (second l)))
		 (push (cons (second l) (third l)) bl)))
	      ((varp (second l)) (push (cons (second l) (third l)) bl))
	      ((varp (third l)) (push (cons (third l) (second l)) bl)))))
      (values (delete '(= ?x ?x) (plug lits (setq bl (nreverse (cons '(t . t) bl)))) :test #'matchp) bl)))

#|
;;;; remove ground equality -- untested

(defun remove-ground-equality (p)
  "(REMOVE-GROUND-EQUALITY P) returns a version of P where all the ground
   equality is stripped.  Assumes all equality is ground, no quantifiers, and UNA."
  (cond ((atom p) p)
        ((eq (car p) '=) (if (eq (second p) (third p)) 'true 'false))
        ((eq (car p) 'not) (maknot (remove-ground-equality (cadr p))))
        ((eq (car p) 'and) (simplify-and (mapcar #'remove-ground-equality p)))
        ((eq (car p) 'or) (simplify-or (mapcar #'remove-ground-equality p)))
        ((eq (car p) '<=)
         (let ((newlits (mapcar #'remove-ground-equality (cdr p))))
           (cond ((eq (car newlits) 'true) 'true)
                 ((eq (car newlits) 'false) (simplify-or (mapcar #'maknot (cdr newlits))))
                 (t 
                  (let ((newbod (simplify-and (cdr newlits))))
                    (cond ((eq newbod 'true) (car newlits))
                          ((eq newbod 'false) 'false)
                          (t (list* '<= (car newlits) newbod))))))))

        ((eq (car p) '=>) 
         (let ((newp (remove-ground-equality `(<= (car (last p)) (cdr (butlast p))))))
           (list* '=> (append (cddr newp) (cadr newp)))))

        ((eq (car p) '<=>)
         (let ((first (remove-ground-equality (second p)))
               (second (remove-ground-equality (third p))))
           (cond ((and (find first '(true false)) (find second '(true false)) (not (eq first second))) 'false)
                 (t `(<=> ,first ,second)))))))

(defun simplify-and (ps)
  (let ((newps nil))
    (dolist (p ps (if newps (maksand (nreverse newps)) 'true))
      (cond ((eq p 'true))
            ((eq p 'false) (return 'false))
            (t (setq newps (cons p newps)))))))

(defun simplify-or (ps)
  (let ((newps nil))
    (dolist (p ps (if newps (maksor (nreverse newps)) 'false))
      (cond ((eq p 'true) (return 'true))
            ((eq p 'false))
            (t (setq newps (cons p newps)))))))

|#

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Simplification by tautology/subsumption
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun herbrand-contract (rule)
  "(HERBRAND-CONTRACT RULE) returns NIL if RULE is a tautology in Herbrand logic;
    otherwise, returns a possibly simplified rule that is logically equivalent
    according to Herbrand semantics."
  (cond ((and (listp rule) (cdr rule) (cddr rule) (eq (car rule) '<=))
         (herbrand-contract-antecedents rule))
        (t nil)))

(defun herbrand-contract-antecedents (rule)
  (let ((pos (find-positive (cddr rule))))
    (do ((ants (cddr rule) (cdr ants))
         (newrule (list (head rule) '<=))
         (env '((t . t))) (a1) (a2))
        ((or (null ants) (not newrule)) (nreverse newrule))
      ;(format t "~A: ~A~%" newrule env)
      (cond ((positive-literalp (car ants))
             (cond ((eq (relation (car ants)) '=)
                    (setq a1 (plug (second (car ants)) env))
                    (setq a2 (plug (third (car ants)) env))
                    (cond ((varp a1) (setq env (acons a1 a2 env)) (setq newrule (cons (car ants) newrule)))
                          ((varp a2) (setq env (acons a2 a1 env)) (setq newrule (cons (car ants) newrule)))
                          ((equal a1 a2))  ; removing this from the rule, as it is always true.
                          (t (setq newrule nil))))
                   (t (setq newrule (cons (car ants) newrule)))))
            (t (cond ((member (cadr (car ants)) pos :test #'equal) (setq newrule nil))
                     ((eq (relation (car ants)) '=)
                      (setq a1 (plug (second (cadr (car ants))) env))
                      (setq a2 (plug (third (cadr (car ants))) env))
                      ;(format t "a1: ~A, a2: ~A~%" a1 a2)
                      (cond ((and (groundp a1) (groundp a2))
                             (when (equal a1 a2) (setq newrule nil))) ; if not equal, don't need to add to the rule
                            (t (setq newrule (cons (car ants) newrule)))))
                     (t (setq newrule (cons (car ants) newrule)))))))))
                              

(defun subsumption-elimination (th)
  (nreverse 
   (subsumption-elimination-forward
    (nreverse 
     (subsumption-elimination-forward 
      (copy-list (contents th)))))))

; CAUTION: destructive to TH, which must be a list
(defun subsumption-elimination-forward (th)
  (do ((ps th (cdr ps)))
      ((null ps) th)
    (do ((tosubsumes (cdr ps) (cdr tosubsumes))
	 (pointer ps))
	((null tosubsumes))
      ; when subsumed, remove from list directly
      ; only increment pointer when not deleting; otherwise, pointer=tosubsumes
      (cond ((similarp (car tosubsumes) (car ps))
	     (setf (cdr pointer) (cdr tosubsumes)))
	    (t (setq pointer (cdr pointer)))))))

(defun similarp (x y)
  "(SIMILARP X Y) returns T iff X is subsumed by Y, i.e. there is a variable 
   assignment sigma such that Y.sigma is a subset of X."
  (cond ((not (and (consp x) (consp y))) nil) 
        (*subsumption* (subsumptionp y x))
        (t (samep x y))))

(defun subsumptionp (p q)
  (subsumptionps p q truth))

(defun subsumptionps (pl ql al)
  (cond ((null pl) al)
        (t (do ((m ql (cdr m)) (bl))
               ((null m))
             (if (and (setq bl (subsumpmatch (car pl) (car m) al))
                      (setq bl (subsumptionps (cdr pl) ql bl)))
               (return bl))))))

(defun subsumpmatch (p q al)
  (setq *matches* (1+ *matches*))
  (matchpexp p q al))

(defun tautologyelim-constraints (lit1 lit2)
  "(TAUTOLOGYELIM-CONSTRAINTS LIT1 LIT2) given 2 complementary literals,
  construct a set of equality constraints that guarantee lit1 and lit2
  will never be instantiated as tautologies (if possible).  Returns a 
  single disjunction."
  (when (negative-literalp lit1) (setq lit1 (second lit1)))
  (when (negative-literalp lit2) (setq lit2 (second lit2)))
  (maksor (mapcarnot #'(lambda (x y) (if (and (not (varp x)) (not (varp y))) 
					 nil 
					 (maknot `(= ,x ,y))))
		     (cdr lit1) (cdr lit2))))

(defun isolate-relations (p relns)
  "(ISOLATE-RELATIONS P RELNS) takes a closed sentence P and a list of relation constants RELNS.
   It constructs an equivalent sentence comprising two subformulas (phi @x) and (psi @y) such that
   phi contains only relation constants in RELNS.  Possible return forms.
   (forall @x (and/or (phi @x) (psi @x)))
   (exists @x (and/or (phi @x) (psi @x)))
   (and/or (phi @x) (psi @x))
   Attempts to maximize phi.  
   Result is always logically equivalent to P.  If cannot construct such a phi, uses True."
  (isolate-relations-aux (nnf p) relns))

(defun isolate-relations-aux (p relns)
  (setq p (flatten-operator p))
  ; warning: don't use makand with True, as the True will disappear
  (cond ((literalp p) (if (member (relation p) relns) (list 'and p 'true) (list 'and 'true p)))
	((member (car p) '(forall exists))
	 (if (member (signifier (third p)) '(forall exists))
	     p
	     (list (first p) (second p) (isolate-relations (third p) relns))))

	((member (car p) '(and or)) 
	 (let (vocabs isolated others)
	   (setq vocabs (mapcar #'(lambda (x) (list (relns x) x)) (cdr p)))
	   (multiple-value-setq (isolated others) (split #'(lambda (x) (subsetp (first x) relns)) vocabs))
	   (setq isolated (mapcar #'second isolated))
	   (setq others (mapcar #'second others))
	   (if (eq (car p) 'and)
	       (list 'and (maksand isolated) (maksand others))
	       (list 'or (maksor isolated) (maksor others)))))
	(t p)))

(defun toimp (p)
  "(TOIMP P) By only changing the top-level operator in P, returns
   a sentence equivalent to P but written as an implication.
   Defaults to (=> True P)."
  (cond ((literalp p) `(=> true ,p))
	((eq (car p) 'or)
	 (do ((ds (cdr p) (cdr ds))
	      (result nil))
	     ((null ds) (cons '=> (nreverse result)))
	   (if (null (cdr ds))
	       (push  (car ds) result)
	       (push (maknot (car ds)) result))))
	(t `(=> true ,p))))

(defun toand (p)
  "(TOAND P) By only changing the toplevel operator in P, returns 
   a sentence equivalent to P but written as a conjunction.
   Defaults to (and True P)."
  (cond ((literalp p) `(and true ,p))
	((eq (car p) 'and) p)
	(t `(and true ,p))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Coersion to Horn/negative-horn 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun rewrite-if-horn (p th)
  "(REWRITE-IF-HORN P TH) checks if all the rules in TH are essentially horn clauses.
   If Horn and P is positive, writes all rules in TH in positive form.  If essentially
   Horn, i.e. changing all signs produces Horn, and P is negative, rewrites all rules
   in TH in negative form."
  (let ((newth)
        (horn-type (horn-type th)))

    ; set newth to a theory where we can avoid contrapositives if possible 
    (cond ((and (is-horn horn-type) (not (negative-literalp p)))
           (setq newth (transform-theory #'to-horn-rule th)))
          ((and (is-negative-horn horn-type) (negative-literalp p))
           (setq newth (transform-theory #'to-negative-horn-rule th)))
          (t (setq newth th)))
    
    ; return values
    (values horn-type p newth))
)

(defun to-horn-rule (rule &optional (ignorepreds nil))
  "(TO-HORN-RULE RULE) assuming rule is a definite Horn clause (while ignoring literals including 
    pred from IGNOREPREDS), returns the contrapositive of RULE where all literals are positive 
    (except for IGNOREPREDS)."
  (cond ((atom rule) rule)
	((not (eq (car rule) '<=)) rule)
	((and (positive-literalp (head rule)) 
	      (not (member (relation (head rule)) ignorepreds :key #'parameter-symbol)))
	 rule)
	(t
	 (do ((ls (body rule) (cdr ls))
	      (newhead nil)
	      (newbody (list (maknot (head rule)))))
	     ((null ls))
	   (cond ((and (negative-literalp (car ls))   ; found the head
		       (not (member (relation (car ls)) ignorepreds :key #'parameter-symbol)))
		  (setq newhead (maknot (car ls)))
		  (setq newbody (nconc (nreverse newbody) (cdr ls)))
		  (return (list* '<= newhead newbody)))
		 (t
		  (push (car ls) newbody)))))))

(defun to-negative-horn-rule (rule)
  "(TO-NEGATIVE-HORN-RULE RULE) returns the contrapositive of RULE where all literals are negative
    or NIL if that is not possible."
  (cond ((negative-literalp (head rule)) rule)   ; already done if horn
        ((and (positive-literalp (head rule)) (every #'negative-literalp (cddr rule))) nil)
        (t
         ; otherwise, swap the negative literal on the right with the negative literal in the head.
         (let ((negbody nil) (posbodylit nil))
           (do ((ls (cddr rule) (cdr ls)))
               ((null ls))
             (cond ((positive-literalp (car ls))
                    (setq posbodylit (car ls))
                    (setq negbody (cons (maknot (cadr rule)) negbody)))
                   (t
                    (setq negbody (cons (car ls) negbody)))))
           (cons '<= (cons (maknot posbodylit) negbody))))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Conjunct ordering 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun order-conjuncts (p orderer &rest args)
  (cond ((or (atom p) (not (eq (car p) '<=)) (null (cddr p))) p)
        (t
         (cons '<= (cons (cadr p) (apply orderer (cddr p) args))))))


;; put extensionally defined conjuncts before other conjuncts
(defun order-conjuncts-first (literals set)
  (let (p1 p2)
    (multiple-value-setq (p1 p2) (split #'(lambda (x) (member (signed-relation x) set :test #'equal)) literals))
    (nconc p1 p2)))


(defun order-datalog-conjuncts (p hashedtypes &optional (defaulttype 'univ) (attach nil) (bound nil))
  "(ORDER-DATALOG-CONJUNCTS P HASHEDTYPES) takes a list of literals P, a hash 
   table keyed on predicates, and optionally a DEFAULTTYPE.  Returns P where
   the negative literals may be reordered, possibly with additional type guards 
   inserted that guarantees left-to-right evaluation always results in ground
   negative literals at evaluation time, as long as DEFAULTTYPE is non-nil.  
   The p entry in HASHEDTYPES is a dotted
   pair of the form ((p ?x1 ... ?xn) . (q ?x1 ... ?xm)).  ATTACH is a list of 
   predicates that should be treated as though they were negatives.  BOUND is a list
   of variables that are known to be bound at evaluation time."
  (let (pos neg poshash neghash res vs typeexpr bl needsguardp)
    (setq needsguardp #'(lambda (x) (or (negative-literalp x) (member (relation x) attach))))
    (setq poshash (make-hash-table))
    (setq neghash (make-hash-table))
    (multiple-value-setq (neg pos) (split needsguardp p))
    (setq res nil)
    ; hash neg on all variables, and if no vars in neg literal, add at front.
    (dolist (n neg)
      (setq vs (set-difference (vars n) bound))
      (if (null vs)
	  (push n res)
	  (dolist (v vs)
	    (setf (gethash v neghash) (cons n (gethash v neghash))))))

;    (format t "result after initial construction: ~A~%" res)
;    (format t "pos: ~A~%" pos)
;    (format t "neg: ~A~%" neg)
;    (pretty-print neghash)

    ; create new conjunct ordering by adding positive literals and negative literals
    ;   that the positive literals make safe.
    (dolist (v bound) (setf (gethash v poshash) t))
    (dolist (p pos)
      ; add positive conjunct and mark all added variables
      (push p res)
      (setq vs (vars p))  ;(setq vs (union (vars p) bound))
      (dolist (v vs)
	(setf (gethash v poshash) t))
      ; add all negative literals whose variables were just positively marked
      (dolist (v vs)
	(dolist (n (gethash v neghash))
	  (when (and (every #'(lambda (x) (gethash x poshash)) (vars n))
		     (member n neg))
	    (push n res)
	    (setq neg (delete n neg))))))


;    (format t "result after adding positive conjuncts: ~A~%" res)
;    (format t "pos: ~A~%" pos)
;    (pretty-print poshash)
;    (format t "neg: ~A~%" neg)
;    (pretty-print neghash)

    ; for each remaining negative literal, add type guards.
    ;  Want to apply univ only as a last resort.  So walk over the
    ;  remaining literals and if we have types for all arguments,
    ;  add that literal.  Keep iterating over set until no
    ;  changes are made.
    (setq neg (set-difference neg (remove-if-not needsguardp res)))
    (do ((change t) (toremove nil nil)) 
	((not change))
      (setq change nil)
      (dolist (n neg)
	(setq vs (vars n))
	(cond ((every #'(lambda (x) (gethash x poshash)) vs) 
	       (push n res)
	       (push n toremove))
	      ((setq typeexpr (gethash (relation n) hashedtypes))
	       (setq typeexpr (stdize typeexpr))
	       (setq bl nil)
	       (when typeexpr (setq bl (mgu (first typeexpr) (drop-not n))))
	       (when bl
		 (push (plug (cdr typeexpr) bl) res)
		 (push n res)
		 (setq change t)
		 (mapc #'(lambda (x) (setf (gethash x poshash) t)) vs)
		 (push n toremove)))))
      (setq neg (delete-if #'(lambda (x) (member x toremove)) neg)))

    ; all of the neg that remain have no types: use univ
    (when defaulttype
      (dolist (n neg)
	(dolist (v (vars n))
	  (unless (gethash n poshash)
	    (push (list defaulttype v) res)))
	(push n res)))
    (nreverse res)))


; cp take the form of (+ c c p)
; functions to extract for flexibility

#|
;; unfinished -- once we have the variables ordered how do we then order the conjuncts?

(defun order-conjuncts-by-producer-consumer (literals cps)
  (declare (ignore literals) (ignore cps))
  literals)

(defun consumer-producer-symbol (cp) (signed-relation cp))
(defun consumer-producer-list (cp) (arguments cp))

(defun build-variable-dependencies (lits cps)
  "(BUILD-VARIABLE-DEPENDENCIES LITS CPS) returns a graph that depicts the variable dependencies
   that exist in LITS, according to the consumer-producer info in CPS."
  (let ((g (make-graph)))
    (mapc #'(lambda (x) (add-variable-dependency x cps g)) lits)
    g))

(defun add-variable-dependency (literal cps graph)
  "(ADD-VARIABLE-DEPENDENCY LITERAL) adds all the edges to GRAPH for variable
   dependencies in LITERAL.  An edge from x to y means computing x requires having computed y."
  (let ((cp (find (signed-relation literal) cps :key #'consumer-producer-symbol))
        (producers) (consumers))
    (multiple-value-setq (producers consumers) (split-cp (arguments literal) (consumer-producer-list cp)))

    (dolist (p producers)
      (dolist (c consumers)
        (graph-insert-edged c p graph)))))


(defun split-cp (args cps)
  "(SPLIT-CP ARGS CPS) returns two lists: the arguments in ARGS corresponding to Cs in 
   CPS and all other args."
  (do ((as args (cdr as))
       (cs cps (cdr cs))
       (cargs nil)
       (pargs nil))
      ((or (null as) (null cs)) (if (or (not (null as)) (not (null cs))) 
                                  (values nil nil) 
                                  (values (nreverse cargs) (nreverse pargs))))
    (if (eq (car cs) 'c)
      (setq cargs (cons (car as) cargs))
      (setq pargs (cons (car as) pargs)))))

|#


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Poss ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; The poss reformulation constructs complete definitions for poss(phi, xbar) such that all 
;    tbar such that phi(tbar) is consistent with the background theory are entailed
;    for poss(phi,xbar).  And only those tbar are entailed.

; creating/analyzing poss (and impl) sentences.  impl(phi,xbar) is the same as poss but captures
;    all those tbar such that phi(tbar) is entailed by the background theory.  Obviously
;    poss reformulation is sufficient for impl reformulation (by negating twice).
(defun possp (p) (eq (relation (head p)) 'poss))
(defun poss-sent (p) (second (second p)))
(defun poss-args (p) (cddr p))
(defun makposs (p vars &optional (quotefn #'quotify))
  (list* 'poss (funcall quotefn p) (intersection (freevars p) vars)))

(defun implp (p) (eq (relation (head p)) 'impl))
(defun impl-sent (p) (poss-sent p))
(defun impl-args (p) (poss-args p))
(defun makimpl (p vars &optional (quotefn #'quotify))
  (list* 'impl (funcall quotefn p) (intersection (freevars p) vars)))

(defun inconp (p) (eq (relation (head p)) 'inconsistent))
(defun makincon (p vars &optional (quotefn #'quotify))
  (list* 'inconsistent (funcall quotefn p) (intersection vars (freevars p))))
(defun incon-sent (p) (second (second p)))
(defun incon-args (p) (cddr p))

; need to deal with quoted variables at some point
(defun rewrite-quoted-escaping-vars (p)
  (cond ((atom p)  p)
        ((eq (car p) 'quote) (quotify-escaping-vars (second p)))
        (t (mapcar #'rewrite-quoted-escaping-vars p))))

(defun quotify-escaping-vars (p)
  (cond ((varp p) p)
        ((atom p) (quotify p))
        (t (cons 'listof (mapcar #'quotify-escaping-vars p)))))

;;;;;;; Finding poss predicates
(defun find-posses (p)
  "(FIND-POSSES P) returns a list of all the poss atoms in p."
  (cond ((atom p) nil)
        ((member (first p) '(and or not => <= <=> forall exists)) 
	 (mapunion #'find-posses (cdr p) :test #'samep))
        ((eq (car p) 'poss) (list p))
        (t nil)))

;;;;;;; Simplify poss

(defun simplify-poss-paper (p)
  "The version of simplify-poss used in the what-to-how? paper.  Assumes no =>, <=, or <=>.  
   Also assumes negations are pushed all the way in."
  (declare (notinline simplify-poss-paper))
  (setq p (poss-sent p))
  (cond ((eq (car p) 'exists) 
	 `(exists ,(second p) ,(simplify-poss-paper (makposs (third p) (freevars (third p))))))
        ((eq (car p) 'or) `(or ,(simplify-poss-paper (makposs (second p) (freevars (second p)))) 
                               ,(simplify-poss-paper (makposs (third p) (freevars (third p))))))
        ((and (eq (car p) 'and) (indep-paperp (second p) (third p)))
         `(and ,(simplify-poss-paper (makposs (second p) (freevars (second p)))) 
               ,(simplify-poss-paper (makposs (third p) (freevars (third p))))))
        ((complete-paperp p) p)
        (t (makposs p (freevars p)))))

(defun indep-paperp (p q)
  (or (complete-paperp p) (complete-paperp q)))

(defun complete-paperp (p)
  (let ((comppreds (list (make-parameter :symbol 'p :arity 2 :type 'relation) 
                         (make-parameter :symbol 'q :arity 1 :type 'relation))))
    (subsetp (preds p) comppreds :test #'equalp)))



(defun simplify-poss (p th crelns)
  "(POSS P TH CRELNS) walks sentence P and distributes poss based on the
   relations with complete definitions CRELNS and the properties of the sentences
   TH, where the definitions for CRELNS have been removed from TH."
  (simplify-poss-aux p (agraph-connected-components (undirected-dependency-graph th crelns) :test #'equalp) crelns))

(defun simplify-poss-aux (p components crelns)
  (cond ((atom p) p)
        ((member (car p) '(and or not => <= <=> forall exists))
         (cons (car p) (mapcar #'(lambda (x) (simplify-poss-aux x components crelns)) (cdr p))))
        ((eq (car p) 'poss) (simplify-poss-poss p components crelns))
        (t p)))

(defun simplify-poss-poss (p components crelns)
  "(SIMPLIFY-POSS-POSS P components CRELNS) simplifies a poss sentence using 
   the properties of poss."
  (declare (notinline simplify-poss-poss))
  (assert (and (listp p) 
               (eq (car p) 'poss)
               (listp (second p))
               (eq (car (second p)) 'quote))
          nil "simplify-poss-poss requires a (poss 'phi ?x1 ... ?xn) sentence.")
  (let (q tmp)
    ; the sentence
    (setq q (second (second p)))
    (cond ((atom q) (if (member q crelns :key #'parameter-symbol) q p))

          ; poss doesn't distribute over a universal quantifier unless all the preds 
	  ;  in the sentence are complete
          ((eq (car q) 'forall)
           (if (subsetp (preds q) crelns :test #'equalp)
             q
             p))

          ; try to push negations in.  
          ; If successful, recurse; if not, check for completeness and if complete, 
	  ;   drop poss, else return p
          ((eq (car q) 'not)
           (setq tmp (nnf q))
           (cond ((eq (car tmp) 'not)
                  (if (subsetp (preds q) crelns :test #'equalp)
                    (makposs q (cddr p))
                    (makposs tmp (cddr p))))
                 (t (simplify-poss-poss (makposs tmp (cddr p)) components crelns))))

          ; if imps, drop imps and recurse
          ((member (car q) '(<= => <=>))
           (simplify-poss-poss (makposs (noimps q) (cddr p)) components crelns))

          ; if conjunction, a little more involved
          ((eq (car q) 'and) (simplify-poss-poss-and (flatten-operator q) (cddr p) components crelns))

          ; poss"phi(xbar) V psi(ybar)"(zbar) becomes poss"phi(xbar)"(xbar) V poss"phi(ybar)"(ybar)
          ((eq (car q) 'or) (simplify-poss-aux (cons 'or (mapcar #'(lambda (x) (makposs x (cddr p)))
                                                                  (cdr q)))
                                               components
                                               crelns))
          ; poss"Exbar.phi(xbar,ybar)"(ybar) becomes Exbar.poss"phi(xbar,ybar)"(ybar,xbar)
          ((eq (car q) 'exists) 
           (if (listp (second q)) (setq tmp (second q)) (setq tmp (list (second q))))
           (simplify-poss-aux `(exists ,(second q) ,(makposs (third q) (append (cddr p) tmp)))
                               components 
                               crelns))
          (t (if (member (relation q) crelns :key #'parameter-symbol) q p)))))

(defun simplify-poss-poss-and (p vars components crelns)
  "(SIMPLIFY-POSS-POSS-AND P VARS COMPONENTS CRELNS) returns a statement that is logically equivalent to 
   (poss p vars) when P is a conjunction of sentences.  Takes completeness and independence into account
   via CRELNS and COMPONENTS."
  (assert (and (listp p) (eq (car p) 'and)) nil 
	  "simplify-poss-poss-and takes a conjunction of sentences.")
  (let (pcs bins)

    ; grab the preds for each conjunct: ((preds1 . conjunct1) (preds2 . conjunct2) ...)
    (setq pcs (mapcar #'(lambda (x) (cons (preds x) x)) (cdr p)))
    ; greedy algorithm: bin together all pcs where one of the preds is in the same 
    ;   component as a pred from another pc.
    ; those pcs in the same bin are dependent on one another
    (setq bins nil)
    (do ((ps pcs (cdr ps))
         (inbin nil nil))
        ((null ps))
      (dolist (b bins)
        (when (some #'(lambda (x y) (not (independentp x y components crelns))) (car (car ps)) (car b))
          (setf (car b) (union (car (car ps)) (car b) :test #'equalp))
          (setf (cdr b) (makand (cdr (car ps)) (cdr b)))
          (setq inbin t)
          (return)))
      (when (not inbin) (setq bins (cons (car ps) bins))))
    ; cleanup bins
    (dolist (b bins)
      (setf (cdr b) (maksand (nreverse (drop-and (flatten-operator (cdr b)))))))
    (setq bins (nreverse bins))

    ; once we've binned sentences, return poss of each one
    (maksand (mapcar #'(lambda (x) (makposs (cdr x) vars)) bins))))

    
#|
    ; as long as there are at least two bins, recurse
    ;    otherwise, just return
    (if (cdr bins)
      (simplify-poss-aux p components crelns)
      p)))
|#

   
;;;;;;;;; Compute oneof definitions

(defun oneof-definition (size number)
  "(ONEOF-DEFINITION SIZE NUMBER) returns definitions for 
     (oneof ?x11 ... ?x1(size) ... ?xnumber1 .... ?x(number)(size))"
  (let (body head tmp)

    ; construct the body and input arguments to head
    (setq head nil)
    (setq body nil)
    (dotimes (i number)
      (setq tmp nil)
      (dotimes (j size)
        (setq tmp (cons `(= ,(varn j) ,(varnm i j)) tmp))
        (setq head (cons (varnm i j) head)))
      (setq body (cons (maksand (nreverse tmp)) body)))
    
    ; finish off the head
    (dotimes (i size)
      (setq head (cons (varn i) head)))

    `(<=> ,(cons 'oneof (nreverse head)) ,(maksor body))))

;;;;;;;;; Compute poss definitions

(defun poss-definitions (posses incomp crelns)
  (let (partitions defns partition-preds)
    (setq partitions (partition-by-independence incomp crelns))
    (setq partition-preds (mapcar #'(lambda (x) (mapunion #'preds x :test #'equalp)) partitions))
    (dolist (p posses)
      (setq defns (nconc (poss-definition p 
					  (find-partitions (preds (poss-sent p)) 
							   (mapcar #'cons partition-preds partitions)) 
					  crelns) 
			 defns)))
    defns))

; here we work at generating a small search space by inferring types, etc.
(defun poss-definitions2 (posses incomp crelns)
  (let (partitions defns partition-preds)
    (setq partitions (partition-by-independence incomp crelns))
    (setq partition-preds (mapcar #'(lambda (x) (mapunion #'preds x :test #'equalp)) partitions))
    (dolist (p posses)
      (setq defns (nconc (poss-definition2 p 
					  (find-partitions (preds (poss-sent p)) 
							   (mapcar #'cons partition-preds partitions)) 
					  crelns) 
			 defns)))
    defns))

(defun find-partitions (preds alist)
  (do ((parts nil)
       (al alist (cdr al)))
      ((null al) parts)
    (when (some #'(lambda (x) (member x (caar al) :test #'equalp)) preds)
      (setq parts (cons (cdar al) parts)))))

(defun poss-definition (p partitions crelns)
  "(POSS-DEFINITION P PARTITIONS) computes a logical definition for the poss atom P, which
  is constrained by the partitions PARTITIONS in a theory with complete definitions for CRELNS."
  ; build a single set of sentences out of the partitions (need to copy here, not modify)
  (let (ttype qtype)
    (setq partitions (apply #'concatenate 'list partitions))
    (setq ttype (get-theory-type partitions))
    (setq qtype (get-poss-type p))
    (cond ((and (eq ttype 'quantifier-free) (eq qtype 'quantifier-free)) (poss-qf-qf p partitions crelns))
          (t (assert nil nil (format nil "Don't know how to define ~A when sentences are ~%~A~%~%" p partitions))))))
    
; compute types, etc. to shrink search space
(defun poss-definition2 (p partitions crelns)
  "(POSS-DEFINITION P PARTITIONS) computes a logical definition for the poss atom P, which
  is constrained by the partitions PARTITIONS in a theory with complete definitions for CRELNS."
  ; build a single set of sentences out of the partitions (need to copy here, not modify)
  (let (ttype qtype)
    (setq partitions (apply #'concatenate 'list partitions))
    (setq ttype (get-theory-type partitions))
    (setq qtype (get-poss-type p))
    (cond ((and (eq ttype 'quantifier-free) (eq qtype 'quantifier-free)) (poss-qf-qf2 p partitions crelns))
          (t (assert nil nil (format nil "Don't know how to define ~A when sentences are ~%~A~%~%" p partitions))))))

(defun get-theory-type (th)
  (cond ((quantifier-free-sentencep (maksand (contents th))) 'quantifier-free)
        (t 'quantified)))

(defun get-poss-type (p)
  (setq p (second p))  ; since p is of the form (poss ?p @x)
  (cond ((quantifier-free-sentencep p) 'quantifier-free)
        (t 'quantified)))

(defun poss-qf-qf (p th crelns)
  "(POSS-QF-QF P TH CRELNS) computes a definition for the poss atom P
   using sentences TH, where the predicates with complete definitions can be found in CRELNS.
   TH must belong to the universal fragment, as must the poss sentence."
  (let (tmp poss)
    (setq tmp (dnf (poss-sent p)))
    (setq poss (simplify-poss (makposs tmp (vars tmp)) th crelns))   ; list of conjunctions
    (setq tmp (mapcan #'(lambda (x) (poss-qf-conj x th crelns)) (find-posses poss)))
    (if (equal p poss)
      tmp
      (cons `(<=> ,p ,poss) tmp))))

(defun poss-qf-qf2 (p th crelns)
  "(POSS-QF-QF P TH CRELNS) computes a definition for the poss atom P
   using sentences TH, where the predicates with complete definitions can be found in CRELNS.
   TH must belong to the universal fragment, as must the poss sentence."
  (let (tmp poss)
    (setq tmp (dnf (poss-sent p)))
    (setq poss (simplify-poss (makposs tmp (vars tmp)) th crelns))   ; list of conjunctions
    (setq tmp (mapcan #'(lambda (x) (poss-qf-conj2 x th crelns)) (find-posses poss)))
    (if (equal p poss)
      tmp
      (cons `(<=> ,p ,poss) tmp))))

(defvar *oneofs* nil "list of needed *oneofs*")

(defun poss-qf-conj (p th crelns)
  "(POSS-CONJ-QF P TH CRELNS) computes a definition for the poss atom P using the sentences
   TH, assuming TH is in the universal fragment and P is of the form poss'phi(xbar)'(xbar), where 
   phi(xbar) is a quantifier-free conjunction."
  (let (conditions predoneof vars newq)

    ; create new variables for p
    (setq newq (new-vars (poss-sent p)))
    (setq p (makposs newq (freevars newq)))

    ; saturate theory
    (setq th (cdr (cnf (maksand (contents th)))))
    (setq th (funcall *resolution-closure* th *limit*))

    ; construct the oneof subst list for the query
    (setq predoneof (oneof-sublist-conj (poss-sent p)))

    ; only care about those clauses where every not(signed literal) is a subset of the signed literals in q 
    (setq th (remove-if-not #'(lambda (x) (possibly-inconsistent x (mapcar #'car predoneof) crelns)) th))

    ; for each clause, construct the appropriate implication for inconsistency.
    (setq conditions nil)
    (dolist (c th)
      (setq conditions (cons (query-inconsistent-with-clause predoneof c) conditions)))

    ; construct self-consistency check for conjunction
    (setq conditions (nconc conditions (self-inconsistent (poss-sent p) predoneof)))

    (setq vars (vars p))
    ;(setq incon (makincon (poss-sent p) vars))
    (list `(<=> ,p ,(maknot (equantify-except (maksor conditions) vars))))))

(defun poss-qf-conj2 (p th crelns)
  "(POSS-CONJ-QF P TH CRELNS) computes a definition for the poss atom P using the sentences
   TH, assuming TH is in the universal fragment and P is of the form poss'phi(xbar)'(xbar), where 
   phi(xbar) is a quantifier-free conjunction."
  (let (conditions predoneof vars newq)

    ; create new variables for p
    (setq newq (new-vars (poss-sent p)))
    (setq p (makposs newq (freevars newq)))

    ; saturate theory
    (setq th (cdr (cnf (maksand (contents th)))))
    (setq th (funcall *resolution-closure* th *limit*))

    ; construct the oneof subst list for the query
    (setq predoneof (oneof-sublist-conj (poss-sent p)))

    ; only care about those clauses where every not(signed literal) is a subset of the signed literals in q
    (setq th (remove-if-not #'(lambda (x) (possibly-inconsistent x (mapcar #'car predoneof) crelns)) th))

    ; for each clause, construct the appropriate implication for inconsistency.
    (setq conditions nil)
    (dolist (c th)
      (setq conditions (cons (query-inconsistent-with-clause predoneof c) conditions)))

    ; construct self-consistency check for conjunction
    (setq conditions (nconc conditions (self-inconsistent (poss-sent p) predoneof)))

    (setq vars (vars p))
    ;(setq incon (makincon (poss-sent p) vars))
    (list `(<=> ,p ,(maknot (equantify-except (maksor conditions) vars))))))

(defun self-inconsistent (q predoneof)
  "(SELF-INCONSISTENT Q PREDONEOF) constructs the conditions under which Q 
   is inconsistent with itself.  Assumes PREDONEOF goes (((not p) . ??) (p . ??) ?????)."
    (do ((al predoneof)
         (preds (preds q))
         (commonvars)
         (arity)
         (conditions nil))
        ((or (null al) (null (cdr al))) conditions)
      (cond ((eq (relation (car (first al))) (relation (car (second al))))   ; have complementary literals
             (setq arity (parameter-arity (find (relation (car (first al))) preds :key #'parameter-symbol)))
             (setq commonvars (maptimes #'newindvar arity))
             (setq conditions (cons (makand (append (cons 'oneof (cdr (first al))) commonvars)
                                            (append (cons 'oneof (cdr (second al))) commonvars))
                                    conditions))
             (add-oneof-arity arity (/ (length (cdr (first al))) arity))
             (add-oneof-arity arity (/ (length (cdr (second al))) arity))
             (setq al (cddr al)))
            (t (setq al (cdr al))))))

(defun add-oneof-arity (m n)
  (setq *oneofs* (adjoin (list m n) *oneofs* :test #'equal)))

(defun possibly-inconsistent (clause signedpreds crelns)
  (when (and (listp clause) (not (eq (car clause) 'or))) (setq clause (list 'or clause)))
  (dolist (l (cdr clause) t)
    (when (and (not (member (pred l) crelns :test #'equalp))
               (not (member (maknot (signed-relation l)) signedpreds :test #'equal)))
      (return nil))))

(defun oneof-sublist-conj (q)
  (let (tmp posp negp (predoneof nil))
    (unless (and (listp q) (eq (car q) 'and)) (setq q (list 'and q)))
    (dolist (p (preds q) predoneof)
      ; find all positive/negative literals with predicate p
      (setq tmp (remove-if-not #'(lambda (x) (equalp (first (preds x)) p)) q))
      (setq posp (remove-if #'negative-literalp tmp))
      (setq negp (remove-if-not #'negative-literalp tmp))
      (when posp (setq predoneof (acons (parameter-symbol p) (mapcan #'(lambda (x) (copy-list (args x))) posp) predoneof)))
      (when negp (setq predoneof (acons (maknot (parameter-symbol p)) (mapcan #'(lambda (x) (copy-list (args x))) negp) predoneof))))))

(defun query-inconsistent-with-clause (predoneof c)
  "(QUERY-INCONSISTENT-WITH-CLAUSE Q C CRELNS) constructs an expression that represents
   the conditions under which conjunction Q is inconsistent with clause C."
  (let (new tmp neg arity)
    (unless (and (listp c) (eq (car c) 'or)) (setq c (list 'or c)))
    (setq c (mapcar #'maknot (cdr c)))
    (dolist (l c (maksand (nreverse new)))
      (setq neg nil)
      (when (negative-literalp l)
        (setq l (second l))
        (setq neg t))
      (setq tmp (cdr (assoc (signed-relation l) predoneof :test #'equal)))
      (cond (tmp
             (setq arity (length (args l)))
             (add-oneof-arity arity (/ (length tmp) arity))
             ;(setq *oneofs* (adjoin (list arity (/ (length tmp) arity)) *oneofs* :test #'equal)) ; keep track of the oneof 
             (setq tmp (append (cons 'oneof tmp) (cdr l))))
            (t
             (setq tmp l)))
      (when neg (setq tmp (maknot tmp)))
      (setq new (cons tmp new)))))

(defun poss-nr-horn (ps)
  "(POSS-NR-HORN PS) takes a nonrecursive set of Horn clauses PS
   each of which have the same predicate in the head, p.  Returns definitions for possp and possnegp."
  (let (newp possp possnegp p vars possdef possnegdef)
    (setq newp (car (contents (predicate-completion ps))))
    (setq p (second newp))
    (setq vars (vars p))
    (setq possp (makposs p vars))
    (setq possnegp (makposs (maknot p) vars))
    ; first add possp: possp(xbar) <=> univ(xbar)
    (setq possdef `(<=> ,possp ,(make-univ-conj vars)))
    ; then add possnegp: 
    (setq possnegdef `(<=> ,possnegp ,(maknot (makposs (maknot (third newp)) vars))))
    ; return the two defs
    (list possdef possnegdef)))

(defun make-univ-conj (vars)
  (maksand (mapcar #'(lambda (x) `(univ ,x)) vars)))



(defun poss-ground-disjunctions (ds)
  "(POSS-GROUND-DISJUNCTIONS DS) takes a list of ground disjunctions and returns definitions
   for possp and possnegp for every p in DS."
  (let (m os d ddef part partdef sat satdefs preds posspdefs possnpdefs eqpdefs )
    ; number of disjunctions
    (setq m (length ds))
    ; turn each disjunct into an object constant.
    (setq os (mapcar #'disj-to-obj ds))
    ; reify the structure of the disjunction
    (setq d (gentemp "d"))
    (setq ddef (create-bicond-with-order d (mapcar #'list os)))
    ; create the definition for part
    (setq part (gentemp "part"))
    (setq partdef (create-bicond part (mapcan #'disj-to-part ds)))
    ; create the sat definitions
    (setq sat (gentemp "sat"))
    (setq satdefs (create-sats d part m sat))
    ; for each predicate p in DS, construct a possp, a poss-p, and a eqp definition
    (setq preds (preds (maksand ds)))
    (setq posspdefs (mapcar #'(lambda (x) (create-poss-sat-defn x '+ sat m)) preds))
    (setq possnpdefs (mapcar #'(lambda (x) (create-poss-sat-defn x '- sat m)) preds))
    (setq eqpdefs (mapcar #'(lambda (x) (create-eqp-defn x ds)) preds))
    (list* ddef partdef (nconc satdefs posspdefs possnpdefs eqpdefs))))

(defun create-eqp-defn (p ds)
  "(CREATE-EQP DEFN P DS) produces a biconditional definition for eqP, i.e. it
   relates a tuple turned object constant to the tuple values."
  (create-bicond (eqp-predicate (parameter-symbol p)) (mapcan #'(lambda (x) (create-eqp-defn-aux p x)) ds)))

(defun create-eqp-defn-aux (p d)
  (let ((res nil))
    (cond ((atom d) (setq d (list d)))
          ((eq (car d) 'or) (setq d (cdr d)))
          (t (setq d (list d))))
    (dolist (l d)
      (setq l (drop-not l))
      (when (eq (car l) (parameter-symbol p))
        (setq res (cons (list* (atom-to-obj (cdr l)) (cdr l)) res))))
    (nreverse res)))

(defun eqp-predicate (p)
  (read-from-string (stringappend "eq" (string p))))

(defun create-poss-sat-defn (p sign sat m)
  (let (headvars satvars body negsign samep head objs)
    (setq negsign (if (eq sign '+) '- '+))
    (setq headvars (maptimes #'newindvar (parameter-arity p)))
    (setq satvars (maptimes #'newindvar (* 3 m)))
    (setq body (list (cons sat satvars)))
    (setq samep (eqp-predicate (parameter-symbol p)))
    ; ensure each of the headvars is one of the original objects
    (setq objs (maksand (mapcar #'(lambda (x) `(object ,x)) headvars)))
    ; for each triple in satvars, the triple is not negation of headvars
    (do ((sv satvars (cdddr sv)))
        ((null sv))
      (setq body (cons `(not (and (= ,(first sv) ,negsign) (= ,(second sv) ,(parameter-symbol p)) ,(list* samep (third sv) headvars))) body)))
    (if (eq sign '+)
      (setq head (makposs (cons (parameter-symbol p) headvars) headvars #'quotify-escaping-vars))
      (setq head (makposs (maknot (cons (parameter-symbol p) headvars)) headvars #'quotify-escaping-vars)))
    `(<=> ,head (and ,objs (exists ,satvars ,(maksand (nreverse body)))))))


(defun create-bicond-with-order (p values)
  (let (newvalues)
    (do ((vs values (cdr vs))
         (i 1 (1+ i)))
        ((null vs))
      (setq newvalues (cons (cons i (car vs)) newvalues)))
    (create-bicond p (nreverse newvalues))))

(defun create-bicond (p values)
  (let (body conj vars)
    (setq vars (maptimes #'newindvar (length (car values))))
    (do ((vas values (cdr vas)))
        ((null vas))
      (setq conj nil)
      (do ((vrs vars (cdr vrs))
           (s (car vas) (cdr s)))
          ((or (null vrs) (null s)))        
        (setq conj (cons `(= ,(car vrs) ,(car s)) conj)))
      (setq body (cons (maksand (nreverse conj)) body)))
    `(<=> ,(cons p vars) ,(maksor (nreverse body)))))

(defun create-sats (d part m head)
  (let (res sat2 sat1)
    (setq sat1 nil)
    (dotimes (k m (nreverse res))
      (setq sat2 (if (= k (1- m)) head (gentemp (stringappend "sat" (1+ k) "-"))))
      (setq res (cons (create-sat d part (1+ k) sat2 sat1) res))
      (setq sat1 sat2))))

(defun create-sat (d part k head2 head1)
  "(CREATE-SAT D PART K M HEAD2 HEAD1) returns a definition for SAT over a set of disjunctions
   when the disjunctions are reified using the predicate D and information extracted 
   using the predicate PART.  The head of the biconditional is HEAD2, and finds 
   a satisfying choice of literals from the last first k disjunctions.  HEAD1 is the recursive
   call if k < m ."
  (let (vars zvar existential recursive satconstraints) 
    (setq vars (maptimes #'newindvar (* 3 k)))
    (setq zvar (newindvar))
    (setq existential `(exists ,zvar (and (,d ,k ,zvar) (,part ,zvar ,(first vars) ,(second vars) ,(third vars)))))
    (when (cdddr vars) (setq recursive (cons head1 (cdddr vars))))
    (setq satconstraints (create-sat-constraints (first vars) (second vars) (third vars) (cdddr vars)))
    (if recursive
      `(<=> ,(cons head2 vars) ,(maksand (list* existential recursive satconstraints)))
      `(<=> ,(cons head2 vars) ,(maksand (list* existential satconstraints))))))

(defun create-sat-constraints (x y z others)
  "(CREATE-SAT-CONSTRAINTS X Y Z OTHERS) returns constraints that ensure satisfiability
   for CREATE-SAT."
  (do ((os others (cdddr os))
       (res nil))
      ((null os) (nreverse res))
    (setq res (cons `(=> (= ,x ,(first os)) (= ,y ,(second os)) (not (= ,z ,(third os))))
                    res))))

(defun disj-to-part (d)
  (let (partvalues disjobj)
    (setq disjobj (disj-to-obj d))
    (cond ((atom d) (setq d (list d)))
          ((eq (car d) 'or) (setq d (cdr d)))
          (t (setq d (list d))))
    (dolist (l d (nreverse partvalues))
      (setq partvalues (cons (list disjobj (if (negative-literalp l) '- '+) (relation l) (atom-to-obj (cdr (drop-not l))))
                             partvalues)))))
    
(defun disj-to-obj (disj)
  "(DISJUNCTION-TO-OBJECT DISJ) returns an object constant corresponding to the disjunction DISJ."
  ; turn DISJ into a list of literals as necessary
  (cond ((atom disj) (setq disj (list disj)))
        ((eq (car disj) 'or) (setq disj (cdr disj)))
        (t (setq disj (list disj))))
  (read-from-string (apply #'concatenate 'string (mapcar #'lit-to-string disj))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;