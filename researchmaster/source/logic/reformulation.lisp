;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; reformulation.lisp: hodgepodge logical reformulation routines
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(eval-when (compile load eval)
  (proclaim '(special *SUBSUMPTION-CLOSURE* *RESOLUTION-CLOSURE*)))

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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;; Basic sentence manipulation ;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun saturate (r arity terms)
  "(SATURATE R ARITY TERMS) computes all the possible atoms of arity ARITY with relation
   constant R using TERMS, of which there are TERMS^ARITY."
  (cond ((or (not (numberp arity)) (null terms)) nil)
        ((= arity 0) (list '(r)))
        (t
         (mapcar #'(lambda (x) (cons r x)) (cross-product terms arity)))))

(defun equantify (vars fact)
  "(EQUANTIFY FACT VARS) existentially quantifies variables VARS in FACT."
  (if vars
      `(exists ,vars ,fact)
      fact))

(defun equantify-except (fact vars)
  "(EQUANTIFY FACT VARS) existentially quantifies all free variables in FACT except for VARS."
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

(defun uniquify-vars (p)
  "(UNIQUIFY-VARS P) takes a closed sentence and ensures all quantified variables are
   distinct from other variables. 
   e.g. Ax.(p(x) => Ex.q(x)) ^ Ex.r(x) becomes Ax.(p(x) => Ey.q(y)) ^ Ez.r(z)."
  (labels ((aux (p bl)
	     (cond ((atom p) p)
		   ((member (car p) '(and or not => <= <=>))
		    (cons (car p) (mapcar #'(lambda (x) (aux x bl)) (cdr p))))
		   ((member (car p) '(forall exists))
		    (let (newvs)
		      (dolist (v (tolist (second p)))
			(when (member v universals)  ; universals is a global for tracking used vars
			  (push (cons v (newindvar)) bl)  ;
			  (setq v (cdr (car bl))))
			(push v universals)
			(push v newvs))
		      (list (car p) (nreverse newvs) (aux (third p) bl))))
		   (t (sublis bl p)))))
    (let ((universals nil))
      (aux p nil))))

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

(defun drop-op (p)
  (cond ((atom p) p)
	((atom (car p)) (cdr p))
	(t p)))

(defun drop-ops (p)
  "(DROP-OPS P) turns p into a list by dropping outermost operator."
  (cond ((atom p) (list p))
	((atom (car p)) (cdr p))
	(t (list p))))

(defun drop-not (p)
  (cond ((atom p) p)
        ((eq (car p) 'not) (second p))
        (t p)))

(defun drop-or (p)
  (cond ((atom p) p)
        ((eq (car p) 'or) (cdr p))
        (t p)))

(defun drop-ors (p)
  "(DROP-ORS P) turns P into a list by dropping outermost OR."
  (cond ((atom p) (list p))
        ((eq (car p) 'or) (cdr p))
        (t (list p))))

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

(defun drop-exists (p)
  (cond ((atom p) p)
	((eq (car p) 'exists) (third p))
	(t p)))

(defun drop-forall (p)
  (cond ((atom p) p)
	((eq (car p) 'forall) (third p))
	(t p)))

(defun drop-posneg (p)
  (cond ((atom p) p)
	((member (car p) '(pos neg)) (second p))
	(t p)))

(defun drop-things (p thing &key (test #'eq))
  (cond ((atom p) (list p))
	((funcall test (car p) thing) 
	 (mapcan #'(lambda (x) (drop-things x thing :test test)) (cdr p)))
	(t (list p))))

(defun to-orless-list (p) (if (and (listp p) (eq (car p) 'or)) (cdr p) (list p)))
(defun or2list (p) (to-orless-list p))
(defun and2list (p) (if (and (listp p) (eq (car p) 'and)) (cdr p) (list p)))
(defun list2p (p) (if (atom p) p (if (atom (car p)) p (maksand p))))

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

(defun mapbool (func p)
  "(MAPBOOL FUNC P) applies function FUNC to all subformulas of P 
   (children first then formula) and returns result."
  (cond ((atom p) p)
	((member (car p) '(or not and => <= <=>))
	 (funcall func (cons (car p) (mapcar #'(lambda (x) (mapbool func x)) (cdr p)))))
	((member (car p) '(forall exists))
	 (funcall func (list (first p) (second p) (mapbool func (third p)))))
	(t p)))

(defun someterm (func p)
  "(SOMETERM FUNC P) walks the sentence P and applies FUNC to each term.  If FUNC
   is ever true, returns T; else returns NIL."
  (cond ((atom p) nil)
	((member (car p) '(or not and => <= <=>)) (some #'(lambda (x) (someterm func x)) (cdr p)))
	((member (car p) '(forall exists)) (someterm func (third p)))
	(t (some #'(lambda (x) (funcall func x)) (cdr p)))))

(defun mapatomterm (func p)
  "(MAPOPANDS FUNC P) applies function FUNC to all of the atoms and terms occurring in P 
   (children before parent) and returns the result."
  (cond ((atom p) (funcall func p))
	((member (car p) '(or not and => <= <=>)) 
	 (cons (car p) (mapcar #'(lambda (x) (mapatomterm func x)) (cdr p))))
	((member (car p) '(forall exists)) (list (first p) (second p) (mapatomterm func (third p))))
	(t 
	 (let (args)
	   (setq args (mapcar #'(lambda (x) (mapatomterm func x)) (cdr p)))
	   (funcall func (cons (car p) args))))))

(defun mapatoms (func p) (mapopands func p))
(defun mapopands (func p)
  "(MAPOPANDS FUNC P) applies function FUNC to all of the operands of the sentence P and
   returns the result."
  (cond ((atom p) (funcall func p))
	((member (car p) '(or not and => <= <=>)) 
	 (cons (car p) (mapcar #'(lambda (x) (mapopands func x)) (cdr p))))
	((member (car p) '(forall exists)) (list (first p) (second p) (mapopands func (third p))))
	(t (funcall func p))))

(defun mapterms (func p)
  "(MAPTERMS FUNC P) applies function FUNC to all of the terms occurring in P 
   (children before parent) and returns the result.  Treats quoted sentences as proper sentences."
  (labels ((mapmyfunc (x)
	     (cond ((atom x) (funcall func x))
		   ((eq (first x) 'quote) (list (first x) (mapterms func (second x))))
		   (t (funcall func (cons (car x) (mapcar #'mapmyfunc (cdr x))))))))
    (cond ((atom p) p)
	  ((member (car p) '(or not and => <= <=>)) 
	   (cons (car p) (mapcar #'(lambda (x) (mapterms func x)) (cdr p))))
	  ((member (car p) '(forall exists)) (list (first p) (second p) (mapterms func (third p))))
	  (t (cons (car p) (mapcar #'mapmyfunc (cdr p)))))))

(defun subobj (alist p &key (test #'eq))
  "(SUBOBJ ALIST P) applies the substitution list ALIST to all of the
   object constants in the sentence P.  Returns the new sentence."
  (mapterms #'(lambda (x) (if (atom x) (sublis alist x :test test) x)) p))

(defun subfun (alist p &key (test #'eq))
  "(SUBFUN ALIST P) applies the substitution list ALIST to all of the
   function constants (but not object constants) in the sentence P.  Returns the new sentence."
  (mapterms #'(lambda (x) (if (listp x) (cons (sublis alist (car x) :test test) (cdr x)) x)) p))

(defun subrel (alist p &key (test #'eq))
  "(SUBFUN ALIST P) applies the substitution list ALIST to all of the
   relation constants (but not object constants) in the sentence P.  Returns the new sentence."
  (mapopands #'(lambda (x) (if (atom x) (sublis alist x :test test) (cons (sublis alist (car x) :test test) (cdr x))))
	     p))

(defun nsubfun (alist p)
  "(NSUBFUN ALIST P) destructively applies the substitution list ALIST to all of the
   function constants (but not object constants) in the sentence P.  Returns the new sentence."
  (cond ((atom p))
	((member (car p) '(and or not <= => <=>))
	 (mapc #'(lambda (x) (nsubfun alist x)) (cdr p)))
	((member (car p) '(forall exists))
	 (nsubfun alist (third p)))
	((eq (car p) 'quote) (nsubfun alist (second p)))
	(t (mapc #'(lambda (x) (nsubfun-term alist x)) (cdr p))))
  p)

(defun nsubfun-term (alist term)
  "(NSUBFUN-TERM ALIST TERM) destructively applies the substitution list ALIST to all of
   the function constants (but not object constants) in the term TERM.  Returns the new term."
  (cond ((atom term))
	((eq (car term) 'quote) (nsubfun alist (second term)))
	((listp term)
	 (let (f)
	   (setq f (assoc (first term) alist))
	   (when f
	     (setf (first term) (cdr f)))
	   (mapc #'(lambda (x) (nsubfun-term alist x)) (cdr term)))))
  term)

(defun transform-theory (f th &optional (saver #'save))
  "(TRANSFORM-THEORY F TH) applies F to all the sentences in TH, putting the
   results in a new theory and returning that theory."
  (let ((newth (make-instance (type-of th))))
    (mapc #'(lambda (y) (let ((newrule (funcall f y))) 
                          (when newrule (funcall saver newrule newth))
                          (if (not newrule) (format t "broken~%"))))
          (sentences '? th))
    newth))

(defun shallow-term (thing)
  "(SHALLOW-TERM THING) returns T iff the KIF expression given has a nesting of at most 1
   where a quoted term is defined to have depth 0."
  (cond ((atom thing) t)
	((eq (car thing) 'quote) t)
	(t (every #'(lambda (x) (or (atom x) (and (listp x) (eq (car x) 'quote)))) (cdr thing)))))

(defun flatten-functions (p &key (universal t))
  "(FLATTEN-FUNCTIONS P) takes a NNF formula P and returns a QF sentence 
   equivalent to p except the depth of function nesting is
   always at most 1, e.g. (p (f a (g ?x (h ?y)))) becomes
   (and (p (f a ?z2)) (= ?z2 (g ?x ?z3)) (= ?z3 (h ?y))).
   UNIVERSAL controls whether the implicit quantifier is universal or existential."
  (cond ((atom p) p)
	;((member (car p) '(and or not))   ;  OLD version of code???  Leave in case not
	; (multiple-value-bind (newps extra) (mapcaraccum #'(lambda (x) (flatten-functions x fullflat)) (cdr p))
	;   (maksand (cons (cons (car p) newps) extra))))
	((member (car p) '(or and)) 
	  ; don't include =>, <=, or <=> b/c the new variables need to be existentially quantified.
	  ;   Can of course rewrite this code, but don't have the time.  Can instead convert to NNF before flattening.
	 (flatten-operator (cons (car p) (mapcar #'(lambda (q) 
						     (flatten-functions q :universal universal)) 
						 (cdr p)))))	 
	((eq (car p) 'not)
	 (let (newp)
	   (setq newp (flatten-functions (second p) :universal universal))
	   (cond ((atom newp) (maknot newp))
		 ((member (car newp) '(or and)) (list* (first newp) (maknot (second newp)) (cddr newp)))
		 (t (maknot newp)))))
	((and (eq (car p) '=) (shallow-term (second p)) (shallow-term (third p))) p)
	(t
	 (multiple-value-bind (newargs extra) (mapcaraccum #'flatten-term (cdr p))
	   (if universal
	       (maksor (cons (cons (car p) newargs) (mapcar #'maknot extra)))
	       (maksand (cons (cons (car p) newargs) extra)))))))
#|  Old version of flatten functions
	(t (multiple-value-bind (newargs extra) 
	       (mapcaraccum 
		#'(lambda (term) 
		    (let (newterm newextra)
		      (if (atom term)
			  (setq newterm term)
			  (multiple-value-bind (termargs termextra) (mapcaraccum #'flatten-term (cdr term))
			    (setq newterm (cons (car term) termargs))
			    (setq newextra termextra)))
		      (values newterm newextra)))
		(cdr p))
	     (if universal
		 (maksred (cons (cons (car p) newargs) extra))
		 (maksand (cons (cons (car p) newargs) extra)))))))
|#

(defun flatten-term (term)
  (cond ((atom term) (values term nil))
	(t (let (newargs extra var)
	     (multiple-value-setq (newargs extra) (mapcaraccum #'flatten-term (cdr term)))
	     (setq var (tosymbol (gentemp "?flat")))
	     (values var (cons `(= ,var ,(cons (car term) newargs)) extra))))))	

(defun functions-to-relations (p)
  "(FUNCTIONS-TO-RELATIONS P) returns P but where all functions have been turned into
   relations.  This requires flattening embedded functions, e.g. (p (f (g ?x))) becomes
   (and (= (g ?x) ?x1) (= (f ?x1) ?x2) (p ?x2)), and all functions are turned into relations, e.g.
   (= (g ?x) ?x1) becomes (g ?x ?x1).  Returns 2 values: the new sentences and the additional constraints
   required to ensure semantics are preserved."
  (cond ((atom p) p)
;	((eq (car p) '<=)  ; special case so rules look nice
;	 (let ((newp (and2list (functions-to-relations (head p)))) (newvar (newindvar)))
;	   (list* '<= (car newp) (and2list (flatten-operator (maksand (nconc (cdr newp) (mapcar #'functions-to-relations (cddr p)))))))))
	((member (car p) '(=> and or not <=> <=))
	 (cons (car p) (mapcar #'functions-to-relations (cdr p))))
	((member (car p) '(forall exists))
	 (list (first p) (second p) (functions-to-relations (third p))))
	((eq (car p) '=)  ; special case = to reduce number of (= ?x ?y) statements
	 (multiple-value-bind (term1 rest1 newvars1) (function-to-relation (second p))
	   (multiple-value-bind (term2 rest2 newvars2) (function-to-relation (third p))
	     (if (or rest1 rest2)
		 (equantify (delete term1 (nconc newvars1 newvars2)) (plug (maksand (nconc rest1 rest2)) (list (cons term1 term2) '(t . t))))
		 `(= ,term1 ,term2)))))
	(t 
	 (multiple-value-bind (newp rest newvars) (mapcaraccum #'function-to-relation (cdr p))
	   (equantify newvars (maksand (cons (cons (car p) newp) rest)))))))

(defun function-to-relation (term)
  "(FUNCTION-TO-RELATION TERM) takes a term and flattens it, returning two values:
   a term and a list of supporting atoms, e.g. (f (g ?x)) becomes ?z and ((g ?x ?y) (f ?y ?z))."
  (cond ((atom term) (values term nil nil))
	(t (let ((newvar (newindvar)))
	     (multiple-value-bind (newterm rest newvars) (mapcaraccum #'function-to-relation (cdr term))
	       (setq newterm (append newterm (list newvar)))
	       (setq newterm (cons (car term) newterm))
	       (values newvar (cons newterm rest) (cons newvar newvars)))))))

(defun functional-axioms (param types)
  "(FUNCTIONAL-AXIOMS PARAM) returns f(@x,y) ^ f(@x,z) => y=z and
   types(@x) => Ey.type(y) ^ f(@x,y) as a list.  TYPES is a hash table keyed on (pred index)"
  (labels ((gettype (f i) (let ((y (gethash (list f i) types)))
			    (if y y 'univ))))
    (let* ((args1 nil) (args2 nil)
	   (res1 (newindvar))
	   (res2 (newindvar))
	   (p (parameter-symbol param))
	   typelist)
      (dotimes (i (parameter-arity param))
	(push (newindvar) args1)
	(push (list (gettype p (1+ i)) (car args1)) typelist))
      (setq args1 (nreverse args1))
      (setq typelist (nreverse typelist))

      (setq args2 (append args1 (list res2)))
      (setq args1 (nconc args1 (list res1)))

      (list
       `(=> ,(cons p args1)
	    ,(cons p args2)
	    (= ,res1 ,res2))
       `(=> ,(maksand typelist) ,(equantify res1 (makand (list (gettype p (1+ (parameter-arity param))) res1) 
							(cons p args1))))))))
  
(defun to-canonical-datalog (rule)
  "(TO-CANONICAL-DATALOG RULE) takes a datalog rule where the head is a literal
   and the body is an arbitrary FOL formula and converts it into a list of rules 
   that are equivalent to RULE except that
   the body of each consists of literals, i.e. quantifiers and boolean connectives
   (besides the implicit AND) are all removed."
  (assert (literalp (head rule)) nil (format nil "TO-CANONICAL-DATALOG expects an input where the head is a literal: ~A" (head rule)))
  (assert (or (atomicp rule) (eq (car rule) '<=)) nil 
	  (format nil "TO-CANONICAL-DATALOG expects an input where the head is a literal: ~A" (head rule)))
  (cond ((atomicp rule) (list rule))
	((null (cddr rule)) (list rule))
	(t (multiple-value-bind (entries defs) (mapcaraccum #'lloyd-topor (and2list (nnf (maksand (cddr rule)))))
	     (cons (list* '<= (head rule) (and2list (flatten-operator (maksand entries)))) defs)))))

(defun lloyd-topor (p &optional (neg nil))
  "(LLOYD-TOPOR P) translates the FOL sentence P into a set of datalog rules.
   NEG is true iff the entry is to be used inside a negation -- used internally just to simplify output a bit.
   Returns two values: a conjunction of literals (often just an entry point) and the set of rules."
  (cond ((atomicp p) (values p nil))
	((eq (car p) 'not)
	 (cond ((and (listp (second p)) (eq (car (second p)) 'not)) ; double negation
		(lloyd-topor (second (second p)) neg))
	       (t (multiple-value-bind (entry defs) (lloyd-topor (second p) t)
			     (values (if neg entry (maknot entry)) defs)))))
	((eq (car p) 'forall) (lloyd-topor `(not (exists ,(second p) (not ,(third p)))) neg)) 
	((eq (car p) 'exists) 
	 (multiple-value-bind (entry defs) (lloyd-topor (third p))
	   (cond (neg
		  (let (newentry)
		    (setq newentry (cons (tosymbol (gentemp "reln")) (set-difference (vars entry) (tolist (second p)))))
		    (values newentry (cons (list* '<= newentry (and2list entry)) defs))))
		 (t (values entry defs)))))
	((member (car p) '(and or))
	 (setq p (flatten-operator p))
	 (multiple-value-bind (entries defs) (mapcaraccum #'lloyd-topor (cdr p))
	   (let (newentry)
	     (setq newentry (cons (tosymbol (gentemp "reln")) (freevars p)))
	     (if (eq (car p) 'and)
		 (if neg
		     (values newentry (cons (list* '<= newentry (and2list (flatten-operator (maksand entries)))) defs))
		     (values (flatten-operator (maksand entries)) defs))
		 (values newentry (nconc (mapcar #'(lambda (x) (list* '<= newentry (and2list x))) entries) defs))))))
	((member (car p) '(<= => <=>)) (lloyd-topor (nnf p) neg))
	(t (assert nil nil (format nil "Lloyd-Topor expects a KIF formula but received ~A" p)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Eliminating syntactic sugar from superset of FHL in KIF (called PL-FHL)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun pl-fhl-to-fhl (p builtins)
  "(PL-FHL-TO-FHL P BUILTINS) takes a sentence P in an extended KIF language
   together with the builtins (a list of parameters) and converts into regular KIF."
    ; AND/OR/NOT/... have many synonyms --- canonicalize into KIF
    (setq p (pl-ops-to-kif p))
    ; AND/OR can be treated as boolean functions -- turn them back into traditional operators
    (setq p (boolean-funcs-to-ops p))
    ; change non-kif boolean connectives (e.g. XOR) into KIF boolean connectives
    (setq p (boolops2kif p))
    ; eliminate duplicates inside boolean ops
    (setq p (mapbool #'(lambda (x) (delete-duplicates x :test #'sentequal)) p))
    ; turn functional terms appearing as atoms into proper relational sentences
    (setq p (improper-funcs-to-relns p builtins))
    p)

(defun pl-ops-to-kif (p)
  (setq p (sublis '((&& . and)
		    (! . not)
		    (== . =)
		    (=== . =))
		  p)))

(defun improper-funcs-to-relns (p vocab)
  "(IMPROPER-FUNCS-TO-RELNS P) translates all functions that appear as atoms (implicitly casted to bool) into relations"
  (let (isfunc)
    (setq isfunc #'(lambda (x y) (and (eq x (parameter-symbol y)) (isfunction y))))
    (mapatoms #'(lambda (q) (let (b)
			      (setq b (find (relation q) vocab :test isfunc))
			      (if b `(= true (tobool ,q)) q)))
	      p)))

(defun boolean-funcs-to-ops (p)
  "(BOOLEAN-FUNCS-TO-OPS P) tries to simplify P so that all the boolean ops AND/OR/NOT appear
   at the top-level of the sentences, as prescribed in FOL.  When this cannot be accomplished,
   throws ss-boolean-error.  Assumes semantics for TOBOOL BOOL and BOOLEAN are type-casts to boolean." 
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

(defun boolops2kif (p)
  "(BOOLOPS2KIF P) translate non-kif boolean operators to KIF operators."
  (labels ((rec (p)
	     (mapopands #'(lambda (x)
			    (cond ((atom x) x)
				  ((eq (car x) 'xor) (rec (xor2kif x)))
				  ((eq (car x) 'ifelse) (rec (ifelse2kif x)))
				  (t x)))
			p)))
    (rec p)))

(defun xor2kif (p)
  (let (res)
    (setq p (cdr p))
    (dolist (thing p (maksor (nreverse res)))
      (push (maksand (cons thing (mapcar #'maknot (remove thing p)))) res))))
      
(defun ifelse2kif (p)
  `(and (=> ,(second p) ,(third p))
	(=> ,(maknot (second p)) ,(fourth p))))

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

(defun propagate-equality (l vars &optional (varsonly nil))
  "(PROPAGATE-EQUALITY L VARS) takes a list of literals L and a list of variables VARS.
   It replaces equals for equals in the usual way until a fixed point is reached,
   while never removing a variable in VARS.  If VARSONLY is true, only removes (= ?x ?y)."
  (assert (and (listp l) (listp vars)) nil "propagate-equality takes two lists")

  ; first copy l so we can destructively modify it
  (propagate-equality-aux (copy-tree l) vars varsonly))

(defun propagate-equality-aux (l vars varsonly)
  ; find the first substitution: (= x y) means replace x with y (modulo VARS)
  (let (tgt)
    (setq tgt (first-result #'(lambda (x) (orientedrewrite x vars varsonly)) l))
    (if tgt
      (propagate-equality-aux (remove-if #'(lambda (x) (and (listp x) (eq (car x) '=) (equal (second x) (third x))))
                                         (nsubst (third tgt) (second tgt) l))
                              vars varsonly)
      l)))

(defun orientedrewrite (lit vars varsonly)
  (cond ((atom lit) nil)
        ((not (eq (car lit) '=)) nil)
        (varsonly
	 (cond ((and (varp (second lit)) (varp (third lit)))
		(cond ((not (member (second lit) vars)) lit)
		      ((not (member (third lit) vars)) `(= ,(third lit) ,(second lit)))
		      (t nil)))
	       (t nil)))
	(t
	 (cond ((and (varp (second lit)) (not (member (second lit) vars))) lit)
	       ((and (varp (third lit)) (not (member (third lit) vars))) `(= ,(third lit) ,(second lit)))
	       (t nil)))))

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

(defun delete-simple= (lits &key (test #'eq))
  "(DELETE-SIMPLE= LITS) takes a list of literals and returns a logically equivalent list of literals
   without any lits of the form var = ground or ground=ground
   (and the binding list as a second value).  Or returns :UNSAT. Destructive.
   TEST determines truth of ground=ground.  Assumes UNA."
  (flet ((plugsimp (ps bl)
	   (setq ps (nsublis bl ps))
	   (values ps (nconc bl '((t . t)))))
	 (if-violates-una (ps)
	   (dolist (p ps ps)
	     (when (and (listp p) (eq (car p) '=) (groundp (second p)) (groundp (third p))
			(not (funcall test (second p) (third p))))
	       (return :unsat)))))
    (do ((ls lits (cdr ls)) (prev nil) (newl) (l) (del nil nil) (v) (h (make-hash-table)))
	((null ls) (if-violates-una (plugsimp newl (hash2bl h))))
      (setq l (car ls))
      ; accummulate binding list
      (when (and (listp l) (eq (car l) '=))
	(cond ((and (groundp (second l)) (groundp (third l)))
	       (unless (funcall test (second l) (third l)) (return :unsat))
	       (setq del t))
	      ((and (varp (second l)) (groundp (third l)))
	       (setq v (gethash (second l) h))
	       (when (and v (not (funcall test v (third l)))) (return :unsat))
	       (when (not v) (setf (gethash (second l) h) (third l)))
	       (setq del t))
	      ((and (groundp (second l)) (varp (third l)))
	       (setq v (gethash (third l) h))
	       (when (and v (not (funcall test v (second l)))) (return :unsat))
	       (when (not v) (setf (gethash (third l) h) (second l)))
	       (setq del t))))
      ; delete item when necessary
      (cond ((and del prev)
	     (setf (cdr prev) (cddr prev)))  ; delete this item
	    ((and (not del) (not prev)) ; found first element of return list
	     (setq prev ls)
	     (setq newl ls))
	    ((and (not del) prev)
	     (setq prev (cdr prev)))))))



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

(defun remove-truth1 (p)
  "(REMOVE-TRUTH1 P) removes all truth values (true and false) from the toplevel of P,
   which is assumed to be in NNF, e.g. a positive boolean combination of literals."
  (cond ((atom p) p)
	((eq (car p) 'and) 
	 (setq p (remove-override 'true 'false (cdr p)))
	 (if (listp p) (maksand p) p))
	((eq (car p) 'or)
	 (setq p (remove-override 'false 'true (cdr p)))
	 (if (listp p) (maksor p) p))
	((eq (car p) 'not) (maknot (second p)))
	(t p)))

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
                              

(defun subsumption-elimination (th &key (test #'similarp) (key #'identity))
  (nreverse 
   (subsumption-elimination-forward
    (nreverse 
     (subsumption-elimination-forward 
      (copy-list (contents th))
      test key))
    test key)))

; CAUTION: destructive to TH, which must be a list
(defun subsumption-elimination-forward (th test key)
  (do ((ps th (cdr ps)))
      ((null ps) th)
    (do ((tosubsumes (cdr ps) (cdr tosubsumes))
	 (pointer ps))
	((null tosubsumes))
      ; when subsumed, remove from list directly
      ; only increment pointer when not deleting; otherwise, pointer=tosubsumes
      (cond ((funcall test (funcall key (car tosubsumes)) (funcall key (car ps)))
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
;;; Definability and interpolation 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun definability-to-interpolation (p th preds)
  "(DEFINABILITY-TO-INTERPOLATION P TH PREDS) returns 2 values P' and TH' such that
   TH U TH' implies P <=> P' and the only symbols in the intersection of P and P'
   (including TH and TH') are PREDS.  
   Thus, P is definable in terms of PREDS iff there is an interpolant of P => P'."
  (let (allpreds predstoelim bl th2 p2)
    (setq allpreds (relns (makand p (maksand (contents th)))))
    (setq predstoelim (set-difference allpreds preds))
    (setq bl (mapcar #'(lambda (x) (cons x (gentemp (tostring x)))) predstoelim))
    (setq th2 (and2list (subrel bl (maksand (contents th)))))
    (setq p2 (subrel bl p))
    (values p2 th2)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Simplification by a complete theory 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun simplify-complete (p preds th)
  "(SIMPLIFY-COMPLETE P TH) takes a ground sentence P, a set of PREDS defined by
   the complete theory TH (whose extension is extractable with viewfindp)
   and returns a version of P with all occurrences of PREDS removed.
   May return True or False."
  (assert (groundp p) nil (format nil "Simplify-complete takes only ground sentences, but received ~A" p))
  (cond ((atom p) (if (member p preds) (if (viewfindp p th) 'true 'false) p))
	((member (car p) '(and or not <= => <=>))
	 (simplify-truth-top (cons (car p) (mapcar #'(lambda (x) (simplify-complete x preds th)) (cdr p)))))
	((member (car p) '(forall exists))
	 (simplify-truth-top (list (car p) (second p) (simplify-complete (third p) preds th))))
	(t (if (member (relation p) preds) (if (viewfindp p th) 'true 'false) p))))
  
(defun simplify-truth-top (p)
  "(SIMPLIFY-TRUTH-TOP P) returns a sentence equivalent to P but with all truth values
   (true false) removed (except possibly the top-level). Destructive. Does not recurse."
  (cond ((atom p) p)
	((member (car p) '(and or not <=>))
	 (case (car p)
	   (and (if (member 'false (cdr p)) 'false (maksand (delete 'true (cdr p)))))
	   (or (if (member 'true (cdr p)) 'true (maksor (delete 'false (cdr p)))))
	   (not (maknot (second p)))
	   (<=> (cond ((and (truthvaluep (second p)) (truthvaluep (third p)))
		       (if (eq (second p) (third p)) 'true 'false))
		      ((eq (second p) 'false) (maknot (third p)))
		      ((eq (second p) 'true) (third p))
		      ((eq (third p) 'false) (maknot (second p)))
		      ((eq (third p) 'true) (second p))
		      (t p)))
	   (<= (cond ((member 'false (cddr p)) 'true)
		     ((eq (second p) 'true) 'true)
		     ((eq (second p) 'false) (maknot (maksand (delete 'true (cddr p)))))
		     (t (delete 'true p))))
	   (=> (let (head body)
		 (setq head (car (last p)))
		 (cond ((eq head 'true) 'true)
		       ((and (setq body (cdr (butlast p))) (member 'false body)) 'true)
		       ((eq head 'false) (maknot (maksand (delete 'true body))))
		       (t (delete 'true p)))))))
	((member (car p) '(forall exists))
	 (if (truthvaluep (third p)) (third p) p))
	(t p)))
		 
	 
  
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

(defun test-order-datalog (&optional (rule nil) (debug nil))
  (let (tests failures out h hash)
    (setq h (make-hash-table))
    (setf (gethash 'r h) '((r ?x ?y ?z) . (rtype ?x ?y ?z)))
    (setf (gethash 'p1 h) '((p1 ?x) . (ab ?x)))
    (setf (gethash 'q1 h) '((q1 ?x) . (ab ?x)))
    ; (input output bound assign)
    (setq tests `(
		  ; basic
		  ((<= p q r s) (<= p q r s))
		  ((<= (p ?x) (not (q ?x)) (r ?x)) (<= (p ?x) (r ?x) (not (q ?x))))
		  ((<= (p ?x) (not (q ?y)) (r ?x ?y)) (<= (p ?x) (r ?x ?y) (not (q ?y)))) 
		  ((<= (p ?x ?y)) (<= (p ?x ?y) (univ ?x) (univ ?y)))
		  ((<= (p ?x) (not (r ?y)) (q ?y)) (<= (p ?x) (q ?y) (not (r ?y)) (univ ?x)))
		  ((<= (p ?x) (not (r ?x ?y)) (not (r ?y ?z)) (s ?y) (s ?x) (s ?z))
		   (<= (p ?x) (s ?y) (s ?x) (not (r ?x ?y)) (s ?z) (not (r ?y ?z))))
		  ((<= (p ?w) (not (r ?w ?t)) (not (r ?x ?y)) (not (r ?y ?z)) (s ?y) (s ?x) (s ?z))
		   (<= (p ?w) (s ?y) (s ?x) (not (r ?x ?y)) (s ?z) (not (r ?y ?z)) (univ ?w) (univ ?t) (not (r ?w ?t))))
		  
		  ; with equality
		  ((<= (p ?x) (= (f ?y) ?x) (q ?y)) (<= (p ?x) (q ?y) (<- ?x (f ?y))))
		  ((<= (p ?x) (= (f ?y) (g ?z)) (q ?y)) (<= (p ?x) (q ?y) (univ ?x) (univ ?z) (= (f ?y) (g ?z))))
		  ((<= (p ?x) (= (f ?y) a) (q ?y)) (<= (p ?x) (q ?y) (= (f ?y) a) (univ ?x)))
		  ((<= (p ?x) (= ?x ?y) (q ?y)) (<= (p ?x) (q ?y) (<- ?x ?y)))
		  ((<= (p ?x) (= (f ?y ?z) ?x) (q ?y)) (<= (p ?x) (q ?y) (univ ?z) (<- ?x (f ?y ?z))))
		  ((<= (p ?x) (= (f ?y ?z) ?x) (r ?z) (q ?y)) (<= (p ?x) (r ?z) (q ?y) (<- ?x (f ?y ?z))))
		  ((<= (p ?t2) (= (f ?t1 ?t3) ?t2) (q ?t1) (not (r ?w ?t)) (not (r ?x ?y)) (not (r ?y ?z)) (s ?y) (s ?x) (s ?z))
		   (<= (p ?t2) (q ?t1) (s ?y) (s ?x) (not (r ?x ?y)) (s ?z) (not (r ?y ?z)) (univ ?t3) (<- ?t2 (f ?t1 ?t3)) 
		       (univ ?w) (univ ?t) (not (r ?w ?t))))
		  ((<= (Q ?X0) (= ?X0 ?TLH0) (P ?TLH0)) (<= (Q ?X0) (P ?TLH0) (<- ?X0 ?TLH0)))

		  ; with initial binding
		  ((<= (p ?x)) (<= (p ?x)) (?x))
		  ((<= (p ?x ?y)) (<= (p ?x ?y) (univ ?y)) (?x))
		  ((<= (p ?x ?y) (q ?y) (not (r ?x))) (<= (p ?x ?y) (not (r ?x)) (q ?y)) (?x))

		  ; with hashed types
		  ((<= (p ?x) (not (r ?x ?y ?z)))
		   (<= (p ?x) (rtype ?x ?y ?z) (not (r ?x ?y ?z)))
		   nil nil ,h)
		  ((<= (Q1 ?X0) (= ?X0 ?TLH0) (P1 ?TLH0)) (<= (Q1 ?X0) (P1 ?TLH0) (<- ?X0 ?TLH0))
		   nil nil ,h)

		  ; with attachments
		  ((<= (p ?x) (q ?x)) (<= (p ?x) (q ?x)) 
		   nil ,(list (make-parameter :symbol 'q :type 'function :arity 1)))
		  ((<= (p ?x) (q ?x)) (<= (p ?x) (univ ?x) (q ?x)) 
		   nil ,(list (make-parameter :symbol 'q :type 'relation :arity 1)))
		  ))
    (setq failures 0)
    (dolist (e tests)
      (when (or (not rule) (equal rule (first e)))
	(if (fifth e) (setq hash (fifth e)) (setq hash (make-hash-table)))
	(setq out (order-datalog-rule (first e) :bound (third e) :attach (fourth e) :hashedtypes hash :assign '<- :debug debug))
	(format t "~A ----->>>> ~A~%" (first e) out)
	(unless (equal (second e) out)
	  (setq failures (1+ failures))
	  (format t "Error on ~A~%" (first e)))))
    (if (> failures 0) (format t "ERRORS: ~A~%" failures) (format t "SUCCESS~%"))))
	  
(defun order-datalog-rule (p &key (hashedtypes (make-hash-table)) (defaulttype 'univ) (attach nil) (bound nil) (assign nil) (debug nil))
  "(ORDER-DATALOG-RULE P) takes a sentence P of the form (<= LIT0 LIT1 .... LITN).  Returns 
   a rule equivalent to P except with the body reordered and possibly with additional
   body literals to make left-to-right evaluation safe and efficient.  See ORDER-DATALOG-CONJUNCTS
   for explanation of other parameters."
  (cond ((atom p) p)
	((eq (car p) '<=) 
	 (let (nlit b)
	   ; add nlit to body to ensure the variables in the head are made safe
	   (setq nlit (maknot (drop-not (head p))))
	   (setq b  (order-datalog-conjuncts (adjoin nlit (body p) :test #'equalp) :hashedtypes hashedtypes 
					     :defaulttype defaulttype :attach attach 
					     :bound bound :assign assign :debug debug))
	   ; remove nlit from the body
	   (setq b (remove nlit b))
	   (list* '<= (head p) b)))
	(t (order-datalog-rule (list '<= p) :hashedtypes hashedtypes :defaulttype defaulttype 
			       :attach attach :bound bound :assign assign :debug debug))))
	   
(defun order-datalog-conjuncts (p &key (hashedtypes (make-hash-table)) (defaulttype 'univ) (attach nil) (bound nil) (assign nil) (debug nil))
  "(ORDER-DATALOG-CONJUNCTS P HASHEDTYPES) takes a list of literals P, a hash 
   table keyed on predicates, and optionally a DEFAULTTYPE.  Returns P where
   the negative literals may be reordered, possibly with additional type guards 
   inserted that guarantees left-to-right evaluation always results in ground
   negative literals at evaluation time, as long as DEFAULTTYPE is non-nil.  
   The p entry in HASHEDTYPES is a dotted
   pair of the form ((p ?x1 ... ?xn) . (q ?x1 ... ?xm)).  ATTACH is a list of 
   parameters that are only implemented via procedural code.  BOUND is a list
   of variables that are known to be bound at evaluation time.  Allows
   function constants to appear but (i) only in positive = lits 
   (i.e. flatten-functions has been called) and (ii) assumes all functions are all attached.
   ASSIGN when non-NIL changes (= t u)  to (,assign t u) to indicate the 2nd arg (a var) is being assigned
   the third arg.  Assumes ASSIGN does not appear in P.  Also, assumes no (= ?x ?y), i.e. where both
   args to = are vars -- if assumption broken, ordering produced may be less efficient.
   Warning: we assume DEFAULTTYPE is the union of all domains of built-ins as well as relations.
      In practice, this doesn't happen since Univ is always finite and builtins are often infinite.
      Thus in practice, we may miss answers."
  (let (res equality pos neg varhash eqhash poshash neghash) 
    (labels ((needsguardp (lit)
	       (or (negative-literalp lit) 
		   (and (positive-literalp lit) (eq (relation lit) '=) (not (varp (second lit))) (not (varp (third lit))))
		   (let (r a)
		     (setq r (relation lit))
		     (setq a (arity lit))
		     (find-if #'(lambda (param) (and (eq a (parameter-arity param))
						     (eq r (parameter-symbol param))
						     (isrelation param)))
			      attach))))
	     (markvar (v) (setf (gethash v varhash) t))
	     (varmarked (v) (gethash v varhash))
	     (hashonvars (vs lit hash)
	       (dolist (v vs)
		 (setf (gethash v hash) (cons lit (gethash v hash)))))
	     (unboundvars (thing) (remove-if #'(lambda (x) (gethash x varhash)) (vars thing)))
	     (varsbound (thing) (every #'(lambda (x) (gethash x varhash)) (vars thing)))
	     (indexeq () ; for each equality atom, hash on all unbound vars *in 2nd arg* and if none return it.
	       (let ((safe nil) (vs nil))
		 (dolist (p equality) 
		   (setq vs (unboundvars (third p)))
		   (if vs (hashonvars vs p eqhash) (push (modeq p) safe)))
		 (mapc #'(lambda (x) (pushlit x 'equality)) (nreverse safe))))
	     (indexpos ()  ; written out so that we can modify 'pos' directly
	       (let ((safe nil) (vs nil))
		 (dolist (p pos) 
		   (setq vs (unboundvars p))
		   (if vs (hashonvars vs p poshash) (push p safe)))
		 (mapc #'(lambda (x) (pushlit x 'pos)) (nreverse safe))))
	     (indexneg ()
	       (let ((safe nil) (vs nil))
		 (dolist (p neg) 
		   (setq vs (unboundvars p))
		   (if vs (hashonvars vs p neghash) (push p safe)))
		 (mapc #'(lambda (x) (pushlit x 'neg)) (nreverse safe))))
	     (modeq (e) (if assign (if (varsbound (second e)) e (list assign (second e) (third e))) e)) ; turn = into <-
	     (addall (vars) (let (newvs)
			      (setq newvs (addeq vars))  ;addeq can mark new vars; pos/neg can't
			      (setq newvs (nconc newvs vars))
			      (addpos newvs) 
			      (addneg newvs)))
	     (addeq (vars)  ; return list of variables newly by addeq marked
	       (let (newvs)
		 (dolist (v vars)
		   (dolist (e (gethash v eqhash))
		     (when (and (varsbound (third e)) (member e equality))
		       (setq newvs (nconc (pushlit e 'equality) newvs)))))
		 (when newvs (setq newvs (nconc (addeq newvs) newvs)))		 
		 ;(format t "addeq(~A) returns ~A~%" vars newvs)
		 newvs))
	     (addneg (vars)  ; add negative literals whose variables were just marked.
	       (dolist (v vars)
		 (dolist (n (gethash v neghash))
		   (when (and (varsbound n) (member n neg))
		     (pushlit n 'neg)))))
	     (addpos (vars)  ; add positive literals whose vars just marked.
	       (dolist (v vars)
		 (dolist (p (gethash v poshash))
		   (when (and (varsbound p) (member p pos))
		     (pushlit p 'pos)))))
	     (pushlitadd (lit type)
	       (addall (pushlit lit type)))
	     (pushlit (lit type) ; returns list of newly marked vars
	       (let (vs)
		 (setq vs (unboundvars lit))
		 (case type
		   (equality (setq equality (delete lit equality)) (setq lit (modeq lit)))
		   (pos (setq pos (delete lit pos)))
		   (neg (setq neg (delete lit neg))))
		 (push lit res)
		 (mapc #'markvar vs)
		 ;(format t "pushinglit ~A ~A returns ~A~%" lit type vs)
		 vs))
	     ; idea for these: walk over lits and compute (i) the set of vars that will be bound
	     ;    if we add this lit (including those set by =) and (ii) the number of literals
	     ;    that will then be safe.  Then choose the lit that makes the most new lits safe.
	     (choose-positive-literal (l) (car l))
	     (choose-typed-negative-literal (l) (find-if #'(lambda (x) (gethash (relation x) hashedtypes)) l))
	     (choose-negative-literal (l) (car l))
	     (choose-equality-literal (l) (car l))
	     (output-internals (msg)
	       (format t "~&~A: ~A~%" msg res)
	       (format t "varhash: ~A~%" (hash2bl varhash))
	       (format t "pos: ~A~%" pos)
	       (format t "poshash: ~A~%" (hash2bl poshash))
	       (format t "equality: ~A~%" equality)
	       (format t "eqhash: ~A~%" (hash2bl eqhash))
	       (format t "neg: ~A~%" neg)
	       (format t "neghash: ~A~%" (hash2bl neghash))))
      ; initialize "global" variables; the labels funcs below can modify these directly
      (setq attach (remove '= attach :key #'parameter-symbol))  ; treat = specially
      (setq res nil)
      (multiple-value-setq (neg pos) (split #'needsguardp p))
      (multiple-value-setq (equality pos) (split #'(lambda (x) (eq (relation x) '=)) pos))
      (setq pos (remove 'true pos))
      ; don't have all positive = literals--only those where at least one arg is a variable.
      ;   Orient them all so that the first arg is a variable. 
      (setq equality (mapcar #'(lambda (x) (if (varp (second x)) x `(= ,(third x) ,(second x)))) equality))
      (setq varhash (make-hash-table)) ; whether or not a given var is bound
      (setq eqhash (make-hash-table))  ; pointers to equality literals, keyed on vars
      (setq poshash (make-hash-table)) ; pointer to positive literals, keyed on vars
      (setq neghash (make-hash-table))  ; pointers to negative literals, keyed on vars
      ; initialize those hashes: varhash with bound, eq/pos/neg-hash with equality/pos/neg
      ; index-eq/pos/neg return lits that have no unbound vars, so make them conjuncts immediately
      (mapc #'markvar bound)	
      (indexeq)
      (indexpos)
      (indexneg)
      (when debug (output-internals "result after initial construction"))

      ; add positive literals one at a time and each time add the now-safe neg/equality literals.
      (do ((p (choose-positive-literal pos) (choose-positive-literal pos)))
	  ((not p))
	(pushlitadd p 'pos))
      (when debug (output-internals "result after adding positive conjuncts"))

      ; for each remaining negative literal, add type guards.
      ;  (Equality literals have no types, since we don't have types for builtins.)
      (do ((n (choose-typed-negative-literal neg) (choose-typed-negative-literal neg))
	   (bl) (typeexpr))
	  ((not n))
	(setq typeexpr (gethash (relation n) hashedtypes))
	(when typeexpr
	  (setq typeexpr (stdize typeexpr))
	  (setq bl nil)
	  (when typeexpr (setq bl (mgu (first typeexpr) (drop-not n))))
	  (when bl
	    (push (plug (cdr typeexpr) bl) res)
	    (pushlitadd n 'neg))))
      (when debug (output-internals "result after adding negative conjuncts"))

      ; all of the eq/neg that remain have no types: use univ.  Start with eq to bind more.
      ; infinite domain limitation occurs here. consider (= 7 (f ?y)).
      ;   We just produce (and (univ ?y) (= 7 (f ?y))), even though UNIV will always be finite.
      (do ((e (choose-equality-literal equality) (choose-equality-literal equality)))
	  ((not e))
	(dolist (v (vars (third e))) ; just need to bind the 2nd arg vars
	  (unless (varmarked v) (push (list defaulttype v) res) (markvar v)))
	(pushlitadd e 'equality))
      (when debug (output-internals "result after adding equality with univ"))
      
      (do ((e (choose-negative-literal neg) (choose-negative-literal neg)))
	  ((not e))
	(dolist (v (vars e))
	  (unless (varmarked v) (push (list defaulttype v) res) (markvar v)))
	(pushlitadd e 'neg))
      (when debug (output-internals "result after adding negatives with univ")))
    (nreverse res)))
  
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Logical Compression
;;;     Taking logical constraints and constructing constraints + tables
;;;     that have the same semantics. 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
#|
(compress-with-database '(OR 
	(AND (SKU "PD 6600") (PEDALTYPE "SPD"))
        (AND (SKU "PD 5500") (PEDALTYPE "SPD"))
        (AND (SKU "PD M545") (PEDALTYPE "Clip"))
        (AND (SKU "PD M434") (PEDALTYPE "Clip"))
        (AND (SKU "Campagnolo Record") (PEDALTYPE "SPD"))
        (AND (SKU "Campagnolo Chorus") (PEDALTYPE "SPD"))
        (AND (SKU "PD C105") (PEDALTYPE "Standard"))
        (AND (SKU "Black Plastic") (PEDALTYPE "Standard"))
        (AND (SKU "PD C101") (PEDALTYPE "Standard"))))

becomes
(=> (sku ?x) (pedaltype ?y) (p ?x ?y))

	(p "PD 6600" "SPD")
        (p "PD 5500" "SPD")
        (p "PD M545" "Clip")
        (p "PD M434" "Clip")
        (p "Campagnolo Record" "SPD")
        (p "Campagnolo Chorus" "SPD")
        (p "PD C105" "Standard")
        (p "Black Plastic" "Standard")
        (p "PD C101" "Standard")

|#

(define-condition structural-mismatch (error) ((comment :initarg :comment)))

(defun compress-to-database (p)
  "(COMPRESS-TO-DATABASE P) takes a monadic, function-free KIF formula as input and 
   returns (i) a new KIF formula and (ii) a set of database tables such that the 
   semantics is preserved." 
  (setq p (nnf p))
  (unless (or (clausep p) (compressiblep p)) (setq p (coerce-to-compressible p)))  ; since coersion is sometimes inefficient
  (if (compressiblep p)
      (handler-case (compress-or-and p)
	(structural-mismatch () (values p nil)))
      p))

(defun coerce-to-compressible (p)
  "(COERCE-TO-COMPRESSIBLE P) coerces P into a compressible form, i.e. 
   a disjunction of conjunctions of simple disjunctions or literals.
   Assumes P is in NNF, i.e. it contains AND/OR combinations of literals.
   Always returns a sentence of depth at most 3: (or (and (or ...))).  Each
   inner OR mentions at most one relation."
  (cond ((literalp p) p)
	((eq (car p) 'and) (dnf p))
	((eq (car p) 'or) 
	 ; make p a collection of ANDs and literals
	 (setq p (flatten-operator p))  
	 ; for each AND, ensure all argscheck if each conjunct mentions 1 predicate; if not, split
	 (flatten-operator (maksor (mapcar #'coerce-to-compressible (cdr p)))))
	(t p)))

(defun simple-orp (x) (and (listp x) (eq (car x) 'or) (every #'literalp (cdr x))))

(defun compressiblep (p)
  (and (listp p) 
       (not (clausep p))
       (eq (first p) 'or)
       (every #'(lambda (x) (setq x (flatten-operator x)) 
			(or (literalp x)
			    (and (eq (car x) 'and)
				 (every #'(lambda (y) (or (literalp y)
							  (simple-orp y)))
					(cdr x)))))
	      (cdr p))))

(defun compress-or-and (p)
  "(COMPRESS-OR-AND P NEWRELNNAME) breaks and OR of ANDs P into a number of blocks,
   each of which satisfy the assumptions of COMPRESS-OR-AND-BLOCK.  Constructs 
   a table for each block and returns an equivalent P together with a set of tables
   (represented as a set of ground atoms)."
  (let (blocks head body rest)
    (setq blocks (group-by (or2list p) #'(lambda (x) (sort (uniquify (signed-relns x)) #'string< :key #'tostring)) 
			   :test #'equal))
    ;(print blocks)
    (dolist (b blocks)
      (multiple-value-bind (newp newtables) (compress-or-and-block (cdr b) (tosymbol (gensym "reln")))
	(push (second newp) head)
	(setq body (nconc (cddr newp) body))
	(setq rest (nconc newtables rest))))
    (values (list* '<= (maksor head) body) rest)))  
  
(defun compress-or-and-block (p &optional (newrelnname 'q))
  "(COMPRESS-OR-AND-FLAT-BLOCK P NEWRELNNAME) takes a list of conjunctions of
   disjunctions of ground, monadic literals where each disjunction mentions exactly one predicate.
   Assumes each predicate occurs
   exactly once in each AND, and that the set of predicates occurring in all ANDs is the same."
  (let ((h (make-hash-table :test #'equal)) (signs (make-hash-table))
	result (fields nil) formula vars entry head body newreln2)
    (setq fields (uniquify (relns (first p))))
    (setq p (mapcar #'and2list p))
    ;(print fields)
    ; arrange atoms into canonical ordering and record signs for each predicate
    (do ((i 0 (1+ i))
	 (ps p (cdr ps)) (sign) (qreln))
	((null ps))
      (dolist (q (car ps))  ; q is an atom, a negative literal, or a disjunction of literals
	(setq sign (uniquify (signed-relns q)))
	(when (cdr sign) 
	  (error 'structural-mismatch :comment (format nil "An embedded OR had differing signs: ~A" q)))
	(setq sign (first sign))
	(setq qreln (reln sign))
	(if (gethash qreln signs)   
	    (unless (samesigns (gethash qreln signs) sign) 
	      (error 'structural-mismatch 
		     :comment (format nil "Two disjuncts had differing signs for pred: ~A " qreln)))
	    (setf (gethash qreln signs) sign))
	(setf (gethash (list i (reln q)) h) (drop-not q))))
    ;(pretty-print h)
    ;(pretty-print signs)
    ; construct datalog definitions
    (setq result nil)
    (dotimes (i (length p))
      (setq head (list newrelnname))
      (setq body nil)
      (dolist (f fields)
	(setq entry (gethash (list i f) h))
	(cond ((atomicp entry) (push (second entry) head))
	      ((eq (car entry) 'or)
	       (push (newindvar) head)
	       (setq newreln2 (tosymbol (gentemp "reln")))
	       (push (list newreln2 (car head)) body)
	       (setq result (nconc (mapcar #'(lambda (x) (list newreln2 (second x))) (or2list entry)) result)))))
      (push (if body
		(list* '<= (nreverse head) body)
		(nreverse head)) 
	    result))
    ; construct formula
    (setq formula nil)
    (setq vars nil)
    (dolist (f fields)
      (push (newindvar) vars)
      (push (if (negative-literalp (gethash f signs))
		(maknot (list f (car vars)))
		(list f (car vars)))
	    formula))
    (setq formula (list* '<= (cons newrelnname (nreverse vars)) formula))
    (values formula (nreverse result))))

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
