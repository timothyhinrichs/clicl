;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; analysis.lisp
;;;     routines for analyzing logical sentences (usually polytime)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(eval-when (compile load eval)
  (proclaim '(special *real-ops*)))

(defun truthvaluep (thing) (or (eq thing 'true) (eq thing 'false)))

(defun head-literal (rule)
  "(HEAD-LITERAL RULE) returns the relation constants of the head literal in 
   RULE with NOT prepended if the head is negative (return is a string)."
  (cond ((atom rule) (symbol-name rule))
        ((listp rule)
         (cond ((eq (car rule) '<=)
                (stringify-head (cadr rule)))
               (t
                (stringify-head rule))))
        (t (stringify-head rule))))

(defun stringify-head (literal)
  "(STRINGIFY-HEAD LITERAL)"
  (cond ((atom literal) (symbol-name literal))
        ((listp literal)
         (cond ((eq (car literal) 'not)
                (cond ((atom (cadr literal)) 
		       (format nil "not~A" (cadr literal)))
                      ((listp (cadr literal)) 
		       (format nil "not~A" (caadr literal)))
                      (t "")))
               (t
                (symbol-name (car literal)))))
        (t "")))

(defun samesigns (lit1 lit2) 
  (or (and (positive-literalp lit1) (positive-literalp lit2))
      (and (negative-literalp lit1) (negative-literalp lit2))))

;(defun literalp (lit) 
;  (or (atomicp lit) 
;      (and (listp lit) (eq (car lit) 'not) (atomicp (second lit)))))

(defun positive-literalp (lit) (atomicp lit))
(defun negative-literalp (lit) (and (listp lit) (eq (car lit) 'not) (atomicp (second lit))))

(defun body (p)
  (cond ((atom p) nil)
        ((eq '<= (car p)) (cddr p))
        ((eq '=> (car p)) (butlast p))
        (t nil)))

(defun args (p)
  (cond ((atom p) nil)
        ((negative-literalp p) (if (atom (second p)) nil (cdr (second p))))
        ((positive-literalp p) (cdr p))
        (t nil)))

(defun monadic-atom (p) (and (atomicp p) (second p) (null (third p))))

(defun datap (p) (and (null (body p)) (groundp p)))

(defun disjunctionp (p)
  (cond ((atom p) nil)
        ((and (listp p) (eq (car p) 'or)) t)
        (t nil)))

(defun base-defn (p)
  "(BASE-DEFN P) is true iff p is a biconditional where the body is defined
   entirely in terms of equality."
  (cond ((atom p) nil)
        ((not (eq (car p) '<=>)) nil)
        ; (<=> (p @x) (phi @x))
        ((not (complex-sentencep (cadr p))) 
	 (or (and (boolean-sentencep (caddr p)) (equality-onlyp (caddr p))) 
	     (and (listp (caddr p)) (eq (car (caddr p)) '=))))
        ; (<=> (phi @x) (p @x))
        ((not (complex-sentencep (caddr p))) 
	 (or (and (boolean-sentencep (cadr p)) (equality-onlyp (cadr p))) 
	     (and (listp (cadr p)) (eq (car (caddr p)) '=))))
        (t nil)))
 
(defun boolean-sentencep (p)
  "(BOOLEAN-SENTENCEP P) is true iff P is a complex sentence built out of 
   only ors, ands, and nots, where there is at least one such connective.  
   Thus, atomic sentences are not boolean-sentences."
  (and (complex-sentencep p)
       (quantifier-free-sentencep p)))

(defun quantifier-free-sentencep (p)
  (cond ((atom p) t)
        ((find (car p) '(forall exists)) nil)
        ((eq (car p) 'not) (quantifier-free-sentencep (cadr p)))
        (t (every #'quantifier-free-sentencep (cdr p)))))

(defun complex-sentencep (p)
  (and (listp p)
       (find (car p) '(not forall exists or and => <= <=>))))

(defun universal-prenexp (p)
  "(UNIVERSAL-PRENEXP P) returns T iff P is a sentence of the form A*.M, where
   M is quantifier-free."
  (cond ((atom p) t)
        ((eq (car p) 'forall) (universal-prenexp (third p)))
        ((eq (car p) 'exists) nil)
        (t (quantifier-free-sentencep p))))

(defun existential-prenexp (p)
  "(EXISTENTIAL-PRENEXP P) returns T iff P is a sentence of the form E*.M, 
   where M is quantifier-free."
  (cond ((atom p) t)
        ((eq (car p) 'exists) (existential-prenexp (third p)))
        ((eq (car p) 'forall) nil)
        (t (quantifier-free-sentencep p))))

(defun prenexp (p)
  "(PRENEXP P) returns T iff P is a sentence of the form (E|A)*.M, where
   M is quantifier-free."
  (cond ((atom p) t)
        ((eq (car p) 'forall) (prenexp (third p)))
        ((eq (car p) 'exists) (prenexp (third p)))
        (t (quantifier-free-sentencep p))))

(defun literal-disjunctionp (p)
  (cond ((atom p) nil)
        (t (and (eq (car p) 'or) (every #'literalp (cdr p))))))

(defun equality-onlyp (p)
  "(EQUALITY-ONLY P) returns T iff the only relation in p is =."
  (every #'(lambda (x) (or (not (isrelation x)) 
			   (eq (parameter-symbol x) '=))) 
	 (get-vocabulary p)))

(defun size (p)
    (cond ((atom p) 1)
          ((member (car p) '(forall exists)) (1+ (size (third p))))
          ((member (car p) '(and or <= =>)) 
	   (1+ (reduce #'+ (mapcar #'size (cdr p)))))
          ((eq (car p) 'not) (1+ (size (second p))))
          ((eq (car p) '<=>) (1+ (+ (size (second p)) (size (third p)))))
          (t 1)))

(defun instantiated-vars (bl)
  "(INSTANTIATED-VARS BL) returns the list of variables bound in binding list BL 
   that are (transitively) bound to a ground term."
  (mapcarnot #'(lambda (x) (if (and (not (eq (car x) t)) (not (varp (cdr x)))) (car x) nil)) 
	     (transitively-close-bl bl)))

(defun transitively-close-bl (bl)
  "(TRANSITIVELY-CLOSE-BL BL) returns a binding list that is the transitive closure of 
   binding list BL.  Assumes BL is non-recursive, e.g. never includes x/y, y/z, z/x.
   Always returns a copy of BL, transitively closed."
  (setq bl (copy-list bl))  ; so we can be destructive
  (do ((keepgoing t)
       (newval)) 
      ((not keepgoing) bl)
    (setq keepgoing nil)
    (dolist (b bl)
      (when (varp (cdr b))
	(setq newval (plug (cdr b) bl))
	(when (not (eq newval (cdr b))) (setq keepgoing t))
	(setf (cdr b) newval)))))
  
(defun mgun (list &optional (mgu truth))
  "(MGUN LIST) computes an n-way unifier for LIST or NIL if the list is not unifiable."
  (cond ((null (cdr list)) mgu)
	(t
	 (do ((ls (cdr list) (cdr ls))
	      (tmp) 
	      (prev (car list)))
	     ((null ls) mgu)
	   (setq tmp (mgu prev (car ls) mgu))
	   (cond (tmp
		  (setq mgu (compose-mgus (list mgu tmp)))
		  (setq prev (plug (car ls) mgu)))
		 (t (return nil)))))))

(defun compose-mgus (mgus)
  "(COMPOSE-MGUS) returns an MGU that represents the application of the mgus in MGUS 
   in the order they appear, transitively closing, 
   e.g. {x/y} and {x/a,y/z} produces {x/z, y/z}."
  (let ((hash (make-hash-table)) (new (car mgus)) todelete)
    ; init hash table
    (dolist (n new)
      (setf (gethash (car n) hash) t))

    (dolist (mgu (cdr mgus))
      (setq todelete nil)
      ; apply the next mgu to the range of the current one,
      ;   keeping track of x/x bindings
      (dolist (n new)
	(setf (cdr n) (plug (cdr n) mgu))
	(when (and (eq (car n) (cdr n)) (not (eq (car n) t)))
	  (push (car n) todelete)))
      ; delete x/x instances
      (do ((n (cdr new) (cdr n))
	   (prev new (cdr prev)))
	  ((null (cdr n)))
	(when (member (caar n) todelete)
	  (setf (cdr prev) (cddr prev))
	  (setf (gethash (caar n) hash) nil)))
      (when (member (caar new) todelete)
	(setf (gethash (caar new) hash) nil)
	(setq new (cdr new)))
      ; add the new variables from the next mgu
      (dolist (m mgu)
	(unless (gethash (car m) hash)
	  (push m new)
	  (setf (gethash (car m) hash) t))))
    new))

(defun sentequal (p q &key (test #'equal))
  "(SENTEQUAL P Q) Assumes P,Q is a boolean combination of sentences.  
   Checks whether or not P and Q are equal up to reordering of AND/OR."
  (cond ((atomicp p) (funcall test p q))
	((atomicp q) (funcall test p q))
	((and (eq (car p) 'and) (eq (car q) 'and)) 
	 (seteq (cdr p) (cdr q) #'(lambda (x y) (sentequal x y :test test))))
	((and (eq (car p) 'or) (eq (car q) 'or))
	 (seteq (cdr p) (cdr q) #'(lambda (x y) (sentequal x y :test test))))
	(t (funcall test p q))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;; Tautology Detection ;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun tautp (list &optional (contrapreds))
  "(TAUTP LIST returns T iff the LIST of literals contains complementary literals.
   CONTRAPREDS is an alist of atoms to be treated as contradictory."
  (cond ((atom list) (eq list 'true))
	(t
	 (some #'(lambda (lit1) 
		   (some #'(lambda (lit2) 
			     (cond ((or (and (positive-literalp lit1) (positive-literalp lit2))
					(and (negative-literalp lit1) (negative-literalp lit2)))
				    (and (equal (args lit1) (args lit2))
					 (or (find (cons (reln lit1) (reln lit2)) contrapreds :test #'equal)
					     (find (cons (reln lit2) (reln lit1)) contrapreds :test #'equal))))
				   (t (and (eq (reln lit1) (reln lit2))
					   (equal (args lit1) (args lit2))))))
			 list)) list))))

(defun tautologyp (rule)
  "(TAUTOLOGYP RULE) returns T iff rule is a tautology, i.e. if within the body 
   there is both p and (not p)."
  (cond ((and (listp rule) (cdr rule) (cddr rule) (eq (car rule) '<=))
         (contradictory-antecedents rule))
        (t nil)))

(defun contradictory-antecedents (rule)
  "(CONTRADICTORY-ANTECEDENTS RULE) returns T iff rule has both 
    a literal and its negation in its body."
  (do ((poss (find-positive (cddr rule)) (cdr poss))
       (contradiction nil))
      ((or (null poss) contradiction) contradiction)
    (do ((ants (cddr rule) (cdr ants)))
        ((null ants))
      (when (equal (maknot (car poss)) (car ants))
        (setq contradiction t)) )))

(defun find-positive (list)
  "(FIND-POSITIVE LIST) returns all elements of list that are positive
   literals, i.e. do not begin with 'not."
  (remove-if #'negative-literalp list))


(defvar *matches* 0 "Number of matches performed.")
(defvar *subsumption* t
  "whether when checking for redundancy in residues to use subsumption
   or just samep")
(defvar *check-subsumption* nil
  "whether to check if the current residue is subsumed by any of the
   already-completed residues")

(defun epilog-tautologyp (p pl al context)
  "(EPILOG-TAUTOLOGYP PL AL CONTEXT) returns T if there are complementary
   literals on the stack."
  (declare (ignore pl))
  ; assume no tautologies within the rules, so that pl is not a tautology.
  ; check whether p is a tautology wrt context
  ;(format t "P: ~A, Alist(AL): ~A, plug(p,al): ~A~%" p (alist al) (plugstdexp p al))
  (setq p (plugstdexp p al))
  ; walk over all the contexts [outer loop]
  (do ((cs context (cdr cs))
       (taut nil) (cal nil))
      ((or (null cs) taut) taut)
      ; grab the binding list for the current context
      (setq cal (second (car cs)))
      ; walk over the literals in the context
      (do ((lits (caar cs) (cdr lits)))
          ((or (null lits) taut))
        ;(format t "    Lit: ~A, AL: ~A, alist(al): ~A, plugged lit: ~A~%" (car lits) al (alist cal) (plugstdexp (car lits) cal))
        (when (complementaryp p (plugstdexp (car lits) cal))
          (setq taut t)
          (return)))))


(defun complementaryp (lit1 lit2)
  "(COMPLEMENTARYP LIT1 LIT2) returns T iff LIT1 and LIT2 are complementary,
   i.e. one is p and one is (not p)."
  (cond ((atom lit1)
         (cond ((atom lit2) nil)
               ((and (eq (car lit2) 'not) (eq lit1 (cadr lit2))) t)
               (t nil)))
        ((and (eq (car lit1) 'not) (equal (cadr lit1) lit2)) t)
        ((atom lit2) nil)
        ((eq (car lit2) 'not) (equal lit1 (cadr lit2)))
        (t nil)))

(defun gen-tautologyelim-constraints (p)
  "(GEN-TAUTOLOGYELIM-CONSTRAINTS) takes an arbitrary formula P and
   constructs equality constraints on the free variables of P that 
   reduce the number of instances of P that are tautologies.
   Returns a list of constraints."
  (setq p (nnf p))
  (cond ((atom p) nil)
	((not (eq (car p) 'or)) nil)
	(t (setq p (flatten-operator p))
	   (do ((disjs (cdr p) (cdr disjs))
		(result nil) (bl nil))
	       ((null disjs) result)
	     (dolist (d (cdr disjs))
	       (when (or (and (positive-literalp (car disjs)) (negative-literalp d))
			 (and (negative-literalp (car disjs)) (positive-literalp d)))
		 (setq bl (mgu (drop-not (car disjs)) (drop-not d)))
		 (when bl 
		   (push (maksor (mapcarnot #'(lambda (x) 
						(if (eq (car x) t) nil `(not (= ,(car x) ,(cdr x)))))
					    bl))
			 result))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;; (Negative) Horn Detection ;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun horn-clausep (p &optional (ignorepreds nil))
  "(HORN-CLAUSEP P) returns T iff P is a disjunction of literals with at most one positive literal
   when all literals with predicate from IGNOREPREDS is ignored."
  (cond ((atom p) t)
	((eq (car p) 'or) 
	 (do ((ls (cdr p) (cdr ls))
	      (poscnt 0))
	     ((null ls) t)
	   (when (and (positive-literalp (car ls)) 
		      (not (member (relation (car ls)) ignorepreds :key #'parameter-symbol))) 
	     (setq poscnt (1+ poscnt))
	     (when (> poscnt 1) (return nil)))))
	(t nil)))

(defun horn-rulep (p)
  "(HORN-RULEP P) returns T iff P is the rule form of a horn clause."
  (cond ((atom p) t)   ; since must be a rule with 1 literal
        ((and (eq (car p) '<=) (cddr p))   ; rule with a body
         (cond ((positive-literalp (cadr p)) (every #'positive-literalp (cddr p)))
               (t                
                (do ((ls p (cddr ls))
                     (negcnt 0))
                    ((null ls) t)
                  (when (negative-literalp (car ls))
                    (setq negcnt (1+ negcnt)))
                  (when (> negcnt 1) (return nil))))))
        (t t))
)
(defun negative-horn-rulep (p)
  "(NEGATIVE-HORN-RULEP P) returns T iff negating all the literals in p is Horn."
  (cond ((atom p) t)  ; must be a rule with 1 literal
        ((and (eq (car p) '<=) (cddr p))
         (cond ((negative-literalp (cadr p)) (every #'negative-literalp (cddr p)))
               (t                
                (do ((ls p (cddr ls))
                     (poscnt 0))
                    ((null ls) t)
                  (when (positive-literalp (car ls))
                    (setq poscnt (1+ poscnt)))
                  (when (> poscnt 1) (return nil))))))
        (t t))
)

;;; Check horn type
(defun horn-type (th)
  "(HORN-TYPE TH) returns HORN if all rules in TH and included theories are Horn.  
  Returns NEGATIVE-HORN if all rules are the reverse of horn, i.e. every clause includes at most one negative
  literal.  Returns NIL otherwise."
  (all-same #'theory-horn-type (cons th (includees th)))
)

(defun theory-horn-type (th)
  "(THEORY-HORN-TYPE) returns HORN-TYPE of theory TH."
  (all-same #'rule-horn-type (contents th) :skip '(unknown))
)
(defun rule-horn-type (rule)
  "(RULE-HORN-TYPE RULE) returns HORN-TYPE of rule RULE."
  (cond ((literalp rule) 'unknown)
        ((horn-rulep rule) 'horn)
        ((negative-horn-rulep rule) 'negative-horn)
        (t nil))
)
(defun is-horn (x) (eq x 'horn))
(defun is-negative-horn (x) (eq x 'negative-horn))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;; Vocabulary ;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar *real-ops* '(forall exists <= => <=> and or not))
(defvar *propositions* nil 
  "Whether to differentiate propositions from relations of 0 arity.")
(defstruct parameter symbol arity type (univ :unknown))


(defun isfunction (param) (eq (parameter-type param) 'function))
(defun isrelation (param) (eq (parameter-type param) 'relation))
(defun isobject (param) (and (eq (parameter-type param) 'function)
			     (= (parameter-arity param) 0)))
(defun isproposition (param) (eq (parameter-type param) 'proposition))

(defun function-arity (param) (parameter-arity param))
(defun relation-arity (param) (parameter-arity param))

(defun preds (p) (if (member p '(true false)) nil (delete-if-not #'isrelation (get-vocabulary p))))
(defun pred (p) (first (preds p)))
(defun relns (p) (mapcar #'parameter-symbol (preds p)))
(defun reln (p) (first (relns p)))
(defun funcs (p) (if (member p '(true false)) nil (delete-if #'(lambda (x) (or (isobject x) (isrelation x))) (get-vocabulary p))))
(defun objs (p) (if (member p '(true false)) 
		    nil 
		    (mapcar #'parameter-symbol 
			    (delete-if-not #'isobject (get-vocabulary p)))))


(defun relation (rule)
  "(RELATION RULE) returns the relation for rule."
  (setq rule (head rule))
  (cond ((atom rule) rule)
        ((and (listp rule) (eq (car rule) 'not)) (relation (cadr rule)))
        (t (car rule))))

(defun signed-relation (rule)
  "(SIGNED-RELATION RULE) returns the positive or negative relation for rule."
  (setq rule (head rule))
  (if (and (listp rule) (eq (car rule) 'not))
    (cons 'not (list (relation rule)))
    (relation rule)))

; ignoring universe, which is not consistently computable
(defun param-equal (x y &key (test #'equalp))
  (cond ((and (parameter-p x) (parameter-p y))
	 (and (eq (parameter-symbol x) (parameter-symbol y))
	      (eq (parameter-type x) (parameter-type y))
	      (= (parameter-arity x) (parameter-arity y))))
	(t (funcall test x y))))

(defun find-atoms (p)
  "(FIND-ATOMS P) takes an arbitrarily complex sentence and returns 
   a list of all atoms, as they occur.  Useful for type inference."
  (cond ((atom p) (list p))
	((member (car p) '(and or not => <= <=>))
	 (mapcan #'find-atoms (cdr p)))
	((member (car p) '(forall exists))
	 (find-atoms (third p)))
	(t (list p))))

(defun get-vocabulary (p)
  "(GET-VOCABULARY P) returns a list of parameters representing all
    the function and relation constants in the sentence P.  Assumes
    the operations allowed in the logic are given by *real-ops* -- 
    allows for finding the vocabulary of subsets of FOL."
  (cond ((atom p) (list (make-parameter :symbol p 
					:arity 0 
					:type (if *propositions* 
						  'proposition 
						  'relation))))
        ((and (find (car p) '(or and <=> => <=))
              (find (car p) *real-ops*))
         (mapunion #'get-vocabulary (cdr p) :test #'equalp))
        ((and (eq (car p) 'not) (find 'not *real-ops*))
         (get-vocabulary (cadr p)))
	((and (eq (car p) 'naf) (find 'naf *real-ops*))
	 (get-vocabulary (cadr p)))
        ((and (find (car p) '(forall exists))
              (find (car p) *real-ops*))
         (get-vocabulary (caddr p)))
        (t
         (cons (make-parameter :symbol (car p) 
                               :arity (1- (length p)) 
                               :type 'relation)
               (mapunion #'get-functions (cdr p) :test #'equalp)))))
 
(defun get-functions (term)
  "(GET-FUNCTIONS TERM) returns a list of parameters, all of type function,
   with all duplicates removed when TERM is a term."
  (cond ((varp term) nil)
        ((atom term) 
	 (list (make-parameter :symbol term :arity 0 :type 'function)))
        (t (append (list (make-parameter :symbol (car term) 
					 :arity (1- (length term))
					 :type 'function))
                   (mapunion #'get-functions (cdr term) :test #'equalp)))))

(defun sentence-in-vocabp (p vocab)
  "(SENTENCE-IN-VOCABP P RELNS) checks whether the relations in P are a subset of RELNS."
  (subsetp (mapcar #'parameter-symbol (remove-if-not #'isrelation (get-vocabulary p)))
           vocab))

; versions where we care about signed relations

(defun signed-relns (p)
  "(SIGNED-RELNS P) returns a list of possibly negated symbols representing the occurrences of predicates
   in P."
  (mapcar #'(lambda (x) (if (parameter-p x) (parameter-symbol x) (list (first x) (parameter-symbol (second x)))))
	  (signed-preds p)))

(defun signed-preds (p) 
  "(SIGNED-RELNS P) returns a list of possibly negated parameters 
   representing the occurrences of predicates in P."
  (if (find p '(true false)) 
      nil
      (remove-if-not #'(lambda (x) (or (and (parameter-p x) (isrelation x)) (listp x)))
		     (get-signed-vocabulary p))))

(defun get-signed-vocabulary (p)
  "(GET-SIGNED-VOCABULARY P) returns a list of possibly negated parameters.
   Only negates a parameter for relation p if a literal of the form (not (p @t)) appears.
   Most useful when P is in NNF."
  (cond ((atom p) (list (make-parameter :symbol p 
					:arity 0 
					:type (if *propositions* 
						  'proposition 
						  'relation))))
        ((and (find (car p) '(or and <=> => <=))
              (find (car p) *real-ops*))
         (mapunion #'get-signed-vocabulary (cdr p) :test #'equalp))
        ((and (find (car p) '(naf not)) (find 'not *real-ops*))
	 (let ((v (get-signed-vocabulary (second p))))
	   (if (atomicp (second p))
	       (cons (maknot (first v)) (cdr v))
	       v)))
        ((and (find (car p) '(forall exists))
              (find (car p) *real-ops*))
         (get-signed-vocabulary (third p)))
        (t
         (cons (make-parameter :symbol (car p) 
                               :arity (1- (length p)) 
                               :type 'relation)
	       (mapunion #'get-functions (cdr p) :test #'equalp)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;; Dependency Graphs ;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; Note: dependency-graph treats predicates as symbols, whereas undirected-dependency-graph treats predicates
;   as Parameters (with arities, types, etc.)  
;  I believe it's fine to use just symbols, assuming that the rules are syntactically well-formed;
;    but, I need to check all uses of undirected-dependency-graph to ensure nothing is fundamentally wrong.

(defun dependency-graph (th &optional (ignorelist nil))
  "(DEPENDENCY-GRAPH TH IGNORELIST TEST) takes a datalog/prolog theory TH and
   returns a graph where the nodes are predicates (symbols), and there is an edge from 
   u to v if u appears in the body of a rule and v appears in the head.  
   Ignores included theories.  IGNORELIST is a list of symbols that are 
   not included in the graph, according to TEST."
  (let ((graph (make-agraph)) (preds) h)
    (setq ignorelist (adjoin '= ignorelist))

    ; build dependency graph from rules
    (dolist (r (contents th) graph)
      (setq h (reln (head r)))
      (unless (member h ignorelist)
	(setq preds (set-difference (relns (maksand (body r))) ignorelist))
	(when preds
	  (agraph-adjoin-noded h graph)
	  (dolist (p preds)
	    (agraph-adjoin-edged p h nil graph)))))))  

(defun ground-positive-dependency-graph (th &optional (ignorelist nil))
  "(GROUND-POSITIVE-DEPENDENCY-GRAPH TH IGNORELIST TEST) assumes TH is a ground datalog/prolog theory
   and does the same thing as dependency-graph but ignores negative literals and makes vertices of
   graph ground atoms."
  (let ((graph (make-agraph)) atoms h)
    ; build dependency graph from rules
    (dolist (r (contents th) graph)
      (setq h (head r))
      (unless (member h ignorelist :test #'equal)
	(setq atoms nil)
	(dolist (b (body r))
	  (if (negative-literalp b) 
	      (agraph-adjoin-noded (second b) graph :test #'equal)
	      (unless (member b ignorelist :test #'equal) (push b atoms))))
	(when atoms
	  (agraph-adjoin-noded h graph :test #'equal)
	  (dolist (p atoms)
	    (agraph-adjoin-edged p h nil graph :test #'equal)))))))  

(defun undirected-dependency-graph-vars (th &optional (ignorelist nil))
  "(UNDIRECTED-DEPENDENCY-GRAPH-VARS TH) returns a graph where the nodes are variables
   and there is an edge between u and v if and only if there is some sentence 
   that mentions both the variables u and v.  Ignores included theories.
   IGNORELIST is a list of variables that are not included in the graph."
  (let ((graph (make-agraph)) vars)

    ; build dependency graph from sentences
    (dolist (r (contents th) graph)
      (setq vars (set-difference (vars r) ignorelist))
      (when vars
        (agraph-adjoin-noded (car vars) graph)
        (do ((ps vars (cdr ps)))
            ((null ps))
          (dolist (otherp (cdr ps))
            (agraph-adjoin-edged (car ps) otherp nil graph)
            (agraph-adjoin-edged otherp (car ps) nil graph)))))))
  
(defun undirected-dependency-graph (th ignorelist &key (test #'equalp))
  "(UNDIRECTED-DEPENDENCY-GRAPH TH IGNORELIST) returns a graph where the nodes are predicates
   and there is an edge between u and v if and only if there is some sentence 
   that mentions both the predicates u and v.  Ignores included theories.
   IGNORELIST (which always includes =) is a list of relation constants that 
   are not included in the graph."
  (let ((graph (make-agraph)) (preds))
    (setq ignorelist (adjoin (make-parameter :symbol '= :arity 2 :type 'relation) ignorelist 
			     :test #'param-equal))

    ; build dependency graph from rules
    (dolist (r (contents th) graph)
      (setq preds (set-difference (preds r) ignorelist :test test))
      (when preds
        (agraph-adjoin-noded (car preds) graph :test test)
        (do ((ps preds (cdr ps)))
            ((null ps))
          (dolist (otherp (cdr ps))
            (agraph-adjoin-edged (car ps) otherp nil graph :test test)
            (agraph-adjoin-edged otherp (car ps) nil graph :test test)))))))

(defun partition-by-independence (ps crelns)
  "(PARTITION-BY-INDEPENDENCE PS) partitions the sentences PS so that none of the predicates in
  any one partition are dependent on any predicate in any other partition.  CRELNS are the
  relation constants with complete definitions, which are not included in PS."
  (let (components partitions comp a)
    (setq components (agraph-connected-components (undirected-dependency-graph ps crelns) :test #'equalp))
    (dolist (p ps)
      (setq comp (component (find crelns (preds p) 
				  :test #'(lambda (x y) (not (member y x :test #'equalp)))) components))
      (setq a (assoc comp partitions :test #'equalp))
      (if a
        (setf (cdr a) (cons p (cdr a)))
        (setq partitions (acons comp (list p) partitions))))
    (mapcar #'cdr partitions)))

(defun independentp (x y components crelns)
  "(INDEPENDENTP X Y COMPONENTS CRELNS) predicates X and Y are independent iff either one
   of them is complete, i.e. in CRELNS, or they are in different connected components."
  (cond ((or (member x crelns :test #'equalp) (member y crelns :test #'equalp)) t)
        ((not (equalp (component x components) (component y components))) t)
        (t nil)))

(defun component (x components)
  (dolist (c components nil)
    (when (member x c :test #'equalp)
      (return (car c)))))

(defun syntactic-finiteness-graph (th &optional (ignorelist nil))
  "(SYNTACTIC-FINITENESS-GRAPH TH) takes a set of clauses TH and returns a graph 
   where the nodes are signed predicates, and there is an edge from u and v if and only if 
   there is a clause that includes ~u and v in distinct literals. Ignores included theories.
   IGNORELIST is a list of signed predicates that are not to be included in the graph."
  (let ((graph (make-agraph)) (preds))
    (setq ignorelist (adjoin (make-parameter :symbol '= :arity 2 :type 'relation) ignorelist))

    ; build dependency graph from clauses
    (dolist (r (contents th) graph)
      (setq preds (set-difference (signed-relns r) ignorelist :test #'equal))
      (when preds
        (agraph-adjoin-noded (car preds) graph :test #'equal)
        (do ((ps preds (cdr ps)))
            ((null ps))
	  (do ((qs preds (cdr qs)))
	      ((null qs))
	    (when (not (eq ps qs))
	      (agraph-adjoin-edged (maknot (car ps)) (car qs) nil graph :test #'equal))))))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;; Loop Formulas ;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun loop-formulas (th)
  "(LOOP-FORMULAS TH) takes a set of ground logic programming rules and 
   returns loop formulas in ground classical logic for the maximal loops."
  (loop-formulas-aux th (max-loops th)))

(defun loop-formulas* (th)
  "(LOOP-FORMULAS TH) takes a set of ground logic programming rules and 
   returns loop formulas in ground classical logic for the maximal loops
   comprised of only atoms in the heads of rules."
  (let (candidates)
    (setq candidates (uniquify (mapcar #'head th)))
    (loop-formulas-aux th (delete-if-not #'(lambda (l) (subsetp l candidates :test #'equal)) (max-loops th)))))

(defun loop-formulas-aux (th loops)
  (let (result formulas)
    (setq th (group-by-hash (contents th) #'head :test #'equal))
    (dolist (l loops)
      (setq formulas (loop-formulas-es l th))
      (cond ((equal formulas '(true)) (push (maksand l) result))
	    ((null formulas) (push (maknot (maksand l)) result))
	    (t (push `(=> ,(maksand l) ,(maksor formulas)) result))))
    result))

(defun loop-formulas-es (loop hash)
  (let (ybodies results)
    (setq results nil)
    (dolist (y loop)
      (setq ybodies (mapcar #'body (gethash y hash)))
      (setq ybodies (delete-if-not #'(lambda (body) (null (intersection body loop :test #'equal))) ybodies))
      (setq results (nconc (mapcar #'maksand ybodies) results)))
    results))

(defun max-loops (th)
  "(MAX-LOOPS TH) finds all maximal loops in LP theory TH."
  (agraph-strongly-connected-components (ground-positive-dependency-graph th) :test #'equal))
  
(defun max-loops* (th)
  "(MAX-LOOPS TH) finds all maximal loops in LP theory TH involving just predicates in the rule heads."
  (agraph-strongly-connected-components* (ground-positive-dependency-graph th) :test #'equal)) 
  

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;; Detecting Completeness ;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun ps2iff (ps)
  "(PS2IFFMAX PS) takes a list of sentences PS and either computes an equivalent set of
   nonrecursive, biconditionals or returns NIL."
  (to-biconds3 ps (list (make-parameter :symbol '= :arity '2 :type 'relation))))

(defun ps2iff-old (ps)
  "(PS2IFFMAX PS) takes a list of sentences PS and either computes an equivalent set of
   nonrecursive, biconditionals or returns NIL and the list of disjunctions where that
   rewriting failed."
  (setq ps (mapcar #'(lambda (x) (simplify-andors (nnf (noimps x)))) ps))
  (setq ps (mapcan #'(lambda (x) (let ((p (flatten-toplevel-ands x))) 
                                   (if (and (listp p) (eq (car p) 'and)) (cdr p) (list p))))
                   ps))
  (ors2iff ps))


(defun ps2iffmax (ps)
  "(PS2IFFMAX PS) takes a list of sentences PS and either computes an equivalent set of
   nonrecursive, biconditionals or returns NIL and the list of sentences where that
   rewriting failed.  MAX version finds the maximal subset of the theory that can
   be so expressed and returns it along with the leftover sentences."
  (to-biconds3-max ps (list (make-parameter :symbol '= :arity '2 :type 'relation))))

(defun ps2iffmax-old (ps)
  "(PS2IFFMAX PS) takes a list of sentences PS and either computes an equivalent set of
   nonrecursive, biconditionals or returns NIL and the list of disjunctions where that
   rewriting failed.  MAX version finds the maximal subset of the theory that can
   be so expressed and returns it."
  (setq *globaltmp* nil)
  (setq ps (mapcar #'(lambda (x) (simplify-andors (nnf (noimps x)))) ps))
  (setq ps (mapcan #'(lambda (x) (let ((p (flatten-toplevel-ands x))) 
                                   (if (and (listp p) (eq (car p) 'and)) (cdr p) (list p))))
                   ps))
  (ors2iffmax ps))


(defun ors2iff (ps)
  "(ORS2IFF PS) takes a list of disjunctions and tries to transform them into a set of 
   nonrecursive, biconditional sentences."
  (ors2iff-aux ps (list (make-parameter :symbol '= :arity 2))))

(defun ors2iffmax (ps)
  "(ORS2IFF PS) takes a list of disjunctions and tries to transform them into a set of 
   nonrecursive, biconditional sentences. MAX version finds the largest subset of the 
   theory that can be written as iffs and returns it."
  (ors2iffmax-aux ps (list (make-parameter :symbol '= :arity 2))))

(defun ors2iff-aux (ps basetables)
  ;(declare (notinline ors2iff-aux))
  (let (relns sentences remaining error rdisjunctions iffs)

    ; find all sentences with some relation defined in terms of basetables
    ;   each element of sentences is (parameter . sentence)

    (setq sentences (mapcar #'(lambda (x) (cons (onereln x basetables) x)) ps))
    (multiple-value-setq (sentences remaining) (split #'car sentences))
    (setq remaining (mapcar #'cdr remaining))
    (setq relns (mapadjoin #'car sentences :test #'equalp))

    ; for each of the relations defined in this round, 
    ;    convert the sentences with those relations into <=>
    (setq iffs nil)
    (do ((rs relns (cdr rs))
         (iff))
        ((null rs))
      (multiple-value-setq (rdisjunctions sentences) 
	(split #'(lambda (x) (equalp (car x) (car rs))) sentences))
      (setq rdisjunctions (mapcar #'cdr rdisjunctions))
      (setq iff (imps2iff (car rs) (mapcar #'(lambda (x) (or2imp (car rs) x)) rdisjunctions)))
      (if iff
        (setq iffs (cons iff iffs))
        (return (setq error t))))

    ; if error, return error and disjunctions errored on
    ; if made progress and there is progress left to be made, recurse, ensuring errors are handled right
    ; else return all the iffs
    (cond (error (values nil rdisjunctions))
          ((and relns remaining)
           (multiple-value-setq (sentences rdisjunctions) (ors2iff-aux remaining (append basetables relns)))
           (if sentences
             (nconc iffs sentences)
             (values nil rdisjunctions)))
          (t iffs))))

(defun ors2iffmax-aux (ps basetables)
  (let (relns sentences remaining rdisjunctions iffs)

    ; find all sentences with some relation defined in terms of basetables
    ;   each element of sentences is (parameter . sentence)

    (setq sentences (mapcar #'(lambda (x) (cons (onereln x basetables) x)) ps))
    (multiple-value-setq (sentences remaining) (split #'car sentences))
    (setq remaining (mapcar #'cdr remaining))
    (setq relns (mapadjoin #'car sentences :test #'equalp))

    ; for each of the relations defined in this round, 
    ;    convert the sentences with those relations into <=>
    (setq iffs nil)
    (do ((rs relns (cdr rs))
         (iff))
        ((null rs) (cond ((and relns remaining) 
                          (nconc iffs (ors2iffmax-aux remaining (append basetables relns))))
                         (t (setq *globaltmp* (append remaining *globaltmp*)) 
                            iffs)))
      (multiple-value-setq (rdisjunctions sentences) 
	(split #'(lambda (x) (equalp (car x) (car rs))) sentences))
      (setq rdisjunctions (mapcar #'cdr rdisjunctions))
      (setq iff (imps2iff (car rs) (mapcar #'(lambda (x) (or2imp (car rs) x)) rdisjunctions)))
      (if iff
        (setq iffs (cons iff iffs))
        (setq *globaltmp* (append rdisjunctions *globaltmp*))))))

(defun or2imp (r p)
  "(OR2IMP R P) turns the disjunction p into an implication with a literal using 
   relation r at the head."
  (let (head body)
    (setq head (find (parameter-symbol r) (cdr p) :key #'relation))
    (setq body (remove head p :test #'equal))
    `(<= ,head ,(maknot body))))

(defun onereln (p ignore)
  (cond ((atom p) p)
        ((member (car p) '(and or not <= => <=>)) 
         (reduce #'(lambda (x y) (accum-relns x y ignore)) (mapcar #'(lambda (z) (onereln z ignore)) (cdr p))))
        ((member (car p) '(forall exists)) (onereln (third p) ignore))
        (t (make-parameter :symbol (relation p) :arity (if (atom p) 0 (length (cdr p)))))))

(defun accum-relns (x y ignore)
  (cond ((equalp x y) y)
        ((member x ignore :test #'equalp) y)
        ((member y ignore :test #'equalp) x)
        (t nil)))

(defun imps2iff (r ps)
  "(IMPS2IFF R PS) takes a parameter object R and a list of sentences PS.  Assume there is no sentence
   in ps that says the relation is true of everything."
  (let (newps pos neg head)
    ;(setq vs (maptimes #'newindvar (parameter-arity r)))
    (setq newps (mapcar #'(lambda (x) (orientimp (parameter-symbol r) x)) ps))
    (multiple-value-setq (neg pos) (split #'(lambda (x) (and (listp x) (eq (car x) '=>))) newps))
    ;(print pos)
    ;(print neg)
    (cond ((or (null pos) (null neg)) nil)
          (t
           ; for the head of the <=> it doesn't matter which one we pick--choose the first rule in pos
           (setq head (second (first pos)))
           ; construct the body of the positive rules, i.e. p <= p1, p <= p2 ... becomes p1 | p2 | ...
           (setq pos (maksor (mapcar #'(lambda (x) (if (fourth x) (maksand (cddr x)) (third x))) pos)))
           ; construct the body of the negative rules, i.e. p => p1, p => p2, ... becomes p1 ^ p2 ^ ...
           (setq neg (maksand (mapcar #'(lambda (x) (if (fourth x) (maksand (cddr x)) (third x))) neg)))
           ; check if the bodies are equal
           (setq pos (equivalentp2 pos neg))
           (if pos
             `(<=> ,head ,pos)
             nil)))))
 
(defun equivalentp (p q)
  "(EQUIVALENTP P Q) returns T if P and Q are equivalent.  Uses syntactic checks as opposed to semantic.
   Hence, sound but incomplete."
  (setq p (simplify-andors (nnf p)))
  (setq q (simplify-andors (nnf q)))
  (if (equal p q) p nil))

(defun equivalentp2 (p q)
  (setq p (canonicalize p))
  (setq q (canonicalize q))
  (if (equivalentp2-aux p q) p nil))

(defun canonicalize (p)
  (if (quantifier-free-sentencep p) 
    (dnf p)
    (simplify-andors (nnf p))))

(defun equivalentp2-aux (p q)
  (cond ((atom p) (eq p q))
        ((and (eq (car p) (car q)) (member (car p) '(and or)))  ; associative/commutative
         (and (subsetp (cdr p) (cdr q) :test #'equivalentp2-aux)
              (subsetp (cdr q) (cdr p) :test #'equivalentp2-aux)))
        (t (equal p q))))

(defun simplify-andors (p)
  (declare (notinline simplify-andors))
  (cond ((atom p) p)
        ((member (car p) '(and or)) 
         (cond ((null (cddr p)) (simplify-andors (second p)))
               (t (cons (car p) (mapcar #'simplify-andors (cdr (flatten-operator p)))))))
        ((member (car p) '(<= => <=> forall exists))
         (cons (car p) (mapcar #'simplify-andors (cdr p))))
        (t p)))



;;; New, hopefully cleaner, implementation
; algorithm for grouping sentences so that we can rewrite each group as a biconditional independent of all the others



(defun to-biconds3 (ps basepreds)
  "(TO-BICONDS3 PS) transforms PS into a logically equivalent set of biconditional, nonrecursive definitions
   with one definition per predicate in PS except =, if possible.  If not possible, returns NIL.
   Complete, with n^2/2 running time in the number of predicates."
  ;(format t "TO-BICONDS3(ps, ~A)~%" (parameter-symbol (car basepreds)))
  (let (sents preds partition bicond remaining rest pred)
    (setq sents (mapcarnot #'(lambda (x) (let ((p (set-difference (preds x) basepreds :test #'equalp)))
                                           (if (cdr p) nil (cons (first p) x))))
                           ps))
    (setq preds (mapadjoin #'car sents :test #'equalp))
    ;(format t "Preds choicepoint: ~A~%" preds)

    ; find a biconditional
    (setq bicond nil)
    (dolist (p preds nil)
      (setq partition (mapcarnot #'(lambda (x) (if (equalp (car x) p) (cdr x) nil)) sents))
      (setq bicond (reformulate-to-biconditional partition p))
      (when bicond 
        (setq pred p) 
        (return)))

    ; recurse or fail as appropriate
    (format t "Continuing with: ~A~%" pred)
    (cond ((not bicond) nil)
          (t
           (setq remaining (set-difference ps partition :test #'equal))
           (cond (remaining
                  (setq rest (to-biconds3 remaining (cons pred basepreds)))
                  (if rest (cons bicond rest) nil))
                 (t
                  (list bicond)))))))

(defun to-biconds3-max (ps basepreds)
  "(TO-BICONDS3-MAX PS) transforms a maximal subset of PS into a logically equivalent set of biconditional, 
   nonrecursive definitions with one definition per predicate in PS except =.  Returns the biconditionals
   and the remaining sentences.  Complete (assuming reformulate-to-biconditional is complete), 
   with n^2/2 running time in the number of predicates."
  ; only changes from to-biconds3: (1) recursive call (2) returns bicond even if REST is nil
  ;    (3) returning not only bicond but also remaining sentences
  ;(format t "TO-BICONDS3MAX(ps, ~A)~%" (parameter-symbol (car basepreds)))
  (let (sents preds partition bicond remaining rest pred)
    (setq sents (mapcarnot #'(lambda (x) (let ((p (set-difference (preds x) basepreds :test #'equalp)))
                                           (if (cdr p) nil (cons (first p) x))))
                           ps))
    (setq preds (mapadjoin #'car sents :test #'equalp))
    (when *trace-reform2iffs* (format t "Preds choicepoint: ~A~%" preds))

    ; find a biconditional
    (setq bicond nil)
    (dolist (p preds nil)
      (setq partition (mapcarnot #'(lambda (x) (if (equalp (car x) p) (cdr x) nil)) sents))
      (setq bicond (reformulate-to-biconditional partition p))
      (when bicond 
        (setq pred p) 
        (return)))

    ; recurse or fail as appropriate
    (when *trace-reform2iffs* (format t "Continuing with: ~A~%" pred))
    (cond ((not bicond) (values nil ps))
          (t
           (setq remaining (set-difference ps partition :test #'equal))
           (cond (remaining
                  (multiple-value-setq (rest remaining) (to-biconds3-max remaining (cons pred basepreds)))
                  (values (cons bicond rest) remaining))
                 (t
                  (values (list bicond) nil)))))))

(defun to-biconds2 (ps basepreds)
  "(TO-BICONDS2 PS) transforms PS into a logically equivalent set of biconditional, nonrecursive definitions
   with one definition per predicate in PS except =, if possible.  If not possible, returns NIL.
   Complete, but n!ish running time in the number of predicates."
  ;(format t "TO-BICONDS2(ps, ~A)~%" (parameter-symbol (car basepreds)))
  (let (sents preds partition bicond remaining rest)
    (setq sents (mapcarnot #'(lambda (x) (let ((p (set-difference (preds x) basepreds :test #'equalp)))
                                           (if (cdr p) nil (cons (first p) x))))
                           ps))
    (setq preds (mapadjoin #'car sents :test #'equalp))
    (format t "Preds choicepoint: ~A~%" preds)
    (dolist (p preds nil)
      (setq partition (mapcarnot #'(lambda (x) (if (equalp (car x) p) (cdr x) nil)) sents))
      (setq bicond (reformulate-to-biconditional partition p))
      (when bicond
        (setq remaining (set-difference ps partition :test #'equal))
        (cond (remaining
               (setq rest (to-biconds2 remaining (cons p basepreds)))
               (if rest (return (cons bicond rest))))
              (t (return (list bicond))))))))



(defun to-biconds (ps)
  "(TO-BICONDS PS) transforms PS into a logically equivalent set of biconditional, nonrecursive definitions
   with one definition per predicate in PS except =, if possible.  If not possible, returns NIL.
   Linear in the number of predicates but incomplete."
  (let (groups vocab preds bs newpred equality)
    (setq equality (make-parameter :symbol '= :arity 2 :type 'relation))
    (setq vocab (adjoin equality (get-vocabulary (maksand ps)) :test #'equalp))
    (setq groups (group-for-iff ps))
    (cond ((or (not groups) (not (= (length groups) (1- (length (remove-if-not #'isrelation vocab)))))) (print "Wrong number of groups") nil)
          (t
           (setq preds (list equality))
           (dolist (g groups (nreverse bs))
             (setq newpred (set-difference (remove-if-not #'isrelation (adjoin equality (get-vocabulary (maksand g)) :test #'equalp)) preds :test #'equalp))
             (when (or (not newpred) (cdr newpred)) (print "no newpred found") (return nil))
             (setq newpred (first newpred))
             (setq bs (cons (reformulate-to-biconditional g newpred) bs))
             (when (not (first bs)) (format t "(REFORMULATE-TO-BICONDITIONAL ~A ~A) returned NIL~%" g newpred) (return nil))
             (setq preds (cons newpred preds)))))))

(defun group-for-iff (ps)
  "(GROUP-FOR-IFF PS) returns a partitioning of PS or NIL.  A partitioning is returned whenever
    the sentences in PS can totally ordered: the smallest partition is a set of sentences where only one
    predicate besides = appears.  Every parition besides the smallest has one predicate besides all the predicates
    that appear below it.  If there is no such ordering, we return NIL." 
  (let (hash preds partitions)
    (setq hash (make-hash-table))
    (setq preds (remove-if-not #'isrelation (get-vocabulary (maksand ps))))
    (group-for-iff-aux ps (list (make-parameter :symbol '= :arity 2)) hash 0)

    (dolist (p preds)
      (when (get (parameter-symbol p) hash)
        (setq partitions (cons (get (parameter-symbol p) hash) partitions))))
    (print partitions)
    (mapcar #'car (sort partitions #'< :key #'cdr))))

#|
    (cond ((not hash) nil)
          (t (mapcar #'car (sort (maphash #'(lambda (key value) (declare (ignore key)) value) hash) #'< :key #'cdr))))))
|#

(defun group-for-iff-aux (ps baserelns hash cnt)
  (declare (notinline group-for-iff-aux))
  (let (sentences remaining relns partition)
    (setq sentences (mapcar #'(lambda (x) (cons (onereln x baserelns) x)) ps))
    (multiple-value-setq (sentences remaining) (split #'car sentences))
    (setq remaining (mapcar #'cdr remaining))
    (setq relns (mapadjoin #'car sentences :test #'equalp))
    ; partition sentences so that each set has the same one predicate besides BASERELNS
    ; put each predicate in hash
    (dolist (r relns)
      (setq partition nil)
      (dolist (s sentences)
        (when (equalp r (car s))
          (setq partition (cons (cdr s) partition))))
      (if (get (parameter-symbol r) hash)
        (setf (get (parameter-symbol r) hash) (cons (nconc partition (car (print (get (parameter-symbol r) hash)))) cnt))
        (setf (get (parameter-symbol r) hash) (cons partition cnt)))
      (setq cnt (1+ cnt)))
    (cond ((and remaining relns) (group-for-iff-aux remaining (nconc relns baserelns) hash cnt))
          ((and (not remaining) relns) hash)
          ((and remaining (not relns)) nil)
          (t nil))))
#|
    (when (and remaining relns)
      (setq res (group-for-iff-aux remaining (append relns baserelns))))
    (cond ((null remaining) partitions)
          ((null relns) nil)
          ((null res) nil)
          (t (format t "partitions: ~A~%res: ~A~%" partitions res)(print (nconc partitions res))))))
|#

(defun reformulate-to-biconditional (ps pred)
  "(REFORMULATE-TO-BICONDITIONAL PS PRED) tries to find a rewriting of PS that takes the form of
   pred(xbar) <=> phi(xbar).  PRED is actually a parameter.  
   Returns biconditional if successful; else returns NIL."
  ; turn ps into disjunctions where possible
  (setq ps (mapcar #'(lambda (x) (simplify-andors (nnf (noimps x)))) ps))
  (setq ps (mapcan #'(lambda (x) (let ((p (flatten-toplevel-ands x))) 
                                   (if (and (listp p) (eq (car p) 'and)) (cdr p) (list p))))
                   ps))  
  (imps2iff pred (mapcar #'(lambda (x) (or2imp pred x)) ps)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
