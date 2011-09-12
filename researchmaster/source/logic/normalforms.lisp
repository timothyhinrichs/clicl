;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; normalforms.lisp
;;    routines for translating logical sentences to canonical forms
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun brfss (p)
  "(BRFSS P) returns the backwards rule form of P, without the <= if
  there there are no antecedents. stands for brfs simplified"
  (let ((rf (brfs p)) (result nil))
    (dolist (r rf result)
      (setq result (cons (if (null (cddr r))
                           (cadr r)
                           r)
                         result)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;; Predicate Completion ;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun predicate-completion (th)
  "(PREDICATE-COMPLETION TH) returns TH after having applied predicate completion.
   Ignores included theories."

  (let ((newth nil)
        (vocab (get-vocabulary (maksand (contents th)))))

    ; walk over all the relations
    (do ((rs (remove-if-not #'isrelation vocab) (cdr rs))
         (es) (tmplist) (vs) (newhead))
        ((null rs) newth)

      ; find all the rules in th with (car rs) as the head relation--we assume they are all positive.  
      (setq tmplist (remove-if-not #'(lambda (x) (eq (relation x) (parameter-symbol (car rs)))) 
                                   (indexps (parameter-symbol (car rs)) th)))

      (when tmplist
        ; generate the variables for the head of the completed rule
        (setq vs (maptimes #'newindvar (parameter-arity (car rs))))

        ; turn each rule into an existentialized version, using vs as the  variables in the head of the rule
        (setq es (mapcar #'(lambda (x) (existentialize-rule x vs)) tmplist))

        ; create the new head
        (cond (vs (setq newhead (cons (parameter-symbol (car rs)) vs)))
              (t (setq newhead (head (car tmplist)))))

        ; create <=> rule and store in newth
        (setq newth (cons `(<=> ,newhead ,(maksor (mapcar #'caddr es))) newth))))))

(defun predicate-completion-th (th)
  "(PREDICATE-COMPLETION TH) returns a new theory after having applied predicate completion."
  (define-theory (make-instance 'theory) "" (predicate-completion th)))

(defun predicate-completion-reln (r th)
  "(PREDICATE-COMPLETION-RELN R TH) returns an iff sentence from all the rules
   with R in the head in the theory TH."
  (let (vs (rules (indexps `(,r @x) th)))
    (setq vs (maptimes #'newindvar (length (vars (head (car rules))))))
    (setq rules (mapcar #'(lambda (x) (caddr (existentialize-rule x vs))) rules))
    (cond (vs `(<=> ,(cons r vs) ,(maksor rules)))
          (t  `(<=> ,r ,(maksor rules))))))


(defun rewrite-fol (p)
  "(REWRITE-FOL P) applies some rewritings to the first-order formula P."
  ; replace distinct with (not (= ...))
  (cond ((atom p) p)
        ((find (car p) '(<= => <=> or and forall exists))
         (mapcar #'rewrite-fol p))
        ((eq (car p) 'not)
         (maknot (rewrite-fol (cadr p))))
        (t
         (if (and (listp p) (eq (car p) 'distinct))
           `(not (= ,(cadr p) ,(caddr p)))
           p)))
)
(defun existentialize-rule (r vs)
  "(EXISTENTIALIZE-RULE R VS) replaces the terms in the head of the rule R with VS, adds conjuncts to the 
   rule body that state the old variables equal the new variables, and existentially quantifies all old variables 
   in the body."
  (existentially-quantify-body (similarize-head r vs)))

(defun head-args (arity) (nunique arity "?X"))
(defun nunique (n &optional (prefix ""))
  (do ((i 0 (1+ i))
       (v nil))
      ((= i n) (nreverse v))
    (push (tosymbol (format nil "~A~A" prefix i)) v)))

(defun similarize-head (r vars)
  "(SIMILARIZE-HEAD R VARS) takes any sentence of the forms (Op Lit p1 ... pn) or Lit and a list of variables VARS.
   Returns a rule equivalent to R except all of the arguments to Lit are VARS."
  (cond ((null vars) r)
	((atom r) r)
        (t
         (unless (member (car r) '(<= =>)) (setq r (list '<= r)))  ; stick a <= on the front, if not already there
	 (let (head newhead equality oldargs)
	   (setq head (drop-not (head r)))
	   (setq oldargs (cdr head))
	   (cond ((atom head) r)
		 ((not (= (length oldargs) (length vars))) r)
		 (t
		  (setq equality nil)
		  (do ((vs vars (cdr vs))
		       (as oldargs (cdr as)))
		      ((null vs))
		    (unless (equal (car vs) (car as)) (push `(= ,(car vs) ,(car as)) equality)))
		  (setq equality (nreverse equality))
		  (setq newhead (cons (first head) vars))
		  (when (negative-literalp (head r)) (setq newhead (maknot newhead)))
		  (maksred (cons newhead (nconc equality (cddr r))))))))))

(defun existentially-quantify-body (p)
  (cond ((atom p) p)
        ((not (eq (car p) '<=)) p)
        (t
         (let (bodyvars headvars diff)
           (setq bodyvars (freevars (cddr p)))
           (setq headvars (freevars (second p)))
           (setq diff (set-difference bodyvars headvars))
           (if diff
             `(<= ,(second p) (exists ,diff ,(third p)))
             p)))))

(defun first-result (func list)
  (let (m)
    (dolist (l list m)
      (setq m (funcall func l))
      (when m (return m)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;; Small CNF ;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun inseado-ground (p dca)
  "(INSEADO-GROUND P DCA) returns a ground form of P, according to DCA (a list of symbols),
   after applying inseado.  May introduce new propositional constants."
  ; start by translating into negation normal form, i.e. and, or, not with not pushed all the way in
  (let ((np)
        (newps))
    (setq np (nnf p))
    (setq newps (ground np dca))
    (brfs (maksand newps))))

(defun ground-herbrands-small-clauses (ps &optional (tables t))
  (mapcan #'clauses (ground-herbrands-small ps tables)))

(defun ground-herbrands-small (ps &optional (tables t))
  (let ((dca (compute-dca ps)))
    (mapcan #'(lambda (x) (ground-herbrand-small x dca tables)) (contents ps))))

(defun ground-herbrand-small (p dca &optional (tables t))
  (let (newp aux)
    (if (and tables (base-defn p))
      (setq newp p aux nil)
      (multiple-value-setq (newp aux) (formula-renaming p)))
    (mapcan #'(lambda (x) (mapcan #'drop-ands (ground-herbrand x dca nil))) (cons newp aux))))


;;;;;;;;;; small cnf conversion

(defun brf-small (p)
  (let (a b)
    (multiple-value-setq (a b) (formula-renaming p))
    (mapcan #'brfs (cons a b))))

(defun cnf-small (p)
  (let (a b) 
    (multiple-value-setq (a b) (formula-renaming p))
    (mapcan #'clauses (cons a b))))

(defun formula-renamings (p)
  (let (a b)
    (multiple-value-setq (a b) (formula-renaming p))
    (cons a b)))

(defun formula-renaming (p &optional (pol 1))
  "(FORMULA-RENAMING P POL) returns a new formula and a list of formulas that are satisfiable 
   iff p is satisfiable such that there are no embedded formulas, i.e. the complexity of each 
   formula in the result is bounded by 4."
  (let (newp aux r)
    (cond ((atom p) (values p nil))
          ((eq (car p) 'not)
           (multiple-value-setq (newp aux) 
	     (formula-renaming (second p) (new-polarity 'not 1 pol)))
           (values (maknot newp) aux))
          ; doing something sneaky here.
          ;    since (literalp '(?x ?y ?z)) and (literalp '?x) are true, 
	  ;    we're handling forall and exists by treating vars as though they were formulas.
          ((member (car p) '(and or <= => <=> forall exists)) 
           (setq newp nil)
           (setq aux nil)
           (do ((ps (cdr p) (cdr ps))
                (cnt 1 (1+ cnt))
                (tmpp) (tmpaux)
                (newpol nil))
               ((null ps) (setq newp (cons (car p) (nreverse newp))))
             
             ; only recurse if the current piece is not a literal
             (cond ((literalp (first ps)) (setq newp (cons (first ps) newp)))
                   (t
                    ; new relation
                    (setq r (cons (gentemp "reln") (freevars (first ps))))
                    ; recurse
                    (setq newpol (new-polarity (first p) 
					       (if (and (eq (first p) '=>) (null (cdr ps))) 
						   '* cnt) pol))
                    (multiple-value-setq (tmpp tmpaux) (formula-renaming (first ps) newpol))
                    
                    ; produce new sentence
                    (cond ((= newpol 1) (setq tmpp `(<= ,tmpp ,r)))
                          ((= newpol -1) (setq tmpp `(<= ,r ,tmpp)))
                          ((= newpol 0) (setq tmpp `(<=> ,r ,tmpp)))
                          (t (setq tmpp `(uhoh ,p))))
                    
                    ; join new relation to current sentence and add new sentence and
		    ;   other sentences to set
                    (setq newp (cons r newp))
                    (setq aux (nconc (cons tmpp tmpaux) aux)))))
           (values newp aux))
          (t
           (values p nil)))))

(defun formula-renaming-small (p &optional (type 'unsat) (pol 1))
  "(FORMULA-RENAMING P POL) returns a new formula and a list of formulas that are satisfiable 
   iff p is
   satisfiable such that there are no embedded formulas, i.e. the complexity of each formula
   in the result is bounded by 4."
  (cond ((atom p) (values p nil))
        ((eq (car p) 'not) 
         (cond ((atom (second p)) (values p nil))  ; since this must be a literal
               ((eq (car (second p)) 'not) (formula-renaming-small (second (second p)) type pol))
               ((allbutone #'literalp (cdr (second p))) 
		(reify-subsubformulae p type (new-polarity 'not 1 pol)))
               (t (reify-subformulae p type (new-polarity 'not 1 pol)))))
        ((eq (car p) 'forall)
         (cond ((atom (third p)) (values p nil))
               ((member (car (third p)) '(and <=>)) (reify-subsubformulae p type pol))
               (t (reify-subformulae p type pol))))
        ((eq (car p) 'exists)
         (cond ((atom (third p)) (values p nil))
               ((member (car (third p)) '(or => <=)) (reify-subsubformulae p type pol))
               (t (reify-subformulae p type pol))))
        ((member (car p) '(and or <= => <=>))
         (cond ((allbutone #'literalp (cdr p))
                (reify-subsubformulae p type pol))
               (t (reify-subformulae p type pol))))
        (t
         (values p nil))))         

(defun reify-subsubformulae (p type pol)
  "(REIFY-SUBSUBFORMULAE P POL) introduces new relation constants for every complex formulae
   two levels deep.  Returns new P and supporting definitions."
  (let ((newp nil) (aux nil))
    (do ((ps (cdr p) (cdr ps)) (tmpp) (tmpaux) (newpol) (cnt 1 (1+ cnt)))
        ((null ps) (setq newp (cons (first p) (nreverse newp))))
      (setq newpol (new-polarity (first p) 
				 (if (and (eq (first p) '=>) (null (cdr ps))) '*  cnt) 
				 pol)) 
      (multiple-value-setq (tmpp tmpaux) (reify-subformulae (car ps) type newpol))
      (setq newp (cons tmpp newp))
      (setq aux (nconc tmpaux aux)))
    (values newp aux)))
  
(defun reify-subformulae (p type pol)
  "(REIFY-SUBFORMULAE P POL) introduces new relation constants for each of the subformulae in P
   unless the subformulae is a literal.  Returns the new version of the sentence P and the 
   list of supporting constraints."
  (let (newp aux)
    ; for all the subpieces of p, recurse: produces newp, the replacement for p; and aux, 
    ;    a set of new formulas, already reduced
    (setq newp nil)
    (setq aux nil)
    (do ((ps (cdr p) (cdr ps))  ; even if a quantified sentence, (?x) and ?x both satisfy literalp
         (cnt 1 (1+ cnt))
         (tmpp) (tmpaux)
         (newpol nil))
        ((null ps) (setq newp (cons (car p) (nreverse newp))))
             
      ; only recurse if the current piece is not a literal
      (cond ((literalp (first ps)) (setq newp (cons (first ps) newp)))
            (t 
             ; recurse
             (setq newpol (new-polarity (first p) 
					(if (and (eq (first p) '=>) (null (cdr ps))) '* cnt) 
					pol)) 
             (multiple-value-setq (tmpp tmpaux) (reify-formula (first ps) type newpol))
                          
             ; join new relation to current sentence and add new sentence and other sentences 
	     ;   to set
             (setq newp (cons tmpp newp))
             (setq aux (nconc tmpaux aux)))))
    (values newp aux)))

(defun reify-formula (p type pol)
  "(REIFY-FORMULA P POL) defines a new relation constant that is polarity-equivalent to p
   and returns an atom of that constant together with its definitions."
  (let (newp aux r aux2)
    (multiple-value-setq (newp aux) (reify-subformulae p type pol))
    
    ; new predicate
    (setq r (cons (gentemp "reln") (freevars newp)))

    ; create definition for r
    (cond ((eq type 'valid) (setq newp `(<=> ,r ,newp)))
	  ((eq type 'unsat)  
	   (cond ((= pol 1) (setq newp `(<= ,newp ,r)))
		 ((= pol -1) (setq newp `(<= ,r ,newp)))
		 ((= pol 0) (setq newp `(<=> ,r ,newp)))
		 (t (setq newp `(uhoh ,p)))))
	  (t (setq newp `(other-uhoh ,p))))

    ; reduce it as well
    (multiple-value-setq (newp aux2) (formula-renaming-small newp type))

    ; finally return the atom corresponding to P and the supporting formulae
    (values r (cons newp (nconc aux2 aux)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;; Negation Normal Form ;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun noimps (p)
  "(NOIMPS P) returns a version of P without implications or biconditionals."
  (cond ((atom p) p)
        ((eq (car p) 'not) (maknot (noimps (cadr p))))
        ((eq (car p) '<=) 
	 (maksor (cons (noimps (cadr p)) 
		       (mapcar #'(lambda (x) (noimps (maknot x))) (cddr p)))))
        ((eq (car p) '=>) 
	 (maksor (nconc (mapcar #'(lambda (x) 
				    (noimps (maknot x))) (cdr (butlast p))) 
			(list (noimps (car (last p)))))))
        ((eq (car p) '<=>) 
	 (noimps `(and (<= ,(cadr p) ,(caddr p)) (=> ,(cadr p) ,(caddr p)))))
        ((find (car p) '(and or)) (cons (car p) (mapcar #'noimps (cdr p))))
        ((find (car p) '(forall exists)) 
	 (list (car p) (cadr p) (noimps (caddr p))))
        (t p)))

(defun nnf (p)
  (cond ((atom p) p)
        ((eq (car p) 'and) (cons 'and (mapcar #'nnf (cdr p))))
        ((eq (car p) 'or) (cons 'or (mapcar #'nnf (cdr p))))
        ((eq (car p) 'not) (nnfn (cadr p)))
        ((eq (car p) '<=) (list* 'or  (nnf (cadr p)) (mapcar #'nnfn (cddr p))))
        ((eq (car p) '=>) (append (cons 'or (mapcar #'nnfn (cdr (butlast p)))) 
				  (list (nnf (car (last p))))))
        ((eq (car p) '<=>)  (nnf `(and (<= ,(second p) ,(third p))
				       (=> ,(second p) ,(third p)))))
        ((eq (car p) 'forall) (list 'forall (second p) (nnf (third p))))
        ((eq (car p) 'exists) (list 'exists (second p) (nnf (third p))))
        (t p)))

(defun nnfn (p)
  (cond ((atom p) (maknot p))
        ((eq (car p) 'and) (cons 'or (mapcar #'nnfn (cdr p))))
        ((eq (car p) 'or) (cons 'and (mapcar #'nnfn (cdr p))))
        ((eq (car p) 'not) (nnf (cadr p)))
        ((eq (car p) '<=) (list* 'and  (nnfn (cadr p)) (mapcar #'nnf (cddr p))))
        ((eq (car p) '=>) (append (cons 'and (mapcar #'nnf (cdr (butlast p)))) 
				  (list (nnfn (car (last p))))))
        ((eq (car p) '<=>) (nnfn `(and (<= ,(second p) ,(third p))
				       (=> ,(second p) ,(third p)))))
        ((eq (car p) 'forall) (list 'exists (second p) (nnfn (third p))))
        ((eq (car p) 'exists) (list 'forall (second p) (nnfn (third p))))
        (t (maknot p))))

(defun insea (p) (insea-aux p nil nil))
(defun insea-aux (p univs bl)
  "(INSEA-AUX P UNIVS BL) performs INEA on P in the context of UNIVS universal
   variables with (skolem) binding list BL." 
  (cond ((atom p) p)
        ((find (car p) '(and or)) 
         (cons (car p) (mapcar #'(lambda (x) (insea-aux x univs bl)) (cdr p))))
        ((eq (car p) 'not) 
         (insean (cadr p) univs bl))
        ((eq (car p) 'forall) 
         (insea-aux (caddr p) 
                (if (listp (cadr p)) 
		    (union (cadr p) univs) 
		    (adjoin (cadr p) univs))
                (add-new-vars (cadr p) bl)))
        ((eq (car p) 'exists)
         (insea-aux (caddr p) univs (add-skolemized-vars (cadr p) univs bl)))
        (t (insea-exp p bl))))

(defun insean (p univs bl)
  "(INSEA-EXP P UNIVS BL) performs INSEA on the negation of P."
  (cond ((atom p) (maknot p))
        ((eq (car p) 'and)
         (cons 'or (mapcar #'(lambda (x) (insean x univs bl)) (cdr p))))
        ((eq (car p) 'or)
         (cons 'and (mapcar #'(lambda (x) (insean x univs bl)) (cdr p))))
        ((eq (car p) 'not) 
         (insea-aux (cadr p) univs bl))
        ((eq (car p) 'forall) 
         (setq bl (add-skolemized-vars (cadr p) univs bl))
         (insean (caddr p) univs bl))
        ((eq (car p) 'exists) 
         (insean (caddr p) 
                 (if (listp (cadr p)) 
		     (union (cadr p) univs) 
		     (adjoin (cadr p) univs))
                 (add-new-vars (cadr p) bl)))
        (t (maknot (insea-exp p bl)))))

(defun insea-exp (p bl)
  (cond ((varp p)
         (let ((val (assoc p bl)))
           (if (cdr val) (cdr val) p)))
        ((atom p) p)
        (t (mapcar #'(lambda (x) (insea-exp x bl)) p))))

(defun add-skolemized-vars (vs univs bl)
  (cond ((atom vs) (acons vs (cons (gentemp "F") univs) bl))
        (t (dolist (v vs bl)
	     (setq bl (acons v (cons (gentemp "F") univs) bl))))))
(defun add-new-vars (vs bl)
  (cond ((atom vs) (acons vs (newindvar) bl))
        (t (dolist (v vs bl)
	     (setq bl (acons v (newindvar) bl))))))

(defun flatten-toplevel-ands (p)
  "(FLATTEN-TOPLEVEL-ANDS P) flattens nested ands at the toplevel of P."
  (labels ((aux (q) 
             (cond ((atom q) (list q))
                   ((eq (car q) 'and)
                    (let ((res nil))
                      (dolist (h (cdr q) res)
                        (setq res (nconc res (aux h))))))
                   (t (list q)))))
    (cons 'and (aux p))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Contrapositives -- where certain predicates are ignored 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun contrapositives* (p &optional (ignorepreds nil))
  (mapcan #'(lambda (x) (contrapositivesexp* x ignorepreds)) (brfs p)))

(defun contrapositivesexp* (rule ignorepreds)
  (cond ((atom rule) (list rule))
	((not (eq (car rule) '<=)) (list rule))
	(t
	 (do ((ls (body rule) (cdr ls))
	      (result (if (member (relation (head rule)) ignorepreds :key #'parameter-symbol) nil (list rule)))
	      (sofar (list (maknot (head rule)))))
	     ((null ls) result)
	   (when (not (member (relation (car ls)) ignorepreds :key #'parameter-symbol))
	     (push (list* '<= (maknot (car ls)) (revappend sofar (cdr ls))) result))
	   (push (car ls) sofar)))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
