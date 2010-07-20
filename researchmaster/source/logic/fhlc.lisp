(defvar *tree* nil 
  "Discrimination tree that holds mapping from atoms to propositions.")

#|
(defmethod fhlc (th lang preserve &key (query nil) (dca t) (una t) (univ nil))
  "(FHLC TH LANG PRESERVE QUERY DCA UNA UNIV)
   Compiles the theory TH into language LANG while preserving either
   validity ('valid), or (un)satisfiability ('unsat).
   Suppose result of FHLC is TH' and QUERY'.
   In the case of 'valid, TH |= query iff TH' |= query'.
   In the case of 'unsat, TH U {query} is (un)sat iff TH' U {query'} is (un)sat.
   Additional options exist for equality.  If no query is specified,
   FHLC preserves validity/satisfiability of the theory.
   Also, the UNIV option provides a way to set a lower bound
   on the elements of the universe.")
|#

;;;;;;;;;;;;;;;;;;;;;;;;;;;; FOL ;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmethod fhlc (th (lang (eql 'fol)) preserve 
		 &key (query nil) (dca t) (una t) (univ nil) 
		 (reflex nil) (sym nil) (trans nil) (subst nil)
		 (options nil))
  (declare (ignore preserve))
  (let (preds data)
    ; convert extensional facts to biconditionals
    (setq preds (union (cdr (assoc 'extensions options))
		       (factfinds '?x '(extension ?x) th)))
    (multiple-value-setq (data th) 
      (split #'(lambda (x) (and (datap x) (member (relation x) preds))) 
		    (contents th)))
    (setq th (nconc (predicate-completion data) 
		    (remove-if #'(lambda (x) (eq (relation x) 'extension)) th)))
    (setq th (nconc (equality-axioms (if query (cons query th) th) 
				     :reflex reflex
				     :sym sym
				     :trans trans
				     :subst subst
				     :unique-names una 
				     :dca dca
				     :univ univ)
		    th))
    (values th query)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;; Ground Clauses ;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmethod fhlc (th (lang (eql 'groundclauses)) preserve
		 &key (query nil) (dca nil) (una nil) (univ nil)
		 (reflex nil) (sym nil) (trans nil) (subst nil)
		 (options nil))
  (declare (ignore options))
  (let (newth objs)
    ; convert to FOL (adding appropriate equality)
    (multiple-value-setq (th query) 
      (fhlc th 'fol preserve :query query 
	    :dca dca :una una :univ univ 
	    :reflex reflex :sym sym :trans trans :subst subst))
    ; apply formula renaming
    (setq th (formulae-renaming th preserve))
    (when query
      (multiple-value-setq (query newth) (formulae-renaming (list query) preserve))
      (setq th (nconc newth th)))

    ; ground each of the sentences and the query and convert to clausal form
    (setq objs (union univ (objs (maksand (cons query th)))))
    (dolist (p th)
      (setq newth (nconc newth (clauses (maksand (ground-herbrand p objs nil))))))
    (when query (setq query (maksand (clauses (maksand (ground-herbrand query objs nil))))))
    (values newth query)))

(defun formulae-renaming (th preserve)
  (let (newth newq others)
    (dolist (q th newth)
      (cond ((base-defn q) (setq newth (cons q newth)))
	    (t
	     (multiple-value-setq (newq others) (formula-renaming-small q preserve))
	     (setq newth (nconc (cons newq others) newth)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;; Propositional Logic ;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmethod fhlc (th (lang (eql 'prop)) preserve
		 &key (query nil) (dca nil) (una t) (univ nil) 
		 (reflex t) (sym nil) (trans nil) (subst nil)
		 (options nil))
  ; notice that sym, trans, dca are nil as defaults--when grounded they are always 
  ;   superfluous in FHL: dca is a tautology, sym and trans are subsumed by una/reflex.
  (let (*tree*)
   (multiple-value-setq (th query) 
      (fhlc th 'groundclauses preserve :query query 
	    :dca dca :una una :univ univ :reflex reflex :sym sym :trans trans 
	    :subst subst :options options))
    (setq th (ground2prop th))
    (when query (setq query (ground2prop (list query) *tree*)))
    (values th query)))

(defun ground2prop (th &optional (*tree* nil))
  "(GROUND2PROP FOL) takes a set of ground sentences and turns them
   into a set of propositional sentences.  Assumes = has already
   been dealt with appropriately."
  ; create index for quickly associating a ground atom with a 
  ; propositional constant
  (let (newth)
    ; this is nice so that we can use multiple calls to the function
    ;   using the same discrimination tree.
    ; need tree if propositional constants are opaque.  Leave for now.
    (unless *tree* 
      (setq *tree* (make-dtree))
      (dt-prep (get-vocabulary (maksand (contents th))) *tree*))
  
    ; recursively walk each formula, making a copy and replacing
    ;  each atom with a number
    (setq newth nil)
    (dolist (p (contents th))
      (push (ground2prop-aux p *tree*) newth))
    newth))

(defun ground2prop-aux (p tree)
  (cond ((atom p) p)
	((find (car p) '(<= => and or not <=>))
	 (cons (car p) 
	       (mapcar #'(lambda (x) (ground2prop-aux x tree)) 
		       (cdr p))))
	((find (car p) '(forall exists))
	 (list* (first p) 
		(second p) 
		(mapcar #'(lambda (x) (ground2prop-aux x tree)) 
			(cddr p))))
	(t 
	 (let (val)
	   (setq val (dt-lookup p tree))
	   (cond (val val)
		 (t (setq val (tostructuredsymbol p))
		    (dt-insert p val tree)
		    val))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;; Biconds theory ;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; write as much of the theory as a nonrecursive set of biconditionals
;   as possible.  Returns theory as dotted pair: car is complete, cdr is remaining
;   If theory is bilevel, just returns the given partitioning.
(defmethod fhlc (th (lang (eql 'compinc)) preserve
		 &key (query nil) (dca nil) (una nil) (univ nil) 
		 (reflex nil) (sym nil) (trans nil) (subst nil)
		 (options nil))  
  (let (iffs remaining)
    (cond ((typep th 'bileveltheory) 
	   (values (cons (extensions th) (intensions th)) query))
	  (t
	   (setq th
		 (fhlc th 'fol preserve :dca dca :una una :univ (union univ (objs query))
		       :reflex reflex :sym sym :trans trans :subst subst 
		       :options options))
	   (multiple-value-setq (iffs remaining) (ps2iffmax (contents th)))
	   (values (cons iffs remaining) query)))))
    
;;;;;;;;;;;;;;;;;;;;;;;;;;;; Complete theory ;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; (fhlc '((=> (color ?x ?c) (adj ?x ?y) (not (color ?y ?c))) (=> (color ?x ?y) (and (region ?x) (hue ?y))) (<=> (hue ?x) (or (= ?x red)) (or (= ?x blue))) (<=> (region ?x) (or (= ?x a) (= ?x b) (= ?x c))) (<=> (adj ?x ?y) (or (and (= ?x a) (= ?y b)) (and (= ?x b) (= ?y c))))) 'complete 'unsat :query '(and (color a ?x) (color b ?y) (color c ?z)) :options '((transformation . topdown )))

; (fhlc v 'complete 'unsat :query q :options (list '(query-sensitive . t) (cons 'predicates (list (make-parameter :symbol 'overlap :arity 4 :type 'relation) (make-parameter :symbol 'hourspersession :arity 2 :type 'relation) (make-parameter :symbol 'whole :arity 1 :type 'relation) (make-parameter :symbol 'hour :arity 1 :type 'relation) (make-parameter :symbol 'halfhour :arity 1 :type 'relation) (make-parameter :symbol 'time :arity 1 :type 'relation) (make-parameter :symbol 'faculty :arity 1 :type 'relation) (make-parameter :symbol 'classname :arity 1 :type 'relation) (make-parameter :symbol 'room :arity 1 :type 'relation) (make-parameter :symbol 'classreqs :arity 5 :type 'relation) (make-parameter :symbol 'roomequip :arity 3 :type 'relation)))))

; Here, PRESERVE means something slightly different than in other cases.
;  Instead of UNSAT causing the query to be included with the theory when there is one,
;  UNSAT causes the query submitted to be considered a satisfiability query.
;  VALID causes the query submitted to be considered an entailment query.
;  Should probably make the two ideas orthogonal.  
(defmethod fhlc (th (lang (eql 'complete)) preserve
		 &key (query nil) (dca nil) (una nil) (univ nil) 
		 (reflex nil) (sym nil) (trans nil) (subst nil)
		 (options nil))  
  (assert (or query (eq (cdr (assoc 'transformation options)) 'allpreds))
	  nil "FHLC to COMPLETE: if query is NIL then TRANSFORMATION must be ALLPREDS.")
  (let (iffs remaining relns tmp (*oneofs* nil))
    ; partition the theory into the complete and incomplete portions
    (multiple-value-setq (th query)
	  (fhlc th 'compinc preserve :query query 
		:dca dca :una una :univ (union univ (objs query))
		:reflex reflex :sym sym :trans trans :subst subst :options options))
    (setq iffs (car th) remaining (cdr th))
    (setq relns (cdr (assoc 'predicates options)))
    (assert (every #'parameter-p relns) nil 
	    "Predicates supplied must be parameter structs.")
    (setq relns (union relns (complete-preds (maksand iffs)) :test #'equalp))

    ; construct the query in terms of poss for bottomup and topdown
    (when query
      (if (eq preserve 'valid)
	  (setq query (maknot (makposs (maknot query) (freevars query))))
	  (setq query (makposs query (freevars query))))
      (setq query (simplify-poss query remaining relns)))

    (case (cdr (assoc 'transformation options))
      ; use resolution closure to construct all queries simultaneously
      (allpreds
       (assert (eq (get-theory-type remaining) 'quantifier-free)
	       nil "FHLC to COMPLETE when TRANSFORMATION is ALLQUERIES requires
               incomplete portion of theory to be quantifier-free.")
       ; create a valid query for each positive and negative predicate.
       ;(unless (cdr (assoc 'fullyfactored options))
	; (format t "Warning: Non fullyfactored ALLQUERIES is experimental and produces implications instead of biconditionals.~%"))
       (setq th (simultaneous-er-all-predicates remaining 
						relns
						(cdr (assoc 'fullyfactored options))
						(cdr (assoc 'transform options))))
       (nconc th iffs))
      
      ; use resolution closure to construct each poss query individually
      (bottomup 
       (setq th (poss-definitions (find-posses query) remaining relns))
       (setq th (nconc (mapcar #'(lambda (x) (oneof-definition (first x) (second x))) 
			       *oneofs*) th))
       (setq th (nconc th iffs))
       (setq th (mapcar #'rewrite-quoted-escaping-vars th))
       (setq query (rewrite-quoted-escaping-vars query))
       (values th query))

      ; use residues to construct each poss query individually
      (topdown 
       (setq th nil)
       (dolist (p (find-posses query))
	 (setq tmp (rewrite-query (maknot (poss-sent p)) 
				  remaining 
				  (mapcar #'parameter-symbol relns)))
	 (push `(<=> ,p ,(maknot tmp)) th))
       (setq th (nconc th iffs))
       (setq th (mapcar #'rewrite-quoted-escaping-vars th))
       (setq query (rewrite-quoted-escaping-vars query))
       (values th query)))))
 
;;;;;;;;;;;;;;;;;;;;;;;;;;;; Datalog ;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; (fhlc '((=> (color ?x ?c) (adj ?x ?y) (not (color ?y ?c))) (=> (color ?x ?y) (and (region ?x) (hue ?y))) (<=> (hue ?x) (or (= ?x red)) (or (= ?x blue))) (<=> (region ?x) (or (= ?x a) (= ?x b) (= ?x c))) (<=> (adj ?x ?y) (or (and (= ?x a) (= ?y b)) (and (= ?x b) (= ?y c))))) 'datalog 'unsat :query '(and (color a ?x) (color b ?y) (color c ?z)) :options '((query-sensitive .t )))
(defmethod fhlc (th (lang (eql 'datalog)) preserve
		 &key (query nil) (dca nil) (una t) (univ nil) 
		 (reflex t) (sym nil) (trans nil) (subst nil)
		 (options nil))  
  (let (objs support)
    (setq objs (objs (maksand (contents th))))
    (multiple-value-setq (th query)
      (fhlc th 'complete preserve :query query :dca dca :una una :univ univ 
	    :reflex reflex :sym sym :trans trans :subst subst :options options))	  
    ; convert iffs to datalog
    (multiple-value-setq (query support) (query2datalog query))
    (setq th (iffth2datalog th (union univ objs)))
    (values (nconc support th) query)))
 

;;;;;;;;;;;;;;;;;;;;;;;;;;;; CSP ;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; (fhlc '((=> (color ?x ?c) (adj ?x ?y) (not (color ?y ?c))) (=> (color ?x ?y) (and (region ?x) (hue ?y))) (<=> (hue ?x) (or (= ?x red)) (or (= ?x blue))) (<=> (region ?x) (or (= ?x a) (= ?x b) (= ?x c))) (<=> (adj ?x ?y) (or (and (= ?x a) (= ?y b)) (and (= ?x b) (= ?y c))))) 'csp 'unsat :query '(and (color a ?x) (color b ?y) (color c ?z)) :options '((query-sensitive .t )))
; (define-theory 'sched "" (read-file "/Users/thinrich/Apps/working/class.kif"))
;(fhlc 'sched 'csp 'unsat :query '(and (class 10200 ?10200days ?10200time ?10200room ?10200faculty) (class 15400 ?15400days ?15400time ?15400room ?15400faculty)) :options '((query-sensitive . t)))
(defmethod fhlc (th (lang (eql 'csp)) preserve
		 &key (query nil) (dca nil) (una nil) (univ nil) 
		 (reflex nil) (sym nil) (trans nil) (subst nil)
		 (options nil))
  (format t "Note: very experimental--may not be treating = correctly...~%")
  (let (preds objs q)
    (multiple-value-setq (th query)
      (fhlc th 'complete preserve :query query :dca dca :una una :univ univ
	    :reflex reflex :sym sym :trans trans :subst subst :options options))
    ;(print query)
    ; find the predicates used to define the query
    (setq objs (objs (maksand (contents th))))
    (setq q (find-posses query))
    ;(print q)
    (cond ((cdr q) ; more than one poss in query
	   (setq preds (list q)))
	  (t
	   (setq q (find (car q) th :key #'head :test #'equal))
	   ;(print q)
	   (setq q (third q))
	   (setq preds (preds q))))
    ;(print th)
    ;(print preds)
    ;(break)

    ; convert the theory to datalog and materialize those predicates
    (setq th (iffth2datalog th (union univ objs)))
    (setq th (materialize th preds))
    (values th q)))

(defun materialize (th preds)
  ; ensure indexing
  (setq th (define-ddbtheory '__tlh "" (contents th))) 
  (mapcan #'(lambda (x) (let ((p (cons (parameter-symbol x) 
				       (maptimes #'newindvar (parameter-arity x)))))
			  (viewfinds p p th)))
	  preds))
    


  
