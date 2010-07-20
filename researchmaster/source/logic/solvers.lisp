;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; solvers.lisp
;;    top-level automated reasoning routines for answering entailment/satisfiability queries
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; global variables for controlling which implementation of basic algs we use

(eval-when (compile load eval)
  (proclaim '(special *resolution-closure* *subsumption-closure*)))

(defparameter *resolution-closure* 
  #'(lambda (clauses limit &optional (ignorelist nil)) 
      (declare (ignore ignorelist))
      (resolution-closure-snark clauses limit)))

(defparameter *subsumption-closure* #'subsumption-elimination)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;; Leveraging complete subtheories (Extensional Reasoning) ;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar *trace-reform2iffs* nil "whether to trace algorithm for reformulating sentences to biconditionals")

(defun fhl-fullfinds (vars lit theory)
  "(FHL-FINDP LIT THEORY) is a version of findp for finite herbrand logic 
   implemented by grounding out before using fullfinds."
  ; ground theory using object constants in the theory and then convert to clausal form
  ;(when (not (listp vars)) (setq vars (list vars)))
  (let ((newps)
        (*ancestry* t)
        (newth (make-instance 'theory)))

    ; close all the sentences
    (setq newps (mapcar #'(lambda (x) `(forall ,(freevars x) ,x)) (contents theory)))
    (setq newps (inseado-ground (maksand newps) 
                                (compute-dca theory)))
    ; use ME to check if LIT is entailed by THEORY
    (pcontents (define-theory newth "" newps))
    (fullfinds vars lit newth)))

(defun fhlfindp (p th)
  "(FHLFINDP P TH) ERFINDP uses extensional reasoning when appropriate to determine whether 
   the satisfiable FHL theory TH entails the closed sentence P."
  (assert (null (freevars p)) nil "FHLfindp takes a closed sentence.")
  (let (maxiffs relns remaining)
    (multiple-value-setq (maxiffs remaining) (ps2iffmax (contents th)))
    (setq relns (preds (maksand maxiffs)))
    (cond ((sentence-in-vocabp p relns) 
           (ifffindp p maxiffs))
          ((and (prenexp p) (rewriting-inexpensivep p remaining relns))
           (rewritefindp p th))
          ((and (theory-amenable-to-poss remaining) (query-amenable-to-poss p remaining))
           (possfindp p th))
          (t
           (setq th (define-theory (make-instance 'metheory) "" (contrapositives (maksand (contents th)))))
           (fullprovep p (add-equality th :contras t :unique-names t :dca t))))))

;;;;;;; Complete theories
(defun completefindp (p th &optional (ps2iff #'ps2iff))
  "(COMPLETEFINDP P TH) takes a closed sentence P and a complete theory TH which can be turned into
   a set of nonrecursive biconditionals using PS2IFF. Answers the entailment query by converting the 
   theory to a database and a set of view definitions and using datalog routines to answer the query."
  (let (iffs)
    (setq iffs (funcall ps2iff (contents th)))
    (ifffindp p iffs)))

(defun ifffindp (p th &optional (univ nil) (dbengine #'viewfindx))
  "(IFFFINDP P TH) takes a closed sentence P and a list of nonrecursive biconditionals.
   Answers the entailment query by converting the IFFS to a database and a set of view definitions
   and using datalog routines to answer the query.  Assumes that biconditionals are oriented:
   if one of them is p(xbar) <=> q(xbar) then the biconditional defines p in terms of q."
  (ifffindx t p th univ dbengine))

(defun ifffindx (thing p th &optional (univ nil) (dbengine #'viewfindx))
  "(IFFFINDP P TH) takes a sentence P and a list of nonrecursive biconditionals.
   Answers the entailment query by converting the IFFS to a database and a set of view definitions
   and using datalog routines to answer the query.  Assumes that biconditionals are oriented:
   if one of them is p(xbar) <=> q(xbar) then the biconditional defines p in terms of q."
  (let (query)
    (multiple-value-setq (*prologentry* query) (query2datalog p))
    (setq *prologtheory* (define-ddbtheory 'prologtheory "" (append query (iffth2datalog th univ))))
    (let ((traceexpressions *tracedatalogsearch*))
      (funcall dbengine thing *prologentry* *prologtheory*))))

(defun ifffinds (thing p th &optional (univ nil) (dbengine #'viewfinds))
  (ifffindx thing p th univ dbengine))

(defun iffth2datalog (th &optional (univ nil))
  (let (extras)
    (unless univ
      (setq univ (objs (maksand (contents th)))))
    (assert univ nil "IFFTH2DATALOG requires there be at least one object in the universe.")
    (setq extras (cons '(= ?x ?x) (mapcar #'(lambda (x) `(univ ,x)) univ)))
    (append extras (iffs2datalog (contents th)))))


;;;;;;; Incomplete theories

;; Rewriting

(defun rewritefindp (p th)
  "(REWRITEFINDP P TH) finds the maximal complete subtheory of TH.  Rewrite P 
   in terms of that complete subtheory and hand off the new query and the iff 
   definitions to ifffindp."
  (let (maxiffs relns remaining)
    (multiple-value-setq (maxiffs remaining) (ps2iffmax (contents th)))
    (setq relns (mapcar #'parameter-symbol (remove-if-not #'isrelation (get-vocabulary (maksand maxiffs)))))
    (setq p (rewrite-query p remaining relns))
    (ifffindp p maxiffs)))

#|
  (let (maxiffs relns remaining)
    (multiple-value-setq (maxiffs remaining) (ps2iffmax (contents th)))
    (setq relns (mapcar #'parameter-symbol (remove-if-not #'isrelation (get-vocabulary (maksand maxiffs)))))
    (cond ((and (prenexp p) 
                ;(literal-disjunctionp (strip-quantifiers p))
                (rewriting-inexpensivep p remaining relns))
           (setq p (rewrite-query p remaining relns))
           (completefindp p maxiffs))
          (t
           (setq th (define-theory (make-instance 'metheory) "" (contrapositives (maksand (contents th)))))
           (fullprovep p (add-equality th :contras t :unique-names t :dca t))))))
|#

(defun rewriting-inexpensivep (p th relns)
  (declare (ignore p th relns))
  ; if relns is empty, then the query would need to be rewritten in terms of =
  t)

(defun complete-preds (p)
  (let ((e (make-parameter :symbol '= :arity 2 :type 'relation)))
    (cond ((not p) (list e))
          (t (adjoin e (preds p) :test #'equalp)))))

(defun possfindp (p th)
  "(POSSFINDP P TH) finds the maximal complete subtheory of TH.  Transforms the query
   into a poss query and after simplification, constructs the definitions for the 
   resulting poss predicates.  Hands off the complete theory and the new query to ifffindp." 
  (let (univ)
    (multiple-value-setq (*logicquery* *logictheory* univ) (what2how p th))
    (ifffindp *logicquery* *logictheory* univ)))

(defun what2how (p th)
  "(WHAT2HOW P TH) takes a classical entailment query th |= p and transforms it into a nonrecursive 
   datalog with negation query (as multiple values)."
  (let (maxiffs relns remaining posses oneofs (*oneofs* nil) univ 
		(*resolution-closure* #'resolution-closure-snark))
    (setq univ (objs (maksand (contents th))))
    (multiple-value-setq (maxiffs remaining) (ps2iffmax (contents th)))
    (setq relns (complete-preds (maksand maxiffs)))
    (setq p (simplify-poss (maknot (makposs (maknot p) nil)) remaining relns))
    (setq posses (find-posses p))
    (setq remaining (poss-definitions posses remaining relns))
    (setq remaining (mapcar #'rewrite-quoted-escaping-vars remaining))
    (setq p (rewrite-quoted-escaping-vars p))
    (setq oneofs (mapcar #'(lambda (x) (oneof-definition (first x) (second x))) *oneofs*))
    (values p (nconc maxiffs remaining oneofs) univ)))

#| Test
(what2how '(forall (?x ?y) (=> (p ?x) (q ?y) (w ?x ?y))) 
          '((<=> (w ?x ?y) (or (and (= ?x a) (= ?y c)) (and (= ?x a) (= ?y d)) (and (= ?x b) (= ?y c)) (and (= ?x b) (= ?y d)))) 
            (or (p a) (p b)) 
            (not (q a)) 
            (not (q b)) 
            (or (q c) (q d)) 
            (not (p c)) 
            (not (p d))))  

|#
#|
  (let (maxiffs relns remaining objs)    
    (setq objs (remove-if-not #'isobject (get-vocabulary (maksand (contents th)))))
    (multiple-value-setq (maxiffs remaining) (ps2iffmax (contents th)))
    (setq maxiffs (cons (create-bicond 'object (mapcar #'(lambda (x) (list (parameter-symbol x))) objs)) maxiffs))
    (setq relns (remove-if-not #'isrelation (get-vocabulary (maksand maxiffs))))
    (cond ((and (theory-amenable-to-bcd remaining) 
                (query-amenable-to-bcd p th))
           (setq p (rewrite-quoted-escaping-vars (simplify-poss (maknot (makposs (maknot (quantify p)) nil)) remaining relns)))
           (setq remaining (mapcan #'poss-ground-disjunctions (partition-by-independence remaining)))
           (ifffindp p (nconc maxiffs remaining)))
          (t
           (setq th (define-theory (make-instance 'metheory) "" (contrapositives (maksand (contents th)))))
           (fullprovep p (add-equality th :contras t :unique-names t :dca t))))))
|#

(defun theory-amenable-to-poss (th)
  (every #'(lambda (x) (and (groundp x) (or (literalp x) (literal-disjunctionp x)))) (contents th)))

(defun query-amenable-to-poss (p th)
  (declare (ignore th))
  (universal-prenexp p))

;;;;;; Tracing

(defun tde (&rest l)
  (cond ((null l) *tracedatalogsearch*)
	(t (mapc 'tdexp l)
           t)))
(defun ude () (setq *tracedatalogsearch* nil))
(defun tdexp (x)
  (if (not (memp x *tracedatalogsearch* 'samep))
      (setq *tracedatalogsearch* (cons x *tracedatalogsearch*))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;; Running Epilog on KIF theories on disk ;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun kif2epilogl (infile outfile theory-name 
		    &key (doc "") (goalify nil) (contrapositives nil) 
		    (sort nil))
  "(KIF2EPILOGL ...) calls kif2epilog then loads the theory created."
  (kif2epilog infile outfile theory-name :doc doc :goalify goalify 
	      :contrapositives contrapositives :sort sort)
  (load outfile))

(defun kif2epilog (infile outfile theory-name
		   &key (doc "") (goalify nil) (contrapositives nil) (sort nil))
  "(KIF2EPILOG INFILE OUTFILE THEORY-NAME DOC)
   KIF2EPILOG reads in the sentences in infile, asks kif2epilog-core to
   convert the kif into rule form, and outputs the result to outfile wrapped
   in a deftheory with specified theory-name and documentation doc."
    ;(with-open-file (handle outfile :direction :output
     ;                        :if-exists :supersede
     ;                        :if-does-not-exist :create)

      (save-theory theory-name doc 
                   (kif2epilog-core (read-file infile) 'tlhgoal goalify 
				    :contrapositives contrapositives 
				    :sort sort) 
                   outfile))

(defun kif2epilog-core (kifsents goal goalify 
			&key (contrapositives nil) (sort nil))
  "(KIF2EPILOG-CORE KIFSENTS GOAL GOALIFY) 
   It translates the kif sentences KIFSENTS into CNF rule form for Epilog 
   and if GOALIFY is true, generates a goal of the last sentence, naming
   it the contents of GOAL."
  (let ((axioms 
         (if goalify
           ; use goal as the proof goal
           (let* ((goal (brfss `(<= ,goal ,(maknot (car (last kifsents)))))))
             (if contrapositives
               (append (contrapositives (maksand goal)) 
		       (contrapositives (maksand (butlast kifsents))))
               (append goal (brfss (maksand (butlast kifsents))))))
           
           ; else just the contrapositives of all the sentences
           (if contrapositives
             (contrapositives (maksand kifsents))
             (brfss (maksand kifsents))))))
    (if sort
      (sort axioms #'string-lessp :key #'head-literal)
      axioms)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;; Find Wrappers ;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar *print-progress* nil 
  "Print out a count of how many solutions findkx has found, every 1000 
   solutions." )
(defvar *print-time* nil
  "Print out number of seconds since start of solution finding whenever
   print-progress is turned on.")
(defvar *print-mod* 1000
  "Print out progress every *print-mod* solutions.")

(defun findk-params ()
  "(FINDK-PARAMS) prints out the parameters used for findkx."
   (format t "*print-progress*: ~30T~A~%" *print-progress*)
   (format t "*print-time*: ~30T~A~%" *print-time*)
   (format t "*print-mod*: ~30T~A~%" *print-mod*))

(defun findkx (x p *theory* k)
 "(FINDKX X P *THEORY* k)
  FINDX takes as argument a term, a sentence, a theory, and a constant.  It 
  tries to prove
  the specified sentence from the specified theory and its included theories
  using the model elimination.  The search is done in iterative deepening
  fashion, controlled by the variables *START*, *INCREMENT*, and *DEPTH*.  If
  FINDX is able to prove the sentence, it returns a copy of the specified term
  with variables replaced by values obtained during the proof process.  If it
  fails to prove the sentence, the value is NIL.  It returns the first 
  k solutions."
 (let ((fx (findg x p *theory*)))
   (do ((i 0 (+ 1 i))
        (starttime (get-universal-time))
        (results nil)
        (sol (funcall fx) (funcall fx)))
       ((or (null sol) (= i k)) (nreverse results))
     (setq results (cons sol results))
     (when (and *print-progress* (= (mod i *print-mod*) 0))
       (when *print-time*
         (format t "Elapsed time: ~A seconds;   " 
		 (- (get-universal-time) starttime))) 
       (format t "Found ~A solutions~%" i)))))


(defun seekx (x p *theory* k)
 "(SEEKX X P *THEORY* k)
  SEEKX takes as argument a term, a sentence, a theory, and a constant. 
  It tries to prove
  the specified sentence from the specified theory and its included theories
  using the model elimination.  The search is done in iterative deepening
  fashion, controlled by the variables *START*, *INCREMENT*, and *DEPTH*.  SEEKX
  finds the first k solutions, but does not store any in memory.  It is useful
  only as a diagnostic when all solutions cannot be stored in memory."
 (let ((fx (findg x p *theory*)))
   (do ((i 0 (+ 1 i))
        (starttime (get-universal-time))
        (sol (funcall fx) (funcall fx)))
       ((or (null sol) (= i k)) t)
     ;(setq results (cons sol results))
     (when (and *print-progress* (= (mod i *print-mod*) 0))
       (when *print-time*
         (format t "Elapsed time: ~A seconds;   " 
		 (- (get-universal-time) starttime))) 
       (format t "Found ~A solutions~%" i)))))

;;; for handling cascade theories
;;; toplevel theory has global scope
(defun cascadeviewfindp (p th)
  (cascadeviewfindx t p th))

(defun cascadeviewfindx (x p th)
  (let (ans)
    (setq ans (viewfindx x p th))
    (if ans ans (if (child th) 
		    (cascadeviewfindx-aux x p (child th) (cons th (includees th))) 
		    nil))))

(defun cascadeviewfindx-aux (x p th includelist)
  (let (ans)
    (mapc #'(lambda (x) (includes th x)) includelist)
    (setq ans (viewfindx x p th))
    (decludes th)
    (if ans ans (if (child th) 
		    (cascadeviewfindx-aux x p (child th) includelist) 
		    nil))))

(defun cascadeviewfinds (x p th)
  (let (ans)
    (setq ans (viewfinds x p th))
    (if ans ans (if (child th) 
		    (cascadeviewfinds-aux x p (child th) (cons th (includees th))) 
		    nil))))

(defun cascadeviewfinds-aux (x p th includelist)
  (let (ans)
    (mapc #'(lambda (x) (includes th x)) includelist)
    (setq ans (viewfinds x p th))
    (decludes th)
    (if ans ans (if (child th) 
		    (cascadeviewfinds-aux x p (child th) includelist) 
		    nil))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;; Threaded Solving ;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun findx-time (time func thing p th)
  (let* ((start (get-universal-time))
         (id (process-run-function "findx-time" func thing p th)) ) 
    (process-wait "finish or time-out" #'process-finished start time id)
    (process-kill id)))


(defstruct findcall (func #'fullfindx) (thing t) (phi nil) (th nil))
(defvar *some-findx-finished* nil) ; whether any thread has finished
(defvar *some-findx-result* nil)   ; answer from first thread

(defun some-findx (findcalls &optional (timelimit -1))
  "(SOME-FINDX FINDCALLS) spawns threads to run each of the find calls."
  (let ((ids nil) (finishlock (make-lock)))
    (setq *some-findx-finished* nil *some-findx-result* nil)
    (do ((fs findcalls (cdr fs)))
        ((null fs))
      (setq ids (cons (process-run-function 
                       "some-findx thread"
                       #'(lambda (x)
                         (let ((myresult (funcall 
                                               (findcall-func x)
                                               (findcall-thing x)
                                               (findcall-phi x)
                                               (findcall-th x))))
                           ;(process-lock finishlock)
                           (with-lock-grabbed (finishlock)
                             (unless *some-findx-finished* 
                               (setq *some-findx-result* myresult 
				     *some-findx-finished* t)))
                             )) (car fs))
                      ids)) )
    (process-wait "some-findx check" #'check-some-findx ids timelimit)
    (mapcar #'process-kill ids)
    *some-findx-result*))

(defun check-some-findx (ids &optional (timelimit -1))
  "(CHECK-SOME-FINDX IDS) returns T when at least one of the processes IDS 
   has finished."
  (cond (*kill* 'killed)
        ((some #'(lambda (x) (not (ccl::process-active-p x))) ids)
         'done)
        ((and (> timelimit 0) 
              (some #'(lambda (x) (> (/ (print (process-total-run-time x)) 60)
                                     timelimit)) ids)) 
         'timeout)
        (t nil)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;; ESODatalog ;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;(systema '((=> (s ?x ?y) (t ?x)) (=> (t ?x) (q ?x ?y)) (=> (r ?x ?y) (p ?x ?y)) (=> (p ?x ?y) (q ?x ?y))) '(p t) '((r a b) (q a b) (q b a) (s a a) (q a a)))
; the above should produce ((T A) (P A B)).  Remove (q a a) in data, and get FALSE.
(defun systema (th unknowns data iffs)
  "(SYSTEMB TH) takes a set of sentences, which are assumed to be
    quantifier-free, where each sentence mentions at most one 
    unknown relation constant, and a list of UNKNOWNS that determines
    whether a given relation constant is unknown.  The known relations
    are contained in DATA.  The output
    is a relation for each unknown that satisfies TH and DATA."
  (let (clauses groups results tmp)
    (setq clauses (clausesets (maksand (contents th))))
    ; break into groups dealing with each unknown relation
    (setq groups (group-by clauses 
			   #'(lambda (x) 
			       (relation (find-if #'(lambda (y) 
						      (member (relation y) unknowns)) 
						  x)))))
    ; solve each group individually and union the results.
    (dolist (g groups results)
      (setq tmp (systema-group (cdr g) (car g) data iffs))
      (if (eq tmp 'false)
	  (return 'false)
	  (setq results (nconc tmp results))))))

;(systema-group '(((not (p ?x ?y)) (q ?x ?y)) ((not (r ?x ?y)) (p ?x ?y))) 'p '((r a b) (q a b) (q b a)))
(defun systema-group (th unknown data iffs)
  "(SYSTEMA-GROUP TH UNKNOWN DATA) workhorse of SYSTEMA.  Solves clauses TH for a given 
   unknown predicate UNKNOWN, where all the remaining predicates are defined in DATA.
   Assumes one occurrence of a UNKNOWN literal in each sentence in TH."
  (let (outer inner others constraints unknownatom)    
    ; first split off the constraints
    (setq constraints (split #'(lambda (x) (not (some #'(lambda (y) (eq unknown (relation y))) x))) th))
    (setq others (cdr constraints))
    (setq constraints (car constraints))

    ; similarize all of the literals so that they constrain the same unknown relation.
    (setq others (mapcar #'(lambda (x) (canon-unknown-clauseset x unknown)) others))
    (setq unknownatom (drop-not (find-in-theory unknown others :key #'relation)))

    ; now grab the outer constraints on the unknown (those where the unknown is negative)
    ;   and the inner constraints on the unknown (those where the unknown is positive)
    (setq outer (split #'(lambda (x) (some #'(lambda (y) (and (negative-literalp y)
							      (eq unknown (relation y)))) x))
		       others))
    (setq inner (cdr outer))
    (setq outer (car outer))

    ; remove all the unknown literals and turn back into clauses
    (setq inner (mapcar #'(lambda (x) (maksor (remove-if #'(lambda (y) (eq unknown (relation y))) x))) inner))
    (setq outer (mapcar #'(lambda (x) (maksor (remove-if #'(lambda (y) (eq unknown (relation y))) x))) outer))

    ; for the outer, we conjoin the sentences.
    ; for the inner, we disjoin the sentences.
    ; for the constraints, we conjoin the sentences.
    (setq outer (maksand outer))
    (setq inner (maksor inner))
    (setq constraints (maksand constraints))

    ; now check whether constraints are satisfiable and inner is a subset of outer using database 
    ;   techniques.  If so, return the materialization of the inner
    (setq data (append (contents iffs) (predicate-completion data)))
    (format t "~&Demonstrating the existence of an answer for unknown ~A..." unknown)
    (if (ifffindp (quantify `(and ,constraints (=> ,(maknot inner) ,outer))) data)
	(progn (format t "Success!~%Computing the solution.~%")
	       (ifffinds unknownatom (maknot inner) data))
	'false)))

(defun find-in-theory (item th &key (key #'identity) (test #'eq))
  (let (tmp)
    (dolist (p (contents th))
      (setq tmp (find item p :key key :test test))
      (when tmp (return tmp)))))

(defun canon-unknown-clauseset (p predicate)
  "(CANON-PREDICATE P PREDICATE) for the atom in which PREDICATE occurs in P, CANON-UNKNOWN
   replaces the atom (predicate t1 ... tn) with (predicate ?tlh1 ... ?tlhn), renaming other
   variables as appropriate and gensyms all the other variables."
  (let ((result nil) newlit bl tmp extra unk newunk)
    ; create the binding list and the extra set of distinction constraints
    ; bindings are necessary for variables, and distinction is necessary for constants
    (setq unk (find predicate p :key #'relation))   ; should only be 1
    (multiple-value-setq (bl extra newunk) (canon-bl unk))

    ; now walk over p.
    ; replace unk with newunk; otherwise apply the binding list if possible; otherwise, gensym
    (dolist (lit p (nconc (nreverse result) extra))
      (setq newlit nil)
      (cond ((eq lit unk) (push newunk result))
	    (t
	     (dolist (a (args lit))
	       (cond ((and (varp a) (setq tmp (assoc a bl)))
		      (push (cdr tmp) newlit))
		     ((varp a)
		      (push (cons a (newindvar)) bl)
		      (push (cdar bl) newlit))
		     (t (push a newlit))))
	     (setq newlit (cons (relation lit) (nreverse newlit)))
	     (push (maksamesign newlit lit) result))))))

(defun canon-bl (lit)
    ; create the binding list and the extra set of distinction constraints
    ; bindings are necessary for variables, and distinction is necessary for constants
    (do ((as (args lit) (cdr as))
	 (i 0 (1+ i)) (newvar) (bl) (extra) (newlit))
	((null as) (values bl extra (maksamesign (cons (relation lit) (nreverse newlit)) lit)))
      (setq newvar (makevariable (tosymbol (format nil "tlh~A" i))))
      (if (varp (car as)) 
	  (push (cons (car as) newvar) bl)
	  (push `(not (= ,(car as) ,newvar)) extra))
      (push newvar newlit)))


#|
(defun dbqueryp (p th)
  "(DBQUERYP P TH) takes a first-order sentence P and a datalog theory TH and
   determines whether or not TH |= P."
  ; use LloydTopor to translate ans <=> p to datalog.  
  ; Ask viewfindp for ans using TH and result of first step.
  (let (q)
    (setq q (cons 'ans (freevars p)))
    (ifffindp q (cons `(<=> ,q p) (contents th)))))
|#


(deftheory constraints
  (=> (class-reqs ?class ?hoursperweek ?daysperweek ?proj ?wetlab)
      (and (onetoten ?hoursperweek) (onetofive ?daysperweek) (yn ?proj) (yn ?wetlab)))
  (=> (class ?class ?days ?time ?room ?faculty) 
      (and (classname ?class) (days ?days) (time ?time) (room ?room) (faculty ?faculty)))
  (=> (class-reqs 10200 ?h ?d ?p ?w) (or (= ?h 2) (= ?h 3)))
  (=> (class ?class ?days ?time ?room ?faculty)
      (class-reqs ?class ?hoursperweek ?daysperweek ?proj ?wetlab)
      (validdays ?daysperweek ?days))
  (=> (class ?class ?days ?time ?room ?faculty)
      (hourpersession ?class ?h)
      (whole ?h)
      (hour ?time))
  
  (=> (class ?class ?days ?time ?room ?faculty)
      (hourpersession ?class ?h)
      (not (whole ?h))
      (halfhour ?time))
  
  (=> (room-equip ?room ?proj ?wetlab)
      (and (room ?room) (yn ?proj) (yn ?wetlab)))
  
  (=> (class ?class ?days ?time ?room ?faculty)
      (class-reqs ?class ?hoursperweek ?daysperweek ?proj ?wetlab)
      (room-equip ?room ?proj ?wetlab))
  
)

(deftheory defs 
  (<=> (time ?x) (or (hour ?x) (halfhour ?x)))
  (<=> (hourspersession ?class ?h)
       (exists (?hoursperweek ?daysperweek ?proj ?wetlab)
	       (and (class-reqs ?class ?hoursperweek ?daysperweek ?proj ?wetlab)
		    (div-hwdw ?hoursperweek ?daysperweek ?h))))
  (<=> (whole ?x) (not (= ?x 1point5)))
)

(deftheory data

(ROOM-EQUIP RY276 Y N)
(ROOM-EQUIP RY277 Y N)
(ROOM-EQUIP RY251 Y N)
(ROOM-EQUIP RY152 Y N)
(ROOM-EQUIP BLS103 N Y)

(DIV-HWDW 5 5 1)
(DIV-HWDW 4 4 1)
(DIV-HWDW 3 2 1POINT5)
(DIV-HWDW 3 3 1)
(DIV-HWDW 2 2 1)
(DIV-HWDW 5 1 5)
(DIV-HWDW 4 1 4)
(DIV-HWDW 3 1 3)
(DIV-HWDW 2 1 2)
(DIV-HWDW 1 1 1)

(VALIDDAYS 4 MTWR)
(VALIDDAYS 3 MWF)
(VALIDDAYS 2 TR)
(VALIDDAYS 1 M)
(VALIDDAYS 1 T)
(VALIDDAYS 1 W)
(VALIDDAYS 1 R)
(VALIDDAYS 1 F)

(ONETOTEN 1)
(ONETOTEN 2)
(ONETOTEN 3)
(ONETOTEN 4)
(ONETOTEN 5)
(ONETOTEN 6)
(ONETOTEN 7)
(ONETOTEN 8)
(ONETOTEN 9)
(ONETOTEN 10)

(ONETOFIVE 1)
(ONETOFIVE 2)
(ONETOFIVE 3)
(ONETOFIVE 4)
(ONETOFIVE 5)

(yn y)
(yn n)

(class-reqs 10200 3 3 n n)
(class-reqs 15400 5 5 y n)
(class-reqs 22200 1 1 y n)

(CLASSNAME 10200)
(CLASSNAME 15400)
(CLASSNAME 22200)
(CLASSNAME 23500)
(CLASSNAME 23800)
(CLASSNAME 25030)
(CLASSNAME 25040)
(CLASSNAME 27400)
(CLASSNAME 27600)
(CLASSNAME 28100)
(CLASSNAME 32001)
(CLASSNAME 32200)
(CLASSNAME 34500)
(CLASSNAME 34900)
(CLASSNAME 35040)
(CLASSNAME 35400)
(CLASSNAME 35900)
(CLASSNAME 37200)
(CLASSNAME 37600)
(CLASSNAME 38100)
(CLASSNAME 39600)

(DAYS MONDAY)
(DAYS TUESDAY)
(DAYS WEDNESDAY)
(DAYS THURSDAY)
(DAYS FRIDAY)
(DAYS MWF)
(DAYS TR)
(DAYS MTWR)

(HOUR 800)
(HOUR 900)
(HOUR 1000)
(HOUR 1100)
(HOUR 1200)
(HOUR 1300)
(HOUR 1400)
(HOUR 1500)
(HOUR 1600)
(HOUR 1700)
(HOUR 1800)

(HALFHOUR 800)
(HALFHOUR 930)
(HALFHOUR 1100)
(HALFHOUR 1230)
(HALFHOUR 1400)
(HALFHOUR 1530)
(HALFHOUR 1700)

(ROOM RY276)
(ROOM RY277)
(ROOM RY251)
(ROOM RY152)
(ROOM BLS103)

(FACULTY AMIT)
(FACULTY BABAI)
(FACULTY DUPONT)
(FACULTY FELZENSZWALB)
(FACULTY FINDLER)
(FACULTY FOSTER)
(FACULTY GOLDSMITH)
(FACULTY HINRICHS)
(FACULTY KLIVANS)
(FACULTY KURTZ)
(FACULTY LEVOW)
(FACULTY MACQUEEN)
(FACULTY MULMULEY)
(FACULTY NIYOGI)
(FACULTY ODONNELL)
(FACULTY REN)
(FACULTY REPPY)
(FACULTY ROGERS)
(FACULTY SCOTT)
(FACULTY SIMON)
(FACULTY SOARE)
(FACULTY STEVENS)

)



