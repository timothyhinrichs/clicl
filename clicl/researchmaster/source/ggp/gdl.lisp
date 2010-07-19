
(defmethod contents ((th list))
  th)
(defun mapcarnot (func seq) (remove-if #'not (mapcar func seq)))

(defstruct response (type 'error) msg vocab)
(defstruct responses local global)

(defun local-response-count (responses)
  (reduce #'+ (mapcar #'(lambda (x) (length (cdr x))) (responses-local responses))))

(defun global-response-count (responses)
  (length (responses-global responses)))

(defvar *responses* nil)
(defvar *ordering* nil)

(defun game-responses (th)
  "(GAME-RESPONSES TH) returns a responses struct which includes a set of global responses
    (warnings, errors) found after analyzing the game TH.  If no responses, the game is
    valid GDL, that is: (1) Datalog (2) stratified (3) appropriate use of next/true/etc. (4) finite/decidable."
  (let ((*responses* (make-responses :local (maptimes #'failure (length (contents th))) :global nil)) 
        (g (dependency-graph-with-negations (contents th)))
        (*real-ops* '(<= or not))
        (*propositions* t)
        (*object-constants* t))
    (datalog-responses (contents th) (mapunion #'get-vocabulary (contents th) :test #'eqparam))
    (stratified-responses g)
    (fsm-responses (contents th) g)
    (finitary-responses (contents th) g)
    (when *ordering* (ordering-responses (contents th)))
    (make-responses :global (responses-global *responses*)
                    :local (mapcar #'cons (contents th) (responses-local *responses*)))))  

(defun datalog-responses (rules vocab) 
  (let ((l) (g))
    (setq l (include-local-responses (mapcar #'datalog-failures rules)))
    (setq g (include-global-responses (inconsistent-vocabulary vocab)))
    (or l g)))

(defun stratified-responses (graph)
  (include-global-responses (cycles-through-negative-edges graph)))

(defun fsm-responses (rules g)
  (let ((l (include-local-responses (mapcar #'(lambda (x) (local-fsm-failures x)) rules)))
        (g (include-global-responses (global-fsm-failures g))))
    (or l g)))

(defun finitary-responses (rules g)
  (include-local-responses (mapcar #'(lambda (x) (finitary-failures x g)) rules)))

(defun ordering-responses (rules)
  (include-local-responses (mapcar #'(lambda (x) (ordering-failures x)) rules)))

(defun args (lit)
  (cond ((negative-literalp lit) (args (cadr lit)))
        ((listp lit) (cdr lit))
        (t nil)))

(defun datalog-failures (rule)
  "(DATALOG-FAILURES RULE) returns a list of reasons RULE is not Datalog, i.e. a safe implication with a positive head."
  (let ((neg) (pos) (responses nil) (ors) )
    (setq pos (remove-if-not #'(lambda (x) (and (positive-literalp x) (not (find (relation x) '(or distinct))))) (body rule)))  ; positive literals
    (setq ors (remove-if-not #'(lambda (x) (eq (relation x) 'or)) (body rule)))  ; the ors
    (setq neg (car (split-or #'(lambda (x) (or (negative-literalp x) (eq (relation x) 'distinct))) (body rule)))) ; negative literals, including those in ors

    ; ensure the head is positive
    (when (negative-literalp (head rule))
      (setq responses (list (make-response :type 'error :msg "the head of the rule should be positive"))))

    ; find unsafe variables in the head
    (dolist (v (vars (head rule)))
      (when (not (var-safep v pos ors))
        (setq responses (cons (make-response :type 'error :msg (format nil "unsafe variable in the head: ~A" v))
                              responses))))

    ; find unsafe variables in the body (in negative literals)
    (dolist (v (vars neg))
      (when (not (var-safep v pos ors))
        (setq responses (cons (make-response :type 'error :msg (format nil "unsafe variable in the body: ~A" v))
                              responses))))

    ; check if any of the boolean connectives appear where they are not supposed to
    (when (or (treememberp '<= (head rule))
              (treememberp '<= (body rule)))
      (setq responses (cons (make-response :type 'error :msg "<= appears somewhere it shouldn't")
                            responses)))

    ; check if or appears in the wrong place
    (when (treememberp 'or pos)
      (setq responses (cons (make-response :type 'error :msg "or should not appear as a function constant")
                            responses)))
    (when (treememberp 'or neg)
      (setq responses (cons (make-response :type 'error :msg "or should not appear as a function constant")
                            responses)))
    (when (some #'(lambda (x) (treememberp 'or (cdr x))) ors)
      (setq responses (cons (make-response :type 'error :msg "or should not appear as a function constant")
                            responses)))

    ; check if not is used incorrectly
    (when (not (not-validp rule))
      (setq responses (cons (make-response :type 'error :msg "not is not used appropriately")
                            responses)))
    responses))

(defun not-validp (rule)
  "(NOT-VALIDP RULE) returns T iff NOT is used appropriately within RULE."
  (cond ((atom rule) t)
        ((not (eq (car rule) '<=)) (not (treememberp 'not (strip-not rule))))
        (t
         (and (not-validp (head rule))
              (every #'(lambda (b) (if (eq (relation b) 'or)
                                     (every #'not-validp (cdr b))
                                     (not-validp b)))
                     (body rule))))))

(defun var-safep (var pos ors)
  "(VAR-SAFE VAR POSLITS ORS) returns T iff VAR either appears as a subterm of one of the atoms
   in POS or positively in all disjuncts of one of ORS."
  (or (some #'(lambda (poslit) (treememberp var (args poslit))) pos)
      (some #'(lambda (np) (every #'(lambda (lit) (and (positive-literalp lit) 
                                                       (treememberp var (args lit)))) (cdr np))) ors)))
  

(defun inconsistent-vocabulary (vocab)
  "(INCONSISTENT-VOCABULARY VOCAB) returns a list of responses for an inconsistent vocabulary, i.e.
   some constant is assigned multiple arities/types."
  (setq vocab (append (mapcar #'(lambda (x) (make-parameter :symbol x :type 'connective)) '(<= or not))
                      vocab))
  (let ((symbols nil) (res))

    ; compute the set of all symbols
    (dolist (v vocab)
      (setq symbols (adjoin (parameter-symbol v) symbols)))

    ; find all the entries for each symbol
    (setq res (mapcar #'(lambda (x) (cons x (remove-if-not #'(lambda (y) (eq (parameter-symbol y) x)) vocab))) symbols))

    ; for each symbol with more than one entry, add a response
    (mapcarnot #'(lambda (x) (if (cdr (cdr x)) 
                               (make-response :msg (format nil 
                                                           "the symbol ~A has multiple types and/or arities: ~A" 
                                                           (car x)
                                                           (mapcar #'(lambda (y) (cons (parameter-type y) (parameter-arity y)))
                                                                   (cdr x))))))
               res)))
                                                                   

(defun split-or (test list)
  "(SPLIT-OR TEST LIST) returns two lists as a dotted pair.  The first is the
    list of items where test returns t (descending into ors).  The second is the rest of the list."
  (do ((ls list (cdr ls))
       (pos nil)
       (neg nil)
       (tmp))
      ((null ls) (cons (nreverse pos) (nreverse neg)))
    (cond ((eq (relation (car ls)) 'or)
           (setq tmp (split test (cdar ls)))
           (setq pos (nconc pos (car tmp)))
           (setq neg (nconc neg (cdr tmp))))
          ((funcall test (car ls))
           (setq pos (cons (car ls) pos)))
          (t
           (setq neg (cons (car ls) neg))))))

(defun cycles-through-negative-edges (agraph)
  "(CYCLES-THROUGH-NEGATIVE-EDGES AGRAPH) takes an agraph as argument and returns a list of responses
   for every negative edge that occurs in a cycle, where an edge is negative if it is labeled 'not."
  ; for each negative edge x->y, check if there is a path from y to x.
  (let ((res))
    (mapcarnot #'(lambda (x) (if (setq res (agraph-find-path (cdr x) (car x) agraph))
                               (make-response :msg (format nil "axiom set is not stratified: there is a cycle through a negative edge")
                                              :vocab (cons (car x) res))
                               nil))
               (find-negative-edges agraph))))

(defun find-negative-edges (agraph)
  "(FIND-NEGATIVE-EDGES AGRAPH) returns an alist of node names.  The car is the
   from node and the cdr is the to node."
  (let ((result nil))
    (dolist (ns (agraph-adjacency-list agraph))  ; walk over all the nodes
      ; for each one, add every adjacent node that has 'not as its label to result
      (dolist (adjs (agraph-dest-nodes ns))
        (when (eq (agraph-edge-label adjs) 'not)
          (setq result (cons (cons (agraph-node-name (agraph-source-node ns))
                                   (agraph-node-name (agraph-source-node (agraph-edge-node adjs))))
                             result)))))
    result))

(defun local-fsm-failures (rule)
  "(LOCAL-FSM-FAILURES RULE) returns a list of reasons RULE violates the use of the GDL finite state
   machine ontology."
  (let ((responses nil))
    
    (when (and (eq (relation rule) 'role) (or (body rule) (vars rule)))
      (setq responses (cons (make-response :msg "role should be ground atomic") responses)))

    (when (eq (relation rule) 'true)
      (setq responses (cons (make-response :msg "true should not occur in the head") responses)))

    (when (eq (relation rule) 'does)
      (setq responses (cons (make-response :msg "does should not occur in the head") responses)))

    (when (member-bodyp 'next (body rule) :key #'relation)
      (setq responses (cons (make-response :msg "next should not occur in the body") responses)))

    (when (member-bodyp 'init (body rule) :key #'relation)
      (setq responses (cons (make-response :msg "init should not occur in the body") responses)))

    responses))

(defun member-bodyp (item body &key (test #'eq) (key #'identity))
  "(MEMBER-BODYP ITEM BODY TEST KEY) returns T iff ITEM is a member of some literal in
   BODY, where we recurse on OR."
  (some #'(lambda (x) (if (eq (relation x) 'or) 
                        (member-bodyp item (cdr x) :test test :key key)
                        (funcall test item (funcall key x))))
        body))

(defun global-fsm-failures (g)
  "(GLOBAL-FSM-FAILURES G) returns a list of all the reasons the rule set corresponding to 
   the dependency graph G as a whole violate the finite state machine ontology."
  (let ((responses nil))

    ; does reachable from goal/terminal/legal 
    (setq responses (nconc (mapcarnot #'(lambda (x) (let ((res (agraph-find-path 'does x g)))
                                                      (if res
                                                        (make-response :msg (format nil "DOES should not be reachable from ~A, but it is" x)
                                                                       :vocab res)
                                                        nil)))
                                      '(goal terminal legal))
                           responses))

    ; true/does/next/goal/terminal/legal reachable from init
    (setq responses (nconc (mapcarnot #'(lambda (x) (let ((res (agraph-find-path x 'init g)))
                                                      (if res
                                                        (make-response :msg (format nil "~A should not be reachable from INIT, but it is" x)
                                                                       :vocab res)
                                                        nil)))
                                      '(true does next goal terminal legal))
                           responses))
    responses))


(defun finitary-failures (rule g)
  "(FINITARY-FAILURES RULE G) returns a list of responses stating why RULE over the dependency
   graph G does not pass the finitary test."
  (let* ((h (relation rule))
         (nr (split #'(lambda (x) (and (positive-literalp x)
                                       (agraph-find-path h (relation x) g)
                                       (agraph-find-path (relation x) h g)))
                    (body rule))))
    (mapcan #'(lambda (x) (nrconjunct (head rule) x (cdr nr))) (car nr))))

(defun nrconjunct (head a nrconjs)
  "(NRCONJUNCT HEAD A LOWERCONJS) returns a list of problems with the atomic sentence
   A in the rule with head HEAD and nonrecursive conjuncts NRCONJS."
  ; return an error for each term that is neither exactly in the head or 
  (mapcarnot #'(lambda (x) (when (and (not (groundp x))
                                      (not (member x (cdr head)))
                                      (not (some  #'(lambda (y) (and (not (eq (relation y) 'distinct)) (member x (cdr y)))) nrconjs)))
                             (make-response :msg (format nil "~A occurs in the literal ~A, which is recursive with the head of the rule ~A, but does not occur exactly in the head or somewhere in the nonrecursive part of the body." x a head))))
             (cdr a)))


(defun treememberp (elem tree)
  "(TREEMEMBERP ELEM TREE) returns T iff ELEM is an atom in TREE."
  (cond ((atom tree) (eq elem tree))
        (t
         (some #'(lambda (x) (treememberp elem x)) tree))))

(defun dependency-graph-with-negations (rules)
  "(DEPENDENCY-GRAPH-WITH-NEGATIONS RULES) returns a graph with an edge from x to y if
   x is a relation constant in a literal in the body of a rule whose head is y.  That
   edge is labeled not if x is in a negative literal.  
  Ignores included theories."
  (let ((graph (make-agraph)))
    (mapc #'(lambda (rule)
              (mapc #'(lambda (bodylit)
                        (dependency-graph-with-negations-lit (relation rule) bodylit graph))
                    (body rule)))
          rules)
    graph))
(defun dependency-graph-with-negations-lit (headreln bodylit graph)
  "(DEPENDENCY-GRAPH-WITH-NEGATIONS-LIT HEADRELN BODYLIT GRAPH) adds all
   the dependencies in BODYLITS wrt the relation HEADRELN to GRAPH."
  (cond ((disjunctionp bodylit) 
         (mapc #'(lambda (x) (dependency-graph-with-negations-lit headreln x graph)) (cdr bodylit)))
        ((negative-literalp bodylit)
         (agraph-adjoin-edged (relation bodylit) headreln 'not graph))
        (t
         (agraph-adjoin-edged (relation bodylit) headreln 'dontcare graph))))

(defun rulesafep (rule alist)
  "(RULESAFEP RULE) returns T iff for every conjunct of RULE whose relation is 
   in a cycle (given by the association list ALIST) with the head of RULE is safe."
  (let ((candidaterelns (cdr (assoc (relation rule) alist))))
    (every #'(lambda (x) (conjunctsafep (head rule) x))
           (remove-if-not #'(lambda (x) (member (relation x) candidaterelns)) (body rule)))))

(defun conjunctsafep (head conj)
  "(CONJUNCTSAFEP HEAD CONJ) returns T iff no term in CONJ is a strict subterm
   of any term in head."
  (cond ((atom head) t)
        (t
         (not (some #'(lambda (conjterm) 
                        (some #'(lambda (headterm) 
                                  (strict-subtermp conjterm headterm))
                              (cdr head)))
                    (cdr (strip-not conj)))))))


(defun strip-not (l)
  "(STRIP-NOT L) removes a not if one exists from L."
  (cond ((atom l) l)
        ((eq (car l) 'not) (cadr l))
        (t l)))

(defun strict-subtermp (s term)
  "(STRICT-SUBTERMP S TERM) returns T iff S is a strict subterm of TERM."
  (and (not (equal s term)) (subtermp s term)))

(defun subtermp (s term)
  "(SUBTERMP S TERM) return T iff S is a subterm of TERM."
  (cond ((equal s term))
        ((atom term) (eq s term))
        (t
         (some #'(lambda (x) (subtermp s x)) (cdr term)))))

(defun ordering-failures (rule)
  "(ORDERING-FAILURES RULE) returns a list of reasons RULE is not ordered correctly, i.e. there is some variable
   in a negative literal in the body that precedes all the positive occurrences of that variable in the body."
  (cond ((atom rule) nil)
        ((and (eq (car rule) '<=) (null (cddr rule))) nil)
        ((not (eq (car rule) '<=)) nil)
        (t
         (do ((resp nil) vs
              (lits (cddr rule) (cdr lits)))
             ((null lits) (nreverse resp))
           (when (setq vs (vars-not-bound (car lits) (car lits) rule))
             (setq resp (cons (make-response :msg (format nil "variables ~A appear in a negative literal in ~A before they occur positively." vs (car lits)))
                              resp)))))))

(defun vars-not-bound (target conj rule)
  "(VARS-NOT-BOUND TARGET CONJ RULE) returns a list of variables in TARGET that are not bound positively before the conjunction
   CONJ in the rule RULE."
  (cond ((atom target) nil)
        ((eq (car target) 'or) (mapcan #'(lambda (x) (vars-not-bound x conj rule)) (cdr target)))
        ((positive-literalp target) nil)
        (t
         (mapcarnot #'(lambda (var) (if (var-boundp var conj rule) nil var)) (vars target)))))

(defun var-boundp (var conj rule)
  "(VAR-BOUNDP VAR CONJ RULE) returns T iff VAR occurs positively in rule before the conjunct CONJ in the body of RULE."
  (do ((cs (cddr rule) (cdr cs)))
      ((or (eq (car cs) conj) (null cs)) nil)
    (cond ((atom (car cs)))
          ((and (eq (car cs) 'or) (every #'(lambda (x) (and (positive-literalp x) (treememberp var x))) (cdar cs))) (return t))
          ((and (positive-literalp (car cs)) (treememberp var (car cs))) (return t)))))

;;;;;;;;;;;;;;;; checking vocabulary ;;;;;;;;;;;;;;;;

(defun check-vocabulary (p)
  "(CHECK-VOCABULARY P) returns a list of parameters representing all
    the function and relation constants in the sentence P."
  (cond ((atom p) (list (make-parameter :symbol p :arity 0 :type 'relation)))
        ((or (eq (car p) 'or)
             (eq (car p) 'and)
             (eq (car p) '<=>)
             (eq (car p) '=>)
             (eq (car p) '<=))
         (mapunion #'get-vocabulary (cdr p) :test #'eqparam))
        ((eq (car p) 'not)
         (get-vocabulary (cadr p)))
        ((or (eq (car p) 'forall)
             (eq (car p) 'exists))
         (get-vocabulary (caddr p)))
        (t
         (cons (make-parameter :symbol (car p) 
                               :arity (1- (length p)) 
                               :type 'relation)
               (mapunion #'get-functions (cdr p) :test #'eqparam)))))

#| 
(defun get-functions (term)
  "(GET-FUNCTIONS TERM) returns a list of parameters, all of type function,
   with all duplicates removed when TERM is a term."
  (cond ((varp term) nil)
        ((atom term) (list (make-parameter :symbol term :arity 0 :type 'function)))
        (t (append (list (make-parameter :symbol (car term) :arity (1- (length term)) :type 'function))
                   (mapunion #'get-functions (cdr term) :test #'eqparam)))))

(defun get-relations (p)
  "(GET-RELATIONS P) returns a list of parameter objects representing the relation
   constants in the sentence P."
  (delete-if-not #'isrelation (get-vocabulary p)))
|#


(defun include-local-responses (rs) 
  "(INCLUDE-LOCAL-RESPONSES RS) adds the responses RS, a list of lists of responses--one for each 
   rule in the current game, to the current list and then 
   returns T iff there was some non-empty response."
  (setf (responses-local *responses*) (mapcar #'nconc (responses-local *responses*) rs))
  (some #'(lambda (x) x) rs))

(defun include-global-responses (rs)
  "(INCLUDE-GLOBAL-RESPONSES RS) adds the responses RS, a list of responses,
   to the global portion of *responses* and then returns T iff RS is nonempty."
  (setf (responses-global *responses*) (nconc (responses-global *responses*) rs))
  rs)



;;;;;;;;;;;;;;;;;;;;;; testing ;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(deftheory game
  ; unsafe
  (<= (not (p ?x)) (q ?x))
  (<= (p ?x) (q ?y))
  (<= (p ?x) (q ?x) (not (q ?y)))
  (<= (p ?x) (q ?y) (not (q ?z)))
  (<= (p ?x) (q ?x) (or (not (r ?x)) (not (q ?y))))
  (<= (clearcolumn ?p ?x ?y1 ?y3)
      (adjacent ?y1 ?y2)
      (adjacent ?y2 ?y3)
      (or (true (cell ?x ?y2 b)) (true (cell ?x ?y2 ?p))))


  ; safe
  (<= (p ?x) (or (s ?x) (t ?x)))
  (<= (p ?x ?y) (q ?x ?z) (r ?y ?x) (not (r ?y ?z)))

  ; unstratified
  (<= (s1 ?x) (s2 ?x))
  (<= (s2 ?x) (s3 ?x))
  (<= (s3 ?x) (s4 ?x) (not (s1 ?x)))

  ; fsm
  (<= (role a) (true a))
  (<= (role ?x))
  (<= (true a) (true b))
  (<= (does a) (true b))
  (<= (true a) (and (next b) (next c)))
  (<= (does a) (init b))
  (<= d (does a))
  (<= (goal a) d)
  (<= (terminal a) d)
  (<= (legal a) d)
  (<= happy (or (next a) (init a) (init b) (init c) (init d) (init e)))  

  ; finiteness
  (<= (p (f ?x))
      (p ?x))

)


