;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; scrap -- contains code fragments to be deleted unless we find we need them
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;; Hodgepodge ;;;;;

(defun update-theory (th)
  "(UPDATE-THEORY TH) runs rewrite-conjuncts on TH."
  (let ((c (contents th)))
    (setq c (mapcar #'rewrite-conjuncts c))
    (setq c (remove-if #'tautologyp c))
    (define-theory th "" c))
  th
)
(defun purify-theory (th)
  "(PURIFY-THEORY TH) cleans up the theory TH."
  (let ((c (contents th)))
    (setq c (purify c))
    (define-theory th "" c))
)
(defun purify (ps)
  (uniquify (remove-if #'tautologyp ps))
)

(defun rewrite-conjuncts (rule)
  "(REWRITE-CONJUNCTS RULE) returns a new rule with some of the conjuncts
   rewritten."

  (cond ((atom rule) rule)
        ((and (cdr rule) (cddr rule)) 
         (append `(<= ,(cadr rule)) (mapcar #'replace-conjunct (cddr rule))))
        (t rule))
)
(defun replace-conjunct (conjunct)
  "(REPLACE-CONJUNCTS CONJUNCT) returns conjunct rewritten, possibly."
  (cond ((atom conjunct) conjunct)
        ((and (eq (car conjunct) 'not) (atom (cadr conjunct))) conjunct)
        (t
         (cond 
          ; (not (same..)) replaced with distinct
          ((and (eq (car conjunct) 'not) (eq (caadr conjunct) 'same))
           (list 'distinct (cadadr conjunct) (caddadr conjunct)))
          
          ; recurse on negation
          ((eq (car conjunct) 'not)
           (list 'not (replace-conjunct (cadr conjunct))) )

          ; otherwise just return the conjunct
          (t 
           conjunct ))))
) 

; duplicate, but with reversed args
(defun group-by (keygen list &key (test #'eq))
  "(GROUP-BY TEST LIST) returns an association list where the keys are the result of applying
   TEST to each element in LIST, and the value for each key is the set of list items that 
   produced that KEY."
  (do ((ls list (cdr ls))
       (res nil) (key) (entry))
      ((null ls) res)
    (setq key (funcall keygen (car ls)))
    (setq entry (assoc key res :test test))
    (if entry
	(setf (cdr entry) (cons (car ls) (cdr entry)))
	(setq res (acons key (list (car ls)) res)))))

;;;;;;;;;;;;;;;; Extensionally-defined ;;;;;;;;;;;;;;;;;;;

(defun extensionally-defined (th)
  "(GET-EXTENSIONALLY-DEFINED TH) returns a list of signed relation constants that indicates
   which relations are positively/negatively extensionally defined in TH.  "

    (mapcan #'(lambda (x) (extensionally-definedp (parameter-symbol x) th))
            (filter #'isrelation (get-vocabulary (maksand (contents th))))))


(defun extensionally-definedp (r th)
  "(EXTENSIONALLY-DEFINEDP R TH) returns (r) if r is positively extensionally defined.  Returns
   ((not r)) if r is negatively extensionally defined.  Returns (r (not r)) if both.  Returns nil otherwise.
   A relation r is 
   extensionally defined positively if a set of ground atoms in TH gives all the information 
   about atoms r(tbar) entailed by the theory.  Negatively uses negative ground atoms to
   provide all info about -r(tbar).
   For rule form only."

  (do ((rs (contents th) (cdr rs))
       (result `(,r (not ,r))))
      ((or (not result) (null rs)) result)
    (cond ((or (atom (car rs))           ; propositional
               (not (eq (caar rs) '<=))  ; atomic
               (null (cdar rs)))         ; no body
           'donothing)
          ((equal (relation (car rs)) r) (setq result (remove (signed-relation (car rs)) result :test #'equal))))))


;;;;;;;;;;;;;; Queue ;;;;;;;;;;;;;;;;
(defstruct queue (head nil) (tail nil))
(defun queue-append (l q)
  "(QUEUE-APPEND QUEUE L) appends L to the tail of Q."
  (cond ((queue-head q)
         (setf (cdr (queue-tail q)) l)
         (setf (queue-tail q) (last (queue-tail q))))
        (t
         (setf (queue-head q) l)
         (setf (queue-tail q) (last l))))
  (queue-head q))

(defun queue-enqueue (d q)
  "(QUEUE-ENQUEUE Q D) enqueues d at the end of the Q."
  (cond ((queue-head q)
         (setf (cdr (queue-tail q)) (list d))
         (setf (queue-tail q) (last (queue-tail q))))
        (t
         (setf (queue-head q) (list d))
         (setf (queue-tail q) (last (queue-head q)))))
  (car (queue-tail q)))  ; return d
  
(defun queue-adjoin (d q &key (key #'identity) (test #'eq))
  "(QUEUE-ENQUEUE Q D) enqueues d at the end of the Q."
  (let ((mem (member (funcall key d) (queue-head q) :test test :key key)))
    (if mem
      (car mem)
      (queue-enqueue d q))))
 
(defun queue-union (l q &key (key #'identity) (test #'eq))
  "(QUEUE-UNION Q L TEST) desctructively unions queue Q and list
   L."
  (mapcar #'(lambda (x) (queue-adjoin q x :key key :test test)) l))

(defun queue-peek (q) (car (queue-head q)))
(defun queue-pop (q) 
  (let ((r (queue-peek q))) 
    (setf (queue-head q) (cdr (queue-head q)))
    r))

;;;;;;;;;;;;;;;;;; Graph ;;;;;;;;;;;;;;;;;;

(defstruct graph (adjacency-list (make-queue)))
(defstruct graph-node name (slot nil))  ; e.g. use slot for DFS color

(defun graph-insert-noded (name graph)
  "(GRAPH-INSERT-NODED NAME GRAPH) inserts a new node named NAME into the 
   graph GRAPH. Returns object inserted.  Destructive."
  (let ((newq (make-queue)))
    (queue-enqueue (make-graph-node :name name) newq)
    (queue-enqueue newq (graph-adjacency-list graph))
    newq))

(defun graph-find-node (name graph)
  "(GRAPH-FIND-NODE NAME GRAPH) finds the node named NAME in GRAPH and returns
    its adjacency list."
  (find name 
        (queue-head (graph-adjacency-list graph))
        :key #'(lambda (x) (graph-node-name (first (queue-head x))))))

(defun graph-insert-edged (from to graph)
  "(GRAPH-INSERT-EDGED FROM TO GRAPH) adds a new edge into the graph.
    Destructive. Returns FROM adjacency list.  Adds nodes if not found"
  (let ((nodelist (graph-adjoin-noded from graph)) 
        (target (graph-adjoin-noded to graph)))

    (queue-enqueue (make-graph-node :name target) nodelist)
    nodelist))

(defun graph-adjoin-noded (name graph)
  "(GRAPH-ADJOIN-NODED NAME ADJLIST) inserts a new node named NAME into the 
   adjacency list. Treats assoc of nodes as a set. Destructive"
  (let ((newq (make-queue)))
    (queue-enqueue (make-graph-node :name name) newq)
    (queue-adjoin newq (graph-adjacency-list graph)
                  :key #'(lambda (x) (graph-node-name (car (queue-head x)))))))

(defun graph-adjoin-edged (from to graph)
  "(graph-ADJOIN-EDGED FROM TO ADJLIST) adds a new edge into the adjacency list.
    Treats edges as a set.  Returns FROM adjacency list.  Destructive. "
  (let ((nodelist (graph-adjoin-noded from graph))
        (target (graph-adjoin-noded to graph)))
    (queue-adjoin (make-graph-node :name target) nodelist :key #'graph-node-name)
    nodelist))

(defun graph-num-children (name graph)
  "(NUM-CHILDREN NAME GRAPH) returns the number of children node NAME has in graph."
  (1- (length (queue-head (graph-find-node name graph)))))

(defun graph-leaves (graph)
  "(GRAPH-LEAVES GRAPH) returns all the leaf nodes of graph, i.e. those without
   children."
  (remove-if-not #'(lambda (x) (cdr (queue-head x))) (graph-adjacency-list graph)))

(defun graph-node-names (graph)
  "(GRAPH-NODE-NAMES GRAPH) returns a list of all the node names in graph GRAPH."
  (mapcar #'graph-node-name (queue-head (graph-adjacency-list graph))))

(defun graph-nodes (graph)
  "(GRAPH-NODES GRAPH) returns a list of all the node adjacency lists in GRAPH."
  (queue-head (graph-adjacency-list graph)))

(defun graph-source-node (node-adjlist) (car (queue-head node-adjlist)))
(defun graph-dest-nodes (node-adjlist) (cdr (queue-head node-adjlist)))

(defun graph-print (graph)
  "(GRAPH-PRINT GRAPH) prints GRAPH to the screen."
  (format t "~&")
  (dolist (nl (queue-head (graph-adjacency-list graph)))
    (format t "~A:" (graph-node-name (graph-source-node nl)))
    (dolist (n (cdr (queue-head nl)))
      (format t " ~A" (graph-node-name (graph-source-node (graph-node-name n)))))
    (format t "~%"))
  graph)

(defun graph-cyclicp (graph)
  "(GRAPH-CYCLICP GRAPH) returns T iff GRAPH is cyclic.  Assumes SLOT of every node
   is NIL to start; sets to NIL at end."
  (let ((alist nil) (result nil))  ; alist is a dynamic variable we'll use to reset SLOTs.
    (do ((ns (graph-nodes graph) (cdr ns)))
        ((null ns) result)
      (when (and (not (graph-node-slot (graph-source-node (car ns)))) 
                 (graph-cyclic-nodep (car ns) graph))  ; not visited but contains cycle
        (setq result t)
        (when *graph-debug*
          (format *trace-device* "found cycle starting with ~A~%" 
		  (graph-node-name (graph-source-node (car ns)))))
        (return t)))
    (mapc #'(lambda (x) (setf (graph-node-slot x) nil)) alist)
    result))

(defun graph-cyclic-nodep (node graph)
  "(GRAPH-CYCLIC-NODE NAME GRAPH) checks whether there is a cycle reachable from the node NODE.
   Returns T if there is and NIL otherwise."
  (when *graph-debug*
    (format *trace-device* "(GRAPH-CYCLIC-NODEP (:name ~A :slot ~A)~%" 
            (graph-node-name (graph-source-node node))
            (graph-node-slot (graph-source-node node))))

  (cond ((and (graph-node-slot (graph-source-node node))   ; has been visited 
              (not (cdr (graph-node-slot (graph-source-node node))))) t) ; but has not been finished
        (t
         (setf (graph-node-slot (graph-source-node node)) (cons t nil))
         (setf alist (cons (graph-source-node node) alist))  ; remember so we can reset quickly
         (prog1 
           (some #'(lambda (x) (graph-cyclic-nodep (graph-node-name x) graph)) (graph-dest-nodes node))
           (setf (cdr (graph-node-slot (graph-source-node node))) t)))))


(defun graph-component (node graph)
  "(GRAPH-COMPONENT NODE GRAPH) returns all the nodes in GRAPH that are reachable from NODE."
  (let ((alist nil))  ; alist is a dynamic variable we'll use to reset slots.
    (graph-cyclic-nodep node graph)
    (mapc #'(lambda (x) (setf (graph-node-slot x) nil)) alist)
    (nreverse alist)))

;;;;;;;;;;;;;;;;; sGraph ;;;;;;;;;;;;;;;;
; Same as above, but with smaller memory footprint.  

(defstruct sgraph (adjacency-list (make-queue)))
(defstruct sgraph-node name (slot nil))  ; e.g. use slot for DFS color

(defun sgraph-insert-noded (name graph)
  "(SGRAPH-INSERT-NODED NAME GRAPH) inserts a new node named NAME into the 
   sgraph GRAPH. Returns object inserted.  Destructive."
  (let ((newq (list (make-sgraph-node :name name))))
    (queue-enqueue newq (sgraph-adjacency-list graph))
    newq))

(defun sgraph-find-node (name graph &key (test #'eq))
  "(SGRAPH-FIND-NODE NAME GRAPH) finds the node named NAME in GRAPH and returns
    its adjacency list."
  (find name 
        (queue-head (sgraph-adjacency-list graph))
        :key #'(lambda (x) (sgraph-node-name (car x)))
        :test test))

(defun sgraph-insert-edged (from to graph)
  "(SGRAPH-INSERT-EDGED FROM TO GRAPH) adds a new edge into the graph.
    Destructive. Returns FROM adjacency list.  Adds nodes if not found"
  (let ((nodelist (sgraph-adjoin-noded from graph)) 
        (target (sgraph-adjoin-noded to graph)))

    (setf (cdr nodelist) (cons target (cdr nodelist)))
    nodelist))

(defun sgraph-adjoin-noded (name graph)
  "(SGRAPH-ADJOIN-NODED NAME ADJLIST) inserts a new node named NAME into the 
   adjacency list. Treats assoc of nodes as a set. Destructive"
  (let ((new (list (make-sgraph-node :name name))))
    (queue-adjoin new (sgraph-adjacency-list graph)
                  :key #'(lambda (x) (sgraph-node-name (car x))))))

(defun sgraph-adjoin-edged (from to graph)
  "(SGRAPH-ADJOIN-EDGED FROM TO ADJLIST) adds a new edge into the adjacency list.
    Treats edges as a set.  Returns FROM adjacency list.  Destructive. "
  (let ((nodelist (sgraph-adjoin-noded from graph))
        (target (sgraph-adjoin-noded to graph)))
    (setf (cdr nodelist) (adjoin target (cdr nodelist) :key #'(lambda (x) (sgraph-node-name (car x)))))
    nodelist))

(defun sgraph-source-node (node-adjlist) (car node-adjlist))
(defun sgraph-dest-nodes (node-adjlist) (cdr node-adjlist))

(defun sgraph-print (graph)
  "(SGRAPH-PRINT GRAPH) prints GRAPH to the screen. Returns T."
  (format t "~&")
  (dolist (nl (queue-head (sgraph-adjacency-list graph)))
    (format t "~A:" (sgraph-node-name (sgraph-source-node nl)))
    (dolist (n (sgraph-dest-nodes nl))
      (format t " ~A" (sgraph-node-name (sgraph-source-node n))))
    (format t "~%"))
  t)

(defun sgraph-find-connected (name graph &key (test #'eq))
  "(SGRAPH-CONNECTED NAME GRAPH) finds all the names of nodes reachable from the source node
   named NAME in the graph GRAPH."
  (let ((node (sgraph-find-node name graph :test test)) (result)
        (alist nil))
    (cond ((not node) nil)
          (t
           (setq result (sgraph-find-connected-mark node graph))  ; get answer
           ; reset slots
           (dolist (n alist)
             (setf (sgraph-node-slot n) nil))
           result))))

(defun sgraph-find-connected-mark (adjlist graph)
  "(SGRAPH-CONNECTED-MARK NODE GRAPH) finds all the nodes reachable from the source node
   of ADJLIST in GRAPH.  Marks visited nodes as it goes
   and stores pointers to those nodes in alist.  Returns a list of node names."
  (when *graph-debug*
    (format *trace-device* "(SGRAPH-FIND-CONNECTED-MARK (:name ~A :slot ~A)~%" 
            (sgraph-node-name (sgraph-source-node adjlist))
            (sgraph-node-slot (sgraph-source-node adjlist))))

  (cond ((sgraph-node-slot (sgraph-source-node adjlist)) nil)
        (t
         (setf (sgraph-node-slot (sgraph-source-node adjlist)) t)   ; mark the node
         (setf alist (cons (sgraph-source-node adjlist) alist))  ; remember so we can reset quickly
         (cons (sgraph-node-name (sgraph-source-node adjlist))
               (mapcan #'(lambda (x) (sgraph-find-connected-mark x graph)) (sgraph-dest-nodes adjlist))))))




;;;;;;;;;;;;;;;;;;;; Discrimination Tree ;;;;;;;;;;;;;;;;;;;;;


;; SPEED-UP: take p and al and avoid flattening the key to avoid touching entire
;;   thing twice before looking at the tree at all.
(defun dt-instances (key tree)
  "(DT-INSTANCES KEY TREE) finds all the elements in discrimination tree TREE
   that are instances of KEY, e.g. p(a) and p(b) are instances of p(x)."
  (dt-instances-noflat key tree))

(defun dt-instances-noflat (key tree &optional (al (environment)))
  "(DT-INSTANCES KEY TREE) finds all the elements in discrimination tree TREE
   that are instances of KEY, e.g. p(a) and p(b) are instances of p(x).  This version
   does not flatten the key."
  (let ((nodes (dt-instances-noflat-aux (if (listp key) (car key) key) (if (listp key) (cdr key) nil) al tree nil 0)))
    (if nodes
      (mapcar #'data nodes)
      nil)))

(defun dt-instances-noflat-aux (key keyl al tree context depth)
  "(DT-INSTANCES-FLAT-AUX KEY AL TREE) computes all the node/binding-list pairs 
   in TREE that are instances of KEY after applying AL."
  (declare (notinline dt-instances-noflat-aux))
  ;(format t "Called on tree ~A with label ~A~%" tree (label tree))
  (cond ((not tree) nil)
        ((not key) (list tree))
        ((varp key)   ; choice poinht
         (do ((bs (data tree) (cdr bs))
              (result nil) (ol))
             ((null bs) result)
           (setq ol (match key (node-term-term (car bs)) al))
           (when ol 
             (setq result (nconc (nextcall keyl al (node-term-node (car bs)) context depth) result)))
           (backup ol)))
        ((atom key)
         (nextcall keyl al (discrimination-tree-find-child key tree) context depth))
        (t  ; list
         (dt-instances-noflat-aux (car key) (cdr key) al tree (cons keyl context) depth))))

(defun nextcall (pl al tree context depth)
  "(NEXTCALL PL AL TREE CONTEXT) is similar to fulloneexit in epilog.  Helps turn recursive
   walk into linear one."
  (cond (pl (dt-instances-noflat-aux (car pl) (cdr pl) al tree context (1+ depth)))
        ((null context) (list tree))   ; have an answer
        ((not (car context)) (nextcall pl al tree (cdr context) depth))
        (t (dt-instances-noflat-aux (caar context) (cdar context) al tree (cdr context) (1+ depth)))))

(defun dt-instances-flat (key tree)
  "(DT-INSTANCES KEY TREE) finds all the elements in discrimination tree TREE
   that are instances of KEY, e.g. p(a) and p(b) are instances of p(x).  This version
   flattens the key."
  (let* ((flatkey (flatten key))
         (nodes (dt-instances-flat-aux flatkey (environment) tree)))
    (if nodes
      (mapcar #'data nodes)
      nil)))

(defun dt-instances-flat-aux (key al tree)
  "(DT-INSTANCES-FLAT-AUX KEY AL TREE) computes all the node/binding-list pairs 
   in TREE that are instances of KEY after applying AL.  KEY has been flattened."
  (cond ((not tree) nil)
        ((null key) (list tree))
        (t
         (let ((k (plug (car key) al)))
           (cond ((varp k)  ; variable
                  ; walk over all the possible instances of the variable
                  (do ((bs (data tree) (cdr bs))
                       (ol) (results nil))
                      ((null bs) results)
                    (setq ol (match (car key) (node-term-term (car bs)) al))
                    (when ol (setq results (nconc (dt-instances-flat-aux (cdr key) al (node-term-node (car bs)))
                                                  results)))
                    (backup ol)))

                 (t   ; no branching here
                  (dt-instances-flat-aux (cdr key) al (discrimination-tree-find-child k tree))))))))




(defun match (x y al)
  "(MATCH X Y AL) takes two expressions as arguments and checks whether Y
  expression is an instance of X, i.e. whether there is a set of
  variable bindings that, when substituted into the first expression,   
  produces an expression meta-equal to the second expression.
  MATCH alters AL so that Y is an instance of X if possible.  It returns 
  the list of changes made to AL or NIL if not possible.  Does not
  know about strings, quote, listof, etc.  Just the basics."
  
  (let ((alist truth))
    (cond ((matchexp x y al) alist)
          (t (backup alist)))))

(defun matchexp (x y al)
  "(MATCHEXP X Y AL) alters the environment AL so that X.AL is Y, if possible.
   Returns nil if not possible or changes to AL if possible."
  (cond ((eq x y) t)
        ((varp x) (matchindvar x y al))
        ((atom x) nil)   ; ignore strings
        (t
         (if (listp y) (matchexpexp x y al) nil))))

(defun matchindvar (x y al)
  "(MATCHINDVAR X Y AL) binds X to Y if possible in the environment AL."
  (let (dum)
    (cond ((eq x y) nil)   ; equal already
          ((cdr (setq dum (getbdg x al)))   ; x is bound to something in al
           (equal (cadr dum) y))   ; whatever x is bound to must be equal to y
          ((null dum)
           (setq x (list x y))
           (setf (alist al) (cons x (alist al))) ; make new assignment
           (setq alist (cons x alist)) ) ; remember assignment
          (t
           (setf (cdr dum) (cons y nil))  ; make old assignment
           (setq alist (cons dum alist))))))  

(defun matchexpexp (x y al)
  "(MATCHEXPEXP X Y AL) handles the case when x and y are both lists.
   Returns T if the X and Y match; returns NIL otherwise.  Sets alist
   to list of changes made to AL."
  (do ((xs x (cdr xs)) (ys y (cdr ys)))
      (nil)
    (cond ((null xs) (if (null ys) (return t) (return nil)))  ; end of xs
          ((null ys) (return nil))  ; end of ys before end of xs
          ((matchexp (car xs) (car ys) al) nil)  ; don't return--just continue
          (t (return nil)))))
          

;;;;;;;;;; Testing discrimination tree
(defvar *tree*)
(defun test-dt (&optional (n '*))
  (let ( (keys) (result1) (result2) (result3) (result4))
    (when (member* 1 n)
      (setq *tree* (make-instance 'discrimination-tree))
      (setq keys '((has_job c (f a b))
                   (has_job e (f e b))
                   (has_job c (f (h b) (g l m n)))
                   (has_job (f a b) c)
                   (has_job (f b c) a)
                   (not p)
                   p))
      (mapc #'(lambda (x) (discrimination-tree-insert x x *tree*)) keys)
      (dtp *tree*)
      (format t "~%")
      (setq result1 (dt-samex '(has_job (f a b) c) *tree*))
      )

    ; want to test with large set of sentences, e.g.
    ;  p(x1,x2,x3)
    ;  p(x1,x1,x3)
    ;  p(x1,x2,x1)
    ;  p(x1,x2,x2)
    ;  p(x1,x1,x1)

    (when (member* 2 n)
      (setq *tree* (make-instance 'discrimination-tree))
      (setq keys '((g ?x a ?x)
                   (g ?y ?y b)
                   (g ?z ?x b)
                   (g ?x1 (f ?x2 (t ?x3 ?x4) ?x5 ?x6) ?x3)
                   (g ?x (f ?y) c)
                   (g ?x (f ?x) c)
                   ))
      (mapc #'(lambda (x) (discrimination-tree-insert x x *tree*)) keys)
      (dtp *tree*)
      (format t "~%")
      (setq result2 (dt-samex '(g ?x (f ?y) c) *tree*))
      )

    (when (member* 3 n)
      (setq *tree* (make-instance 'discrimination-tree))
      (setq keys '((p q r)
                   (g a a a)
                   (g a a b)
                   (g a b a)
                   (g a b b)
                   (g b a a)
                   (g b a b)
                   (g b b a)
                   (g b b b)
                   (g ?x a a)
                   (g ?x a b)
                   (g ?x b a)
                   (g ?x b b)
                   (g a ?x a)
                   (g a ?x b)
                   (g b ?x a)
                   (g b ?x b)
                   (g a a ?x)
                   (g a b ?x)
                   (g b a ?x)
                   (g b b ?x)
                   (g ?x ?x a)
                   (g ?x ?x b)
                   (g ?x a ?x)
                   (g ?x b ?x)
                   (g a ?x ?x)
                   (g b ?x ?x)
                   (g ?x ?x ?x)

                   (g ?x ?y a)
                   (g ?x ?y b)
                   (g ?x a ?y)
                   (g ?x b ?y)
                   (g a ?x ?y)
                   (g b ?x ?y)
                   (g ?x ?x ?y)
                   (g ?x ?y ?x)
                   (g ?y ?x ?x)
                   (g ?x ?y ?z)

                   (g ?y a (f ?y))
                   (g (f ?y) a (f ?y))
                   (g (f ?y) a ?y)
                   (g (f (f (f ?y))) a ?y)
                   (g (f (f (g ?x ?y))) ?x ?y)
                   ))
      (mapc #'(lambda (x) (discrimination-tree-insert x x *tree*)) keys)
      (dtp *tree*)
      (format t "~%")
      (setq result3 (dt-instances '(g ?x a ?x) *tree*)))

    (when (member* 4 n)
      (let ((al (environment)))
        (match '(p ?x) '(p a) al)
        (setq *tree* (make-instance 'discrimination-tree))
        (setq keys '((f (h a) (g b c))
                     (f (h b) c)
                     (f ?x (g a a))
                     (f a (g b c))
                     (f ?x (g b c))))
        (mapc #'(lambda (x) (discrimination-tree-insert x x *tree*)) keys)
        (dtp *tree*)
        (format t "~%")
        (setq result4 (list (dt-instances-noflat '(f ?x (g b c)) *tree* al) (alist al) (plug '(p ?x) al)))))


    (values result1 result2 result3 result4)))


(defun test-dt2 (&optional (n '*))
  (let (keys tmp maxkids symb2index)
    (when (member* 1 n)
      (setq *tree* (make-instance 'discrimination-tree))
      (setq keys '((p a a a) (q c b a)
                   (p a a b)
                   (p a a c)
                   (p a b a)
                   (p a b b)
                   (p a b c)
                   (p a c a)
                   (p a c b)
                   (p a c c)
                   (p b a a)
                   (p b a b)
                   (p b a c)
                   (p b b a)
                   (p b b b)
                   (p b b c)
                   (p b c a)
                   (p b c b)
                   (p b c c)
                   (p c a a)
                   (p c a b)
                   (p c a c)
                   (p c b a)
                   (p c b b)
                   (p c b c)
                   (p c c a)
                   (p c c b)
                   (p c c c)))

      ; prepare the function for mapping a constant to a number
      (multiple-value-setq (symb2index maxkids) (discrimination-tree-prep2 (get-vocabulary (maksand keys)) *tree*))

      ; insert all the keys
      (setq tmp 0)
      (dolist (k keys)
        (discrimination-tree-insert2 k tmp *tree* maxkids symb2index)
        (setq tmp (1+ tmp)))

      (dtp *tree* #'array2list)
      (format t "~%")
      (eq 0 (dt-lookup2 '(p a a a) *tree* symb2index))
        )))

(defun symb2indx (x) (get x 'tlh))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
