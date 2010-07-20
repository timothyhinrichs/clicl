;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Rada ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; TODO: (3) REWRITE-QUERY-RADA (actually rewrite, don't just print)

; (simplify-viewset (construct-w-views (construct-w-subgoals '(?x ?y ?z) (list (make-table :parameter (make-parameter :symbol 'hue :arity 1 :type 'relation) :size 2)))))

; (time (setq v (construct-w-views (construct-w-subgoals '(?x ?y ?z) (list (make-table :parameter (make-parameter :symbol 'hue :arity 1 :type 'relation) :size 2) (make-table :parameter (make-parameter :symbol 'distinct :arity 2 :type 'relation) :size 10)))) b 0))
; (time (setq a (simplify-viewset v) b 0))

; schema: list of tables
(defstruct table parameter size)
(defun find-table (reln schema) 
  (find reln schema :test #'(lambda (x) (parameter-symbol (table-parameter x)))))
(defun sum-table-sizes (schema) (reduce #'+ (table-size schema)))

; queries: list of query
; query: (cons sentence percentage)
(defun query-sentence (query) (car query))
(defun query-percentage (query) (cdr query))


(defun generate-optimal-viewset (r q b c)
  "(GENERATE-OPTIMAL-VIEWSET R Q B C) takes as input a database schema R, a finite query workload
   Q, a storage limit B, and a cost model C.  It returns a set of view definitions such that
   once materialized, they can be used to answer the query workload Q in the minimal amount of 
   time, according to the cost model C under the constraint that the size of the materializations
   is less than B.  This is Rada's thesis.  Assumes conjunctive queries and views in 
   non-recursive, negation-free Datalog, using one of two
   cost models.  Notice that this algorithm comes from Theorem 4.4.1--it allows a view to be
   reused multiple times in the query rewritings."

  (let (best-rewritings s rewritings min-eval-time eval-time w vars subgoals newr)
    (setq best-rewritings q)
    (setq min-eval-time (funcall c best-rewritings r))

    ; first construct the viewset W, which Thm 4.4.1 guarantees must exist -- 
    ;   doubly exponential in Q
    (setq vars (construct-W-vars q))
    (setq subgoals (construct-W-subgoals vars r))
    (setq w (construct-w-views subgoals))
    (setq w (simplify-viewset w))

    ; then check all subsets of W--Thm 4.4.1 guarantees the optimal answer is one of them 
    (do ((Wsubsets (nonempty-subsets w) (cdr Wsubsets)))
        ((null Wsubsets) best-rewritings)
      (setq s (car Wsubsets))

      ; compute the new schema that would result from materializing this viewset
      (setq newr (construct-new-schema r s c))

      ; check if the materialization is too large
      (when (space-limit-not-exceeded b newr)

        ; compute the new query definitions based on this viewset
        (setq rewritings (rewrite-workload q s))

        ; compute their cost, saving the cheapest
        (when rewritings

          ; then estimate the cost of evaluating those new query definitions on the new database.
          (setq eval-time (funcall c rewritings newr))

          ; and save the cheapest one
          (when (< eval-time min-eval-time)
            (setq best-rewritings rewritings)
            (setq min-eval-time eval-time)))))))


;; Naive Constructing W ;;

(defun construct-W-vars (queries)
  "(CONSTRUCT-W-VARS QUERIES) returns the required number of variables, as dictated by
   Rada's proof, which I believe is just the product of the number of variables in 
   the queries."
  (reduce #'* (mapcar #'(lambda (x) (length (vars (query-sentence x)))) queries)))

#|
  "(CONSTRUCT-W-VARS Q) count how many new variables we need and return that many.
   QUERIES is a list of rules.  Expression: 
   U (k1<=n1,...,km<=nm) (Var(Q1))^k1 x ... x (Var(Qm))^km,
   where ni is the number of subgoals for query Qi.  I think this is right again.
   In her prose she says each variable represents a p-tuple, where each element of that
   tuple is in Var(Q1) U ... U Var(Qm), with at most ni elements from Var(Qi) for every i.
   But I think this description vastly overcounts, since only the first k1 elements of every tuple
   can be from Var(Q1).  The prose includes an extra p!, where there shouldn't be one.  
   Also, p is just k1 + ... + km.  Rada was describing the tuple of tuples as being flattened
   into a single tuple."
  
   I've decided to go back to the other version, where the number of variables is just 
   the multiplication of the number of variables in each of the queries.  It looks like that
   is all Rada's proof requires. 
  (assert (= (length queries) 1) nil "Construct-W-vars only works when number of queries is 1.")

  (let ((sum 0) (varcount (length (vars (query-sentence (car queries))))))
    ;(varcounts (mapcar #'(lambda (x) (length (vars x))) (mapcar #'query-sentence queries))))
    
    ; now just sum over all ki. -- screw it.  For now assume there is a single query.
    (dotimes (i (length (cddr (query-sentence (car queries)))))
      (setq sum (+ sum (expt varcount (1+ i)))))

    ; return the new set of variables
    (maptimes #'newindvar sum)))
|#

(defun construct-w-subgoals (vars schema)
  "(CONSTRUCT-W-SUBGOALS VARS SCHEMA) returns all the possible subgoals in terms
   of VARS and all the relations in SCHEMA."
  (let ((relations (delete-if-not #'isrelation (mapcar #'table-parameter schema)))
        (results nil))
    (dolist (r relations)
      (setq results (nconc (saturate (parameter-symbol r) (parameter-arity r) vars) results)))
    results))

(defun saturate (r arity terms)
  "(SATURATE R ARITY TERMS) computes all the possible atoms of arity ARITY with relation
   constant R using TERMS, of which there are TERMS^ARITY."
  (cond ((or (not (numberp arity)) (null terms)) nil)
        ((= arity 0) (list '(r)))
        (t
         (mapcar #'(lambda (x) (cons r x)) (cross-product terms arity)))))

(defun construct-w-views (subgoals)
  "(CONSTRUCT-W-VIEWS SUBGOALS) compute all the conjunctive implications where the bodies
   are built out of SUBGOALS, which include VARS variables."
  (let ((bodies (nonempty-subsets subgoals))
        (result nil) (heads))
    (dolist (b bodies)
      (setq heads (nonempty-subsets (vars b)))
      (dolist (h heads)
        (setq result (cons (append `(<= ,(cons (gentemp "r") h)) b)
                           result))))
    (nreverse result)))
 
(defun simplify-viewset (views)
  "(SIMPLIFY-VIEWSET VIEWS) removes views that are subsumed by some other view."
  (format t "~&Standardizing variables...~%")
  (setq views (mapcar #'stdize views))
  (do ((r1 views (cdr r1))
       (lastl nil) (lastlupdated nil nil) (i 1 (1+ i)) (l (length views)))
      ((null r1) views)
    (format t "Checking ~A out of ~A...~%" i l)
    (do ((r2 (cdr r1) (cdr r2)) (innerdone nil))
        ((or innerdone (null r2)))
      (when (eqview (car r1) (car r2))
        ; remove (car r1) and go onto the next r1
        (cond ((not lastl) 
               ;(format t "changing start from ~A to ~A~%" views (cdr views))
               (setq views (cdr views))
               (setq lastl nil)
               (setq lastlupdated t))
              (t 
               ;(format t "changing cdr of ~A to ~A~%" lastl (cdr r1)) 
               (setf (cdr lastl) (cdr r1))
               (setq lastlupdated t)))
        (setq innerdone t)))
    ;(format t "Setting lastl from ~A to ~A~%" lastl r1)
    (when  (not lastlupdated)
      (setq lastl r1))))

#|
      (dolist (r2 (cdr r1))
        (when (eqview (car r1) r2) 
          (setq removed (cons r2 removed)))))
    (set-difference views removed :test #'equal)))
|#

(defun eqview (v1 v2)
  "(EQVIEW V1 V2) returns T iff V1 and V2 are equivalent views."
  (setq v1 (cons (cdr (cadr v1)) (cddr v1)))
  (setq v2 (cons (cdr (cadr v2)) (cddr v2)))

    (cond ((not (similarp v1 v2)) nil)
          ((not (similarp v2 v1)) nil)
          (t t)))

;; Query evaluation costs ;;

(defun total-product-cost (queryset r)
  "(TOTAL-PRODUCT-COST QUERYSET R) takes a set of queries and estimates the total cost for 
   those queries over the database schema R.  R includes statistics that approximate the size 
   of each relation."
  (reduce #'+ (mapcar #'(lambda (x) (* (product-cost x r) (query-percentage x))) queryset)))

(defun product-cost (query r)
  "(PRODUCT-COST QUERY R).  Assume no selections; leave projections until the end.  That is,
   count the joins and multiply their sizes.  For now, just ignore = and #."
  (setq query (cddr query))  ; lop off <= and head
  (reduce #'* (mapcar #'(lambda (x) (if (or (eq (car x) '=)
                                            (eq (car x) 'distinct))
                                      0
                                      (table-size (find-table (car x) r))))
                      query)))

;; Construct a new schema using view definitions ;;
(defun construct-new-schema (r s c)
  "(CONSTRUCT-NEW-SCHEMA R S C) computes the schema resulting from materializing all the views
   in S on the schema R using cost model C."
  (mapcar #'(lambda (x) (cons (relation x) (funcall c x r))) s))


;; Meet the size constraint?
(defun space-limit-not-exceeded (b r)
  "(SPACE-LIMIT-NOT-EXCEEDED B R) returns T iff the cost of the relations in R is less than B."
  (< (sum-table-sizes r) b))
  
;; Rewrite a workload in terms of views ;;
(defun rewrite-workload (queries views)
  (mapcar #'(lambda (x) (rewrite-query-rada x views)) queries))

(defun rewrite-query-rada (q views)
  "(REWRITE-QUERY Q VIEWS) given a query Q of the form (<= h b1 ... bn) and a set of view
   definitions, each of which is in the same form, return an equivalent definition for Q
   in terms of VIEWS."
  ; don't know how to do this efficiently for now just print to the screen so I can see whether
  ;   the right answer would appear.
  (format t "How do we rewrite ~A in terms of ~A~%" q views)
  nil)