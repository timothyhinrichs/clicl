

(in-package :tlh)

(defmethod index ((key symbol) value th)
  "(INDEX KEY VALUE TH) index value on key for theory."
    (setf (get th key) (cons value (get th key)))
)

(defmethod insert ((key symbol) value th)
  "(INSERT KEY VALUE TH) indexes key for value in theory th and keeps track of the
   vocabulary for th."
   
  (when (and (null (get th key))
             (not (member 'key (vocab th))))
    (index 'vocab key th))
  (index key value th)
)
(defun vocab (th)
  (get th 'vocab)
)
 
(defmethod unindex ((key symbol) th)
  "(UNINDEX KEY TH) removes the values for KEY in TH."
  (setf (get th key) nil)
)
(defmethod lookup ((key symbol) th)
  "(LOOKUP KEY TH) returns all the values for key in theory TH."
  (get th key)
)

(defun index-relns (clause th)
  "(INDEX-RELN CLAUSE TH) indexes clause on all its relations."

  (dolist (c clause)
    (insert (relation c) clause th))
)

(defun clear-index-vocab (th)
  "(CLEAR-INDEX-RELNS TH) cleans out all the indexes for theory TH."

  (dolist (r (vocab th))
    (unindex r th))
  (setf (get th 'vocab) nil)
)

;;;;;;;;;; subsumption  ;;;;;;;;;;

;;;;;; using property list
(defun clear-index (th)
  (clear-index-vocab th)
)
(defun index-all (l th)
  (mapcar #'(lambda (x) (index-clause x th)) l)
)
(defun lookup-subsumers (clause th)
  "(LOOKUP-SUBSUMERS CLAUSE) computes the set of clauses that might subsume CLAUSE."

  (if *indexing*
    (let ((maxrelns (make-queue)))
      ; maximum size set of relations--any clause that has more cannot subsume
      (mapcar #'(lambda (x) (queue-adjoin maxrelns (relation x))) clause)
      
      ; pick through index, removing any clause that has more than maxrelns
      (do ((cs clause (cdr cs))
           (result (make-queue)))
          ((null cs) (queue-head result))
        (queue-union result (remove-if-not #'(lambda (x) (possible-subsumer (clause-disjunction x) (queue-head maxrelns))) 
                                           (lookup (relation (car cs)) th))
                     :test #'equal) ))
    (lookup 'all th))
)
(defun possible-subsumer (clause maxrelns)
  "(POSSIBLE-SUBSUMERP CLAUSE MAXRELNS) check whether the relations of CLAUSE are a subset
   of MAXRELNS."
  (subsetp (mapcar #'relation clause) maxrelns)
)
(defun index-clause (clause th)
  (if *indexing* 
    (mapcar #'(lambda (x) (insert (relation x) clause th)) (clause-disjunction clause))
    (mapcar #'(lambda (x) (declare (ignore x)) (insert 'all clause th)) (clause-disjunction clause)) )
)


;;;; using hash table
(defvar *subsumption-index* (make-hash-table :test #'equal))

(defun subsump-clear-index()
  (clrhash *subsumption-index*)
)
(defun subsump-add-entry (key additionalvalue)
  (setf (gethash key *subsumption-index*) (cons additionalvalue (gethash key *subsumption-index*) ))
)
(defun subsump-index-insert (clause)
  "(SUBSUMP-INDEX-INSERT CLAUSE) inserts the clause CLAUSE into the subsumption index."
  (let ((key (subsump-compute-key (clause-disjunction clause))))
    (subsump-add-entry key clause)
    )
)
(defun subsump-compute-key (clause)
  "(COMPUTE-KEY CLAUSE) compute the subsumption key for CLAUSE."
  (do ((cs clause (cdr cs))
       (result (make-queue)))
      ((null cs) (sort (queue-head result) #'string<))
    (queue-adjoin result (relation (car cs))))
)
(defun subsump-index-lookup (clause)
  "(SUBSUMP-INDEX-LOOKUP CLAUSE) returns the set of clauses stored at the key of CLAUSE."

  (gethash (subsump-compute-key (clause-disjunction clause)) *subsumption-index*)
)

(defun subsump-index-all (l)
  (mapcar #'subsump-index-insert l)
) 
