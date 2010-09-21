;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; indexing.lisp
;;;     indexing logical sentences
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Theories
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod contents ((th list)) th)
(defmethod contents ((th string)) (if (probe-file th) (read-file th) nil))

(defmethod save (p (th list) &optional (f 'samep))
  (adjoin p th :test f))
(defmethod drop (p (th list) &optional (f 'samep))
  (delete p th :test f))

(defmethod definemore ((th list) facts)
  (dolist (p facts th)
    (setq th (save p th))))

(defun save-theory (th doc facts fn)
  "(SAVE-THEORY TH DOC FACTS FN) saves FACTS to file FN as theory TH
    with documentation DOC and pretty-prints the facts in the file."
  (with-open-file (f fn :direction :output :if-exists :supersede)
    (princ "(deftheory " f)
    (princ th f)
    (princ (format nil " ") f)
    (prin1 doc f)
    (princ (format nil "~%") f)
    (pretty-print facts f)
    (princ (format nil ")") f)
    )
  th)

; indexing is signed relational on the head
; lookup is signed relational
(defclass metheory (theory)
  ((content   :accessor content   :initarg :content   :initform nil)
   ;(extensions :accessor extensions :initarg :extensions :initform nil)
   ;(intensions :accessor intensions :initarg :intensions :initform nil)
   (includees :accessor includees :initarg :includees :initform nil)
   (includers :accessor includers :initarg :includers :initform nil)
   (doc       :accessor doc       :initarg :doc       :initform "")))

; indexing is relational on the head
(defclass prologtheory (theory)
  ((content   :accessor content   :initarg :content   :initform nil)
   ;(extensions :accessor extensions :initarg :extensions :initform nil)
   ;(intensions :accessor intensions :initarg :intensions :initform nil)
   (includees :accessor includees :initarg :includees :initform nil)
   (includers :accessor includers :initarg :includers :initform nil)
   (doc       :accessor doc       :initarg :doc       :initform "")))

(defclass modeltheory (theory)
  ((content   :accessor content   :initarg :content   :initform nil)
   ;(extensions :accessor extensions :initarg :extensions :initform nil)
   ;(intensions :accessor intensions :initarg :intensions :initform nil)
   (includees :accessor includees :initarg :includees :initform nil)
   (includers :accessor includers :initarg :includers :initform nil)
   (doc       :accessor doc       :initarg :doc       :initform "")))

; structure that splits a theory into two pieces, which we will call
;   the extensions and the intensions.  The intuition is that the intensions
;   is a conservative extension of the extensions.  In reality, this may
;   or may not be true.  
(defclass bileveltheory (theory)
  ((content   :accessor content   :initarg :content   :initform nil)
   (extensions :accessor extensions :initarg :extensions :initform nil)
   (intensions :accessor intensions :initarg :intensions :initform nil)
   (includees :accessor includees :initarg :includees :initform nil)
   (includers :accessor includers :initarg :includers :initform nil)
   (doc       :accessor doc       :initarg :doc       :initform "")))

; full indexing for extensional tables
; relational indexing on the head for intensional tables
; lookup for extensional tables grabs the first ground arg; defaults to relational
; lookup for intensional tables is relational
(defclass ddbtheory (bileveltheory)
  ((content   :accessor content   :initarg :content   :initform nil)
   (extensions :accessor extensions :initarg :extensions :initform nil)
   (intensions :accessor intensions :initarg :intensions :initform nil)
   (includees :accessor includees :initarg :includees :initform nil)
   (includers :accessor includers :initarg :includers :initform nil)
   (doc       :accessor doc       :initarg :doc       :initform "")))

; this theory is structurally different than the others.  
; It includes a list of children, representing the FSL cascades.
; Each descendent theory can be indexed individually.
;   Thus, must include all elements from other theories.
(defclass prologcascadetheory (prologtheory)
  ((content   :accessor content   :initarg :content   :initform nil)
   (extensions :accessor extensions :initarg :extensions :initform nil)
   (intensions :accessor intensions :initarg :intensions :initform nil)
   (includees :accessor includees :initarg :includees :initform nil)
   (includers :accessor includers :initarg :includers :initform nil)
   (doc       :accessor doc       :initarg :doc       :initform "")
   (child :accessor child :initarg :child :initform nil)
   (indextype :accessor indextype :initarg :indextype :initform nil)))

(defmethod extensions ((th symbol))
  (get th 'extensions))

(defmethod (setf extensions) (extensions (th symbol))
  (setf (get th 'extensions) extensions))

(defmethod intensions ((th symbol))
  (get th 'intensions))

(defmethod (setf intensions) (intensions (th symbol))
  (setf (get th 'intensions) intensions))


; defining specially-indexed theories
(defmacro defcascadetheory (x &rest l)
  (if (stringp (car l))
    `(define-prologcascadetheory ',x ,(car l) ',(cdr l))
    `(define-prologcascadetheory ',x nil ',l)))

(defmacro defddbtheory (x &rest l)
  (if (stringp (car l))
    `(define-ddbtheory ',x ,(car l) ',(cdr l))
    `(define-ddbtheory ',x nil ',l)))

(defmacro defmetheory (x &rest l)
  (if (stringp (car l))
    `(define-metheory ',x ,(car l) ',(cdr l))
    `(define-metheory ',x nil ',l)))

(defmacro defprologtheory (x &rest l)
  (if (stringp (car l))
    `(define-prologtheory ',x ,(car l) ',(cdr l))
    `(define-prologtheory ',x nil ',l)))
    
(defmacro defmodeltheory (x &rest l)
  (if (stringp (car l))
    `(define-modeltheory ',x ,(car l) ',(cdr l))
    `(define-modeltheory ',x nil ',l)))


(defun define-prologcascadetheory (th doc facts)
  (let (child)
    (multiple-value-setq (child facts) 
      (split #'(lambda (x) (and (listp x) (eq (car x) 'defcascade))) facts))
    (assert (null (cdr child)) nil "Cascades must be totally ordered.")
    (setq child (car child))
    (cond ((symbolp th) 
	   (set th (make-instance 'prologcascadetheory))    
	   (define-theory (symbol-value th) doc facts))
	  (t (define-theory th doc facts)))
    (when child
      (setf (child (if (symbolp th) (symbol-value th) th))
	    (define-prologcascadetheory (make-instance 'prologcascadetheory) "" (cdr child))))
    th))

(defun define-metheory (th doc facts)
  (cond ((symbolp th) 
	 (set th (make-instance 'metheory))
	 (define-theory (symbol-value th) doc facts))
	(t (define-theory th doc facts))))

(defun define-prologtheory (th doc facts)
  (cond ((symbolp th) 
	 (set th (make-instance 'prologtheory))
	 (define-theory (symbol-value th) doc facts))
	(t (define-theory th doc facts))))

(defun define-modeltheory (th doc facts)
  (cond ((symbolp th)
	 (set th (make-instance 'modeltheory))
	 (define-theory (symbol-value th) doc facts))
	(t (define-theory th doc facts))))

(defun define-ddbtheory (th doc facts)
  (let ((all (preds (maksand facts)))
	(exts (extensional-preds facts))
	(trueth (make-instance 'ddbtheory)))
    (setf (extensions trueth) (hashpreds exts (make-hash-table)))
    (setf (intensions trueth) (hashpreds (set-difference all exts 
							 :test #'equalp) 
					 (make-hash-table)))
    (cond ((symbolp th)
	   (set th trueth)
	   (define-theory trueth doc facts))
	  (t (define-theory trueth doc facts)))))

(defun extensionp (p th)
  (gethash (parameter-symbol p) (extensions th)))

(defun hashpreds (ps hash)
  (dolist (p ps hash)
    (setf (gethash (parameter-symbol p) hash) p)))

(defun extensional-preds (th)
  (let ((ps nil) (intensions nil))
    (dolist (p (contents th) ps)
      (when (and (datap p) (not (member (pred p) intensions :test #'equalp)))
        (setq ps (adjoin (pred p) ps :test #'equalp)))
      (when (not (datap p))
        (setq intensions (adjoin (pred (head p)) intensions :test #'equalp))
        (setq ps (remove (pred (head p)) ps :test #'equalp))))))


; defining theories that reformulate the input before indexing
(defmacro deffoltheory (x &rest l)
  "(DEFFOLTHEORY X L) takes a theory name X and a list L and
   converts all the sentences in L to rule form before defining
   a model elimination theory."
  (if (stringp (car l))
      `(define-foltheory ',x ,(car l) ',(cdr l))
      `(define-foltheory ',x nil ',l)))

#|
(defmacro defcontratheory (x &rest l)
  "(DEFCONTRATHEORY X L) takes a theory name X and a list L and
   computes all the contrapositives of the ruleform of L before
   defining a model elimination theory."
  (if (stringp (car l))
      `(define-foltheory ',x ,(car l) ',(cdr l) #'contrapositives)
      `(define-foltheory ',x nil ',l #'contrapositives)))
|#

(defmacro defcontra=theory (x &rest l)
  "(DEFCONTRA=THEORY X L) takes a theory name X and a list L and
   computes all the contrapositives without putting = literals at the head
   of the ruleform of L before defining a model elimination theory."
  (if (stringp (car l))
      `(define-foltheory ',x ,(car l) ',(cdr l) #'contras-wo=)
      `(define-foltheory ',x nil ',l #'contras-wo=)))

(defmethod define-foltheory (th doc facts &optional (contras nil))
  "(DEFINE-FOL-THEORY) defines a theory of FOL sentences, converting to rule 
   form (including all contrapositives if CONTRAS is true), using model 
   elimination indexing."
  (let* ((qfacts (mapcar #'quantify facts))
         (rules (if contras (funcall contras (maksand qfacts)) 
		    (brfs (maksand qfacts)))))
    (define-metheory th doc rules)))


;;;;;;;;;;;;; Indexing ;;;;;;;;;;;;
(defstruct posneg pos neg)
(defstruct indexall index all)

(defmethod index (key sent (th metheory))
  "(INDEX SENT TH) when a ME theory, i.e. is in rule form but may
   have negative literals in the heads of rules, index on the signed relations 
   in the heads of rules."
  (index-me key sent th))

(defun index-me (key sent th)
  (declare (ignore key))
  (let ((sr (signed-relation sent)))
    (cond ((atom sr) (index-me-pos-atom sr sent th))
          (t (index-me-neg-atom (cadr sr) sent th)))))

(defmethod index (key sent (th prologtheory))
  "(INDEX SENT TH) when a prologtheory, index SENT on
   relations in the heads of rules (all of which we assume are positive)."
  (index-prolog key sent th))

(defun index-prolog (key sent th)
  (declare (ignore key))
  (indexatom (relation sent) sent th))

(defmethod index (key sent (th modeltheory))
  "(INDEX SENT TH) when a modeltheory, index SENTENCE on the
   relations and their first argument in the heads of rules, 
   all of which we assume are positive."
  (index-model key sent th))

(defun index-model (key sent th)
  (declare (ignore key))
  (let ((sr (signed-relation sent)))
    (cond ((atom sr) (index-model-pos-atom sent sent th))
          (t (index-model-neg-atom (cadr sent) sent th)))))

(defmethod index (key sent (th ddbtheory))
  "(INDEX SENT TH) when a ddbtheory, index SENT according to the following 
    policy.  full indexing for extensional tables.
    relational indexing on the head for intensional tables.
   Assumes the theory's extensional and intensional fields have been assigned 
   the set of predicates that are extensional and intensional, respectively."
  (index-ddb key sent th))

(defun index-ddb (key sent th)
  (if (extensionp (pred (head sent)) th)
    (fullindex key sent th)
    (indexatom (relation sent) sent th)))

(defun index-me-pos-atom (key sent th)
  (when (not (getproperty key th))
    (setproperty key th (make-posneg)))
  (setf (posneg-pos (getproperty key th))
        (fancyendq sent (posneg-pos (getproperty key th)))))

(defun index-me-neg-atom (key sent th)
  (when (not (getproperty key th))
    (setproperty key th (make-posneg)))
  (setf (posneg-neg (getproperty key th))
        (fancyendq sent (posneg-neg (getproperty key th)))))

(defun firstarg (h)
  (setq h (stripnot (head h)))
  (if (listp h) (cadr h) nil))

(defun index-relnarg (key sent th)
  (declare (ignore key))
  (let ((h (head sent)))
    ; initialize entry if it hasn't been already
    (when (not (getproperty (relation h) th))
      (setproperty (relation sent) th (make-indexall :index (make-hash-table))))
    ; index
    (index-indexall-atom (firstarg h) sent (getproperty (relation h) th))))

(defun index-indexall-atom (a sent prop)
  "(INDEX-INDEXALL-ATOM A SENT PROP) takes A, the first argument to SENT,
   SENT, the sentence to be indexed, and PROP, the indexall structure 
   in which to index.  Indexes the sentence on the argument."
  ; add to it to the hash if possible
  (when a
    ; vars and non-atoms sent to '?
    (when (or (not (atom a)) (varp a)) (setq a '?))  
    (setf (gethash a (indexall-index prop))
          (fancyendq sent (gethash a (indexall-index prop)))))
  
  ; always add sentence to the list
  (setf (indexall-all prop)
        (fancyendq sent (indexall-all prop))))

(defun index-model-pos-atom (key sent th)
  (index-model-init key th)
  (index-indexall-atom (firstarg key) sent (posneg-pos
					    (getproperty (relation key) th))))

(defun index-model-neg-atom (key sent th)
  (index-model-init key th)
  (index-indexall-atom (firstarg key) sent (posneg-neg
					    (getproperty (relation key) th))))

(defun index-model-init (key th)
  "(INDEX-MODEL-INIT KEY TH) initialize the indexing structure for
   modeltheory TH and key KEY."
  (let ((prop nil) (r (relation key)))
    (when (not (getproperty r th))
      (setproperty r th (make-posneg)))
    (setq prop (getproperty r th))
    (unless (posneg-pos prop)
      (setf (posneg-pos prop) (make-indexall :index (make-hash-table))))
    (unless (posneg-neg prop)
      (setf (posneg-neg prop) (make-indexall :index (make-hash-table))))))


;; lookup
(defmethod indexps (p (th metheory))
  (cond ((varp p) (contents th))
        (t (indexees p th))))

(defmethod indexees (p (th metheory))
  (let* ((sr (signed-relation p))
         (prop (getproperty (relation sr) th)))
    (cond ((not prop) nil)
          ((atom sr) (car (posneg-pos prop)))
          (t (car (posneg-neg prop))))))


(defun indexeesal (p al th)
  (let* ((r (relation p))
         (prop (getproperty r th))
         (a (plug (firstarg p) al)))
    (cond ((not prop) nil)
          ((atom p) (car (indexall-all prop)))
          (t (indexees-indexall a prop)))))

(defun indexees-indexall (a indexall)
  (cond ((and (atom a) (not (varp a)))
         (car (gethash a (indexall-index indexall))))
        (t (car (indexall-all indexall)))))


(defmethod indexps (p (th modeltheory))
  (cond ((varp p) (contents th))
        (t (indexees p th))))

(defmethod indexees (p (th modeltheory))
  (let* ((r (relation p))
         (prop (getproperty r th))
         (a (firstarg p)))
    (cond ((not prop) nil)
          ((atom p) (car (indexall-all prop)))
          ((negative-literalp p) (indexees-indexall a (posneg-neg prop)))
          (t (indexees-indexall a (posneg-pos prop))))))

(defmethod envindexps (p al (th ddbtheory))
 "(ENVINDEXPS P ENV TH) "
 ; assuming p is an atomic sentence or a variable
 (cond ((varp p) (contents th))
       ((extensionp (pred p) th) (indexees-firstgrnd p al th))
       (t (indexees p th))))

(defmethod indexees (p (th ddbtheory))
  (call-next-method (relation p) th))

(defun indexees-firstgrnd (p al th)
  ; assuming p is an atomic sentence
  (cond ((atom p) (indexees p th))
        (t
         (do ((args (cdr p) (cdr args))
              (ret nil) (val))
             ((or (null args) ret) (if (not ret) 
				       (indexees (car p) th) 
				       (indexees ret th)))
           ;(format t "~A: ~A~%" (car args) (getbdg (car args) al))
           (cond ((groundp (car args))
                  (setq ret (car args)))   
                 ((groundp (setq val (second (getbdg (car args) al))))
                  (setq ret val)))))))
           

;; emptying

(defmethod clearindex (x (th metheory))
  (remproperty (relation x) th))

(defmethod clearindex (x (th prologtheory))
  (remproperty (relation x) th))

(defmethod clearindex (x (th modeltheory))
  (remproperty (relation x) th))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Discrimination tree
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass discrimination-tree (theory)
  ((children :accessor children :initarg :children :initform nil)
   (data :accessor data :initarg :data :initform nil)
   (label :accessor label :initarg :label :initform nil)))

(defvar *bindinglist* nil "global binding-list for use by anyone.")
(defvar *dt-var-count* 0)
(defvar *discrimination-tree-insert-value-function* nil
  "the function called to insert a data value into the appropriate leaf of the discrimination tree.")

(defstruct node-term node term)
(defstruct dtree (tree (make-instance 'discrimination-tree)) symb2index maxkids)

;;; Insertion and lookup where the key is a ground atom without function constants
;;;  -  both the dt-x version and the discrimination-tree-x2 version.
;;; The dt-x version does a better job of encapsulation since the two functions
;;;   returned by the preparation routine in v2 are part of a top-level struct.

(defun dt-prep (vocab tree)
  "(DISCRIMINATION-TREE-PREP VOCAB TREE) prepares the discrimination tree for handling
   ground, atomic, function-free keys.  TREE is a dtree struct."
  (let (symb2index maxkids)
    (multiple-value-setq (symb2index maxkids)
      (discrimination-tree-prep2 vocab (dtree-tree tree)))
    (setf (dtree-maxkids tree) maxkids)
    (setf (dtree-symb2index tree) symb2index)
    tree))
 
(defun dt-insert (key value tree)
  "(DISCRIMINATION-TREE-INSERT KEY VALUE TREE) assumes key is a ground
   atom without function constants.  Inserts into the discrimination tree TREE the key KEY with
   value VALUE."
  (discrimination-tree-insert2 key value (dtree-tree tree) 
			       (dtree-maxkids tree) (dtree-symb2index tree)))

(defun dt-lookup (key tree)
  "(DT-LOOKUP KEY TREE) finds the node corresponding to KEY, if it exists
   and returns the value at that node. Assumes key is a ground atom without function constants."
  (dt-lookup2 key (dtree-tree tree) (dtree-symb2index tree)))


;;;;;;;;;;; Version where TREE is just a discrimination-tree class instance.
(defun discrimination-tree-prep2 (vocab tree)
  "(DISCRIMINATION-TREE-PREP VOCAB TREE) prepares the discrimination tree for handling
   ground, atomic, function-free keys.  Returns the symbol2index function and the maxkids value."
  (let (objs relns prop tmp)
    ; prepare the function for mapping a constant to a number
    (setq objs (remove-if-not #'isobject vocab))
    (setq relns (remove-if-not #'isrelation vocab))
    (setq prop 'tlhdcsymb2index)

    (setq tmp 0)
    (dolist (v objs)
      (setf (get (parameter-symbol v) prop) tmp)
      (setq tmp (1+ tmp)))
  
    (setq tmp 0)
    (dolist (v relns)
      (setf (get (parameter-symbol v) prop) tmp)
      (setq tmp (1+ tmp)))
  
    ; ensure the root node has 1 child for each of the relation constants
    (discrimination-tree-force-children2 tree (length relns))

    (values #'(lambda (x) (get x prop)) (length objs))))


(defun discrimination-tree-insert2 (key value tree maxkids symb2index)
  "(DISCRIMINATION-TREE-INSERT2 KEY VALUE TREE MAXKIDS SYMB2INDEX) assumes key is a ground
   atom without function constants.  Inserts into the discrimination tree TREE the key KEY with
   value VALUE.  Assumes SYMB2INDEX is a function that given an object constant returns an integer
   from 0 to maxkids-1."

  (let (lastarg)
    (when (atom key) (setq key (list key)))

    ; ensure the structure is there, except for the last obj constant
    (do ((ks key (cdr ks)))
        ((null (cdr ks)) (setq lastarg (car ks)))
      (discrimination-tree-force-children2 tree maxkids)
      (setq tree (aref (children tree) (funcall symb2index (car ks))))
      (unless (label tree) (setf (label tree) (car ks))))

    ; add the children if necessary for the last tree
    (discrimination-tree-force-children2 tree maxkids)
    (unless (label (aref (children tree) (funcall symb2index lastarg))) 
      (setf (label (aref (children tree) (funcall symb2index lastarg))) lastarg))

    ; insert the value at the appropriate child
    (discrimination-tree-set-value value (aref (children tree) (funcall symb2index lastarg)))))

(defun discrimination-tree-force-children2 (tree maxkids)
  (unless (arrayp (children tree)) 
    (setf (children tree) (make-array maxkids))
    (dotimes (i maxkids) 
      (setf (aref (children tree) i) (make-instance 'discrimination-tree)))))

(defun dt-lookup2 (key tree symb2index)
  "(DT-LOOKUP2 KEY TREE SYMB2INDEX) finds the node corresponding to KEY, if it exists
   and returns the value at that node, using SYMB2INDEX to compute which child a particular
   object constant corresponds to.  Assumes key is a ground atom without function constants."

  (when (atom key) (setq key (list key)))
  (dolist (k key (data tree))
    (cond ((not (children tree)) (return nil))
          (t
           (setq tree (aref (children tree) (funcall symb2index k)))))))
 
;;; Insertion is straightforward as we can compute a canonical version of any
;;;    sentence with variables (on the fly).  The first variable encountered is
;;;    assigned ?dtvar_1, the second ?dtvar_2, and so on, except that we need to
;;;    keep these assignments in a binding list and pass it around.  Only assign
;;;    a new variable if a new variable hasn't already been assigned.

(defun discrimination-tree-insert (key value tree)
  (let ((*bindinglist* (environment))
        (*dt-var-count* 0))
    (discrimination-tree-insert-aux key value tree t)))

(defun discrimination-tree-insert-aux (key value tree insert-valuep)
  "(DISCRIMINATION-TREE-INSERT KEY VALUE TREE INSERT-VALUEP) inserts VALUE into TREE using the
   key KEY when INSERT-VALUEP is true; otherwise, just finds node at which VALUE should
   be inserted.  Destructively creates path in tree if it does not exist."
  (cond ((atom key) (assert (or (symbolp key) (numberp key)))
         (let ((kid (discrimination-tree-force-child key tree)))
           (when insert-valuep (discrimination-tree-set-value value kid))
           kid))
        (t
         ; loop over items in the list, locating the place in the tree to insert
         ;   a new value.  Notice that we should only be adding a value at the very end,
         ;   which we use insert-value to keep track of.
         ; Also set data of each internal node to point to the last node of the term
         (do ((es key (cdr es))
              (node tree)
              (lastnode nil))
             ((null es) node)   
           (setq node 
                 (discrimination-tree-insert-aux (car es) 
                                                 value 
                                                 node
                                                 (and (null (cdr es)) insert-valuep)
                                                 ))
           ;(when (and lastnode (listp (car es))) (adjoin-datad node lastnode))
           (when lastnode (adjoin-datad (make-node-term :node node :term (bind-flat (car es) *bindinglist*)) lastnode :key #'node-term-node))
           (setq lastnode node)) )))

(defun discrimination-tree-force-child (symbol tree)
  "(DISCRIMINATION-TREE-FORCE-CHILD SYMBOL TREE) treats TREE as a discrimination tree and SYMBOL
   as a symbol.  Extends tree (if necessary) and returns node corresponding to SYMBOL.
   Treats variables as named variables."
  (let ((bnd symbol) (node nil) (newbdgp nil))
    (when (varp symbol)   
      (setq bnd (binding symbol *bindinglist*))

      ; if existent binding, use the binding instead of the symbol
      ; if non-existent binding, use current value for *dt-var-count*
      ;    then add binding to list
      (when (not bnd) 
        (setq bnd (newdtvar) newbdgp t)) )

    (setq node (discrimination-tree-force-child-aux bnd tree))

    (when newbdgp (setnewbdg symbol *bindinglist* (label node) (environment)))
    node))

(defun discrimination-tree-force-child-aux (symbol tree)
  "(DISCRIMINATION-TREE-CHILD SYMBOL TREE) treats children of TREE as a list and SYMBOL
   as a symbol.  Does not treat variables specially. "
  (let ((n nil))
    (setq n (member symbol (children tree) :key #'label))
    (cond (n (car n))  ; if n exists, member returns list with it as first element 
          (t
           (setf (children tree) 
                 (cons (make-instance 'discrimination-tree  :label symbol)
                       (children tree)))
           (car (children tree))))))

(defun newdtvar () 
  "(NEWDTVAR) returns a new DTVAR; next time will return the next dtvar.  
   To restart at 0, assign *dt-var-count* to 0."
  (setq *dt-var-count* (1+ *dt-var-count*))
  (makevariable (format nil "dt~A" (1- *dt-var-count*))))

(defun bind-flat (p al)
  "(BIND-FLAT P AL) returns p after replacing variables in P according to the first
   level of binding in AL."
  (cond ((varp p) (let ((b (binding p al))) (if b b p)))
        ((atom p) p)
        (t
         (mapcar #'(lambda (x) (bind-flat x al)) p))))
(defun discrimination-tree-variable-children (tree)
  "(DISCRIMINATION-TREE-VARIABLE-CHILDREN TREE) returns all the children of TREE that
   are variables."
  (remove-if-not #'(lambda (x) (varp (label x))) (children tree)))

(defun discrimination-tree-set-data (value tree)
  "(DISCRIMINATION-TREE-SET-DATA VALUE TREE) sets the data value of TREE to VALUE."
  (setf (data tree) value))

(defun adjoin-datad (x dt &key (key #'identity))
  "(ADJOIN-DATAD X DT) adds X to (data DT) if X is not already a member."
  (when (not (member (funcall key x) (data dt) :key key)) 
    (setf (data dt) (cons x (data dt)))))


(defun discrimination-tree-set-value (value tree)
 "(DISCRIMINATION-TREE-ADD-VALUE VALUE TREE)
  adds sets children of TREE to NIL and sets its value to VALUE."
  (setf (children tree) nil)
  (funcall *discrimination-tree-insert-value-function* value tree))

(defun discrimination-tree-find-child (symbol tree)
  "(DISCRIMINATION-TREE-FIND-CHILD SYMBOL TREE) finds the child of TREE corresponding
   to SYMBOL.  Returns NIL if none exist."
  (find symbol (children tree) :key #'label))

(setq *discrimination-tree-insert-value-function* #'discrimination-tree-set-data)

(defun dtp (tree &optional (childrenop #'identity)) (discrimination-tree-walk tree 0 #'discrimination-node-print childrenop))
(defun discrimination-tree-walk (tree depth nodeop &optional (childrenop #'identity))
  "(DISCRIMINATION-TREE-WALK TREE DEPTH NODEOP CHILDRENOP) walks the TREE depth-first, 
    treating the children of a node as the list of children returned by CHILDRENOP
    when given the original children, incrementing DEPTH at each level, while applying
    NODEOP to each node, leaf and non-leaf alike.  NODEOP is called with (NODEOP tree depth)."
  (funcall nodeop tree depth)
  (cond ((null (children tree)))
        (t
         (mapc #'(lambda (x) (discrimination-tree-walk x (1+ depth) nodeop childrenop))
                 (funcall childrenop (children tree)))))
  t)

(defun discrimination-node-print (node depth) (discrimination-node-print-s node depth t))
(defun discrimination-node-print-s (node depth s)
  "(DISCRIMINATION-NODE-PRINT-S NODE DEPTH S) prints the current node at depth DEPTH
    to stream S."
  (indent depth s #'(lambda (x) (if (< x depth) "|" (label node))))
  (cond ((children node)
         (format s "~A~A" 
                 (if (data node) " -> " "")
                 (if (data node) (mapcar #'node-term-term (data node)) "")))
        (t
         (format s ": ~A" (data node))))
  (format s "~&"))


;;;;;;;;;; Discrimination-tree lookup routines.

;; SPEED UP: take p and al instead of just key, and do the plugging as we go.  This will avoid
;;    touching the entire sentence if we can avoid it.
(defun dt-samex (key tree)
  "(DT-SAMEX P DT) returns the data element for key P if it exists and 
   NIL otherwise, where DT is a discrimination tree."
  (let* ((*bindinglist* (environment))
         (*dt-var-count* 0)
         (node (dt-samex-aux key tree)))
    (if node
      (data node)
      nil)))

(defun dt-samex-aux (key tree)
  (declare (notinline dt-samex-aux))
  (cond ((varp key)
         (let ((bnd key) (node nil) (newbdgp nil))
           (setq bnd (binding key *bindinglist*))
           
           ; if existent binding, use the binding instead of the symbol
           ; if non-existent binding, use current value for *dt-var-count*
           ;    then add binding to list
           (when (not bnd) 
             (setq bnd (newdtvar) newbdgp t)) 
           (setq node (discrimination-tree-find-child bnd tree))
           (when (and node newbdgp) (setnewbdg key *bindinglist* (label node) (environment)))
           node))

        ((atom key) 
         (discrimination-tree-find-child key tree))

        (t
         ; loop over items in the list, locating the place in the tree corresponding
         ;   to the end of each item. 
         (do ((es key (cdr es))
              (node tree))
             ((null es) node)   
           (setq node (dt-samex-aux (car es) node))
           (if (not node) (return nil))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
