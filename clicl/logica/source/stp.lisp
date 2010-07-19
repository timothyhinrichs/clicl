;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; (c) Copyright 1991 Michael R. Genesereth;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; *transforms*;;; *rules*;;; *eliminations*;;; *subsumptions*;;; *methods*;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; (mgu <expression> <expression>);;;;;; (cnf <sentence>);;; (dnf <sentence>);;; (brf <sentence>);;; (frf <sentence>);;;;;; (clauses <sentence>);;;;;; (apply-transforms <transforms> <database>);;; (apply-rules <rules> <database>);;; (apply-eliminations <eliminations> <database>);;;;;; (prove <database> <sentence>;;;   &key schemata transforms rules eliminations subsumptions method);;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;(proclaim '(special *lisp*                    *doschemata* *dotransforms* *dorules*                    *doeliminations* *dosubsumptions*))(defun apply-transforms (transforms data)  (do ((l data (cdr l)) (nl))      ((null l) (nreverse nl))      (do ((r transforms (cdr r)))          ((null r))          (setq nl (cons (funcall (car r) (car l)) nl)))))(defun apply-rules (rules data)  (do ((l data (cdr l)) (nl))      ((null l) (nreverse nl))      (do ((m data (cdr m)))          ((null m))          (do ((r rules (cdr r)))              ((null r))              (setq nl (nreconc (funcall (car r) (car l) (car m)) nl))))))(defun apply-eliminations (eliminations data)  (do ((l data (cdr l)) (nl))      ((null l) (nreverse nl))      (do ((r eliminations (cdr r)))          ((null r) (setq nl (cons (car l) nl)))          (if (funcall (car r) (car l)) (return t)))))(defun prove (data goal &key              (schemata)              (transforms '(factor))              (rules '(resolution))              (eliminations '(tautology))              (subsumptions '(subsumes))              (method 'bf))  (let ((*lisp* t)        (*doschemata* schemata)        (*dotransforms* transforms)        (*dorules* rules)        (*doeliminations* eliminations)        (*dosubsumptions* subsumptions))    (funcall method (cons `(or (not ,goal) goal) data))))(defclass proof ()  ((sentences :accessor sentences :initarg :sentences :initform nil)))(defclass sentence () ((name  :accessor name  :initarg :name  :initform nil)  (form  :accessor form  :initarg :form  :initform nil)  (justs :accessor justs :initarg :justs :initform nil)  (users :accessor users :initarg :users :initform nil)));;; A justification is a list consisting of a rule of inference and premises.(defparameter s1  (make-instance 'sentence :name 's1 :form '(p a)))(defparameter s2  (make-instance 'sentence :name 's2 :form '(=> (p a) (q a))))(defparameter s3  (make-instance 'sentence :name 's3 :form '(q a)))(setf (users s1) (list s3))(setf (users s2) (list s3))(setf (justs s3) (list (list 'mp s1 s2)))(defparameter *proof*  (make-instance 'proof    :sentences (list s1 s2 s3)))(defun printproof (proof f)  (do ((l (sentences proof) (cdr l)))      ((null l) 'done)      (fresh-line f)      (princ (name (car l)) f) (princ ": " f) (princ (form (car l)) f)));;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;