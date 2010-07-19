;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; educational.lisp -- routines for helping to teach logic
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Naive search for first-order models
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defstruct db tree symb2index maxkids dca)

(defun incompletep (reln p &optional (maxsize 100))
  "(DETECT-INC RELN P MAXSIZE) checks whether there is some instantiation
   of all the relations in P except RELN so that RELN is incomplete; i.e.
   whether there are two models that are the same for all those relations
   but different wrt RELN."
  (let (vocab base univ db oldmodel newmodel)
    (setq vocab (get-vocabulary p))
    (setq base (remove reln (remove-if-not #'isrelation vocab) 
		       :key #'parameter-symbol))

    ; iterate from universe of size 1 to maxsize
    (setq univ nil)
    (dotimes (i maxsize)

      ; need a larger universe by one since last iteration
      (setq univ (cons (gentemp "a") univ))

      ; walk over all the possible models of the base relations
      (dolist (m (possible-models base univ))
        
        ; for each one, check whether there are two extensions for RELN 
	;   that both satisfy P.
        (dolist (r (possible-models (list reln) univ))
          (setq newmodel (nconc r m))
          (setq db (model2db newmodel))

          ; if the sentence is satisfied, if this is the second one, exit; 
	  ;   otherwise, remember this model
          (when (dblookup p db) 
            (cond (oldmodel
                   (format t "Detected incompleteness~%")
                   (format t "Model 1~%")
                   (dolist (v oldmodel) (print v))
                   (format t "Model 2~%")
                   (dolist (v newmodel) (print v)))
                  (t 
                   (setq oldmodel newmodel)))))))
    (values oldmodel newmodel)))

(defun possible-models (rs univ)
  (subsets (mapcan #'(lambda (x) (saturate (parameter-symbol x) 
					   (parameter-arity x) univ)) rs)))

(defun dblookup (p db)
  "(DBLOOKUP P DB) returns T iff the sentence closed sentence P is satisfied by
   the database DB."
  (cond ((atom p) nil)
        ((eq (car p) 'and) (every #'(lambda (x) (dblookup x db)) (cdr p)))
        ((eq (car p) 'or) (some #'(lambda (x) (dblookup x db)) (cdr p)))
        ((eq (car p) 'not) (not (dblookup (second p) db)))
        ((eq (car p) '<=) 
	 (dblookup (makor (second p) (maknot (maksand (cddr p)))) db))
        ((eq (car p) '=>) (dblookup (makor (maknot (maksand (butlast (cdr p)))) 
					   (car (last p))) db))
        ((eq (car p) '<=>)
         (let (r1 r2)
           (setq r1 (dblookup (second p) db))
           (setq r2 (dblookup (third p) db))
           (or (and r1 r2)
               (and (not r1) (not r2)))))
        ((member (car p) '(forall exists)) (ground p (db-dca db) nil))
        (t (dt-lookup2 p (db-tree db) (db-symb2index db)))))

(defun model2db (model)
  "(MODEL2DB MODEL) takes a list of function-free ground atoms MODEL and 
   returns a lisp object that represents that model."
  (let (tree symb2index maxkids dca vocab)
    (setq tree (make-instance 'discrimination-tree))
    (setq vocab (get-vocabulary (maksand model)))
    (multiple-value-setq (symb2index maxkids) 
      (discrimination-tree-prep2 vocab tree))
    (dolist (a model)
      (discrimination-tree-insert2 a t tree maxkids symb2index))
    (setq dca (mapcar #'parameter-symbol (remove-if-not #'isobject vocab)))
    (make-db :tree tree :maxkids maxkids :symb2index symb2index :dca dca)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Propositional Truth Tables
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun enum-theories (ps)
  "(ENUM-THEORIES PS) takes a set of propositional constants and enumerates
   all subsets of all subsets of PS.  Obviously doubly exponential."
  (subsets (subsets ps)))

(defun ttfindp (p th)
  "(TTFINDP P TH) takes a propositional sentence and a propositional theory.
   It uses the truth-table method to determine whether TH |= P, i.e.
   it enumerates all models in the language and then checks whether TH => P
   in each one."
  (every #'(lambda (x) (sat x (list '<= p (maksand (contents th))))) 
         (subsets (mapcar #'parameter-symbol (get-vocabulary (maksand (contents th)))))))

(defun sat (m p)
  (cond ((atom p) (member p m))
        ((eq (car p) 'not) (not (sat m (second p))))
        ((eq (car p) 'and) (every #'(lambda (x) (sat m x)) (cdr p)))
        ((eq (car p) 'or) (some #'(lambda (x) (sat m x)) (cdr p)))
        ((eq (car p) '<=) (sat m `(or ,(second p) ,(maknot (maksand (cddr p))))))
        ((eq (car p) '=>) (sat m `(or ,(maknot (maksand (butlast (cdr p)))) ,(car (last p)))))
        ((eq (car p) '<=>) (sat m `(and (<= ,(second p) ,(third p)) (=> ,(second p) ,(third p)))))
        (t nil)))

(defun enum-sentences (ps)
  (declare (ignore ps))
  '(p (not p) q (not q) (or p q) (or (not p) q) (or p (not q)) (or (not p) (not q))
    (and p q) (and p (not q)) (and (not p) q) (and (not p) (not q)) 
    (and (or p q) (or (not p) (not q))) (and (or (not p) q) (or p (not q)))
    (or p (not p)) (and p (not p))))

(defun compute-entailed-per-theory (ths ps)
  (let ((results nil) (cnt 0))
    (dolist (h ths results)
      (setq cnt 0)
      (dolist (p ps)
        (when (every #'(lambda (x) (sat x p)) h) (setq cnt (1+ cnt))))
      (push (cons h cnt) results))))

#|
? (setq r (compute-entailed-per-theory (enum-theories '(p q)) (enum-sentences nil)))
((NIL . 16) ((NIL) . 8) (((Q)) . 8) (((Q) NIL) . 4) (((P)) . 8) (((P) NIL) . 4) (((P) (Q)) . 4) (((P) (Q) NIL) . 2) (((P Q)) . 8) (((P Q) NIL) . 4) (((P Q) (Q)) . 4) (((P Q) (Q) NIL) . 2) (((P Q) (P)) . 4) (((P Q) (P) NIL) . 2) (((P Q) (P) (Q)) . 2) (((P Q) (P) (Q) NIL) . 1))
? (sort r #'< :key #'(lambda (x) (length (car x))))
((NIL . 16) ((NIL) . 8) (((Q)) . 8) (((P)) . 8) (((P Q)) . 8) (((Q) NIL) . 4) (((P) NIL) . 4) (((P) (Q)) . 4) (((P Q) NIL) . 4) (((P Q) (Q)) . 4) (((P Q) (P)) . 4) (((P) (Q) NIL) . 2) (((P Q) (Q) NIL) . 2) (((P Q) (P) NIL) . 2) (((P Q) (P) (Q)) . 2) (((P Q) (P) (Q) NIL) . 1))
? (dolist (v *) (print v))

(NIL . 16) 
((NIL) . 8) 
(((Q)) . 8) 
(((P)) . 8) 
(((P Q)) . 8) 
(((Q) NIL) . 4) 
(((P) NIL) . 4) 
(((P) (Q)) . 4) 
(((P Q) NIL) . 4) 
(((P Q) (Q)) . 4) 
(((P Q) (P)) . 4) 
(((P) (Q) NIL) . 2) 
(((P Q) (Q) NIL) . 2) 
(((P Q) (P) NIL) . 2) 
(((P Q) (P) (Q)) . 2) 
(((P Q) (P) (Q) NIL) . 1) 
NIL

|#



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Logica
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defparameter *res-stream* t)

(defparameter *res-subsumption* t)

(defparameter *res-tautology* t)

(defparameter *res-unit* nil)

(defparameter *res-input* nil)

(defparameter *res-support* nil)

(defparameter *res-ordered* nil)

(defclass conclusion ()
  ((sequence :accessor sequence :initarg :sequence :initform 1)
   (sentence :accessor sentence :initarg :sentence :initform 'false)
   (positive :accessor positive :initarg :positive :initform nil)
   (negative :accessor negative :initarg :negative :initform nil)))

(defun makeconclusion (s p n)
  (make-instance 'conclusion :sentence s :positive p :negative n))

(defun makclauses (p)
  (mapcar #'(lambda (x) (cons 'or x)) (clausesets p)))
#|
(defun output-proof (s proof)
  (format-proof s (convertproof proof)))
|#

(defun convertproof (proof)
  (do ((l proof (cdr l)) (step 1 (1+ step)) (dum) (nl))
      ((null l) (nreverse nl))
      (setf (sequence (car l)) step)
      (cond ((symbolp (positive (car l)))
             (setq dum (list step (sentence (car l)) (positive (car l)))))
            (t (setq dum (list step (sentence (car l))
                               (sequence (positive (car l)))
                               (sequence (negative (car l)))))))
      (setq nl (cons dum nl))))

(defun pruneproof (proof)
  (markproof (car (last proof)))
  (do ((l proof (cdr l)) (nl))
      ((null l) (nreverse nl))
      (when (equal (sequence (car l)) 0) (setq nl (cons (car l) nl)))))

(defun markproof (node)
  (cond ((equal (sequence node) 0))
        (t (setf (sequence node) 0)
           (when (typep (positive node) 'conclusion)
             (markproof (positive node))
             (markproof (negative node))))))

(defun resolution-closure-snark (th end)
  (let ((thcl nil) newth neweq tmp r)
    (setq neweq '__tlheq)
    (setq newth (mapcar #'(lambda (x) (subst neweq '= x)) (contents th)))
    (setq newth (cons `(,neweq ?x ?x) newth))
    (snark:initialize :verbose nil)
    (snark:use-resolution t)
    (snark:print-summary-when-finished nil)
    (snark:print-options-when-starting nil)
    (snark:print-rows-when-derived nil)
    (snark:print-assertion-analysis-notes nil)
    (snark:print-final-rows nil)
    (mapc #'snark::assert newth)
    (snark:closure :number-of-rows-limit end)
    (dotimes (i snark::*number-of-rows*)
      (setq r (snark::row (1+ i)))
      ; r is nil if row was subsumed
      (when r
        (setq tmp (snark-to-kif (snark::row-wff r))))
        (unless (and (listp tmp) (eq (car tmp) neweq))
          (setq thcl (cons tmp thcl))))
    (mapcar #'(lambda (x) (subst '= neweq x)) (nreverse thcl))))

(defun snark-to-kif (p)
  (cond ((eq p snark-lisp:false) '(or))
        ((snark::constant-p p) (if (symbolp p) p (snark::function-name p)))
        ((snark::variable-p p)  (read-from-string (format nil "?~A" (snark::variable-number p))))
        ((listp p) (mapcar #'snark-to-kif p))
        ((snark::compound-appl-p p) (cons (snark-to-kif (snark::compound-appl-head p))
                                          (snark-to-kif (snark::compound-appl-args p))))
        (t (assert nil nil "Don't know how to convert to KIF."))))

#|
(deftheory ww 
  (or (gold a) (gold b) (gold c))
  (not (gold d))
  (not (gold e))
  (not (gold f))
  (or (not (gold ?x)) (not (gold ?y)) (= ?x ?y)))
|#

(defun resolution-closure (premises end)
  (mapcar #'second (convertproof (logica premises nil end))))

(defun logica (premises conclusions end)
  (do ((l premises (cdr l)) (nl))
      ((null l) (setq premises (nreverse nl)))
      (setq nl (cons (makeconclusion (car l) 'premise nil) nl)))
  (do ((l conclusions (cdr l)) (nl))
      ((null l) (setq conclusions (nreverse nl)))
      (setq nl (cons (makeconclusion (car l) 'goal nil) nl)))
  (cond (*res-input* (runinput premises conclusions end))
        (t (runregular premises conclusions end))))

(defun runinput (premises conclusions end)
  (setq premises (nconc premises conclusions))
  (unless *res-support* (setq conclusions premises))
  (do ((l premises (cdr l)) (copy (copy-list premises))
       (flag) (step (length premises)))
      ((null l) premises)
      (when (or flag (>= step end)) (return premises))
      (do ((m copy (cdr m)))
          ((null m))
          (when (or flag (>= step end)) (return t))
          (dolist (result (newresolve (car l) (car m)))
            (unless (find (sentence result) premises :key #'sentence :test #'res-similarp)
              (setq premises (nconc premises (list result)))
              (setq step (1+ step)))
            (cond ((equal (sentence result) '(or)) (setq flag t) (return t))
                  ((>= step end) (return t)))))))

(defun runregular (premises conclusions end)
  (setq premises (nconc premises conclusions))
  (unless *res-support* (setq conclusions premises))
  (do ((l conclusions (cdr l)) (flag) (step (length premises)))
      ((null l) premises)
      (when (or flag (>= step end)) (return premises))
      (do ((m premises (cdr m)))
          ((eq m (cdr l)))
          (when (or flag (>= step end)) (return t))
          (dolist (result (newresolve (car l) (car m)))
            (unless (find (sentence result) premises :key #'sentence :test #'res-similarp)
              (setq premises (nconc premises (list result)))
              (setq step (1+ step)))
            (cond ((equal (sentence result) '(or)) (setq flag t) (return t))
                  ((>= step end) (return t)))))))

(defun newresolve (p q)
  (let (results)
    (cond ((and *res-unit* (not (or (unitp (sentence p)) (unitp (sentence q)))))
           (setq results nil))
          (*res-ordered* (setq results (newordered (sentence p) (sentence q))))
          (t (setq results (newresolution (sentence p) (sentence q)))))
    (do ((l results (cdr l)) (nl))
        ((null l) (nreverse nl))
        (setq nl (cons (makeconclusion (car l) q p) nl)))))

(defun newresolution (p q)
  (do ((l (cdr p) (cdr l)) (al (environment)) (bl (environment)) (result) (nl))
      ((null l) (nreverse nl))
      (do ((m (cdr q) (cdr m)))
          ((null m))
          (when (unify (maknot (car m)) bl (car l) al)
            (setq result (newresolvent (remove (car m) (cdr q) :test #'eq) bl
                                       (remove (car l) (cdr p) :test #'eq) al))
            (setf (alist al) nil)
            (setf (alist bl) nil)
            (cond ((and *res-tautology* (res-tautologyp result)))
                  (t (setq nl (adjoin result nl :test #'equal))))))))

(defun newresolvent (x alist y bl)
  (setq x (newplugstdexp x alist) y (newplugstdexp y bl))
  (cons 'or (unionize x y)))

(defun newordered (p q)
  (let ((al (environment)) (bl (environment)) result)
    (cond ((null (cdr p)) nil)
          ((null (cdr q)) nil)
          ((unify (maknot (cadr q)) bl (cadr p) al)
           (setq result (newresolvent (remove (cadr q) (cdr q) :test #'eq) bl
                                      (remove (cadr p) (cdr p) :test #'eq) al))
           (setf (alist al) nil)
           (setf (alist bl) nil)
           (cond ((and *res-tautology* (res-tautologyp result)) nil)
                 (t (list result)))))))

(defun enderton (p q)
  (do ((l (cdr p) (cdr l)) (result) (nl))
      ((null l) (nreverse nl))
      (do ((m (cdr q) (cdr m)))
          ((null m))
          (when (equal (car l) (maknot (car m)))
            (setq result (maksor (unionize (remove (car l) (cdr p) :test #'eq)
                                           (remove (car m) (cdr q) :test #'eq))))
            (cond ((and *res-tautology* (res-tautologyp result)))
                  (t (setq nl (adjoin result nl :test #'equal))))))))


(defun newplugstdexp (x al)
  (cond ((indvarp x) (newplugstdexpindvar x al))
	((atom x) x)
	((eq 'quote (car x)) x)
	(t (newplugstdexpcdr x al))))

(defun newplugstdexpcdr (x al)
  (do ((l x (cdr l)) (nl))
      ((null l) (nreverse nl))
      (if (seqvarp (car l))
          (return (nreconc nl (newplugstdexpseqvar (car l) al)))
          (setq nl (cons (newplugstdexp (car l) al) nl)))))

(defun newplugstdexpindvar (x al)
  (let (dum)
    (cond ((eq '?* x) '?*)
          ((setq dum (getbdg x al))
           (cond ((cddr dum) (newplugstdexp (cadr dum) (cddr dum)))
                 ((eq alist al) x)
                 ((cdr dum) (cadr dum))
                 (t (setoldbdg dum (gentemp "?") alist))))
          ((eq alist al) x)
	  (t (setnewbdg x al (gentemp "?") alist)))))

(defun newplugstdexpseqvar (x al)
  (let (dum)
    (cond ((eq '@* x) (list x))
          ((setq dum (getbdg x al))
           (cond ((cddr dum) (newplugstdexpcdr (cadr dum) (cddr dum)))
                 ((eq alist al) (list x))
                 ((cdr dum) (cadr dum))
                 (t (setoldbdg dum (list (gentemp "@")) nil))))
          ((eq alist al) (list x))
          (t (setnewbdg x al (list (gentemp "@")) nil)))))


(defun runit (premises step end)
  (do ((l premises (cdr l)) (i 1 (1+ i)))
      ((or (null l) (>= step end)))
      (do ((m premises (cdr m)) (j 1 (1+ j)))
          ((or (eq m (cdr l)) (>= step end)))
          (dolist (result (resolve (car l) (car m)))
            (cond ((>= step end))
                  ((find result premises :test #'equalp))
                  (t (setq premises (nconc premises (list result)))
                     (format-step *res-stream* (incf step) result (format nil "~A, ~A" j i))))))))

(defun format-step (s num step just)
  (let ((*print-case* :downcase))
    (format s "<TR><TH HALIGN=RIGHT>~A.</TH>" num)
    (format s "<TD><TT>~A</TT></TD>" step)
    (format s "<TD>~A</TD></TR>" just)))

(defun resolve (p q)
  (unless (and (listp p) (eq 'or (car p))) (setq p (list 'or p)))
  (unless (and (listp q) (eq 'or (car q))) (setq q (list 'or q)))
  (cond ((and *res-unit* (not (or (unitp p) (unitp q)))) nil)
        (*res-ordered* (ordered p q))
        (t (resolution p q))))

(defun resolution (p q)
  (setq q (stdize q))
  (do ((l (cdr p) (cdr l)) (al) (result) (nl))
      ((null l) (nreverse nl))
      (do ((m (cdr q) (cdr m)))
          ((null m))
          (when (setq al (mgu (car l) (maknot (car m))))
            (setq result (maksor (unionize (remove (car l) (cdr p) :test #'equal)
                                           (remove (car m) (cdr q) :test #'equal))))
            (setq result (decolonize (plug result al)))
            (cond ((and *res-tautology* (res-tautologyp result)))
                  (t (setq nl (adjoin result nl :test #'equalp))))))))

(defun ordered (p q)
  (let (al result)
    (unless (and (listp p) (eq 'or (car p))) (setq p (list 'or p)))
    (unless (and (listp q) (eq 'or (car q))) (setq q (list 'or q)))
    (setq q (stdize q))
    (cond ((null (cdr p)) nil)
          ((null (cdr q)) nil)
          ((setq al (mgu (cadr p) (maknot (cadr q))))
           (setq result (maksor (unionize (cddr p) (cddr q))))
           (setq result (decolonize (plug result al)))
           (cond ((and *res-tautology* (res-tautologyp result)) nil)
                 (t (list result)))))))

(defun res-tautologyp (x)
  (cond ((atom x) nil)
        ((eq 'not (car x)) nil)
        ((eq 'or (car x))
         (do ((l (cdr x) (cdr l)))
             ((null l) nil)
             (when (find (maknot (car l)) (cdr l) :test #'equalp)
               (return t))))))

(defun unitp (x)
  (cond ((atom x))
        ((eq 'not (car x)))
        ((eq 'or (car x)) (null (cddr x)))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Miscellaneous
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun res-similarp (x y)
  (cond (*res-subsumption* (res-subsumptionp y x))
        (t (samep x y))))

(defun samesetp (x y)
  (and (samesubsetp (cdr x) (cdr y) truth) (samesubsetp (cdr y) (cdr x) truth)))

(defun samesubsetp (x y al)
  (cond ((null x) al)
        (t (do ((m y (cdr m)) (bl))
               ((null m) nil)
               (when (and (setq bl (samepexp (car x) (car m) al))
                          (samesubsetp (cdr x) y bl))
                 (return bl))))))

(defun res-subsumptionp (p q)
  (res-subsumptionps (cdr p) (cdr q) truth))

(defun res-subsumptionps (pl ql al)
  (cond ((null pl) al)
        (t (do ((m ql (cdr m)) (bl))
               ((null m))
               (if (and (setq bl (matchpexp (car pl) (car m) al))
                        (setq bl (res-subsumptionps (cdr pl) ql bl)))
                   (return bl))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
