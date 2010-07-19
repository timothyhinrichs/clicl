;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; (c) Copyright 2005-2008 by Michael Genesereth.  All rights reserved.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; viewserver.lisp
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; viewserver
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass viewserver (agent) ())

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Knowledge commands
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; affirm, retract
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod affirm (p sender (receiver viewserver))
  (declare (ignore sender))
  (save p receiver))

(defmethod retract (p sender (receiver viewserver))
  (declare (ignore sender))
  (drop p receiver))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; askabout
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod askabout (x sender (receiver viewserver))
  (declare (ignore sender))
  (rules x receiver))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Data commands
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; revise, revisions, errors, materializations, notifications, reactions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod revise (p sender (receiver viewserver))
  (declare (ignore sender))
  (let (revisions materializations reactions)
    (setq revisions (revisions p receiver) p (maksand revisions))
    (cond ((errors p receiver))
          (t (setq materializations (materializations p receiver))
             (setq reactions (reactions p receiver))
             (dolist (p revisions) (change p receiver))
             (dolist (p materializations) (change p receiver))
             (dolist (dum reactions) (eval dum))
             'done))))

(defmethod revisions (p (receiver viewserver))
  (let (temp pat answers)
    (setq temp (make-instance 'theory))
    (insertplusminus p temp)
    (includes temp receiver)
    (dolist (r (supports p receiver))
      (setq pat `(,r @l))
      (setq answers (nconc answers (viewfinds `(not ,pat) `(minus ,pat) temp)))
      (setq answers (nconc answers (viewfinds pat `(plus ,pat) temp))))
    (decludes temp)
    (empty temp)
    answers))

(defmethod errors (p (receiver viewserver))
  (let (temp answers)
    (setq temp (make-instance 'theory))
    (insertposneg p temp)
    (includes temp receiver)
    (setq answers (viewfinds '?x '(error ?x) temp))
    (decludes temp)
    (empty temp)
    answers))

(defmethod materializations (p (th viewserver))
  (declare (ignore p th))
  nil)

(defmethod reactions (p (receiver viewserver))
  (let (temp answers)
    (setq temp (make-instance 'theory))
    (insertposneg p temp)
    (includes temp receiver)
    (setq answers (viewfinds '?x '(trigger ?x) temp))
    (decludes temp)
    (empty temp)
    answers))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; askp, askx, asks
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod askp (p sender (th viewserver))
  (declare (ignore sender))
  (viewfindp p th))

(defmethod askx (x p sender (th viewserver))
  (declare (ignore sender))
  (viewfindx x p th))

(defmethod asks (x p sender (th viewserver))
  (declare (ignore sender))
  (viewfinds x p th))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; askclass, askframe, asktable
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod askclass (x sender (receiver viewserver))
  (declare (ignore sender))
  (classify x receiver))

(defmethod askframe (x class sender (receiver viewserver))
  (declare (ignore x class sender))
  nil)

(defmethod asktable (ol sl sender (receiver viewserver))
  (call-next-method ol sl sender receiver))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Inference commands
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; save, drop, change
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod change (p (receiver viewserver))
  (cond ((atom p) (save p receiver))
        ((eq (car p) 'not) (drop (cadr p) receiver))
        ((eq (car p) 'and) (dolist (p (cdr p)) (change p receiver)))
        (t (save p receiver))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; facts, rules
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod facts (x (th viewserver) &optional (f #'matchp))
  (call-next-method x th f))

(defmethod rules (x (th factserver) &optional (f #'matchp))
  (call-next-method x th f))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Structure Commands
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Class Commands
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; classify
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod classify (x (th viewserver))
  (let (dum)
    (cond ((characterp x) 'character)
          ((stringp x) 'string)
          ((numberp x) 'number)
          ((setq dum (find-classifier (name th)))
           (or (result dum x th) 'thing))
          ((transclass x th))
          ((ruleclass x th th))
          (t 'thing))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Miscellaneous
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun insertplusminus (p th)
  (cond ((atom p) (save `(plus ,p) th))
        ((eq (car p) 'not) (save `(minus ,(cadr p)) th))
        ((eq (car p) 'and) (mapc #'(lambda (p) (insertplusminus p th)) (cdr p)))
        (t (save `(plus ,p) th))))

(defun insertposneg (p th)
  (cond ((atom p) (save `(pos ,p) th))
        ((eq (car p) 'not) (save `(neg ,(cadr p)) th))
        ((eq (car p) 'and) (mapc #'(lambda (p) (insertposneg p th)) (cdr p)))
        (t (save `(pos ,p) th))))

(defun insertatoms (p th)
  (cond ((atom p) (save `(plus ,p) th))
        ((eq (car p) 'and) (mapc #'(lambda (p) (insertatoms p th)) (cdr p)))
        ((eq (car p) 'plus) (save p th))
        ((eq (car p) 'minus) (save p th))
        ((eq (car p) 'pos) (save `(plus ,(cadr p)) th))
        ((eq (car p) 'neg) (save `(minus ,(cadr p)) th))
        ((eq (car p) 'not) (save `(minus ,(cadr p)) th))
        (t (save `(plus ,p) th))))

(defun factinsert (p th)
  (cond ((atom p) (addcontent p th) (index p p th) p)
        ((eq (car p) 'and) (dolist (p (cdr p)) (factinsert p th)))
        ((eq (car p) 'minus) (drop (cadr p) th))
        ((eq (car p) 'neg) (drop (cadr p) th))
        ((eq (car p) 'not) (drop (cadr p) th))
        ((eq (car p) 'plus) (factinsert (cadr p) th))
        ((eq (car p) 'pos) (factinsert (cadr p) th))
        (t (addcontent p th) (index p p th) p)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun supports (p *theory*)
   (let (*answers* *stack*)
     (supportsexp p)
     (nreverse *answers*)))

(defun supportsexp (p)
  (cond ((atom p))
        ((eq (car p) 'not) (supportsexp (cadr p)))
        ((eq (car p) 'and) (dolist (p (cdr p)) (supportsexp p)))
        ((eq (car p) 'or) (dolist (p (cdr p)) (supportsexp p)))
        ((find (car p) '(same distinct)))
        ((eq (car p) 'oneof))
        ((eq (car p) 'choose) (supportsexp (caddr p)))
        ((eq (car p) 'bagofall) (supportsexp (caddr p)))
        ((eq (car p) 'unprovable) (supportsexp (cadr p)))
        ((find (car p) '(ground nonground execute evaluate)))
        ((get (car p) 'basicval))
        ((get (car p) 'basic))
        (t (supportsrs p))))

(defun supportsrs (p)
  (cond ((find (car p) *stack*))
        ((nonviewp (car p) *theory*)
         (setq *answers* (cons (car p) *answers*))
         (setq *stack* (cons (car p) *stack*)))
        (t (setq *stack* (cons (car p) *stack*))
           (supportsdb p *theory*))))

(defun supportsdb (p th)
  (supportsth p th)
  (dolist (th (includees th)) (supportsdb p th)))

(defun supportsth (p th)
  (do ((l (indexees (car p) th) (cdr l)))
      ((null l))
      (when (and (listp (car l)) (eq (caar l) '<=)
                 (listp (cadar l)) (eq (caadar l) (car p)))
            (dolist (p (cddar l)) (supportsexp p)))))


(defun nonviewp (x th)
  (cond ((find x '(plus minus pos neg error trigger)) nil)
        (t (not (fullviewp x th)))))

(defun fullviewp (x th)
  (cond ((fullviewpth x th))
        (t (some #'(lambda (th) (fullviewp x th)) (includees th)))))

(defun fullviewpth (x th)
  (do ((l (indexees x th) (cdr l)))
      ((null l) nil)
      (cond ((atom (car l)))
            ((eq '<= (caar l))
             (when (and (listp (cadar l)) (eq x (caadar l))) (return t))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun dependents (p *theory* *test*)
   (let (*answers* *stack*)
     (dependentsexp p)
     (nreverse *answers*)))

(defun dependentsexp (p)
  (cond ((atom p))
        ((eq (car p) 'not) (dependentsexp (cadr p)))
        ((eq (car p) 'and) (dolist (p (cdr p)) (dependentsexp p)))
        ((eq (car p) 'or) (dolist (p (cdr p)) (dependentsexp p)))
        ((find (car p) '(same distinct)))
        ((find (car p) '(oneof choose bagofall unprovable)))
        ((find (car p) '(ground nonground execute evaluate)))
        ((get (car p) 'basicval))
        ((get (car p) 'basic))
        (t (dependentsrs p))))

(defun dependentsrs (p)
  (cond ((find (car p) *stack*))
        ((funcall *test* (car p))
         (setq *answers* (cons (car p) *answers*))
         (setq *stack* (cons (car p) *stack*)))
        (t (setq *stack* (cons (car p) *stack*))
           (dependentsdb p *theory*))))

(defun dependentsdb (p th)
  (dependentsth p th)
  (dolist (th (includees th)) (dependentsdb p th)))

(defun dependentsth (p th)
  (do ((l (indexees (car p) th) (cdr l)))
      ((null l))
      (when (and (listp (car l)) (eq (caar l) '<=))
        (dolist (sub (cddar l))
          (when (and (listp sub) (eq (operator sub) (car p)))
            (dependentsrs (cadar l)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
