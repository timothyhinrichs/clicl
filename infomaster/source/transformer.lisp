;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; (c) Copyright 1999-2005 by Michael Genesereth.  All rights reserved.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; transformer.lisp
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(eval-when (compile load eval)
  (proclaim '(special *manager* *agent* *library* *target*)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; transformer
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass transformer (agent) ())

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Knowledge commands
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; affirm, retract
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod affirm (p sender (receiver transformer))
  (declare (ignore sender))
  (affirm p receiver (get-rulebase receiver)))

(defmethod retract (p sender (receiver transformer))
  (declare (ignore sender))
  (retract p receiver (get-rulebase receiver)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; askabout
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod askabout (x sender (receiver transformer))
  (declare (ignore sender))
  (askabout x receiver (get-rulebase receiver)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Data commands
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; revise, revisions, errors, materializations, notifications, reactions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod revise (p sender (receiver transformer))
  (declare (ignore sender))
  (let (revisions materializations reactions factserver)
    (setq revisions (revisions p receiver) p (maksand revisions))
    (cond ((errors p receiver))
          (t (setq materializations (materializations p receiver))
             (setq reactions (reactions p receiver))
             (setq factserver (find-target (name receiver)))
             (dolist (p revisions) (change p factserver))
             (dolist (p materializations) (change p factserver))
             (dolist (dum reactions) (eval dum))
             'done))))

(defmethod revisions (p (receiver transformer))
  (let (*agent* *library* *target* ruleserver temp pat pos neg answers)
    (setq *agent* (name receiver))
    (setq *library* (find-rulebase *agent*))
    (setq *target* (find-target *agent*))
    (setq ruleserver (symbol-value *library*))
    (setq temp (make-instance 'theory))
    (insertplusminus p temp)
    (includes temp ruleserver)
    (dolist (r (supports p ruleserver)) ; TLH #'extensionalp))
      (setq pat `(,r @l))
      (setq neg (maksor (viewresidues `(minus ,pat) temp #'specialtyp)))
      (setq answers (nconc answers (asks `(not ,pat) neg receiver *target*)))
      (setq pos (maksor (viewresidues `(plus ,pat) temp #'specialtyp)))
      (setq answers (nconc answers (asks pat pos receiver *agent*))))
    (decludes temp)
    (empty temp)
    answers))

(defmethod errors (p (receiver transformer))
  (let (*agent* *library* *target* ruleserver temp residues answers)
    (setq *agent* (name receiver))
    (setq *library* (find-rulebase *agent*))
    (setq *target* (find-target *agent*))
    (setq ruleserver (symbol-value *library*))
    (setq temp (make-instance 'theory))
    (insertposneg p temp)
    (includes temp ruleserver)
    (setq residues (viewresidues '(error ?e) temp #'specialtyp))
    (unless (null residues)
      (setq answers (asks '?e (maksor residues) receiver *target*)))
    (decludes temp)
    (empty temp)
    answers))

(defun materialp (r)
  (and (groundp r) (findp `(material ?a ,r) *manager*)))

(defmethod materializations (p (receiver transformer))
  (let (*agent* *library* *target* ruleserver temp pat pos neg answers)
    (setq *agent* (name receiver))
    (setq *library* (find-rulebase *agent*))
    (setq *target* (find-target *agent*))
    (setq ruleserver (symbol-value *library*))
    (setq temp (make-instance 'theory))
    (insertposneg p temp)
    (includes temp ruleserver)
    (dolist (r (dependents p ruleserver #'materialp))
      (setq pat `(,r @l))
      (setq neg (maksor (viewresidues `(neg ,pat) temp #'specialtyp)))
      (setq answers (nconc answers (asks `(not ,pat) neg receiver *target*)))
      (setq pos (maksor (viewresidues `(pos ,pat) temp #'specialtyp)))
      (setq answers (nconc answers (asks pat pos receiver *target*))))
    (decludes temp)
    (empty temp)
    answers))

(defmethod reactions (p (receiver transformer))
  (let (*agent* *library* *target* ruleserver temp residues answers)
    (setq *agent* (name receiver))
    (setq *library* (find-rulebase *agent*))
    (setq *target* (find-target *agent*))
    (setq ruleserver (symbol-value *library*))
    (setq temp (make-instance 'theory))
    (insertposneg p temp)
    (includes temp ruleserver)
    (setq residues (viewresidues '(trigger ?x) temp #'specialtyp))
    (unless (null residues)
      (setq answers (asks '?x (maksor residues) receiver *target*)))
    (decludes temp)
    (empty temp)
    answers))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; askp, askx, asks
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod askp (p sender (receiver transformer))
  (askx t p sender receiver))

(defmethod askx (x p sender (receiver transformer))
  (declare (ignore sender))
  (let (ruleserver *target* query)
    (setq ruleserver (symbol-value (find-rulebase receiver)))
    (setq *target* (find-target (name receiver)))
    (setq query (maksor (viewresidues p ruleserver #'specialtyp)))
    (request `(ask-one ,x ,query) receiver *target*)))

(defmethod asks (x p sender (receiver transformer))
  (declare (ignore sender))
  (let (ruleserver *target* query)
    (setq ruleserver (symbol-value (find-rulebase receiver)))
    (setq *target* (find-target (name receiver)))
    (setq query (maksor (viewresidues p ruleserver #'specialtyp)))
    (request `(ask-all ,x ,query) receiver *target*)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; askclass, askframe, asktable
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod askclass (x sender (receiver transformer))
  (declare (ignore sender))
  (classify x receiver))

(defmethod askframe (x class sender (receiver transformer))
  (declare (ignore x class sender))
  nil)

(defmethod asktable (ol sl sender (receiver transformer))
  (call-next-method ol sl sender receiver))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Theory commands
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; save, drop, change
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod save (p (th transformer) &optional (f 'samep))
  (cond ((atom p) (save p (find-target (name th)) f))
        ((eq (car p) '<=) (save p (get-rulebase th) f))
        ((eq (car p) '=>) (save p (get-rulebase th) f))
        (t (save p (find-target (name th)) f))))

(defmethod drop (p (th transformer) &optional (f 'samep))
  (cond ((atom p) (drop p (find-target (name th)) f))
        ((eq (car p) '<=) (drop p (get-rulebase th) f))
        ((eq (car p) '=>) (drop p (get-rulebase th) f))
        (t (drop p (find-target (name th)) f))))

(defmethod change (p (th transformer))
  (change p (find-target (name th))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; knownp, knownx, knowns
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; classify
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod classify (x (th transformer))
  (let (dum)
    (cond ((characterp x) 'character)
          ((stringp x) 'string)
          ((numberp x) 'number)
          ((setq dum (find-classifier (name th)))
           (or (askx '?class (list dum x '?class) th th) 'thing))
          ((not (eq (setq dum (classify x (find-target (name th)))) 'thing)) dum)
          ((ruleclass x th (get-rulebase th)))
          (t 'thing))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; facts, rules
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod facts (x (th transformer) &optional (f #'matchp))
  ;(declare (ignore sender))
  (facts x (find-target (name th)) f))

(defmethod rules (x (th transformer) &optional (f #'matchp))
  ;(declare (ignore sender))
  (facts x (find-rulebase (name th)) f))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Miscellaneous
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun interestedp (r)
  (or (basep r)
      (and (groundp r) (truep `(interest ,(name *target*) ,r) *manager*))))

(defun specialtyp (r)
  (or (basep r)
      (and (groundp r) (findp `(specialty ,(name *target*) ,r) *manager*))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
