;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; (c) Copyright 1999-2008 by Michael Genesereth.  All rights reserved.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; translator.lisp
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(eval-when (compile load eval)
  (proclaim '(special *agent* *library* *target*)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; translator
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass translator (agent) ())

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Knowledge commands
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; affirm, retract
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod affirm (p sender (receiver translator))
  (declare (ignore sender))
  (affirm p receiver (get-rulebase receiver)))

(defmethod retract (p sender (receiver translator))
  (declare (ignore sender))
  (retract p receiver (get-rulebase receiver)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; askabout
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod askabout (x sender (receiver translator))
  (declare (ignore sender))
  (askabout x receiver (get-rulebase receiver)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Data commands
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; revise, errors, revisions, materializations, notifications, reactions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod revise (p sender (receiver translator))
  (declare (ignore sender))
  (let (revisions reactions factserver)
    (cond ((errors p receiver))
          (t (setq revisions (revisions p receiver))
             (setq reactions (reactions p receiver))
             (setq factserver (find-target (name receiver)))
             (revise (maksand revisions) receiver factserver)
             (dolist (dum reactions) (eval dum))
             'done))))

(defmethod revisions (p (receiver translator))
  (let (*agent* *library* *target* ruleserver temp pat pos neg answers)
    (setq *agent* (name receiver))
    (setq *library* (find-rulebase *agent*))
    (setq *target* (find-target *agent*))
    (setq ruleserver (symbol-value *library*) *library* ruleserver)
    (setq temp (make-instance 'theory))
    (insertplusminus p temp)
    (includes temp ruleserver)
    (dolist (r (supports p ruleserver)) ; TLH #'nonviewp))
      (setq pat `(,r @l))
      (setq neg (maksor (viewresidues `(minus ,pat) temp #'nonviewp)))
      (setq answers (nconc answers (asks `(not ,pat) neg receiver *target*)))
      (setq pos (maksor (viewresidues `(plus ,pat) temp #'nonviewp)))
      (setq answers (nconc answers (asks pat pos receiver *agent*))))
    (decludes temp)
    (empty temp)
    answers))

(defmethod revisions (p (receiver translator))
  (let (*agent* *library* *target* ruleserver temp pat pos neg answers)
    (setq *agent* (name receiver))
    (setq *library* (find-rulebase *agent*))
    (setq *target* (find-target *agent*))
    (setq ruleserver (symbol-value *library*) *library* ruleserver)
    (setq temp (make-instance 'theory))
    (insertplusminus p temp)
    (includes temp ruleserver)
    (dolist (r (supports p ruleserver)) ; TLH: (basetables p ruleserver))
      (setq pat `(,r @l))
      (setq neg (maksor (viewresidues `(minus ,pat) temp #'nonviewp)))
      (setq answers (nconc answers (asks `(not ,pat) neg receiver *target*)))
      (setq pos (maksor (viewresidues `(plus ,pat) temp #'nonviewp)))
      (setq answers (nconc answers (asks pat pos receiver *agent*))))
    (decludes temp)
    (empty temp)
    answers))

(defmethod errors (p (receiver translator))
  (let (*agent* *library* *target* ruleserver temp residues answers)
    (setq *agent* (name receiver))
    (setq *library* (find-rulebase *agent*))
    (setq *target* (find-target *agent*))
    (setq ruleserver (symbol-value *library*))
    (setq temp (make-instance 'theory))
    (insertposneg p temp)
    (includes temp ruleserver)
    (setq residues (viewresidues '(error ?e) temp #'nonviewp))
    (unless (null residues)
      (setq answers (asks '?e (maksor residues) receiver *target*)))
    (decludes temp)
    (empty temp)
    answers))

(defmethod materializations (p (receiver translator))
  (declare (ignore p receiver))
  nil)

(defmethod reactions (p (receiver translator))
  (let (*agent* *library* *target* ruleserver temp residues answers)
    (setq *agent* (name receiver))
    (setq *library* (find-rulebase *agent*))
    (setq *target* (find-target *agent*))
    (setq ruleserver (symbol-value *library*))
    (setq temp (make-instance 'theory))
    (insertposneg p temp)
    (includes temp ruleserver)
    (setq residues (viewresidues '(trigger ?x) temp #'nonviewp))
    (unless (null residues)
      (setq answers (asks '?x (maksor residues) receiver *target*)))
    (decludes temp)
    (empty temp)
    answers))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; askp, askx, asks
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod askp (p sender (th translator))
  (askx t p sender th))

(defmethod askx (x p sender (th translator))
  (declare (ignore sender))
  (let (*rules* *target* query)
    (setq *rules* (symbol-value (find-rulebase th)))
    (setq *target* (find-target (name th)))
    (setq query (maksor (viewresidues p *rules* #'nonviewp)))
    (request `(ask-one ,x ,query) th  *target*)))

(defmethod asks (x p sender (th translator))
  (declare (ignore sender))
  (let (*rules* *target* query)
    (setq *rules* (symbol-value (find-rulebase th)))
    (setq *target* (find-target (name th)))
    (setq query (maksor (viewresidues p *rules* #'nonviewp)))
    (request `(ask-all ,x ,query) th  *target*)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; askclass, askframe, asktable
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod askclass (x sender (receiver translator))
  (declare (ignore sender))
  (classify x receiver))

(defmethod askframe (x class sender (receiver translator))
  (declare (ignore x class sender))
  nil)

(defmethod asktable (ol sl sender (receiver translator))
  (call-next-method ol sl sender receiver))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Theory commands
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; classify, facts, rules
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod classify (x (source translator))
  (let (dum)
    (cond ((characterp x) 'character)
          ((stringp x) 'string)
          ((numberp x) 'number)
          ((setq dum (find-classifier (name source)))
           (or (askx '?class (list dum x '?class) source source) 'thing))
          ((not (eq (setq dum (classify x (find-target (name source)))) 'thing)) dum)
          ((ruleclass x source (get-rulebase source)))
          (t 'thing))))

(defmethod facts (x (th translator) &optional (f #'matchp))
  ;(declare (ignore sender))
  (facts x (find-target (name th)) f))

(defmethod rules (x (th translator) &optional (f #'matchp))
  ;(declare (ignore sender))
  (facts x (find-rulebase (name th)) f))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
