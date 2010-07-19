;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; (c) Copyright 1999-2006 by Michael Genesereth.  All rights reserved.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; facilitator.lisp
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(eval-when (compile load eval)
  (proclaim '(special *manager* *library* *warehouse* *ancestry*
                      *agents* *collapse* *compress*
                      *message* *sender* *receiver* *target*)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; facilitator
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass facilitator (agent) ())

(defmethod message-handler (*message* *sender* (*receiver* facilitator))
  (let (*library* *warehouse* *agents*)
    (setq *library* (referent (find-rulebase *receiver*)))
    (setq *warehouse* (referent (find-database *receiver*)))
    (eval *message*)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Knowledge commands
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; affirm, retract
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod affirm (p sender (receiver facilitator))
  (declare (ignore sender))
  (affirm p receiver (get-rulebase receiver)))

(defmethod retract (p sender (receiver facilitator))
  (declare (ignore sender))
  (retract p receiver (get-rulebase receiver)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; askabout
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod askabout (x sender (receiver facilitator))
  (declare (ignore sender))
  (askabout x receiver (get-rulebase receiver)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Data commands
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; revise, revisions, errors, materializations, notifications, reactions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod revise (p sender (receiver facilitator))
  (declare (ignore sender))
  (let (notifications materializations reactions factserver)
    (cond ((errors p receiver))
          (t (setq materializations (materializations p receiver))
             (setq notifications (notifications p receiver))
             (setq reactions (reactions p receiver))
             (setq factserver (find-target (name receiver)))
             (dolist (p materializations) (change p factserver))
             (dolist (dum notifications)
               (revise (maksand (cdr dum)) receiver (car dum)))
             (dolist (dum reactions) (eval dum))
             'done))))

(defmethod errors (p (receiver facilitator))
  (let (*agent* *library* *agents* ruleserver temp query answers)
    (setq *agent* (name receiver))
    (setq *library* (find-rulebase *agent*))
    (setq *agents* (find-agents))
    (setq ruleserver (symbol-value *library*))
    (setq temp (make-instance 'theory))
    (insertplusminus p temp)
    (includes temp ruleserver)
    (setq query (maksor (fullresidues '(error ?e) temp #'specialp)))
    (setq answers (askallquery '?e query))
    (decludes temp)
    (empty temp)
    answers))

(defmethod materializations (p (receiver facilitator))
  (let (*agent* *library* *agents* ruleserver temp pat pos neg answers)
    (setq *agent* (name receiver))
    (setq *library* (find-rulebase *agent*))
    (setq *agents* (find-agents))
    (setq ruleserver (symbol-value *library*))
    (setq temp (make-instance 'theory))
    (insertplusminus p temp)
    (includes temp ruleserver)
    (dolist (r (viewfinds '?r '(material ?a ?r) *manager*))
      (setq pat `(,r @l))
      (setq neg (maksor (fullresidues `(neg ,pat) temp #'specialp)))
      (setq answers (nconc answers (askallquery `(not ,pat) neg)))
      (setq pos (maksor (fullresidues `(pos ,pat) temp #'specialp)))
      (setq answers (nconc answers (askallquery pat pos))))
    (decludes temp)
    (empty temp)
    answers))

(defmethod notifications (p (receiver facilitator))
  (let (*agent* *library* *agents* ruleserver temp pat pos neg answers subscripts)
    (setq *agent* (name receiver))
    (setq *library* (find-rulebase *agent*))
    (setq *agents* (find-agents))
    (setq ruleserver (symbol-value *library*))
    (setq temp (make-instance 'theory))
    (insertplusminus p temp)
    (includes temp ruleserver)
    (dolist (agent (subscribers '?))
      (dolist (r (subscriptions agent))
        (setq pat `(,r @l))
        (setq neg (maksor (fullresidues `(neg ,pat) temp #'specialp)))
        (setq answers (nconc answers (askallquery `(not ,pat) neg)))
        (setq pos (maksor (fullresidues `(pos ,pat) temp #'specialp)))
        (setq answers (nconc answers (askallquery pat pos))))
      (when answers (setq subscripts (cons (cons agent answers) subscripts)))
      (setq answers nil))
    (decludes temp)
    (empty temp)
    subscripts))

(defmethod reactions (p (receiver facilitator))
  (let (*agent* *library* *agents* ruleserver temp query answers)
    (setq *agent* (name receiver))
    (setq *library* (find-rulebase *agent*))
    (setq *agents* (find-agents))
    (setq ruleserver (symbol-value *library*))
    (setq temp (make-instance 'theory))
    (insertplusminus p temp)
    (includes temp ruleserver)
    (setq query (maksor (fullresidues '(trigger ?x) temp #'specialp)))
    (setq answers (askallquery '?x query))
    (decludes temp)
    (empty temp)
    answers))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; askp, askx, asks
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod askp (p sender (receiver facilitator))
  (askx t p sender receiver))

(defmethod askx (x p sender (receiver facilitator))
  (declare (ignore sender))
  (let (*agents*)
    (setq *agents* (find-agents))
    (askonequery x (facilitateask x p receiver))))

(defmethod asks (x p sender (receiver facilitator))
  (declare (ignore sender))
  (let (*agents*)
    (setq *agents* (find-agents))
    (askallquery x (facilitateask x p receiver))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; askclass, askframe, asktable
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod askclass (x sender (receiver facilitator))
  (declare (ignore sender))
  (classify x receiver))

(defmethod askframe (x class sender (receiver facilitator))
  (declare (ignore x class sender))
  nil)

(defmethod asktable (ol sl sender (receiver facilitator))
  (call-next-method ol sl sender receiver))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Theory commands
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; classify, facts, rules
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod classify (x (receiver facilitator))
  (let (dum)
    (cond ((characterp x) 'character)
          ((stringp x) 'string)
          ((numberp x) 'number)
          ((setq dum (find-classifier (name receiver)))
           (or (askx '?class (list dum x '?class) receiver receiver) 'thing))
          ((dolist (a (finds '?a '(specialty ?a ?r) *manager*))
             (when (not (eq (setq dum (classify x a)) 'thing)) (return dum))))
          ((ruleclass x receiver (get-rulebase receiver)))
          (t 'thing))))

(defmethod facts (x (th facilitator) &optional (f #'matchp))
  (do ((l (find-agents) (cdr l)) (nl)) 
      ((null l) nl)
      (cond ((eq (car l) (name th)))
            (t (setq nl (unionize nl (facts x (car l) f)))))))

(defmethod rules (x (th facilitator) &optional (f #'matchp))
  ;(declare (ignore sender))
  (facts x (find-rulebase (name th)) f))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Reduction
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun facilitateask (x p receiver)
  (let (library temp residues answer)
    (setq library (get-rulebase receiver))
    (setq temp (make-instance 'theory))
    (includes temp library)
    (dolist (x (contrapositives `(<= (answer ,x) ,p))) (insert x temp))
    (when (setq residues (fullresidues `(answer ,x) temp #'specialp))
      (setq answer (decolonize (maksor residues)))
      (if *collapse* (setq answer (collapse x answer))))
    (decludes temp)
    (empty temp)
    answer))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
