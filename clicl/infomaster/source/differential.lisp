;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; (c) Copyright 1999-2003 by Michael Genesereth.  All rights reserved.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(eval-when (compile load eval)
  (proclaim '(special *library* *warehouse*)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; differentialdatabase
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass differentialdatabase (theory) ())

(defmethod insert (p (th differentialdatabase))
  (when (and (listp p) (eq 'not (car p))) (uninsertitem (cadr p) th))
  (addcontent p th)
  (index p p th)
  p)

(defmethod save (p (th differentialdatabase) &optional (f 'samep))
  (unless (knownp p th f) (insert p th)))

(defmethod drop (p (th differentialdatabase) &optional (f 'samep))
  (declare (ignore f))
  (cond ((truep p th) (uninsertitem p th))
        ((truep (maknot p) th))
        ((some #'(lambda (x) (knownp p x)) (includees th))
         (insert (maknot p) th))))

(defmethod findx (x p (th differentialdatabase))
  (do ((l (includees th) (cdr l)) (query (diff p th)) (dum))
      ((null l) (truex x p th))
      (when (setq dum (findx x query (car l))) (return dum))))

(defmethod finds (x p (th differentialdatabase))
  (do ((l (includees th) (cdr l)) (query (diff p th)) (nl))
      ((null l) (nreverse (nreconc (trues x p th) nl)))
      (setq nl (nreconc (finds x query (car l)) nl))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; differential
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass differential (agent) ())

(defmethod create (obj (type (eql 'differential)))
  (unless (and (boundp obj) (typep (symbol-value obj) 'differential))
    (set obj (make-instance 'differential :name obj))))

(defmethod destroy (obj (type (eql 'differential)))
  (cond ((and (symbolp obj) (boundp obj)
              (eq (type-of (symbol-value obj)) 'differential))
         (makunbound obj))))

(defmethod message-handler (*message* *sender* (*receiver* differential))
  (cond ((atom *message*))
        ((eq 'update (car *message*)) (change (maksand (cdr *message*)) *receiver*))
        ((eq 'tell (car *message*)) (assume (cadr *message*) *receiver*))
        ((eq 'untell (car *message*)) (forget (cadr *message*) *receiver*))
        ;((eq 'eliminate (car *message*)) nil)
        ((eq 'ask-if (car *message*)) (findx t (cadr *message*) *receiver*))
        ((eq 'ask-one (car *message*)) (findx (cadr *message*) (caddr *message*) *receiver*))
        ((eq 'ask-all (car *message*)) (finds (cadr *message*) (caddr *message*) *receiver*))
        ((eq 'ask-table (car *message*))
         (asktable (cadr *message*) (caddr *message*) *sender* *receiver*))
        ;((eq 'ask-about (car *message*)) (request *message* *receiver* target))
        ((eq 'quote (car *message*)) *message*)
        ((macro-function (car *message*)) (request *message* *sender* *receiver*))
        ((fboundp (car *message*)) (eval *message*))))

(defmethod findx (x p (th differential))
  (let (target)
    (setq target (referent (find-original (name th))))
    (cond ((findx x (diff p th) target))
          (t (truex x p th)))))

(defmethod finds (x p (th differential))
  (let (target)
    (setq target (referent (find-original (name th))))
    (unionize (finds x (diff p th) target)
              (trues x p th))))

(defmethod assume (p (th differential))
  (cond ((atom p) (save p th))
        ((eq 'unprovable (car p)) (drop (cadr p) th))
        ((eq 'not (car p)) (drop (cadr p) th))
        ((eq 'and (car p)) (dolist (p (cdr p)) (assume p th)) p)
        (t (save p th))))

(defmethod forget (p (th differential))
  (cond ((atom p) (drop p th))
        ((eq 'and (car p)) (dolist (p (cdr p)) (forget p th)) p)
        (t (drop p th))))

(defmethod save (p (th differential) &optional (f 'samep))
  (declare (ignore f))
  (let (target)
    (setq target (referent (find-original (name th))))
    (cond ((truep p th))
          ((truep (maknot p) th) (uninsertitem (maknot p) th))
          ((knownp p target))
          (t (insert p th)))))

(defmethod drop (p (th differential) &optional (f 'samep))
  (declare (ignore f))
  (let (target)
    (setq target (referent (find-original (name th))))
    (cond ((truep p th) (uninsertitem p th))
          ((truep (maknot p) th))
          ((knownp p target) (insert (maknot p) th)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; differentialagent
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass differentialagent (agent) ())

(defmethod create (obj (type (eql 'differentialagent)))
  (unless (and (boundp obj) (typep (symbol-value obj) 'differentialagent))
    (set obj (make-instance 'differentialagent :name obj))))

(defmethod destroy (obj (type (eql 'differentialagent)))
  (cond ((and (symbolp obj) (boundp obj)
              (eq (type-of (symbol-value obj)) 'differentialagent))
         (makunbound obj))))

(defmethod message-handler (*message* *sender* (*receiver* differentialagent))
  (cond ((atom *message*))
        ((eq 'update (car *message*)) (change (maksand (cdr *message*)) *receiver*))
        ((eq 'tell (car *message*)) (assume (cadr *message*) *receiver*))
        ((eq 'untell (car *message*)) (forget (cadr *message*) *receiver*))
        ;((eq 'eliminate (car *message*)) nil)
        ((eq 'ask-if (car *message*)) (findx t (cadr *message*) *receiver*))
        ((eq 'ask-one (car *message*)) (findx (cadr *message*) (caddr *message*) *receiver*))
        ((eq 'ask-all (car *message*)) (finds (cadr *message*) (caddr *message*) *receiver*))
        ((eq 'ask-table (car *message*))
         (asktable (cadr *message*) (caddr *message*) *sender* *receiver*))
        ;((eq 'ask-about (car *message*)) (request *message* *receiver* target))
        ((eq 'quote (car *message*)) *message*)
        ((macro-function (car *message*)) (request *message* *sender* *receiver*))
        ((fboundp (car *message*)) (eval *message*))))

(defmethod findx (x p (th differentialagent))
  (let (target)
    (setq target (referent (find-original (name th))))
    (cond ((findx x (diff p th) target))
          (t (truex x p th)))))

(defmethod finds (x p (th differentialagent))
  (let (target)
    (setq target (referent (find-original (name th))))
    (unionize (finds x (diff p th) target)
              (trues x p th))))

(defmethod assume (p (th differentialagent))
  (cond ((atom p) (save p th))
        ((eq 'unprovable (car p)) (drop (cadr p) th))
        ((eq 'not (car p)) (drop (cadr p) th))
        ((eq 'and (car p)) (dolist (p (cdr p)) (assume p th)) p)
        (t (save p th))))

(defmethod forget (p (th differentialagent))
  (cond ((atom p) (drop p th))
        ((eq 'and (car p)) (dolist (p (cdr p)) (forget p th)) p)
        (t (drop p th))))

(defmethod save (p (th differentialagent) &optional (f 'samep))
  (declare (ignore f))
  (let (target)
    (setq target (referent (find-original (name th))))
    (cond ((truep p th))
          ((truep (maknot p) th) (uninsertitem (maknot p) th))
          ((knownp p target))
          (t (insert p th)))))

(defmethod drop (p (th differentialagent) &optional (f 'samep))
  (declare (ignore f))
  (let (target)
    (setq target (referent (find-original (name th))))
    (cond ((truep p th) (uninsertitem p th))
          ((truep (maknot p) th))
          ((knownp p target) (insert (maknot p) th)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; deltaagent
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass deltaagent (agent) ())

(defmethod create (obj (type (eql 'deltaagent)))
  (unless (and (boundp obj) (typep (symbol-value obj) 'deltaagent))
    (set obj (make-instance 'deltaagent :name obj))))

(defmethod destroy (obj (type (eql 'deltaagent)))
  (cond ((and (symbolp obj) (boundp obj)
              (eq (type-of (symbol-value obj)) 'deltaagent))
         (makunbound obj))))

(defmethod message-handler (*message* *sender* (*receiver* deltaagent))
  (cond ((atom *message*))
        ((eq 'update (car *message*)) (change (maksand (cdr *message*)) *receiver*))
        ((eq 'tell (car *message*)) (assume (cadr *message*) *receiver*))
        ((eq 'untell (car *message*)) (forget (cadr *message*) *receiver*))
        ;((eq 'eliminate (car *message*)) nil)
        ((eq 'ask-if (car *message*)) (findx t (cadr *message*) *receiver*))
        ((eq 'ask-one (car *message*)) (findx (cadr *message*) (caddr *message*) *receiver*))
        ((eq 'ask-all (car *message*)) (finds (cadr *message*) (caddr *message*) *receiver*))
        ((eq 'ask-table (car *message*))
         (asktable (cadr *message*) (caddr *message*) *sender* *receiver*))
        ;((eq 'ask-about (car *message*)) (request *message* *receiver* target))
        ((eq 'quote (car *message*)) *message*)
        ((macro-function (car *message*)) (request *message* *sender* *receiver*))
        ((fboundp (car *message*)) (eval *message*))))

(defmethod findx (x p (th deltaagent))
  (let (target)
    (setq target (referent (find-original (name th))))
    (cond ((request `(ask-one ,x ,(diff p th)) th target))
          (t (truex x p th)))))

(defmethod finds (x p (th deltaagent))
  (let (target)
    (setq target (referent (find-original (name th))))
    (unionize (request `(ask-all ,x ,(diff p th)) th target)
              (trues x p th))))

(defmethod assume (p (th deltaagent))
  (cond ((atom p) (save p th))
        ((eq 'unprovable (car p)) (drop (cadr p) th))
        ((eq 'not (car p)) (drop (cadr p) th))
        ((eq 'and (car p)) (dolist (p (cdr p)) (assume p th)) p)
        (t (save p th))))

(defmethod forget (p (th deltaagent))
  (cond ((atom p) (drop p th))
        ((eq 'and (car p)) (dolist (p (cdr p)) (forget p th)) p)
        (t (drop p th))))

(defmethod save (p (th deltaagent) &optional (f 'samep))
  (declare (ignore f))
  (let (target)
    (setq target (referent (find-original (name th))))
    (cond ((truep p th))
          ((truep (maknot p) th) (uninsertitem (maknot p) th))
          ((request `(ask-if ,p) th target))
          (t (insert p th)))))

(defmethod drop (p (th deltaagent) &optional (f 'samep))
  (declare (ignore f))
  (let (target)
    (setq target (referent (find-original (name th))))
    (cond ((truep p th) (uninsertitem p th))
          ((truep (maknot p) th))
          ((request `(ask-if ,p) th target) (insert (maknot p) th)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; diffagent
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass diffagent (agent) ())

(defmethod create (obj (type (eql 'diffagent)))
  (unless (and (boundp obj) (typep (symbol-value obj) 'diffagent))
    (set obj (make-instance 'diffagent :name obj))))

(defmethod destroy (obj (type (eql 'diffagent)))
  (cond ((and (symbolp obj) (boundp obj)
              (eq (type-of (symbol-value obj)) 'diffagent))
         (makunbound obj))))

(defmethod message-handler (*message* *sender* (*receiver* diffagent))
  (cond ((atom *message*))
        ((eq 'update (car *message*)) (change (maksand (cdr *message*)) *receiver*))
        ((eq 'tell (car *message*)) (assume (cadr *message*) *receiver*))
        ((eq 'untell (car *message*)) (forget (cadr *message*) *receiver*))
        ;((eq 'eliminate (car *message*)) nil)
        ((eq 'ask-if (car *message*)) (findx t (cadr *message*) *receiver*))
        ((eq 'ask-one (car *message*)) (findx (cadr *message*) (caddr *message*) *receiver*))
        ((eq 'ask-all (car *message*)) (finds (cadr *message*) (caddr *message*) *receiver*))
        ((eq 'ask-table (car *message*))
         (asktable (cadr *message*) (caddr *message*) *sender* *receiver*))
        ;((eq 'ask-about (car *message*)) (request *message* *receiver* target))
        ((eq 'quote (car *message*)) *message*)
        ((macro-function (car *message*)) (request *message* *sender* *receiver*))
        ((fboundp (car *message*)) (eval *message*))))

(defmethod findx (x p (th diffagent))
  (truex x p th))

(defmethod finds (x p (th diffagent))
  (trues x p th))

(defmethod assume (p (th diffagent))
  (cond ((atom p) (save p th))
        ((eq 'unprovable (car p)) (drop (cadr p) th))
        ((eq 'and (car p)) (dolist (p (cdr p)) (assume p th)) p)
        (t (save p th))))

(defmethod forget (p (th diffagent))
  (cond ((atom p) (drop p th))
        ((eq 'and (car p)) (dolist (p (cdr p)) (forget p th)) p)
        (t (drop p th))))

(defmethod save (p (th diffagent) &optional (f 'samep))
  (declare (ignore f))
  (cond ((truep p th))
        ((truep (maknot p) th) (uninsertitem (maknot p) th))
        (t (insert p th))))

(defmethod drop (p (th diffagent) &optional (f 'samep))
  (declare (ignore f))
  (cond ((truep p th) (uninsertitem p th))
        ((truep (maknot p) th))))


(defun uninsertitem (p th)
  (uninsert (find p (indexps p th) :test #'equalp) th))

(defun tellall (p th)
  (cond ((atom p) (save p th))
        ((eq 'unprovable (car p)) (save (maknot (cadr p)) th) p)
        ((eq 'and (car p)) (dolist (p (cdr p)) (tellall p th)) p)
        (t (save p th))))

(defun untellall (p th)
  (cond ((atom p) (drop p th))
        ((eq 'unprovable (car p)) (drop (maknot (cadr p)) th) p)
        ((eq 'and (car p)) (dolist (p (cdr p)) (untellall p th)) p)
        (t (drop p th))))

(defun differentialmessage (msg agent)
  (list (car msg) (cadr msg) (diff (caddr msg) agent)))

(defun diff (p agent)
  (cond ((atom p) p)
        ((eq 'and (car p))
         (maksand (mapcar #'(lambda (x) (diff x agent)) (cdr p))))
        ((eq 'or (car p))
         (maksor (mapcar #'(lambda (x) (diff x agent)) (cdr p))))
        ((groundp p) (unless (truep (maknot p) agent) p))
        (t (do ((l (facts (maknot p) agent) (cdr l)) (nl))
               ((null l) (maksand (cons p (nreverse nl))))
               (setq nl (cons `(distinct ,p ,(cadar l)) nl))))))

(defun find-differential (x)
  (findx '?x `(differential ,x ?x) *manager*))


(defun differential (agent1 agent2)
  (do ((l (contents agent1) (cdr l)) (contents (contents agent2)) (nl))
      ((null l) (nreconc nl contents))
      (cond ((equalp (car l) (car contents))
             (setq contents (cdr contents)))
            (t (setq nl (cons `(not ,(car l)) nl))))))

(defun applydifferential (agent1 agent2)
  (copyagentdata agent1 agent2))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; differentialagent tests
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
#|
(definemore *manager*
  '((isa mrg differentialagent)
    (original mrg warehouse)))

(definemore *warehouse*
  '((p a b)
    (p a c)
    (p b d)
    (p c f)
    (q b)
    (q c)))

(request '(tell (p a g)) nil mrg)
DONE

(request '(untell (p a c)) nil mrg)
DONE

(facts 'p mrg)
((p a g) (not (p a c)))

(request '(ask-all (?x ?y) (p ?x ?y)) nil *warehouse*)
((a b) (a c) (b d) (c f))

(request '(ask-all (?x ?y) (p ?x ?y)) nil mrg)
((A B) (B D) (C F) (A G))
|#

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; simulator
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass simulator (agent) ())
#|
(defmethod message-handler (*message* *sender* (*receiver* simulator))
  (let (*library* *warehouse*)
    (setq *library* (referent (find-rulebase *receiver*)))
    (setq *warehouse* (referent (find-database *receiver*)))
    (setq *agents* (finds '?a '(specialty ?a ?x) *manager*))
    (facilitate *message* *sender* *receiver*)))
|#
(defmethod message-handler (*message* *sender* (*receiver* facilitator))
  (let (*library* *warehouse* *agents*)
    (setq *library* (referent (find-rulebase *receiver*)))
    (setq *warehouse* (referent (find-database *receiver*)))
    (setq *agents* (finds '?a '(specialty ?a ?x) *manager*))   ;;; (setq *agents* (find-agents))
    (eval *message*)))

(defmethod message-handler (msg (sender simulator) receiver)
  (let (surrogate differential)
    (cond ((typep receiver 'differentialagent)
           (call-next-method msg sender receiver))
          ((setq surrogate (find-surrogate (name sender) (name receiver)))
           (request msg sender surrogate))
          ((atom msg) (call-next-method msg sender receiver))
          ((find (car msg) '(tell untell))
           (setq differential (make-instance 'dataserver :name (gentemp) :kind 'indexedcompiledrules))
           (setq surrogate (make-instance 'differentialagent :name (gentemp)))
           (telladministrator `(differential ,(name surrogate) ,(name differential)))
           (telladministrator `(repository ,(name sender) ,(name surrogate)))
           (telladministrator `(original ,(name surrogate) ,(name receiver)))
           (request msg sender surrogate))
          (t (call-next-method msg sender receiver)))))

(defun find-original (agent)
  (findx '?x `(original ,agent ?x) *manager*))

(defun find-surrogate (simulator agent)
  (findx '?x `(and (original ?x ,agent) (repository ,simulator ?x)) *manager*))

(defun find-repositories (simulator)
  (finds '?x `(repository ,simulator ?x) *manager*))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; copyagent
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun copyagent (source target)
  (setq source (name source))
  (telladministrator `(isa ,target dataserver))
  (copyslot 'capability source target)
  (copyslot 'extractmetadata source target)
  (copyslot 'classifier source target)
  (copyslot 'frontpage source target)
  (copyslot 'page source target)
  (copyslot 'header source target)
  (copyslot 'footer source target)
  (copyslot 'rootclass source target)
  (copyslot 'rootrelation source target)
  (copyslot 'editor source target)
  (copyslot 'capability source target)
  (copyslot 'nocreate source target)
  (copyslot 'nochange source target)
  (copyslot 'noupdate source target)
  (copyslot 'nocommand source target)
  (copyslot 'frontpage source target)
  (copyslot 'page source target)
  (copyslot 'header source target)
  (copyslot 'footer source target)
  (copyslot 'securityagent source target)
  (request `(tell ,(maksand (get-sentences source))) *receiver* target)
  (telladministrator `(original ,target ,source))
  'done)

(defun killagent (target)
  (decludes (referent target))
  (empty (referent target))
  (kill target *manager*)
  'done)

(defun copyslot (slot source target)
  (dolist (val (finds '?y (list slot source '?y) *manager*))
    (telladministrator (list slot target val))))

(defun find-agentcapability (agent)
  (findx '?x `(agentcapability ,agent ?x) *manager*))

(defun get-sentences (agent)
  (request '(ask-about ?) *receiver* agent))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; setup and breakdown
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod create (obj (type (eql 'simulator)))
  (unless (and (boundp obj) (typep (symbol-value obj) 'simulator))
    (set obj (make-instance 'simulator :name obj))))

(defmethod destroy (obj (type (eql 'simulator)))
  (cond ((and (symbolp obj) (boundp obj)
              (eq (type-of (symbol-value obj)) 'simulator))
         (makunbound obj))))

(defmethod createsimulator (obj agent &rest agents)
  (declare (ignore obj agent agents))
  "CreateSimulator works only on facilitators.")

(defmethod createsimulator (obj (agent symbol) &rest agents)
  (cond ((and (boundp agent) (not (symbolp (symbol-value agent))))
         (apply #'createsimulator obj (symbol-value agent) agents))
        (t (call-next-method agent))))

(defmethod createsimulator (obj (agent facilitator) &rest agents)
  (setq agent (name agent))
  (create obj 'simulator)
  (telladministrator `(isa ,obj simulator))
  (copyslot 'rulebase agent obj)
  (copyslot 'recipient agent obj)
  (copyslot 'rootclass agent obj)
  (copyslot 'rootrelation agent obj)
  (copyslot 'editor agent obj)
  (copyslot 'capability agent obj)
  (copyslot 'nocreate agent obj)
  (copyslot 'nochange agent obj)
  (copyslot 'noupdate agent obj)
  (copyslot 'nocommand agent obj)
  (copyslot 'frontpage agent obj)
  (copyslot 'page agent obj)
  (copyslot 'header agent obj)
  (copyslot 'footer agent obj)
  (copyslot 'securityagent agent obj)
  (do ((l agents (cdr l)) (new))
      ((null l))
      (cond ((find-original (car l))
             (telladministrator `(repository ,obj ,(car l))))
            (t (setq new (gentemp))
               (copyagent (car l) new)
               (telladministrator `(repository ,obj ,new)))))
  'done)

(defun destroysimulator (obj)
  (dolist (agent (find-repositories obj))
    (unless (findp `(and (repository ?x ?y) (distinct ?y ,obj)) *manager*)
      (killagent agent)))
  (kill obj *manager*)
  (makunbound obj)
  'done)


(defmethod commit (agent)
  (let (original)
    (when (setq original (find-original (name agent)))
      (empty (referent original))
      (request `(tell ,(maksand (contents agent))) *receiver* original)
      'done)))

(defmethod commit ((agent symbol))
  (cond ((and (boundp agent) (not (symbolp (symbol-value agent))))
         (commit (symbol-value agent)))
        (t (call-next-method agent))))

(defmethod commit ((agent differentialagent))
  (let (original)
    (setq original (find-original (name agent)))
    (request `(update . ,(contents agent)) *receiver* original)
    (empty agent)
    'done))

(defmethod commit ((agent deltaagent))
  (let (original)
    (setq original (find-original (name agent)))
    (request `(update . ,(contents agent)) *receiver* original)
    (empty agent)
    'done))

(defmethod commit ((agent diffagent))
  (let (original)
    (setq original (find-original (name agent)))
    (request `(update . ,(contents agent)) *receiver* original)
    (empty agent)
    'done))

(defmethod commit ((agent simulator))
  (dolist (helper (find-repositories (name agent))) (commit helper))
  'done)


(defmethod rollback (agent)
  (let (original)
    (when (setq original (find-original (name agent)))
      (empty agent)
      (request `(tell ,(maksand (get-sentences original))) *receiver* agent)
      'done)))

(defmethod rollback ((agent symbol))
  (cond ((and (boundp agent) (not (symbolp (symbol-value agent))))
         (rollback (symbol-value agent)))
        (t (call-next-method agent))))

(defmethod rollback ((agent differentialagent))
  (empty agent)
  'done)

(defmethod rollback ((agent deltaagent))
  (empty agent)
  'done)

(defmethod rollback ((agent simulator))
  (dolist (helper (find-repositories (name agent))) (rollback helper))
  'done)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; simulator tests
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
#|

(definemore *manager*
  '((rootrelation integrator p)
    (isa p naryrelation)
    (arity p 2)
    (specialty warehouse p)
    (interest warehouse p)))
DONE

(definemore *warehouse*
  '((p a b)
    (p a c)
    (p b d)
    (p c f)
    (q b)
    (q c)))
DONE

(createsimulator 'adam 'integrator)
DONE

(request '(tell (p a g)) nil adam)
DONE

(request '(untell (p a c)) nil adam)
DONE

(request '(ask-all (?x ?y) (p ?x ?y)) nil *warehouse*)
((a b) (a c) (b d) (c f))

(request '(ask-all (?x ?y) (p ?x ?y)) nil adam)
((a b) (a c) (b d) (c f))

|#

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
