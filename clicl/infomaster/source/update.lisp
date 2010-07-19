;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; (c) Copyright 1999-2003 by Michael Genesereth.  All rights reserved.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(eval-when (compile load eval)
  (proclaim '(special *manager* *intensions* *target*)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; reviser
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; general
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod reviser (facts receiver)
  (declare (ignore facts receiver))
  nil)

(defmethod reviser (facts (receiver symbol))
  (cond ((and (boundp receiver) (not (symbolp (symbol-value receiver))))
         (reviser facts (symbol-value receiver)))
        (t (call-next-method facts receiver))))

(defmethod reviser (facts (th fastserver))
  (change (maksand facts) th))

(defmethod reviser (facts (th dataserver))
  (change (maksand facts) th))

(defmethod reviser (facts (th ruleserver))
  (change (maksand facts) th))

(defmethod reviser (facts (th fullserver))
  (change (maksand facts) th))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Translator
;;; needs work
;;; nonviewp checks infotheory.  ugh.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod change (p (th translator))
  (let (*target*)
    (setq *target* (find-target (name th)))
    (request (planchange p th) th *target*)))

(defun planchange (p th)
  (let (*library* *target* (*intensions* nil))
    (setq *library* (symbol-value (find-rulebase th)))
    (setq *target* (find-target (name th)))
    (decludes 'infotheory)
    (empty 'infotheory)
    (includes 'infotheory *library*)
    (do ((l (newconsequences p 'infotheory #'unviewp #'unsavep) (cdr l)) (nl))
        ((null l) `(update . ,(nreverse nl)))
        (cond ((and (listp (car l)) (eq 'execute (caar l)))
               (setq nl (cons (cadar l) nl)))
              ((and (listp (car l)) (eq 'evaluate (caar l)))
               (setq nl (cons `(apply ',(caadar l) ',(cdadar l)) nl)))
              ((and (listp (car l)) (eq '=> (caar l)))
               (setq nl (cons (decolonize `(==> ,(cadar l) ,(caddar l))) nl)))
              (t (setq nl (cons (decolonize (car l)) nl)))))))

(defmethod reviser (facts (th translator))
  (let (*library* factserver changes)
    (setq *library* (symbol-value (find-rulebase th)))
    (setq factserver (find-target (name th)))
    (setq facts (transreducer facts th))
    (request `(update . ,facts) th factserver)
    (dolist (rule (ramifications (maksand facts) *library* #'unviewp #'interestp))
      (cond ((atom rule) (setq changes (cons rule changes)))
            ((eq '<= (car rule))
             (setq changes (nreconc (request `(ask-all ,(cadr rule) ,(maksand (cddr rule))) nil factserver) changes)))
            (t (setq changes (cons rule changes)))))
    (request `(update . ,(nreverse changes)) th factserver)
    'done))

(defun transreducer (facts receiver)
  (let (rulebase (*intensions* nil))
    (setq rulebase (find-rulebase (name receiver)))
    (when (and rulebase (symbolp rulebase) (boundp rulebase))
      (setq rulebase (symbol-value rulebase)))
    (getconjuncts (residue t (maksand facts) rulebase #'unviewp))))

(defun unviewp (x)
  (not (fullviewp x *library*)))

(defun unsavep (x)
  (not (fullsavep x *library*)))

(defun fullsavep (x th)
  (cond ((fullsavepth x th))
        (t (some #'(lambda (th) (fullsavep x th)) (includees th)))))

(defun fullsavepth (x th)
  (do ((l (indexees x th) (cdr l)))
      ((null l) nil)
      (cond ((atom (car l)))
            ((eq '=> (caar l))
             (when (and (listp (cadar l)) (eq x (caadar l))) (return t))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Facilitator
;;; okay but could be better
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod reviser (facts (th facilitator))
  (let (ruleserver changes)
    (setq ruleserver (symbol-value (find-rulebase th)))
    (setq facts (reducer facts th))
    (eval (divvy `(update . ,facts)))
    (dolist (rule (ramifications (maksand facts) ruleserver #'basicp #'interestp))
      (cond ((atom rule) (setq changes (cons rule changes)))
            ((eq '<= (car rule))
             (setq changes (nreconc (finds (cadr rule) (maksand (cddr rule)) th) changes)))
            (t (setq changes (cons rule changes)))))
    (eval (divvy `(update . ,changes)))
    'done))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Translator
;;; needs work
;;; nonviewp checks infotheory.  ugh.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod revise (facts (th translator))
  (let (ruleserver factserver)
    (setq ruleserver (symbol-value (find-rulebase th)))
    (setq factserver (find-target (name th)))
    (decludes 'infotheory)
    (empty 'infotheory)
    (includes 'infotheory ruleserver)
    (setq facts (reducetransviews facts th))
    (request `(update . ,facts) th factserver)
    (setq facts (ramifytransviews facts factserver ruleserver))
    (request `(update . ,facts) th factserver)
    'done))

(defun reducetransviews (facts receiver)
  (let (rulebase (*intensions* nil))
    (setq rulebase (find-rulebase (name receiver)))
    (when (and rulebase (symbolp rulebase) (boundp rulebase))
      (setq rulebase (symbol-value rulebase)))
    (getconjuncts (residue t (maksand facts) rulebase #'nonviewp))))

(defun ramifytransviews (facts factserver ruleserver)
  (let (insertions deletions bases materials changes)
    (multiple-value-setq (insertions deletions) (divide facts))
    (dolist (p facts) (setq bases (getbases p bases)))
    (dolist (r bases) (setq materials (gettransmaterials r ruleserver materials)))
    (dolist (r materials)
      (dolist (rule (gettranschanges r bases insertions deletions ruleserver))
        (setq changes (nreconc (request `(ask-all ,(cadr rule) ,(maksand (cddr rule))) nil factserver) changes))))
    (nreverse (uniquify changes))))

(defun gettransmaterials (p th nl)
  (do ((l (indexees p th) (cdr l)))
      ((null l) nl)
      (cond ((atom (car l)))
            ((eq '<= (caar l))
             (cond ((atom (cadar l)))
                   ((find (operator (cadar l)) nl))
                   ((amongp p (cddar l) #'eq)
                    (setq nl (cons (operator (cadar l)) nl))
                    (setq nl (getmaterials (operator (cadar l)) th nl))))))))

(defun gettranschanges (r bases positives negatives th)
  (do ((l (getrules r th) (cdr l)) (nl))
      ((null l) (nreverse nl))
      (do ((m (cddar l) (cdr m)) (om) (data) (new))
          ((null m))
          (cond ((atom (car m)))
                ((find (caar m) bases)
                 (when (setq data (getdata (caar m) positives))
                   (setq new `(oneof ,(car m) . ,data))
                   (setq new `(<= ,(cadar l) ,new . ,(revappend om (cdr m))))
                   (setq nl (cons new nl))))
                ((and (eq (caar m) 'not) (find (caadar m) bases))
                 (when (setq data (getdata (caadar m) negatives))
                   (setq new `(oneof ,(cadar m) . ,data))
                   (setq new `(<= ,(cadar l) ,new . ,(revappend om (cdr m))))
                   (setq nl (cons new nl)))))
          (setq om (cons (car m) om)))))

(defun gettransrules (r th)
  (nconc (normalizevars (nonreductions r th #'nonviewp #'success))
         (normalizevars (reductions r th #'nonviewp #'success))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Facilitator
;;; okay but could be better
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod revise (facts (th facilitator))
  (let (ruleserver)
    (setq ruleserver (symbol-value (find-rulebase th)))
    (setq facts (reduceviews facts th))
    (eval (divvy `(update . ,facts)))
    (setq facts (ramifyviews facts th ruleserver))
    (eval (divvy `(update . ,facts)))
    'done))

(defun reduceviewsfac (facts receiver)
  (let (rulebase (*intensions* nil))
    (setq rulebase (find-rulebase (name receiver)))
    (when (and rulebase (symbolp rulebase) (boundp rulebase))
      (setq rulebase (symbol-value rulebase)))
    (getconjuncts (residue t (maksand facts) rulebase #'basicp))))

(defun ramifyviewsfac (facts facilitator ruleserver)
  (let (insertions deletions bases materials changes)
    (multiple-value-setq (insertions deletions) (divide facts))
    (dolist (p facts) (setq bases (getbases p bases)))
    (dolist (r bases) (setq materials (getmaterials r ruleserver materials)))
    (dolist (r materials)
      (dolist (rule (getchanges r bases insertions deletions ruleserver))
        (setq changes (nreconc (finds (cadr rule) (maksand (cddr rule)) facilitator) changes))))
    (nreverse (uniquify changes))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
