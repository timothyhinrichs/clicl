;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; (c) Copyright 1999-2003 by Michael Genesereth.  All rights reserved.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(eval-when (compile load eval)
  (proclaim '(special *manager* *intensions* *target*)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; achieve
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmacro achieve (&rest facts)
  `(effectuate ',(maksand facts) *receiver*))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; effectuate
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod effectuate (p (th symbol))
  (cond ((and (boundp th) (not (symbolp (symbol-value th))))
         (effectuate p (symbol-value th)))
        (t (call-next-method p th))))

(defmethod effectuate (p (th fastserver))
  (dolist (p (literals p)) (save p th)))

(defmethod effectuate (p (th dataserver))
  (dolist (p (literals p)) (save p th)))

(defmethod effectuate (p (th fullserver))
  (dolist (p (literals p)) (save p th)))

(defmethod effectuate (p (agent transformer))
  (let (rulebase *target* plan (*intensions* nil))
    (setq rulebase (get-rulebase agent))
    (setq *target* (get-recipient agent))
    (decludes 'infotheory)
    (empty 'infotheory)
    (includes 'infotheory rulebase)
    (setq p (makand p `(legal)))
    (setq plan (fullresidue p 'infotheory #'failure #'basicp #'success))
    (effectuate plan *target*)))

(defmethod effectualization (p (agent transformer))
  (let (rulebase *target* (*intensions* nil))
    (setq rulebase (get-rulebase agent))
    (setq *target* (get-recipient agent))
    (decludes 'infotheory)
    (empty 'infotheory)
    (includes 'infotheory rulebase)
    (fullresidue p 'infotheory #'failure #'basicp #'success)))

(defmethod effectualizations (p (agent transformer))
  (let (rulebase *target* (*intensions* nil))
    (setq rulebase (get-rulebase agent))
    (setq *target* (get-recipient agent))
    (decludes 'infotheory)
    (empty 'infotheory)
    (includes 'infotheory rulebase)
    (fullresidues p 'infotheory #'failure #'basicp #'success)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; tests
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defparameter *mytransformer* (make-instance 'transformer :name 'mytransformer))

(definemore *library*
  '((<= (r ?x ?y ?z) (p ?x ?y) (q ?y ?z))
    (<= (legal) (not (p k1 k2)))
    (<= (legal) (s k1 k2))))

(definemore *manager*
  '((rulebase mytransformer library)
    (base repository p)
    (base repository q)
    (base repository s)))

;;; (effectualization '(and (r a b c) (legal)) *mytransformer*)

(defun stopperp (x)
  (find x '(p q r s p+ p- q+ q- r+ r- s+ s-)))

(defun stopperp (x)
  (find x '(p+ p- q+ q- r+ r- s+ s-)))

(define-theory *library* ""
  '((<= legal rp
        (or (not rp) pp qp) (or sp (not qp))
        (not p+) (not p-) (not r+) (not r-))

    (<= rp pp)
    (<= rp qp)
    (<= sp qp)

    (<= pp p (not p-))
    (<= pp p+)

    (<= qp q (not q-))
    (<= qp q+)

    (<= rp r (not r-))
    (<= rp r+)

    (<= sp s (not s-))
    (<= sp s+)))

(define-theory *library* ""
  '((<= legal
        (or (not rp) pp qp) (or sp (not qp))
        (not p+) (not p-) (not r+) (not r-))

    rp

    (<= pp p (not p-))
    (<= pp p+)

    (<= qp q (not q-))
    (<= qp q+)

    (<= rp r (not r-))
    (<= rp r+)

    (<= sp s (not s-))
    (<= sp s+)))

;;; (fullresidues 'legal *library* #'failure #'stopperp #'success)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
