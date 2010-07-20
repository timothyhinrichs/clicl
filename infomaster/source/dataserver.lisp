;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; (c) Copyright 1999-2005 by Michael Genesereth.  All rights reserved.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; dataserver.lisp
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass dataserver (agent) ())

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Knowledge commands
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; affirm, retract
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod affirm (p sender (receiver dataserver))
  (declare (ignore p sender receiver))
  nil)

(defmethod retract (p sender (receiver dataserver))
  (declare (ignore p sender receiver))
  nil)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; askabout
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod askabout (x sender (receiver dataserver))
  (declare (ignore x sender receiver))
  nil)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Data commands
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; revise, revisions, errors, materializations, notifications, reactions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod revise (p sender (receiver dataserver))
  (declare (ignore sender))
  (change p receiver))

(defmethod revisions (p (th dataserver))
  (declare (ignore th))
  (integrals p))

(defmethod errors (p (th dataserver))
  (declare (ignore p th))
  nil)

(defmethod materializations (p (th dataserver))
  (declare (ignore p th))
  nil)

(defmethod notifications (p (th dataserver))
  (declare (ignore p th))
  nil)

(defmethod reactions (p (th dataserver))
  (declare (ignore p th))
  nil)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; askp, askx, asks
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod askp (p sender (th dataserver))
  (declare (ignore sender))
  (datafindp p th))

(defmethod askx (x p sender (th dataserver))
  (declare (ignore sender))
  (datafindx x p th))

(defmethod asks (x p sender (th dataserver))
  (declare (ignore sender))
  (datafinds x p th))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; askclass, askabout, asktable
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod askclass (x sender (receiver dataserver))
  (declare (ignore sender))
  (classify x receiver))

(defmethod askframe (x class sender (receiver dataserver))
  (declare (ignore x class sender receiver))
  nil)

(defmethod asktable (ol sl sender (receiver dataserver))
  (call-next-method ol sl sender receiver))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Theory commands
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; save, drop, change
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod change (p (th dataserver))
  (cond ((atom p) (save p th))
        ((eq (car p) 'not) (drop (cadr p) th))
        ((eq (car p) 'and) (dolist (p (cdr p)) (change p th)))
        (t (save p th))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; classify, facts, rules
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod classify (x (th dataserver))
  (let (dum)
    (cond ((characterp x) 'character)
          ((stringp x) 'string)
          ((numberp x) 'number)
          ((setq dum (find-classifier (name th)))
           (or (askx '?class (list dum x '?class) th th) 'thing))
          ((getclass x th))
          (t 'thing))))

(defmethod facts (x (th dataserver) &optional (f #'matchp))
  (call-next-method x th f))

(defmethod rules (x (th dataserver) &optional (f #'matchp))
  (declare (ignore x th f))
  nil)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; envindexps, indexps
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod envindexps (p al (th dataserver))
  (cond ((varp p) (contents th))
        ((atom p) (indexees p th))
        (t (do ((l (cdr p) (cdr l)) (dum))
               ((null l) (indexps (car p) th))
               (setq dum (unival (car l) al))
               (cond ((varp dum))
                     ((atom dum) (return (indexees dum th))))))))

(defmethod indexps (p (th dataserver))
  (flatindexps p th))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
