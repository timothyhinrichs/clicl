;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; (c) Copyright 2005-2008 by Michael Genesereth.  All rights reserved.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; dualserver.lisp
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; dualserver.lisp
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass dualserver (agent) ())

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Knowledge commands
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; affirm, retract
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod affirm (p sender (receiver dualserver))
  (declare (ignore sender))
  (affirm p receiver (includee receiver)))

(defmethod retract (p sender (receiver dualserver))
  (declare (ignore sender))
  (retract p receiver (includee receiver)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; askabout
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod askabout (x sender (receiver dualserver))
  (declare (ignore sender))
  (askabout x receiver (includee receiver)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Data commands
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; revise, revisions, errors, materializations, notifications, reactions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod revise (p sender (receiver dualserver))
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

(defmethod revisions (p (receiver dualserver))
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

(defmethod errors (p (receiver dualserver))
  (let (temp answers)
    (setq temp (make-instance 'theory))
    (insertposneg p temp)
    (includes temp receiver)
    (setq answers (viewfinds '?x '(error ?x) temp))
    (decludes temp)
    (empty temp)
    answers))


(defmethod materializations (p (th dualserver))
  (declare (ignore p th))
  nil)

(defmethod reactions (p (receiver dualserver))
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

(defmethod askp (p sender (th dualserver))
  (declare (ignore sender))
  (viewfindp p th))

(defmethod askx (x p sender (th dualserver))
  (declare (ignore sender))
  (viewfindx x p th))

(defmethod asks (x p sender (th dualserver))
  (declare (ignore sender))
  (viewfinds x p th))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; askclass, askframe, asktable
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod askclass (x sender (receiver dualserver))
  (declare (ignore sender))
  (classify x receiver))

(defmethod askframe (x class sender (receiver dualserver))
  (declare (ignore x class sender receiver))
  nil)

(defmethod asktable (ol sl sender (receiver dualserver))
  (call-next-method ol sl sender receiver))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Inference commands
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; save, drop, change
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod change (p (receiver dualserver))
  (cond ((atom p) (save p receiver))
        ((eq (car p) 'not) (drop (cadr p) receiver))
        ((eq (car p) 'and) (dolist (p (cdr p)) (change p receiver)))
        (t (save p receiver))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; facts, rules
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod facts (x (th dualserver) &optional (f #'matchp))
  (call-next-method x th f))

(defmethod rules (x (th dualserver) &optional (f #'matchp))
  (declare (ignore x th f))
  nil)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; envindexps, indexps
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod envindexps (p al (th dualserver))
  (cond ((varp p) (contents th))
        ((atom p) (indexees p th))
        (t (do ((l (cdr p) (cdr l)) (dum))
               ((null l) (indexps (car p) th))
               (setq dum (unival (car l) al))
               (cond ((varp dum))
                     ((atom dum) (return (indexees dum th))))))))

(defmethod indexps (p (th dualserver))
  (flatindexps p th))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Structure Commands
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Class Commands
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; instancep, classify, instance, instances
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod classify (object (source dualserver))
  (let (dum)
    (cond ((characterp object) 'character)
          ((stringp object) 'string)
          ((numberp object) 'number)
          ((setq dum (find-classifier (name source)))
           (or (result dum object source) 'thing))
          ((transclass object source))
          ((ruleclass object source (includee source)))
          (t 'thing))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; doublep, itemx, items
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod doublep (predicate object (source dualserver))
  (cond ((fullviewp predicate (includee source))
         (viewfindp (list predicate object) source))
        (t (factdoublep predicate object source))))

(defmethod itemx (predicate (source dualserver))
  (cond ((fullviewp predicate (includee source))
         (viewfindx '?x (list predicate '?x) source))
        (t (factitemx predicate source))))

(defmethod items (predicate (source dualserver))
  (cond ((fullviewp predicate (includee source))
         (viewfinds '?x (list predicate '?x) source))
        (t (factitems predicate source))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; triplep, object, objects, result, results, options
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod triplep (slot object result (source dualserver))
  (cond ((fullviewp slot (includee source))
         (viewfindp (list slot object result) source))
        (t (facttriplep slot object result source))))

(defmethod object (slot result (source dualserver))
  (cond ((fullviewp slot (includee source))
         (viewfindx '?x (list slot '?x result) source))
        (t (factobject slot result source))))

(defmethod objects (slot result (source dualserver))
  (cond ((fullviewp slot (includee source))
         (viewfinds '?x (list slot '?x result) source))
        (t (factobjects slot result source))))

(defmethod result (slot object (source dualserver))
  (cond ((fullviewp slot (includee source))
         (viewfindx '?y (list slot object '?y) source))
        (t (factresult slot object source))))

(defmethod results (slot object (source dualserver))
  (cond ((fullviewp slot (includee source))
         (viewfinds '?y (list slot object '?y) source))
        (t (factresults slot object source))))

(defmethod options (slot class (source dualserver))
  (cond ((or (fullviewp slot (includee source))
	     (fullviewp (find-predicate class) (includee source)))
         (genoptions slot class source))
        (t (factoptions slot class source))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
