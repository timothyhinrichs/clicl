;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; (c) Copyright 2005-2008 by Michael Genesereth.  All rights reserved.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; fastserver.lisp
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; fastserver
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass fastserver (agent) ())

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Knowledge commands
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; affirm, retract
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod affirm (p sender (receiver fastserver))
  (declare (ignore sender))
  (affirm p receiver (includee receiver)))

(defmethod retract (p sender (receiver fastserver))
  (declare (ignore sender))
  (retract p receiver (includee receiver)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; askabout
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod askabout (x sender (receiver fastserver))
  (declare (ignore sender))
  (askabout x receiver (includee receiver)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Data commands
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; revise, revisions, errors, materializations, notifications, reactions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod revise (p sender (receiver fastserver))
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

(defmethod revisions (p (receiver fastserver))
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

(defmethod errors (p (receiver fastserver))
  (let (temp answers)
    (setq temp (make-instance 'theory))
    (insertposneg p temp)
    (includes temp receiver)
    (setq answers (viewfinds '?x '(error ?x) temp))
    (decludes temp)
    (empty temp)
    answers))

(defmethod materializations (p (th fastserver))
  (declare (ignore p th))
  nil)

(defmethod reactions (p (receiver fastserver))
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

(defmethod askp (p sender (th fastserver))
  (declare (ignore sender))
  (fastfindp p th))

(defmethod askx (x p sender (th fastserver))
  (declare (ignore sender))
  (fastfindx x p th))

(defmethod asks (x p sender (th fastserver))
  (declare (ignore sender))
  (fastfinds x p th))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; askclass, askframe, asktable
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod askclass (x sender (receiver fastserver))
  (declare (ignore sender))
  (classify x receiver))

(defmethod askframe (x class sender (receiver fastserver))
  (declare (ignore x class sender receiver))
  nil)

(defmethod asktable (ol sl sender (receiver fastserver))
  (call-next-method ol sl sender receiver))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Inference commands
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; findp, findx, finds
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod findp (p (th fastserver))
  (fastfindp p th))

(defmethod findx (x p (th fastserver))
  (fastfindx x p th))

(defmethod finds (x p (th fastserver))
  (fastfinds x p th))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Theory commands
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; save, drop, change
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod change (p (th fastserver))
  (cond ((atom p) (save p th))
        ((eq (car p) 'not) (drop (cadr p) th))
        ((eq (car p) 'and) (dolist (p (cdr p)) (change p th)))
        (t (save p th))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; knownp, knownx, knowns
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod knownp (p (th fastserver) &optional (f 'matchp))
  (declare (ignore f))
  (dataknownp p th))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; facts, rules
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod facts (x (th fastserver) &optional (f #'matchp))
  (call-next-method x th f))

(defmethod rules (x (th fastserver) &optional (f #'matchp))
  (declare (ignore x th f))
  nil)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; envindexps, indexps
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod envindexps (p al (th fastserver))
  (cond ((varp p) (contents th))
        ((atom p) (indexees p th))
        (t (do ((l (cdr p) (cdr l)) (dum))
               ((null l) (indexps (car p) th))
               (setq dum (unival (car l) al))
               (cond ((varp dum))
                     ((atom dum) (return (indexees dum th))))))))

(defmethod indexps (p (th fastserver))
  (flatindexps p th))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Structure Commands
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; findinstancep, findinstance, findinstances, findoptions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Class Commands
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; instancep, classify, instance, instances
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod instancep (object class (source fastserver))
  (triplep 'isa object class source))

(defmethod classify (object (source fastserver))
  (cond ((characterp object) 'character)
        ((stringp object) 'string)
        ((numberp object) 'number)
        ((result 'isa object source))
        (t 'thing)))

(defmethod instance (class (source fastserver))
  (object 'isa class source))

(defmethod instances (class (source fastserver))
  (objects 'isa class source))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; doublep, items, filteritems
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod doublep (predicate object (source fastserver))
  (cond ((fullviewp predicate (includee source))
         (fastfindp (list predicate object) source))
        (t (factdoublep predicate object source))))

(defmethod itemx (predicate (source fastserver))
  (cond ((fullviewp predicate (includee source))
         (fastfindx '?x (list predicate '?x) source))
        (t (factitemx predicate source))))

(defmethod items (predicate (source fastserver))
  (cond ((fullviewp predicate (includee source))
         (fastfinds '?x (list predicate '?x) source))
        (t (factitems predicate source))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; triplep, objects, results, options
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod triplep (slot object result (source fastserver))
  (cond ((fullviewp slot (includee source))
         (fastfindp (list slot object result) source))
        (t (facttriplep slot object result source))))

(defmethod object (slot result (source fastserver))
  (cond ((fullviewp slot (includee source))
         (fastfindx '?x (list slot '?x result) source))
        (t (factobject slot result source))))

(defmethod objects (slot result (source fastserver))
  (cond ((fullviewp slot (includee source))
         (fastfinds '?x (list slot '?x result) source))
        (t (factobjects slot result source))))

(defmethod result (slot object (source fastserver))
  (cond ((fullviewp slot (includee source))
         (fastfindx '?y (list slot object '?y) source))
        (t (factresult slot object source))))

(defmethod results (slot object (source fastserver))
  (cond ((fullviewp slot (includee source))
         (fastfinds '?y (list slot object '?y) source))
        (t (factresults slot object source))))

(defmethod options (slot class (source fastserver))
  (cond ((fullviewp slot (includee source))
         (genoptions slot class source))
        (t (fastserveroptions slot class source))))

(defun fastserveroptions (attribute class source)
  (do ((l (indexees attribute source) (cdr l)) (nl))
      ((null l) (nreverse (uniquify nl)))
      (when (and (eq (caar l) attribute)
                 (facttriplep 'isa (cadar l) class source))
        (setq nl (cons (caddar l) nl)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
