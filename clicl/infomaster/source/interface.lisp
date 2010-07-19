;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; (c) Copyright 1999-2006 by Michael Genesereth.  All rights reserved.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; interface.lisp
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(eval-when (compile load eval)
  (proclaim '(special *manager* *intensions* *target*)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; interface
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass interface (agent) ())

(defmethod message-handler (*message* *sender* (*receiver* interface))
  (let ((target (find-recipient (name *receiver*))))
    (when target (request *message* *sender* target))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Knowledge commands
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; affirm, retract
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod affirm (p sender (receiver interface))
  (let ((target (find-recipient (name receiver))))
    (when target (affirm p sender receiver))))

(defmethod retract (p sender (receiver interface))
  (let ((target (find-recipient (name receiver))))
    (when target (retract p sender receiver))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; askabout
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod askabout (x sender (receiver interface))
  (let ((target (find-recipient (name receiver))))
    (when target (askabout x sender receiver))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Data commands
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; revise, revisions, errors, materializations, notifications, reactions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod revise (p sender (receiver interface))
  (declare (ignore sender))
  (let ((target (find-recipient (name receiver))))
    (when target (revise p receiver target))))

(defmethod revisions (p (receiver interface))
  (let ((target (find-recipient (name receiver))))
    (when target (revisions p target))))

(defmethod errors (p (receiver interface))
  (let ((target (find-recipient (name receiver))))
    (when target (errors p target))))

(defmethod materializations (p (receiver interface))
  (let ((target (find-recipient (name receiver))))
    (when target (materializations p target))))

(defmethod notifications (p (receiver interface))
  (let ((target (find-recipient (name receiver))))
    (when target (notifications p target))))

(defmethod reactions (p (receiver interface))
  (let ((target (find-recipient (name receiver))))
    (when target (reactions p target))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; askp, askx, asks
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod askp (p sender (receiver interface))
  (let ((target (find-recipient (name receiver))))
    (when target (askp p sender target))))

(defmethod askx (x p sender (receiver interface))
  (let ((target (find-recipient (name receiver))))
    (when target (askx x p sender target))))

(defmethod asks (x p sender (receiver interface))
  (let ((target (find-recipient (name receiver))))
    (when target (asks x p sender target))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; askclass, askframe, asktable
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod askclass (x sender (receiver interface))
  (declare (ignore sender))
  (classify x receiver))

(defmethod askframe (x class sender (receiver interface))
  (declare (ignore x class sender receiver))
  nil)

(defmethod asktable (ol sl sender (receiver interface))
  (let ((target (find-recipient (name receiver))))
    (when target (asktable ol sl sender target))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Theory commands
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; save, drop, change
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod save (p (source diffserver) &optional (f 'samep))
  (let ((target (find-recipient (name source))))
    (when target (facts p target f))))

(defmethod drop (p (source diffserver) &optional (f 'samep))
  (let ((target (find-recipient (name source))))
    (when target (drop p target f))))

(defmethod change (p (source diffserver))
  (let ((target (find-recipient (name source))))
    (when target (change p target))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; knownp, knownx, knowns
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; facts, rules
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod facts (x (source interface) &optional (f 'matchp))
  (let ((target (find-recipient (name source))))
    (when target (facts x target f))))

(defmethod rules (x (source interface) &optional (f 'matchp))
  (let ((target (find-recipient (name source))))
    (when target (rules x target f))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Structure Commands
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; findinstancep, findinstances, findoptions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod saveobject (structure (source interface))
  (let ((target (find-recipient (name source))))
    (when target (saveobject structure target))))

(defmethod killobject (object (source interface))
  (let ((target (find-recipient (name source))))
    (when target (killobject object target))))

(defmethod findinstancep (x structure (source interface))
  (let ((target (find-recipient (name source))))
    (when target (findinstancep x structure target))))

(defmethod findinstance (structure (source interface))
  (let ((target (find-recipient (name source))))
    (when target (findinstance structure target))))

(defmethod findinstances (structure (source interface))
  (let ((target (find-recipient (name source))))
    (when target (findinstances structure target))))

(defmethod findoptions (attribute structure (source interface))
  (let ((target (find-recipient (name source))))
    (when target (findoptions attribute structure target))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Class Commands
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; instancep, classify, instance, instances
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod instancep (x class (source interface))
  (let ((target (find-recipient (name source))))
    (when target (instancep x class target))))

(defmethod classify (x (source interface))
  (let (dum)
    (cond ((setq dum (find-classifier (name source)))
           (or (askx '?class (list dum x '?class) source source) 'thing))
          (t (classify x (find-target (name source)))))))

(defmethod instance (class (source interface))
  (let ((target (find-recipient (name source))))
    (when target (instance class target))))

(defmethod instances (class (source interface))
  (let ((target (find-recipient (name source))))
    (when target (instances class target))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; doublep, itemx items
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod doublep (predicate item (source interface))
  (let ((target (find-recipient (name source))))
    (when target (doublep predicate item target))))

(defmethod itemx (predicate (source interface))
  (let ((target (find-recipient (name source))))
    (when target (itemx predicate target))))

(defmethod items (predicate (source interface))
  (let ((target (find-recipient (name source))))
    (when target (items predicate target))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; triplep, objects, result, results, options
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod triplep (slot object value (source interface))
  (let ((target (find-recipient (name source))))
    (when target (triplep slot object value target))))

(defmethod object (slot value (source interface))
  (let ((target (find-recipient (name source))))
    (when target (object slot value target))))

(defmethod objects (slot value (source interface))
  (let ((target (find-recipient (name source))))
    (when target (objects slot value target))))

(defmethod result (slot item (source interface))
  (let ((target (find-recipient (name source))))
    (when target (result slot item target))))

(defmethod results (slot item (source interface))
  (let ((target (find-recipient (name source))))
    (when target (results slot item target))))

(defmethod options (attribute class (source interface))
  (let ((target (find-recipient (name source))))
    (when target (options attribute class target))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
