;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; (c) Copyright 2001-2005 by Michael Genesereth.  All rights reserved.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; factserver.lisp
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass factserver (agent) ())

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Knowledge commands
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; affirm, retract
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod affirm (p sender (receiver factserver))
  (declare (ignore p sender receiver))
  nil)

(defmethod retract (p sender (receiver factserver))
  (declare (ignore p sender receiver))
  nil)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; askabout
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod askabout (x sender (receiver factserver))
  (declare (ignore x sender receiver))
  nil)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Data commands
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; revise, revisions, errors, materializations, notifications, reactions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod revise (p sender (receiver factserver))
  (declare (ignore sender))
  (change p receiver))

(defmethod revisions (p (receiver factserver))
  (declare (ignore receiver))
  (integrals p))

(defmethod errors (p (receiver factserver))
  (declare (ignore p receiver))
  nil)

(defmethod materializations (p (receiver factserver))
  (declare (ignore p receiver))
  nil)

(defmethod notifications (p (receiver factserver))
  (declare (ignore p receiver))
  nil)

(defmethod reactions (p (receiver factserver))
  (declare (ignore p receiver))
  nil)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; askp, askx, asks
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod askp (p sender (receiver factserver))
  (declare (ignore sender))
  (factfindp p receiver))

(defmethod askx (x p sender (receiver factserver))
  (declare (ignore sender))
  (factfindx x p receiver))

(defmethod asks (x p sender (receiver factserver))
  (declare (ignore sender))
  (factfinds x p receiver))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; askclass, askframe, asktable
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod askclass (x sender (receiver factserver))
  (declare (ignore sender))
  (classify x receiver))

(defmethod askframe (x class sender (receiver factserver))
  (declare (ignore x class sender receiver))
  nil)

(defmethod asktable (ol sl sender (receiver factserver))
  (call-next-method ol sl sender receiver))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Theory commands
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; save, drop, change
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod change (p (th factserver))
  (cond ((atom p) (save p th))
        ((eq (car p) 'not) (drop (cadr p) th))
        ((eq (car p) 'and) (dolist (p (cdr p)) (change p th)))
        (t (save p th))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; knownp, knownx, knowns
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; facts, rules
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod facts (x (th factserver) &optional (f #'matchp))
  (call-next-method x th f))

(defmethod rules (x (th factserver) &optional (f #'matchp))
  (declare (ignore x th f))
  nil)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; envindexps, indexps
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod envindexps (p al (th factserver))
  (cond ((varp p) (contents th))
        ((atom p) (indexees p th))
        (t (do ((l (cdr p) (cdr l)) (dum))
               ((null l) (indexps (car p) th))
               (setq dum (unival (car l) al))
               (cond ((varp dum))
                     ((atom dum) (return (indexees dum th))))))))

(defmethod indexps (p (th factserver))
  (flatindexps p th))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Miscellaneous
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun differentials (p)
  (cond ((atom p) p)
        ((eq (car p) 'and) (cons 'and (mapcar #'differentials (cdr p))))
        ((eq (car p) 'minus) `(not ,(cadr p)))
        ((eq (car p) 'neg) `(not ,(cadr p)))
        ((eq (car p) 'plus) (cadr p))
        ((eq (car p) 'pos) (cadr p))
        (t p)))

(defun integrals (p)
  (nreverse (integralsexp p nil)))

(defun integralsexp (p nl)
  (cond ((atom p) (add p nl))
        ((eq (car p) 'and)
         (do ((l (cdr p) (cdr l)))
             ((null l) nl)
             (setq nl (integralsexp (car l) nl))))
        ((eq (car p) 'minus) (add `(not ,(cadr p)) nl))
        ((eq (car p) 'neg) (add `(not ,(cadr p)) nl))
        ((eq (car p) 'plus) (add (cadr p) nl))
        ((eq (car p) 'pos) (add (cadr p) nl))
        (t (add p nl))))

(defun add (x nl)
  (adjoin x nl :test #'equalp))

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

(defmethod classify (x (th factserver))
  (let (dum)
    (cond ((characterp x) 'character)
          ((stringp x) 'string)
          ((numberp x) 'number)
          ((setq dum (find-classifier (name th)))
           (or (result dum x th) 'thing))
          ((getclass x th))
          (t 'thing))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; doublep, itemx, items
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod doublep (predicate object (source factserver))
  (factdoublep predicate object source))

(defun factdoublep (predicate object source)
  (do ((l (indexees object source) (cdr l)))
      ((null l) nil)
      (when (and (eq (caar l) predicate) (equalp (cadar l) object))
        (return t))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod itemx (predicate (source factserver))
  (factitemx predicate source))

(defun factitemx (predicate source)
  (do ((l (indexees predicate source) (cdr l)))
      ((null l) nil)
      (when (eq (caar l) predicate) (return (cadar l)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod items (predicate (source factserver))
  (factitems predicate source))

(defun factitems (predicate source)
  (do ((l (indexees predicate source) (cdr l)) (nl))
      ((null l) (nreverse nl))
      (when (eq (caar l) predicate) (setq nl (cons (cadar l) nl)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; triplep, object, objects, result, results, options
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod triplep (slot object result (source factserver))
  (facttriplep slot object result source))

(defun facttriplep (slot item value source)
  (do ((l (indexees item source) (cdr l)))
      ((null l) nil)
      (when (and (eq (caar l) slot)
                 (equalp (cadar l) item)
                 (equalp (caddar l) value))
        (return t))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod object (attribute value (source factserver))
  (factobject attribute value source))

(defun factobject (slot value source)
  (do ((l (indexees value source) (cdr l)))
      ((null l) nil)
      (when (and (eq (caar l) slot) (equalp (caddar l) value))
        (return (cadar l)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod objects (attribute value (source factserver))
  (factobjects attribute value source))

(defun factobjects (slot value source)
  (do ((l (indexees value source) (cdr l)) (nl))
      ((null l) (nreverse nl))
      (when (and (eq (caar l) slot) (equalp (caddar l) value))
        (setq nl (cons (cadar l) nl)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod result (attribute object (source factserver))
  (factresult attribute object source))

(defun factresult (attribute object source)
  (do ((l (indexees object source) (cdr l)))
      ((null l) nil)
      (when (and (eq (caar l) attribute) (equalp (cadar l) object))
        (return (caddar l)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod results (attribute object (source factserver))
  (factresults attribute object source))

(defun factresults (attribute object source)
  (do ((l (indexees object source) (cdr l)) (nl))
      ((null l) (nreverse nl))
      (when (and (eq (caar l) attribute) (equalp (cadar l) object))
        (setq nl (cons (caddar l) nl)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod options (attribute class (source factserver))
  (factoptions attribute class source))

(defun factoptions (attribute class source)
  (let (predicate)
    (setq predicate (find-predicate class))
    (do ((l (indexees attribute source) (cdr l)) (nl))
        ((null l) (nreverse (uniquify nl)))
        (when (and (eq (caar l) attribute)
                   (doublep predicate (cadar l) source))
          (setq nl (cons (caddar l) nl))))))

(defun factoptions (slot class source)
  (do ((l (factitems (find-predicate class) source) (cdr l)) (nl))
      ((null l) (nreverse nl))
      (do ((m (factresults slot (car l) source) (cdr m)))
          ((null m))
          (setq nl (adjoin (car m) nl :test #'equalp)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
