;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; (c) Copyright 2005-2008 by Michael Genesereth.  All rights reserved.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; secureserver.lisp
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; secureserver.lisp
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass secureserver (dualserver) ())

; See dualserver class definition for additional methods
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; askp, askx, asks
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; left alone for now
(defmethod askp (p sender (th secureserver))
  (declare (ignore sender))
  (viewfindp p th))

(defmethod askx (x p sender (th secureserver))
  (declare (ignore sender))
  (viewfindx x p th))

(defmethod asks (x p sender (th secureserver))
  (declare (ignore sender))
  (viewfinds x p th))

;  Idea: authorization policy concludes statements of the form
;     deny(user,phi(x)), where phi(x) is a metalevel statement.
;     Then we (recursively) augment a query so that for each atomic statement
;     p(tbar), the result is p(tbar) ^ -deny(user,'p(tbar)).  Then we
;     use metalevel unification.  
;  Right now, we can't describe arbitrary-arity predicates where our
;     special variable (documents) can occur anywhere.  To do so, we'd
;     need (deny tlh (listof ?p @x ?doc @y)), and that's illegal in KIF.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Inference commands
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; save, drop, change
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
#|
; don't know
(defmethod change (p (receiver secureserver))
  (cond ((atom p) (save p receiver))
        ((eq (car p) 'not) (drop (cadr p) receiver))
        ((eq (car p) 'and) (dolist (p (cdr p)) (change p receiver)))
        (t (save p receiver))))
|#
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; facts, rules
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#|
; don't know
(defmethod facts (x (th secureserver) &optional (f #'matchp))
  (call-next-method x th f))

(defmethod rules (x (th secureserver) &optional (f #'matchp))
  (declare (ignore x th f))
  nil)
|#
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; envindexps, indexps
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
#|
; don't know
(defmethod envindexps (p al (th secureserver))
  (cond ((varp p) (contents th))
        ((atom p) (indexees p th))
        (t (do ((l (cdr p) (cdr l)) (dum))
               ((null l) (indexps (car p) th))
               (setq dum (unival (car l) al))
               (cond ((varp dum))
                     ((atom dum) (return (indexees dum th))))))))

(defmethod indexps (p (th secureserver))
  (flatindexps p th))
|#
#|
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; doublep, itemx, items
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; unaltered for now
(defmethod doublep (predicate object (source secureserver))
  (cond ((fullviewp predicate (includee source))
         (viewfindp (list predicate object) source))
        (t (factdoublep predicate object source))))

(defmethod itemx (predicate (source secureserver))
  (cond ((fullviewp predicate (includee source))
         (viewfindx '?x (list predicate '?x) source))
        (t (factitemx predicate source))))

(defmethod items (predicate (source secureserver))
  (cond ((fullviewp predicate (includee source))
         (viewfinds '?x (list predicate '?x) source))
        (t (factitems predicate source))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; triplep, object, objects, result, results, options
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; unaltered for now
(defmethod triplep (slot object result (source secureserver))
  (cond ((fullviewp slot (includee source))
         (viewfindp (list slot object result) source))
        (t (facttriplep slot object result source))))

(defmethod object (slot result (source secureserver))
  (cond ((fullviewp slot (includee source))
         (viewfindx '?x (list slot '?x result) source))
        (t (factobject slot result source))))

(defmethod objects (slot result (source secureserver))
  (cond ((fullviewp slot (includee source))
         (viewfinds '?x (list slot '?x result) source))
        (t (factobjects slot result source))))

(defmethod result (slot object (source secureserver))
  (cond ((fullviewp slot (includee source))
         (viewfindx '?y (list slot object '?y) source))
        (t (factresult slot object source))))

(defmethod results (slot object (source secureserver))
  (cond ((fullviewp slot (includee source))
         (viewfinds '?y (list slot object '?y) source))
        (t (factresults slot object source))))

(defmethod options (slot class (source secureserver))
  (cond ((or (fullviewp slot (includee source))
	     (fullviewp (find-predicate class) (includee source)))
         (genoptions slot class source))
        (t (factoptions slot class source))))
|#

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
