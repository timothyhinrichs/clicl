;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; (c) Copyright 2006-2008 by Michael Genesereth.  All rights reserved.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; manager code
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod initialize ((target (eql *manager*)))
  (define-theory target "" *sentences*)
  'done)

(defmethod insert (p (receiver (eql *manager*)))
  (prog1 (call-next-method p receiver)
         (unless (atom p) (setvalue (car p) (cadr p) (caddr p)))))

(defmethod uninsert (p (receiver (eql *manager*)))
  (prog2 (call-next-method p receiver)
         (unless (atom p) (remvalue (car p) (cadr p) (caddr p)))))

(defmethod define-theory ((th (eql *manager*)) doc facts)
  (empty th)
  (setf (documentation th 'concept) doc)
  (dolist (p facts)
    (cond ((atom p) (insert p th))
          ((find (car p) '(not unprovable)) (drop (cadr p) th))
          (t (insert p th))))
  th)

(defmethod create (obj type)
  (cond ((not (symbolp obj)) nil)
        ((eq type 'remote)
         (unless (and (boundp obj) (remotep (symbol-value obj)))
           (let ((host (or (find-host obj) "")) (port (or (find-port obj) 0)))
             (set obj (make-remote obj host port)))))
        ((and (boundp obj) (eq (type-of (symbol-value obj)) type)))
        ((find type '(aclserver soapserver sqlserver))
         (set obj (make-instance type :name obj)))))

(defmethod destroy (obj type)
  (cond ((not (symbolp obj)) nil)
        ((and (boundp obj) (eq (type-of (symbol-value obj)) type)
              (find type '(interface counter basket aclserver soapserver sqlserver
                           facilitator authorizer)))
         (makunbound obj))))

(defmethod create (obj (type (eql 'factserver)))
  (unless (and (boundp obj) (typep (symbol-value obj) 'factserver))
    (set obj (make-instance 'factserver :name obj))))

(defmethod destroy (obj (type (eql 'factserver)))
  (when (and (symbolp obj) (boundp obj)
             (eq (type-of (symbol-value obj)) 'factserver))
    (empty (symbol-value obj))
    (makunbound obj)))

(defmethod create (obj (type (eql 'dualserver)))
  (unless (and (boundp obj) (typep (symbol-value obj) 'dualserver))
    (set obj (make-instance 'dualserver :name obj))))

(defmethod destroy (obj (type (eql 'dualserver)))
  (when (and (symbolp obj) (boundp obj)
             (eq (type-of (symbol-value obj)) 'dualserver))
    (empty (symbol-value obj))
    (makunbound obj)))

(defmethod create (obj (type (eql 'dataserver)))
  (unless (and (boundp obj) (typep (symbol-value obj) 'dataserver))
    (set obj (make-instance 'dataserver :name obj))))

(defmethod destroy (obj (type (eql 'dataserver)))
  (when (and (symbolp obj) (boundp obj)
             (eq (type-of (symbol-value obj)) 'dataserver))
    (empty (symbol-value obj))
    (makunbound obj)))

(defmethod create (obj (type (eql 'viewserver)))
  (unless (and (boundp obj) (typep (symbol-value obj) 'viewserver))
    (set obj (make-instance 'viewserver :name obj))))

(defmethod destroy (obj (type (eql 'viewserver)))
  (when (and (symbolp obj) (boundp obj)
             (eq (type-of (symbol-value obj)) 'viewserver))
    (empty (symbol-value obj))
    (makunbound obj)))

(defmethod create (obj (type (eql 'fullserver)))
  (unless (and (boundp obj) (typep (symbol-value obj) 'fullserver))
    (set obj (make-instance 'fullserver :name obj))))

(defmethod destroy (obj (type (eql 'fullserver)))
  (when (and (symbolp obj) (boundp obj)
             (eq (type-of (symbol-value obj)) 'fullserver))
    (empty (symbol-value obj))
    (makunbound obj)))

(defmethod create (obj (type (eql 'fastserver)))
  (unless (and (boundp obj) (typep (symbol-value obj) 'fastserver))
    (set obj (make-instance 'fastserver :name obj))))

(defmethod destroy (obj (type (eql 'fastserver)))
  (when (and (symbolp obj) (boundp obj)
             (eq (type-of (symbol-value obj)) 'fastserver))
    (empty (symbol-value obj))
    (makunbound obj)))

(defmethod create (obj (type (eql 'ruleserver)))
  (unless (and (boundp obj) (typep (symbol-value obj) 'ruleserver))
    (set obj (make-instance 'ruleserver :name obj))))

(defmethod destroy (obj (type (eql 'ruleserver)))
  (when (and (symbolp obj) (boundp obj)
             (eq (type-of (symbol-value obj)) 'ruleserver))
    (empty (symbol-value obj))
    (makunbound obj)))

(defmethod create (obj (type (eql 'diffserver)))
  (unless (and (boundp obj) (typep (symbol-value obj) 'diffserver))
    (set obj (make-instance 'diffserver :name obj))))

(defmethod destroy (obj (type (eql 'diffserver)))
  (cond ((and (symbolp obj) (boundp obj)
              (eq (type-of (symbol-value obj)) 'diffserver))
         (makunbound obj))))

(defmethod create (obj (type (eql 'translator)))
  (unless (and (boundp obj) (typep (symbol-value obj) 'translator))
    (set obj (make-instance 'translator :name obj))))

(defmethod destroy (obj (type (eql 'translator)))
  (when (and (symbolp obj) (boundp obj) (typep (symbol-value obj) 'translator))
    (makunbound obj)))

(defmethod create (obj (type (eql 'transformer)))
  (unless (and (boundp obj) (typep (symbol-value obj) 'transformer))
    (set obj (make-instance 'transformer :name obj))))

(defmethod destroy (obj (type (eql 'transformer)))
  (when (and (symbolp obj) (boundp obj) (typep (symbol-value obj) 'transformer))
    (makunbound obj)))

(defmethod create (obj (type (eql 'integrator)))
  (unless (and (boundp obj) (typep (symbol-value obj) 'integrator))
    (set obj (make-instance 'integrator :name obj))))

(defmethod destroy (obj (type (eql 'integrator)))
  (when (and (symbolp obj) (boundp obj) (typep (symbol-value obj) 'integrator))
    (makunbound obj)))

(defmethod create (obj (type (eql 'facilitator)))
  (unless (and (boundp obj) (typep (symbol-value obj) 'facilitator))
    (set obj (make-instance 'facilitator :name obj))))

(defmethod destroy (obj (type (eql 'facilitator)))
  (when (and (symbolp obj) (boundp obj) (typep (symbol-value obj) 'facilitator))
    (makunbound obj)))

(defmethod create (obj (type (eql 'interface)))
  (unless (and (boundp obj) (typep (symbol-value obj) 'interface))
    (set obj (make-instance 'interface :name obj))))

(defmethod destroy (obj (type (eql 'interface)))
  (when (and (symbolp obj) (boundp obj) (typep (symbol-value obj) 'interface))
    (makunbound obj)))


(defmethod setvalue (slot obj val)
  (cond ((eq 'isa slot) (create obj val))
        ((eq 'includee slot) (setincludee obj val))
        ((eq 'host slot) (sethost obj val))
        ((eq 'port slot) (setport obj val))))

(defmethod remvalue (slot obj val)
  (cond ((eq 'isa slot) (destroy obj val))
        ((eq 'includee slot) (unsetincludee obj val))
        ((eq 'host slot) (sethost obj ""))
        ((eq 'port slot) (setport obj 0))))

(defun setincludee (t1 t2)
  (if (and (symbolp t1) (boundp t1) (setq t1 (symbol-value t1))
           (symbolp t2) (boundp t2) (setq t2 (symbol-value t2))
           (typep t2 'agent))
      (includes t1 t2)))

(defun unsetincludee (t1 t2)
  (if (and (symbolp t1) (boundp t1) (setq t1 (symbol-value t1))
           (symbolp t2) (boundp t2) (setq t2 (symbol-value t2))
           (typep t2 'agent))
      (unincludes t1 t2)))

(defun sethost (x h)
  (if (and (symbolp x) (boundp x) (remotep (setq x (symbol-value x)))
           (or (not (equal h *host*)) (not (equal (port x) *port*))))
    (setf (host x) h)))

(defun setport (x p)
  (if (and (symbolp x) (boundp x)  (remotep (setq x (symbol-value x)))
           (or (not (equal (host x) *host*)) (not (equal p *port*))))
    (setf (port x) p)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
