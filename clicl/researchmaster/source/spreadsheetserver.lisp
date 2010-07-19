(defpackage :spreadsheetserver)
(in-package :spreadsheetserver)
(export '(dictionary dictionary-set dictionary-get
	  collection hashbag expr set pair 
	  make-class
	  collection-member collection-adjoin collection-add
	  collection-element collection-first collection-second collection-size
	  collection-equal collection-tolistr collection-union
	  with-collection-iterator collection-mapcar
	  hascellvalue cellvalue cellvalues 
	  functionname normreturn varp
	  *cellvalues*
	  print-object))
;;;;;;;;;;;;;;;;;;;; Data structures supporting server-side code ;;;;;;;;;;;;;;;;;;;;



(defclass dictionary () ; key,value pairs
  ((hash :accessor hash :initarg :hash :initform (make-hash-table))))  

(defmethod dictionary-get ((c dictionary) key) (gethash key (hash c)))
(defmethod dictionary-set ((c dictionary) key val) (setf (gethash key (hash c)) val))

(defclass collection ()
  ((data :accessor data :initarg :data :initform (make-array 0 :adjustable t :fill-pointer 0))))

(defclass ordered () ())

(defclass set (collection) ())   ; set of values
(defclass hashbag (set) ())      ; a bag of values, where lookup should be fast
(defclass tuple (collection ordered) ()) ; list of values
(defclass pair (tuple) ())  ; two values
(defclass expr (tuple) ())  ; list of expressions

(defun make-class (class &rest content) 
  (if (member class '(collection set hashbag expr pair))
      (make-collection class content)
      (make-instance class)))

(defun make-collection (class content)
  (setq class (make-instance class))
  (dolist (o content)
    (collection-add class o))
  class)

; basic methods and macros
(defmethod collection-first ((c collection))  (aref (data c) 0))
(defmethod collection-second ((c collection)) (aref (data c) 1))
(defmethod collection-size ((c collection)) (fill-pointer (data c)))
    ; for use during iteration
(defmethod collection-element ((c collection) index) (aref (data c) index))
(defmethod collection-add ((c collection) val) (vector-push-extend val (data c)))
(defmethod collection-member ((c collection) val &optional (test #'equal)) 
  (find val (data c) :test test))
(defmethod collection-adjoin ((c collection) val &optional (test #'equal)) 
  (unless (collection-member c val test)
    (vector-push-extend val (data c)))
  c)
(defmacro with-collection-iterator ((var coll &optional (returnval nil)) &body body)
  "WITH-COLLECTION-ITERATOR ((var collection) &body body)
   provides a method of looping over an arbitrary collection."
  (let ((upper (gentemp "__upper")))
    `(let ((,upper (collection-size ,coll)))
       (do ((,var 0 (1+ ,var)))
	   ((>= ,var ,upper) ,returnval)
	 ,@body))))

; overloaded basic methods
(defmethod collection-add ((c set) val) (collection-adjoin c val))


; layered on top of those methods
(defun collection-equal (c1 c2 &optional (test #'equal))
  (cond ((funcall test c1 c2) t)
	((and (typep c1 'collection) (typep c2 'collection))
	 (cond ((and (typep c1 'ordered) (typep c2 'ordered))
		(if (not (= (collection-size c1) (collection-size c2)))
		    nil
		    (with-collection-iterator (i c1 t)
		      (with-collection-iterator (j c2)
			(unless (collection-equal (collection-element c1 i) (collection-element c2 j) test)
			  (return-from collection-equal nil))))))		      
	       (t (and (collection-subset c1 c2 test) (collection-subset c2 c1 test)))))
	(t nil)))
		
(defun collection-subset (c1 c2 &optional (test #'equal))
  (cond ((and (listp c1) (listp c2)) (subsetp c1 c2 :test test))
	((not (typep c1 'collection)) nil)
	((not (typep c2 'collection)) nil)
	(t
	 (with-collection-iterator (i c1)
	   (unless (collection-member c2 (collection-element c1 i) #'(lambda (x y) (collection-equal x y test)))
	     (return-from collection-subset nil)))
	 t)))

(defmethod collection-mapcar (f (c collection))
  (let ((tup (make-class 'tuple)))
    (with-collection-iterator (i c)
      (collection-add tup (funcall f (collection-element c i))))
    tup))  

(defgeneric collection-tolistr (c) (:documentation "Transform C into a list, recursively"))
(defmethod collection-tolistr (c) c)
(defmethod collection-tolistr ((c cons)) (mapcar #'collection-tolistr c))
(defmethod collection-tolistr ((c collection))
  (let ((res))
    (with-collection-iterator (i c)
      (push (collection-tolistr (collection-element c i)) res))
    (nreverse res)))

(defun collection-union (c1 c2 &optional (test #'equal))
  (let ((s (make-class 'set)))
    (with-collection-iterator (i c1)
      (collection-adjoin s (collection-element c1 i) test))
    (with-collection-iterator (i c2)
      (collection-adjoin s (collection-element c2 i) test))
    s))

(defmethod print-object ((c dictionary) stream) (print-collection c 'dictionary stream))
(defmethod print-object ((c collection) stream) (print-collection c 'collection stream))
(defmethod print-object ((c hashbag) stream) (print-collection c 'hashbag stream))
(defmethod print-object ((c expr) stream) (print-collection c 'expr stream))
(defmethod print-object ((c set) stream) (print-collection c 'set stream))
(defmethod print-object ((c pair) stream) (print-collection c 'pair stream))
(defmethod print-object ((c tuple) stream) (print-collection c 'tuple stream))

(defun print-collection (c class stream)
  (format stream "(~(~A~)" class)
  (with-collection-iterator (i c)
    (format stream " ")
    (print-object (collection-element c i) stream))
  (format stream ")"))

(defvar *cellvalues* (make-hash-table))
(defun hascellvalue (cell) (cellvalue cell))
(defun cellvalues (cell) (gethash cell *cellvalues*))
(defun cellvalue (cell) (let ((v (cellvalues cell))) (if v (collection-first v) nil)))

(defun varp (x)
  (and (symbolp x)
       (setq x (char (symbol-name x) 0))
       (or (eql #\? x) (eql #\@ x))))

; could write translator for these two, but for now just use macro
(defmacro functionname (f) (list 'quote f))
(defmacro normreturn (fname v) (if fname `(return-from ,fname ,v) v))
