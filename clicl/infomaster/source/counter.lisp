;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; counter
;;;  supplies sequential numbers for relations
;;;  (<indexrelation> <object> <number>)
;;;  if <object> is ground and no entry exists for <number>,
;;;  it automatically generates the next.
;;;  size stored in size and is resettable but does not erase listings.
;;;  (indexsize <relation> <number>)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass counter (dataserver) ())

(defmethod indexps (p (th counter))
  (cond ((varp p) (contents th))
        ((atom p) (indexees p th))
        ((eq 'indexsize (car p)) (countindexps p th))
        ((not (findp `(specialty ,(name th) ,(car p)) *manager*)) nil)
        ((or (null (cddr p)) (cdddr p)) nil)
        ((groundp (caddr p)) (countindexps p th))
        ((and (groundp (cadr p)) (varp (caddr p)) (not (indexp p th)))
         (let ((size (or (truex '?x `(indexsize ,(car p) ?x) th) 0)) (datum))
           (drop `(indexsize ,(car p) ,size) th)
           (setq size (1+ size) datum (list (car p) (cadr p) size))
           (insert `(indexsize ,(car p) ,size) th)
           (insert datum th)
           (list datum)))
        (t (countindexps p th))))

(defun countindexps (p th)
  (do ((l (cdr p) (cdr l)))
      ((null l) (indexps (car p) th))
      (cond ((varp (car l)))
            ((atom (car l)) (return (indexees (car l) th))))))

(defun indexp (p th)
  (find p (indexees (cadr p) th) :test #'matchp))

(defmethod envindexps (p al (th counter))
  (indexps (plug p al) th))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
