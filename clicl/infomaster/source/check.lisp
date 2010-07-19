;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; checkclass checks predicate and attributes
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod checkclass (class agent)
  (let (predicate (errors 0))    
    (when (setq predicate (find-predicate class))
      (setq errors (+ (checkpredicate predicate agent) errors)))
    (dolist (attribute (slots-of-class class))
      (setq errors (+ (checkattribute attribute agent) errors)))
    errors))

(defmethod checkclass (class (agent symbol))
  (cond ((and (boundp agent) (not (symbolp (symbol-value agent))))
         (checkclass class (symbol-value agent)))
        (t (call-next-method class agent))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; checkpredicate checks arity
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod checkpredicate (relation agent)
  (let ((errors 0))
    (dolist (violation (find-arity-violations relation agent))
      (setq errors (1+ errors))
      (fresh-line) (princ (cadr violation)) (princ " -- ") (princ (caddr violation)))
    errors))

(defmethod checkpredicate (relation (agent symbol))
  (cond ((and (boundp agent) (not (symbolp (symbol-value agent))))
         (checkpredicate relation (symbol-value agent)))
        (t (call-next-method relation agent))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; checkattribute checks arity, domain, range, uniqueness, and totality
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod checkattribute (slot agent)
  (let ((errors 0))
    (dolist (violation (find-arity-violations slot agent))
      (setq errors (1+ errors))
      (fresh-line) (princ (cadr violation)) (princ " -- ") (princ (caddr violation)))
    (dolist (violation (find-domain-violations slot agent))
      (setq errors (1+ errors))
      (fresh-line) (princ (cadr violation)) (princ " -- ") (princ (caddr violation)))
    (dolist (violation (find-range-violations slot agent))
      (setq errors (1+ errors))
      (fresh-line) (princ (cadr violation)) (princ " -- ") (princ (caddr violation)))
    (when (totalp slot)
      (dolist (violation (find-total-violations slot agent))
        (setq errors (1+ errors))
        (fresh-line) (princ (cadr violation)) (princ " -- ") (princ (caddr violation))))
    (when (uniquep slot)
      (dolist (violation (find-unique-violations slot agent))
        (setq errors (1+ errors))
        (fresh-line) (princ (cadr violation)) (princ " -- ") (princ (caddr violation))))
    errors))

(defmethod checkattribute (attribute (agent symbol))
  (cond ((and (boundp agent) (not (symbolp (symbol-value agent))))
         (checkattribute attribute (symbol-value agent)))
        (t (call-next-method attribute agent))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; checktables checks relations for domain and range restrictions, uniqueness,
;;; and total restrictions.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod checktables (agent)
  (let ((errors 0))
    (dolist (slot (get-all-attributes agent))
      (setq errors (+ (checkattribute slot agent) errors)))
    (dolist (error (finds '?x '(error ?x) 'infotheory))
      (setq errors (1+ errors)) (fresh-line) (princ error))
    errors))

(defmethod checktables ((agent symbol))
  (cond ((and (boundp agent) (not (symbolp (symbol-value agent))))
         (checktables (symbol-value agent)))
        (t (call-next-method agent))))

(defun get-all-attributes (agent)
  (declare (ignore agent))
  (finds '?r `(isa ?r attributerelation) *manager*))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Miscellaneous
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun find-arity-violations (relation agent)
  (let (arity)
    (setq arity (find-arity relation))
    (cond ((not (integerp arity)) nil)
          (t (do ((l (request `(ask-about ,relation) nil agent) (cdr l)) (nl))
                 ((null l) (nreverse nl))
                 (when (and (listp (car l)) (eq (caar l) relation)
                            (not (= (length (cdar l)) arity)))
                   (setq nl (cons `(error "Arity violation" ,(car l)) nl))))))))

(defun find-domain-violations (slot agent)
  (let (domain)
    (setq domain (find-domain slot))
    (cond ((not domain) nil)
          ((eq domain 'number)
           (do ((l (get-domain slot agent) (cdr l)) (nl))
               ((null l) (nreverse nl))
               (unless (numberp (car l))
                 (setq nl (cons `(error "Domain violation" (,slot ,(car l) ??)) nl)))))
          ((eq domain 'character)
           (do ((l (get-domain slot agent) (cdr l)) (nl))
               ((null l) (nreverse nl))
               (unless (characterp (car l))
                 (setq nl (cons `(error "Domain violation" (,slot ,(car l) ??)) nl)))))
          ((eq domain 'string)
           (do ((l (get-domain slot agent) (cdr l)) (nl))
               ((null l) (nreverse nl))
               (unless (stringp (car l))
                 (setq nl (cons `(error "Domain violation" (,slot ,(car l) ??)) nl)))))
          ((eq domain 'date)
           (do ((l (get-domain slot agent) (cdr l)) (nl))
               ((null l) (nreverse nl))
               (unless (datep (car l))
                 (setq nl (cons `(error "Domain violation" (,slot ,(car l) ??)) nl)))))
          ((eq domain 'thing) nil)
          (t (do ((l (get-domain slot agent) (cdr l)) (nl))
                 ((null l) nl)
	         (unless (instancep (car l) domain agent)
		   (setq nl (cons `(error "Domain violation" (,slot ,(car l) ??)) nl))))))))

(defun find-range-violations (slot agent)
  (let (range objects okays)
    (setq range (find-range slot))
    (cond ((not range) nil)
          ((eq range 'number)
           (do ((l (get-range slot agent) (cdr l)) (nl))
               ((null l) (nreverse nl))
               (unless (numberp (car l))
                 (setq nl (cons `(error "Range violation" (,slot ?? ,(car l))) nl)))))
          ((eq range 'character)
           (do ((l (get-range slot agent) (cdr l)) (nl))
               ((null l) (nreverse nl))
               (unless (characterp (car l))
                 (setq nl (cons `(error "Range violation" (,slot ?? ,(car l))) nl)))))
          ((eq range 'string)
           (do ((l (get-range slot agent) (cdr l)) (nl))
               ((null l) (nreverse nl))
               (unless (stringp (car l))
                 (setq nl (cons `(error "Range violation" (,slot ?? ,(car l))) nl)))))
          ((eq range 'date)
           (do ((l (get-range slot agent) (cdr l)) (nl))
               ((null l) (nreverse nl))
               (unless (datep (car l))
                 (setq nl (cons `(error "Range violation" (,slot ?? ,(car l))) nl)))))
          ((eq range 'thing) nil)
          (t (setq objects (get-range slot agent))
             (setq okays (filter-instances objects range agent))
             (do ((l (set-difference objects okays) (cdr l)) (nl))
                 ((null l) nl)
                 (setq nl (cons `(error "Range violation" (,slot ?? ,(car l))) nl)))))))

(defun find-total-violations (slot agent)
  (let (domain objects okays)
    (when (setq domain (find-domain slot))
      (setq objects (request `(ask-all ?x ,(makpred '?x domain (name agent))) nil agent))
      (setq okays (request `(ask-all ?x (and (oneof ?x . ,objects) ,(list slot '?x '?y)))
                           nil agent))
      (do ((l (set-difference objects okays) (cdr l)) (nl))
          ((null l) nl)
          (setq nl (cons `(error "Total violation" (,slot ,(car l) ??)) nl))))))

(defun find-unique-violations (slot agent)
  (request `(ask-all (error "Uniqueness violation" (,slot ?x ?y))
                     (and ,(list slot '?x '?y) ,(list slot '?x '?z) (distinct ?y ?z)))
           nil agent))

(defun totalp (slot)
  (findp `(total ,slot yes) *manager*))

(defun get-domain (slot agent)
  (request `(ask-all ?x ,(list slot '?x '?y)) nil agent))

(defun get-range (slot agent)
  (request `(ask-all ?y ,(list slot '?x '?y)) nil agent))

(defun filter-instances (objects class agent)
  (request `(ask-all ?x (and (oneof ?x . ,objects)
                             ,(makpred '?x class agent)))  ;;;makisa
           nil agent))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
