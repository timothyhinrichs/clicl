;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Simple
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun simplefindor (p th)
  (do ((l (cdr p) (cdr l)) (dum))
      ((null l) nil)
      (when (setq dum (simplefindp (car l) th)) (return dum))))

(defun simplefindp (p *theory*)
  (let ((alist (environment)))
    (setq *unifications* 0)
    (setq *inferences* (length (contents *theory*)))
    (setq *termination* nil)
    (cond ((atom p) (simplefind p (list p) alist 0 nil))
          ((eq (car p) 'and) (simpleexit p alist 0 nil))
          (t (simplefind p (list p) alist 0 nil)))))

(defun simplefind (p pl al depth cont)
  (setq *inferences* (1+ *inferences*))
  (cond ((>= *inferences* *limit*) (setq *termination* t) nil)
        ((> depth *depth*) (setq *termination* t) nil)
        ((and *ancestry* (simpleancestor p al cont)) nil)
        ((and *reduction* (simplereduce p pl al depth cont)))
        (*termination* nil)
        (t (simplefindth p pl al depth cont))))

(defun simpleancestor (p al cont)
  (do ((l cont (cdr l)))
      ((null l) nil)
      (if (identify (caaar l) (cadar l) p al) (return t))))

(defun simplereduce (p pl al depth cont)
  (do ((l cont (cdr l)) (ol))
      ((null l))
      (when (setq ol (unify (maknot (caaar l)) (cadar l) p al))
        (cond ((simpleexit pl al depth cont)
               (backup ol)
               (return t))
              (*termination* (backup ol) (return nil))
              (t (backup ol))))))

(defun simplefindth (p pl al depth cont)
  (do ((l (envindexps p al *theory*) (cdr l)) (bl (environment)) (ol)
       (new (cons (list pl al depth) cont)))
      ((null l))
      (cond ((setq ol (unify (car l) bl p al))
             (cond ((simpleexit pl al depth cont)
                    (backup ol)
                    (return t))
                   (*termination* (backup ol) (return nil))
                   (t (backup ol))))
            ((and (listp (car l)) (eq '<= (caar l))
                  (setq ol (unify (cadar l) bl p al)))
             (cond ((simpleexit (cdar l) bl (1+ depth) new)
                    (backup ol)
                    (return t))
                   (*termination* (backup ol) (return nil))
                   (t (backup ol)))))))

(defun simpleexit (pl al depth cont)
  (cond ((cdr pl) (simplefind (cadr pl) (cdr pl) al depth cont))
        ((null cont) (setq *inferences* (1+ *inferences*)) (list 'proof))
        (t (simpleexit (caar cont) (cadar cont) (caddar cont) (cdr cont)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Simple
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defparameter *result* nil)

(defun simplefindoneor (x p th)
  (do ((l (cdr p) (cdr l)) (dum))
      ((null l) nil)
      (when (setq dum (simpleone x (car l) th)) (return dum))))

(defun simpleone (*thing* p *theory*)
  (let (alist *result*)
    (setq *unifications* 0)
    (setq *inferences* (length (contents *theory*)))
    (setq *termination* nil)
    (setq alist (environment))
    (cond ((atom p) (simplefindone p (list p) alist 0 nil))
          ((eq (car p) 'and) (simplefindoneexit p alist 0 nil))
          (t (simplefindone p (list p) alist 0 nil)))
    *result*))

(defun simplefindone (p pl al depth cont)
  (setq *inferences* (1+ *inferences*))
  (cond ((>= *inferences* *limit*) (setq *termination* t) nil)
        ((> depth *depth*) (setq *termination* t) nil)
        ((and *ancestry* (simplefindoneancestor p al cont)) nil)
        ((and *reduction* (simplefindonereduce p pl al depth cont)))
        (*termination* nil)
        (t (simplefindfindoneth p pl al depth cont))))

(defun simplefindoneancestor (p al cont)
  (do ((l cont (cdr l)))
      ((null l) nil)
      (if (identify (caaar l) (cadar l) p al) (return t))))

(defun simplefindonereduce (p pl al depth cont)
  (do ((l cont (cdr l)) (ol))
      ((null l))
      (when (setq ol (unify (maknot (caaar l)) (cadar l) p al))
        (cond ((simplefindoneexit pl al depth cont)
               (backup ol)
               (return t))
              (*termination* (backup ol) (return nil))
              (t (backup ol))))))

(defun simplefindfindoneth (p pl al depth cont)
  (do ((l (envindexps p al *theory*) (cdr l)) (bl (environment)) (ol)
       (new (cons (list pl al depth) cont)))
      ((null l))
      (cond ((setq ol (unify (car l) bl p al))
             (cond ((simplefindoneexit pl al depth cont)
                    (backup ol)
                    (return t))
                   (*termination* (backup ol) (return nil))
                   (t (backup ol))))
            ((and (listp (car l)) (eq '<= (caar l))
                  (setq ol (unify (cadar l) bl p al)))
             (cond ((simplefindoneexit (cdar l) bl (1+ depth) new)
                    (backup ol)
                    (return t))
                   (*termination* (backup ol) (return nil))
                   (t (backup ol)))))))

(defun simplefindoneexit (pl al depth cont)
  (cond ((cdr pl) (simplefindone (cadr pl) (cdr pl) al depth cont))
        ((null cont)
         (setq *inferences* (1+ *inferences*))
         (setq *result* (plugstdexp *thing* alist))
         t)
        (t (simplefindoneexit (caar cont) (cadar cont) (caddar cont) (cdr cont)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; tests
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(deftheory foo ""
  (m a a)
  (m a b)
  (m a c)
  (n b a)
  (n b b)
  (n b c)
  (<= (s ?x ?y) (r ?x ?y) (r ?y ?x))
  (<= (r ?x ?z) (p ?x ?y) (q ?y ?z))
  (<= (p ?x ?y) (m ?x ?y))
  (<= (q ?x ?y) (n ?x ?y)))

;;; (simplefindp '(r a c) 'foo)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
