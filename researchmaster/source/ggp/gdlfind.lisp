;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; gdlfind.lisp
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(eval-when (compile load eval)
  (proclaim '(special *ancestry* *consistency* *depth* *limit*
                      *unifications* *inferences* *termination*
                      *thing* *theory* *filter* *variables*
                      *answer* *answers* *residue*
                      alist level tracecalls traceexpressions)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; gdlfindp
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun gdlfindp (p th)
  (gdlfindx 't p th))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; gdlfindx
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun gdlfindx (*thing* p *theory*)
  (let (alist *answer*)
    (setq *unifications* 0)
    (setq *inferences* 0)
    (setq *termination* nil)
    (setq alist (environment))
    (when (gdlone p (list p) alist 0 nil) *answer*)))

(defun gdlone (p pl al depth cont)
  (setq *inferences* (1+ *inferences*))
  (gdlcall p al depth)
  (cond ((>= *inferences* *limit*) (setq *termination* t) (gdlstop (car pl) al depth))
        ((>= depth *depth*) (setq *termination* t) (gdlstop (car pl) al depth))
        (t (gdloneexp p pl al depth cont))))

(defun gdloneexp (p pl al depth cont)
  (cond ((atom p) (gdloners p pl al depth cont))
        ((eq (car p) 'not) (gdloneunprovable p pl al depth cont))
        ((eq (car p) 'or) (gdloneor p pl al depth cont))
        ((eq (car p) 'distinct) (gdlonedistinct p pl al depth cont))
        (t (gdloners p pl al depth cont))))

(defun gdloneor (p pl al depth cont)
  (setq cont (cons (list pl al depth) cont))
  (do ((l (cdr p) (cdr l)))
      ((null l) (gdlfail (car pl) al depth))
      (when (gdlone (car l) (list (car l)) al depth cont)
        (return t))))

(defun gdlonedistinct (p pl al depth cont)
  (let (ol)
    (cond ((setq ol (unify (cadr p) al (caddr p) al))
           (backup ol) (gdlfail (car pl) al depth))
          (t (gdlonelast pl al depth cont)))))

(defun gdloneunprovable (p pl al depth cont)
  (cond ((gdlone (cadr p) (cdr p) al depth nil) (gdlfail (car pl) al depth))
        (t (gdlonelast pl al depth cont))))

(defun gdloners (p pl al depth cont)
  (cond ((and *ancestry* (gdloneancestor p al cont)) (gdlfail (car pl) al depth))
        ((gdlonedb p pl al depth cont *theory*))
        (t (gdlfail (car pl) al depth))))

(defun gdloneancestor (p al cont)
  (do ((l (cdr cont) (cdr l)))
      ((null l) nil)
      (if (identify (caaar l) (cadar l) p al) (return t))))

(defun gdlonedb (p pl al depth cont th)
  (cond ((gdloneth p pl al depth cont th))
        (t (do ((l (includees th) (cdr l)))
               ((null l) nil)
               (when (gdlonedb p pl al depth cont (car l)) (return t))))))

(defun gdloneth (p pl al depth cont th)
  (do ((l (envindexps p al th) (cdr l)) (bl (environment)) (ol))
      ((null l))
      (cond ((and (listp (car l)) (eq '<= (caar l)) (null (cddar l)))
             (cond ((not (setq ol (unify (cadar l) bl p al))))
                   ((gdloneexit pl al depth cont) (backup ol) (return t))
                   ((subolp ol (alist bl)) (backup ol) (return nil))
                   (t (backup ol))))
            ((and (listp (car l)) (eq '<= (caar l)))
             (cond ((not (setq ol (unify (cadar l) bl p al))))
                   ((gdlone (caddar l) (cddar l) bl
                             (1+ depth) (cons (list pl al depth) cont))
                    (backup ol) (return  t))
                   (t (backup ol))))
            ((setq ol (unify (car l) bl p al))
             (cond ((gdloneexit pl al depth cont) (backup ol) (return t))
                   ((subolp ol (alist bl)) (backup ol) (return nil))
                   (t (backup ol)))))))

(defun gdloneexit (pl al depth cont)
  (let (ans)
    (gdlexit (car pl) al depth)
    (cond ((cdr pl) (setq ans (gdlone (cadr pl) (cdr pl) al depth cont)))
          (cont (setq ans (gdloneexit (caar cont) (cadar cont) (caddar cont) (cdr cont))))
          (t (setq *answer* (plugstdexp *thing* alist) ans t)))
    (if ans t (gdlredo (car pl) al depth))))

(defun gdlonelast (pl al depth cont)
  (gdlexit (car pl) al depth)
  (cond ((cdr pl) (gdlone (cadr pl) (cdr pl) al depth cont))
        (cont (gdloneexit (caar cont) (cadar cont) (caddar cont) (cdr cont)))
        (t (setq *answer* (plugstdexp *thing* alist)))))

(defun gdlcall (p al depth)
  (cond ((not traceexpressions) nil)
        ((find (setq p (plugstdexp p al)) traceexpressions :test #'instp)
         (tracemessage depth '|Call: | p) nil)))

(defun gdlexit (p al depth)
  (cond ((not traceexpressions) nil)
        ((find (setq p (plugstdexp p al)) traceexpressions :test #'instp)
         (tracemessage depth '|Exit: | p) nil)))

(defun gdlredo (p al depth)
  (cond ((not traceexpressions) nil)
        ((find (setq p (plugstdexp p al)) traceexpressions :test #'instp)
         (tracemessage depth '|Redo: | p) nil)))

(defun gdlfail (p al depth)
  (cond ((not traceexpressions) nil)
        ((find (setq p (plugstdexp p al)) traceexpressions :test #'instp)
         (tracemessage depth '|Fail: | p) nil)))

(defun gdlstop (p al depth)
  (cond ((not traceexpressions) nil)
        ((find (setq p (plugstdexp p al)) traceexpressions :test #'instp)
         (tracemessage depth '|Stop: | p) nil)))

(defun gdlsave (p al depth)
  (cond ((not traceexpressions) nil)
        ((find (setq p (plugstdexp p al)) traceexpressions :test #'instp)
         (tracemessage depth '|Save: | p) nil)))

(defun gdldrop (p al depth)
  (cond ((not traceexpressions) nil)
        ((find (setq p (plugstdexp p al)) traceexpressions :test #'instp)
         (tracemessage depth '|Drop: | p) nil)))

(defun gdldone (p al depth)
  (cond ((not traceexpressions) nil)
        ((find (setq p (plugstdexp p al)) traceexpressions :test #'instp)
         (tracemessage depth '|Done: | p) nil)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; gdlfinds
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun gdlfinds (*thing* p *theory*)
  (let (alist *answers*)
    (setq *unifications* 0)
    (setq *inferences* 0)
    (setq *termination* nil)
    (setq alist (environment))
    (gdlall p (list p) alist 0 nil)
    (nreverse (uniquify *answers*))))

(defun gdlall (p pl al depth cont)
  (setq *inferences* (1+ *inferences*))
  (gdlcall p al depth)
  (cond ((>= *inferences* *limit*) (setq *termination* t) (gdlstop (car pl) al depth))
        ((>= depth *depth*) (setq *termination* t) (gdlstop (car pl) al depth))
        (t (gdlallexp p pl al depth cont))))

(defun gdlallexp (p pl al depth cont)
  (cond ((atom p) (gdlallrs p pl al depth cont))
        ((eq (car p) 'not) (gdlallunprovable p pl al depth cont))
        ((eq (car p) 'or) (gdlallor p pl al depth cont))
        ((eq (car p) 'distinct) (gdlalldistinct p pl al depth cont))
        (t (gdlallrs p pl al depth cont))))

(defun gdlallor (p pl al depth cont)
  (setq cont (cons (list pl al depth) cont))
  (do ((l (cdr p) (cdr l)))
      ((null l) (gdlfail (car pl) al depth))
      (gdlall (car l) (list (car l)) al depth cont)))

(defun gdlalldistinct (p pl al depth cont)
  (let (ol)
    (cond ((setq ol (unify (cadr p) al (caddr p) al))
           (backup ol) (gdlfail p al depth))
          (t (gdlalllast pl al depth cont)))))

(defun gdlallunprovable (p pl al depth cont)
  (cond ((gdlone (cadr p) (cdr p) al depth nil) (gdlfail (car pl) al depth))
        (t (gdlalllast pl al depth cont))))

(defun gdlallrs (p pl al depth cont)
  (cond ((and *ancestry* (gdlallancestor p al cont)) (gdlfail (car pl) al depth))
        ((and (numberp *ancestry*) (gdlallnumber p al cont 0))
         (setq *termination* t) (gdlfail (car pl) al depth))
        ((gdlalldb p pl al depth cont *theory*))
        (t (gdlfail (car pl) al depth))))

(defun gdlallancestor (p al cont)
  (do ((l cont (cdr l)))
      ((null l) nil)
      (if (identify (caaar l) (cadar l) p al) (return t))))

(defun gdlallnumber (p al cont n)
  (let (ol)
    (cond ((numgeqp n *ancestry*))
          ((null cont) nil)
          ((atom p)
           (gdlallnumber p al (cdr cont) (if (eq p (caaar cont)) (1+ n) n)))
          ((setq ol (unify p al (caaar cont) (cadar cont)))
           (prog1 (gdlallnumber p al (cdr cont) (1+ n)) (backup ol)))
          (t (gdlallnumber p al (cdr cont) n)))))

(defun gdlalldb (p pl al depth cont th)
  (gdlallth p pl al depth cont th)
  (do ((l (includees th) (cdr l)))
      ((null l))
      (gdlalldb p pl al depth cont (car l))))

(defun gdlallth (p pl al depth cont th)
  (do ((l (envindexps p al th) (cdr l)) (bl (environment)) (ol))
      ((null l))
      (cond ((and (listp (car l)) (eq '<= (caar l)) (null (cddar l)))
             (when (setq ol (unify (cadar l) bl p al))
               (gdlallexit pl al depth cont)
               (cond ((subolp ol (alist bl)) (backup ol) (return nil))
                   (t (backup ol)))))
            ((and (listp (car l)) (eq '<= (caar l)))
             (when (setq ol (unify (cadar l) bl p al))
               (gdlall (caddar l) (cddar l) bl
                        (1+ depth) (cons (list pl al depth) cont))
               (backup ol)))
            ((setq ol (unify (car l) bl p al))
             (gdlallexit pl al depth cont)
             (cond ((subolp ol (alist bl)) (backup ol) (return nil))
                   (t (backup ol)))))))

(defun gdlallexit (pl al depth cont)
  (gdlexit (car pl) al depth)
  (cond ((cdr pl) (gdlall (cadr pl) (cdr pl) al depth cont))
        (cont (gdlallexit (caar cont) (cadar cont) (caddar cont) (cdr cont)))
        (t (setq *answers* (cons (plugstdexp *thing* alist) *answers*))))
  (gdlredo (car pl) al depth))

(defun gdlalllast (pl al depth cont)
  (gdlexit (car pl) al depth)
  (cond ((cdr pl) (gdlall (cadr pl) (cdr pl) al depth cont))
        (cont (gdlallexit (caar cont) (cadar cont) (caddar cont) (cdr cont)))
        (t (setq *answers* (cons (plugstdexp *thing* alist) *answers*)) nil)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
