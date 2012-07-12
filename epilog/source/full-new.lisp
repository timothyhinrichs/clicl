;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; (c) Copyright 2001-2005 by Michael Genesereth.  All rights reserved.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; full.lisp
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; updated functions can be found by searching for UPDATED
; put in package so as not to conflict with infomaster's version

(eval-when (compile load eval)
  (proclaim '(special *answer* *answers* *theory*
                      *residue* *consistency* *filter* *test*
                      alist level tracecalls tracefacts traceexpressions
                      *tautology-elim* *ignore-goal-rules* *goal* *check-answers*)))

; UPDATED
(defvar *tautology-elim* t)
(defvar *ignore-goal-rules* nil "whether to ignore rules with *goal* in the head")
(defvar *goal* nil "relation constant used to name the goal")
(defvar *check-answers* nil "whether to check for redundancy in solutions")
;(defvar *subsumption* t "whether when checking for redundancy in residues to use subsumption or just samep")
;(defvar *matches* 0 "Number of matches performed.")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; defcontratheory
;;; define-contratheory
;;; fullinsert
;;; fullprovep
;;; fullprovex
;;; fullproves
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defparameter *limit* 1000000000)
(defparameter *answer* nil)
(defparameter *answers* nil)
(defparameter *naf* nil)

(defmacro deffulltheory (x &rest l)
  `(define-fulltheory ',x ',l))

(defun define-fulltheory (th facts)
  (empty th)
  (dolist (p facts) (fullassume p th))
  th)

(defmacro defcontratheory (x &rest l)
  `(define-contratheory ',x ',l))

(defun define-contratheory (th facts)
  (empty th)
  (dolist (p facts) (fullinsert p th))
  th)

(defun fullinsert (p th)
  (dolist (x (contrapositives p)) (save x th))
  'done)

(defun fullprovep (p th)
  (fullprovex 't p th))

; UPDATED: changed call to fullone
(defun fullprovex (*thing* p th)
  (let (alist *theory* *answer* (*ancestry* t))
    (setq *unifications* 0)
    (setq *inferences* 0)
    (setq *termination* nil)
    (setq p (adjust *thing* (order *thing* p)))
    (setq alist (environment))
    (setq *theory* (make-instance 'theory))
    (includes *theory* th)
    (dolist (x (contrapositives `(<= (answer ,*thing*) ,p))) (save x *theory*))
    (do ((old *depth*) (*depth* (min *start* *depth*) (+ *depth* *increment*)))
        ((> *depth* old) nil)
      (fullone `(answer ,*thing*) `((answer ,*thing*)) alist 0 nil)
      ;(fullone `(answer ,*thing*) nil alist 1 (list (list (list `(answer ,*thing*)) alist 1)))
      (cond (*answer* (return t))
            ((not *termination*) (return nil))
            (t (setq *termination* nil))))
    (decludes *theory*)
    (empty *theory*)
    *answer*))

; UPDATED: changed call to fullall and included *filter* as optional
(defun fullproves (*thing* p th &optional (*filter* #'failure))
  (let (alist *theory* *answers* (*ancestry* t))
    (setq *unifications* 0)
    (setq *inferences* 0)
    (setq *termination* nil)
    (setq p (adjust  *thing* (order *thing* p)))
    (setq alist (environment))
    (setq *theory* (make-instance 'theory))
    (includes *theory* th)
    (dolist (x (contrapositives `(<= (answer ,*thing*) ,p))) (save x *theory*))
    (fullall `(answer ,*thing*) `((answer ,*thing*)) alist 0 nil)
    ;(fullall `(answer ,*thing*) nil alist 1 (list (list (list `(answer ,*thing*)) alist 1)))
    (decludes *theory*)
    (empty *theory*)
    (nreverse (uniquify *answers*))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; fullassume
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defparameter *obsessions* nil)

(defun obsessionp (p)
  (or (not (listp *obsessions*)) (find (operator p) *obsessions* :test #'eq)))

(defun fullassume (p *theory*)
  (let (alist level tracecalls)
    (setq *termination* nil)
    (setq *unifications* 0)
    (setq alist (environment))
    (setq level 0)
    (fullassumedepth p alist 1)
    'done))

(defun fullassumedepth (p al depth)
  (cond ((numgreaterp depth *depth*) (setq *termination* t) nil)
        (t (fullsave p al depth)
           (fullassumeexp p al depth)
           (fulldone p al depth))))

(defun fullassumeexp (p al depth)
  (cond ((atom p) (fullassumeexpexit p al depth))
        ((eq 'and (car p))
         (mapc #'(lambda (x) (fullassumedepth x al depth)) (cdr p)))
        (t (fullassumeexpexit p al depth))))

(defun fullassumeexpexit (p al depth)
  (setq p (plugstdexp p al))
  (cond ((knownp p *theory* #'samep) nil)
        ((atom p) (fullassumers p al depth))
        ((eq 'execute (car p)) (ignore-errors (eval (cadr p))))
        ((eq 'evaluate (car p)) (ignore-errors (apply (caadr p) (cdadr p))))
        (t (fullassumers p al depth))))

(defun fullassumers (p al depth)
  (when (savep (operator p)) (insert p *theory*))
  (unless (obsessionp (operator p)) (fullassumedb p al depth *theory*)))

(defun fullassumedb (p al depth th)
  (cond ((fullassumeth p al depth th))
        (t (do ((l (includees th) (cdr l)))
               ((null l) nil)
               (fullassumedb p al depth (car l))))))

(defun fullassumeth (p al depth th)
  (do ((l (envindexps p al th) (cdr l)) (ol) (bl (environment)) (*thing*) (*answers*) (dum))
      ((null l))
      (when (and (listp (car l)) (eq (caar l) '<=))
        (do ((m (cddar l) (cdr m)) (om))
            ((null m))
            (when (setq ol (unify (car m) bl p al))
              (if tracefacts (tracefact (car l)))
              (setq dum (maksand (revappend om (cdr m))))
              (setq *thing* (cadar l) *answers* nil)
              (dolist (q (fullallknownspecials dum bl depth))
                (fullassumedepth q alist (1+ depth)))
              (backup ol))
            (setq om (cons (car m) om))))))

(defun fullallknownspecials (dum bl depth)
  (cond ((eq dum 'true) (list (plugstdexp *thing* bl)))
        (t (let ((alist bl))
             (fullallknowndb dum (list dum) bl (1+ depth) nil *theory*)
             (nreverse *answers*)))))

(defun fullassumeth (p al depth th)
  (do ((l (envindexps p al th) (cdr l)) (ol) (bl (environment)) (*thing*) (*answers*) (dum))
      ((null l))
      (when (and (listp (car l)) (eq (caar l) '<=))
        (do ((m (cddar l) (cdr m)) (om))
            ((null m))
            (when (setq ol (unify (car m) bl p al))
              (if tracefacts (tracefact (car l)))
              (setq dum (maksand (revappend om (cdr m))))
              (dolist (q (fullallspecials (cadar l) dum bl depth))
                (fullassumedepth q alist (1+ depth)))
              (backup ol))
            (setq om (cons (car m) om))))))

(defun fullallspecials (*thing* dum alist depth)
  (let (*answers*)
    (fullall dum (list dum) alist (1+ depth) nil)
    (nreverse (uniquify *answers*))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; fullfindp
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun fullfindp (p th)
  (fullfindx 't p th))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; fullfindx
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun fullfindx (*thing* p *theory*)
  (let (alist *answer*)
    (setq *unifications* 0)
    (setq *inferences* 0)
    (setq *termination* nil)
    (setq alist (environment))
    (when (fullone p (list p) alist 0 nil) *answer*)))

(defun fullone (p pl al depth cont)
  (setq *inferences* (1+ *inferences*))
  (fullcall p al depth)
  (cond ((>= *inferences* *limit*) (setq *termination* t) (fullstop (car pl) al depth))
        ((>= depth *depth*) (setq *termination* t) (fullstop (car pl) al depth))
        (t (fulloneexp p pl al depth cont))))

(defun fulloneexp (p pl al depth cont)
  (cond ((atom p) (fulloneconstant p pl al depth cont))
        ((eq (car p) 'not) (fullonenot (cadr p) pl al depth cont))
        ((eq (car p) 'and) (fulloneand p pl al depth cont))
        ((eq (car p) 'or) (fulloneor p pl al depth cont))
        ((eq (car p) 'same) (fullonesame p pl al depth cont))
        ((eq (car p) 'distinct) (fullonedistinct p pl al depth cont))
        ((eq (car p) 'oneof) (fulloneoneof p pl al depth cont))
        ((eq (car p) 'choose) (fullonechoose p pl al depth cont))
        ((eq (car p) 'bagofall) (fullonebagofall p pl al depth cont))
        ((eq (car p) 'unprovable) (fulloneunprovable p pl al depth cont))
	((eq (car p) 'ground) (fulloneground p pl al depth cont))
	((eq (car p) 'nonground) (fullonenonground p pl al depth cont))
        ((eq (car p) 'execute) (fulloneexecute p pl al depth cont))
        ((eq (car p) 'evaluate) (fulloneevaluate p pl al depth cont))
        ((eq (car p) 'provable) (fulloneprovabledb p pl al depth cont *theory*))
        ((eq (car p) 'stringmatch) (fullonestrmatch p pl al depth cont))
	((get (car p) 'basicval) (fullonebasicval p pl al depth cont))
        ((get (car p) 'basic) (fullonebasic p pl al depth cont))
        (t (fulloners p pl al depth cont))))

(defun fullonenot (p pl al depth cont)
  (cond ((atom p) (fullonenotconstant p pl al depth cont))
        ((eq (car p) 'not) (fulloneexp (cadr p) pl al depth cont))
        ((eq (car p) 'and) (fullonenotand p pl al depth cont))
        ((eq (car p) 'or) (fullonenotor p pl al depth cont))
        ((eq (car p) 'same) (fullfail (car pl) al depth))
        ((eq (car p) 'distinct) (fullfail (car pl) al depth))
        ((eq (car p) 'oneof) (fullonenotoneof p pl al depth cont))
        ((eq (car p) 'choose) (fullonenotchoose p pl al depth cont))
        ((eq (car p) 'bagofall) (fullonenotbagofall p pl al depth cont))
        ((eq (car p) 'unprovable) (fulloneexp (cadr p) pl al depth cont))
	((eq (car p) 'ground) (fullfail (car pl) al depth))
	((eq (car p) 'nonground) (fullfail (car pl) al depth))
        ((eq (car p) 'execute) (fullonenotexecute p pl al depth cont))
        ((eq (car p) 'evaluate) (fullonenotevaluate p pl al depth cont))
        ((eq (car p) 'stringmatch) (fullonenotstrmatch p pl al depth cont))
	((get (car p) 'basicval) (fullonenotbasicval p pl al depth cont))
        ((get (car p) 'basic) (fullonenotbasic p pl al depth cont))
        (*naf* (fulloneunprovable `(not ,p) pl al depth cont))
        (t (fulloners `(not ,p) pl al depth cont))))

(defun fulloneconstant (p pl al depth cont)
  (cond ((eq p 'true) (fullonelast pl al depth cont))
        ((eq p 'false) (fullfail (car pl) al depth))
        (t (fulloners p pl al depth cont))))

(defun fullonenotconstant (p pl al depth cont)
  (cond ((eq p 'true) (fullfail (car pl) al depth))
        ((eq p 'false) (fullonelast pl al depth cont))
        (*naf* (fulloneunprovable `(not ,p) pl al depth cont))
        (t (fulloners `(not ,p) pl al depth cont))))

(defun fulloneand (p pl al depth cont)
  (cond ((null (cdr p)) (fullonelast pl al depth cont))
        ((fullone (cadr p) (cdr p) al depth (cons (list pl al depth) cont)))
        (t (fullfail (car pl) al depth))))

(defun fullonenotand (p pl al depth cont)
  (setq cont (cons (list pl al depth) cont))
  (do ((l (cdr p) (cdr l)))
      ((null l) (fullfail (car pl) al depth))
      (when (fullone (maknot (car l)) (list (maknot (car l))) al depth cont)
        (return t))))

(defun fulloneor (p pl al depth cont)
  (setq cont (cons (list pl al depth) cont))
  (do ((l (cdr p) (cdr l)))
      ((null l) (fullfail (car pl) al depth))
      (when (fullone (car l) (list (car l)) al depth cont)
        (return t))))

(defun fullonenotor (p pl al depth cont)
  (cond ((null (cdr p)) (fullonelast pl al depth cont))
        ((fullone (maknot (cadr p)) (mapcar #'maknot (cdr p)) al depth
                  (cons (list pl al depth) cont)))
        (t (fullfail (car pl) al depth))))

(defun fullonesame (p pl al depth cont)
  (let (ol)
    (cond ((setq ol (unify (cadr p) al (caddr p) al))
           (prog1 (fullonelast pl al depth cont) (backup ol)))
          (t (fullfail (car pl) al depth)))))

(defun fullonedistinct (p pl al depth cont)
  (let (ol)
    (cond ((setq ol (unify (cadr p) al (caddr p) al))
           (backup ol) (fullfail (car pl) al depth))
          (t (fullonelast pl al depth cont)))))

(defun fulloneoneof (p pl al depth cont)
  (when (seqvarp (caddr p)) (setq p (plug p al)))
  (do ((l (cddr p) (cdr l)) (ol))
      ((null l) (fullfail (car pl) al depth))
      (cond ((not (setq ol (unify (cadr p) al (car l) al))))
            ((fulloneexit pl al depth cont) (backup ol) (return t))
            (t (backup ol)))))

(defun fullonenotoneof (p pl al depth cont)
  (when (seqvarp (caddr p)) (setq p (plug p al)))
  (do ((l (cddr p) (cdr l)) (ol))
      ((null l) (fullonelast pl al depth cont))
      (when (setq ol (unify (cadr p) al (car l) al))
        (backup ol)
        (return (fullfail (car pl) al depth)))))

(defun fullonechoose (p pl al depth cont)
  (let (x ol)
    (setq p (plugstdexp p al))
    (setq x (fullfindx (cadr p) (caddr p) *theory*))
    (cond ((and (not (null x)) (setq ol (unify (cadddr p) alist x alist)))
           (prog1 (fullonelast pl al depth cont) (backup ol)))
          (t (fullfail (car pl) al depth)))))

(defun fullonenotchoose (p pl al depth cont)
  (setq p (plugstdexp p al))
  (cond ((findp (caddr p) *theory*) (fullfail (car pl) al depth))
        (t (fullonelast pl al depth cont))))

(defun fullonebagofall (p pl al depth cont)
  (let (answer ol)
    (setq p (plug p al))
    (setq answer (cons 'listof (fullfinds (cadr p) (caddr p) *theory*)))
    (cond ((setq ol (unify answer al (cadddr p) al))
           (prog1 (fullonelast pl al depth cont) (backup ol)))
          (t (fullfail (car pl) al depth)))))

(defun fullonenotbagofall (p pl al depth cont)
  (let (answer ol)
    (setq p (plug p al))
    (setq answer (cons 'listof (fullfinds (cadr p) (caddr p) *theory*)))
    (cond ((setq ol (unify answer al (cadddr p) al))
           (backup ol) (fullfail (car pl) al depth))
          (t (fullonelast pl al depth cont)))))

(defun fulloneunprovable (p pl al depth cont)
  (cond ((fullone (cadr p) (cdr p) al depth nil) (fullfail (car pl) al depth))
        (t (fullonelast pl al depth cont))))

(defun fulloneground (p pl al depth cont)
  (cond ((and (groundp (plug (cadr p) al))) (fullonelast pl al depth cont))
        (t (fullfail (car pl) al depth))))

(defun fullonenonground (p pl al depth cont)
  (cond ((groundp (plug (cadr p) al)) (fullfail (car pl) al depth))
        (t (fullonelast pl al depth cont))))

(defun fulloneexecute (p pl al depth cont)
  (let (values ol)
    (setq p (plug p al))
    (cond ((null (cddr p))
           (cond ((ignore-errors (eval (cadr p)))
                  (fullonelast pl al depth cont))
                 (t (fullfail (car pl) al depth))))
          ((and (car (setq values (ignore-errors (multiple-value-list (eval (cadr p))))))
                (setq ol (matchify `(listof . ,(cddr p)) al `(listof . ,(mapcar #'quotify values)) al)))
           (prog1 (fullonelast pl al depth cont) (backup ol)))
          (t (fullfail (car pl) al depth)))))

(defun fullonenotexecute (p pl al depth cont)
  (let (values ol)
    (setq p (plug p al))
    (cond ((null (cddr p))
           (cond ((ignore-errors (not (eval (cadr p))))
                  (fullonelast pl al depth cont))
                 (t (fullfail (car pl) al depth))))
          ((and (car (setq values (ignore-errors (multiple-value-list (eval (cadr p))))))
                (not (setq ol (matchify `(listof . ,(cddr p)) al `(listof . ,(mapcar #'quotify values)) al))))
           (fullonelast pl al depth cont))
          (t (backup ol) (fullfail (car pl) al depth)))))          

(defun fulloneevaluate (p pl al depth cont)
  (let (values ol)
    (setq p (plug p al))
    (cond ((null (cddr p))
           (cond ((evals (cadr p)) (fullonelast pl al depth cont))
                 (t (fullfail (car pl) al depth))))
          ((and (car (setq values (evals (cadr p))))
                (setq ol (matchify `(listof . ,(cddr p)) al `(listof . ,values) al)))
           (prog1 (fullonelast pl al depth cont) (backup ol)))
          (t (fullfail (car pl) al depth)))))

(defun evals (x)
  (cond ((atom x) (ignore-errors (list (eval x))))
        (t (ignore-errors (multiple-value-list (apply (car x) (cdr x)))))))

(defun fullonenotevaluate (p pl al depth cont)
  (let (values ol)
    (setq p (plug p al))
    (cond ((null (cddr p))
           (cond ((ignore-errors (not (apply (caadr p) (cdadr p))))
                  (fullonelast pl al depth cont))
                 (t (fullfail (car pl) al depth))))
          ((and (car (setq values (ignore-errors (multiple-value-list (apply (caadr p) (cdadr p))))))
                (not (setq ol (matchify `(listof . ,(cddr p)) al `(listof . ,values) al))))
           (fullonelast pl al depth cont))
          (t (backup ol) (fullfail (car pl) al depth)))))

(defun fullonestrmatch (p pl al depth cont)
  (fulloneexp `(execute (strmatches ,(cadr p) ,(caddr p)) ? . ,(cdddr p)) pl al depth cont))

(defun fullonenotstrmatch (p pl al depth cont)
  (fullonenot `(execute (strmatches ,(cadr p) ,(caddr p)) ? . ,(cdddr p)) pl al depth cont))

(defun fullonebasicval (p pl al depth cont)
  (let (x y ol)
    (setq p (plug p al) x (butlast p) y (car (last p)))
    (cond ((not (groundp x)) (fullfail (car pl) al depth))
          ((setq ol (unify (funcall (get (car x) 'basicval) x) al y al))
           (prog1 (fullonelast pl al depth cont) (backup ol)))
          (t (fullfail (car pl) al depth)))))

(defun fullonenotbasicval (p pl al depth cont)
  (let (x y ol)
    (setq p (plug p al) x (butlast p) y (car (last p)))
    (cond ((not (groundp x)) (fullfail (car pl) al depth))
          ((setq ol (unify (funcall (get (car x) 'basicval) x) al y al))
           (backup ol) (fullfail (car pl) al depth))
          (t (fullonelast pl al depth cont)))))

(defun fullonebasic (p pl al depth cont)
  (setq p (plug p al))
  (cond ((and (groundp p) (apply (get (car p) 'basic) (cdr p)))
         (fullonelast pl al depth cont))
        (t (fullfail (car pl) al depth))))

(defun fullonenotbasic (p pl al depth cont)
  (setq p (plug p al))
  (cond ((and (groundp p) (not (apply (get (car p) 'basic) (cdr p))))
         (fullonelast pl al depth cont))
        (t (fullfail (car pl) al depth))))

(defun fulloners (p pl al depth cont)
  (cond ((and *ancestry* (fulloneancestor p al cont)) (fullfail (car pl) al depth))
        ((and *reduction* (fullonereduce p pl al depth cont)))
        ((fullonedb p pl al depth cont *theory*))
        (t (fullfail (car pl) al depth))))

(defun fulloneancestor (p al cont)
  (do ((l (cdr cont) (cdr l)))
      ((null l) nil)
      (if (identify (caaar l) (cadar l) p al) (return t))))

(defun fullonereduce (p pl al depth cont)
  (do ((l cont (cdr l)) (ol))
      ((null l))
      (cond ((not (setq ol (unify (maknot (caaar l)) (cadar l) p al))))
            ((fulloneexit pl al depth cont) (backup ol) (return t))
            (t (backup ol)))))

(defun fullonedb (p pl al depth cont th)
  (cond ((fulloneth p pl al depth cont th))
        (t (do ((l (includees th) (cdr l)))
               ((null l) nil)
               (when (fullonedb p pl al depth cont (car l)) (return t))))))

(defun fulloneth (p pl al depth cont th)
  (do ((l (envindexps p al th) (cdr l)) (bl (environment)) (ol))
      ((null l))
      (cond ((and (listp (car l)) (eq '<= (caar l)) (null (cddar l)))
             (cond ((not (setq ol (unify (cadar l) bl p al))))
                   ((fulloneexit pl al depth cont) (backup ol) (return t))
                   ((subolp ol (alist bl)) (backup ol) (return nil))
                   (t (backup ol))))
            ((and (listp (car l)) (eq '<= (caar l)))
             (cond ((not (setq ol (unify (cadar l) bl p al))))
                   ((fullone (caddar l) (cddar l) bl
                             (1+ depth) (cons (list pl al depth) cont))
                    (backup ol) (return  t))
                   (t (backup ol))))
            ((setq ol (unify (car l) bl p al))
             (cond ((fulloneexit pl al depth cont) (backup ol) (return t))
                   ((subolp ol (alist bl)) (backup ol) (return nil))
                   (t (backup ol)))))))

(defun fulloneexit (pl al depth cont)
  (let (ans)
    (fullexit (car pl) al depth)
    (cond ((cdr pl) (setq ans (fullone (cadr pl) (cdr pl) al depth cont)))
          (cont (setq ans (fulloneexit (caar cont) (cadar cont) (caddar cont) (cdr cont))))
          (t (setq *answer* (plugstdexp *thing* alist) ans t)))
    (if ans t (fullredo (car pl) al depth))))

(defun fullonelast (pl al depth cont)
  (fullexit (car pl) al depth)
  (cond ((cdr pl) (fullone (cadr pl) (cdr pl) al depth cont))
        (cont (fulloneexit (caar cont) (cadar cont) (caddar cont) (cdr cont)))
        (t (setq *answer* (plugstdexp *thing* alist)))))

(defun fullcall (p al depth)
  (cond ((not traceexpressions) nil)
        ((find (setq p (plugstdexp p al)) traceexpressions :test #'instp)
         (tracemessage depth '|Call: | p) nil)))

(defun fullexit (p al depth)
  (cond ((not traceexpressions) nil)
        ((find (setq p (plugstdexp p al)) traceexpressions :test #'instp)
         (tracemessage depth '|Exit: | p) nil)))

(defun fullredo (p al depth)
  (cond ((not traceexpressions) nil)
        ((find (setq p (plugstdexp p al)) traceexpressions :test #'instp)
         (tracemessage depth '|Redo: | p) nil)))

(defun fullfail (p al depth)
  (cond ((not traceexpressions) nil)
        ((find (setq p (plugstdexp p al)) traceexpressions :test #'instp)
         (tracemessage depth '|Fail: | p) nil)))

(defun fullstop (p al depth)
  (cond ((not traceexpressions) nil)
        ((find (setq p (plugstdexp p al)) traceexpressions :test #'instp)
         (tracemessage depth '|Stop: | p) nil)))

(defun fullsave (p al depth)
  (cond ((not traceexpressions) nil)
        ((find (setq p (plugstdexp p al)) traceexpressions :test #'instp)
         (tracemessage depth '|Save: | p) nil)))

(defun fulldrop (p al depth)
  (cond ((not traceexpressions) nil)
        ((find (setq p (plugstdexp p al)) traceexpressions :test #'instp)
         (tracemessage depth '|Drop: | p) nil)))

(defun fulldone (p al depth)
  (cond ((not traceexpressions) nil)
        ((find (setq p (plugstdexp p al)) traceexpressions :test #'instp)
         (tracemessage depth '|Done: | p) nil)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; fullfinds
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; UPDATED
(defun fullfinds (*thing* p *theory* &optional (*filter* #'failure))
  (let (alist *answers*)
    (setq *unifications* 0)
    (setq *inferences* 0)
    (setq *termination* nil)
    (setq alist (environment))
    (let ((*reduction* (not (eq (type-of *theory*) 'prologtheory))))
      (fullall p (list p) alist 0 nil)
      (nreverse (uniquify *answers*)))))

; UPDATED
(defun fullall (p pl al depth cont)
  (setq *inferences* (1+ *inferences*))
  (fullcall p al depth)
  (cond ((>= *inferences* *limit*) (setq *termination* t) (fullstop (car pl) al depth))
	((>= depth *depth*) (setq *termination* t) (fullstop (car pl) al depth))
        ((and *tautology-elim* (epilog-tautologyp p pl al cont)) (fullstop (car pl) al depth))
        (t (fullallexp p pl al depth cont))))

(defun fullallexp (p pl al depth cont)
  (cond ((atom p) (fullallconstant p pl al depth cont))
        ((eq (car p) 'not) (fullallnot (cadr p) pl al depth cont))
        ((eq (car p) 'and) (fullalland p pl al depth cont))
        ((eq (car p) 'or) (fullallor p pl al depth cont))
        ((eq (car p) 'same) (fullallsame p pl al depth cont))
        ((eq (car p) 'distinct) (fullalldistinct p pl al depth cont))
        ((eq (car p) 'oneof) (fullalloneof p pl al depth cont))
        ((eq (car p) 'choose) (fullallchoose p pl al depth cont))
        ((eq (car p) 'bagofall) (fullallbagofall p pl al depth cont))
        ((eq (car p) 'unprovable) (fullallunprovable p pl al depth cont))
	((eq (car p) 'ground) (fullallground p pl al depth cont))
	((eq (car p) 'nonground) (fullallnonground p pl al depth cont))
        ((eq (car p) 'execute) (fullallexecute p pl al depth cont))
        ((eq (car p) 'evaluate) (fullallevaluate p pl al depth cont))
        ((eq (car p) 'strmatch) (fullallstrmatch p pl al depth cont))
	((get (car p) 'basicval) (fullallbasicval p pl al depth cont))
        ((get (car p) 'basic) (fullallbasic p pl al depth cont))
        ((funcall *filter*) (fullallknowndb p pl al depth cont *theory*))
        (t (fullallrs p pl al depth cont))))

(defun fullallnot (p pl al depth cont)
  (cond ((atom p) (fullallnotconstant p pl al depth cont))
        ((eq (car p) 'not) (fullallexp (cadr p) pl al depth cont))
        ((eq (car p) 'and) (fullallnotand p pl al depth cont))
        ((eq (car p) 'or) (fullallnotor p pl al depth cont))
        ((eq (car p) 'same) (fullfail (car pl) al depth))
        ((eq (car p) 'distinct) (fullfail (car pl) al depth))
        ((eq (car p) 'oneof) (fullallnotoneof p pl al depth cont))
        ((eq (car p) 'choose) (fullallnotchoose p pl al depth cont))
        ((eq (car p) 'bagofall) (fullallnotbagofall p pl al depth cont))
        ((eq (car p) 'unprovable) (fullallexp (cadr p) pl al depth cont))
	((eq (car p) 'ground) (fullfail (car pl) al depth))
	((eq (car p) 'nonground) (fullfail (car pl) al depth))
        ((eq (car p) 'execute) (fullallnotexecute p pl al depth cont))
        ((eq (car p) 'evaluate) (fullallnotevaluate p pl al depth cont))
        ((eq (car p) 'strmatch) (fullallnotstrmatch p pl al depth cont))
	((get (car p) 'basicval) (fullallnotbasicval p pl al depth cont))
        ((get (car p) 'basic) (fullallnotbasic p pl al depth cont))
        (*naf* (fullallunprovable `(not ,p) pl al depth cont))
        (t (fullallrs `(not ,p) pl al depth cont))))

(defun fullallconstant (p pl al depth cont)
  (cond ((eq p 'true) (fullalllast pl al depth cont))
        ((eq p 'false) (fullfail (car pl) al depth))
        (t (fullallrs p pl al depth cont))))

(defun fullallnotconstant (p pl al depth cont)
  (cond ((eq p 'true) (fullfail (car pl) al depth))
        ((eq p 'false) (fullalllast pl al depth cont))
        (*naf* (fullallunprovable `(not ,p) pl al depth cont))
        (t (fullallrs `(not ,p) pl al depth cont))))

(defun fullalland (p pl al depth cont)
  (cond ((null (cdr p)) (fullalllast pl al depth cont))
        ((fullall (cadr p) (cdr p) al depth (cons (list pl al depth) cont)))
        (t (fullfail (car pl) al depth))))

(defun fullallnotand (p pl al depth cont)
  (setq cont (cons (list pl al depth) cont))
  (do ((l (cdr p) (cdr l)))
      ((null l) (fullfail (car pl) al depth))
      (fullall (maknot (car l)) (list (maknot (car l))) al depth cont)))

(defun fullallor (p pl al depth cont)
  (setq cont (cons (list pl al depth) cont))
  (do ((l (cdr p) (cdr l)))
      ((null l) (fullfail (car pl) al depth))
      (fullall (car l) (list (car l)) al depth cont)))

(defun fullallnotor (p pl al depth cont)
  (cond ((null (cdr p)) (fullalllast pl al depth cont))
        ((fullall (maknot (cadr p)) (mapcar #'maknot (cdr p)) al depth
                  (cons (list pl al depth) cont)))
        (t (fullfail (car pl) al depth))))

(defun fullallsame (p pl al depth cont)
  (let (ol)
    (cond ((setq ol (unify (cadr p) al (caddr p) al))
           (fullalllast pl al depth cont)
           (backup ol))
          (t (fullfail p al depth)))))

(defun fullalldistinct (p pl al depth cont)
  (let (ol)
    (cond ((setq ol (unify (cadr p) al (caddr p) al))
           (backup ol) (fullfail p al depth))
          (t (fullalllast pl al depth cont)))))

(defun fullalloneof (p pl al depth cont)
  (when (seqvarp (caddr p)) (setq p (plug p al)))
  (do ((l (cddr p) (cdr l)) (ol))
      ((null l) (fullfail (car pl) al depth))
      (when (setq ol (unify (cadr p) al (car l) al))
        (fullallexit pl al depth cont)
        (backup ol))))

(defun fullallnotoneof (p pl al depth cont)
  (when (seqvarp (caddr p)) (setq p (plug p al)))
  (do ((l (cddr p) (cdr l)) (ol))
      ((null l) (fullalllast pl al depth cont))
      (when (setq ol (unify (cadr p) al (car l) al))
        (backup ol)
        (return (fullfail (car pl) al depth)))))

(defun fullallchoose (p pl al depth cont)
  (let (x ol)
    (setq p (plugstdexp p al))
    (setq x (findx (cadr p) (caddr p) *theory*))
    (cond ((and (not (null x)) (setq ol (unify (cadr p) alist x alist)))
           (prog1 (fullallexit pl al depth cont) (backup ol)))
          (t (fullfail (car pl) al depth)))))

(defun fullallnotchoose (p pl al depth cont)
  (setq p (plugstdexp p al))
  (cond ((findp (caddr p) *theory*) (fullfail (car pl) al depth))
        (t (fullallexit pl al depth cont))))

(defun fullallbagofall (p pl al depth cont)
  (let (answer ol)
    (setq p (plug p al))
    (setq answer (cons 'listof (fullfinds (cadr p) (caddr p) *theory*)))
    (cond ((setq ol (unify answer al (cadddr p) al))
           (prog1 (fullalllast pl al depth cont) (backup ol)))
          (t (fullfail (car pl) al depth)))))

(defun fullallnotbagofall (p pl al depth cont)
  (let (answer ol)
    (setq p (plug p al))
    (setq answer (cons 'listof (fullfinds (cadr p) (caddr p) *theory*)))
    (cond ((setq ol (unify answer al (cadddr p) al))
           (backup ol) (fullfail (car pl) al depth))
          (t (fullalllast pl al depth cont)))))

(defun fullallunprovable (p pl al depth cont)
  (cond ((fullone (cadr p) (cdr p) al depth nil) (fullfail (car pl) al depth))
        (t (fullalllast pl al depth cont))))

(defun fullallground (p pl al depth cont)
  (setq p (plug p al))
  (if (groundp p) (fullallexit pl al depth cont)))

(defun fullallnonground (p pl al depth cont)
  (setq p (plug p al))
  (if (groundp p) nil (fullallexit pl al depth cont)))

(defun fullallexecute (p pl al depth cont)
  (let (values ol)
    (setq p (plug p al))
    (cond ((null (cddr p))
           (cond ((ignore-errors (eval (cadr p))) (fullallexit pl al depth cont))
                 (t (fullfail (car pl) al depth))))
          ((and (car (setq values (ignore-errors (multiple-value-list (eval (cadr p))))))
                (setq ol (matchify `(listof . ,(cddr p)) al `(listof . ,(mapcar #'quotify values)) al)))
           (prog1 (fullallexit pl al depth cont) (backup ol)))
          (t (fullfail (car pl) al depth)))))

(defun fullallnotexecute (p pl al depth cont)
  (let (values ol)
    (setq p (plug p al))
    (cond ((null (cddr p))
           (cond ((ignore-errors (not (eval (cadr p)))) (fullallexit pl al depth cont))
                 (t (fullfail (car pl) al depth))))
          ((and (car (setq values (ignore-errors (multiple-value-list (eval (cadr p))))))
                (not (setq ol (matchify `(listof . ,(cddr p)) al `(listof . ,(mapcar #'quotify values)) al)))
           (fullallexit pl al depth cont)))
          (t (backup ol) (fullfail (car pl) al depth)))))          

(defun fullallevaluate (p pl al depth cont)
  (let (values ol)
    (setq p (plug p al))
    (cond ((null (cddr p))
           (cond ((ignore-errors (apply (caadr p) (cdadr p))) (fullallexit pl al depth cont))
                 (t (fullfail (car pl) al depth))))
          ((and (car (setq values (ignore-errors (multiple-value-list (apply (caadr p) (cdadr p))))))
                (setq ol (matchify `(listof . ,(cddr p)) al `(listof . ,values) al)))
           (prog1 (fullallexit pl al depth cont) (backup ol)))
          (t (fullfail (car pl) al depth)))))

(defun fullallnotevaluate (p pl al depth cont)
  (let (values ol)
    (setq p (plug p al))
    (cond ((null (cddr p))
           (cond ((ignore-errors (not (apply (caadr p) (cdadr p)))) (fullallexit pl al depth cont))
                 (t (fullfail (car pl) al depth))))
          ((and (car (setq values (ignore-errors (multiple-value-list (apply (caadr p) (cdadr p))))))
                (not (setq ol (matchify `(listof . ,(cddr p)) al `(listof . ,values) al))))
           (fullallexit pl al depth cont))
          (t (backup ol) (fullfail (car pl) al depth)))))

(defun fullallstrmatch (p pl al depth cont)
  (fullallexp `(execute (strmatches ,(cadr p) ,(caddr p)) ? . ,(cdddr p)) pl al depth cont))

(defun fullallnotstrmatch (p pl al depth cont)
  (fullallnot `(execute (strmatches ,(cadr p) ,(caddr p)) ? . ,(cdddr p)) pl al depth cont))

(defun fullallbasicval (p pl al depth cont)
  (let (x y ol)
    (setq p (plug p al) x (butlast p) y (car (last p)))
    (cond ((not (groundp x)) (fullfail (car pl) al depth))
          ((setq ol (unify (funcall (get (car x) 'basicval) x) al y al))
           (prog1 (fullalllast pl al depth cont) (backup ol)))
          (t (fullfail (car pl) al depth)))))

(defun fullallnotbasicval (p pl al depth cont)
  (let (x y ol)
    (setq p (plug p al) x (butlast p) y (car (last p)))
    (cond ((not (groundp x)) (fullfail (car pl) al depth))
          ((setq ol (unify (funcall (get (car x) 'basicval) x) al y al))
           (backup ol) (fullfail (car pl) al depth))
          (t (fullalllast pl al depth cont)))))

(defun fullallbasic (p pl al depth cont)
  (setq p (plug p al))
  (cond ((and (groundp p) (apply (get (car p) 'basic) (cdr p))
              (fullallexit pl al depth cont)))
        (t (fullfail (car pl) al depth))))

(defun fullallnotbasic (p pl al depth cont)
  (setq p (plug p al))
  (cond ((and (groundp p) (not (apply (get (car p) 'basic) (cdr p)))
              (fullallexit pl al depth cont)))
        (t (fullfail (car pl) al depth))))

(defun fullallrs (p pl al depth cont)
  (cond ((and *ancestry* (fullallancestor p al cont)) (fullfail (car pl) al depth))
        ((and (numberp *ancestry*) (fullallnumber p al cont 0))
         (setq *termination* t) (fullfail (car pl) al depth))
        ((and *reduction* (fullallreduce p pl al depth cont)))
        ((fullalldb p pl al depth cont *theory*))
        (t (fullfail (car pl) al depth))))

(defun fullallancestor (p al cont)
  (do ((l cont (cdr l)))
      ((null l) nil)
      (if (identify (caaar l) (cadar l) p al) (return t))))

(defun fullallnumber (p al cont n)
  (let (ol)
    (cond ((numgeqp n *ancestry*))
          ((null cont) nil)
          ((atom p)
           (fullallnumber p al (cdr cont) (if (eq p (caaar cont)) (1+ n) n)))
          ((setq ol (unify p al (caaar cont) (cadar cont)))
           (prog1 (fullallnumber p al (cdr cont) (1+ n)) (backup ol)))
          (t (fullallnumber p al (cdr cont) n)))))

(defun fullallreduce (p pl al depth cont)
  (do ((l cont (cdr l)) (ol))
      ((null l))
      (when (setq ol (unify (maknot (caaar l)) (cadar l) p al))
        (fullallexit pl al depth cont)
        (backup ol))))

(defun fullalldb (p pl al depth cont th)
  (fullallth p pl al depth cont th)
  (do ((l (includees th) (cdr l)))
      ((null l))
      (fullalldb p pl al depth cont (car l))))

; UPDATED
; avoid parts of the search space by checking if the answer for this part of the space
;   has already been found.
; Should probably remove the first and third already-foundp checks since finding
;    redundancy here cannot prune any of the search space, and making the check for every
;    data item will likely be much more expensive than just uniquifying at the end.
(defun fullallth (p pl al depth cont th)
  (do ((l (envindexps p al th) (cdr l)) (bl (environment)) (ol))
      ((null l))
    (cond ((and (listp (car l)) (eq '<= (caar l)) (null (cddar l)))
           (when (setq ol (unify (cadar l) bl p al))
             (cond  ((already-foundp) (backup ol))
                    (t 
                     (fullallexit pl al depth cont)
                     (cond ((subolp ol (alist bl)) (backup ol) (return nil))
                           (t (backup ol)))))))
          ((and (listp (car l)) (eq '<= (caar l)))
           (when (setq ol (unify (cadar l) bl p al))
             (cond  ((already-foundp) (backup ol))
                    (t 
		     (fullall (caddar l) (cddar l) bl
			      (1+ depth) (cons (list pl al depth) cont))
		     (backup ol)))))
          ((setq ol (unify (car l) bl p al))
           (cond  ((already-foundp) (backup ol))
                  (t 
		   (fullallexit pl al depth cont)
		   (cond ((subolp ol (alist bl)) (backup ol) (return nil))
			 (t (backup ol)))))))))

(defun already-foundp () 
  ;(format t "    ~A~%" *answers*) 
  (member (plugstdexp *thing* alist) *answers* :test #'matchp-flipped))

(defun matchp-flipped (x y) (matchp y x))

(defun fullallknowndb (p pl al depth cont th)
  (fullallknownth p pl al depth cont th)
  (do ((l (includees th) (cdr l)))
      ((null l))
      (fullallknowndb p pl al depth cont (car l))))

(defun fullallknownth (p pl al depth cont th)
  (do ((l (newindexps p al th) (cdr l)) (bl (environment)) (ol))
      ((null l) (fullfail (car pl) al depth))
      (when (setq ol (unify (car l) bl p al))
        (fullallexit pl al depth cont)
        (cond ((subolp ol (alist bl)) (backup ol) (return nil))
              (t (backup ol))))))

(defun fullallexit (pl al depth cont)
  (fullexit (car pl) al depth)
  (cond ((cdr pl) (fullall (cadr pl) (cdr pl) al depth cont))
        (cont (fullallexit (caar cont) (cadar cont) (caddar cont) (cdr cont)))
        (t (setq *answers* (cons (plugstdexp *thing* alist) *answers*))))
  (fullredo (car pl) al depth))

(defun fullalllast (pl al depth cont)
  (fullexit (car pl) al depth)
  (cond ((cdr pl) (fullall (cadr pl) (cdr pl) al depth cont))
        (cont (fullallexit (caar cont) (cadar cont) (caddar cont) (cdr cont)))
        (t (setq *answers* (cons (plugstdexp *thing* alist) *answers*)) nil)))

(defmethod newindexps (p al th)
  (envindexps p al th))

(defmethod newindexps (p al (th symbol))
  (cond ((and (boundp th) (not (symbolp (symbol-value th))))
         (newindexps p al (symbol-value th)))
        (t (call-next-method p al th))))

#|
(defun epilog-tautologyp (p pl al context)
  "(EPILOG-TAUTOLOGYP PL AL CONTEXT) returns T if there are complementary literals on the stack."
  (declare (ignore pl))

  ; assume no tautologies within the rules, so that pl is not a tautology.
  ; check whether p is a tautology wrt context

  ;(format t "P: ~A, Alist(AL): ~A, plug(p,al): ~A~%" p (alist al) (plugstdexp p al))
  (setq p (plugstdexp p al))

  ; walk over all the contexts [outer loop]
  (do ((cs context (cdr cs))
       (taut nil) (cal nil))
      ((or (null cs) taut) taut)

      ;(format t "Context: ~A~%" (car cs))

      ; grab the binding list for the current context
      (setq cal (second (car cs)))

      ; walk over the literals in the context
      (do ((lits (caar cs) (cdr lits)))
          ((or (null lits) taut))

        ;(format t "    Lit: ~A, AL: ~A, alist(al): ~A, plugged lit: ~A~%" (car lits) al (alist cal) (plugstdexp (car lits) cal))

        (when (complementaryp p (plugstdexp (car lits) cal))
          (setq taut t)
          (return)))))


(defun complementaryp (lit1 lit2)
  "(COMPLEMENTARYP LIT1 LIT2) returns T iff LIT1 and LIT2 are complementary, i.e.
   one is p and one is (not p)."
  (cond ((atom lit1) 
         (cond ((atom lit2) nil)
               ((and (eq (car lit2) 'not) (eq lit1 (cadr lit2))) t)
               (t nil)))
        ((and (eq (car lit1) 'not) (equal (cadr lit1) lit2)) t)
        ((atom lit2) nil)
        ((eq (car lit2) 'not) (equal lit1 (cadr lit2)))
        (t nil)))

(defun similarp (x y)
  "(SIMILARP X Y) returns T iff X is subsumed by Y, i.e. there is a variable 
   assignment sigma such that Y.sigma is a subset of X."
  (cond ((not (and (consp x) (consp y))) nil) 
        (*subsumption* (subsumptionp y x))
        (t (samep x y))))

(defun subsumptionp (p q)
  (subsumptionps p q truth))

(defun subsumptionps (pl ql al)
  (cond ((null pl) al)
        (t (do ((m ql (cdr m)) (bl))
               ((null m))
             (if (and (setq bl (match (car pl) (car m) al))
                      (setq bl (subsumptionps (cdr pl) ql bl)))
               (return bl))))))
|#

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; fullresidue
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defparameter *filter* nil)
(defparameter *variables* nil)

(defun fullresidue (p *theory* &optional (*filter* #'basep) (*test* #'success))
  (let (alist *residue* *answer* tracecalls)
    (setq *unifications* 0)
    (setq *inferences* 0)
    (setq *termination* nil)
    (setq alist (environment))
    (setq *variables* p)
    (when (fullresiduedepth p (list p) alist 0 nil) *answer*)))

(defun fullresiduedepth (p pl al depth cont)
  (setq *inferences* (1+ *inferences*))
  (fullcall p al depth)
  (cond ((>= *inferences* *limit*) (setq *termination* t) (fullstop (car pl) al depth))
        ((>= depth *depth*) (setq *termination* t) (fullstop (car pl) al depth))
        (t (fullresidueexp p pl al depth cont))))

(defun fullresidueexp (p pl al depth cont)
  (cond ((atom p) (fullresidueconstant p pl al depth cont))
        ((eq (car p) 'not) (fullresiduenot (cadr p) pl al depth cont))
        ((eq (car p) 'and) (fullresidueand p pl al depth cont))
        ((eq (car p) 'or) (fullresidueor p pl al depth cont))
        ((eq (car p) 'same) (fullresiduesame p pl al depth cont))
        ((eq (car p) 'distinct) (fullresiduedistinct p pl al depth cont))
        ((eq (car p) 'oneof) (fullresidueassumption p pl al depth cont))
        ((eq (car p) 'choose) (fullresiduechoose p pl al depth cont))
        ((eq (car p) 'bagofall) (fullresiduebagofall p pl al depth cont))
        ((eq (car p) 'unprovable) (fullresidueunprovable p pl al depth cont))
	((eq (car p) 'ground) (fullresidueground p pl al depth cont))
	((eq (car p) 'nonground) (fullresiduenonground p pl al depth cont))
        ((eq (car p) 'execute) (fullresidueexecute p pl al depth cont))
        ((eq (car p) 'evaluate) (fullresidueevaluate p pl al depth cont))
        ((eq (car p) 'stringmatch) (fullresiduestrmatch p pl al depth cont))
	((get (car p) 'basicval) (fullresiduebasicval p pl al depth cont))
        ((get (car p) 'basic) (fullresiduebasic p pl al depth cont))
        ((funcall *filter* (operator p)) (fullresidueassumption p pl al depth cont))
        (t (fullresiduers p pl al depth cont))))

(defun fullresiduenot (p pl al depth cont)
  (cond ((atom p) (fullresiduenotconstant p pl al depth cont))
        ((eq (car p) 'not) (fullresidueexp (cadr p) pl al depth cont))
        ((eq (car p) 'and) (fullresiduenotand p pl al depth cont))
        ((eq (car p) 'or) (fullresiduenotor p pl al depth cont))
        ((eq (car p) 'same) (fullfail (car pl) al depth))
        ((eq (car p) 'distinct) (fullfail (car pl) al depth))
        ((eq (car p) 'oneof) (fullresidueassumption `(not ,p) pl al depth cont))
        ((eq (car p) 'choose) (fullresiduenotchoose p pl al depth cont))
        ((eq (car p) 'bagofall) (fullresiduenotbagofall p pl al depth cont))
        ((eq (car p) 'unprovable) (fullresidueexp (cadr p) pl al depth cont))
	((eq (car p) 'ground) (fullfail (car pl) al depth))
	((eq (car p) 'nonground) (fullfail (car pl) al depth))
        ((eq (car p) 'execute) (fullresiduenotexecute p pl al depth cont))
        ((eq (car p) 'evaluate) (fullresiduenotevaluate p pl al depth cont))
        ((eq (car p) 'stringmatch) (fullresiduenotstrmatch p pl al depth cont))
	((get (car p) 'basicval) (fullresiduenotbasicval p pl al depth cont))
        ((get (car p) 'basic) (fullresiduenotbasic p pl al depth cont))
        ((funcall *filter* (operator p)) (fullresidueassumption `(not ,p) pl al depth cont))
        (t (fullresiduers `(not ,p) pl al depth cont))))

(defun fullresidueconstant (p pl al depth cont)
  (cond ((eq p 'true) (fullresiduelast pl al depth cont))
        ((eq p 'false) (fullfail (car pl) al depth))
        ((funcall *filter* p) (fullresidueassumption p pl al depth cont))
        (t (fullresiduers p pl al depth cont))))

(defun fullresiduenotconstant (p pl al depth cont)
  (cond ((eq p 'true) (fullfail (car pl) al depth))
        ((eq p 'false) (fullresiduelast pl al depth cont))
        ((funcall *filter* p) (fullresidueassumption `(not ,p) pl al depth cont))
        (t (fullresiduers `(not ,p) pl al depth cont))))

(defun fullresidueand (p pl al depth cont)
  (cond ((null (cdr p)) (fullresiduelast pl al depth cont))
        ((fullresiduedepth (cadr p) (cdr p) al depth (cons (list pl al depth) cont)))
        (t (fullfail (car pl) al depth))))

(defun fullresiduenotand (p pl al depth cont)
  (setq cont (cons (list pl al depth) cont))
  (do ((l (cdr p) (cdr l)))
      ((null l) (fullfail (car pl) al depth))
      (when (fullresiduedepth (maknot (car l)) (list (maknot (car l))) al depth cont)
        (return t))))

(defun fullresidueor (p pl al depth cont)
  (setq cont (cons (list pl al depth) cont))
  (do ((l (cdr p) (cdr l)))
      ((null l) (fullfail (car pl) al depth))
      (when (fullresiduedepth (car l) (list (car l)) al depth cont)
        (return t))))

(defun fullresiduenotor (p pl al depth cont)
  (cond ((null (cdr p)) (fullresiduelast pl al depth cont))
        ((fullresiduedepth (maknot (cadr p)) (mapcar #'maknot (cdr p))
                           al depth (cons (list pl al depth) cont)))
        (t (fullfail (car pl) al depth))))

(defun fullresiduesame (p pl al depth cont)
  (let (ol)
    (cond ((setq ol (unify (cadr p) al (caddr p) al))
           (prog1 (fullresiduelast pl al depth cont) (backup ol)))
          (t (fullfail (car pl) al depth)))))

(defun fullresiduedistinct (p pl al depth cont)
  (let (x y)
    (cond ((and (setq x (groundplugstdexp (cadr p) al))
                (setq y (groundplugstdexp (caddr p) al)))
           (cond ((equal x y) (fullfail (car pl) al depth))
                 (t (fullresiduelast pl al depth cont))))
          (t (fullresidueassumption p pl al depth cont)))))

(defun fullresidueoneof (p pl al depth cont)
  (when (seqvarp (caddr p)) (setq p (plug p al)))
  (do ((l (cddr p) (cdr l)) (ol))
      ((null l) (fullfail (car pl) al depth))
      (cond ((not (setq ol (unify (cadr p) al (car l) al))))
            ((fullresidueexit pl al depth cont) (backup ol) (return t))
            (t (backup ol)))))

(defun fullresiduechoose (p pl al depth cont)
  (let ((*residue* (cons p *residue*))) (fullresiduelast pl al depth cont)))

(defun fullresiduenotchoose (p pl al depth cont)
  (let ((*residue* (cons `(not ,p) *residue*))) (fullresiduelast pl al depth cont)))

(defun fullresiduebagofall (p pl al depth cont)
  (let (residue)
    (setq p (plugstdexp p al))
    (setq residue (maksor (fullresidues (caddr p) *theory* *filter* *test*)))
    (let ((*residue* (cons `(bagofall ,(cadr p) ,residue ,(cadddr p)) *residue*)))
      (fullresiduelast pl al depth cont))))

(defun fullresiduenotbagofall (p pl al depth cont)
  (let (residue)
    (setq p (plugstdexp p al))
    (setq residue (maksor (fullresidues (caddr p) *theory* *filter* *test*)))
    (let ((*residue* (cons `(not (bagofall ,(cadr p) ,residue ,(cadddr p))) *residue*)))
      (fullresiduelast pl al depth cont))))

(defun fullresidueunprovable (p pl al depth cont)
  (let (residue)
    (setq p (plugstdexp (cadr p) al))
    (setq residue (maksor (fullresidues p *theory* *filter*)))
    (do ((l (let (alist) (demorgantrick (cnfs residue))) (cdr l)))
        ((null l) (fullfail (cadr pl) al depth))
        (let ((*residue* (nreconc (car l) *residue*)))
          (when (fullresiduelast pl al depth cont) (return t))))))

(defun fullresidueground (p pl al depth cont)
  (cond ((and (groundp (plug (cadr p) al))) (fullresiduelast pl al depth cont))
        (t (fullfail (car pl) al depth))))

(defun fullresiduenonground (p pl al depth cont)
  (cond ((groundp (plug (cadr p) al)) (fullfail (car pl) al depth))
        (t (fullresiduelast pl al depth cont))))

(defun fullresidueexecute (p pl al depth cont)
  (let (values ol)
    (setq p (plug p al))
    (cond ((null (cddr p))
           (cond ((ignore-errors (eval (cadr p)))
                  (fullresiduelast pl al depth cont))
                 (t (fullfail (car pl) al depth))))
          ((and (car (setq values (ignore-errors (multiple-value-list (eval (cadr p))))))
                (setq ol (matchify `(listof . ,(cddr p)) al `(listof . ,(mapcar #'quotify values)) al)))
           (prog1 (fullresiduelast pl al depth cont) (backup ol)))
          (t (fullfail (car pl) al depth)))))

(defun fullresiduenotexecute (p pl al depth cont)
  (let (values ol)
    (setq p (plug p al))
    (cond ((null (cddr p))
           (cond ((ignore-errors (not (eval (cadr p))))
                  (fullresiduelast pl al depth cont))
                 (t (fullfail (car pl) al depth))))
          ((and (car (setq values (ignore-errors (multiple-value-list (eval (cadr p))))))
                (not (setq ol (matchify `(listof . ,(cddr p)) al `(listof . ,(mapcar #'quotify values)) al)))
           (fullresiduelast pl al depth cont)))
          (t (backup ol) (fullfail (car pl) al depth)))))          

(defun fullresidueevaluate (p pl al depth cont)
  (let (values ol)
    (setq p (plug p al))
    (cond ((null (cddr p))
           (cond ((ignore-errors (apply (caadr p) (cdadr p)))
                  (fullresiduelast pl al depth cont))
                 (t (fullfail (car pl) al depth))))
          ((and (car (setq values (ignore-errors (multiple-value-list (apply (caadr p) (cdadr p))))))
                (setq ol (matchify `(listof . ,(cddr p)) al `(listof . ,values) al)))
           (prog1 (fullresiduelast pl al depth cont) (backup ol)))
          (t (fullfail (car pl) al depth)))))

(defun fullresidueevaluate (p pl al depth cont)
  (setq p (plugstdexp p al))
  (let ((*residue* (cons p *residue*))) (fullresiduelast pl al depth cont)))

(defun fullresiduenotevaluate (p pl al depth cont)
  (let (values ol)
    (setq p (plug p al))
    (cond ((null (cddr p))
           (cond ((ignore-errors (not (apply (caadr p) (cdadr p))))
                  (fullresiduelast pl al depth cont))
                 (t (fullfail (car pl) al depth))))
          ((and (car (setq values (ignore-errors (multiple-value-list (apply (caadr p) (cdadr p))))))
                (not (setq ol (matchify `(listof . ,(cddr p)) al `(listof . ,values) al))))
           (fullresiduelast pl al depth cont))
          (t (backup ol) (fullfail (car pl) al depth)))))

(defun fullresiduestrmatch (p pl al depth cont)
  (fullresidueexp `(execute (strmatches ,(cadr p) ,(caddr p)) ? . ,(cdddr p)) pl al depth cont))

(defun fullresiduenotstrmatch (p pl al depth cont)
  (fullresiduenot `(execute (strmatches ,(cadr p) ,(caddr p)) ? . ,(cdddr p)) pl al depth cont))

(defun fullresiduebasicval (p pl al depth cont)
  (let (x y ol)
    (setq x (butlast p) y (car (last p)))
    (cond ((setq x (groundplugstdexp x al))
           (setq x (funcall (get (car x) 'basicval) x))
           (if (and (not (null x)) (setq ol (unify y al x al)))
             (prog1 (fullresiduelast pl al depth cont) (backup ol))
             (fullfail (car pl) al depth)))
          (t (fullresidueassumption p pl al depth cont)))))

(defun fullresiduenotbasicval (p pl al depth cont)
  (let (x y ol)
    (setq x (butlast p) y (car (last p)))
    (cond ((setq x (groundplugstdexp x al))
           (setq x (funcall (get (car x) 'basicval) x))
           (cond ((null x) (fullfail (car pl) al depth))
                 ((setq ol (unify y al x al)) (backup ol) (fullfail (car pl) al depth))
                 (t (fullresiduelast pl al depth cont))))
          (t (fullresidueassumption `(not ,p) pl al depth cont)))))

(defun fullresiduebasic (p pl al depth cont)
  (setq p (plug p al))
  (cond ((and (groundp p) (apply (get (car p) 'basic) (cdr p)))
         (fullresiduelast pl al depth cont))
        (t (fullfail (car pl) al depth))))

(defun fullresiduenotbasic (p pl al depth cont)
  (setq p (plug p al))
  (cond ((and (groundp p) (not (apply (get (car p) 'basic) (cdr p))))
         (fullresiduelast pl al depth cont))
        (t (fullfail (car pl) al depth))))

(defun fullresiduepartial (p pl al depth cont)
  (setq p (plugstdexp p al))
  (cond ((find p *residue* :test #'equalp) (fullresiduelast pl al depth cont))
        ((or (not *consistency*)
             (not (or (rebuttalp p *residue*) (rebuttheoryp p *theory*)))
             (consistentp p *residue*))
         (let ((*residue* (cons p *residue*))) (fullresidueexit pl al depth cont)))
        (t  (fullfail (car pl) al depth))))

(defun fullresidueassumption (p pl al depth cont)
  (setq p (plugstdexp p al))
  (cond ((find p *residue* :test #'equalp) (fullresiduelast pl al depth cont))
        ((or (not *consistency*)
             (not (or (rebuttalp p *residue*) (rebuttheoryp p *theory*)))
             (consistentp p *residue*))
         (let ((*residue* (cons p *residue*))) (fullresiduelast pl al depth cont)))
        (t  (fullfail (car pl) al depth))))

(defun fullresiduers (p pl al depth cont)
  (cond ((and *ancestry* (fullresidueancestor p al cont)) (fullfail (car pl) al depth))
        ((and *reduction* (fullresiduereduce p pl al depth cont)))
        ((fullresiduedb p pl al depth cont *theory*))
        (t (fullfail (car pl) al depth))))

(defun fullresidueancestor (p al cont)
  (do ((l cont (cdr l)))
      ((null l) nil)
      (when (identify (caaar l) (cadar l) p al) (return t))))

(defun fullresiduereduce (p pl al depth cont)
  (do ((l cont (cdr l)) (ol))
      ((null l))
      (cond ((not (setq ol (unify (maknot (caaar l)) (cadar l) p al))))
            ((fullresidueexit pl al depth cont) (backup ol) (return t))
            (t (backup ol)))))

(defun fullresiduedb (p pl al depth cont th)
  (cond ((fullresidueth p pl al depth cont th))
        (t (do ((l (includees th) (cdr l)))
               ((null l) nil)
               (when (fullresiduedb p pl al depth cont (car l)) (return t))))))

(defun fullresidueth (p pl al depth cont th)
  (do ((l (envindexps p al th) (cdr l)) (bl (environment)) (ol))
      ((null l))
      (cond ((and (listp (car l)) (eq '<= (caar l)) (null (cddar l)))
             (cond ((not (setq ol (unify (cadar l) bl p al))))
                   ((fullresidueexit pl al depth cont)
                    (backup ol)
                    (return t))
                   ((subolp ol (alist bl)) (backup ol) (return nil))
                   (t (backup ol))))
            ((and (listp (car l)) (eq '<= (caar l)))
             (cond ((not (setq ol (unify (cadar l) bl p al))))
                   ((fullresiduedepth (caddar l) (cddar l) bl
                                      (1+ depth) (cons (list pl al depth) cont))
                    (backup ol) (return  t))
                   (t (backup ol))))
            ((setq ol (unify (car l) bl p al))
             (cond ((fullresidueexit pl al depth cont)
                    (backup ol)
                    (return t))
                   ((subolp ol (alist bl)) (backup ol) (return nil))
                   (t (backup ol)))))))

(defun fullresidueexit (pl al depth cont)
  (let (dum ans)
    (fullexit (car pl) al depth)
    (cond ((cdr pl) (setq ans (fullresiduedepth (cadr pl) (cdr pl) al depth cont)))
          (cont (setq ans (fullresidueexit (caar cont) (cadar cont) (caddar cont) (cdr cont))))
          (t (setq ans (plugstdexp (reverse *residue*) alist))
             (dolist (var *variables*)
               (unless (eq (setq dum (plugstdexp var alist)) var)
                 (setq ans (cons `(same ,var ,dum) ans))))
             (setq *answer* (maksand ans) ans t)))
    (if ans t (fullredo (car pl) al depth))))

(defun fullresiduelast (pl al depth cont)
  (let (dum ans)
    (fullexit (car pl) al depth)
    (cond ((cdr pl) (fullresiduedepth (cadr pl) (cdr pl) al depth cont))
          (cont (fullresidueexit (caar cont) (cadar cont) (caddar cont) (cdr cont)))
          (t (setq ans (plugstdexp (reverse *residue*) alist))
             (dolist (var *variables*)
               (unless (eq (setq dum (plugstdexp var alist)) var)
                 (setq ans (cons `(same ,var ,dum) ans))))
             (setq *answer* (maksand ans)) t))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; fullresidues
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; UPDATED
(defvar *una* nil)
(defvar *equality* nil)
(defvar *ifreductionnoextension* nil "If reduction can be applied to a literal then do not apply extensions.")

; changed p to *thing* so I can access it later
(defun fullresidues (*thing* *theory* &optional (*filter* #'basep) (*test* #'success))
  (let (alist *variables* *residue* *answers* tracecalls)
    (setq *unifications* 0)
    (setq *inferences* 0)
    (setq *termination* nil)
    (setq alist (environment))
    (setq *variables* (vars *thing*))
    (fullresiduesdepth *thing* (list *thing*) alist 0 nil)
    (nreverse (simplify-residues *thing* *answers*))))

#|
(defun fullresidues (p *theory* &optional (*filter* #'basep) (*test* #'success))
  (let (alist *variables* *residue* *answers* tracecalls)
    (setq *unifications* 0)
    (setq *inferences* 0)
    (setq *termination* nil)
    (setq alist (environment))
    (setq *variables* p)
    (fullresiduesdepth p (list p) alist 0 nil)
    (nreverse *answers*)))
|#

; UPDATED
; added tautology-elim branch in cond
(defun fullresiduesdepth (p pl al depth cont)
  (setq *inferences* (1+ *inferences*))
  (fullcall p al depth)
  (cond ((>= *inferences* *limit*) (setq *termination* t) (fullstop (car pl) al depth))
	((>= depth *depth*) (setq *termination* t) (fullstop (car pl) al depth))
        ((and *tautology-elim* (epilog-tautologyp p pl al cont)) (fullstop (car pl) al depth))
	(t (fullresiduesexp p pl al depth cont))))

; UPDATED
; changed so that even if = is assumable, uses x=x to simplify when possible
(defun fullresiduesexp (p pl al depth cont)
  (cond ((atom p) (fullresiduesconstant p pl al depth cont))
        ((eq (car p) 'not) (fullresiduesnot (cadr p) pl al depth cont))
        ((eq (car p) 'and) (fullresiduesand p pl al depth cont))
        ((eq (car p) 'or) (fullresiduesor p pl al depth cont))
        ((eq (car p) 'same) (fullresiduessame p pl al depth cont))
        ((eq (car p) 'distinct) (fullresiduesdistinct p pl al depth cont))
        ((eq (car p) 'oneof) (fullresiduesassumption p pl al depth cont))
        ((eq (car p) 'choose) (fullresidueschoose p pl al depth cont))
        ((eq (car p) 'bagofall) (fullresiduesbagofall p pl al depth cont))
        ((eq (car p) 'unprovable) (fullresiduesunprovable p pl al depth cont))
	((eq (car p) 'ground) (fullresiduesground p pl al depth cont))
	((eq (car p) 'nonground) (fullresiduesnonground p pl al depth cont))
        ((eq (car p) 'execute) (fullresiduesexecute p pl al depth cont))
        ((eq (car p) 'evaluate) (fullresiduesevaluate p pl al depth cont))
        ((eq (car p) 'stringmatch) (fullresiduesstrmatch p pl al depth cont))
	((get (car p) 'basicval) (fullresiduesbasicval p pl al depth cont))
        ((get (car p) 'basic) (fullresiduesbasic p pl al depth cont))
        ((and *equality* (ground=samep p al)) (fullresidueslast pl al depth cont))
        ((and *equality* *una* (ground=distinctp p al)) (fullfail (car pl) al depth))
        ((funcall *filter* (car p)) (fullresiduesassumption p pl al depth cont))
        (t (fullresiduesrs p pl al depth cont))))

; whether p is an equality predicate
(defun ground=samep (p al)
  (setq p (plugstdexp p al))
  (and (eq (car p) *equality*) (groundp p) (eq (second p) (third p))))

(defun ground=distinctp (p al)
  (setq p (plugstdexp p al))
  (and (eq (car p) '=) (groundp p) (not (eq (second p) (third p)))))

(defun fullresiduesexp (p pl al depth cont)
  (cond ((atom p) (fullresiduesconstant p pl al depth cont))
        ((eq (car p) 'not) (fullresiduesnot (cadr p) pl al depth cont))
        ((eq (car p) 'and) (fullresiduesand p pl al depth cont))
        ((eq (car p) 'or) (fullresiduesor p pl al depth cont))
        ((eq (car p) 'same) (fullresiduessame p pl al depth cont))
        ((eq (car p) 'distinct) (fullresiduesdistinct p pl al depth cont))
        ((eq (car p) 'oneof) (fullresiduesassumption p pl al depth cont))
        ((eq (car p) 'choose) (fullresidueschoose p pl al depth cont))
        ((eq (car p) 'bagofall) (fullresiduesbagofall p pl al depth cont))
        ((eq (car p) 'unprovable) (fullresiduesunprovable p pl al depth cont))
	((eq (car p) 'ground) (fullresiduesground p pl al depth cont))
	((eq (car p) 'nonground) (fullresiduesnonground p pl al depth cont))
        ((eq (car p) 'execute) (fullresiduesexecute p pl al depth cont))
        ((eq (car p) 'evaluate) (fullresiduesevaluate p pl al depth cont))
        ((eq (car p) 'stringmatch) (fullresiduesstrmatch p pl al depth cont))
	((get (car p) 'basicval) (fullresiduesbasicval p pl al depth cont))
        ((get (car p) 'basic) (fullresiduesbasic p pl al depth cont))
        ((funcall *filter* (car p)) (fullresiduesassumption p pl al depth cont))
        (t (fullresiduesrs p pl al depth cont))))

; UPDATED
; changed so that even if = is assumable, uses x=x to simplify when possible
(defun fullresiduesnot (p pl al depth cont)
  (cond ((atom p) (fullresiduesnotconstant p pl al depth cont))
        ((eq (car p) 'not) (fullresiduesexp (cadr p) pl al depth cont))
        ((eq (car p) 'and) (fullresiduesnotand p pl al depth cont))
        ((eq (car p) 'or) (fullresiduesnotor p pl al depth cont))
        ((eq (car p) 'same) (fullfail (car pl) al depth))
        ((eq (car p) 'distinct) (fullfail (car pl) al depth))
        ((eq (car p) 'oneof) (fullresiduesassumption `(not ,p) pl al depth cont))
        ((eq (car p) 'choose) (fullresiduesnotchoose p pl al depth cont))
        ((eq (car p) 'bagofall) (fullresiduesnotbagofall p pl al depth cont))
        ((eq (car p) 'unprovable) (fullresiduesexp (cadr p) pl al depth cont))
	((eq (car p) 'ground) (fullfail (car pl) al depth))
	((eq (car p) 'nonground) (fullfail (car pl) al depth))
        ((eq (car p) 'execute) (fullresiduesnotexecute p pl al depth cont))
        ((eq (car p) 'evaluate) (fullresiduesnotevaluate p pl al depth cont))
        ((eq (car p) 'stringmatch) (fullresiduesnotstrmatch p pl al depth cont))
	((get (car p) 'basicval) (fullresiduesnotbasicval p pl al depth cont))
        ((get (car p) 'basic) (fullresiduesnotbasic p pl al depth cont))
        ((and *equality* (ground=samep p al)) (fullfail (car pl) al depth))
        ((and *equality* *una* (ground=distinctp p al)) (fullresidueslast pl al depth cont))
        ((funcall *filter* (car p)) (fullresiduesassumption `(not ,p) pl al depth cont))
        (t (fullresiduesrs `(not ,p) pl al depth cont))))


(defun fullresiduesconstant (p pl al depth cont)
  (cond ((eq p 'true) (fullresidueslast pl al depth cont))
        ((eq p 'false) (fullfail (car pl) al depth))
        ((funcall *filter* p) (fullresiduesassumption p pl al depth cont))
        (t (fullresiduesrs p pl al depth cont))))

(defun fullresiduesnotconstant (p pl al depth cont)
  (cond ((eq p 'true) (fullfail (car pl) al depth))
        ((eq p 'false) (fullresidueslast pl al depth cont))
        ((funcall *filter* p) (fullresiduesassumption `(not ,p) pl al depth cont))
        (t (fullresiduesrs `(not ,p) pl al depth cont))))

(defun fullresiduesand (p pl al depth cont)
  (cond ((null (cdr p)) (fullresidueslast pl al depth cont))
        ((fullresiduesdepth (cadr p) (cdr p) al depth (cons (list pl al depth) cont)))
        (t (fullfail (car pl) al depth))))

(defun fullresiduesnotand (p pl al depth cont)
  (setq cont (cons (list pl al depth) cont))
  (do ((l (cdr p) (cdr l)))
      ((null l) (fullfail (car pl) al depth))
      (fullresiduesdepth (maknot (car l)) (list (maknot (car l))) al depth cont)))

(defun fullresiduesor (p pl al depth cont)
  (setq cont (cons (list pl al depth) cont))
  (do ((l (cdr p) (cdr l)))
      ((null l) (fullfail (car pl) al depth))
      (when (fullresiduesdepth (car l) (list (car l)) al depth cont))))

(defun fullresiduesnotor (p pl al depth cont)
  (cond ((null (cdr p)) (fullresidueslast pl al depth cont))
        ((fullresiduesdepth (maknot (cadr p)) (mapcar #'maknot (cdr p)) al depth
                  (cons (list pl al depth) cont)))
        (t (fullfail (car pl) al depth))))

(defun fullresiduessame (p pl al depth cont)
  (let (ol)
    (cond ((setq ol (unify (cadr p) al (caddr p) al))
           (prog1 (fullresidueslast pl al depth cont) (backup ol)))
          (t (fullfail (car pl) al depth)))))

(defun fullresiduesdistinct (p pl al depth cont)
  (let (x y)
    (cond ((and (setq x (groundplugstdexp (cadr p) al))
                (setq y (groundplugstdexp (caddr p) al)))
           (cond ((equal x y) (fullfail (car pl) al depth))
                 (t (fullresidueslast pl al depth cont))))
          (t (fullresiduesassumption p pl al depth cont)))))

(defun fullresiduesoneof (p pl al depth cont)
  (when (seqvarp (caddr p)) (setq p (plug p al)))
  (do ((l (cddr p) (cdr l)) (ol))
      ((null l) (fullfail (car pl) al depth))
      (when (setq ol (unify (cadr p) al (car l) al))
        (fullresiduesexit pl al depth cont)
        (backup ol))))

(defun fullresidueschoose (p pl al depth cont)
  (let ((*residue* (cons p *residue*))) (fullresidueslast pl al depth cont)))

(defun fullresiduesnotchoose (p pl al depth cont)
  (let ((*residue* (cons `(not ,p) *residue*))) (fullresidueslast pl al depth cont)))

(defun fullresiduesbagofall (p pl al depth cont)
  (let (residue)
    (setq p (plugstdexp (cadr p) al))
    (setq residue (maksor (fullresidues p *theory* *filter* *test*)))
    (let ((*residue* (cons `(bagofall ,(cadr p) ,residue ,(cadddr p)) *residue*)))
          (fullresidueslast pl al depth cont))))

(defun fullresiduesnotbagofall (p pl al depth cont)
  (let (residue)
    (setq p (plugstdexp (cadr p) al))
    (setq residue (maksor (fullresidues p *theory* *filter* *test*)))
    (let ((*residue* (cons `(not (bagofall ,(cadr p) ,residue ,(cadddr p))) *residue*)))
          (fullresidueslast pl al depth cont))))

(defun fullresiduesunprovable (p pl al depth cont)
  (let (residue)
    (setq p (plugstdexp (cadr p) al))
    (setq residue (maksor (fullresidues p *theory* *filter*)))
    (do ((l (let (alist) (demorgantrick (cnfs residue))) (cdr l)))
        ((null l) (fullfail (car pl) al depth))
        (let ((*residue* (nreconc (car l) *residue*)))
          (fullresiduesexit pl al depth cont)))))

(defun fullresiduesground (p pl al depth cont)
  (cond ((and (groundp (plug (cadr p) al))) (fullresidueslast pl al depth cont))
        (t (fullfail (car pl) al depth))))

(defun fullresiduesnonground (p pl al depth cont)
  (cond ((groundp (plug (cadr p) al)) (fullfail (car pl) al depth))
        (t (fullresidueslast pl al depth cont))))

(defun fullresiduesexecute (p pl al depth cont)
  (let (values ol)
    (setq p (plug p al))
    (cond ((null (cddr p))
           (cond ((ignore-errors (eval (cadr p))) (fullresidueslast pl al depth cont))
                 (t (fullfail (car pl) al depth))))
          ((and (car (setq values (ignore-errors (multiple-value-list (eval (cadr p))))))
                (setq ol (matchify `(listof . ,(cddr p)) al `(listof . ,(mapcar #'quotify values)) al)))
           (prog1 (fullresidueslast pl al depth cont) (backup ol)))
          (t (fullfail (car pl) al depth)))))

(defun fullresiduesnotexecute (p pl al depth cont)
  (let (values ol)
    (setq p (plug p al))
    (cond ((null (cddr p))
           (cond ((ignore-errors (not (eval (cadr p)))) (fullresidueslast pl al depth cont))
                 (t (fullfail (car pl) al depth))))
          ((and (car (setq values (ignore-errors (multiple-value-list (eval (cadr p))))))
                (not (setq ol (matchify `(listof . ,(cddr p)) al `(listof . ,(mapcar #'quotify values)) al)))
           (fullresidueslast pl al depth cont)))
          (t (backup ol) (fullfail (car pl) al depth)))))          

(defun fullresiduesevaluate (p pl al depth cont)
  (let (values ol)
    (setq p (plug p al))
    (cond ((null (cddr p))
           (cond ((ignore-errors (apply (caadr p) (cdadr p))) (fullresidueslast pl al depth cont))
                 (t (fullfail (car pl) al depth))))
          ((and (car (setq values (ignore-errors (multiple-value-list (apply (caadr p) (cdadr p))))))
                (setq ol (matchify `(listof . ,(cddr p)) al `(listof . ,values) al)))
           (prog1 (fullresidueslast pl al depth cont) (backup ol)))
          (t (fullfail (car pl) al depth)))))

(defun fullresiduesevaluate (p pl al depth cont)
  (setq p (plugstdexp p al))
  (let ((*residue* (cons p *residue*))) (fullresidueslast pl al depth cont)))

(defun fullresiduesnotevaluate (p pl al depth cont)
  (let (values ol)
    (setq p (plug p al))
    (cond ((null (cddr p))
           (cond ((ignore-errors (not (apply (caadr p) (cdadr p)))) (fullresidueslast pl al depth cont))
                 (t (fullfail (car pl) al depth))))
          ((and (car (setq values (ignore-errors (multiple-value-list (apply (caadr p) (cdadr p))))))
                (not (setq ol (matchify `(listof . ,(cddr p)) al `(listof . ,values) al))))
           (fullresidueslast pl al depth cont))
          (t (backup ol) (fullfail (car pl) al depth)))))

(defun fullresiduesstrmatch (p pl al depth cont)
  (fullresiduesexp `(execute (strmatches ,(cadr p) ,(caddr p)) ? . ,(cdddr p)) pl al depth cont))

(defun fullresiduesnotstrmatch (p pl al depth cont)
  (fullresiduesnot `(execute (strmatches ,(cadr p) ,(caddr p)) ? . ,(cdddr p)) pl al depth cont))

(defun fullresiduesbasicval (p pl al depth cont)
  (let (x y ol)
    (setq x (butlast p) y (car (last p)))
    (cond ((setq x (groundplugstdexp x al))
           (setq x (funcall (get (car x) 'basicval) x))
           (if (and (not (null x)) (setq ol (unify y al x al)))
             (prog1 (fullresidueslast pl al depth cont) (backup ol))
             (fullfail (car pl) al depth)))
          (t (fullresiduesassumption p pl al depth cont)))))

(defun fullresiduesnotbasicval (p pl al depth cont)
  (let (x y ol)
    (setq x (butlast p) y (car (last p)))
    (cond ((setq x (groundplugstdexp x al))
           (setq x (funcall (get (car x) 'basicval) x))
           (cond ((null x) (fullfail (car pl) al depth))
                 ((setq ol (unify y al x al)) (backup ol) (fullfail (car pl) al depth))
                 (t (fullresidueslast pl al depth cont))))
          (t (fullresiduesassumption `(not ,p) pl al depth cont)))))

(defun fullresiduesbasic (p pl al depth cont)
  (setq p (plugstdexp p al))
  (cond ((and (groundp p) (apply (get (car p) 'basic) (cdr p)))
         (fullresidueslast pl al depth cont))
        (t (let ((*residue* (cons p *residue*))) (fullresidueslast pl al depth cont)))))

(defun fullresiduesnotbasic (p pl al depth cont)
  (setq p (plug p al))
  (cond ((and (groundp p) (not (apply (get (car p) 'basic) (cdr p))))
         (fullresidueslast pl al depth cont))
        (t (let ((*residue* (cons `(not ,p) *residue*))) (fullresidueslast pl al depth cont)))))

(defun fullresiduespartial (p pl al depth cont)
  (setq p (plugstdexp p al))
  (cond ((find p *residue* :test #'equalp) (fullresidueslast pl al depth cont))
        ((or (not *consistency*)
             (not (or (rebuttalp p *residue*) (rebuttheoryp p *theory*)))
             (consistentp p *residue*))
         (let ((*residue* (cons p *residue*))) (fullresiduesexit pl al depth cont)))
        (t  (fullfail (car pl) al depth))))

; UPDATED
; changed so that after checking consistency, it checks whether
;   the answer we're about to add is subsumed by something in *answers*
(defun fullresiduesassumption (p pl al depth cont)
  (setq p (plugstdexp p al))
  (cond ((find p *residue* :test #'equalp) (fullresidueslast pl al depth cont))  ; already exists
	((or (not *consistency*)
	     (not (or (rebuttalp p *residue*) (rebuttheoryp p *theory*))) ; ensure negation of p's relation appears somewhere in residue/theory
	     (consistentp p *residue*))
         (cond ((and *check-answers* (residue-subsumedp p)) 
                ;(format t "residue ~A subsumed by ~A~%" (augment-residue) *answers*) 
                (fullfail (car pl) al depth))
               (t
		(let ((*residue* (cons p *residue*))) (fullresidueslast pl al depth cont)))))
	(t (fullfail (car pl) al depth))))

; UPDATED
; fails after reductions if goal
(defun fullresiduesrs (p pl al depth cont)
  (cond ((and *ancestry* (fullresiduesancestor p al cont)) (fullfail (car pl) al depth))
	((and (numberp *ancestry*) (fullresiduesnumber p al cont 0))
	 (fullresiduesassumption p pl al depth cont))
	((and *reduction* *ifreductionnoextension* (reducible p pl al depth cont)) 
	 (if (fullresiduesreduce p pl al depth cont) t (fullfail (car pl) al depth)))
        ((and *reduction* (fullresiduesreduce p pl al depth cont)))
        ((and *ignore-goal-rules* (equal *goal* (signed-relation p))) (fullfail (car pl) al depth))
	((fullresiduesdb p pl al depth cont *theory*))
	(t (fullfail (car pl) al depth))))

; UPDATED (NEW)
(defun reducible (p pl al depth cont)
  (declare (ignore pl depth))
  (do ((l cont (cdr l)))
      ((null l) nil)
      (when (unify (maknot (caaar l)) (cadar l) p al)
        (return t))))

; UPDATED (NEW)
(defun residue-subsumedp (p)
  "(RESIDUE-SUBSUMEDP P) checks whether adding P to the residue will produce a residue
   that is subsumed by something in *answers*."
  (cond ((null *answers*) nil)
        (t
         (let ((*residue* (cons p *residue*)))
           (let ((r (augment-residue)))
             (some #'(lambda (x) (similarp r x)) *answers*))))))

#|
; UPDATED (NEW)
(defun relation (rule)
  "(RELATION RULE) returns the relation for rule."
  (setq rule (head rule))
  (cond ((atom rule) rule)
        ((and (listp rule) (eq (car rule) 'not)) (relation (cadr rule)))
        (t (car rule))))

; UPDATED (COPIED FROM UTIL)
(defun signed-relation (rule)
  "(SIGNED-RELATION RULE) returns the positive or negative relation for rule."
  (setq rule (head rule))
  (if (and (listp rule) (eq (car rule) 'not))
    (cons 'not (list (relation rule)))
    (relation rule)))

; UPDATED (NEW)
(defun match (p q al)
  (setq *matches* (1+ *matches*))
  (matchpexp p q al))
|#

; UPDATED (NEW)
(defun simplify-residues (thing residues)
  "(SIMPLIFY-RESIDUES RESIDUES) uses subsumption to remove extraneous 
   residues from RESIDUES."
  (let (vs vsinv newresidues)
    (setq vs (freevars thing))
    (setq vs (mapcar #'(lambda (x) (cons x (gentemp))) vs))
    (setq vsinv (mapcar #'(lambda (x) (cons (cdr x) (car x))) vs))
    (setq newresidues (sublis vs residues))
    
  ;(setq residues (mapcar #'(lambda (x) (list* thing (mapcar #'maknot (if (and (listp x) (eq (car x) 'and)) (cdr x) x)))) 
  ;                       (remove-duplicates residues :test #'equal)))
    ;(print newresidues)
    (let ((removed nil))
      (do ((r1 newresidues (cdr r1)) (s12) (s21))
          ((null r1))
        (dolist (r2 (cdr r1))
          (setq s12 (similarp (car r1) r2))
          (setq s21 (similarp r2 (car r1)))
          (cond ((and s12 s21) (setq removed (cons r2 removed))) ;arbitrarily pick one to remove
                (s12 (setq removed (cons (car r1) removed)))
                (s21 (setq removed (cons r2 removed))))))
      (nsublis vsinv (set-difference newresidues removed :test #'equal)))))

(defun fullresiduesancestor (p al cont)
  (do ((l cont (cdr l)))
      ((null l) nil)
      (if (identify (caaar l) (cadar l) p al) (return t))))

(defun fullresiduesnumber (p al cont n)
  (let (ol)
    (cond ((numgeqp n *ancestry*))
          ((null cont) nil)
          ((atom p)
           (fullresiduesnumber p al (cdr cont) (if (eq p (caaar cont)) (1+ n) n)))
          ((setq ol (unify p al (caaar cont) (cadar cont)))
           (prog1 (fullresiduesnumber p al (cdr cont) (1+ n)) (backup ol)))
          ;((eq (operator p) (operator (caaar cont)))
          ; (fullresiduesnumber p al (cdr cont) (1+ n)))
          (t (fullresiduesnumber p al (cdr cont) n)))))

(defun fullresiduesreduce (p pl al depth cont)
  (do ((l cont (cdr l)) (ol))
      ((null l))
      (when (setq ol (unify (maknot (caaar l)) (cadar l) p al))
        (fullresiduesexit pl al depth cont)
        (backup ol))))

(defun fullresiduesdb (p pl al depth cont th)
  (fullresiduesth p pl al depth cont th)
  (do ((l (includees th) (cdr l)))
      ((null l) nil)
      (fullresiduesdb p pl al depth cont (car l))))

; UPDATED
; added check for residue-subsumed  -- for now, commented out, but left in case
;    we find a situation where we can prune a large part of the search space because
;    certain rules bind the variables in *residue* so that the result is subsumed
;    by one of *answers*.  For now, it looks like the only pruning that really occurs is
;    when we assume something.
(defun fullresiduesth (p pl al depth cont th)
  (do ((l (envindexps p al th) (cdr l)) (bl (environment)) (ol))
      ((null l))
    (cond ((and (listp (car l)) (eq '<= (caar l)) (null (cddar l)))
	   (when (setq ol (unify (cadar l) bl p al))
	     (fullresiduesexit pl al depth cont)
	     (cond ((subolp ol (alist bl)) (backup ol) (return nil))
		   (t (backup ol)))))
	  ((and (listp (car l)) (eq '<= (caar l)))
	   (cond  (nil ;(and *check-subsumption* (residue-subsumedp (augment-residue))) 
		   (format t "residue ~A subsumed by ~A~%" (augment-residue) *answers*) 
		   (backup ol))
		  (t 
		   (when (setq ol (unify (cadar l) bl p al))
		     (fullresiduesdepth (caddar l) (cddar l) bl
					(1+ depth) (cons (list pl al depth) cont))
		     (backup ol)))))
	  ((setq ol (unify (car l) bl p al))
	   (fullresiduesexit pl al depth cont)
	   (cond ((subolp ol (alist bl)) (backup ol) (return nil))
		 (t (backup ol)))))))

; UPDATED
(defun fullresiduesexit (pl al depth cont)
  (let (dum ans)
    (fullexit (car pl) al depth)
    (cond ((cdr pl) (fullresiduesdepth (cadr pl) (cdr pl) al depth cont))
          (cont (fullresiduesexit (caar cont) (cadar cont) (caddar cont) (cdr cont)))
          (t (setq ans (plugstdexp (reverse *residue*) alist))
             (dolist (var *variables*)
               (unless (eq (setq dum (plugstdexp var alist)) var)
                 (setq ans (cons `(= ,var ,dum) ans))))
             (setq *answers* (adjoin (maksand ans) *answers* :test #'equalp))))
    (fullredo (car pl) al depth)))

#|
; changed residue so that it includes the plugged in goal instead of the sequence
;   of equalities.
(defun fullresiduesexit (pl al depth cont)
Ê (let (ans)
Ê Ê (fullexit (car pl) al depth)
Ê Ê (cond ((cdr pl) (fullresiduesdepth (cadr pl) (cdr pl) al depth cont))
Ê Ê Ê Ê Ê (cont (fullresiduesexit (caar cont) (cadar cont) (caddar cont) (cdr cont)))
Ê Ê Ê Ê Ê (t (setq ans (plugstdexp (reverse *residue*) alist))
             (setq ans (augment-residue))
             (setq ans (herbrand-simplify ans))
ÊÊ Ê Ê Ê Ê Ê ;(print ans)
             (unless (eq ans 'false) (setq *answers* (cons ans *answers*)))
             ;(print *answers*)
             ))
Ê Ê (fullredo (car pl) al depth)))
|#

(defun augment-residue ()
  "(AUGMENT-RESIDUE) takes the current residue and augments it with the plugged in goal.
    Returns after making the result a conjunction. "
  (let ((r (plugstdexp *residue* alist)) (dum) (bl))

    ; grab the binding list of variables -- we know all var/var mappings
    (dolist (var *variables*)
      (unless (eq (setq dum (plugstdexp var alist)) var)
        (setq bl (cons (cons var dum) bl))))

    ; rewrite the residue in terms of the original variables
    ;(print bl)
    (maksand (cons (sublis bl *thing*) r))))


(defun fullresidueslast (pl al depth cont)
  (let (dum ans)
    (fullexit (car pl) al depth)
    (cond ((cdr pl) (fullresiduesdepth (cadr pl) (cdr pl) al depth cont))
          (cont (fullresiduesexit (caar cont) (cadar cont) (caddar cont) (cdr cont)))
          (t (setq ans (plugstdexp (reverse *residue*) alist))
             (dolist (var *variables*)
               (unless (eq (setq dum (plugstdexp var alist)) var)
                 (setq ans (cons `(same ,var ,dum) ans))))
             (setq *answers* (cons (maksand ans) *answers*))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; *gamma*
;;; *delta*
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defparameter *gamma* (make-instance 'theory))

(defmethod envindexps (p al (th (eql *gamma*)))
  (cond ((varp p) (contents th))
        ((atom p) (indexees p th))
        ((find (car p) '(believes knows exportable importable))
         (envindexps (caddr p) al th))
        (t (do ((l (cdr p) (cdr l)) (dum))
               ((null l) (indexps (car p) th))
               (setq dum (unival (car l) al))
               (cond ((varp dum))
                     ((atom dum) (return (indexees dum th))))))))

(defmethod indexps (p (th (eql *gamma*)))
  (cond ((varp p) (contents th))
        ((atom p) (indexees p th))
        ((find (car p) '(believes knows exportable importable)) (indexps (caddr p) th))
        (t (do ((l (cdr p) (cdr l)))
               ((null l) (indexps (car p) th))
               (cond ((varp (car l)))
                     ((atom (car l)) (return (indexees (car l) th))))))))


(defparameter *delta* (make-instance 'theory))

(defmethod indexps (p (th (eql *delta*)))
  (cond ((varp p) (contents th))
        ((atom p) (indexees p th))
        ((find (car p) '(believes knows exportable importable)) (indexps (caddr p) th))
        (t (do ((l (cdr p) (cdr l)))
               ((null l) (indexps (car p) th))
               (cond ((varp (car l)))
                     ((atom (car l)) (return (indexees (car l) th))))))))

(defmethod indexps (p (th (eql *delta*)))
  (cond ((varp p) (contents th))
        ((atom p) (indexees p th))
        ((find (car p) '(pos neg plus minus)) (indexps (cadr p) th))
        (t (do ((l (cdr p) (cdr l)))
               ((null l) (indexps (car p) th))
               (cond ((varp (car l)))
                     ((atom (car l)) (return (indexees (car l) th))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; fullupdate
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun fullupdate (p theory &optional (obssession  #'failure) (specialty #'failure))
  (fullretract (maknot p) theory obssession specialty)
  (fullassert p theory obssession specialty)
  'done)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; fullassert
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defparameter *specialty* #'failure)
(defparameter *obsession* #'failure)

(defun fullasserts (facts th &optional (obsession  #'failure) (specialty #'failure))
  (dolist (p facts) (fullassert p th obsession specialty)))

(defun fullassert (p *theory* &optional (*obsession*  #'failure) (*specialty* #'failure))
  (let (alist level tracecalls)
    (setq *termination* nil)
    (setq *unifications* 0)
    (setq alist (environment))
    (setq level 0)
    (fullassertdepth p alist 1)
    'done))

(defun fullassertdepth (p al depth)
  (cond ((numgreaterp depth *depth*) (setq *termination* t) nil)
        (t (fullsave p al depth)
           (fullassertexp p al depth)
           (fulldone p al depth))))

(defun fullassertexp (p al depth)
  (cond ((atom p) (fullassertexpexit p al depth))
        ((eq 'and (car p))
         (mapc #'(lambda (x) (fullassertdepth x al depth)) (cdr p)))
        (t (fullassertexpexit p al depth))))

(defun fullassertexpexit (p al depth)
  (setq p (plugstdexp p al))
  (cond ((knownp p *theory* #'samep) nil)
        ((atom p) (fullassertrs p al depth))
        ((eq 'execute (car p)) (ignore-errors (eval (cadr p))))
        ((eq 'evaluate (car p)) (ignore-errors (apply (caadr p) (cdadr p))))
        (t (fullassertrs p al depth))))

(defun fullassertrs (p al depth)
  (cond ((funcall *obsession* (operator p)) (insert p *theory*))
        (t (when (funcall *specialty* (operator p)) (insert p *theory*))
           (fullassertdb p al depth *theory*))))

(defun fullassertdb (p al depth th)
  (cond ((fullassertth p al depth th))
        (t (do ((l (includees th) (cdr l)))
               ((null l) nil)
               (fullassertdb p al depth (car l))))))

(defun fullassertth (p al depth th)
  (do ((l (envindexps p al th) (cdr l)) (ol) (bl (environment)) (*thing*) (*answers*) (dum))
      ((null l))
      (when (and (listp (car l)) (eq (caar l) '<=))
        (do ((m (cddar l) (cdr m)) (om))
            ((null m))
            (when (setq ol (unify (car m) bl p al))
              (if tracefacts (tracefact (car l)))
              (setq dum (maksand (revappend om (cdr m))))
              (setq *thing* (cadar l) *answers* nil)
              (let ((alist bl) (*filter* *specialty*))
                (fullall dum (list dum) bl (1+ depth) nil))
              (dolist (q *answers*) (fullassertdepth q alist (1+ depth)))
              (backup ol))
            (setq om (cons (car m) om))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; fullretract
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defparameter *conclusions* nil)

(defun fullretracts (facts th &optional (obsession  #'failure) (specialty #'failure))
  (dolist (p facts) (fullretract p th obsession specialty)))

(defun fullretract (p *theory* &optional (*obsession*  #'failure) (*specialty* #'failure))
  (let (conclusions)
    (setq conclusions (fullconclusions (list p) *theory* *obsession* *specialty*))
    (when (funcall *specialty* (operator p)) (drop p *theory*))
    (dolist (q conclusions)
      (unless (fullprovablep q *theory*) (drop q *theory*)))
    'done))

(defun fullprovablep (p *theory*)
  (let (alist)
    (setq alist (environment))
    (fulloneprovabledb p (list p) alist 1 nil *theory*)))

(defun fulloneprovabledb (p pl al depth cont th)
  (cond ((fulloneprovableth p pl al depth cont th))
        (t (do ((l (includees th) (cdr l)))
               ((null l))
               (when (fulloneprovableth p pl al depth cont (car l)) (return t))))))

(defun fulloneprovableth (p pl al depth cont th)
  (do ((l (envindexps p al th) (cdr l)) (bl (environment)) (ol))
      ((null l))
      (cond ((and (listp (car l)) (eq '<= (caar l)) (null (cddar l)))
             (cond ((not (setq ol (unify (cadar l) bl p al))))
                   ((fulloneexit pl al depth cont) (backup ol) (return t))
                   ((subolp ol (alist bl)) (backup ol) (return nil))
                   (t (backup ol))))
            ((and (listp (car l)) (eq '<= (caar l)))
             (cond ((not (setq ol (unify (cadar l) bl p al))))
                   ((fullone (caddar l) (cddar l) bl
                             (1+ depth) (cons (list pl al depth) cont))
                    (backup ol) (return  t))
                   (t (backup ol)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; fullassertions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun fullassertions (facts *theory* &optional (*obsession*  #'failure) (*specialty* #'failure))
  (decludes *gamma*)
  (includes *gamma* *theory*)
  (empty *gamma*)
  (fullasserts facts *gamma* *obsession* *specialty*)
  (contents *gamma*))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; fullconclusions - version without indexing
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun fullconclusions (facts *theory* &optional (*obsession*  #'failure) (*specialty* #'failure))
  (let (*conclusions* alist (level 0) tracecalls)
    (setq *termination* nil)
    (setq *unifications* 0)
    (setq alist (environment))
    (dolist (p facts) (fullconclusionsdepth p 0))
    (nreverse *conclusions*)))

(defun fullconclusionsdepth (p depth)
  (cond ((numgreaterp depth *depth*) (setq *termination* t) nil)
        (t (fulldrop p alist depth)
           (fullconclusionsexp p depth)
           (fulldone p alist depth))))

(defun fullconclusionsexp (p depth)
  (cond ((atom p) (fullconclusionstest p depth))
        ((eq 'and (car p))
         (mapc #'(lambda (x) (fullconclusionsdepth x depth)) (cdr p)))
        (t (fullconclusionstest p depth))))

(defun fullconclusionstest (p depth)
  (cond ((find p *conclusions* :test #'equalp))
        (t (when (funcall *specialty* (operator p))
             (setq *conclusions* (cons p *conclusions*)))
           (fullconclusionsdb p depth *theory*))))

(defun fullconclusionsdb (p depth th)
  (fullconclusionsth p depth th)
  (dolist (th (includees th)) (fullconclusionsdb p depth th) nl))

(defun fullconclusionsth (p depth th)
  (do ((l (indexps p th) (cdr l)) (ol) (bl (environment)) (*thing*) (*answers*) (dum))
      ((null l))
      (when (and (listp (car l)) (eq (caar l) '<=))
        (do ((m (cddar l) (cdr m)) (om))
            ((null m))
            (when (setq ol (unify (car m) bl p alist))
              (if tracefacts (tracefact (car l)))
              (setq dum (maksand (revappend om (cdr m))))
              (setq *thing* (cadar l) *answers* nil)
              (let ((alist bl) (*filter* *specialty*))
                (fullall dum (list dum) bl (1+ depth) nil))
              (dolist (q *answers*) (fullconclusionsdepth q (1+ depth)))
              (backup ol))
            (setq om (cons (car m) om))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; test
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
#|
(defparameter *foo* (make-instance 'fullserver :name 'foo))

(define-theory *foo* ""
  '((p a) (q a) (r a) (p b) (r b)
    (s a b) (s a c) (t a) (s d e) (t d)
    (u a b) (v b c) (w a c)
    (i a) (m a)
    (<= (r ?x) (p ?x))
    (<= (r ?x) (q ?x))
    (<= (t ?x) (s ?x ?y))
    (<= (w ?x ?z) (u ?x ?y) (v ?y ?z))
    (<= (j ?x) (i ?x))
    (<= (k ?x) (i ?x))
    (<= (m ?x) (j ?x) (k ?x))))

(fullassertions '((p c)) *foo* #'failure #'success)
((p c) (r c))

(fullassertions '((s b b)) *foo* #'failure #'success)
((s b b) (t b))

(fullassertions '((u b b)) *foo* #'failure #'success)
((u b b) (w b c))

(fullassertions '((i b)) *foo* #'failure #'(lambda (x) (find x '(i m))))
((i b) (m b))

(fullretractions '((p a)) *foo* #'failure #'success)
((p a))

(fullretractions '((p b)) *foo* #'failure #'success)
((p b) (r b))

(fullretractions '((s a b)) *foo* #'failure #'success)
((s a b))

(fullretractions '((s d e)) *foo* #'failure #'success)
((s d e) (t d))

(fullretractions '((i a)) *foo* #'failure #'(lambda (x) (find x '(i m))))
((i a) (m a))
|#

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
