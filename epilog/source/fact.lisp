;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; (c) Copyright 1999-2005 by Michael Genesereth.  All rights reserved.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; fact.lisp
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(eval-when (compile load eval)
  (proclaim '(special *thing* *answers* *theory*)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; factfindp
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun factfindp (p th)
  (factfindx t p th))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; factfindx
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun factfindx (*thing* p *theory*)
  (let ((al))
    (setq p (order *thing* p))
    (when (setq al (factone (adjust *thing* p) truth nil))
      (plug *thing* al))))

(defun factone (p al pl)
  (cond ((atom p) (factoneconstant p al pl))
        ((eq (car p) 'not) (factoneunprovable p al pl))
        ((eq (car p) 'and) (factoneand p al pl))
        ((eq (car p) 'or) (factoneor p al pl))
        ((eq (car p) 'same) (factonesame p al pl))
        ((eq (car p) 'distinct) (factonedistinct p al pl))
        ((eq (car p) 'oneof) (factoneoneof p al pl))
        ((eq (car p) 'choose) (factonechoose p al pl))
        ((eq (car p) 'bagofall) (factonebagofall p al pl))
        ((eq (car p) 'unprovable) (factoneunprovable p al pl))
	((eq (car p) 'ground) (factoneground p al pl))
	((eq (car p) 'nonground) (factonenonground p al pl))
        ((eq (car p) 'execute) (factoneexecute p al pl))
        ((eq (car p) 'evaluate) (factoneevaluate p al pl))
        ((eq (car p) 'stringmatch) (factonestrmatch p al pl))
	((get (car p) 'basicval) (factonebasicvalue p al pl))
        ((get (car p) 'basic) (factonebasic p al pl))
        (t (factoners p al pl))))

(defun factoneconstant (p al pl)
  (cond ((eq p 'true) (factdone al pl))
        ((eq p 'false) nil)
        (t (factoners p al pl))))

(defun factoneand (p al pl)
  (cond ((null (cdr p)) (factdone al pl))
        (t (factone (cadr p) al (append (cddr p) pl)))))

(defun factoneor (p al pl)
  (do ((l (cdr p) (cdr l)) (bl))
      ((null l) nil)
      (if (setq bl (factone (car l) al pl)) (return bl))))

(defun factonesame (p al pl)
  (if (setq al (mguexp (cadr p) (caddr p) al)) (factdone al pl)))

(defun factonedistinct (p al pl)
  (if (mguexp (cadr p) (caddr p) al) nil (factdone al pl)))

(defun factoneoneof (p al pl)
  (when (seqvarp (caddr p)) (setq p (plug p al)))
  (do ((l (cddr p) (cdr l)) (bl))
      ((null l) nil)
      (if (and (setq bl (mguexp (cadr p) (car l) al))
               (setq bl (factdone bl pl)))
          (return bl))))

(defun factonechoose (p al pl)
  (let (x)
    (setq p (plug p al))
    (setq x (factfindx (cadr p) (caddr p) *theory*))
    (when (and (not (null x)) (setq al (mguexp (cadddr p) x al)))
      (factdone al pl))))

(defun factonebagofall (p al pl)
  (let (answer)
    (setq p (plug p al))
    (setq answer (cons 'listof (factfinds (cadr p) (caddr p) *theory*)))
    (when (setq al (mguexp answer (cadddr p) al)) (finddone al pl))))

(defun factoneunprovable (p al pl)
  (setq p (deskolemize p))
  (cond ((factone (cadr p) al pl) nil)
        (t (factdone al pl))))

(defun factoneground (p al pl)
  (setq p (plug p al))
  (if (groundp p) (factdone al pl)))

(defun factonenonground (p al pl)
  (setq p (plug p al))
  (if (groundp p) nil (factdone al pl)))

(defun factoneexecute (p al pl)
  (let (values)
    (setq p (plug p al))
    (cond ((null (cddr p)) (if (ignore-errors (eval (cadr p))) (factdone al pl)))
          ((and (car (setq values (ignore-errors (multiple-value-list (eval (cadr p))))))
                (setq al (matchpexp (cddr p) (mapcar #'quotify values) al)))
           (factdone al pl)))))

(defun factoneevaluate (p al pl)
  (let (values)
    (setq p (plug p al))
    (cond ((null (cddr p)) (if (ignore-errors (apply (caadr p) (cdadr p))) (factdone al pl)))
          ((and (car (setq values (ignore-errors (multiple-value-list (apply (caadr p) (cdadr p))))))
                (setq al (matchpexp (cddr p) values al)))
           (factdone al pl)))))

(defun factonestrmatch (p al pl)
  (factone `(execute (strmatches ,(cadr p) ,(caddr p)) ? . ,(cdddr p)) al pl))

(defun factonebasicvalue (p al pl)
  (let (x y)
    (setq p (plug p al) x (butlast p) y (car (last p)))
    (if (and (groundp x) (every #'primitivep (cdr x))
             (setq x (funcall (get (car x) 'basicval) x))
             (setq al (mguexp x y al)))
        (factdone al pl))))

(defun factonebasic (p al pl)
  (setq p (plug p al))
  (if (and (groundp p) (apply (get (car p) 'basic) (cdr p))) (factdone al pl)))

(defun factoners (p al pl)
  (factonedb p al pl *theory*))

(defun factonedb (p al pl th)
  (cond ((factoneth p al pl th))
        (t (do ((l (includees th) (cdr l)) (bl))
               ((null l))
               (if (setq bl (factonedb p al pl (car l))) (return bl))))))

(defun factoneth (p al pl th)
  (do ((l (envindexps p al th) (cdr l)) (bl truth))
      ((null l) nil)
      (when (and (setq bl (matchpexp p (car l) al)) (setq bl (factdone bl pl)))
        (return bl))))

(defun factdone (al pl)
  (cond ((null pl) al)
        (t (factone (car pl) al (cdr pl)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; factfinds
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun factfinds (*thing* p *theory*)
  (let (*answers*)
    (setq p (order *thing* p))
    (factall (adjust *thing* p) truth nil) 
    (nreverse (uniquify *answers*))))

(defun factall (p al pl)
  (cond ((atom p) (factallconstant p al pl))
        ((eq (car p) 'not) (factallunprovable p al pl))
        ((eq (car p) 'and) (factalland p al pl))
        ((eq (car p) 'or) (factallor p al pl))
        ((eq (car p) 'same) (factallsame p al pl))
        ((eq (car p) 'distinct) (factalldistinct p al pl))
        ((eq (car p) 'oneof) (factalloneof p al pl))
        ((eq (car p) 'choose) (factallbagofall p al pl))
        ((eq (car p) 'bagofall) (factallbagofall p al pl))
        ((eq (car p) 'unprovable) (factallunprovable p al pl))
	((eq (car p) 'ground) (factallground p al pl))
	((eq (car p) 'nonground) (factallnonground p al pl))
        ((eq (car p) 'execute) (factallexecute p al pl))
        ((eq (car p) 'evaluate) (factallevaluate p al pl))
        ((eq (car p) 'stringmatch) (factallstrmatch p al pl))
	((get (car p) 'basicval) (factallbasicvalue p al pl))
        ((get (car p) 'basic) (factallbasic p al pl))
        (t (factallrs p al pl))))

(defun factallconstant (p al pl)
  (cond ((eq p 'true) (factexit al pl))
        ((eq p 'false) nil)
        (t (factallrs p al pl))))

(defun factalland (p al pl)
  (cond ((null (cdr p)) (factexit al pl))
        (t (factall (cadr p) al (append (cddr p) pl)))))

(defun factallor (p al pl)
  (do ((l (cdr p) (cdr l)))
      ((null l))
      (factall (car l) al pl)))

(defun factallsame (p al pl)
  (if (setq al (mguexp (cadr p) (caddr p) al)) (factexit al pl)))

(defun factalldistinct (p al pl)
  (if (mguexp (cadr p) (caddr p) al) nil (factexit al pl)))

(defun factalloneof (p al pl)
  (when (seqvarp (caddr p)) (setq p (plug p al)))
  (do ((l (cddr p) (cdr l)) (bl))
      ((null l) t)
      (if (setq bl (mguexp (cadr p) (car l) al))
          (factexit bl pl))))

(defun factallchoose (p al pl)
  (let (x)
    (setq p (plug p al))
    (setq x (factfindx (cadr p) (caddr p) *theory*))
    (when (and (not (null x)) (setq al (mguexp (cadddr p) x al)))
      (factexit al pl))))

(defun factallbagofall (p al pl)
  (let (answer)
    (setq p (plug p al))
    (setq answer (cons 'listof (factfinds (cadr p) (caddr p) *theory*)))
    (when (setq al (mguexp answer (cadddr p) al)) (factexit al pl))))

(defun factallunprovable (p al pl)
  (setq p (deskolemize p))
  (cond ((factone (cadr p) al pl) nil)
        (t (factexit al pl))))

(defun factallground (p al pl)
  (setq p (plug p al))
  (if (groundp p) (factexit al pl)))

(defun factallnonground (p al pl)
  (setq p (plug p al))
  (if (groundp p) nil (factexit al pl)))

(defun factallexecute (p al pl)
  (let (values)
    (setq p (plug p al))
    (cond ((null (cddr p))
           (if (ignore-errors (eval (cadr p))) (factexit al pl)))
          ((and (car (setq values (ignore-errors (multiple-value-list (eval (cadr p))))))
                (setq al (matchpexp (cddr p) (mapcar #'quotify values) al)))
           (factexit al pl)))))

(defun factallevaluate (p al pl)
  (let (values)
    (setq p (plug p al))
    (cond ((null (cddr p))
           (if (ignore-errors (apply (caadr p) (cdadr p))) (factexit al pl)))
          ((and (car (setq values (evals (cadr p))))
                (setq al (matchpexp (cddr p) values al)))
           (factexit al pl)))))
    
(defun factallstrmatch (p al pl)
  (factall `(execute (strmatches ,(cadr p) ,(caddr p)) ? . ,(cdddr p)) al pl))

(defun factallbasicvalue (p al pl)
  (let (x y)
    (setq p (plug p al) x (butlast p) y (car (last p)))
    (if (and (groundp x) (every #'primitivep (cdr x))
             (setq x (funcall (get (car x) 'basicval) x))
             (setq al (mguexp x y al)))
        (factexit al pl))))

(defun factallbasic (p al pl)
  (setq p (plug p al))
  (if (and (groundp p) (apply (get (car p) 'basic) (cdr p))) (factexit al pl)))

(defun factallrs (p al pl)
  (factalldb p al pl *theory*))

(defun factalldb (p al pl th)
  (factallth p al pl th)
  (do ((l (includees th) (cdr l)))
      ((null l))
      (factalldb p al pl (car l))))

(defun factallth (p al pl th)
  (do ((l (envindexps p al th) (cdr l)) (bl truth))
      ((null l) nil)
      (when (setq bl (matchpexp p (car l) al)) (factexit bl pl))))

(defun factexit (al pl)
  (cond ((null pl) (setq *answers* (cons (plug *thing* al) *answers*)))
        (t (factall (car pl) al (cdr pl)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;