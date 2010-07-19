;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; cachemore
;;; cacheassume
;;; cacheprovep
;;; cacheprovex
;;; cacheproves
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(eval-when (compile load eval) (proclaim '(special traceexpressions)))

(defparameter *limit* 1000000000)
(defparameter *answer* nil)
(defparameter *answers* nil)

(defmacro defcachetheory (x &rest l)
  `(define-cachetheory ',x ',l))

(defun define-cachetheory (th facts)
  (empty th)
  (dolist (p facts) (cacheassume p th))
  th)

(defun cacheassume (p th)
  (dolist (x (contrapositives p)) (save x th)))

(defun cacheprovep (p th)
  (cacheprovex 't p th))

(defun cacheprovex (*thing* p *theory*)
  (let (alist *answer* (*ancestry* t))
    (setq *unifications* 0)
    (setq *inferences* 0)
    (setq *termination* nil)
    (setq p (adjust  *thing* (order *thing* p)))
    (setq alist (environment))
    (decludes 'epitheory)
    (empty 'epitheory)
    (includes 'epitheory *theory*)
    (setq *theory* 'epitheory)
    (dolist (x (contrapositives `(<= (answer ,*thing*) ,p))) (save x 'epitheory))
    (cacheone `(answer ,*thing*) nil alist 1 (list (list (list `(answer ,*thing*)) alist 1)))
    *answer*))

(defun cacheproves (*thing* p *theory*)
  (let (alist *answers* (*ancestry* t))
    (setq *unifications* 0)
    (setq *inferences* 0)
    (setq *termination* nil)
    (setq p (adjust  *thing* (order *thing* p)))
    (setq alist (environment))
    (decludes 'epitheory)
    (empty 'epitheory)
    (includes 'epitheory *theory*)
    (setq *theory* 'epitheory)
    (dolist (x (contrapositives `(<= (answer ,*thing*) ,p))) (save x 'epitheory))
    (cacheall `(answer ,*thing*) nil alist 1 (list (list (list `(answer ,*thing*)) alist 1)))
    (nreverse (uniquify *answers*))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; cachefindp
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun cachefindp (p th)
  (cachefindx 't p th))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; cachefindx
;;; need to add subolp test and check cacheprovex
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun cachefindx (*thing* p *theory*)
  (let (alist *answer*)
    (setq *cache* nil)
    (setq *directory* nil)
    (setq *unifications* 0)
    (setq *inferences* 0)
    (setq *termination* nil)
    (setq alist (environment))
    (cacheone p (list p) alist 0 nil)
    *answer*))

(defun cacheone (p pl al depth cont)
  (setq *inferences* (1+ *inferences*))
  (when traceexpressions (tracemessage depth '|Call: | (plugstdexp p al)))
  (cond ((>= *inferences* *limit*) (setq *termination* t) (cacheonefail pl al depth))
        ((> depth *depth*) (setq *termination* t) (cacheonefail pl al depth))
        (t (cacheoneexp p pl al depth cont))))

(defun cacheoneexp (p pl al depth cont)
  (cond ((atom p) (cacheoneconstant p pl al depth cont))
        ((eq 'not (car p)) (cacheonenot (cadr p) pl al depth cont))
        ((eq 'and (car p)) (cacheoneand p pl al depth cont))
        ((eq 'or (car p)) (cacheoneor p pl al depth cont))
        ((eq 'oneof (car p)) (cacheoneoneof p pl al depth cont))
        ((eq 'member (car p)) (cacheonemember p pl al depth cont))
        ((eq 'same (car p)) (cacheonesame p pl al depth cont))
        ((eq 'distinct (car p)) (cacheonedistinct p pl al depth cont))
	((eq 'ground (car p)) (cacheoneground p pl al depth cont))
	((eq 'nonground (car p)) (cacheonenonground p pl al depth cont))
	((eq 'primitive (car p)) (cacheoneprimitive p pl al depth cont))
	((eq 'nonprimitive (car p)) (cacheonenonprimitive p pl al depth cont))
	((eq '== (car p)) (cacheonevalue p pl al depth cont))
	((eq 'value (car p)) (cacheonevalue p pl al depth cont))
        ((eq 'execute (car p)) (cacheoneexecute p pl al depth cont))
        ((eq 'evaluate (car p)) (cacheoneevaluate p pl al depth cont))
        ((eq 'unprovable (car p)) (cacheoneunprovable p pl al depth cont))
        ((eq 'bagofall (car p)) (cacheonebagofall p pl al depth cont))
        ((eq 'strmatch (car p)) (cacheonestrmatch p pl al depth cont))
	((get (car p) 'basicval) (cacheonebasicvalue p pl al depth cont))
        ((get (car p) 'basic) (cacheonebasic p pl al depth cont))
        ((cachep p al depth) (cacheonecache p pl al depth cont))
        (t (cacheoners p pl al depth cont))))

(defun cachep (p al depth)
  (do ((l *directory* (cdr l)) (ol))
      ((null l) nil)
      (when (and (setq ol (matchify (caar l) alist p al)) (<= (- *depth* depth) (cadar l)))
        (backup ol)
        (return t))))

(defun cacheonecache (p pl al depth cont)
  (do ((l *cache* (cdr l)) (bl (environment)) (ol))
      ((null l))
      (when (setq ol (unify (car l) bl p al))
        (cond ((cacheoneexit pl al depth cont)
               (backup ol)
               (return t))
              (t (backup ol))))))

(defun cacheonenot (p pl al depth cont)
  (cond ((atom p) (cacheonenotconstant p pl al depth cont))
        ((eq 'not (car p)) (cacheoneexp (cadr p) pl al depth cont))
        ((eq 'and (car p)) (cacheonenotand p pl al depth cont))
        ((eq 'or (car p)) (cacheonenotor p pl al depth cont))
        ((eq 'oneof (car p)) (cacheonefail pl al depth))
        ((eq 'same (car p)) (cacheonefail pl al depth))
        ((eq 'distinct (car p)) (cacheonefail pl al depth))
	((eq 'ground (car p)) (cacheonefail pl al depth))
	((eq 'nonground (car p)) (cacheonefail pl al depth))
	((eq 'primitive (car p)) (cacheonefail pl al depth))
	((eq 'nonprimitive (car p)) (cacheonefail pl al depth))
	((eq '== (car p)) (cacheonenotvalue p pl al depth cont))
	((eq 'value (car p)) (cacheonenotvalue p pl al depth cont))
        ((eq 'execute (car p)) (cacheonenotexecute p pl al depth cont))
        ((eq 'evaluate (car p)) (cacheonenotevaluate p pl al depth cont))
        ((eq 'unprovable (car p)) (cacheoneexp (cadr p) pl al depth cont))
        ((eq 'bagofall (car p)) (cacheonenotbagofall p pl al depth cont))
        ((eq 'strmatch (car p)) (cacheonenotstrmatch p pl al depth cont))
	((get (car p) 'basicval) (cacheonenotbasicvalue p pl al depth cont))
        ((get (car p) 'basic) (cacheonenotbasic p pl al depth cont))
        (t (cacheoners `(not ,p) pl al depth cont))))

(defun cacheoneconstant (p pl al depth cont)
  (cond ((eq 'true p) (cacheoneexit pl al depth cont))
        ((eq 'false p) (cacheonefail pl al depth))
        (t (cacheoners p pl al depth cont))))

(defun cacheonenotconstant (p pl al depth cont)
  (cond ((eq 'true p) (cacheonefail pl al depth))
        ((eq 'false p) (cacheoneexit pl al depth cont))
        (t (cacheoners `(not ,p) pl al depth cont))))



(defun cacheoneand (p pl al depth cont)
  (cond ((null (cdr p)) (cacheoneexit pl al depth cont))
        ((cacheone (cadr p) (cdr p) al depth (cons (list pl al depth) cont)))
        (t (cacheonefail pl al depth))))

(defun cacheonenotand (p pl al depth cont)
  (setq cont (cons (list pl al depth) cont))
  (do ((l (cdr p) (cdr l)))
      ((null l) (cacheonefail pl al depth))
      (when (cacheone (maknot (car l)) (list (maknot (car l))) al depth cont)
        (return t))))

(defun cacheoneor (p pl al depth cont)
  (setq cont (cons (list pl al depth) cont))
  (do ((l (cdr p) (cdr l)))
      ((null l) (cacheonefail pl al depth))
      (when (cacheone (car l) (list (car l)) al depth cont)
        (return t))))

(defun cacheonenotor (p pl al depth cont)
  (cond ((null (cdr p)) (cacheoneexit pl al depth cont))
        ((cacheone (maknot (cadr p)) (mapcar #'maknot (cdr p)) al depth
                  (cons (list pl al depth) cont)))
        (t (cacheonefail pl al depth))))



(defun cacheoneoneof (p pl al depth cont)
  (when (seqvarp (caddr p)) (setq p (plug p al)))
  (do ((l (cddr p) (cdr l)) (ol))
      ((null l) (cacheonefail pl al depth))
      (cond ((not (setq ol (unify (cadr p) al (car l) al))))
            ((cacheoneexit pl al depth cont) (backup ol) (return t))
            (t (backup ol)))))

(defun cacheonemember (p pl al depth cont)
  (do ((l (cdr (plug (caddr p) al)) (cdr l)) (bl))
      ((null l) (cacheonefail pl al depth))
      (if (setq bl (mgwexp (cadr p) (car l) al))
          (cacheoneexit pl bl depth cont))))



(defun cacheonesame (p pl al depth cont)
  (let (ol)
    (cond ((setq ol (unify (cadr p) al (caddr p) al))
           (cond ((cacheoneexit pl al depth cont) (backup ol) t)
                 (t (backup ol) nil)))
          (t (cacheonefail pl al depth)))))

(defun cacheonedistinct (p pl al depth cont)
  (let ((ol))
    (cond ((setq ol (unify (cadr p) al (caddr p) al))
           (backup ol) (cacheonefail pl al depth))
          (t (cacheoneexit pl al depth cont)))))



(defun cacheoneground (p pl al depth cont)
  (setq p (plug p al))
  (cond ((groundp p) (cacheoneexit pl al depth cont))
        (t (cacheonefail pl al depth))))

(defun cacheonenonground (p pl al depth cont)
  (setq p (plug p al))
  (cond ((groundp p) (cacheonefail pl al depth))
        (t (cacheoneexit pl al depth cont))))

(defun cacheoneprimitive (p pl al depth cont)
  (setq p (plug p al))
  (cond ((primitivep p) (cacheoneexit pl al depth cont))
        (t (cacheonefail pl al depth))))

(defun cacheonenonprimitive (p pl al depth cont)
  (setq p (plug p al))
  (cond ((primitivep p) (cacheonefail pl al depth))
        (t (cacheoneexit pl al depth cont))))



(defun cacheonevalue (p pl al depth cont)
  (let (x y)
    (setq p (plug p al) x (cadr p) y (caddr p))
    (cond ((atom x) (when (setq al (mgwexp x y al)) (cacheoneexit pl al depth cont)))
          ((eq 'bagofall (car x))
           (setq x (cons 'listof (cachefinds (cadr x) (caddr x) *theory*)))
           (when (setq al (mgwexp y x al)) (cacheoneexit pl al depth cont)))
          ((and (groundp x) (every #'primitivep (cdr x)))
           (setq x (funcall (get (car x) 'basicval) x))
           (when (setq al (mgwexp x y al)) (cacheoneexit pl al depth cont))))))

(defun cacheonenotvalue (p pl al depth cont)
  (let (x y)
    (setq p (plug p al) x (cadr p) y (caddr p))
    (cond ((atom x) (unless (mgwexp x y al) (cacheoneexit pl al depth cont)))
          ((eq 'bagofall (car x))
           (setq x (cons 'listof (cachefinds (cadr x) (caddr x) *theory*)))
           (unless (mgwexp y x al)) (cacheoneexit pl al depth cont))
          ((and (groundp x) (every #'primitivep (cdr x)))
           (setq x (funcall (get (car x) 'basicval) x))
           (unless (mgwexp x y al) (cacheoneexit pl al depth cont))))))

(defun cacheoneexecute (p pl al depth cont)
  (let (values)
    (setq p (plug p al))
    (cond ((null (cddr p))
           (if (ignore-errors (eval (cadr p))) (cacheoneexit pl al depth cont)))
          ((and (car (setq values (ignore-errors (multiple-value-list (eval (cadr p))))))
                (setq al (matchpexp (cddr p) (mapcar #'quotify values) al)))
           (cacheoneexit pl al depth cont)))))
    
(defun cacheonenotexecute (p pl al depth cont)
  (let (values)
    (setq p (plug p al))
    (cond ((null (cddr p))
           (if (ignore-errors (not (eval (cadr p)))) (cacheoneexit pl al depth cont)))
          ((and (car (setq values (ignore-errors (multiple-value-list (eval (cadr p))))))
                (not (matchpexp (cddr p) (mapcar #'quotify values) al)))
           (cacheoneexit pl al depth cont)))))

(defun cacheoneevaluate (p pl al depth cont)
  (let (values)
    (setq p (plug p al))
    (cond ((null (cddr p))
           (if (ignore-errors (apply (caadr p) (cdadr p))) (cacheoneexit pl al depth cont)))
          ((and (car (setq values (ignore-errors (multiple-value-list (apply (caadr p) (cdadr p))))))
                (setq al (matchpexp (cddr p) values al)))
           (cacheoneexit pl al depth cont)))))
    
(defun cacheonenotevaluate (p pl al depth cont)
  (let (values)
    (setq p (plug p al))
    (cond ((null (cddr p))
           (if (ignore-errors (not (apply (caadr p) (cdadr p)))) (cacheoneexit pl al depth cont)))
          ((and (car (setq values (ignore-errors (multiple-value-list (apply (caadr p) (cdadr p))))))
                (not (matchpexp (cddr p) values al)))
           (cacheoneexit pl al depth cont)))))


(defun cacheoneunprovable (p pl al depth cont)
  (cond ((cacheone (cadr p) pl al depth cont) nil)  ;; cachefindone when ready
        (t (cacheoneexit pl al depth cont))))

(defun cacheonebagofall (p pl al depth cont)
  (cacheonevalue `(value ,(butlast p) ,(cadddr p)) pl al depth cont))

(defun cacheonenotbagofall (p pl al depth cont)
  (cacheonenotvalue `(value ,(butlast p) ,(cadddr p)) pl al depth cont))


(defun cacheonestrmatch (p pl al depth cont)
  (cacheoneexp `(execute (strmatches ,(cadr p) ,(caddr p)) ? . ,(cdddr p)) pl al depth cont))

(defun cacheonenotstrmatch (p pl al depth cont)
  (cacheonenot `(execute (strmatches ,(cadr p) ,(caddr p)) ? . ,(cdddr p)) pl al depth cont))

(defun cacheonebasicvalue (p pl al depth cont)
  (let (x y)
    (setq p (plug p al) x (butlast p) y (car (last p)))
    (if (and (groundp x) (every #'primitivep (cdr x))
             (setq x (funcall (get (car x) 'basicval) x))
             (setq al (mgwexp x y al)))
        (cacheoneexit pl al depth cont))))

(defun cacheonenotbasicvalue (p pl al depth cont)
  (let (x y)
    (setq p (plug p al) x (butlast p) y (car (last p)))
    (if (and (groundp x) (every #'primitivep (cdr x))
             (setq x (funcall (get (car x) 'basicval) x))
             (not (mgwexp x y al)))
        (cacheoneexit pl al depth cont))))

(defun cacheonebasic (p pl al depth cont)
  (setq p (plug p al))
  (when (and (groundp p) (apply (get (car p) 'basic) (cdr p)))
    (cacheoneexit pl al depth cont)))

(defun cacheonenotbasic (p pl al depth cont)
  (setq p (plug p al))
  (if (and (groundp p) (not (apply (get (car p) 'basic) (cdr p))))
      (cacheoneexit pl al depth cont)))


(defun cacheoners (p pl al depth cont)
  (cond ((and *ancestry* (cacheoneancestor p al cont)) nil)
        ((and *reduction* (cacheonereduce p pl al depth cont)))
        (t (cacheonedb p pl al depth cont *theory*))))

(defun cacheoneancestor (p al cont)
  (do ((l cont (cdr l)))
      ((null l) nil)
      (if (identify (caaar l) (cadar l) p al) (return t))))

(defun cacheonereduce (p pl al depth cont)
  (do ((l cont (cdr l)) (ol))
      ((null l))
      (when (setq ol (unify (maknot (caaar l)) (cadar l) p al))
        (cond ((cacheoneexit pl al depth cont)
               (backup ol)
               (return t))
              (t (backup ol))))))

(defun cacheonedb (p pl al depth cont th)
  (cond ((cacheoneth p pl al depth cont th))
        (t (do ((l (includees th) (cdr l)))
               ((null l))
               (when (cacheonedb p pl al depth cont (car l)) (return t))))))

(defun cacheoneth (p pl al depth cont th)
  (do ((l (envindexps p al th) (cdr l)) (bl (environment)) (ol) (flag))
      ((null l) (if flag (cacheonefail pl al depth) (cacheoneplain pl al depth)))
      (cond ((and (listp (car l)) (eq '<= (caar l)))
             (cond ((null (setq ol (unify (cadar l) bl p al))))
                   ((and (setq flag t) nil))
                   ((cacheonegoals (cddar l) (cdr pl) bl (1+ depth)
                                  (cons (list pl al depth) cont))
                    (backup ol)
                    (return t))
                   (t (backup ol))))
            ((setq ol (unify (car l) bl p al))
             (cond ((cacheoneexitsingle pl al depth cont)
                    (backup ol)
                    (return t))
                   (t (backup ol)))))))

(defun cacheonegoals (gl pl al depth cont)
  (cond ((null gl) (cacheoneexit pl al depth cont))
        (t (cacheone (car gl) gl al depth cont))))

(defparameter *cache* nil)

(defparameter *directory* nil)

(defun cacheoneexitsingle (pl al depth cont)
  (when traceexpressions
    (tracemessage depth '|Exit: | (plugstdexp (car pl) al)))
  (cond ((cdr pl) (cacheone (cadr pl) (cdr pl) al depth cont))
        (cont (cacheoneexit (caar cont) (cadar cont) (caddar cont) (cdr cont)))
        (t (setq *answer* (plugstdexp *thing* alist)) t)))

(defun cacheoneexit (pl al depth cont)
  (when traceexpressions
    (tracemessage depth '|Exit: | (plugstdexp (car pl) al)))
  (setq *cache* (adjoin (plugstdexp (car pl) al) *cache* :test #'equalp))
  (cond ((cdr pl) (cacheone (cadr pl) (cdr pl) al depth cont))
        (cont (cacheoneexit (caar cont) (cadar cont) (caddar cont) (cdr cont)))
        (t (setq *answer* (plugstdexp *thing* alist)) t)))

(defun cacheonefail (pl al depth)
  (when traceexpressions
    (tracemessage depth '|Fail: | (plugstdexp (car pl) al)))
  (setq *directory* (adjoin (list (plugstdexp (car pl) al) (- *depth* depth))
                            *directory* :test #'equalp))
  nil)

(defun cacheoneplain (pl al depth)
  (when traceexpressions
    (tracemessage depth '|Fail: | (plugstdexp (car pl) al)))
  nil)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; cachefinds
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun cachefinds (*thing* p *theory*)
  (let (alist *answers*)
    (setq *unifications* 0)
    (setq *inferences* 0)
    (setq *termination* nil)
    (setq alist (environment))
    (cacheall p (list p) alist 0 nil)
    (nreverse (uniquify *answers*))))

(defun cacheall (p pl al depth cont)
  (setq *inferences* (1+ *inferences*))
  (when traceexpressions (tracemessage depth '|Call: | (plugstdexp p al)))
  (cond ((>= *inferences* *limit*) (setq *termination* t) (cacheallfail pl al depth))
        ((> depth *depth*) (setq *termination* t) (cacheallfail pl al depth))
        (t (cacheallexp p pl al depth cont))))

(defun cacheallexp (p pl al depth cont)
  (cond ((atom p) (cacheallconstant p pl al depth cont))
        ((eq 'not (car p)) (cacheallnot (cadr p) pl al depth cont))
        ((eq 'and (car p)) (cachealland p pl al depth cont))
        ((eq 'or (car p)) (cacheallor p pl al depth cont))
        ((eq 'oneof (car p)) (cachealloneof p pl al depth cont))
        ((eq 'member (car p)) (cacheallmember p pl al depth cont))
        ((eq 'same (car p)) (cacheallsame p pl al depth cont))
        ((eq 'distinct (car p)) (cachealldistinct p pl al depth cont))
	((eq 'ground (car p)) (cacheallground p pl al depth cont))
	((eq 'nonground (car p)) (cacheallnonground p pl al depth cont))
	((eq 'primitive (car p)) (cacheallprimitive p pl al depth cont))
	((eq 'nonprimitive (car p)) (cacheallnonprimitive p pl al depth cont))
	((eq '== (car p)) (cacheallvalue p pl al depth cont))
	((eq 'value (car p)) (cacheallvalue p pl al depth cont))
        ((eq 'execute (car p)) (cacheallexecute p pl al depth cont))
        ((eq 'evaluate (car p)) (cacheallevaluate p pl al depth cont))
        ((eq 'unprovable (car p)) (cacheallunprovable p pl al depth cont))
        ((eq 'bagofall (car p)) (cacheallbagofall p pl al depth cont))
        ((eq 'strmatch (car p)) (cacheallstrmatch p pl al depth cont))
	((get (car p) 'basicval) (cacheallbasicvalue p pl al depth cont))
        ((get (car p) 'basic) (cacheallbasic p pl al depth cont))
        (t (cacheallrs p pl al depth cont))))

(defun cacheallnot (p pl al depth cont)
  (cond ((atom p) (cacheallnotconstant p pl al depth cont))
        ((eq 'not (car p)) (cacheallexp (cadr p) pl al depth cont))
        ((eq 'and (car p)) (cacheallnotand p pl al depth cont))
        ((eq 'or (car p)) (cacheallnotor p pl al depth cont))
        ((eq 'oneof (car p)) nil)
        ((eq 'same (car p)) nil)
        ((eq 'distinct (car p)) nil)
	((eq 'ground (car p)) nil)
	((eq 'nonground (car p)) nil)
	((eq 'primitive (car p)) nil)
	((eq 'nonprimitive (car p)) nil)
	((eq '== (car p)) (cacheallnotvalue p pl al depth cont))
	((eq 'value (car p)) (cacheallnotvalue p pl al depth cont))
        ((eq 'execute (car p)) (cacheallnotexecute p pl al depth cont))
        ((eq 'evaluate (car p)) (cacheallnotevaluate p pl al depth cont))
        ((eq 'unprovable (car p)) (cacheallexp (cadr p) pl al depth cont))
        ((eq 'bagofall (car p)) (cacheallnotbagofall p pl al depth cont))
        ((eq 'strmatch (car p)) (cacheallnotstrmatch p pl al depth cont))
	((get (car p) 'basicval) (cacheallnotbasicvalue p pl al depth cont))
        ((get (car p) 'basic) (cacheallnotbasic p pl al depth cont))
        (t (cacheallrs `(not ,p) pl al depth cont))))

(defun cacheallconstant (p pl al depth cont)
  (cond ((eq 'true p) (cacheexit pl al depth cont))
        ((eq 'false p) nil)
        (t (cacheallrs p pl al depth cont))))

(defun cacheallnotconstant (p pl al depth cont)
  (cond ((eq 'true p) nil)
        ((eq 'false p) (cacheexit pl al depth cont))
        (t (cacheallrs `(not ,p) pl al depth cont))))



(defun cachealland (p pl al depth cont)
  (cond ((null (cdr p)) (cacheexit pl al depth cont))
        ((cacheall (cadr p) (cdr p) al depth (cons (list pl al depth) cont)))
        (t (cacheallfail pl al depth))))

(defun cacheallnotand (p pl al depth cont)
  (setq cont (cons (list pl al depth) cont))
  (do ((l (cdr p) (cdr l)))
      ((null l) (cacheallfail pl al depth))
      (cacheall (maknot (car l)) (list (maknot (car l))) al depth cont)))

(defun cacheallor (p pl al depth cont)
  (setq cont (cons (list pl al depth) cont))
  (do ((l (cdr p) (cdr l)))
      ((null l) (cacheallfail pl al depth))
      (cacheall (car l) (list (car l)) al depth cont)))

(defun cacheallnotor (p pl al depth cont)
  (cond ((null (cdr p)) (cacheexit pl al depth cont))
        ((cacheall (maknot (cadr p)) (mapcar #'maknot (cdr p)) al depth
                  (cons (list pl al depth) cont)))
        (t (cacheallfail pl al depth))))


(defun cacheoneoneof (p pl al depth cont)
  (when (seqvarp (caddr p)) (setq p (plug p al)))
  (do ((l (cddr p) (cdr l)) (ol))
      ((null l) (cacheonefail pl al depth))
      (cond ((not (setq ol (unify (cadr p) al (car l) al))))
            ((cacheoneexit pl al depth cont) (backup ol) (return t))
            (t (backup ol)))))

(defun cachealloneof (p pl al depth cont)
  (when (seqvarp (caddr p)) (setq p (plug p al)))
  (do ((l (cddr p) (cdr l)) (ol))
      ((null l) (cacheallfail pl al depth))
      (when (setq ol (unify (cadr p) al (car l) al))
        (cacheexit pl al depth cont)
        (backup ol))))

(defun cacheallmember (p pl al depth cont)
  (do ((l (cdr (plug (caddr p) al)) (cdr l)) (bl))
      ((null l) t)
      (if (setq bl (mgwexp (cadr p) (car l) al))
          (cacheexit pl bl depth cont))))

(defun cacheallsame (p pl al depth cont)
  (let (ol)
    (when (setq ol (unify (cadr p) al (caddr p) al))
      (cacheexit pl al depth cont)
      (backup ol))))

(defun cachealldistinct (p pl al depth cont)
  (if (unify (cadr p) al (caddr p) al) nil (cacheexit pl al depth cont)))

(defun cacheallground (p pl al depth cont)
  (setq p (plug p al))
  (if (groundp p) (cacheexit pl al depth cont)))

(defun cacheallnonground (p pl al depth cont)
  (setq p (plug p al))
  (if (groundp p) nil (cacheexit pl al depth cont)))

(defun cacheallprimitive (p pl al depth cont)
  (setq p (plug p al))
  (if (primitivep p) (cacheexit pl al depth cont)))

(defun cacheallnonprimitive (p pl al depth cont)
  (setq p (plug p al))
  (if (primitivep p) nil (cacheexit pl al depth cont)))

(defun cacheallvalue (p pl al depth cont)
  (let (x y)
    (setq p (plug p al) x (cadr p) y (caddr p))
    (cond ((atom x) (when (setq al (mgwexp x y al)) (cacheexit pl al depth cont)))
          ((eq 'bagofall (car x))
           (setq x (cons 'listof (cachefinds (cadr x) (caddr x) *theory*)))
           (when (setq al (mgwexp y x al)) (cacheexit pl al depth cont)))
          ((and (groundp x) (every #'primitivep (cdr x)))
           (setq x (funcall (get (car x) 'basicval) x))
           (when (setq al (mgwexp x y al)) (cacheexit pl al depth cont))))))

(defun cacheallnotvalue (p pl al depth cont)
  (let (x y)
    (setq p (plug p al) x (cadr p) y (caddr p))
    (cond ((atom x) (unless (mgwexp x y al) (cacheexit pl al depth cont)))
          ((eq 'bagofall (car x))
           (setq x (cons 'listof (cachefinds (cadr x) (caddr x) *theory*)))
           (unless (mgwexp y x al)) (cacheexit pl al depth cont))
          ((and (groundp x) (every #'primitivep (cdr x)))
           (setq x (funcall (get (car x) 'basicval) x))
           (unless (mgwexp x y al) (cacheexit pl al depth cont))))))

(defun cacheallexecute (p pl al depth cont)
  (let (values)
    (setq p (plug p al))
    (cond ((null (cddr p))
           (if (ignore-errors (eval (cadr p))) (cacheexit pl al depth cont)))
          ((and (car (setq values (ignore-errors (multiple-value-list (eval (cadr p))))))
                (setq al (matchpexp (cddr p) (mapcar #'quotify values) al)))
           (cacheexit pl al depth cont)))))
    
(defun cacheallnotexecute (p pl al depth cont)
  (let (values)
    (setq p (plug p al))
    (cond ((null (cddr p))
           (if (ignore-errors (not (eval (cadr p)))) (cacheexit pl al depth cont)))
          ((and (car (setq values (ignore-errors (multiple-value-list (eval (cadr p))))))
                (not (matchpexp (cddr p) (mapcar #'quotify values) al)))
           (cacheexit pl al depth cont)))))

(defun cacheallevaluate (p pl al depth cont)
  (let (values)
    (setq p (plug p al))
    (cond ((null (cddr p))
           (if (ignore-errors (apply (caadr p) (cdadr p))) (cacheexit pl al depth cont)))
          ((and (car (setq values (ignore-errors (multiple-value-list (apply (caadr p) (cdadr p))))))
                (setq al (matchpexp (cddr p) values al)))
           (cacheexit pl al depth cont)))))
    
(defun cacheallnotevaluate (p pl al depth cont)
  (let (values)
    (setq p (plug p al))
    (cond ((null (cddr p))
           (if (ignore-errors (not (apply (caadr p) (cdadr p)))) (cacheexit pl al depth cont)))
          ((and (car (setq values (ignore-errors (multiple-value-list (apply (caadr p) (cdadr p))))))
                (not (matchpexp (cddr p) values al)))
           (cacheexit pl al depth cont)))))

(defun cacheallunprovable (p pl al depth cont)
  (cond ((cacheall (cadr p) pl al depth cont) nil)  ;; cachefindone when ready
        (t (cacheexit pl al depth cont))))

(defun cacheallbagofall (p pl al depth cont)
  (cacheallvalue `(value ,(butlast p) ,(cadddr p)) pl al depth cont))

(defun cacheallnotbagofall (p pl al depth cont)
  (cacheallnotvalue `(value ,(butlast p) ,(cadddr p)) pl al depth cont))

(defun cacheallstrmatch (p pl al depth cont)
  (cacheallexp `(execute (strmatches ,(cadr p) ,(caddr p)) ? . ,(cdddr p)) pl al depth cont))

(defun cacheallnotstrmatch (p pl al depth cont)
  (cacheallnot `(execute (strmatches ,(cadr p) ,(caddr p)) ? . ,(cdddr p)) pl al depth cont))

(defun cacheallbasicvalue (p pl al depth cont)
  (let (x y)
    (setq p (plug p al) x (butlast p) y (car (last p)))
    (if (and (groundp x) (every #'primitivep (cdr x))
             (setq x (funcall (get (car x) 'basicval) x))
             (setq al (mgwexp x y al)))
        (cacheexit pl al depth cont))))

(defun cacheallnotbasicvalue (p pl al depth cont)
  (let (x y)
    (setq p (plug p al) x (butlast p) y (car (last p)))
    (if (and (groundp x) (every #'primitivep (cdr x))
             (setq x (funcall (get (car x) 'basicval) x))
             (not (mgwexp x y al)))
        (cacheexit pl al depth cont))))

(defun cacheallbasic (p pl al depth cont)
  (setq p (plug p al))
  (when (and (groundp p) (apply (get (car p) 'basic) (cdr p)))
    (cacheexit pl al depth cont)))

(defun cacheallnotbasic (p pl al depth cont)
  (setq p (plug p al))
  (if (and (groundp p) (not (apply (get (car p) 'basic) (cdr p))))
      (cacheexit pl al depth cont)))

(defun cacheallrs (p pl al depth cont)
  (cond ((and *ancestry* (cacheallancestor p al cont)) nil)
        ((and *reduction* (cacheallreduce p pl al depth cont)))
        ;(*termination* nil)
        (t (cachealldb p pl al depth cont *theory*))))

(defun cacheallancestor (p al cont)
  (do ((l cont (cdr l)))
      ((null l) nil)
      (if (identify (caar l) (cadar l) p al) (return t))))

(defun cacheallreduce (p pl al depth cont)
  (do ((l cont (cdr l)) (ol))
      ((null l))
      (when (setq ol (unify (maknot (caaar l)) (cadar l) p al))
        (cacheexit pl al depth cont)
        (backup ol))))

(defun cachealldb (p pl al depth cont th)
  (cacheallth p pl al depth cont th)
  (do ((l (includees th) (cdr l)))
      ((null l) (cacheallfail pl al depth))
      (cachealldb p pl al depth cont (car l))))

(defun cacheallth (p pl al depth cont th)
  (do ((l (envindexps p al th) (cdr l)) (bl (environment)) (ol))
      ((null l))
      (cond ((and (listp (car l)) (eq '<= (caar l)))
             (when (setq ol (unify (cadar l) bl p al))
               (cacheallgoals (cddar l) (cdr pl) bl (1+ depth)
                             (cons (list pl al depth) cont))
               (backup ol)))
            ((setq ol (unify (car l) bl p al))
             (cacheexit pl al depth cont)
             (backup ol)))))

(defun cacheallgoals (gl pl al depth cont)
  (cond ((null gl) (cacheexit pl al depth cont))
        (t (cacheall (car gl) gl al depth cont))))

(defun cacheexit (pl al depth cont)
  (when traceexpressions
    (tracemessage depth '|Exit: | (plugstdexp (car pl) al)))
  (cond ((cdr pl) (cacheall (cadr pl) (cdr pl) al depth cont))
        (cont (cacheexit (caar cont) (cadar cont) (caddar cont) (cdr cont)))
        (t (setq *answers* (cons (plugstdexp *thing* alist) *answers*)))))

(defun cacheallfail (pl al depth)
  (when traceexpressions
    (tracemessage depth '|Fail: | (plugstdexp (car pl) al)))
  nil)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; tests
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#|
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

(cachefindp '(r a c) 'foo)

(deftheory bar ""
  (<= (s ?x) (p ?x) (q ?x))
  (<= (s ?x) (p ?x) (r ?x))
  (<= (p ?x) (m ?x ?y) (n ?y))
  (<= (n ?x) (o ?x))

  (m a a)
  (m b b)
  (m c c)
  (m d d)
  (m e e)
  (m f f)
  (m g g)
  (o a)
  (o c)
  (r c))

(cachefindx '?x '(s ?x) 'foo)

(deftheory fib ""
  (f (s 0) (s 0))
  (f (s (s 0)) (s 0))
  (<= (plus 0 ?x ?x))
  (<= (plus (s ?x) ?y (s ?z)) (plus ?x ?y ?z))
  (<= (f (s (s ?x)) ?y) (f ?x ?xb) (f (s ?x) ?xt) (plus ?xb ?xt ?y)))

(cachefindx '?y '(f (s (s (s 0))) ?y) 'fibonacci)

(deftheory fibonacci ""
  (f 1 1)
  (f 2 1)
  (<= (f ?x ?y) (- ?x 1 ?x1) (- ?x 2 ?x2) (+ ?x1 ?x2 ?y)))

(cachefindx '?y '(f 6 ?y) 'fibonacci)
|#

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
