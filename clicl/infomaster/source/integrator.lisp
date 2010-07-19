;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; (c) Copyright 1999-2005 by Michael Genesereth.  All rights reserved.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; integrator.lisp
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(eval-when (compile load eval)
  (proclaim '(special *manager* *library* *warehouse* *repository*
                      *ancestry* *collapse*
                      *message* *sender* *receiver* *target*)))

(defparameter *agents* nil)
(defparameter *collapse* 'simple)
(defparameter *compress* nil)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; integrator
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass integrator (agent) ())

(defmethod message-handler (*message* *sender* (*receiver* integrator))
  (let (*library* *warehouse* *agents*)
    (setq *library* (referent (find-rulebase *receiver*)))
    (setq *warehouse* (referent (find-database *receiver*)))
    (eval *message*)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Knowledge commands
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; affirm, retract
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod affirm (p sender (receiver integrator))
  (declare (ignore sender))
  (affirm p receiver (get-rulebase receiver)))

(defmethod retract (p sender (receiver integrator))
  (declare (ignore sender))
  (retract p receiver (get-rulebase receiver)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; askabout
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod askabout (x sender (receiver integrator))
  (declare (ignore sender))
  (askabout x receiver (get-rulebase receiver)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Data commands
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; revise, revisions, errors, materializations, notifications, reactions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod revise (p sender (receiver integrator))
  (declare (ignore sender))
  (let (revisions notifications materializations reactions factserver)
    (setq revisions (revisions p receiver) p (maksand revisions))
    (cond ((errors p receiver))
          (t (setq materializations (materializations p receiver))
             (setq notifications (notifications p receiver))
             (setq reactions (reactions p receiver))
             (setq factserver (find-target (name receiver)))
             ;(dolist (p revisions) (change p factserver))
             (dolist (p materializations) (change p factserver))
             (dolist (dum notifications)
               (revise (maksand (cdr dum)) receiver (car dum)))
             (dolist (dum reactions) (eval dum))
             'done))))

(defmethod revisions (p (receiver integrator))
  (let (*agent* *library* *agents* ruleserver temp pat pos neg answers)
    (setq *agent* (name receiver))
    (setq *library* (find-rulebase *agent*))
    (setq *agents* (find-agents))
    (setq ruleserver (symbol-value *library*))
    (setq temp (make-instance 'theory))
    (insertplusminus p temp)
    (includes temp ruleserver)
    (dolist (r (supports p ruleserver)) ; TLH #'extensionalp))
      (setq pat `(,r @l))
      (setq neg (maksor (viewresidues `(minus ,pat) temp #'specialp)))
      (setq answers (nconc answers (askallquery `(not ,pat) neg)))
      (setq pos (maksor (viewresidues `(plus ,pat) temp #'specialp)))
      (setq answers (nconc answers (askallquery pat pos))))
    (decludes temp)
    (empty temp)
    answers))

(defmethod errors (p (receiver integrator))
  (let (*agent* *library* *agents* ruleserver temp query answers)
    (setq *agent* (name receiver))
    (setq *library* (find-rulebase *agent*))
    (setq *agents* (find-agents))
    (setq ruleserver (symbol-value *library*))
    (setq temp (make-instance 'theory))
    (insertposneg p temp)
    (includes temp ruleserver)
    (setq query (maksor (viewresidues '(error ?e) temp #'specialp)))
    (setq answers (askallquery '?e query))
    (decludes temp)
    (empty temp)
    answers))

(defmethod materializations (p (receiver integrator))
  (let (*agent* *library* *agents* ruleserver temp pat pos neg answers)
    (setq *agent* (name receiver))
    (setq *library* (find-rulebase *agent*))
    (setq *agents* (find-agents))
    (setq ruleserver (symbol-value *library*))
    (setq temp (make-instance 'theory))
    (insertposneg p temp)
    (includes temp ruleserver)
    (dolist (r (viewfinds '?r '(material ?a ?r) *manager*))
      (setq pat `(,r @l))
      (setq neg (maksor (viewresidues `(neg ,pat) temp #'specialp)))
      (setq answers (nconc answers (askallquery `(not ,pat) neg)))
      (setq pos (maksor (viewresidues `(pos ,pat) temp #'specialp)))
      (setq answers (nconc answers (askallquery pat pos))))
    (decludes temp)
    (empty temp)
    answers))

(defmethod notifications (p (receiver integrator))
  (let (*agent* *library* *agents* ruleserver temp pat pos neg answers subscripts)
    (setq *agent* (name receiver))
    (setq *library* (find-rulebase *agent*))
    (setq *agents* (find-agents))
    (setq ruleserver (symbol-value *library*))
    (setq temp (make-instance 'theory))
    (insertposneg p temp)
    (includes temp ruleserver)
    (dolist (agent (subscribers '?))
      (dolist (r (subscriptions agent))
        (setq pat `(,r @l))
        (setq neg (maksor (viewresidues `(neg ,pat) temp #'specialp)))
        (setq answers (nconc answers (askallquery `(not ,pat) neg)))
        (setq pos (maksor (viewresidues `(pos ,pat) temp #'specialp)))
        (setq answers (nconc answers (askallquery pat pos))))
      (when answers (setq subscripts (cons (cons agent answers) subscripts)))
      (setq answers nil))
    (decludes temp)
    (empty temp)
    subscripts))

(defmethod reactions (p (receiver integrator))
  (let (*agent* *library* *agents* ruleserver temp query answers)
    (setq *agent* (name receiver))
    (setq *library* (find-rulebase *agent*))
    (setq *agents* (find-agents))
    (setq ruleserver (symbol-value *library*))
    (setq temp (make-instance 'theory))
    (insertposneg p temp)
    (includes temp ruleserver)
    (setq query (maksor (viewresidues '(trigger ?x) temp #'specialp)))
    (setq answers (askallquery '?x query))
    (decludes temp)
    (empty temp)
    answers))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; askp, askx, asks
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod askp (p sender (receiver integrator))
  (askx t p sender receiver))

(defmethod askx (x p sender (receiver integrator))
  (declare (ignore sender))
  (let (*agents*)
    (setq *agents* (find-agents))
    (askonequery x (integrateask x p receiver))))

(defmethod asks (x p sender (receiver integrator))
  (declare (ignore sender))
  (let (*agents*)
    (setq *agents* (find-agents))
    (askallquery x (integrateask x p receiver))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; askclass, askframe, asktable
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod askclass (x sender (receiver integrator))
  (declare (ignore sender))
  (classify x receiver))

(defmethod askframe (x class sender (receiver integrator))
  (declare (ignore x class sender))
  nil)

(defmethod asktable (ol sl sender (receiver integrator))
  (call-next-method ol sl sender receiver))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Theory commands
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; classify, facts, rules
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod classify (x (receiver integrator))
  (let (dum)
    (cond ((characterp x) 'character)
          ((stringp x) 'string)
          ((numberp x) 'number)
          ((setq dum (find-classifier (name receiver)))
           (or (askx '?class (list dum x '?class) receiver receiver) 'thing))
          ((dolist (a (finds '?a '(specialty ?a ?r) *manager*))
             (when (not (eq (setq dum (classify x a)) 'thing)) (return dum))))
          ((ruleclass x receiver (get-rulebase receiver)))
          (t 'thing))))

(defmethod facts (x (th integrator) &optional (f #'matchp))
  (do ((l (find-agents) (cdr l)) (nl)) 
      ((null l) nl)
      (cond ((eq (car l) (name th)))
            (t (setq nl (unionize nl (facts x (car l) f)))))))

(defmethod rules (x (th integrator) &optional (f #'matchp))
  ;(declare (ignore sender))
  (facts x (find-rulebase (name th)) f))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Reduction
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun integrateask (x p receiver)
  (let (ruleserver query)
    (setq ruleserver (symbol-value (find-rulebase receiver)))
    (setq query (maksor (viewresidues p ruleserver #'specialp)))
    (setq query (decolonize query))
    (when *collapse* (setq query (collapse x query)))
    ;(when *collapse* (setq query (raisin x query)))
    query))

(defun askonequery (x p)
  (cond ((eq p 'true) x)
        ((eq p 'false) nil)
        (t (eval (route (makaskemone x (source p)))))))

(defun askallquery (x p)
  (cond ((eq p 'true) (list x))
        ((eq p 'false) nil)
        (t (eval (route (makaskemall x (source p)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Collapse stuff
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod collapse (x p)
 "(COLLAPSE X P)
  COLLAPSE takes as argument a term and a boolean sentence.  It removes redundant
  literals from embedded conjunctions by matching variables not included in the
  specified list.  The result is a sentence with the same values for the
  specified variables as the original sentence."
  (cond ((eq *collapse* 'simple) (collapse2 x p))
        (*collapse* (collapse1 x p))
        (t p)))

(defun collapse1 (x p)
  (cond ((atom p) p)
        ((eq 'and (car p))
         (maksand (equalizer (vars x) (cdr p))))
        ((eq 'or (car p))
         (maksor (mapcar #'(lambda (p) (collapse1 x p)) (cdr p))))
        (t p)))

(defun equalizer (vl cl)
  (eqexp nil cl (makvlist vl)))

(defun equalizerexp (ol nl al)
  (cond ((null nl) (remove-duplicates (nreverse ol) :test #'equalp))
        ((do ((l (reverse ol) (cdr l)) (bl) (dum))
               ((null l) nil)
               (cond ((and (setq bl (matchpexp (car nl) (car l) al))
                           (setq dum (eqexp ol (cdr nl) bl)))
                      (return dum)))))
        ((do ((l (cdr nl) (cdr l)) (bl) (dum))
               ((null l) nil)
               (cond ((and (setq bl (matchpexp (car nl) (car l) al))
                           (setq dum (eqexp ol (cdr nl) bl)))
                      (return dum)))))
        ((setq al (clearup (car nl) al))
         (eqexp (cons (car nl) ol) (cdr nl) al))))

(defun clearup (x al)
  (cond ((indvarp x)
         (let ((dum (assoc x al)))
           (cond ((null dum) (acons x x al))
                 ((eq x (cdr dum)) al))))
        ((atom x) al)
        (t (do ((l x (cdr l)))
               ((null l) al)
               (unless (setq al (clearup (car l) al)) (return nil))))))

(defun eqexp  (ol nl al) (equalizerexp ol nl al))

(defun makvlist (x)
  (do ((l (vars x) (cdr l)) (al))
      ((null l) (nreverse (cons '(t . t) al)))
      (setq al (acons (car l) (car l) al))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Intermediate versions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun equalify (vl cl)
  (cond ((null cl) nil)
        ((null (cdr cl)) cl)
        (t (do ((l cl (cdr l)) (al (makvlist vl)) (bl) (ncl) (ol))
               ((null l) (nreverse ol))
               (do ((m cl (cdr m)) (nol) (nl))
                   ((null m) (setq ol (cons (car l) ol)))
                   (cond ((eq (car l) (car m)))
                         ((and (setq bl (matchpexp (car l) (car m) al))
                               (setq nol (sublis bl ol) nl (sublis bl l))
                               (setq ncl (revappend nol (cdr nl)))
                               (subsetp ncl cl :test #'equalp))
                          (setq cl ncl ol nol l nl)
                          (return t))))))))

(defun equalize (vl cl)
  (cond ((null cl) nil)
        ((null (cdr cl)) cl)
        (t (do ((l cl (cdr l)) (al (makvlist vl)) (bl) (dl) (nl))
               ((null l) (nreverse nl))
               (do ((m cl (cdr m)) (nnl) (ncl))
                   ((null m) (setq nl (cons (car l) nl)))
                   (cond ((eq (car l) (car m)))
                         ((and (setq bl (matchpexp (car l) (car m) al))
                               (setq nnl (sublis bl nl) ncl (sublis bl l))
                               (setq dl (revappend nnl (cdr ncl)))
                               (subsumepands dl cl al))
                          (setq cl dl nl nnl l ncl)
                          (return t))))))))

(defun subsumep (p q al)
  (cond ((atom p)
         (cond ((atom q) (eq p q))
               ((eq 'and (car q)) (find p (cdr q)))))
        ((eq 'and (car p))
         (cond ((atom q) nil)
               ((eq 'and (car q)) (subsumepands (cdr p) (cdr q) al))))
        (t (cond ((atom q) nil)
                 ((eq 'and (car q)) (some #'(lambda (x) (matchpexp p x al)) (cdr q)))
                 (t (matchpexp p q al))))))

(defun subsumepands (pl ql al)
  (cond ((null pl) al)
        (t (do ((m ql (cdr m)) (bl))
               ((null m))
               (if (and (setq bl (matchpexp (car pl) (car m) al))
                        (setq bl (subsumepands (cdr pl) ql bl)))
                   (return bl))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Old version
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun oldequalify (vl cl)
  (cond ((null cl) nil)
        ((null (cdr cl)) cl)
        (t (setq vl (varlist cl (varlist vl nil)))
           (do ((l cl (cdr l)) (nl))
               ((null l) nl)
               (setq vl (unvarlist (car l) vl))
               (cond ((member (car l) nl
                              :test #'(lambda (x y) (equalizablep x y truth vl))))
                     ((member (car l) (cdr l)
                              :test #'(lambda (x y) (equalizablep x y truth vl))))
                     (t (setq vl (varlist (car l) vl))
                        (setq nl (cons (car l) nl))))))))

(defun varlist (x al)
  (cond ((varp x)
         (let ((dum (assoc x al :test #'eq)))
           (cond (dum (rplacd dum (1+ (cdr dum))) al)
                 (t (acons x 1 al)))))
        ((atom x) al)
        (t (do ((l x (cdr l)))
               ((null l) al)
               (setq al (varlist (car l) al))))))

(defun unvarlist (x al)
  (cond ((varp x)
         (let ((dum (assoc x al :test #'eq)))
           (rplacd dum (1- (cdr dum)))
           al))
        ((atom x) al)
        (t (do ((l x (cdr l)))
               ((null l) al)
               (setq al (unvarlist (car l) al))))))

(defun equalizablep (x y al vl)
  (cond ((eq x y) al)
        ((indvarp x)
         (let ((dum))
           (cond ((setq dum (assoc x al :test #'eq))
                  (if (equal (cdr dum) y) al))
                 ((< (cdr (assoc x vl)) 1) (acons x y al)))))
        ((atom x) nil)
        ((atom y) nil)
        (t (do ((l x (cdr l)) (m y (cdr m)))
               ((null l) (if (null m) al))
               (cond ((null m) (return nil))
                     ((setq al (equalizablep (car l) (car m) al vl)))
                     (t (return nil)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Latest version
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun collapse2 (x p)
  (cond ((atom p) p)
        ((eq 'or (car p)) (collapse2or x p))
        ((eq 'and (car p)) (collapse2and x p))
        (t p)))

(defun collapse2or (x p)
  (do ((l (cdr p) (cdr l)) (nl))
      ((null l) (maksor (nreverse nl)))
      (setq nl (cons (collapse2 x (car l)) nl))))

(defun collapse2and (x p)
  (let ((cl (copy-list (cdr p))) (al truth))
    (do ((l cl (cdr l)) (bl))
        ((null l))
        (cond ((basep (operator (car l))))
              (t (do ((m (cdr l) (cdr m)))
                     ((null m))
                     (cond ((eq (car l) (car m)))
                           ((setq bl (collapser (car l) (car m) al))
                            (setq al bl)
                            (delete (car m) l)))))))
    (do ((l al (cdr l)) (vl (vars x)) (dum) (bl truth))
        ((null (cdr l)) (setq al bl))
        (cond ((find (caar l) vl)
               (setq dum (plug (cdar l) al))
               (cond ((groundp dum) (setq bl (cons (car l) bl)))
                     ((find dum vl) (setq bl (cons (car l) bl)))
                     (t (setq bl (acons dum (caar l) bl)))))
              (t (setq bl (cons (car l) bl)))))
    (setq cl (mapcar #'(lambda (x) (plug x al)) cl))
    (do ((l (vars x) (cdr l)) (dum))
        ((null l) (maksand cl))
        (if (not (eq (setq dum (plug (car l) al)) (car l)))
            (setq cl (cons `(same ,(car l) ,dum) cl))))))

(defun collapser (p q al)
  (cond ((atom p) (if (equalp p q) al))
        ((atom q) nil)
        ((eq 'not (car p)) nil)
        ((eq 'unprovable (car p)) nil)
        ((eq (car p) (car q)) (collapserexp p q al))))

(defun collapserexp (p q al)
  (let (cols keys)
    (when (and (setq cols (find-columns (car p))) (setq keys (find-keys (car p))))
      (do ((l (cdr p) (cdr l)) (m (cdr q) (cdr m)) (bl) (cl cols (cdr cl)) (kl keys))
          ((or (null l) (null m)) (if (and (null l) (null m) (null kl)) al))
          (cond ((eq (car cl) (car kl))
                 (if (equalp (plug (car l) al) (plug (car m) al))
                     (setq kl (cdr kl))
                     (return nil)))
                ((setq bl (mguexp (car l) (car m) al)) (setq al bl))
                (t (return nil)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; raisin
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun raisin (x p)
  (cond ((atom p) p)
        ((eq 'or (car p)) (raisinor x p))
        (t p)))

(defun raisinor (x p)
  (cond ((null (cdr p)) nil)
        ((null (cddr p)) (cadr p))
        (t (do ((l (cdr p) (cdr l)) (al (makvarlist x)) (ol))
               ((null l) (maksor (nreverse ol)))
               (cond ((some #'(lambda (p) (relsumep p (car l) al)) ol))
                     ((some #'(lambda (p) (relsumep p (car l) al)) (cdr l)))
                     (t (setq ol (cons (car l) ol))))))))

(defun makvarlist (x)
  (do ((l (vars x) (cdr l)) (al))
      ((null l) (nreverse (cons '(t . t) al)))
      (setq al (acons (car l) (car l) al))))

(defun relsumep (p q al)
  (cond ((atom p)
         (cond ((atom q) (eq p q))
               ((eq 'and (car q)) (find p (cdr q)))))
        ((eq 'and (car p))
         (cond ((atom q) nil)
               ((eq 'and (car q)) (relsumepands (cdr p) al (cdr q)))
               (t (relsumepands (cdr p) al (list q)))))
        (t (cond ((atom q) nil)
                 ((eq 'and (car q)) (some #'(lambda (x) (matchpexp p x al)) (cdr q)))
                 (t (matchpexp p q al))))))

(defun relsumepands (pl al ql)
  (cond ((null pl))
        (t (do ((m ql (cdr m)) (bl))
               ((null m))
               (if (and (setq bl (matchpexp (car pl) (car m) al))
                        (relsumepands (cdr pl) bl ql))
                   (return t))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Sourcing
;;; Example: (or (p 1) (and (q 2) (r 3))) where art performs p, bob q and r
;;; Output: (or (ask (art) (p 1)) (ask (bob) (and (q 2) (r 3))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun source (p)
  (setq p (sources p))
  (remedy p))

(defun sources (p)
  (cond ((atom p) `(ask ,(find-specialists p) ,p))
        ((eq 'and (car p)) (source-and p))
        ((eq 'or (car p)) (source-or p))
        ((eq 'not (car p))
         (setq p (sources (cadr p)))
         (cond ((eq 'ask (car p)) `(ask ,(cadr p) (not ,(caddr p))))
               (t `(not ,p))))
        ((eq 'unprovable (car p))
         (setq p (sources (cadr p)))
         (cond ((eq 'ask (car p)) `(ask ,(cadr p) (unprovable ,(caddr p))))
               (t `(unprovable ,p))))
        ((eq 'bagofall (car p))
         (let (plan)
           (setq plan (sources (caddr p)))
           (cond ((eq 'ask (car plan))
                  `(ask ,(cadr plan) (bagofall ,(cadr p) ,(caddr plan) ,(cadddr p))))
                 (t `(bagofall ,(cadr p) ,plan ,(cadddr p))))))
        (t `(ask ,(find-specialists (car p)) ,p))))

(defun source-and (p)
  (do ((l (mapcar #'sources (cdr p))) (sources) (dum) (nl))
      ((null l)
       (cond ((null (cdr nl)) (car nl))
             (t (mapc #'remedy nl) (cons 'and (nreverse nl)))))
      (cond ((not (eq 'ask (caar l))) (setq nl (cons (car l) nl) l (cdr l)))
            (t (setq sources (cadar l))
               (do ((m (cdr l) (cdr m)) (nc (list (caddar l))))
                   ((null m)
                    (setq nl (cons `(ask ,sources ,(maksand (nreverse nc))) nl))
                    (setq l nil))
                 (cond ((and (eq 'ask (caar m))
                             (setq dum (intersection* sources (cadar m))))
                        (setq sources dum nc (cons (caddar m) nc)))
                       (t (setq nl (cons `(ask ,sources ,(maksand (nreverse nc))) nl))
                          (setq l m)
                          (return t))))))))

(defun source-or (p)
  (cons 'or (mapcar #'remedy (mapcar #'sources (cdr p)))))

(defun findallspecialists (p)
  (cond ((atom p) (find-specialists p))
        ((eq 'not (car p)) (findallspecialists (cadr p)))
        ((member (car p) '(and or <= =>))
         (do ((l (cddr p) (cdr l)) (nl (findallspecialists (cadr p))))
             ((null l) (nreverse nl))
             (setq nl (intersect nl (findallspecialists (car l))))))
        ((eq 'unprovable (car p)) (findallspecialists (cadr p)))
        (t (find-specialists (car p)))))

(defun find-specialist (x)
  (cond ((eq 'true x) 'warehouse)
        ((eq 'false x) 'warehouse)
        ((basep x) 'warehouse)
        ((findp `(specialty warehouse ,x) *manager*) 'warehouse)
        ((findp `(specialty library ,x) *manager*) 'library)
        (t (findx '?a `(specialty ?a ,x) *manager*))))

(defun find-specialists (x)
  (cond ((eq 'true x) *agents*)
        ((eq 'false x) *agents*)
        ((basep x) (append *agents* (finds '?a `(specialty ?a ,x) *manager*)))
        (t (sortspecialists (finds '?a `(specialty ?a ,x) *manager*)))))

(defun sortspecialists (specialists)
  (if (find (name *warehouse*) specialists)
      (cons (name *warehouse*) (delete (name *warehouse*) specialists))
      specialists))

(defun remedy (x)
  (cond ((atom x) x)
        ((eq 'ask (car x)) (rplaca (cdr x) (caadr x)) x)
        (t x)))

(defun divvy (msg)
  (let (dum al)
    (dolist (p (cdr msg))
      (dolist (a (finds '?a `(interest ?a ,(operator p)) *manager*))
        (cond ((setq dum (assoc a al)) (rplacd dum (cons p (cdr dum))))
              (t (setq al (acons a (list p) al))))))
    (do ((l al (cdr l)) (pl))
        ((null l) (maksprogn pl))
        (setq pl (cons `(ask ,(caar l) (update . ,(nreverse (cdar l)))) pl)))))

(defun makaskemone (x p)
  (cond ((null p) nil)
        ((atom p) `(askemone ',x ',p))
        ((eq 'ask (car p)) `(ask ,(list (cadr p)) (ask-one ,x ,(caddr p))))
        (t `(askemone ',x ',p))))

(defun makaskemall (x p)
  (cond ((null p) nil)
        ((atom p) `(askemall ',x ',p))
        ((eq 'ask (car p)) `(ask ,(list (cadr p)) (ask-all ,x ,(caddr p))))
        ((eq 'or (car p))
         (cons 'union* (mapcar #'(lambda (p) (makaskemall x p)) (cdr p))))
        (t `(askemall ',x ',p))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Routing
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun route (c)
  (cond ((atom c) c)
        ((eq 'quote (car c)) c)
        ((eq 'ask (car c)) (rplaca (cdr c) (caadr c)) c)
        ((eq 'tellemall (car c)) c)
        (t (dolist (item (cdr c)) (route item)) c)))

(defun router (c)
  (setq c (routes c))
  (cond ((eq (name *library*) (caadr c)) (caddr c))
        (t (rplaca (cdr c) (caadr c)) c)))

(defun routes (c)
  (cond ((atom c) `(ask ,*agents* ,c))
        ((eq 'quote (car c)) `(ask ,*agents* ,c))
        ((eq 'ask (car c)) c)
        ((eq 'tellemall (car c)) `(ask ,*agents* ,c))
        (t (route-call c))))

(defun route-call (c)
  (do* ((args (mapcar #'routes (cdr c))) (l args (cdr l))
        (sources (find-performers (car c))) (s sources))
       ((null l) `(ask ,s ,(cons (car c) (mapcar #'caddr args))))
       (cond ((setq s (intersect s (cadar l))))
             (t (mapc #'(lambda (x) (rplaca (cdr x) (caadr x))) args)
                (return `(ask ,sources ,(cons (car c) args)))))))

(defun find-performers (x)
  (cond ((lispp x) (append *agents* (finds '?a `(performs ?a ,x) *manager*)))
        ((fboundp x) (list (name *library*)))
        ((eq 'if x) *agents*)
        (t (finds '?a `(performs ?a ,x) *manager*))))

(defun lispp (x)
  (find-symbol (symbol-name x) (find-package 'common-lisp)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Execution stuff
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun askfacts (msg)
  (do ((l (find-agents) (cdr l)) (nl))
      ((null l) nl)
      (cond ((and (eq (car l) *sender*) (equal msg *message*)))
            (t (setq nl (unionize nl (request msg *receiver* (car l))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; iterated version
;;;
;;; execution depends on *group*
;;;   nil  means nested
;;;   t    means all-at-once by copying
;;;   yank means all-at-once using oneof
;;;   yank2 means all-at-once using oneof and join
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defparameter *group* 'loyal)

(defun tellemall (q p)
  (dolist (piece (askemall q p)) (tellem piece)))

(defun tellem (p)
  (cond ((atom p))
        ((eq 'tell (car p))
         (dolist (agent (cadr p))
           (request `(tell ,(caddr p)) *receiver* agent)))
        ((eq 'untell (car p))
         (dolist (agent (cadr p))
           (request `(untell ,(caddr p)) *receiver* agent)))
        ((eq 'progn (car p)) (mapc #'tellem (cdr p)))))

(defun newtellemall (&rest rules)
  (let (tells)
    (do ((l rules (cdr l)))
        ((null l))
        (cond ((atom (car l)))
              ((eq 'tell (caar l)) (setq tells (cons (car l) tells)))
              ((eq '==> (caar l))
               (dolist (answer (askemall (caddar l) (cadar l)))
                 (setq tells (contribute answer tells))))))
    (setq tells (coalesce tells))
    (do ((l tells (cdr l)) (nl) (dum))
        ((null l) 'done)
        (setq dum (request `(tell ,(maksand (cdar l))) *receiver* (caar l)))
        (cond ((stringp dum)
               (do ((m nl (cdr m)) (untell))
                   ((null m))
                   (setq untell (maksand (nreverse (mapcar #'maknot (cdar m)))))
                   (request `(tell ,untell) *receiver* (caar m)))
               (return dum))
              (t (setq nl (cons (car l) nl)))))))

(defun newcoalesce (tells)
  (do ((l tells (cdr l)) (nl))
      ((null l) nl)
      (cond ((atom (cadar l)) (setq nl (cons (list (cadar l) (caddar l)) nl)))
            (t (dolist (who (cadar l)) (setq nl (cons (list who (caddar l)) nl)))))))

(defun coalesce (tells)
  (do ((l tells (cdr l)) (nl))
      ((null l) nl)
      (cond ((atom (cadar l)) (setq nl (coal (cadar l) (caddar l) nl)))
            (t (dolist (who (cadar l)) (setq nl (coal who (caddar l) nl)))))))

(defun coal (who what nl)
  (let (dum)
    (cond ((setq dum (assoc who nl :test #'eq))
           (rplacd dum (contribute what (cdr dum))) nl)
          (t (acons who (contribute what nil) nl)))))


(defun askemone (x p)
  (cond ((eq *group* 'loyal) (loyalone x p))
        ((eq *group* 'yank) (yankemone x p))
        ((eq *group* 'yank2) (yankemone2 x p))
        ((eq *group* 'rebel) (rebelone x p))
        (t (getemone x p))))

(defun askemall (x p)
  (cond ((eq *group* 'loyal) (loyalall x p))
        ((eq *group* 'yank) (yankemall x p))
        ((eq *group* 'yank2) (yankemall2 x p))
        ((eq *group* 'rebel) (rebelall x p))
        (*group* (findemall x p))
        (t (getemall x p))))

(defun assumeall (p th)
  (cond ((and th (symbolp th) (boundp th)) (assumeall p (symbol-value th)))
        (t (includes th *library*)
           (assume p th))))

(defun forgetall (p th)
  (cond ((and th (symbolp th) (boundp th)) (forgetall p (symbol-value th)))
        (t (includes th *library*)
           (forget p th))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; loyal - new flat all-at-once execution using oneof and no alists
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun loyalone (x p)
  (loyone x p '(t) '((t))))

(defun loyone (x p vl sl)
  (cond ((null p) nil)
        ((atom p) (loyproject x vl sl))
        ((eq 'ask (car p)) (loyoneask x p vl sl))
        ((eq 'and (car p)) (loyoneand x p vl sl))
        ((eq 'or (car p)) (loyoneor x p vl sl))
        ((eq 'unprovable (car p)) (loyoneunprovable x p vl sl))
        ((eq 'bagofall (car p)) (loyonebagofall x p vl sl))))

(defun loyoneask (x p vl sl)
  (let* ((db (cadr p)) (query (caddr p)))
    (cond ((or (null vl) (equal vl '(t)))
           (request `(ask-one ,x ,query) *receiver* db))
          (t (setq query (makand `(oneof ,vl . ,sl) query))
             (request `(ask-one ,x ,query) *receiver* db)))))

(defun loyoneand (x p vl sl)
  (cond ((null (cdr p)) (loyproject x vl sl))
        ((null (cddr p)) (loyone x (cadr p) vl sl))
        (t (do ((l (cdr p) (cdr l)) (vs (loyalvars x p vl) (cdr vs)))
               ((null (cdr l)) (loyone x (car l) vl sl))
               (if (setq sl (loyal (car vs) (car l) vl sl))
                 (setq vl (car vs))
                 (return nil))))))

(defun loyoneor (x p vl sl)
  (do ((l (cdr p) (cdr l)) (dum))
      ((null l) nil)
      (when (setq dum (loyone x (car l) vl sl)) (return dum))))

(defun loyoneunprovable (x p vl sl)
  (setq sl (set-difference sl (loyal vl (cadr p) vl sl) :test #'equalp))
  (if (equal x vl) (car sl) (loyproject x vl sl)))

(defun loyonebagofall (x p vl sl)
  (let (ivl answers)
    (setq ivl (unionize vl (vars (cadr p))))
    (setq answers (loyal ivl (caddr p) vl sl))
    (do ((l sl (cdr l)) (m answers) (nl))
        ((null l) (loyproject x (append vl (list (cadddr p))) (nreverse nl)))
        (do ((il)) (nil)
            (cond ((or (null m) (not (startp (car l) (car m))))
                   (setq nl (cons (append (car l) (list (cons 'listof (nreverse il)))) nl))
                   (return t))
                  (t (setq il (cons (loyalproj (cadr p) ivl (car m)) il))
                     (setq m (cdr m))))))))

(defun loyproject (x vl sl)
  (findx x `(oneof ,vl . ,sl) *manager*))


(defun loyalall (x p)
  (loyal x p '(t) '((t))))

(defun loyal (x p vl sl)
  (cond ((null p) nil)
        ((atom p) (loyalproject x vl sl))
        ((eq 'ask (car p)) (loyalask x p vl sl))
        ((eq 'and (car p)) (loyaland x p vl sl))
        ((eq 'or (car p)) (loyalor x p vl sl))
        ((eq 'unprovable (car p)) (loyalunprovable x p vl sl))
        ((eq 'bagofall (car p)) (loyalbagofall x p vl sl))))

(defun loyalask (x p vl sl)
  (let* ((db (cadr p)) (query (caddr p)))
    (cond ((or (null vl) (equal vl '(t)))
           (request `(ask-all ,x ,query) *receiver* db))
          (t (setq query (makand `(oneof ,vl . ,sl) query))
             (request `(ask-all ,x ,query) *receiver* db)))))

(defun loyaland (x p vl sl)
  (cond ((null (cdr p)) sl)
        ((null (cddr p)) (loyal x (cadr p) vl sl))
        (t (do ((l (cdr p) (cdr l)) (vs (loyalvars x p vl) (cdr vs)))
               ((null l) (if (equal x vl) sl (loyalproject x vl sl)))
               (if (setq sl (loyal (car vs) (car l) vl sl))
                 (setq vl (car vs))
                 (return nil))))))

(defun loyalor (x p vl sl)
  (do ((l (cdr p) (cdr l)) (nl))
      ((null l) (nreverse nl))
      (setq nl (nreconc (loyal x (car l) vl sl) nl))))

(defun loyalunprovable (x p vl sl)
  (setq sl (set-difference sl (loyal vl (cadr p) vl sl) :test #'equalp))
  (if (equal x vl) sl (loyalproject x vl sl)))

(defun loyalbagofall (x p vl sl)
  (let (ivl answers)
    (setq ivl (unionize vl (vars (cadr p))))
    (setq answers (loyal ivl (caddr p) vl sl))
    (do ((l sl (cdr l)) (m answers) (nl))
        ((null l) (loyalproject x (append vl (list (cadddr p))) (nreverse nl)))
        (do ((il)) (nil)
            (cond ((or (null m) (not (startp (car l) (car m))))
                   (setq nl (cons (append (car l) (list (cons 'listof (nreverse il)))) nl))
                   (return t))
                  (t (setq il (cons (loyalproj (cadr p) ivl (car m)) il))
                     (setq m (cdr m))))))))

(defun loyalproj (x vl s)
  (findx x `(same ,vl ,s) *manager*))

(defun loyalproject (x vl sl)
  (finds x `(oneof ,vl . ,sl) *manager*))

(defun loyalvars (x p vl)
  (cdr (loyalvarscdr vl (cdr p) (vars x))))

(defun loyalvarscdr (vl pl xl)
  (cond ((null pl) (list xl))
        (t (let (nl)
             (setq nl (loyalvarscdr (goodvarsform (car pl) vl) (cdr pl) xl))
             (cons (intersect vl (unionize (vars (car pl)) (car nl))) nl)))))

(defmethod variables ((al list))
  (do ((l al (cdr l)) (nl))
      ((null (cdr l)) nl)
      (setq nl (cons (caar l) nl))))

(defun bindings (al)
  (do ((l al (cdr l)) (nl))
      ((null (cdr l)) nl)
      (setq nl (cons (cdar l) nl))))

(defun startp (l1 l2)
  (do ()
      ((null l1) t)
      (cond ((null l2) (return nil))
            ((equalp (car l1) (car l2)) (setq l1 (cdr l1) l2 (cdr l2)))
            (t (return nil)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; one-by-one execution
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun getemone (x p)
  (let ((al (askone p truth)))
    (if al (sublis al x))))

(defun askone (p al)
  (cond ((null p) nil)
        ((atom p) (list al))
        ((eq 'ask (car p)) (askoneask p al))
        ((eq 'and (car p)) (askoneand p al))
        ((eq 'or (car p)) (askoneor p al))
        (t (askoneask (list 'ask *library* p) al))))

(defun askoneand (p al)
  (askoneands (cdr p) al))

(defun askoneands (cl al)
  (cond ((null cl) al)
        ((null (cdr cl)) (askone (car cl) al))
        (t (do ((l (askem (car cl) al) (cdr l)) (bl))
               ((null l) nil)
               (if (setq bl (askoneands (cdr cl) (car l))) (return bl))))))

(defun askoneask (p al)
  (let ((dum))
    (setq p (sublis al p))
    (when (setq dum (request `(ask-one ,(caddr p) ,(caddr p)) *receiver* (cadr p)))
      (matchpexp (caddr p) dum al))))

(defun askoneor (p al)
  (do ((l (cdr p) (cdr l)) (bl))
      ((null l))
      (if (setq bl (askone (car l) al)) (return bl))))

(defun getemall (x p)
  (do ((l (askem p truth) (cdr l)) (nl))
      ((null l) (nreverse nl))
      (setq nl (adjoin (sublis (car l) x) nl :test #'equal))))

(defun askem (p al)
  (cond ((null p) nil)
        ((atom p) (list al))
        ((eq 'ask (car p)) (askallask p al))
        ((eq 'and (car p)) (askalland p al))
        ((eq 'or (car p)) (askallor p al))
        (t (askallask (list 'ask *library* p) al))))

(defun askallask (p al)
  (setq p (sublis al p))
  (do ((l (request `(ask-all ,(caddr p) ,(caddr p)) *receiver* (cadr p)) (cdr l)) (nl))
      ((null l) (nreverse nl))
      (setq nl (cons (matchpexp (caddr p) (car l) al) nl))))

(defun askalland (p al)
  (askallands (cdr p) al))

(defun askallands (cl al)
  (cond ((null cl) (list al))
        (t (do ((l (askem (car cl) al) (cdr l)) (nl))
               ((null l) (nreverse nl))
               (setq nl (nreconc (askallands (cdr cl) (car l)) nl))))))

(defun askallor (p al)
  (do ((l (cdr p) (cdr l)) (nl))
      ((null l) (nreverse nl))
      (setq nl (nreconc (askem (car l) al) nl))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; new flat all-at-once execution using oneof but all alists
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun rebelone (x p)
  (let ((nl (rebel x p (list truth))))
    (if nl (sublis (car nl) x))))

(defun rebelall (x p)
  (do ((l (rebel x p (list truth)) (cdr l)) (nl))
      ((null l) (uniquify (nreverse nl)))
      (setq nl (cons (sublis (car l) x) nl))))

(defun rebel (x p sl)
  (cond ((null p) nil)
        ((atom p) sl)
        ((eq 'ask (car p)) (rebelask x p sl))
        ((eq 'and (car p)) (rebeland x p sl))
        ((eq 'or (car p)) (rebelor x p sl))
        ((eq 'unprovable (car p)) (rebelunprovable p sl))))

(defun rebelask (x p sl)
  (let* ((db (cadr p)) (query (caddr p)) ins answers)
    (cond ((null sl) nil)
          ((setq ins (variables (car sl)))
           (do ((l sl (cdr l)) (nl))
               ((null l)
                (setq query (makand `(oneof ,ins . ,(nreverse nl)) query)))
               (setq nl (cons (bindings (car l)) nl)))
           (setq x (economize x))
           (setq answers (request `(ask-all ,x ,query) *receiver* db))
           (do ((l answers (cdr l)) (nl))
               ((null l) (nreverse nl))
               (setq nl (cons (matchpexp x (car l) truth) nl))))
          (t (setq x (economize x))
             (setq answers (request `(ask-all ,x ,query) *receiver* db))
             (do ((l answers (cdr l)) (nl))
                 ((null l) (nreverse nl))
                 (setq nl (cons (matchpexp x (car l) truth) nl)))))))

(defun rebeland (x p sl)
  (cond ((null (cdr p)) sl)
        ((null (cddr p)) (rebel x (cadr p) sl))
        (t (do ((l (cdr p) (cdr l)) (vl (rebvars x p (car sl)) (cdr vl)))
               ((null l) sl)
               (setq sl (rebel (car vl) (car l) sl))))))

(defun rebelor (x p sl)
  (do ((l (cdr p) (cdr l)) (nl))
      ((null l) (nreverse nl))
      (setq nl (nreconc (rebel x (car l) sl) nl))))

(defun rebelunprovable (p sl)
  (set-difference sl (rebel (variables (car sl)) (cadr p) sl) :test #'equal))

(defun rebvars (x p al)
  (cdr (rebvarscdr (mapcar #'car al) (cdr p) (vars x))))

(defun rebvarscdr (vl pl xl)
  (cond ((null pl) (list xl))
        (t (let (nl)
             (setq nl (rebvarscdr (goodvarsform (car pl) vl) (cdr pl) xl))
             (cons (intersect vl (unionize (vars (car pl)) (car nl))) nl)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; nested all-at-once execution by copying and returning list of answers
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun findemone (x p)
  (let ((nl (findem (newvars x p) truth)))
    (if nl (sublis (car nl) x))))

(defun findemall (x p)
  (do ((l (findem (newvars x p) (list truth)) (cdr l)) (nl))
      ((null l) (nreverse nl))
      (setq nl (adjoin (sublis (car l) x) nl :test #'equal))))

(defun findem (p sl)
  (cond ((null p) nil)
        ((atom p) sl)
        ((eq 'ask (car p)) (findemask p sl))
        ((eq 'and (car p)) (findemand p sl))
        ((eq 'or (car p)) (findemor p sl))))

(defun findemask (p sl)
  (let* ((db (cadr p)) (aspect (caddr p)) (query (cadddr p)) answers)
    (cond ((null sl) nil)
          ((null (cdr sl))
           (unless aspect (setq aspect t))
           (setq query `(ask-all ,aspect ,(sublis (car sl) query)))
           (setq answers (request query *receiver* db))
           (do ((l answers (cdr l)) (nl))
               ((null l) (nreverse nl))
               (setq nl (cons (matchpexp aspect (car l) (car sl)) nl))))
          (t (unless aspect (setq aspect t))
             (setq query `(ask-all ,aspect ,query))
             (do ((l sl (cdr l)) (nl))
                 ((null l) (setq query (cons 'list (nreverse nl))))
               (setq nl (cons (sublis (car l) query) nl)))
             (setq answers (request query *receiver* db))
             (do ((l answers (cdr l)) (n sl (cdr n)) (nl))
                 ((null l) (nreverse nl))
                 (do ((m (car l) (cdr m)))
                     ((null m))
                     (setq nl (cons (matchpexp aspect (car m) (car n)) nl))))))))

(defun findemand (p sl)
  (findemands (cdr p) sl))

(defun findemands (cl sl)
  (cond ((null cl) sl)
        (t (findemands (cdr cl) (findem (car cl) sl)))))

(defun findemor (p sl)
  (do ((l (cdr p) (cdr l)) (nl))
      ((null l) (nreverse nl))
      (setq nl (nreconc (findem (car l) sl) nl))))

(defun newvars (x p)
  (newvarsexp p nil (vars x)))

(defun newvarsexp (p vl wl)
  (cond ((atom p) p)
        ((eq 'ask (car p))
         (let ((vs (goodvars (caddr p))) (news) (useless) (locals))
           (setq news (set-difference vs vl))
           (setq useless (set-difference news wl))
           (setq locals (set-difference news useless))
           `(ask ,(cadr p) ,locals ,(caddr p))))
        ((eq 'and (car p))
         (do ((l (cdr p) (cdr l)) (nl))
             ((null l) (cons 'and (nreverse nl)))
             (setq nl (cons (newvarsexp (car l) vl (append (vars (cdr l)) wl)) nl))
             (setq vl (nconc (goodvars (car l)) vl))))
        ((eq 'or (car p))
         (do ((l (cdr p) (cdr l)) (nl))
             ((null l) (cons 'or (nreverse nl)))
             (setq nl (cons (newvarsexp (car l) vl wl) nl))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; flat all-at-once execution using oneof but all alists
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun yankemone (x p)
  (let ((nl (yankem (newvars x p) (list truth))))
    (if nl (sublis (car nl) x))))

(defun yankemall (x p)
  (do ((l (yankem (newvars x p) (list truth)) (cdr l)) (nl))
      ((null l) (uniquify (nreverse nl)))
      (setq nl (cons (sublis (car l) x) nl))))

(defun yankem (p sl)
  (cond ((null p) nil)
        ((atom p) sl)
        ((eq 'ask (car p)) (yankemask p sl))
        ((eq 'and (car p)) (yankemand p sl))
        ((eq 'or (car p)) (yankemor p sl))))

(defun yankemask (p sl)
  (let* ((db (cadr p)) (aspect (caddr p)) (query (cadddr p)) ins answers)
    (cond ((null sl) nil)
          ((null (cdr sl))
           (setq aspect (economize aspect))
           (setq query `(ask-all ,aspect ,(sublis (car sl) query)))
           (setq answers (request query *receiver* db))
           (do ((l answers (cdr l)) (nl))
               ((null l) (nreverse nl))
               (setq nl (cons (matchpexp aspect (car l) (car sl)) nl))))
          (t (do ((l (car sl) (cdr l)))
                 ((null (cdr l)))
                 (setq ins (cons (caar l) ins)))
             (setq aspect (economize (unionize ins aspect)))
             (do ((l sl (cdr l)) (nl))
                 ((null l)
                  (setq query (makand `(oneof ,ins . ,(nreverse nl)) query)))
                 (setq nl (cons (sublis (car l) ins) nl)))
             (setq answers (request `(ask-all ,aspect ,query) *receiver* db))
             (do ((l answers (cdr l)) (nl))
                 ((null l) (nreverse nl))
                 (setq nl (cons (matchpexp aspect (car l) truth) nl)))))))

(defun yankemand (p sl)
  (yankemands (cdr p) sl))

(defun yankemands (cl sl)
  (cond ((null cl) sl)
        (t (yankemands (cdr cl) (yankem (car cl) sl)))))

(defun yankemor (p sl)
  (do ((l (cdr p) (cdr l)) (nl))
      ((null l) (nreverse nl))
      (setq nl (nreconc (yankem (car l) sl) nl))))


(defun yankemone2 (x p)
  (let ((nl (yankem2 (newvars x p) (list truth))))
    (if nl (sublis (car nl) x))))

(defun yankemall2 (x p)
  (do ((l (yankem2 (newvars x p) (list truth)) (cdr l)) (nl))
      ((null l) (uniquify (nreverse nl)))
      (setq nl (cons (sublis (car l) x) nl))))

(defun yankem2 (p sl)
  (cond ((null p) nil)
        ((atom p) sl)
        ((eq 'ask (car p)) (yankemask2 p sl))
        ((eq 'and (car p)) (yankemand2 p sl))
        ((eq 'or (car p)) (yankemor2 p sl))))

(defun yankemask2 (p sl)
  (let* ((db (cadr p)) (aspect (caddr p)) (query (cadddr p)) ins answers)
    (cond ((null sl) nil)
          ((null (cdr sl))
           (setq aspect (economize aspect))
           (setq query `(ask-all ,aspect ,(sublis (car sl) query)))
           (setq answers (request query *receiver* db))
           (do ((l answers (cdr l)) (nl))
               ((null l) (nreverse nl))
               (setq nl (cons (matchpexp aspect (car l) (car sl)) nl))))
          (t (setq ins (boundvars query (car sl)))
             (setq aspect (economize (unionize ins aspect)))
             (when ins
               (do ((l sl (cdr l)) (nl))
                   ((null l)
                    (setq query (makand `(oneof ,ins . ,(uniquify (nreverse nl))) query)))
                   (setq nl (cons (sublis (car l) ins) nl))))
             (setq answers (request `(ask-all ,aspect ,query) *receiver* db))
             (do ((l answers (cdr l)) (nl))
                 ((null l) (joinal sl (nreverse nl)))
                 (setq nl (cons (matchpexp aspect (car l) truth) nl)))))))

(defun yankemand2 (p sl)
  (yankemands2 (cdr p) sl))

(defun yankemands2 (cl sl)
  (cond ((null cl) sl)
        (t (yankemands2 (cdr cl) (yankem2 (car cl) sl)))))

(defun yankemor2 (p sl)
  (do ((l (cdr p) (cdr l)) (nl))
      ((null l) (nreverse nl))
      (setq nl (nreconc (yankem2 (car l) sl) nl))))

(defun boundvars (x al)
  (do ((l (vars x) (cdr l)) (nl))
      ((null l) (nreverse nl))
      (if (assoc (car l) al) (setq nl (cons (car l) nl)))))

;;; what do we do about ground queries?
;;; following assumes that alists all have same structure

(defun joinal (as bs)
  (do ((l as (cdr l)) (dum) (nl))
      ((null l) (nreverse nl))
      (do ((m bs (cdr m)))
          ((null m))
          (when (setq dum (joinemal (car l) (car m)))
            (setq nl (cons dum nl))))))

(defun joinemal (al bl)
  (let (dum)
    (cond ((null (cdr al)) bl)
          ((setq dum (assoc (caar al) bl :test #'eq))
           (if (equal (cdar al) (cdr dum)) (joinemal (cdr al) bl)))
          ((setq bl (joinemal (cdr al) bl)) (cons (car al) bl)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Miscellaneous
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun find-agents ()
  (cons 'repository (finds '?a '(specialty ?a ?r) *manager*)))

(defun interestp (r)
  (or (eq r 'evaluate) (findp `(interest ?a ,r) *manager*)))

(defun specialp (r)
  (or (basep r) (findp `(specialty ?a ,r) *manager*)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
