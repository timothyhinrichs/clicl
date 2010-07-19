;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; (c) Copyright 1999-2003 by Michael Genesereth.  All rights reserved.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(eval-when (compile load eval)
  (proclaim '(special *metalibrary* *manager* *library* *warehouse*
                      *theory* *ancestry* *saves* *assumables* *test*
                      *order* *collapse* *host* *port* 
                      *message* *sender* *receiver* *target* *agent*)))

(setq *saves* t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; broker
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass broker (agent) ())

(defmethod message-handler (*message* *sender* (*receiver* broker))
  (let (*library* *warehouse*)
    (setq *library* (referent (find-rulebase *receiver*)))
    (setq *warehouse* (referent (find-database *receiver*)))
    (acleval *message* *sender* *receiver*)))

(defmethod acleval (msg sender (receiver broker))
  (cond ((atom msg) (call-next-method msg sender receiver))
        ((eq 'tell (car msg))
         (includes *receiver* *library*)
         (catch 'assume (assume (cadr msg) receiver)))
        ((eq 'untell (car msg))
         (includes *receiver* *library*)
         (catch 'forget (forget (cadr msg) receiver)))
        ((eq 'eliminate (car msg))
         (includes *receiver* *library*)
         (discard (cadr msg) receiver))
        ((eq 'ask-if (car msg)) (findp (cadr msg) receiver))
        ((eq 'ask-one (car msg)) (findx (cadr msg) (caddr msg) receiver))
        ((eq 'ask-all (car msg)) (finds (cadr msg) (caddr msg) receiver))
        ((eq 'ask-table (car msg)) (findtable (cadr msg) (caddr msg)))
        ((eq 'ask-about (car msg)) (askabout (cadr msg) sender receiver))
        ((findp `(responds ?a ,(car msg)) *manager*) (askmessage msg))
        ((findp `(performs ?a ,(car msg)) *manager*) (askrequest msg))
        (t (call-next-method msg sender receiver))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; affirm, retract, discardall
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod affirm (p sender (receiver broker))
  (declare (ignore sender))
  (save p receiver))

(defmethod retract (p sender (receiver broker))
  (declare (ignore sender))
  (drop p receiver))

(defmethod discardall (p sender (receiver broker))
  (declare (ignore sender))
  (kill p receiver))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; revise, errors, revisions, notifications, reactions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; insert, uninsert
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod insert (p (th broker))
  (do ((l (finds '?a `(interest ?a ,(operator p)) *manager*) (cdr l))
       (msg `(tell ,p)) (dum))
      ((null l) 'done)
      (cond ((and (eq (car l) *sender*) (equal msg *message*)))
            ((stringp (setq dum (request msg *receiver* (car l))))
             (throw 'assume dum)))))

(defmethod uninsert (p (th broker))
  (do ((l (finds '?a `(interest ?a ,(operator p)) *manager*) (cdr l))
       (msg `(untell ,p)) (dum))
      ((null l) 'done)
      (cond ((and (eq (car l) *sender*) (equal msg *message*)))
            ((stringp (setq dum (request msg *receiver* (car l))))
             (throw 'forget dum)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; askp, askx, asks
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; asktable, askabout
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod asktable (ol sl sender (receiver broker))
  (call-next-method ol sl sender receiver))

(defmethod askabout (x sender (receiver broker))
  (do ((l (find-agents) (cdr l)) (msg `(ask-about ,x)) (nl))
      ((null l) nl)
      (cond ((and (eq (car l) sender) (equal msg *message*)))
            (t (setq nl (nconc nl (request msg receiver (car l))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; envindexps
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod envindexps (p al (th broker))
  (setq p (plugstdexp p al))
  (do ((l (finds '?a `(specialty ?a ,(car p)) *manager*) (cdr l)) (nl))
      ((null l) (nreverse nl))
      (setq nl (nreconc (request `(ask-all ,p ,p) *receiver* (car l)) nl))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; stuff
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun find-rulebase (agent)
  (findx '?x `(rulebase ,(name agent) ?x) *manager*)) 

(defun find-database (agent)
  (findx '?x `(database ,(name agent) ?x) *manager*))

;;; needs update handling
;;; Note extreme slowness of indexing.
;;; Note that it still gets all answers to subqueries as part of this indexing!
;;; However, second agent gets narrower request, just more of them.

(defmethod acleval (msg sender (receiver agent))
  (cond ((atom msg) (call-next-method msg sender receiver))
        ((eq 'tell (car msg))
         (cond ((null (cddr msg)) (affirm (cadr msg) sender receiver))
               (t (affirmall (cadr msg) (caddr msg) sender receiver))))
        ((eq 'untell (car msg))
         (cond ((null (cddr msg)) (retract (cadr msg) sender receiver))
               (t (retractall (cadr msg) (caddr msg) sender receiver))))
        ((eq 'eliminate (car msg)) (kill (cadr msg) receiver))
        ((eq 'ask-if (car msg)) (findp (cadr msg) receiver))
        ((eq 'ask-one (car msg)) (findx (cadr msg) (caddr msg) receiver))
        ((eq 'ask-all (car msg)) (finds (cadr msg) (caddr msg) receiver))
        ((eq 'ask-table (car msg)) (findtable (cadr msg) (caddr msg)))
        ((eq 'ask-about (car msg)) (sentences (cadr msg) receiver))
        ((eq 'ask (car msg)) (request (caddr msg) receiver (cadr msg)))
        (t (call-next-method msg sender receiver))))

(defmethod acleval (x sender receiver)
  (cond ((atom x)
         (cond ((numberp x) x)
               ((characterp x) x)
               ((stringp x) x)
               ((symbolp x) (if (boundp x) (symbol-value x)))
               (t x)))
        ((eq 'if (car x))
         (cond ((acleval (cadr x) sender receiver)
                (acleval (caddr x) sender receiver))
               ((cadddr x) (acleval (cadddr x) sender receiver))))
        ((eq 'progn (car x))
         (do ((l (cdr x) (cdr l)) (ans))
             ((null l) ans)
             (setq ans (acleval (car l) sender receiver))))
        ((eq 'quote (car x)) (cadr x))
        ((eq 'setq (car x)) (set (cadr x) (acleval (caddr x) sender receiver)))
        ((special-form-p (car x)) nil)
        ((macro-function (car x)) (acleval (macroexpand x) sender receiver))
        ((fboundp (car x))
         (apply (car x) (mapcar #'(lambda (x) (acleval x sender receiver))
                                (cdr x))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
