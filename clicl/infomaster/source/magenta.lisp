;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; (c) Copyright 1992-2008 by Michael Genesereth.  All rights reserved.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; magenta.lisp
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(eval-when (compile load eval)
  (proclaim '(special *var-count* *manager* *gui*)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; messages
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defparameter *message* nil)

(defparameter *sender* nil)

(defparameter *receiver* nil)

(defparameter *wiretap* nil)

(defmethod message (msg sender receiver)
  (when *wiretap* (fresh-line *wiretap*)
    (format *wiretap* "~A --> ~B:" (name sender) (name receiver))
    (fresh-line *wiretap*) (format *wiretap* "~S" msg)
    (terpri *wiretap*) (force-output *wiretap*))
  (message-handler msg sender receiver)
  (values))

(defmethod request (msg sender receiver)
  (when *wiretap* (fresh-line *wiretap*)
    (format *wiretap* "~A --> ~B:" (name sender) (name receiver))
    (fresh-line *wiretap*) (format *wiretap* "~S" msg)
    (terpri *wiretap*) (force-output *wiretap*))
  (setq msg (message-handler msg sender receiver))
  (when *wiretap* (fresh-line *wiretap*)
    (format *wiretap* "~A <-- ~B:" (name sender) (name receiver))
    (fresh-line *wiretap*) (format *wiretap* "~S" msg)
    (terpri *wiretap*) (force-output *wiretap*))
  msg)

(defmethod message-handler (*message* *sender* *receiver*)
  (eval *message*))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; macros
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmacro tell (x &optional p)
  (cond ((null p) `(affirm ',x *sender* *receiver*))
        (t `(affirmall ',x ',p *sender* *receiver*))))

(defmacro untell (x &optional p)
  (cond ((null p) `(retract ',x *sender* *receiver*))
        (t `(retractall ',x ',p *sender* *receiver*))))

(defmacro eliminate (x)
  `(discard ',x *sender* *receiver*))

(defmacro update (&rest rules)
  `(revise ',(maksand rules) *sender* *receiver*))

(defmacro updates (&rest rules)
  `(revisions ',(maksand rules) *receiver*))

(defmacro ask-if (query)
  `(askp ',query *sender* *receiver*))

(defmacro ask-one (aspect query)
  `(askx ',aspect ',query *sender* *receiver*))

(defmacro ask-all (aspect query)
  `(asks ',aspect ',query *sender* *receiver*))

(defmacro ask-table (objs slots)
  `(asktable ',objs ',slots *sender* *receiver*))

(defmacro ask-about (x)
  `(askabout ',x *sender* *receiver*))

(defmacro ask (who what)
  `(request ',what *receiver* ',who))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; General
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Classic Performatives
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod initialize (x)
  (declare (ignore x))
  'done)

(defmethod affirm (p sender receiver)
  (declare (ignore p sender receiver))
  nil)

(defmethod affirmall (x p sender receiver)
  (declare (ignore x p sender receiver))
  nil)

(defmethod retract (p sender receiver)
  (declare (ignore p sender receiver))
  nil)

(defmethod retractall (x p sender receiver)
  (declare (ignore x p sender receiver))
  nil)

(defmethod askabout (x sender receiver)
  (declare (ignore x sender receiver))
  nil)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod revise (p sender receiver)
  (declare (ignore p sender receiver))
  nil)

(defmethod revisions (p receiver)
  (declare (ignore p receiver))
  nil)

(defmethod errors (p receiver)
  (declare (ignore p receiver))
  nil)

(defmethod materializations (p receiver)
  (declare (ignore p receiver))
  nil)

(defmethod notifications (p receiver)
  (declare (ignore p receiver))
  nil)

(defmethod reactions (p receiver)
  (declare (ignore p receiver))
  nil)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod askp (p sender receiver)
  (declare (ignore p sender receiver))
  nil)

(defmethod askx (x p sender receiver)
  (declare (ignore x p sender receiver))
  nil)

(defmethod asks (x p sender receiver)
  (declare (ignore x p sender receiver))
  nil)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod askclass (x sender receiver)
  (declare (ignore x sender receiver))
  nil)

(defmethod askframe (x class sender receiver)
  (declare (ignore x class sender receiver))
  nil)

(defmethod asktable (ol sl sender receiver)
  (declare (ignore ol sl sender receiver))
  nil)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Inference Commands
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod assume (p source)
  (declare (ignore p source))
  nil)

(defmethod forget (p source)
  (declare (ignore p source))
  nil)

(defmethod findp (p source)
  (declare (ignore p source))
  nil)

(defmethod findx (x p source)
  (declare (ignore x p source))
  nil)

(defmethod finds (x p source)
  (declare (ignore x p source))
  nil)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Structure Commands
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod defineobject (obj class constraints source)
  (killobject obj source)
  (saveobject (list* obj class constraints) source)
  obj)

(defmethod saveobject (structure source)
  (insert (makpred (car structure) (cadr structure) source) source)
  (dolist (pair (cddr structure))
    (when (cdr pair)
      (insert (list (first pair) (car structure) (second pair)) source)))
  structure)

(defmethod killobject (object source)
  (kill `(?r ,object @l) source #'matchp))

(defmethod findinstancep (object structure source)
  (declare (ignore object structure source))
  nil)

(defmethod findinstance (structure source)
  (declare (ignore structure source))
  nil)

(defmethod findinstances (structure source)
  (declare (ignore structure source))
  nil)

(defmethod findoptions (slot structure source)
  (declare (ignore slot structure source))
  nil)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Class Commands
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod instancep (object class source)
  (declare (ignore object class source))
  nil)

(defmethod classify (object source)
  (declare (ignore object source))
  'thing)

(defmethod instance (class source)
  (declare (ignore class source))
  nil)

(defmethod instances (class source)
  (declare (ignore class source))
  nil)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod doublep (predicate object source)
  (declare (ignore predicate object source))
  nil)

(defmethod itemx (predicate source)
  (declare (ignore predicate source))
  nil)

(defmethod items (predicate source)
  (declare (ignore predicate source))
  nil)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod triplep (slot object result source)
  (askp (list slot object result) nil source))

(defmethod object (slot value source)
  (declare (ignore slot value source))
  nil)

(defmethod objects (slot value source)
  (declare (ignore slot value source))
  nil)

(defmethod result (slot object source)
  (declare (ignore slot object source))
  nil)

(defmethod results (slot object source)
  (declare (ignore slot object source))
  nil)

(defmethod options (slot class source)
  (declare (ignore slot class source))
  nil)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Symbols
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Classic Performatives
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod message (msg sender (receiver symbol))
  (cond ((and (boundp receiver) (not (symbolp (symbol-value receiver))))
         (message msg sender (symbol-value receiver)))
        (t (call-next-method msg sender receiver))))

(defmethod request (msg sender (receiver symbol))
  (cond ((and (boundp receiver) (not (symbolp (symbol-value receiver))))
         (request msg sender (symbol-value receiver)))
        (t (call-next-method msg sender receiver))))

(defmethod message-handler (msg sender (receiver symbol))
  (cond ((and (boundp receiver) (not (symbolp (symbol-value receiver))))
         (message-handler msg sender (symbol-value receiver)))
        (t (call-next-method msg sender receiver))))

(defmethod initialize ((x symbol))
  (cond ((and (boundp x) (not (symbolp (symbol-value x))))
         (initialize (symbol-value x)))
        (t (call-next-method x))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod affirm (p sender (receiver symbol))
  (cond ((and (boundp receiver) (not (symbolp (symbol-value receiver))))
         (affirm p sender (symbol-value receiver)))
        (t (call-next-method p sender receiver))))

(defmethod affirmall (x p sender (receiver symbol))
  (cond ((and (boundp receiver) (not (symbolp (symbol-value receiver))))
         (affirmall x p sender (symbol-value receiver)))
        (t (call-next-method x p sender receiver))))

(defmethod retract (p sender (receiver symbol))
  (cond ((and (boundp receiver) (not (symbolp (symbol-value receiver))))
         (retract p sender (symbol-value receiver)))
        (t (call-next-method p sender receiver))))

(defmethod retractall (x p sender (receiver symbol))
  (cond ((and (boundp receiver) (not (symbolp (symbol-value receiver))))
         (retractall x p sender (symbol-value receiver)))
        (t (call-next-method x p sender receiver))))

(defmethod askabout (x sender (receiver symbol))
  (cond ((and (boundp receiver) (not (symbolp (symbol-value receiver))))
         (askabout x sender (symbol-value receiver)))
        (t (call-next-method x sender receiver))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod revise (p sender (receiver symbol))
  (cond ((and (boundp receiver) (not (symbolp (symbol-value receiver))))
         (revise p sender (symbol-value receiver)))
        (t (call-next-method p sender receiver))))

(defmethod revisions (p (receiver symbol))
  (cond ((and (boundp receiver) (not (symbolp (symbol-value receiver))))
         (revisions p (symbol-value receiver)))
        (t (call-next-method p receiver))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod askp (p sender (receiver symbol))
  (cond ((and (boundp receiver) (not (symbolp (symbol-value receiver))))
         (askp p sender (symbol-value receiver)))
        (t (call-next-method p sender receiver))))

(defmethod askx (x p sender (receiver symbol))
  (cond ((and (boundp receiver) (not (symbolp (symbol-value receiver))))
         (askx x p sender (symbol-value receiver)))
        (t (call-next-method x p sender receiver))))

(defmethod asks (x p sender (receiver symbol))
  (cond ((and (boundp receiver) (not (symbolp (symbol-value receiver))))
         (asks x p sender (symbol-value receiver)))
        (t (call-next-method x p sender receiver))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod askclass (x sender (receiver symbol))
  (cond ((and (boundp receiver) (not (symbolp (symbol-value receiver))))
         (askclass x sender (symbol-value receiver)))
        (t (call-next-method x sender receiver))))

(defmethod askframe (x class sender receiver)
  (cond ((and (boundp receiver) (not (symbolp (symbol-value receiver))))
         (askframe x class sender (symbol-value receiver)))
        (t (call-next-method x class sender receiver))))

(defmethod asktable (ol sl sender (receiver symbol))
  (cond ((and (boundp receiver) (not (symbolp (symbol-value receiver))))
         (asktable ol sl sender (symbol-value receiver)))
        (t (call-next-method ol sl sender receiver))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Inference Commands
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod assume (p (source symbol))
  (cond ((and (boundp source) (not (symbolp (symbol-value source))))
         (assume p (symbol-value source)))
        (t (call-next-method p source))))

(defmethod forget (p (source symbol))
  (cond ((and (boundp source) (not (symbolp (symbol-value source))))
         (forget p (symbol-value source)))
        (t (call-next-method p source))))

(defmethod findp (p (source symbol))
  (cond ((and (boundp source) (not (symbolp (symbol-value source))))
         (findp p (symbol-value source)))
        (t (call-next-method p source))))

(defmethod findx (x p (source symbol))
  (cond ((and (boundp source) (not (symbolp (symbol-value source))))
         (findx x p (symbol-value source)))
        (t (call-next-method x p source))))

(defmethod finds (x p (source symbol))
  (cond ((and (boundp source) (not (symbolp (symbol-value source))))
         (finds x p (symbol-value source)))
        (t (call-next-method x p source))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Theory Commands
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod change (p (receiver symbol))
  (cond ((and (boundp receiver) (not (symbolp (symbol-value receiver))))
         (change p (symbol-value receiver)))
        (t (call-next-method p receiver))))

(defmethod facts (x (source symbol) &optional (f #'matchp))
  (cond ((and (boundp source) (not (symbolp (symbol-value source))))
         (facts x (symbol-value source) f))
        (t (call-next-method x source f))))

(defmethod rules (x (source symbol) &optional (f #'matchp))
  (cond ((and (boundp source) (not (symbolp (symbol-value source))))
         (rules x (symbol-value source) f))
        (t (call-next-method x source f))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Structure Commands
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod defineobject (obj class constraints (source symbol))
  (cond ((and (boundp source) (not (symbolp (symbol-value source))))
         (defineobject obj class constraints (symbol-value source)))
        (t (call-next-method obj class constraints  source))))

(defmethod saveobject (structure (source symbol))
  (cond ((and (boundp source) (not (symbolp (symbol-value source))))
         (saveobject structure (symbol-value source)))
        (t (call-next-method structure source))))

(defmethod killobject (object (source symbol))
  (cond ((and (boundp source) (not (symbolp (symbol-value source))))
         (killobject object (symbol-value source)))
        (t (call-next-method object source))))

(defmethod findinstancep (object structure (source symbol))
  (cond ((and (boundp source) (not (symbolp (symbol-value source))))
         (findinstancep object structure (symbol-value source)))
        (t (call-next-method object structure source))))

(defmethod findinstance (structure (source symbol))
  (cond ((and (boundp source) (not (symbolp (symbol-value source))))
         (findinstance structure (symbol-value source)))
        (t (call-next-method structure source))))

(defmethod findinstances (structure (source symbol))
  (cond ((and (boundp source) (not (symbolp (symbol-value source))))
         (findinstances structure (symbol-value source)))
        (t (call-next-method structure source))))

(defmethod findoptions (attribute structure (source symbol))
  (cond ((and (boundp source) (not (symbolp (symbol-value source))))
         (findoptions attribute structure (symbol-value source)))
        (t (call-next-method attribute structure source))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Class Commands
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod instancep (x class (source symbol))
  (cond ((and (boundp source) (not (symbolp (symbol-value source))))
         (instancep x class (symbol-value source)))
        (t (call-next-method x class source))))

(defmethod classify (x (source symbol))
  (cond ((and (boundp source) (not (symbolp (symbol-value source))))
         (classify x (symbol-value source)))
        (t (call-next-method x source))))

(defmethod instance (class (source symbol))
  (cond ((and (boundp source) (not (symbolp (symbol-value source))))
         (instance class (symbol-value source)))
        (t (call-next-method class source))))

(defmethod instances (class (source symbol))
  (cond ((and (boundp source) (not (symbolp (symbol-value source))))
         (instances class (symbol-value source)))
        (t (call-next-method class source))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod doublep (predicate item (source symbol))
  (cond ((and (boundp source) (not (symbolp (symbol-value source))))
         (doublep predicate item (symbol-value source)))
        (t (call-next-method predicate item source))))

(defmethod itemx (predicate (source symbol))
  (cond ((and (boundp source) (not (symbolp (symbol-value source))))
         (itemx predicate (symbol-value source)))
        (t (call-next-method predicate source))))

(defmethod items (predicate (source symbol))
  (cond ((and (boundp source) (not (symbolp (symbol-value source))))
         (items predicate (symbol-value source)))
        (t (call-next-method predicate source))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod triplep (slot object value (source symbol))
  (cond ((and (boundp source) (not (symbolp (symbol-value source))))
         (triplep slot object value (symbol-value source)))
        (t (call-next-method slot object value source))))

(defmethod object (slot value (source symbol))
  (cond ((and (boundp source) (not (symbolp (symbol-value source))))
         (object slot value (symbol-value source)))
        (t (call-next-method slot value source))))

(defmethod objects (slot value (source symbol))
  (cond ((and (boundp source) (not (symbolp (symbol-value source))))
         (objects slot value (symbol-value source)))
        (t (call-next-method slot value source))))

(defmethod result (slot item (source symbol))
  (cond ((and (boundp source) (not (symbolp (symbol-value source))))
         (result slot item (symbol-value source)))
        (t (call-next-method slot item source))))

(defmethod results (slot item (source symbol))
  (cond ((and (boundp source) (not (symbolp (symbol-value source))))
         (results slot item (symbol-value source)))
        (t (call-next-method slot item source))))

(defmethod options (attribute class (source symbol))
  (cond ((and (boundp source) (not (symbolp (symbol-value source))))
         (options attribute class (symbol-value source)))
        (t (call-next-method attribute class source))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Agents
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Basics
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass agent (theory)
  ((name :accessor name :initarg :name :initform nil)))

(defmethod initialize-instance ((agent agent) &rest init-args)
  (apply #'call-next-method agent init-args)
  (when (name agent) (set (name agent) agent)))

(defmethod initialize ((x agent))
  (empty x)
  'done)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Classic Performatives
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod affirm (p sender (receiver agent))
  (cond ((atom p) (assume p receiver))
        ((eq 'error (car p)) (cadr p))
        ((eq 'and (car p)) (affirmemall (cdr p) sender receiver))
        ((eq '==> (car p))
         (affirmall (caddr p) (cadr p) sender receiver))
        (t (assume p receiver))))

(defmethod affirmall (x p sender (receiver agent))
  (do ((l (finds x p receiver) (cdr l)) (dum))
      ((null l) t)
      (setq dum (affirm (car l) sender receiver))
      (when (stringp dum) (return dum))))

(defmethod affirmemall (rules sender (receiver agent))
  (let (tells)
    (do ((l rules (cdr l)))
        ((null l) (setq tells (nreverse tells)))
        (cond ((atom (car l)) (setq tells (cons (car l) tells)))
              ((eq '==> (caar l))
               (dolist (answer (asks (caddar l) (cadar l) sender receiver))
                 (setq tells (contribute answer tells))))
              (t (setq tells (contribute (car l) tells)))))
    (do ((l tells (cdr l)) (dum) (nl))
        ((null l) 'done)
        (setq dum (affirm (car l) sender receiver))
        (cond ((stringp dum)
               (retractemall nl sender receiver)
               (return dum))
              (t (setq nl (cons (car l) nl)))))))

(defmethod retract (p sender (receiver agent))
  (cond ((atom p) (drop p receiver))
        ((eq 'and (car p))
         (dolist (sent (cdr p)) (retract sent sender receiver)))
        (t (drop p receiver))))

(defmethod retractall (x p sender (receiver agent))
  (cond ((atom x) (dropall x p receiver))
        ((eq 'and (car x))
         (dolist (sent (cdr x)) (retractall sent p sender receiver)))
        (t (dropall x p receiver))))

(defun retractemall (untells sender receiver)
  (dolist (untell untells) (retract untell sender receiver)))

(defmethod askabout (x sender (receiver agent))
  (declare (ignore sender))
  (facts x receiver))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod askp (p sender (receiver agent))
  (declare (ignore sender))
  (findp p receiver))

(defmethod askx (x p sender (receiver agent))
  (declare (ignore sender))
  (findx x p receiver))

(defmethod asks (x p sender (receiver agent))
  (declare (ignore sender))
  (finds x p receiver))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod askclass (x sender (receiver agent))
  (declare (ignore sender))
  (classify x receiver))

(defmethod askframe (x class sender (receiver agent))
  (declare (ignore x class sender))
  nil)

(defmethod asktable (items slots sender (receiver agent))
  (cond ((or (null items) (null slots)) nil)
        ((and (null (cdr items)) (null (cdr slots)))
         (list (list (asks '?y (list (car slots) (car items) '?y) 
                           sender receiver))))
        (t (do ((l slots (cdr l)) (dum) (nl))
               ((null l) (invert-table (nreverse nl)))
               (setq dum (asks '(?x ?y) `(and (oneof ?x . ,items) (,(car l) ?x ?y))
                               sender receiver))
               (setq nl (cons (extract-values items dum) nl))))))

;;; Hey, Don, this is pretty slow.

(defun invert-table (table)
  (loop
      for i from 0 to (1- (length (first table)))
      for new-row = (mapcar #'(lambda (old-row) (nth i old-row)) table)
      collect new-row ))

(defun extract-values (items answers)
  (setq answers (cons nil answers))
  (do ((l items (cdr l)) (nl))
      ((null l) (nreverse nl))
      (do ((m answers) (nm))
          ((null (cdr m)) (setq nl (cons (nreverse nm) nl)))
          (cond ((eql (car l) (caadr m))
                 (setq nm (cons (cadadr m) nm))
                 (rplacd m (cddr m)))
                (t (setq m (cdr m)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Inference Commands
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod assume (p (source agent))
  (save p source))

(defmethod forget (p (source agent))
  (drop p source))

(defmethod findp (p (source agent))
  (viewfindp p source))

(defmethod findx (x p (source agent))
  (viewfindx x p source))

(defmethod finds (x p (source agent))
  (viewfinds x p source))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Theory Commands
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun saveall (p q th)
  (dolist (sentence (finds p q th)) (save sentence th)) t)

(defun dropall (p q th)
  (dolist (sentence (finds p q th)) (drop sentence th)) t)

(defmethod change (p (th agent))
  (let (updates errors)
    (setq updates (changes p th))
    (dolist (item updates)
      (when (and (listp item) (eq (car item) 'error))
        (setq errors (cons (cadr item) errors))))
    (cond (errors (nreverse errors))
          (t (dolist (sentence updates) (assume sentence th))
             (sendem `(update . ,updates) th)))))

(defmethod changes (p (th agent))
  (nreverse (computechanges p th nil)))

(defun computechanges (p th nl)
  (cond ((atom p) (adjoin p nl))
        ((eq '==> (car p))
         (dolist (item (finds (caddr p) (cadr p) th))
           (setq nl (contribute item nl)))
         nl)
        ((eq 'and (car p))
         (dolist (item (cdr p)) (setq nl (computechanges item th nl))) nl)
        ((eq 'update (car p))
         (dolist (item (cdr p)) (setq nl (computechanges item th nl))) nl)
        (t (adjoin p nl :test #'equalp))))

(defun contribute (p cl)
  (cond ((atom p) (adjoin p cl))
        ((eq 'and (car p)) (revappend (cdr p) cl))
        (t (adjoin p cl :test #'equalp))))

(defun sendem (msg receiver)
  (dolist (target (find-targets (name receiver)))
    (request msg receiver target))
  'done)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Structure Commands
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod findinstancep (object structure (source agent))
  (cond ((eq (cadr structure) 'number) (numberfindinstancep object structure))
        ((eq (cadr structure) 'string) (stringfindinstancep object structure))
        (t (genfindinstancep object structure source))))

(defun numberfindinstancep (object structure)
  (and (numberp object)
       (do ((l (cddr structure) (cdr l)))
           ((null l) t)
           (cond ((null (cdar l)))
                 ((eq (caar l) 'lowerbound)
                  (unless (leqp (cadar l) object) (return nil)))
                 ((eq (caar l) 'upperbound)
                  (unless (leqp object (cadar l)) (return nil)))))))

(defun stringfindinstancep (object structure)
  (and (stringp object)
       (do ((l (cddr structure) (cdr l)))
           ((null l) t)
           (cond ((null (cdar l)))
                 ((eq (caar l) 'substring)
                  (unless (substringp (cadar l) object) (return nil)))))))

(defun genfindinstancep (object structure source)
  (do ((l (cddr structure) (cdr l)))
      ((null l) (or (eq (cadr structure) 'thing)
                    (instancep object (cadr structure) source)))
      (cond ((null (cdar l)))
            ((atom (cadar l))
             (unless (triplep (caar l) object (cadar l) source)
               (return nil)))
            ((some #'(lambda (x) (findinstancep x (cadar l) source))
                   (results (caar l) object source)))
            (t (return nil)))))

(defmethod findinstance (structure (source agent))
  (cond ((every #'(lambda (pair) (null (cdr pair))) (cddr structure))
         (instance (cadr structure) source))
        (t (genfindinstance structure source))))

(defun genfindinstance (structure source)
  (do ((l (instances (cadr structure) source) (cdr l)) (answer))
      ((null l) nil)
      (do ((m (cddr structure) (cdr m)))
          ((null m) (setq answer (car l)))
          (cond ((null (cdar m)))
                         ((atom (cadar m))
                          (unless (triplep (caar m) (car l) (cadar m) source)
                            (return nil)))
                         ((some #'(lambda (x) (findinstancep x (cadar m) source))
                                (results (caar m) (car l) source)))
                         (t (return nil))))
      (when answer (return answer))))

(defmethod findinstances (structure (source agent))
  (cond ((emptystructurep structure) (instances (cadr structure) source))
        (t (genfindinstances structure source))))

(defun emptystructurep (structure)
  (every #'(lambda (pair) (null (cdr pair))) (cddr structure)))

(defun genfindinstances (structure source)
  (do ((l (instances (cadr structure) source) (cdr l)) (nl))
      ((null l) (nreverse nl))
      (do ((m (cddr structure) (cdr m)))
          ((null m) (setq nl (cons (car l) nl)))
          (cond ((null (cdar m)))
                ((atom (cadar m))
                 (unless (triplep (caar m) (car l) (cadar m) source)
                   (return nil)))
                ((some #'(lambda (x) (findinstancep x (cadar m) source))
                       (results (caar m) (car l) source)))
                (t (return nil))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod findoptions (attribute structure (source agent))
  (cond ((every #'(lambda (pair) (null (cdr pair))) (cddr structure))
         (options attribute (cadr structure) source))
        (t (genfindoptions attribute structure source))))

(defun genfindoptions (slot structure source)
  (do ((l (findinstances structure source) (cdr l)) (nl))
      ((null l) (nreverse nl))
      (do ((m (results slot (car l) source) (cdr m)))
          ((null m))
          (setq nl (adjoin (car m) nl :test #'equalp)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Class Commands
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod instancep (object class (source agent))
  (doublep (find-predicate class) object source))

(defmethod classify (x (source agent))
  (let (dum)
    (cond ((characterp x) 'character)
          ((stringp x) 'string)
          ((numberp x) 'number)
          ((setq dum (find-classifier (name source)))
           (or (result dum x source) 'thing))
          (t 'thing))))

(defmethod instance (class (source agent))
  (itemx (find-predicate class) source))

(defmethod instances (class (source agent))
  (items (find-predicate class) source))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod doublep (predicate object (source agent))
  (findp (list predicate object) source))

(defmethod itemx (predicate (source agent))
  (findx '?x (list predicate '?x) source))

(defmethod items (predicate (source agent))
  (finds '?x (list predicate '?x) source))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod triplep (slot object result (source agent))
  (findp (list slot object result) source))

(defmethod object (slot value (source agent))
  (findx '?x (list slot '?x value) source))

(defmethod objects (slot value (source agent))
  (findx '?x (list slot '?x value) source))

(defmethod result (slot object (source agent))
  (findx '?y (list slot object '?y) source))

(defmethod results (slot object (source agent))
  (finds '?y (list slot object '?y) source))

(defmethod options (slot class (source agent))
  (genoptions slot class source))

(defun genoptions (slot class source)
  (do ((l (instances class source) (cdr l)) (nl))
      ((null l) (nreverse nl))
      (do ((m (results slot (car l) source) (cdr m)))
          ((null m))
          (setq nl (adjoin (car m) nl :test #'equalp)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Lists
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Inference Commands
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod assume (p (source list))
  (declare (ignore p source))
  nil)

(defmethod forget (p (source list))
  (declare (ignore p source))
  nil)

(defmethod findp (p (source list))
  (viewfindp p source))

(defmethod findx (x p (source list))
  (viewfindx x p source))

(defmethod finds (x p (source list))
  (viewfinds x p source))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Structure Commands
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod findinstancep (object structure (source list))
  (cond ((eq (cadr structure) 'number) (numberfindinstancep object structure))
        ((eq (cadr structure) 'string) (stringfindinstancep object structure))
        (t (genfindinstancep object structure source))))

(defmethod findinstance (structure (source list))
  (cond ((every #'(lambda (pair) (null (cdr pair))) (cddr structure))
         (instance (cadr structure) source))
        (t (genfindinstance structure source))))

(defmethod findinstances (structure (source list))
  (cond ((every #'(lambda (pair) (null (cdr pair))) (cddr structure))
         (instances (cadr structure) source))
        (t (genfindinstances structure source))))

(defmethod findoptions (attribute structure (source list))
  (cond ((every #'(lambda (pair) (null (cdr pair))) (cddr structure))
         (options attribute (cadr structure) source))
        (t (genfindoptions attribute structure source))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Class Commands
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod instancep (object class (source list))
  (doublep (find-predicate class) object source))

(defmethod classify (x (source list))
  (cond ((characterp x) 'character)
        ((stringp x) 'string)
        ((numberp x) 'number)
        ((getclass x source))
        ((ruleclass x source source))
        (t 'thing)))

(defmethod instance (class (source list))
  (itemx (find-predicate class) source))

(defmethod instances (class (source list))
  (items (find-predicate class) source))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod doublep (predicate object (source list))
  (findp (list predicate object) source))

(defmethod itemx (predicate (source list))
  (findx '?x (list predicate '?x) source))

(defmethod items (predicate (source list))
  (finds '?x (list predicate '?x) source))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod triplep (slot object value (source list))
  (findp (list slot object value) source))

(defmethod object (slot value (source list))
  (findx '?x (list slot '?x value) source))

(defmethod objects (slot value (source list))
  (finds '?x (list slot '?x value) source))

(defmethod result (slot object (source list))
  (findx '?y (list slot object '?y) source))

(defmethod results (slot object (source list))
  (finds '?y (list slot object '?y) source))

(defmethod options (slot class (source list))
  (genoptions slot class source))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Miscellaneous Stuff
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Subroutines
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun transclass (x th)
  (cond ((getclass x th))
        (t (do ((l (includees th) (cdr l)) (dum))
               ((null l) nil)
               (when (setq dum (transclass x (car l))) (return dum))))))

(defun getclass (x th)
  (do ((l (indexees x th) (cdr l)) (class))
      ((null l))
    (cond ((atom (car l)))
          ((and (eq (caar l) 'isa) (eq (cadar l) x)) (return (caddar l)))
          ((and (eq (cadar l) x) (null (cddar l))
                (setq class (findx '?c `(predicate ?c ,(caar l)) *manager*)))
           (return class)))))

(defun ruleclass (x data rules)
  (do ((l (contents rules) (cdr l)) (class))
      ((null l) nil)
      (when (and (listp (car l)) (eq (caar l) '<=)
                 (listp (cadar l)) (null (cddr (cadar l)))
                 (setq class (findx '?c `(predicate ?c ,(caadar l)) *manager*))
                 (findp (list (caadar l) x) data))
        (return class))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun fastconverter (structure)
  (fastconvert (car structure) (cadr structure) (cddr structure)))

(defmethod fastconvert (object class constraints)
  (reconverter (list* object class constraints)))

(defun reconverter (x)
  (nreverse (reconvertertree x nil)))

(defun reconvertertree (x nl)
  (cond ((atom x) nl)
        (t (setq nl (reconverterslots x nl))
           (adjoiner (makpred (car x) (cadr x) *gui*) nl))))

(defun reconverterslots (x nl)
  (do ((l (cddr x) (cdr l)) (var) (ans))
      ((null l) nl)
    (cond ((null (cdar l)))
          ((fullfindp `(searchstyle ,(caar l) text) *manager*)
           (setq var (genvar))
           (setq nl (adjoiner `(and (,(caar l) ,(car x) ,var) (substring ,(cadar l) ,var)) nl)))
          ((eq (cadar l) 'any)
           (setq nl (adjoiner (list (caar l) (car x) '?*) nl)))
          ((atom (cadar l))
           (setq nl (adjoiner (list (caar l) (car x) (cadar l)) nl)))
          ((eq (caadar l) 'oneof)
           (setq var (genvar))
           (setq nl (adjoiner `(and ,(list (caar l) (car x) var) (oneof ,var . ,(cdadar l))) nl)))
          ((eq (caadar l) 'between)
           (setq ans (makcomparator (car x) (caar l) (cadr (cadar l)) (caddr (cadar l)) '=<))
           (if ans (setq nl (adjoiner ans nl))))
          ((eq (caadar l) 'substring)
           (setq var (genvar))
           (setq nl (adjoiner `(and (,(caar l) ,(car x) ,var) (substring ,(cadar (cdar l)) ,var)) nl)))
          ((eq (caadar l) 'taxonomy)
           (setq nl (adjoiner (raze (caar l) (car x) (car (last (cadar l))) (cdadar l)) nl)))
          (t (setq nl (adjoiner (list (caar l) (car x) (caadar l)) nl))
             (setq nl (reconvertertree (cadar l) nl))))))

(defun adjoiner (x l)
  (adjoin x l :test #'equalp))

(defun raze (slot aspect value references)
  (let (aporels)
    (setq aporels (finds '?x `(expander ,slot ?x) *manager*))
    (do ((l references (cdr l)))
        ((null l))
        (if (equalp (car l) value) (return t) (setq aporels (cdr aporels))))
    (cond ((null aporels) (list slot aspect value))
          (t (do ((l (cdr aporels) (cdr l)) (nl (list (list (car aporels) (genvar) value))))
                 ((null l) (maksand (nreverse (cons (list slot aspect (cadar nl))  nl))))
                 (setq nl (cons (list (car l) (genvar) (cadar nl)) nl)))))))

(defun makcomparator (aspect slot min max op)
  (let (var)
    (cond ((and min max)
           (setq var (genvar))
           `(and (,slot ,aspect ,var) (,op ,min ,var) (,op ,var ,max)))
          (min
           (setq var (genvar))
           `(and (,slot ,aspect ,var) (,op ,min ,var)))
          (max
           (setq var (genvar))
           `(and (,slot ,aspect ,var) (,op ,var ,max))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Metadata Management
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; extensionalize, unextensionalize, extensionists, extensions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod extensionalize (r th)
  (save `(extension ,(name th) ,r) *manager*)
  'done)

(defmethod unextensionalize (r th)
  (drop `(extension ,(name th) ,r) *manager*)
  'done)

(defmethod extensionists (r)
  (fastfinds '?a `(extension ?a ,r) *manager*))

(defmethod extensions (th)
  (fastfinds '?r `(extension ,(name th) ?r) *manager*))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod publish (r th)
  (save `(specialty ,(name th) ,r) *manager*)
  'done)

(defmethod unpublish (r th)
  (save `(specialty ,(name th) ,r) *manager*)
  'done)

(defmethod publishers (r)
  (fastfinds '?a `(specialty ?a ,r) *manager*))

(defmethod publications (th)
  (fastfinds '?r `(specialty ,(name th) ?r) *manager*))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod subscribe (r th)
  (save `(interest ,(name th) ,r) *manager*)
  'done)

(defmethod unsubscribe (r th)
  (save `(interest ,(name th) ,r) *manager*)
  'done)

(defmethod subscribers (r)
  (fastfinds '?a `(interest ?a ,r) *manager*))

(defmethod subscriptions (th)
  (fastfinds '?r `(interest ,(name th) ?r) *manager*))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod maintain (r th)
  (save `(material ,(name th) ,r) *manager*)
  'done)

(defmethod unmaintain (r th)
  (drop `(material ,(name th) ,r) *manager*)
  'done)

(defmethod maintainers (r)
  (fastfinds '?a `(material ?a ,r) *manager*))

(defmethod maintainees (th)
  (fastfinds '?r `(material ,(name th) ?r) *manager*))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Objects with names
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun hostname ()
  (string-upcase (long-site-name)))

(defun universalize (x)
  (cond ((stringp x) x)
        ((symbolp x) (setq x (symbol-name x)))
        (t (setq x (princ-to-string x))))
  (intern (concatenate 'string x "@" (hostname))))

(defmethod name (x)
  (declare (ignore x))
  'anonymous)

(defmethod name ((x symbol)) x)

(defmethod name ((x number)) x)

(defmethod name ((x character)) x)

(defmethod name ((x string)) x)

(defun namep (x) (and (symbolp x) (boundp x) (objectp (symbol-value x))))

(defmethod referent (x) x)

(defmethod referent ((x symbol))
  (if (boundp x) (symbol-value x) x))

(defun objectp (x) (typep x 'object))

(defclass object ()
  ((name :initarg :name :accessor name)))

(defmethod initialize-instance ((object object) &rest inits)
  (apply #'call-next-method object inits)
  (set (name object) object))

(defmethod obliterate ((object object))
  (makunbound (name object))
  object)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Dates
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defparameter *months*
  '(january february march april may june
    july august september october november december))

(defparameter *dates*
  '(1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20 21 22 23 24 25 26 27 28 29 30 31))

(defparameter *month-names*
  '(january february march april may june
    july august september october november december))

(defparameter *short-month-names*
  '(jan feb mar apr may jun jul aug sep oct nov dec))

(defparameter *day-names*
  '(monday tuesday wednesday thursday friday saturday sunday))

(defparameter *days-in-month*
  '(31 28 31 30 31 30 31 31 30 31 30 31))

(defparameter *short-day-names*
  '(mon tue wed thu fri sat sun))

(defun datep (x)
  (and (symbolp x)
       (= (length (setq x (symbol-name x))) 10)
       (digitp (elt x 0))
       (digitp (elt x 1))
       (digitp (elt x 2))
       (digitp (elt x 3))
       (char= (elt x 4) #\-)
       (digitp (elt x 5))
       (digitp (elt x 6))
       (char= (elt x 7) #\-)
       (digitp (elt x 8))
       (digitp (elt x 9))))

(defun makedate (year month day)
  (intern (format nil "~D-~2,'0D-~2,'0D" year month day)))

(defun year (x) 
  (values (read-from-string (stringsubseq (symbol-name x) 1 4)))) 

(defun month (x) 
  (values (read-from-string (stringsubseq (symbol-name x) 6 7)))) 

(defun day (x) 
   (values (read-from-string (stringsubseq (symbol-name x) 9 10))))


(defgeneric date-plus (month date year count interval))

(defmethod date-plus (month date year count interval)
  (declare (ignore interval))
  (date-plus month date year count 'day))

(defmethod date-plus (month date year count (interval (eql 'day)))
  (setq date (+ date count))
  (loop
      for days-in-month = (days-in month year)
      while (> date days-in-month)
      do (setq date (- date days-in-month))
	 (incf month)
	 (when (> month 12)
	   (setq month 1)
	   (incf year) ))
  (values month date year))

(defmethod date-plus (month date year count (interval (eql 'week)))
  (date-plus month date year (* 7 count) 'day) )

(defmethod date-plus (month date year count (interval (eql 'biweek)))
  (date-plus month date year (* 14 count) 'day) )

(defmethod date-plus (month date year count (interval (eql 'month)))
  (setq month (+ month count))
  (multiple-value-bind (delta-year new-month) (truncate month 12)
    (when (= new-month 0)
      (setq new-month 12)
      (decf delta-year) )
    (setq year (+ year delta-year))
    (setq month new-month))
  (values month date year))


(defun today ()
  (multiple-value-bind
    (second minute hour date month year day-of-week dst zone)
    (get-decoded-time)
    (declare (ignore second minute hour day-of-week dst zone))
    (makedate year month date)))

(defun lastmonday (date)
  (let (year month day dow)
    (setq year (year date))
    (setq month (month date))
    (setq day (day date))
    (setq dow (day-of month day year))
    (setq day (- day dow))
    (when (< day 1)
      (decf month)
      (when (< month 1)
	(setq month 12)
	(decf year))
      (setq day (+ (days-in month year) day)))
    (makedate year month day)))
#|
(defun nextmonday (&optional (time (get-universal-time)))
  (multiple-value-bind
    (second minute hour date month year day-of-week dst zone)
    (decode-universal-time time)
    (declare (ignore second minute hour dst zone))
    (let ((days-ahead (mod (- 7 day-of-week) 7))
	  (days-in-month (days-in month year)) )
      (setq date (+ date days-ahead))
      (when (> date days-in-month)
	(setq date (- date days-in-month))
	(incf month) )
      (when (> month 12)
	(setq month 1)
	(incf year) ))
    (makedate year month date)))
|#
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Times
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun hour (time) 
  (read-from-string (stringsubseq (symbol-name time) 1 2))) 

(defun minute (time) 
  (read-from-string (stringsubseq (symbol-name time) 4 5))) 

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Old dates
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun makdate (month date year)
  (intern (format nil "~2,'0D/~2,'0D/~4,'0D" month date year)))

(defun olddatetonewdate (date)
  (makedate (oldyear date) (oldmonth date) (oldday date)))

(defun oldmonth (x)
  (let (pos)
    (setq pos (position #\/ (symbol-name x)))
    (read-from-string (stringsubseq (symbol-name x) 1 pos))))

(defun oldday (x) 
  (let (beg end)
    (setq beg (position #\/ (symbol-name x)))
    (setq end (position #\/ (symbol-name x) :from-end t))
    (read-from-string (stringsubseq (symbol-name x) (+ beg 2) end))))

(defun oldyear (x)
  (let (pos)
    (setq pos (position #\/ (symbol-name x) :from-end t))
    (read-from-string (stringsubseq (symbol-name x) (+ pos 2) (length (symbol-name x))))))

(defun parsedate (input)
  (let (slash1 slash2 part1 part2 part3)
    (setq input (remove #\| input))
    (setq slash1 (or (position #\/ input) (position #\- input)))
    (setq slash2
      (or (position #\/ input :from-end t) (position #\- input :from-end t)))
    (unless slash1 (return-from parsedate nil) )
    ;; Separate parts
    (setq part1 (subseq input 0 slash1))
    (cond ((and slash2 (< slash1 slash2))
	   (setq part2 (subseq input (1+ slash1) slash2))
	   (setq part3 (subseq input (1+ slash2))))
          (t (setq part2 (subseq input (1+ slash1)))
	     (setq part3 (format nil "~D" (current :year)))))
    ;; String -> Integer
    (if (or (string= part1 "") (find-if-not #'digit-char-p part1) )
	(setq part1 (current :month))
        (setq part1 (parse-integer part1)) )
    (if (or (string= part2 "") (find-if-not #'digit-char-p part2) )
	(setq part2 (current :date))
        (setq part2 (parse-integer part2)) )
    (if (or (string= part3 "") (find-if-not #'digit-char-p part3) )
	(setq part3 (current :year))
        (setq part3 (parse-integer part3)) )
    ;; Year abbreviation
    (cond ((< part3 50) (setq part3 (+ part3 2000)))
          ((< part3 100) (setq part3 (+ part3 1900))))    
    ;; Bounds checking
    (when (or (< part1 1) (> part1 12)) (setq part1 (current :month)))
    (when (or (< part2 1) (> part2 31)) (setq part2 (current :date)))
    (when (or (< part3 1900) (> part3 2100)) (setq part3 (current :year)))
    (intern (format nil "~2,'0D/~2,'0D/~4,'0D" part1 part2 part3))))

(defun parseminute (s)
  (intern (strappend "MINUTE." (princ-to-string (parsetime s)))))

(defun parsetime (s)
  (let (old pos hour minute)
    (setq s (remove-if #'whitep s) old 0)
    (cond ((setq pos (position #\: s))
           (setq hour (parseinteger s 0 pos) old (1+ pos))
           (cond ((setq pos (findnondigit s old))
                  (setq minute (parseinteger s old pos) old pos))
                 (t (setq minute 0 old nil))))
          ((setq pos (findnondigit s old))
           (setq hour (parseinteger s old pos) old pos minute 0))
          (t (setq hour 0 old nil minute 0)))
    (when (and old (<= hour 12) (find (elt s old) '(#\p #\P)))
      (setq hour (+ hour 12)))
    (+ (* hour 60) minute)))

(defun parseinteger (s start end)
  (cond ((= start end) 0)
        (t (parse-integer s :start start :end end))))

(defun findnondigit (s old)
  (do ((i old (1+ i)) (n (length s)))
      ((= i n) nil)
      (when (not (digitp (elt s i))) (return i))))

(defun prettifytime (time)
  (let ((hour (floor time 60)) (minute (mod time 60)))
    (cond ((<= hour 12) (format nil "~D:~2,'0D am" hour minute))
          (t (format nil "~D:~2,'0D pm" (- hour 12) minute)))))

(defun whitep (char)
  (find char '(#\space #\tab #\return #\linefeed)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Leftovers
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun dateint-plus (dateint count &optional (interval 'day)) 
  (let ((month (month dateint)) 
    (date (day dateint)) 
    (year (year dateint)) ) 
    (multiple-value-bind (newmonth newdate newyear) 
      (date-plus month date year count interval) 
      (makedate newyear newmonth newdate))))

(defun cookietime (n)
  (multiple-value-bind
    (second minute hour date month year dow dst zone)
    (decode-universal-time n 0)
    (declare (ignore zone dst))
    (format nil "~:(~A~), ~D-~:(~A~)-~D ~2,'0D:~2,'0D:~2,'0D GMT"
	    (nth dow *day-names*) date (nth (1- month) *short-month-names*) year
            hour minute second)))

(defun current-time (&optional (universal-time (get-universal-time)))
  (multiple-value-bind
    (second minute hour date month year dow dst zone)
    (decode-universal-time universal-time)
    (declare (ignore second dow dst zone))
    (format nil "~:(~A~) ~D, ~D at ~A"
	    (nth (1- month) *month-names*) date year
	    (time-string hour minute))))

(defun current-day ()
  (multiple-value-bind
    (second minute hour date month year dow dst zone)
    (get-decoded-time)
    (declare (ignore second minute hour date month year dst zone))
    (format nil "~(~A~)" (nth dow *day-names*))))

(defun current (kind)
  (declare (type symbol kind))
  (multiple-value-bind
    (second minute hour date month year dow dst zone)
    (get-decoded-time)
    (case kind
      (:second second)
      (:minute minute)
      (:hour hour)
      (:date date)
      (:month month)
      (:year year)
      (:day-of-week dow)
      (:daylight-savings-time? dst)
      (:time-zone zone))))

(defun leap-year? (year)
  (declare (type integer year))
  (if (not (zerop (rem year 100)))
      (zerop (rem year 4))
      (zerop (rem year 400))))

(defun seconds-until (hour minute)
  "Returns seconds from now until then"
  (declare (type (integer 0 23) hour))
  (declare (type (integer 0 59) minute))
  (multiple-value-bind
    (now-second now-minute now-hour date month year dow dst zone)
    (get-decoded-time)
    (declare (ignore date month year dow dst zone) )
    (mod (+ (* 60 60 (- hour now-hour))
	    (* 60 (- minute now-minute))
	    (- now-second) )
         (* 24 60 60))))

(defun time-string (hour minute)
  (declare (type (integer 0 23) hour))
  (declare (type (integer 0 59) minute))
  (cond ((= hour 12) (format nil "12:~2,'0D pm" minute))
        ((or (= hour 24) (= hour 0)) (format nil "12:~2,'0D am" minute))
        ((< hour 12) (format nil "~D:~2,'0D am" hour minute))
        (t (format nil "~D:~2,'0D pm" (- hour 12) minute))))

(defun days-in (month &optional (year nil))
  (if (and year (= month 2) (leap-year? year)) 29
      (nth (1- month) *days-in-month*)))

(defun day-of (month date year)
  "Returns 0..6 = mon..sun of specified date"
  (let ((ut (encode-universal-time 0 0 0 date month year)))
    (multiple-value-bind
      (second minute hour date month year dow dst zone)
      (decode-universal-time ut)
      (declare (ignore second minute hour date month year dst zone))
      dow)))

(defun datelessp (x y)
  (setq x (symbol-name x) y (symbol-name y))
  (cond ((string-lessp x y :start1 6 :start2 6))
        ((string-lessp y x :start1 6 :start2 6) nil)
        ((string-lessp x y :end1 5 :end2 5))))

(defun deconstructdate (x)
  (setq x (symbol-name x))
  (values (elt *months* (1- (read-from-string (subseq x 0 2))))
          (read-from-string (subseq x 3 5))
          (read-from-string (subseq x 6))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; manager stuff
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun find-arity (relation)
  (findx '?x `(arity ,relation ?x) *manager*))

(defun find-keys (relation)
  (finds '?x `(key ,relation ?x) *manager*))

(defun find-columns (relation)
  (finds '?x `(column ,relation ?x) *manager*))

(defun find-subrelations (relation)
  (finds '?c `(superrelation ?c ,relation) *manager*))

(defun columns (relation)
  (finds '?x `(column ,relation ?x) *manager*))


(defun find-domain (attribute)
  (findx '?v `(domain ,attribute ?v) *manager*))

(defun find-domains (attribute)
  (finds '?v `(domain ,attribute ?v) *manager*))

(defun find-range (attribute)
  (findx '?v `(range ,attribute ?v) *manager*))

(defun uniquep (attribute)
  (findp `(unique ,attribute yes) *manager*))


(defun find-predicate (class)
  (do ((l (indexees class *manager*) (cdr l)))
      ((null l) (findx '?r `(predicate ,class ?r) *manager*))
      (cond ((atom (car l)))
            ((and (eq (caar l) 'predicate) (eq (cadar l) class))
             (return (caddar l))))))

(defun find-attributes (class)
  (finds '?s `(attribute ,class ?s) *manager*))

(defun find-subclass (class)
  (findx '?c `(superclass ?c ,class) *manager*))

(defun find-subclasses (class)
  (finds '?c `(superclass ?c ,class) *manager*))

(defun slots-of-class (class)
  (finds '?s `(attribute ,class ?s) *manager*))

(defun attributes (class)
  (finds '?s `(attribute ,class ?s) *manager*))



(defun find-recipient (agent)
  (findx '?x `(recipient ,agent ?x) *manager*))

(defun find-classifier (agent)
  (findx '?r `(classifier ,agent ?r) *manager*))

(defun find-target (x)
  (findx '?x `(recipient ,(name x) ?x) *manager*))

(defun find-targets (agent)
  (finds '?x `(recipient ,agent ?x) *manager*))

(defun find-rulebase (agent)
  (findx '?x `(rulebase ,(name agent) ?x) *manager*)) 

(defun find-database (agent)
  (findx '?x `(database ,(name agent) ?x) *manager*))

(defun find-includee (agent)
  (findx '?x `(includee ,(name agent) ?x) *manager*))



(defun get-database (agent)
  (symbol-value (find-recipient (name agent))))

(defun get-rulebase (agent)
  (symbol-value (find-rulebase (name agent))))

(defun get-recipient (agent)
  (symbol-value (find-recipient (name agent))))

(defun get-includee (agent)
  (symbol-value (find-includee (name agent))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; miscellaneous
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun specvars (n)
  (do ((i 1 (1+ i)) (nl))
      ((> i n) (nreverse nl))
      (setq nl (cons (specvar i) nl))))

(defun specvar (x)
  (intern (format nil "?~A" x)))

(defun genvar (&optional (num nil))
  (let (pname)
    (unless num (setq num (1- (incf *var-count*))))
    (setq pname (string-upcase (format nil "~26R" num)))
    (setq pname (map 'string #'radix-to-letters pname))
    (intern (format nil "?_~A" pname))))

(defun table-var ()
  (intern (format nil "?_~A" (incf *var-count*))))

(defun uservar ()
  (intern (format nil "?V~A" (incf *var-count*))))

(defun radix-to-letters (chr)
  (declare (type character chr))
  (let ((code (char-code chr)))
    (if (<= code (char-code #\9))
	(code-char (+ (char-code #\A) (- code (char-code #\0))))
      (code-char (+ code 10)) )))

(defun makpred (obj class agent)
  (let (relation)
    (cond ((setq relation (find-predicate class)) (list relation obj))
          (t (makisa obj class agent)))))

(defun makisa (obj class agent)
  (list (or (find-classifier (name agent)) 'isa) obj class))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
