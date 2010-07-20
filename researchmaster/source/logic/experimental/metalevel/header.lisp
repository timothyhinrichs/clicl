
(in-package :tlh)

(defvar *indexing* t
  "Whether to use indexing.")

(defvar *trfirst* nil 
  "whether to first remove trs from clauses before resolving on other relations.")
(defvar *cspslast* nil
  "whether to resolve on the *csp-relations* last or not.")
(defvar *tr* t
  "whether to do resolutions within positive tr literals")
(defvar *subsumption* t
  "whether to use subsumption elimination.")
(defvar *tautology-elim* t
  "tautology-elimination flag")
(defvar *factoring* t
  "Whether to use factoring.")
(defvar *complex-empty-clause* nil
  "Attaching to determine whether clause of cspslast is unsat.")
(defvar *demote* nil
  "Whether to use the demotion rule of inference on tr literals.")
(defvar *attachments* t
  "Whether to use procedural attachments.")
(defvar *tr-disjunctions* nil
  "Whether to introduce disjunctions when metaunifying.")
(defvar *ground-tr* nil
  "Whether apply metaresolution only to the ground tr literals or not.")
(defvar *mrgmgu* nil
  "Whether to use Mike's unify." )


(defstruct queue (head nil) (tail nil))
(defun queue-append (q l)
  "(QUEUE-APPEND QUEUE L) appends L to the tail of Q."
  (cond ((queue-head q)
         (setf (cdr (queue-tail q)) l)
         (setf (queue-tail q) (last (queue-tail q))))
        (t
         (setf (queue-head q) l)
         (setf (queue-tail q) (last l))))
  (queue-head q)
)
(defun queue-enqueue (q d)
  "(QUEUE-ENQUEUE Q D) enqueues d at the end of the Q."
  (cond ((queue-head q)
         (setf (cdr (queue-tail q)) (list d))
         (setf (queue-tail q) (last (queue-tail q))))
        (t
         (setf (queue-head q) (list d))
         (setf (queue-tail q) (last (queue-head q)))))
  (queue-head q)
)  
(defun queue-adjoin (q d &key (test #'eq))
  "(QUEUE-ENQUEUE Q D) enqueues d at the end of the Q."
  (when (not (member d (queue-head q) :test test))
    (cond ((queue-head q)
           (setf (cdr (queue-tail q)) (list d))
           (setf (queue-tail q) (last (queue-tail q))))
          (t
           (setf (queue-head q) (list d))
           (setf (queue-tail q) (last (queue-head q))))) )
  (queue-head q)
)
 
(defun queue-union (q l &key (test #'eq))
  "(QUEUE-UNION Q L TEST) desctructively unions queue Q and list
   L."
  (mapcar #'(lambda (x) (queue-adjoin q x :test test)) l)
)