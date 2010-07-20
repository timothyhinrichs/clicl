(in-package :tlh)

;;;;;;;;;;;; Logic ;;;;;;;;;;;;;


(defun stdize-apart (p q)
   "(STDIZE-APART P Q) returns a version of p s.t. none of the variables
    in the new p are also in q."
   (let* ((pvars (findvars p))
          (qvars (findvars q))
          (pqvars (intersection pvars qvars)))
     (do ((vs pqvars (cdr vs)))
         ((null vs) p)
       (nsubst (newtmpvar) (car vs) p) ))
    p
)
(defun findvars (p)
   "(FINDVARS P) walks down the tree-structure of p and returns
    a list of all the variables there."
   (cond ((and (atom p) (varp p)) (list p))
         ((atom p) nil)
         (t 
           (do ((ps p (cdr ps))
                (allvars nil))
               ((null ps) allvars)
             (setq allvars (union allvars (findvars (car ps)))) )))
)
(defun newtmpvar ()
   "(NEWTMPVAR) generates a new variable with gentemp."
   (gentemp "?")
)
(defun varp (p)
   "(VARP P) returns T iff p is a variable, i.e. starts with ?"
   (and (symbolp p)
        (eql #\? (char (symbol-name p) 0))) 
)


(defun pos-and-neg (lit1 lit2)
  "(POS-AND-NEG LIT1 LIT2) returns T iff lit1 is negative and lit2 is positive or
   vice versa."
  (cond ((atom lit1)
         (cond ((atom lit2) nil)
               ((eq (car lit2) 'not) t)
               (t nil)))
        ((atom lit2)
         (cond ((eq (car lit1) 'not) t)
               (t nil)))
        ((or
          (and (negativep lit1) (not (negativep lit2)))
          (and (negativep lit2) (not (negativep lit1))))
         t)
        (t nil))
)
(defun negativep (lit)
  "(NEGATIVEP LIT) returns T iff lit is negative."
  (cond ((atom lit) nil)
        ((eq (car lit) 'not) t)
        (t nil))
)
(defun relation (lit)
  "(RELATION LIT) returns the relation constant in LIT."
  (cond ((atom lit) lit)
        ((eq (car lit) 'not)
         (relation (cadr lit)))
        (t (car lit)))
)
(defun signed-relation (lit)
  "(SIGNED-RELATION LIT) returns the relation surrounded by not if literal is negative.  
   Otherwise just returns relation."
  (cond ((atom lit) lit)
        ((eq (car lit) 'not) (list 'not (relation lit)))
        (t (relation lit)))
)
(defun strip-ors (clauses)
   "(STRIP-ORS CLAUSES) removes all the or's from the CLAUSES."
  (mapcar #'(lambda (x) (if (listp x) (cdr x) x)) clauses)
)

(defun positive-literals (clause)
   "(POSITIVE-LITERALS CLAUSE) returns all the positive literals out
    of CLAUSE."
    (remove-if #'(lambda (x) (and (listp x) (eq (car x) 'not))) clause) 
)
(defun negative-literals (clause)
   "(NEGATIVE-LITERALS CLAUSE) returns all the negative literals out
    of CLAUSE."
    (remove-if-not #'(lambda (x) (and (listp x) (eq (car x) 'not))) clause) 
)

(defun maknot(lit)
   "(MAKNOT LIT) returns the negated form of LIT."
   (cond ((atom lit) (list 'not lit))
         ((and (listp lit) (eq (car lit) 'not)) (cadr lit))
         ((listp lit) (list 'not lit))
         (t nil))
)
(defun maksand (l)
  (cond ((atom l) (list 'and l))
        (t (cons 'and l)))
)
(defun maksor (l)
  (cond ((atom l) (list 'or l))
        (t (cons 'or l)))
)
(defun makset (l)
  (cond ((atom l) (list l))
        ((eq (car l) 'or) (cdr l))
        ((eq (car l) 'cons_or) (cdr l))
        (t l))
)
(defun success (&rest r)
  (declare (ignore r))
  t
)
(defun failure (&rest r)
  (declare (ignore r))
  nil
)

(defun strip-or (clause)
  "(STRIP-OR CLAUSE) returns CLAUSE without the initial or, if it has one."
  (cond ((atom clause) clause)
        ((eq (car clause) 'or) (cdr clause))
        (t clause))
)
(defun flatten-ors (clause)
  "(FLATTEN-ORS CLAUSE) turns (or ... (or p q r) ...) into (or ... p q r ...)"

  (let ((result (make-queue)) m)
    (queue-enqueue result 'or)
    (dolist (d (cdr clause))
      (cond ((func-typep d 'or)
             (setq m (flatten-ors d))
             (queue-append result m))
            (t
             (queue-enqueue result d))))
    (queue-head result))
)

