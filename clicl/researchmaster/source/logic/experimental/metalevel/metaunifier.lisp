

;;; mgu-trexp


(in-package :tlh)

;;; mgu
#|
(defun mgu (x y)
  "(MGU X Y) returns a most general unifier for the expressions x and y, taking into
   account the literal tr."
  (cond ((and (listp x) (eq (car x) 'tr))
         (cond ((and (listp y) (eq (car y) 'tr)) (mgu-tr2 (cadr x) (cadr y)) )
               (t
                (mgu-trexp (cadr x) y))) )
        ((and (listp y) (eq (car y) 'tr)) (mgu-trexp (cadr y) x))
        (t (mgu-norm x y)) )
)
|#
(defun metamgu (x y)
  "(MGU X Y) returns a most general unifier for the expressions x and y, taking into
   account the literal tr."
  (cond ((and (listp x) (listp y) (eq (car x) 'tr) (eq (car y) 'tr))
         (mgu-tr2 (car x) (car y)))
        (t (mgu-norm x y)) )
)


;;;;;;; mgu-trexp ;;;;;;;;

(defun mgu-trexp (x y)
  "(MGU-TREXP X Y) takes a tr expression X and a normal expression Y and returns a unifier u
   such that xu = the quoted version of yu."
  (cond ((and (qatom x) (atom y) (metaequal x y)) (bind t t (new-bindinglist)))   ; x is equal to the quoted version of y
        ((varp x) (if (and (not (occurs x y)) (notquotedp y)) (bind x (quotify y) (new-bindinglist)) nil))
        ((varp y) (if (and (not (occurs y x)) (allquoted x)) (bind y (denotation x) (new-bindinglist)) nil))
        ((qvarp x) (if (and (not (occurs x y)) (notquotedp y)) (bind (unquote x) y (new-bindinglist)) nil))
        ((qatom y) nil)  ; can't make a quoted expression x contain quotes
        
        ; at this point, x is either a quoted object, a quoted function term, 
        ;    an object, or a function term, which is metaunequal to y
        ((or (atom y) (atom x)) nil)        
        ((or (qatom x) (qatom y)) nil)

        ; x is either a quoted function or a function
        ; y is a function
        ; only possible unification is a quoted function and a function
        ((not (eq (car x) 'listof)) nil)
        (t
         ; change to call mgu-trexp
         ; start x at (cdr x) to avoid listof
         (do ((xs (cdr x) (cdr xs))
              (ys y (cdr ys))
              (done nil)
              (tau nil)
              (sigma (bind t t (new-bindinglist))))
             ((or (null xs) (null ys) done) (if (and (eq xs ys) (not done)) sigma nil))
           (setq tau (mgu-trexp (car xs) (car ys)))
           (cond ((not tau) (setq done t))
                 (t
                  (setq xs (plug xs tau))
                  (setq ys (plug ys tau))
                  (setq sigma (compose sigma tau)))) )))
)
(defun denotation (x)
  "(DENOTATION X) returns the sentence described by x."
  (cond ((atom x) x)
        ((qatom x) (unquote x))
        ((eq (car x) 'listof) (mapcar #'denotation (cdr x)))
        (t x))
)
(defun quotify (x)
  "(QUOTIFY X) returns the quoted description of the sentence x"
  (cond ((atom x) (list 'quote x))
        (t (cons 'listof (mapcar #'quotify x))) )
)
(defun unquote-all (x)
  "(UNQUOTE-ALL X) returns the expression x with all quotes removed."
  (cond ((qatom x) (unquote x))
        ((and (listp x) (eq (car x) 'listof)) 
         (cons 'listof (mapcar #'unquote-all (cdr x))))
        (t
         x))
)
(defun metaequal (qx y)
  "(METAEQUAL QX Y) takes a quoted expression QX and a regular expression Y and determines
   whether X represents the expression Y."
  (cond ((atom qx) nil)  ; something is broken
        ((qatom qx) (eq (unquote qx) y))
        ((and (eq (car qx) 'listof) (listp y))
         (every #'metaequal (cdr qx) y))
        (t nil))
)
(defun notquotedp (exp)
  "(NOTQUOTEDP EXP) returns T iff exp is an ordinary expression, i.e. no quotes."
  (cond ((atom exp) t)
        ((qatom exp) nil)
        ((eq (car exp) 'listof) nil)
        (t (every #'notquotedp exp)) )
)

;;;;;;;;;;;;;;;;;  mgu-tr2  ;;;;;;;;;;;;;;;;

(defvar *metaunifications* 0)

(defun mgu-tr2-res (x y)
  "(MGU-TR2-RES X Y) returns a list of metaunifiers for arguments to two positive tr literals,
   including the case where those arguments represent disjunctions."

#|
  "<x> | ... | <y>" has two variables and 3 rewritings each, giving 6 total variants.
  Each variant has between 2 and 4 variables.  Each is similar to a factor--a D-factor--,
  for which cons_or disjunction resolution must be attempted.  So it looks like we have
  yet another layer between top and arg: mgu-tr2-disjunction.  

  This function will not only
  introduce all those disjunctions.  It will also switch off to the mgu-tr2-arg if 
  disjunctions are not warranted.  Introduce another fork in the branch: standardizing apart
  the variables in the two tr literals.  Of course we can't do this in all possible ways, so
  we must just require the description to not discriminate between variables, i.e. if p(x) is
  in the set, so must p(y) and p(z) and ....  And we also require LB have infinitely many variables.
|#

  (when (or (not *ground-tr*) (and *ground-tr* (groundp x) (groundp y)))
    ; turn everything into a disjunction that we can
    (when (non-disjunction x) (setq x (list 'or x)))
    (when (non-disjunction y) (setq y (list 'or y)))
    (if (and (func-typep x 'or) (func-typep y 'or))
      (mgu-tr2-all-disjunctions x y)
      nil) )
)
(defun non-disjunction (x) (or (varp x) (func-typep x 'listof) (func-typep x 'cons_not)))

(defun groundp (x)
  "(GROUNDP X) returns T iff x is ground."
  (cond ((atom x) (not (varp x)))
        ((eq 'quote (car x)))
	(t (every 'groundp x)))
)

; We should not be resolving on the z and w for the disjunction introduction z | y | w since the
;   result will always be subsumed by the two-place disjunction introduction.

(defun mgu-tr2-all-disjunctions (x y)
  "(MGU-TR2-ALL-DISJUNCTIONS X Y) returns all the unifiers when x and y are disjunctions in
   tr literals."

  (setq x (stdize-apart x y))
  (let ((xdisjuncts (all-disjuncts x))
        (ydisjuncts (all-disjuncts y))
        (mgus) (allmgus nil) )
    ; walk over every pair of disjunctions
    (dolist (d1 xdisjuncts)
      (dolist (d2 ydisjuncts)
        (setq mgus (mgu-tr2-disjunction (flatten-ors (plug (metaunifier-disjunction d1) (metaunifier-mgu d1)))
                                        (flatten-ors (plug (metaunifier-disjunction d2) (metaunifier-mgu d2)))))
        (setq mgus (mapcar #'(lambda (x) (make-metaunifier :mgu (compose (compose (metaunifier-mgu d2) (metaunifier-mgu d1))
                                                                         (metaunifier-mgu x))
                                                           :disjunction (metaunifier-disjunction x))) mgus))
        (setq allmgus (append mgus allmgus)))) 
    allmgus)
)



(defun all-disjuncts (orclause)
  "(ALL-DISJUNCTS ORCLASUE) returns all the disjuncts for the or-clause ORCLAUSE including
   itself, i.e. for each variable x at the toplevel of ORCLAUSE, generate
   (1) <y> | <z>, (2) <y> | <z> | <w>, (3) x."

  (if (not *tr-disjunctions*)
    (list (make-metaunifier :disjunction orclause))
    (let ((result (list (make-metaunifier :disjunction orclause))))
      (do ((pre nil)    ; clause before d
           (d orclause (cdr d))
           n2 n3)   ; iterator over orclause
          ((null d) result)
        (when (varp (car d))
          (setq n2 (list (newtmpvar) (newtmpvar)))
          (setq n3 (list (newtmpvar) (newtmpvar) (newtmpvar)))
          (setq result (append (list (make-metaunifier :mgu (bind (car d) (cons 'or n3) (new-bindinglist))
                                                       :disjunction (append pre n3 (cdr d))) 
                                     (make-metaunifier :mgu (bind (car d) (cons 'or n2) (new-bindinglist)) 
                                                       :disjunction (append pre n2 (cdr d))))
                               result)))
        (setq pre (append pre (list (car d)))) )))
)
(defun mgu-tr2-disjunction (x y)
  "(MGU-TR2-DISJUNCTION X Y) returns a list of metaunifiers where x and y are n-ary cons_or
   literals.  Assume they have been standardized apart."
  (let ((c1 (cdr x)) (c2 (cdr y)))   ; c1 is the first clause and c2 the second
    (do ((lit1 c1 (cdr lit1))
         (resolvents nil))
        ((null lit1) resolvents)
      (do ((lit2 c2 (cdr lit2))
           (resolvent) (contrib1) (contrib2) (mgu) (newc) )
          ((null lit2))
        
        ; compute the resolvent for these two disjuncts
        (setq mgu (mgu-tr2-arg (car lit1) (car lit2)))
        (when mgu
          (setq contrib1 (plug (remove (car lit1) c1 :test #'equal) (metaunifier-mgu mgu)))
          (setq contrib2 (plug (remove (car lit2) c2 :test #'equal) (metaunifier-mgu mgu)))
          (setq newc (union contrib1 contrib2 :test #'equal))
          (setq resolvent (make-metaunifier :disjunction 
                                            (if newc    ; acccount for the empty tr clause
                                              (cons (maktr (cons 'or newc)) (metaunifier-disjunction mgu))
                                                (metaunifier-disjunction mgu))
                                            :mgu (metaunifier-mgu mgu)))
          (setq resolvents (cons resolvent resolvents)) ) )))
)
(defun maktr (arg) (list 'tr arg))

;;; maybe just build four different versions of mgu-tr2-disjunction: simple, left, right, and middle.  
;;;    It's nasty, but it should work--for each top-level variable x, simple does the above.
;;;    Left introduces y | z and resolves only on y; right only for z.  Middle introduces
;;;    y | w | z and resolves only on w.  But how do we do that efficiently?  Take the y | z example.
;;;    The first part of the unifier is x/{y|z}.  Then we bind y to whatever.  But what happens down
;;;    the road, e.g.  tr("...x .... x") becomes tr("...y|z...y|z...").  Do we only resolve on the second
;;;    y too or do we resolve on both?  This is all seriously irritating.

(defun mgu-tr2-arg (x y)
  "(MGU-TR2-ARG X Y) returns a list of metaunifiers for arguments to two positive tr literals
   when those arguments are of one of the following forms: variables, cons_not(...), and listof(...).
   Assumes proper variable renaming has been taken. "
  (let ((mgu nil) (n (newtmpvar)))

    (cond 
          ; complementary 1
          ((and (func-typep x 'listof) (func-typep y 'cons_not))
           (setq mgu (mgu-tr2 x (cadr y)))
           (if mgu mgu nil))

          ; complementary 2
          ((and (func-typep x 'cons_not) (func-typep y 'listof))
           (setq mgu (mgu-tr2 (cadr x) y))
           (if mgu mgu nil))

          ; variable and cons_not: tr(x) and tr("-...") means mgu-tr2(x,...)
          ((and (varp x) (func-typep y 'cons_not))
           (setq mgu (mgu-tr2 x (cadr y)))
           (if mgu mgu nil))

          ; cons_not and variable
          ((and (varp y) (func-typep x 'cons_not))
           (setq mgu (mgu-tr2 y (cadr x)))
           (if mgu mgu nil))

          ; two variables: tr(x) and tr(y) means x/"-<z>" and mgu-tr2(y,z)
          ((and (varp x) (varp y))
           (setq mgu (mgu-tr2 y n))
           (if mgu 
             (make-metaunifier :mgu (compose (bind x `(cons_not ,n) (new-bindinglist)) (metaunifier-mgu mgu))
                               :disjunction (metaunifier-disjunction mgu))
             nil))

          ; var and listof: tr(x) and tr(listof...) means x/"-<z>" and mgu-tr2(z,listof)
          ((and (varp x) (func-typep y 'listof))
           (setq mgu (mgu-tr2 y n))
           (if mgu
             (make-metaunifier :mgu (compose (bind x `(cons_not ,n) (new-bindinglist)) (metaunifier-mgu mgu))
                               :disjunction (metaunifier-disjunction mgu))
             nil))

          ; listof and var
          ((and (varp y) (func-typep x 'listof))
           (setq mgu (mgu-tr2 x n))
           (if mgu
             (make-metaunifier :mgu (compose (bind y `(cons_not ,n) (new-bindinglist)) (metaunifier-mgu mgu))
                               :disjunction (metaunifier-disjunction mgu))
             nil))


          ; otherwise just return nil
          ((or (and (func-typep x 'cons_not) (func-typep y 'cons_not))
               (and (func-typep x 'listof) (func-typep y 'listof)))
           nil)

          (t nil)))
)

(defun mgu-tr2 (x y)
  "(MGU-TR2 X Y) returns the most general unifier for 2 tr expressions x and y.
   The idea here is to treat them both as normal expressions, except that quoted variables
   cannot be assigned expressions including unquoted variables.  For now assume listof
   must be used instead of quoting the entire expression."
  (setq *metaunifications* (1+ *metaunifications*))
  (cond ((equal x y) (make-metaunifier :mgu (new-bindinglist) :disjunction nil))
        ((varp x) (mgu-var x y))
        ((varp y) (mgu-var y x))

        ; Quoted variables can be assigned to the other expression so long as
        ;   the other expression is entirely quoted, e.g. "(p ?x)" and "(p (f a))" will unify
        ;   but "(p ?x)" and "(p (f <?y>))" will not unify. 
        ((qvarp x) (mgu-qvar x y))
        ((qvarp y) (mgu-qvar y x))

        ((or (atom x) (atom y)) nil)
        ((or (qatom x) (qatom y)) nil)

        ; if not quoted expressions at this point, there can be no unifier.
        ((or (not (func-typep x 'listof)) (not (func-typep y 'listof))) nil)
        (t
         ; walk over the two listof terms, starting after 'listof'
         (do ((xs (cdr x) (cdr xs))
              (ys (cdr y) (cdr ys))
              (done nil)
              (disjunctions nil)
              (tau nil)
              (sigma (new-bindinglist)))
             ((or (null xs) (null ys) done) (if (and (eq xs ys) (not done))  ; ensure both nil
                                              (make-metaunifier :mgu sigma :disjunction disjunctions) 
                                              nil))
           (setq tau (mgu-tr2 (car xs) (car ys)))

           ; keep list of disjunctions and apply unifier as we go
           (when (and tau (metaunifier-disjunction tau)) 
             (if disjunctions
               (setf (cdr (last disjunctions)) (metaunifier-disjunction tau))
               (setq disjunctions (metaunifier-disjunction tau)))
             (setq disjunctions (plug disjunctions (metaunifier-mgu tau))))

           ; unify
           (cond ((not tau) (setq done t))
                 (t
                  (setq xs (plug xs (metaunifier-mgu tau)))
                  (setq ys (plug ys (metaunifier-mgu tau)))
                  (setq sigma (compose sigma (metaunifier-mgu tau))))) )))
)

(defun mgu-var (var p)
  "(MGU-VAR VAR P) handles the case of a normal variable and another term."
          ; occurs check fails
    (cond ((occurs var p) nil)

          ; in all these cases, just require var to unify with p
          ((or (qconst p) (qvarp p) (varp p))
           (make-metaunifier :mgu (new-bindinglist) 
                             :disjunction (list `(not (unifyp ,var ,p)))))

          ; listof
          ((and (listp p) (eq (car p) 'listof))
           (if (not (cdr p))  
             (simple-bind var p)   ; weird case with a single listof
             (let ((g (append `(listof ,(cadr p)) 
                              (mapcar #'(lambda (x) (declare (ignore x)) (newtmpvar)) (cddr p)))))
               ;g is the generalized form of p
               (make-metaunifier :mgu (bind var g (new-bindinglist))
                                 :disjunction (mapcar #'(lambda (x y) `(not (unifyp ,x ,y))) (cddr g) (cddr p)))
               )))

          ; normal binding
          (t (simple-bind var p))
          )
)
(defun mgu-qvar (qvar p)
  "(MGU-QVAR QVAR P) handles the unification of a quoted variable with another term."
  ; this is simpler since p is either a quoted constant, another quoted variable, or a 
  ;    listof term, all of which will unify with qvar so long as they are fully quoted 
  ; Assumes the quoted variables are distinct from the normal variables.
  (if (and (not (occurs qvar p)) (allquoted p)) 
    (make-metaunifier :mgu (bind qvar p (new-bindinglist)) :disjunction nil) 
    nil)
)

;;;;;;;;;; mgu-norm ;;;;;;;;;;;;

(defun mgu-norm (x y)
  "(MGU-NORM X Y) returns the most general unifier of two expressions x and y."
  (cond ((eq x y) (bind t t (new-bindinglist)))
        ((varp x) (if (not (occurs x y)) (bind x y (new-bindinglist)) nil))
        ((varp y) (if (not (occurs y x)) (bind y x (new-bindinglist)) nil))
        ((or (atom x) (atom y)) nil)
        (t
         (do ((xs x (cdr xs))
              (ys y (cdr ys))
              (done nil)
              (tau nil)
              (sigma (bind t t (new-bindinglist))))
             ((or (null xs) (null ys) done) (if (and (eq xs ys) (not done)) sigma nil))
           (setq tau (mgu-norm (car xs) (car ys)))
           (cond ((not tau) (setq done t))
                 (t
                  (setq xs (plug xs tau))
                  (setq ys (plug ys tau))
                  (setq sigma (compose sigma tau)))) )))
)

(defun occurs (x exp &optional (test #'eq))
  "(OCCURS X EXP) checks whether X occurs any where in EXP using the test TEST."
  (cond ((atom exp) (funcall test x exp))
        (t
         (some #'(lambda (subexp) (occurs x subexp)) exp)))
)


#| testing
? (mgu '(tr (listof 'p '?x)) '(p a))
((T . T) (?X . A))
? (mgu '(tr (listof 'p 'a)) '(p ?x))
((T . T) (?X . A))
? (mgu '(tr (listof 'p ?x)) '(p a))
((T . T) (?X . X))
? (mgu '(tr (listof 'p ?x)) '(p a))
((T . T) (?X QUOTE A))
? (plug '(tr (listof 'p ?x)) *)
(TR (LISTOF 'P 'A))
? (mgu '(tr (listof 'p ?x)) '(p ?y))
((T . T) (?X QUOTE ?Y))
? (plug '(tr (listof 'p ?x)) *)
(TR (LISTOF 'P '?Y))
? (mgu '(tr (listof 'p ?x)) '(p '?y))
NIL

|#




#|
(defmethod mgu (x y)
   (mgusentence x y '((t . t))))

(defun mgusentence (x y al)
  (cond ((eq 'tr (car x))
         (cond ((eq 'tr (car y))
                (mguterms (cadr x) (cadr y) al))
               (t (mgumeta (cadr x) y al))))
        (t 
         (cond ((eq 'tr (car y)) (mgumeta (cadr y) x al))
               (t (mguexpexp x y al))))))

(defun mgutrterms (x y al)
  "(MGUTRTERMS X Y AL) takes arguments to 2 different tr expressions."
  (cond ((eq (car x) 'quote)
         (cond ((eq (car y) 'quote) (mguterm (cadr x) (cadr y) al))
               ((eq (car y) 'cons) (mguquotecons (cadr x) y al))
               (t nil)) )
        ((eq (car x) 'cons)
         (cond ((eq (car y) 'cons) (mguterm x y al))
               ((eq (car y) 'quote) (mguquotecons (cadr y) x al))
               (t nil)))
        (t nil)))
(defun mguquotecons (x y al)
  "(MGUQUOTECONS X Y AL) takes as input an unquoted sentence x and a cons sentence y."
   (cond ((not (listp x)) nil)
         ((null x) nil)
         ((setq al (mguterm (car x) (cadr y) al))
          (mguquotecons (cdr x) (caddr y) al))))  
)

(defun mguterm (x y al)
   (cond ((varp x) (mguvariable x y al))
         ((atom x)
          (cond ((varp y) (mguvariable y x al))
                ((equalp x y) al)))
        	((eq 'quote (car x))
          (cond ((varp y) (mguvariable y x al))
                ((atom y) nil)
                ((and (eq 'quote (car y))
                      (equalp (cadr x) (cadr y))) al)))
         (t (cond ((varp y) (mguvariable y x al))
                  ((atom y) nil)
                  ((eq 'quote (car y)) nil)
                  (t (mguexpexp x y al))))))

(defun mguvariable (x y al)
   (let (dum)
     (cond ((setq dum (assq x al)) (mguterm (cdr dum) y al))
           ((eq x (setq y (mguindval y al))) al)
           ((mguchkp x y al) nil)
           (t (acons x y al)))))

(defun mguindval (x al)
   (let (dum)
     (cond ((and (varp x) (setq dum (assq x al)))
            (mguindval (cdr dum) al))
           (t x))))

(defun mguchkp (p q al)
   (cond ((eq p q))
         ((varp q) (mguchkp p (cdr (assq q al)) al))
         ((atom q) nil)
         ((eq 'quote (car q)) nil)
         (t (some #'(lambda (x) (mguchkp p x al)) q))))

(defun mguexpexp (l m al)
   (do ((l l (cdr l)) (m m (cdr m)))
       ((null l) (if (null m) al nil))
       (cond ((null m) (return nil))
             ((setq al (mguterm (car l) (car m) al)))
             (t (return nil)))))

(defun mgumeta (x y al)
   (cond ((varp x) (mgumetavariable x y al))
         ((varp y) (mgumetaunvariable y x al))
         ((atom x) nil)
         ((eq 'quote (car x)) (mguterm (cadr x) y al))
         ((eq 'cons (car x)) (mgumetacons x y al))))

(defun mgumetavariable (x y al)
   (cond ((assq x al) (mgumeta (cdr (assq x al)) y al))
         (t (acons x (myquotify y) al))))


(defun myquotify (x)
  "(MYQUOTIFY X) takes an expression and returns the cons version of that
   expression."
  
)

(defun mgumetacons (x y al)
   (cond ((not (listp y)) nil)
         ((null y) nil)
         ((setq al (mgumeta (cadr x) (car y) al))
          (mgumeta (caddr x) (cdr y) al))))
|#
