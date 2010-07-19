
(in-package :tlh)
(defvar *check-occurs* t)
(defvar *unifications* 0
  "The number of unifications performed." )



(defun mgu (p q)
  (if *mrgmgu* 
    (mgu-mrg p q)
    (mgu-tlh p q))
)

(defun mgu-tlh (p q)
   "(MGU P Q) takes two expressions, which it assumes have been 
    standardized apart if necessary, and returns a most general unifier."
   (setf *unifications* (1+ *unifications*))
   ;(mguexp p q)
   (mguexp (copy-tree p) (copy-tree q))
)

;  Mike's version
(defun mgu-mrg (p q)
  (let ((a (common-lisp-user::environment)) 
        (b (common-lisp-user::environment)) 
        r)
    (setq r (common-lisp-user::unify p a q b))
    (setq a (common-lisp-user::alist a))
    (setq b (common-lisp-user::alist b))
    (if r 
      (new-bindinglist (append (mapcar #'(lambda (x) (cons (car x) (cadr x))) a)
                               (mapcar #'(lambda (x) (cons (car x) (cadr x))) b)))
      nil))
)


(defun mguexp (p q)
   "(MGUEXP P Q) is the recursive workhorse of mgu."
   (cond ((equal p q) (new-bindinglist))
         ((varp p) (assignvar p q))
         ((varp q) (assignvar q p))
         ((or (atom p) (atom q)) nil)
         (t (mguterm p q)) )
)
(defun assignvar (var val)
  "(ASSIGNVAR VAR VAL) first do an occurs check.  Then do the
   binding."
   (let ((bl (new-bindinglist)))
      (cond (*check-occurs* 
             (if (member-rec var val) (setq bl nil) (bind var val bl)))
            (t
             (bind var val bl)) )
      bl )
)
(defun member-rec (item list &key (test #'eq))
   "(MEMBER-REC ITEM LIST TEST) is a recursive version of member, i.e.
   it checks if ITEM shows up anywhere in LIST, using TEST for equality."
   (cond ((funcall test item list) t)
         ((or (null list) (atom list)) nil)
         (t 
            (do ((ls list (cdr ls)))
                ((null ls) nil)
              (when (member-rec item (car ls) :test test)
                    (return t)))))
)

(defun mguterm (p q)
   "(MGUTERM P Q) takes the functional terms p and q 
    and returns the binding list that is the mgu of p and q."
   (do ((ps p (cdr ps))
        (qs q (cdr qs))
        (bl (new-bindinglist))
        (tau nil))
       ((or (null ps) (null qs)) (if (and (null ps) (null qs)) bl nil))
      (setq tau (mguexp (car ps) (car qs)))
      (when (not tau) (return nil))
      (plug-listd ps tau)
      (plug-listd qs tau)
      (composed bl tau) )
)
(defun mgu-n (ps)
   "(MGU-N PS) returns a most general unifier if one exists of all
    the expressions in PS.  Does not standardize apart and 
    relies on my mgu."
   (cond ((atom ps) nil)   ; not a list--don't care
         ((and (listp ps) (cdr ps)) ; list has 2+ elements
          (do ((p (cdr ps) (cdr p))
               (mgu (new-bindinglist))
               (lastp (car ps)))
              ((or (null p) (not mgu)) mgu)
           (setq mgu (compose mgu (mgu lastp (car p))))
           (setq lastp (plug (car p) mgu) )) )
         (t nil))
)