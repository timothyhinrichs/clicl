;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; (c) Copyright 1999-2003 by Michael R. Genesereth.  All rights reserved.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(eval-when (compile load eval)
  (proclaim '(special *client* *agent* *interface* *manager*
                      *var-count* *version* *from* *alist* *trace*)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Dumping everything
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod dump (fn)
  (declare (ignore fn))
  "Dump takes a file name as argument.")

(defmethod dump ((fn string))
  (with-open-file (f fn :direction :output :if-exists :supersede)
    (formatn f ";;;;;;;;;;;;;;;;" 5) (terpri f)
    (format  f ";;; Infomaster Version ~A" *version*) (terpri f)
    (formatn f ";;;;;;;;;;;;;;;;" 5) (terpri f)
    (terpri f)
    (dolist (agent (cons 'manager (remove 'manager (getbaskets))))
      (format f "(define-theory ~A \"\" '(" agent)
      (do ((l (contents (symbol-value agent)) (cdr l)))
          ((null l) (format f "))") (terpri f))
          (terpri f) (format f "  ~S" (car l)))
      (terpri f))
    (formatn f ";;;;;;;;;;;;;;;;" 5) (terpri f)
    (formatn f ";;;;;;;;;;;;;;;;" 5) (terpri f)
    (formatn f ";;;;;;;;;;;;;;;;" 5) (terpri f)
    'done))

(defun formatn (s str n)
  (do ((i 1 (1+ i)))
      ((> i n) 'done)
      (format s str)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; sql
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun sql (x)
  (let ((*var-count* *var-count*))
    (grindsql (sqlify x))))

(defun sqlify (x)
  (cond ((atom x) nil)
        ((eq 'ask-all (car x)) (sqlall (cadr x) (caddr x)))
        ((and (eq 'length (car x)) (listp (cadr x)) (eq 'ask-all (caadr x)))
         (setq x (cadr x))
         (setq x (sqlall (cadr x) (caddr x)))
         (cond ((null (cdadr x)) (rplaca (cdr x) `(count ,(caadr x))) x)
               (t (rplacd (last x) (list (cadr x)))
                  (rplaca (cdr x) '(count (count *)))
                  x)))))

(defun sqlall (x p)
  (setq p (dnf p))
  (cond ((atom p) nil)
        ((eq 'or (car p)) (sqlallor x p))
        (t (sqlallselect x p))))

(defun sqlallor (x p)
  (do ((l (cdr p) (cdr l)) (nl)) 
      ((null l) (cons 'union (nreverse nl)))
      (setq nl (cons (sqlallselect x (car l)) nl))))

(defun sqlallselect (x p)
  (let ((aspect) (where) (*from*) (*alist*))
    (setq where (sqlexp p))
    (do ((l (if (listp x) x (list x)) (cdr l)))
        ((null l) (setq aspect (nreverse aspect)))
        (setq aspect (cons (sqlarg (car l)) aspect)))
    `(select ,aspect ,(nreverse *from*) ,where)))

(defun sqlarg (x)
  (cond ((numberp x) x)
        ((stringp x) x)
        ((cdr (assoc x *alist*)))
        (t x)))


(defun sqlexp (p)
  (cond ((atom p) p)
        ((eq 'not (car p)) (sqlunprovable p))
        ((eq 'unprovable (car p)) (sqlunprovable p))
        ((eq 'and (car p)) (sqlexpand p))
        ((eq 'same (car p)) 'false)
        ((eq 'oneof (car p)) 'false)
        ((find (car p) '(distinct < > >= =< substring)) (sqlexpbuiltin p))
        ((find (car p) '(+ - * /)) (sqlexparith p))
        (t (sqlexpatom p))))

(defun add-to-alist (key value)
  (setq *alist* (acons key value *alist*)))

(defun sqlexpatom (p)
  (let (rel)
    (if (find (car p) *from*)
        (setq rel (newvar) *from* (cons (list (car p) rel) *from*))
        (setq rel (car p) *from* (cons rel *from*)))
    (do ((l (cdr p) (cdr l)) (i 1 (1+ i)) (cols (columns (car p)) (cdr cols))
         (ref) (dum) (nl))
        ((null l) (maksand nl))
        (setq dum (if (car cols) (car cols) i))
        (setq ref (list '\. rel dum))
        (cond ((not (varp (car l)))
               (setq nl (cons `(= ,ref ,(car l)) nl)))
              ((eq '?* (car l)))
              ((setq dum (cdr (assoc (car l) *alist*)))
               (setq nl (cons `(= ,ref ,dum) nl)))
              (t (add-to-alist (car l) ref))))))

(defun sqlunprovable (p)
  (let (vars)
    (setq vars (intersection (mapcar #'car *alist*) (vars (cadr p))))
    `(not (in ,(sublis *alist* vars) ,(sqlall vars (cadr p))))))

(defun sqlexpand (p)
  (sqlexpands (sqlsort (cdr p))))

(defun sqlexpands (pl)
  (cond ((null pl) 'true)
        ((and (listp (car pl)) (eq 'same (caar pl)))
         (sqlexpandoneof `(oneof ,(list (cadar pl)) ,(cddar pl)) (cdr pl)))
        ((and (listp (car pl)) (eq 'oneof (caar pl)))
         (sqlexpandoneof (car pl) (cdr pl)))
        (t (makand (sqlexp (car pl)) (sqlexpands (cdr pl))))))

(defun sqlexpandoneof (p pl)
  (do ((l (cddr p) (cdr l)) (dum) (nl))
      ((null l) (maksor (nreverse nl)))
      (let ((*alist* *alist*))
        (setq dum (sqlexpone (cadr p) (car l)))
        (setq nl (cons (makand dum (sqlexpands pl)) nl)))))

(defun sqlexpone (x y)
  (do ((l x (cdr l)) (m y (cdr m)) (dum) (nl))
      ((null l) (maksand nl))
      (cond ((not (varp (car l)))
             (setq nl (cons `(= ,(car l) ,(car m)) nl)))
            ((setq dum (assoc (car l) *alist*))
	     ;; Added by JPR.  I'm not sure that this is the right thing, but
	     ;; as far as I can tell, variable substitution was never being
	     ;; checked for the second arg to the "=" operator.
	     (let ((dum2 (or (rest (assoc (car m) *alist*))
			     (car m))))
	       (setq nl (cons `(= ,(cdr dum) ,dum2) nl))))
            (t (add-to-alist (car l) (car m))))))

(defun sqlexparith (p)
  (let ((x (butlast p)) (y (car (last p))) (dum))
    (cond ((not (varp y)) `(= ,(mapcar #'sqlarg x) ,y))
          ((setq dum (assoc y *alist*)) `(= ,x ,dum))
          (t (add-to-alist y (mapcar #'sqlarg x)) 'true))))

(defun sqlexpbuiltin (p)
  (cons (car p) (mapcar #'sqlarg (cdr p))))

(defun sqlsort (cl)
  (do ((l cl (cdr l)) (nl) (nm))
      ((null l) (nreconc nl (nreverse nm)))
      (cond ((atom (car l)) (setq nl (cons (car l) nl)))
            ((find (caar l) '(same oneof = < > >= =<))
             (setq nm (cons (car l) nm)))
            (t (setq nl (cons (car l) nl))))))


(defparameter *toplevel* t)

(defun grindsql (p)
  (with-output-to-string (s)
    (grindsqlexp s p 'lparen 'rparen)))

(defun grindsqlexp (s p lop rop)
  (cond ((atom p) (grindsql-atom s p))
        ((eq 'and (car p)) (grindsql-and s p lop rop))
        ((eq 'or (car p)) (grindsql-or s p lop rop))
        ((eq 'same (car p)) (grindsql-= s p lop rop))
        ((eq 'distinct (car p)) (grindsql-\# s p lop rop))
        ((eq 'union (car p)) (grindsql-union s p lop rop))
        ((eq 'in (car p)) (grindsql-in s p lop rop))
        ((eq '\. (car p)) (grindsql-\. s p))
        ((eq '\^ (car p)) (grindsql-\^ s p lop rop))
        ((eq '* (car p)) (grindsql-* s p lop rop))
        ((eq '+ (car p)) (grindsql-+ s p lop rop))
        ((eq '> (car p)) (grindsql-> s p lop rop))
        ((eq '>= (car p)) (grindsql->= s p lop rop))
        ((eq '= (car p)) (grindsql-= s p lop rop))
        ((eq '\# (car p)) (grindsql-\# s p lop rop))
        ((eq '=< (car p)) (grindsql-=< s p lop rop))
        ((eq '< (car p)) (grindsql-< s p lop rop))
        ((eq 'substring (car p)) (grindsql-substring s p))
        ((eq 'concat (car p)) (grindsql-concat s p))
        ((eq 'convert (car p)) (grindsql-convert s p))
        ((eq 'not (car p)) (grindsql-not s p lop rop))
        ((eq 'select (car p)) (grindsql-select s p))
        ((null (cdr p)) (grindsqlexp s (car p) 'paren 'paren) (format s "()"))
        (t (grindsql-complex s p))))

(defun grindsql-atom (s p)
  (cond ((numberp p) (princ p s))
        ((characterp p) (if (char= p #\') (format s "''") (format s "'")))
        ((stringp p) (grindsql-string s p))
        ;((setq dum (findx '?x `(prettyname ,p ?x) *interface*)) (princ dum s))
        ((symbolp p) (princ (string-downcase (symbol-name p)) s))))

(defun grindsql-string (s x)
  (cond ((not (position #\' x)) (format s "'~A'" x))
        (t (write-char #\' s)
           (do ((i 0 (1+ i)) (n (length x)))
               ((>= i n))
               (if (char= (elt x i) #\') (write-char #\' s))
               (write-char (elt x i) s))
           (write-char #\' s))))

(defun grindsql-identifier (s x)
  (cond ((atom x) (grindsql-atom s x))
        ((eq '\. (car x)) (grindsql-\. s x))
        (t (grindsql-identifier s (car x)) (princ " " s) (grindsql-identifier s (cadr x)))))

(defun grindsql-concat (s x)
  (format s "{fn CONCAT(")
  (grindsqlexp s (cadr x) 'paren 'paren)
  (format s ",")
  (grindsqlexp s (caddr x) 'paren 'paren)
  (format s ")}"))

(defun grindsql-convert (s x)
  (format s "{fn CONVERT(")
  (grindsqlexp s (cadr x) 'paren 'paren)
  (format s ",")
  (grindsqlexp s (caddr x) 'paren 'paren)
  (format s ")}"))

(defun grindsql-select (s x)
  (grindsql-aspect (cadr x) s)
  (grindsql-from (caddr x) s)
  (grindsql-where (cadddr x) s)
  (grindsql-group (car (cddddr x)) s))

(defun grindsql-aspect (x s)
  (cond ((equal x '(count (count *)))
         (format s "SELECT COUNT(COUNT(*))"))
        ((and (listp x) (eq 'count (car x)))
         (format s "SELECT COUNT(DISTINCT ")
         (grindsqlexp s (cadr x) 'paren 'paren)
         (format s ")"))
        (t (format s "SELECT DISTINCT ")
           (grindsql-identifier s (car x))
           (dolist (aspect (cdr x))
             (princ "," s)
             (grindsql-identifier s aspect)))))

(defun grindsql-from (x s)
  (when *toplevel* (fresh-line s))
  (format s "  FROM ")
  (grindsql-identifier s (car x))
  (dolist (relation (cdr x))
    (princ "," s)
    (grindsql-identifier s relation)))

(defun grindsql-where (x s)
  (cond ((eq 'true x))
        (t (when *toplevel* (fresh-line s))
           (format s "  WHERE ")
           (let (*toplevel*) (grindsqlexp s x 'paren 'paren)))))

(defun grindsql-group (x s)
  (when x
    (when *toplevel* (fresh-line s))
    (format s "  GROUP BY ")
    (grindsql-identifier s (car x))
    (dolist (aspect (cdr x))
      (princ "," s)
      (grindsql-identifier s aspect))))

(defun grindsql-complex (s p)
  (grindsqlexp s (car p) 'paren 'paren)
  (format s "(")
  (grindsqlexp s (cadr p) 'paren 'paren)
  (dolist (arg (cddr p))
    (format s ",")
    (grindsqlexp s arg 'paren 'paren))
  (format s ")"))

(defun grindsql-union (s p lop rop)
  (grindsqlexp s (cadr p) lop 'union)
  (dolist (disjunct (cddr p))
    (when *toplevel* (fresh-line s))
    (format s " UNION ")
    (when *toplevel* (fresh-line s))
    (grindsqlexp s disjunct 'union rop)))

(defun grindsql-in (s p lop rop)
  (declare (ignore rop))
  (cond ((atom (cadr p)) (grindsqlexp s (cadr p) lop 'in))
        ((null (cdadr p)) (grindsqlexp s (caadr p) lop 'in))
        (t (grindsqllist s (cadr p))))
  (format s " IN ")
  (grindsql-parens s (caddr p)))

(defun grindsqllist (s p)
  (format s "(")
  (grindsqlexp s (car p) 'lparen 'rparen)
  (dolist (item (cdr p))
    (format s ", ")
    (grindsqlexp s item 'lparen 'rparen))
  (format s ")"))

(defun grindsql-\. (s p)
  (grindsql-atom s (cadr p))
  (format s ".")
  (grindsql-atom s (caddr p)))

(defun grindsql-\^ (s p lop rop)
  (grindsqlleft s lop '\^ rop)
  (grindsqlexp s (cadr p) lop '\^)
  (format s "^")
  (grindsqlexp s (caddr p) '\^ rop)
  (grindsqlright s lop '\^ rop))

(defun grindsql-* (s p lop rop)
  (cond ((null (cdr p)) (grindsqlexp s 1 'paren 'paren))
        ((null (cddr p)) (grindsqlexp s (cadr p) lop rop))
        (t (grindsqlleft s lop '* rop)
           (grindsqlexp s (cadr p) lop '*)
           (do ((l (cddr p) (cdr l)))
               ((null (cdr l)) (format s "*") (grindsqlexp s (car l) '* rop))
               (format s "*")
               (grindsqlexp s (car l) '* '*))
           (grindsqlright s lop '* rop))))

(defun grindsql-+ (s p lop rop)
  (cond ((null (cdr p)) (grindsqlexp s 0 'paren 'paren))
        ((null (cddr p)) (grindsqlexp s (cadr p) lop rop))
        (t (grindsqlleft s lop '+ rop)
           (grindsqlexp s (cadr p) lop '+)
           (do ((l (cddr p) (cdr l)))
               ((null (cdr l)) (format s "+") (grindsqlexp s (car l) '+ rop))
               (format s "+")
               (grindsqlexp s (car l) '+ '+))
           (grindsqlright s lop '+ rop))))

(defun grindsql-> (s p lop rop)
  (grindsqlleft s lop '> rop)
  (grindsqlexp s (cadr p) lop '>)
  (format s ">")
  (grindsqlexp s (caddr p) '> rop)
  (grindsqlright s lop '> rop))

(defun grindsql->= (s p lop rop)
  (grindsqlleft s lop '>= rop)
  (grindsqlexp s (cadr p) lop '>=)
  (format s ">=")
  (grindsqlexp s (caddr p) '>= rop)
  (grindsqlright s lop '>= rop))

(defun grindsql-= (s p lop rop)
  (grindsqlleft s lop '= rop)
  (grindsqlexp s (cadr p) lop '=)
  (format s "=")
  (grindsqlexp s (caddr p) '= rop)
  (grindsqlright s lop '= rop))

(defun grindsql-\# (s p lop rop)
  (grindsqlleft s lop '\# rop)
  (grindsqlexp s (cadr p) lop '\#)
  (format s "<>")
  (grindsqlexp s (caddr p) '\# rop)
  (grindsqlright s lop '\# rop))

(defun grindsql-=< (s p lop rop)
  (grindsqlleft s lop '=< rop)
  (grindsqlexp s (cadr p) lop '=<)
  (format s "<=")
  (grindsqlexp s (caddr p) '<= rop)
  (grindsqlright s lop '=< rop))

(defun grindsql-< (s p lop rop)
  (grindsqlleft s lop '< rop)
  (grindsqlexp s (cadr p) lop '<)
  (format s "<")
  (grindsqlexp s (caddr p) '< rop)
  (grindsqlright s lop '< rop))

(defun grindsql-substring (s x)
  (format s "{fn UCASE(")
  (grindsqlexp s (caddr x) 'paren 'paren)
  (format s ")} LIKE ")
  (format s "'%~A%'" (string-upcase (princ-to-string (cadr x)))))

(defun grindsql-not (s p lop rop)
  (declare (ignore lop))
  (format s "NOT ")
  (grindsqlexp s (cadr p) 'not rop))

(defun grindsql-and (s p lop rop)
  (cond ((null (cdr p)) (grindsqlexp s 'true 'paren 'paren))
        ((null (cddr p)) (grindsqlexp s (cadr p) lop rop))
        (t (grindsqlleft s lop 'and rop)
           (grindsqlexp s (cadr p) lop 'and)
           (do ((l (cddr p) (cdr l)))
               ((null (cdr l)) (format s " AND ")
                (when *toplevel* (fresh-line s))
                (when *toplevel* (format s "        "))
                (grindsqlexp s (car l) 'and rop))
               (format s " AND ")
               (when *toplevel* (fresh-line s))
               (when *toplevel* (format s "        "))
               (grindsqlexp s (car l) 'and 'and))
           (grindsqlright s lop 'and rop))))

(defun grindsql-or (s p lop rop)
  (cond ((null (cdr p)) (grindsqlexp s 'false 'paren 'paren))
        ((null (cddr p)) (grindsqlexp s (cadr p) lop rop))
        (t (grindsqlleft s lop 'or rop)
           (grindsqlexp s (cadr p) lop 'or)
           (do ((l (cddr p) (cdr l)))
               ((null (cdr l)) (format s " OR ")
                (when *toplevel* (fresh-line s))
                (when *toplevel* (format s "        "))
                (grindsqlexp s (car l) 'or rop))
               (format s " OR ")
               (when *toplevel* (fresh-line s))
               (when *toplevel* (format s "        "))
               (grindsqlexp s (car l) 'or 'or))
           (grindsqlright s lop 'or rop))))

(defun grindsql-variable (s p)
  (format s (symbol-name p)))

(defun grindsql-list (s p)
  (format s "(")
  (grindsql-atom s (car p))
  (dolist (item (cdr p))
    (format s ", ")
    (grindsql-atom s item))
  (format s ")"))

(defun grindsql-parens (s p)
  (format s "(")
  (grindsqlexp s p 'paren 'paren)
  (format s ")"))

(defun grindsqlleft (s lop op rop)
  (if (or (precedencep lop op) (precedencep rop op)) (format s "(")))

(defun grindsqlright (s lop op rop)
  (if (or (precedencep lop op) (precedencep rop op)) (format s ")")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Mailers
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass mailer (agent) ())

(defmethod message-handler (*message* *sender* (*receiver* mailer))
  (cond ((atom *message*) nil)
        ((eq 'mail (car *message*))
         (let ((server (findx '?x `(server ,(name *receiver*) ?x) *interface*)))
           (when server
             (mail (elt *message* 1) (elt *message* 2) (elt *message* 3) (elt *message* 4) server))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; SQL Listeners
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun acl (x) (aclify (readsql x)))

;;; (sql-acl '(select (a b) (p q (p p1)) (> c 0))) -->
;;; (ask-all (?a ?b) (and (r ?a ?b ?c) (> ?c 0)))

(defun aclify (x)
  (cond ((atom x) x)
        ((eq 'select (car x)) (aclselect x))
        (t (cons (car x) (mapcar #'aclify (cdr x))))))

(defun aclselect (x)
  (let (what from where al)
    (setq al (makalist x))
    (setq what (aclaspect (cadr x) al))
    (setq from (aclfrom (caddr x) al))
    (setq where (aclwhere (cadddr x) al))
    `(ask-all ,what ,(samesame (makand from where)))))

(defun makalist (x)
  (do ((l (caddr x) (cdr l)) (rel) (arity) (cols) (al))
      ((null l) al)
      (cond ((atom (car l))
             (setq rel (car l) arity (find-arity rel) cols (find-columns rel)))
            (t (setq rel (cadar l)
                     arity (find-arity (caar l))
                     cols (find-columns (caar l)))
               (setq al (acons (cadar l) (caar l) al))))
      (do ((i 1 (1+ i)) (m cols (cdr m)))
          ((or (and arity (> i arity)) (and (not arity) (null m))))
          (setq al (acons (list '\. rel (or (car m) i)) (gentemp "?") al)))))

(defun aclaspect (x al)
  (cond ((and (listp x) (eq '\. (car x)))
         (let (dum)
           (if (setq dum (assoc x al :test #'equal)) (cdr dum) x)))
        (t (do ((l x (cdr l)) (dum) (nl))
               ((null l) (nreverse nl))
             (cond ((eq '* (car l)) (setq nl (nreconc (mapcar #'caddr al) nl)))
                   ((setq dum (assoc (car l) al :test #'equal))
                    (setq nl (cons (cdr dum) nl)))
                   (t (setq nl (cons (car l) nl))))))))

(defun aclfrom (rl al)
  (do ((l rl (cdr l)) (rel) (name) (arity) (cols) (nl))
      ((null l) (maksand (nreverse nl)))
      (cond ((atom (car l)) (setq rel (car l) name (car l)))
            (t (setq rel (caar l) name (cadar l))))
      (setq arity (find-arity rel) cols (find-columns rel))
      (do ((i 1 (1+ i)) (m cols (cdr m)) (dum) (nm))
          ((or (and arity (> i arity)) (and (not arity) (null m)))
           (setq nl (cons (cons rel (nreverse nm)) nl)))
          (setq dum (list '\. name (or (car m) i)))
          (setq nm (cons (or (cdr (assoc dum al :test #'equal)) dum) nm)))))

(defun aclwhere (x al)
  (cond ((numberp x) x)
        ((stringp x) x)
        ((or (atom x) (eq '\. (car x)))
         (or (cdr (assoc x al :test #'equal)) x))
        ((eq 'in (car x))
         (let (dum)
           (setq dum (aclwhere (caddr x) al))
           (selsubst (aclwhere (cadr x) al) (cadr dum) (caddr dum))))
        ((eq '= (car x))
         (list 'same (aclwhere (cadr x) al) (aclwhere (caddr x) al)))
        ((eq 'select (car x)) (aclselect x))
        ((eq 'not (car x)) `(unprovable ,(aclwhere (cadr x) al)))
        (t (do ((l x (cdr l)) (nl))
               ((null l) (nreverse nl))
               (setq nl (cons (aclwhere (car l) al) nl))))))

(defun selsubst (x y z)
  (cond ((atom x) (subst x y z))
        (t (do ((l x (cdr l)) (m y (cdr m)))
               ((null l) z)
               (setq z (subst (car l) (car m) z))))))

(defun samesame (p)
  (cond ((atom p) p)
        ((eq 'and (car p))
         (do ((l (cdr p) (cdr l)) (nl))
             ((null l) (maksand (nreverse nl)))
             (cond ((atom (car l)) (setq nl (cons (car l) nl)))
                   ((eq 'same (caar l))
                    (cond ((varp (cadar l))
                           (setq nl (subst (caddar l) (cadar l) nl))
                           (setq l (subst (caddar l) (cadar l) l)))
                          ((varp (caddar l))
                           (setq nl (subst (cadar l) (caddar l) nl))
                           (setq l (subst (cadar l) (caddar l) l)))
                          (t (setq nl (cons (car l) nl)))))
                   (t (setq nl (cons (car l) nl))))))
        (t p)))


(defparameter *string* "")

(defun readsql (s)
  (parsesql (scansql s)))

(defun scansql (*string*)
  (do ((n 0) (char) (lexeme) (nl))
      ((>= n (length *string*)) (nreverse nl))
      (setq char (elt *string* n))
      (cond ((whitep char) (setq n (1+ n)))
            ((idcharp char)
             (multiple-value-setq (lexeme n) (scansqlsymbol n))
             (setq nl (cons lexeme nl)))
            ((char-equal char #\.)
	     (setq nl (cons '\. nl) n (1+ n)))
            ((char-equal char #\')
             (multiple-value-setq (lexeme n) (scansqlstring (1+ n)))
             (setq nl (cons lexeme nl)))
            ((char-equal char #\=)
             (cond ((char-equal (elt *string* (1+ n)) #\<)
                    (setq nl (cons '=< nl) n (+ n 2)))
                   (t (setq nl (cons '= nl) n (1+ n)))))
            ((char-equal char #\<)
             (cond ((char-equal (elt *string* (1+ n)) #\=)
                    (setq nl (cons '<= nl) n (+ n 2)))
                   (t (setq nl (cons '< nl) n (1+ n)))))
            ((char-equal char #\>)
             (cond ((char-equal (elt *string* (1+ n)) #\=)
                    (setq nl (cons '>= nl) n (+ n 2)))
                   (t (setq nl (cons '> nl) n (1+ n)))))
            ((char-equal char #\() (setq nl (cons 'lparen nl) n (1+ n)))
            ((char-equal char #\)) (setq nl (cons 'rparen nl) n (1+ n)))
            (t (setq nl (cons (intern (make-string 1 :initial-element char)) nl)
                     n (1+ n))))))

(defun scansqlstring (n)
  (values (with-output-to-string (s)
            (do ((char))
                ((or (= n (length *string*))
                     (char= #\' (setq char (elt *string* n)))))
                (write-char char s)
                (setq n (1+ n))))
          (1+ n)))

(defun scansqlsymbol (n)
  (values (read-from-string
           (with-output-to-string (s)
             (do ((char))
                 ((or (= n (length *string*))
                      (not (idcharp (setq char (elt *string* n))))))
                 (write-char char s)
                 (setq n (1+ n)))))
          n))


(defun parsesql (*string*)
  (parsesqlexp *string* 'lparen 'rparen))

(defun parsesqlexp (s lop rop)
  (declare (ignore s))
  (do ((left (parsesqlprefix *string* rop)))
      ((null *string*) left)
      (cond ((precedencep lop (car *string*)) (return left))
            (t (setq left (parsesqlinfix *string* left (car *string*) rop))))))

(defun parsesqlprefix (s rop)
  (declare (ignore s))
  (let ((left (pop *string*)))
    (cond ((eq 'select left) (parsesqlselect *string*))
          ((eq 'lparen left) (parsesqlparenlist *string*))
          ((prefixp left) (list left (parsesqlexp *string* left rop)))
          (t left))))

(defun parsesqlinfix (s left op rop)
  (declare (ignore s))
  (cond ((eq 'and op) (parsesqlnary *string* left 'and rop))
        (t (pop *string*) (list op left (parsesqlexp *string* op rop)))))

(defun parsesqlnary (s left op rop)
  (declare (ignore s))
  (do ((nl))
      ((or (null *string*) (not (eq op (car *string*))))
       (list* op left (nreverse nl)))
      (pop *string*)
      (setq nl (cons (parsesqlexp *string* op rop) nl))))

(defun parsesqlparenlist (s)
  (declare (ignore s))
  (let (dum)
    (setq dum (parsesqllist *string*))
    (cond ((eq 'rparen (car *string*))
           (pop *string*)
           (if (null (cdr dum)) (car dum) dum))
          (t (error "Bad parentheses.")))))

(defun parsesqllist (s)
  (declare (ignore s))
  (do ((nl))
      (nil)
      (setq nl (cons (parsesqlexp *string* '\, 'rparen) nl))
      (cond ((eq '\, (car *string*)) (pop *string*))
            ((null (cdr nl)) (return (car nl)))
            (t (return (nreverse nl))))))

(defun parsesqlselect (s)
  (declare (ignore s))
  (let ((count) (aspect) (from) (where 'true))
    (when (eq 'count (car *string*)) (setq count t) (pop *string*))
    (when (find (car *string*) '(all distinct)) (pop *string*))
    (setq aspect (parsesqllist *string*))
    (unless (eq 'from (pop *string*)) (error "Bad select form."))
    (setq from (parsesqlfrom *string*))
    (case (car *string*)
      (where (pop *string*) (setq where (parsesqlexp *string* 'select 'union)))
      (eof (pop *string*) (setq where 'true)))
    (if count `(length (select ,aspect ,from ,where))
        `(select ,aspect ,from ,where))))

(defun parsesqlfrom (s)
  (declare (ignore s))
  (do ((dum) (nl))
      (nil)
      (cond ((null *string*) (return (nreverse nl)))
            ((eq 'eof (car *string*)) (return (nreverse nl)))
            ((eq 'union (car *string*)) (return (nreverse nl)))
            ((eq 'where (car *string*)) (return (nreverse nl)))
            (t (setq dum (pop *string*))))
      (cond ((null *string*))
            ((eq 'eof (car *string*)))
            ((eq 'union (car *string*)))
            ((eq 'where (car *string*)))
            ((eq '\, (car *string*)))
            ((eq 'rparen (car *string*)))
            (t (setq dum (list dum (pop *string*)))))
      (setq nl (cons dum nl))
      (cond ((eq '\, (car *string*)) (pop *string*))
            (t (return (nreverse nl))))))



(setf (get '\. 'subordinates) '(* / + - in < > = >= =< not and or <= => <=> \, from where union rparen))
(setf (get '* 'subordinates) '(+ - in < > = >= =< not and or <= => <=> \, from where union rparen))
(setf (get '/ 'subordinates) '(+ - in < > = >= =< not and or <= => <=> \, from where union rparen))
(setf (get '+ 'subordinates) '(in < > = >= =< not and or <= => <=> \, from where union rparen))
(setf (get '- 'subordinates) '(in < > = >= =< not and or <= => <=> \, from where union rparen))
(setf (get 'in 'subordinates) '(not and or <= => <=> \, from where union rparen))
(setf (get '< 'subordinates) '(not and or <= => <=> \, from where union rparen))
(setf (get '> 'subordinates) '(not and or <= => <=> \, from where union rparen))
(setf (get '= 'subordinates) '(not and or <= => <=> \, from where union rparen))
(setf (get '=< 'subordinates) '(not and or <= => <=> \, from where union rparen))
(setf (get '>= 'subordinates) '(not and or <= => <=> \, from where union rparen))
(setf (get 'not 'subordinates) '(and or <= => <=> \, from where union rparen))
(setf (get 'and 'subordinates) '(and or <= => <=> \, from where union rparen))
(setf (get 'or 'subordinates) '(or <= => <=> \, from where union rparen))
(setf (get '\, 'subordinates) '(\, from where union rparen))
(setf (get 'select 'subordinates) '(from where union rparen))
(setf (get '<= 'subordinates) '(from where union rparen))
(setf (get '=> 'subordinates) '(from where union rparen))
(setf (get '<=> 'subordinates) '(from where union rparen))
(setf (get 'union 'subordinates) '(rparen))
(setf (get 'lparen 'subordinates) '(rparen))

(setf (get 'not 'prefix) t)
(setf (get '\. 'infix) t)
(setf (get '/ 'infix) t)
(setf (get '+ 'infix) t)
(setf (get '- 'infix) t)
(setf (get 'in 'infix) t)
(setf (get '< 'infix) t)
(setf (get '> 'infix) t)
(setf (get '=< 'infix) t)
(setf (get '>= 'infix) t)
(setf (get '<=> 'infix) t)
(setf (get 'union 'infix) t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; manager stuff
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun getbaskets ()
  (sort (finds '?x '(isa ?x basket) *interface*) #'lessp))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
