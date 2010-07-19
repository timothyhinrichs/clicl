;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; (c) Copyright 2001-2008 by Michael Genesereth.  All rights reserved.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; sqlserver.lisp
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass sqlserver (agent) ())

(defmethod message-handler (*message* *sender* (*receiver* sqlserver))
  (cond ((atom *message*) nil)
        ((eq 'ask-if (car *message*))
         (not (null (sqlaskall *message* *sender* *receiver*))))
        ((eq 'ask-one (car *message*))
         (car (sqlaskall *message* *sender* *receiver*)))
        ((eq 'ask-all (car *message*))
         (sqlaskall *message* *sender* *receiver*))
        ((and (eq 'length (car *message*))
              (listp (cadr *message*)) (eq 'ask-all (caadr *message*)))
         (caar (rpc *message* *sender* *receiver*)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; loadschema and dumpschema
;;; <B>loadloadschema</B> takes as arguments an sqlserver and a local agent.
;;; and loads the schema from the sqlserver into the specified agent.
;;;
;;; <B>dumploadschema</B> takes as arguments a data source, a local agent, and
;;; a filename.  It dumps the schema for the specified source from the specified
;;; local agent to the specified file.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun loadsqlschema (db th)
  (do ((l (gettables db) (cdr l)) (i 1 (1+ i)))
      ((null l) 'done)
      (loadtableschema (caar l) db th)))

(defun dumpsqlschema (db th fn)
  (with-open-file (s fn :direction :output :if-exists :supersede)
    (do ((l (finds '?x `(specialty ,db ?x) th) (cdr l)))
        ((null l) 'done)
        (dumptableschema (car l) db th s))))
  
(defun loadtableschema (tablename db th)
  (let (table columns)
    (setq table (intern (strappend (symbol-name (name db)) "."
                                   (string-upcase tablename))))
    (insert `(specialty ,(name db) ,table) th)
    (insert `(isa ,table relation) th)
    (setq columns (getcolumns tablename db))
    (insert `(arity ,table ,(length columns)) th)
    (do ((m columns (cdr m)) (column))
        ((null m))
        (setq column (intern (strappend (symbol-name table) "." 
                                        (string-upcase (caar m)))))
      (insert `(column ,table ,column) th))
    (do ((m columns (cdr m)) (column))
        ((null m))
        (setq column (intern (strappend (symbol-name table) "." 
                                        (string-upcase (caar m)))))
        (insert `(prettyname ,column ,(caar m)) th))
    (insert `(prettyname ,table ,tablename) th)
    'done))

(defun dumptableschema (table db th s)
  (let (icon columns)
    (prin1 `(specialty ,db ,table) s) (terpri s)
    (prin1 `(isa ,table relation) s) (terpri s)
    (setq columns (finds '?x `(column ,table ?x) th))
    (prin1 `(arity ,table ,(length columns)) s) (terpri s)
    (do ((l columns (cdr l)))
        ((null l))
        (prin1 `(column ,table ,(car l)) s) (terpri s))
    (do ((l columns (cdr l)) (icon))
        ((null l))
        (when (setq icon (findx '?x `(icon ,(car l) ?x) th))
          (prin1 `(icon ,(car l) ,icon) s) (terpri s)))
    (when (setq icon (findx '?x `(prettyname ,table ?x) th))
      (prin1 `(prettyname ,table ,icon) s) (terpri s))
    'done))

(defun gettables (db)
  (spc "TABLE" nil (referent db)))

(defun getcolumns (tablename db)
  (spc (strappend "COLUMNS=" tablename) nil (referent db)))

(defun sqlaskall (msg sender receiver)
  (cond ((atom (cadr msg)) (mapcar #'car (sqlaskall1 msg sender receiver)))
        (t (sqlaskall1 msg sender receiver))))

(defun sqlaskall1 (msg sender receiver)
  (cond ((and (eq 'and (caaddr msg)) (eq 'oneof (caadr (caddr msg))))
         (sqlaskall2 (cadr msg) (caddr msg) sender receiver))
        (t (rpc msg sender receiver))))

(defparameter *sqlsize* 200)

(defun sqlaskall2 (x p sender receiver)
  (let ((vars) (vals) (news) (ins) (invals) (outs) (outvals) (columns))
    (setq vars (vars (cadadr p)) vals (cddadr p))
    (setq x (vars x) p (maksand (cddr p)))
    (setq news (vars p))
    (cond ((setq ins (intersect vars news))
           (do ((l vars (cdr l)) (i 1 (1+ i)) (nl))
               ((null l) (setq invals (apply #'project vals (nreverse nl))))
               (when (find (car l) news) (setq nl (cons i nl)))))
          (t (setq ins '(1) invals '((1)))))
    (setq news (goodvars p))
    (setq outs (sqleconomize (intersect (unionize ins x) news)))
    (do ((l invals (nthcdr *sqlsize* l)) (dum) (nl))
        ((null l) (setq outvals (nreverse nl)))
        (setq dum (makand `(oneof ,ins . ,(sqlsubseq l 1 *sqlsize*)) p))
        (setq nl (nreconc (rpc `(ask-all ,outs ,dum) sender receiver) nl)))
    (cond ((subsetp vars outs) (setq vals outvals))
          (t (do ((l vars (cdr l)) (i 1 (1+ i)) (dum) (nl))
                 ((null l) (setq columns (nreverse nl)))
                 (when (setq dum (position (car l) outs))
                   (setq nl (cons (1+ dum) (cons i nl)))))
             (setq vals (apply #'join vals outvals columns))
             (setq outs (unionize vars outs))))
    (do ((l x (cdr l)) (dum) (nl))
        ((null l) (apply #'project vals (nreverse nl)))
        (when (setq dum (position (car l) outs))
          (setq nl (cons (1+ dum) nl))))))

(defun sqleconomize (aspect)
  (cond ((null aspect) '(1))
        ((atom aspect) (list aspect))
        (t aspect)))

(defun sqlsubseq (items start end)
  (do ((l items (cdr l)) (i start (1+ i)) (nl))
      ((or (null l) (> i end)) (nreverse nl))
      (setq nl (cons (car l) nl))))

(defmethod transmit (msg sender (receiver sqlserver))
  (declare (ignore sender))
  (ignore-errors
   (with-open-stream (s (tcp-speaker (host receiver) (port receiver)))
     (when (not (null s)) (speak msg s)))))

(defmethod rpc (msg sender (receiver sqlserver))
  (declare (ignore sender))
  (ignore-errors
   (with-open-stream (s (tcp-speaker (host receiver) (port receiver)))
     (when (not (null s)) (speak (sql msg) s) (hear s receiver)))))

(defmethod spc (msg sender (receiver sqlserver))
  (declare (ignore sender))
  (ignore-errors
   (with-open-stream (s (tcp-speaker (host receiver) (port receiver)))
     (when (not (null s)) (speak msg s) (hear s receiver)))))

(defun speak (msg s)
  (when *trace*
    (fresh-line *trace*) (prin1 msg *trace*) (force-output *trace*))
  (format s "POST / HTTP/1.0") (crlf s)
  (format s "Accept: text/delim") (crlf s)
  (format s "Content-type: text/sql") (crlf s)
  (format s "Content-length: ~A" (length msg)) (crlf s)
  (crlf s)
  (format s "~A" msg)
  (finish-output s)
  'done)

(defun hear (s receiver)
  (do () ((or (not (open-stream-p s)) (listen s))))
  (cond ((search "200" (get-http-line s) :test #'char=)
         (do ((line (get-http-line s) (get-http-line s)))
             ((or (null line) (equal line "")) (when *trace* (fresh-line)))
             (when *trace* (fresh-line) (princ line)))
         (do ((line (get-line s) (get-line s)) (nl))
             ((equal line "") (nreverse nl))
             (when *trace* (fresh-line) (princ line))
             (unless (equal line "") (setq nl (cons (parseline line) nl)))))
        (t (format nil "Bad answer from ~A." (name receiver)))))

(defun get-line (s)
  (with-output-to-string (out)
    (do ((c))
        (nil)
        (setq c (read-char s nil nil))
        (cond ((null c) (return nil))
              ((char= c #\return) (return t))
              ((char= c #\escape) (write-char (read-char s) out))
              (t (write-char c out))))))

(defun parseline (s)
  (do ((old 0) (new (position #\tab s) (position #\tab s :start old)) (nl))
      ((null new) (nreverse (cons (get-from-string s old new) nl)))
      (setq nl (cons (get-from-string s old new) nl) old (1+ new))))

(defun get-from-string (s beg end)
  (read-from-string s nil nil :start beg :end end))

(defmethod host ((receiver sqlserver))
  (findx '?x `(host ,(name receiver) ?x) *manager*))

(defmethod port ((receiver sqlserver))
  (findx '?x `(port ,(name receiver) ?x) *manager*))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
