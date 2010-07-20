;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; (c) Copyright 1999-2000 by Michael R. Genesereth.  All rights reserved.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; XDF Files
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun readxdf (str)
  (parsexdf (scanxml str)))

(defun parsexdf (*string*)
  (xdfexp *string*))

(defun xdfexp (s)
  (declare (ignore s))
   (let (start)
     (setq start (xdfsingle *string*))
     (cond ((atom start) nil)
           ((eq 'single (car start)) nil)
           ((and (eq 'open (car start)) (eq 'xml (cadr start)))
            (do ((dum) (nl))
                ((null *string*) (cons 'xml (nreverse nl)))
                (setq dum (xdfsingle *string*))
                (cond ((atom dum))
                      ((eq 'single (car dum)) (setq nl (cons (cdr dum) nl)))
                      ((eq 'open (car dum)) (xdfbroad *string* (cdr dum))
                       (xdfbroad *string* (cdr dum)))
                      ((and (eq 'close (car dum)) (equalp 'xml (cadr dum)))
                       (return (cons 'xml (nreverse nl)))))))
           ((eq 'close (car start)) nil))))

(defun xdfbroad (s left)
    (declare (ignore s))
    (do ((dum) (nl))
        ((null *string*) (cons 'single (cons left (nreverse nl))))
        (setq dum (xdfsingle *string*))
        (cond ((null dum) (setq nl (cons `(pcdata ,(pop *string*)) nl)))
              ((atom dum)
               (setq nl (cons 'single (cons dum (nreverse nl)))))
              ((eq 'single (car dum))
               (setq nl (cons (cdr dum) nl)))
              ((eq 'open (car dum)) (xdfbroad *string* (cdr dum))
               (setq nl (cons (cdr (xdfbroad *string* (cdr dum))) nl)))
              ((and (eq 'close (car dum)) (equalp (car left) (cadr dum)))
               (return (cons 'single (nconc left (list `(= elements ,(nreverse nl))))))))))

(defun xdfsingle (s)
  (declare (ignore s))
  (cond ((eq 'lparen (car *string*))
         (pop *string*)
         (do ((nl))
             ((null *string*) (cons 'single (nreverse nl)))
             (cond ((eq 'rparen (car *string*))
                    (pop *string*)
                    (return (cons 'open (nreverse nl))))
                   ((eq 'rclose (car *string*))
                    (pop *string*)
                    (return (cons 'single (nreverse nl))))
                   ((eq '= (cadr *string*))
                    (setq nl (cons `(= ,(car *string*) ,(caddr *string*)) nl))
                    (pop *string*) (pop *string*) (pop *string*))
                   (t (setq nl (cons (pop *string*) nl))))))
        ((eq 'lclose (car *string*))
         (pop *string*)
         (do ((nl))
             ((null *string*) (cons 'close (nreverse nl)))
             (cond ((or (eq 'rparen (car *string*)) (eq 'rclose (car *string*)))
                    (pop *string*)
                    (return (cons 'close (nreverse nl))))
                   (t (setq nl (cons (pop *string*) nl))))))))


(defun xdftokif (x)
  (cond ((atom x) x)
        ((eq 'xml (car x))
         (do ((l (cdr x) (cdr l)) (id) (val) (nl))
             ((null l) (nreverse nl))
             (when (eq 'id (cadr (cadar l)))
               (setq id (read-from-string (caddr (cadar l))))
               (setq nl (cons (makpred id (caar l) *manager*) nl))
               (dolist (pair (cdar l))
                 (unless (eq 'id (cadr pair))
                   (setq val (read-from-string (caddr pair)))
                   (setq nl (cons (list (cadr pair) id val) nl)))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
