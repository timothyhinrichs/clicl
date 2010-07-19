;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; (c) Copyright 1999-2000 by Michael R. Genesereth.  All rights reserved.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; NDF files
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod loadrelation (rel fn (target theory) (lang (eql 'ndf)) meta)
  (loadrelationndf fn target rel meta))

(defmethod dumprelation (rel (source theory) fn (lang (eql 'ndf)))
  (dumprelationndf rel source fn))

(defun loadrelationndf (fn th rel meta)
  (with-open-file (s fn :direction :input)
    (do ((line (read-line s nil nil) (read-line s nil nil)))
        ((null line) 'done)
        (unless (equal line "")
          (setq line (cons rel (readndfline line)))
          (when (eq meta 'dynamic) (fancyinsert line th))
          (insert line th)))))

(defun readndfline (s)
  (do ((item) (pos 0) (len (length s)) (nl))
      ((geqp pos len) (nreverse nl))
      (multiple-value-setq (item pos) (read-from-string s nil nil :start pos))
      (setq nl (cons item nl))))

(defun dumprelationndf (rel th fn)
  (with-open-file (f fn :direction :output :if-exists :supersede)
    (do ((l (indexees rel th) (cdr l)))
        ((null l) 'done)
        (when (and (listp (car l)) (eq rel (caar l)))
          (writendfline (cdar l) f)
          (terpri f)))))

(defun writendfline (line s)
  (cond ((null line))
        (t (princ (car line) s)
           (do ((l (cdr line) (cdr l)))
               ((null l) line)
               (write-char #\tab s)
               (prin1 (car l) s)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
