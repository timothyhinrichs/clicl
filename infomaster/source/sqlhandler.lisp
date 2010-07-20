;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; (c) Copyright 2001-2006 by Michael Genesereth.  All rights reserved.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; sqlhandler.lisp
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun sql-handler (s msg)
  (let (answer)
    (setq answer (tdt (request (acl msg) *client* *agent*)))
    (when *trace*
      (fresh-line *trace*) (prin1 msg *trace*) (force-output *trace*))
    (format s "HTTP/1.0 200 OK") (crlf s)
    (format s "Content-type: text/sql") (crlf s)
    (format s "Content-length: ~A" (length answer)) (crlf s)
    (crlf s)
    (format s "~A" answer)
    (finish-output s)
    'done))

(defun tdt (answer)
  (with-output-to-string (s)
    (cond ((atom answer) (princ answer s))
          (t (dolist (line answer) (writecdtline line #\tab s) (terpri s))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
