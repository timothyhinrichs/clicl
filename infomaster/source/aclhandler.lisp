;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; (c) Copyright 2001-2006 by Michael Genesereth.  All rights reserved.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; aclhandler.lisp
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun acl-handler (s msg)
  (let (answer)
    (setq msg (importdata (read-user-string msg)))
    (setq answer (ignore-errors (request msg *client* *receiver*)))
    (setq answer (prin1-to-string (exportdata  answer)))
    (when *trace*
      (fresh-line *trace*) (prin1 msg *trace*) (force-output *trace*))
    (format s "HTTP/1.0 200 OK") (crlf s)
    (format s "Content-type: text/acl") (crlf s)
    (format s "Content-length: ~A" (length answer)) (crlf s)
    (crlf s)
    (format s "~A" answer)
    (finish-output s)
    'done))

(defun exportdata (data)
  (do ((l (find-all-agents) (cdr l)) (al))
      ((null l) (sublis al data))
      (setq al (acons (car l) (intern (externalize (car l))) al))))

(defun importdata (data)
  (do ((l (find-all-agents) (cdr l)) (al))
      ((null l) (sublis (nreverse al) data))
      (setq al (acons (intern (externalize (car l))) (car l) al))))

(defun find-all-agents ()
  (finds '?x `(isa ?x agent) *manager*))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
