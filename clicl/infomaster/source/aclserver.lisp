;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; (c) Copyright 2001-2006 by Michael Genesereth.  All rights reserved.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; aclserver,lisp
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass aclserver (agent) ())

(defmethod message-handler (*message* *sender* (*receiver* aclserver))
  (rpc *message* *sender* *receiver*))

(defmethod rpc (msg sender (receiver aclserver))
  (with-open-stream
    (s (tcp-speaker (find-host (name receiver)) (find-port (name receiver))))
    (when (not (null s)) (sayacl msg s sender receiver) (hearacl s))))

(defun sayacl (msg s sender receiver)
  (when *trace*
    (fresh-line *trace*) (prin1 msg *trace*) (force-output *trace*))
  (setq msg (prin1-to-string (exportdata msg)))
  (format s "POST / HTTP/1.0") (crlf s)
  (format s "Accept: text/delim") (crlf s)
  (format s "Sender: ~A" (externalize sender)) (crlf s)
  (format s "Receiver: ~A" (externalize receiver)) (crlf s)
  (format s "Content-type: text/acl") (crlf s)
  (format s "Content-length: ~A" (length msg)) (crlf s)
  (crlf s)
  (format s "~A" msg)
  (finish-output s)
  'done)

(defun hearacl (s)
  (do () ((or (not (open-stream-p s)) (listen s))))
  (when (search "200" (get-http-line s) :test #'char=)
    (multiple-value-bind (type len client password) (parseheader s)
      (declare (ignore type client password))
      (importdata (read-user-string (read-content s len))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
