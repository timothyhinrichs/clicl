;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; (c) Copyright 2001-2008 by Michael Genesereth.  All rights reserved.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; soapserver.lisp
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass soapserver (agent) ())

(defmethod message-handler (*message* *sender* (*receiver* soapserver))
  (rpcsoap *message* *sender* *receiver*))

(defun rpcsoap (msg sender receiver &optional host port)
  (unless host (setq host (find-host (name receiver))))
  (unless port (setq port (find-port (name receiver))))
  (with-open-stream (s (tcp-speaker host port))
    (when (not (null s)) (saysoap msg s sender receiver) (hearsoap s))))

(defun saysoap (msg s sender receiver)
  (setq msg (soap-envelope msg))
  (when *trace*
    (fresh-line *trace*) (princ msg *trace*) (force-output *trace*))
  (format s "POST / HTTP/1.1") (crlf s)
  (format s "Accept: text/delim") (crlf s)
  (format s "Sender: ~A" (externalize sender)) (crlf s)
  (format s "Receiver: ~A" (externalize receiver)) (crlf s)
  (format s "Content-type: application/soap") (crlf s)
  (format s "Content-length: ~A" (length msg)) (crlf s)
  (crlf s)
  (format s "~A" msg)
  (finish-output s)
  'done)

(defun hearsoap (s)
  (do () ((or (not (open-stream-p s)) (listen s))))
  (when (search "200" (get-http-line s) :test #'char=)
    (multiple-value-bind (type len client password)
      (let (*client* *password* *receiver* *cookies*) (parseheader s))
      (declare (ignore client password))
      (setq type (read-soap s len))
      (when *trace*
        (fresh-line *trace*) (princ type *trace*) (force-output *trace*))
      (cadr (unxclify (soap-content (readxml type)))))))

(defun read-soap (s len &optional (timeout 10))
  (with-output-to-string (out)
    (do ((i 1) (time) (chr nil (read-char-no-hang s nil 'eof)))
        ((or (eq chr 'eof) (> i len)))
        (cond ((null chr) 
	       (unless time (setq time (get-universal-time)))
	       (when (> (get-universal-time) (+ timeout time)) 
		     (print "Stream time out on read" *terminal-io*)
		     (error "Stream time out on read")))
              (t (write-char chr out) (setq i (1+ i) time nil))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
