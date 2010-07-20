;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; (c) Copyright 2001-2008 by Michael Genesereth.  All rights reserved.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; soap.lisp
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun soap-handler (s msg)
  (let (answer error)
    (setq msg (ignore-errors (unxclify (soap-content (readxml msg)))))
    (multiple-value-setq (answer error)
      (ignore-errors (request msg *client* *receiver*)))
    (cond ((not error)
           (setq answer (soap-envelope `(reply ,answer)))
           (format s "HTTP/1.1 200 OK") (crlf s)
           (format s "Content-type: application/soap") (crlf s)
           (format s "Content-length: ~A" (length answer)) (crlf s)
           (crlf s)
           (format s answer)
           'done)
          (t (setq answer (soap-receiver-error))
             (format s "HTTP/1.1 500 Internal Server Error") (crlf s)
             (format s "Content-type: application/soap") (crlf s)
             (format s "Content-length: ~A" (length answer)) (crlf s)
             (crlf s)
             (format s answer)
             'done))))

(defun soap-content (x)
  (cond ((atom x) x)
        ((atom (car x)) nil)
        ((eq (caar x) 'content) (cadr x))
        (t (do ((l (cdr x) (cdr l)) (dum))
               ((null l) nil)
               (when (setq dum (soap-content (car l))) (return dum))))))

(defun readxml (s)
  (parsexml (scanxml (substitute #\$ #\: s))))

(defun soap-envelope (x)
  (format nil "<env:Envelope xmlns:env=\"http://www.w3.org/2001/12/soap-envelope\"><env:Body><content>~%~A</content></env:Body></env:Envelope>"
          (xclify x)))

(defun soap-receiver-error ()
  "<env:Envelope xmlns:env=\"http://www.w3.org/2001/12/soap-envelope\"><env:Fault><faultcode>Receiver</faultcode></env:Fault></env:Envelope>")          

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
