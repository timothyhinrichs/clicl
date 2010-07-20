;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; (c) Copyright 2001-2008 by Michael Genesereth.  All rights reserved.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; remote.lisp
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Stubs for remote objects accessible via tcp
;;;   (setq *name* (make-remote 'name "logic" 4010))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass remote ()
  ((name :initarg :name :accessor name :initform nil)
   (host :initarg :host :accessor host :initform nil)
   (port :initarg :port :accessor port :initform nil)))

(defun remotep (x) (typep x 'remote))

(defun make-remote (name host port)
  (make-instance 'remote :name name :host host :port port))

(defmethod message (msg sender (receiver remote))
  (when *wiretap* (fresh-line *wiretap*)
    (format *wiretap* "~A --> ~B:" (name sender) (name receiver))
    (fresh-line *wiretap*) (format *wiretap* "~S" msg)
    (terpri *wiretap*) (force-output *wiretap*))
  (transmit msg sender receiver))

(defmethod request (msg sender (receiver remote))
  (when *wiretap* (fresh-line *wiretap*)
    (format *wiretap* "~A --> ~B:" (name sender) (name receiver))
    (fresh-line *wiretap*) (format *wiretap* "~S" msg)
    (terpri *wiretap*) (force-output *wiretap*))
  (setq msg (rpc msg sender receiver))
  (when *wiretap* (fresh-line *wiretap*)
    (format *wiretap* "~A <-- ~B:" (name sender) (name receiver))
    (fresh-line *wiretap*) (format *wiretap* "~S" msg)
    (terpri *wiretap*) (force-output *wiretap*))
  msg)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Knowledge commands
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; affirm, retract
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod affirm (p sender (receiver remote))
  (rpc `(affirm ,p) sender receiver))

(defmethod retract (p sender (receiver remote))
  (rpc `(retract ,p) sender receiver))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; askabout
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod askabout (x sender (receiver remote))
  (rpc `(rules ,x) sender receiver))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Data commands
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; revise, revisions, errors, materializations, notifications, reactions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod revise (p sender (receiver remote))
  (rpc `(revise ,p) sender receiver))

(defmethod errors (p (receiver remote))
  (rpc `(errors ,p) nil receiver))

(defmethod revisions (p (receiver remote))
  (rpc `(revisions ,p) nil receiver))

(defmethod materializations (p (th remote))
  (declare (ignore p th))
  nil)

(defmethod notifications (p (receiver remote))
  (rpc `(notifications ,p) nil receiver))

(defmethod reactions (p (receiver remote))
  (rpc `(reactions ,p) nil receiver))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; askp, askx, asks
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod askp (p sender (receiver remote))
  (rpc `(askp ,p) sender receiver))

(defmethod askx (x p sender (receiver remote))
  (rpc `(askx ,x ,p) sender receiver))

(defmethod asks (x p sender (receiver remote))
  (rpc `(asks ,x ,p) sender receiver))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; askclass, askframe, asktable
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod askclass (x sender (receiver remote))
  (rpc `(askclass ,x) sender receiver))

(defmethod askframe (x class sender (receiver remote))
  (rpc `(askclass ,x ,class) sender receiver))

(defmethod asktable (ol sl sender (receiver remote))
  (rpc `(asktable ,ol ,sl) sender receiver))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; remote communication subroutines
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defparameter *error* nil)

(defun problem (str &rest args)
  (cond ((eq *error* 'abort) (apply #'error str args))
        ((eq *error* 'break) (apply #'cerror str args))
        ((eq *error* 'warn) (apply #'warn str args))))

(defmethod transmit (msg sender (receiver remote))
  (ignore-errors
   (with-open-stream (stream (tcp-speaker (host receiver) (port receiver)))
     (when stream
       (send `(package :content ,msg
                       :sender ,(name sender)
                       :receiver ,(name receiver))
             stream)))))

(defmethod rpc (msg sender (receiver remote))
  (let ((id (newmsgid sender)))
    (ignore-errors
     (with-open-stream (stream (tcp-speaker (host receiver) (port receiver)))
       (when stream
         (send `(package :content ,msg
                         :sender ,(name sender)
                         :receiver ,(name receiver)
                         :reply-with ,id)
               stream)
         (setq msg (receive stream))
         (if (equalp id (getf (cdr msg) :in-reply-to)) (reply msg)))))))

(defun reply (pkg)
  (let ((content))
    (cond ((atom pkg) pkg)
          ((atom (setq content (getf (cdr pkg) :content))) content)
          ((eq 'reply (car content)) (cadr content))
          (t content))))

(defun newmsgid (sender)
  (format nil "~A-~A" (name sender) (get-universal-time)) )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; manager stuff
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun find-host (x)
  (findx '?x `(host ,x ?x) *manager*))

(defun find-port (x)
  (findx '?x `(port ,x ?x) *manager*))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
