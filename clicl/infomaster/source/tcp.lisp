;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; (c) Copyright 1992-1999 Michael R. Genesereth.  All rights reserved.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(setq *long-site-name* (machine-instance))

(defparameter *host* *long-site-name*)

(defparameter *ip* "0.0.0.0")

(defparameter *port* 4800)

(defun processp (x) (ccl::processp x))

(defun process-msecs ()
  (* (/ (process-total-run-time *current-process*) 60) 1000))

(defun tcp-servers (function port &optional (n 10))
  (do ((i 1 (1+ i)))
      ((> i n))
      (process-run-function "TCP Loop" #'tcp-loop function port)))

(defun tcp-loop (function port)
  (loop (ignore-errors (tcp-handler function port))))

(defun tcp-handler (function port)
  (let (hostaddr)
    (with-open-stream (s (open-tcp-stream nil port))
      (process-wait "TCP Handler" #'stream-listen s)
      (setq hostaddr (tcp-addr-to-str (stream-remote-host s)))
      (funcall function s port hostaddr))))

(defun tcp-server (function port)
  (do ((s (open-tcp-stream nil port)) (hostaddr))
      (nil)
      (process-wait "Listening" #'stream-listen s)
      (setq hostaddr (tcp-addr-to-str (stream-remote-host s)))
      (process-run-function "TCP Handler" function s port hostaddr)
      (setq s (open-tcp-stream nil port))))

(defun start-tcp-server-process (function port)
  (process-run-function "TCP Server" #'tcp-server function port))

(defun tcp-server-thing (f port &optional (n 10))
  (let (streams)
    (do ((i 1 (1+ i)))
        ((> i n))
        (setq streams (cons (open-tcp-stream nil port) streams)))
    (do ((l streams) (stream) (hostaddr))
        (nil)
        (cond ((null l) (setq l streams))
              ((listen (car l))
               (setq stream (car l))
               (rplaca l (open-tcp-stream nil port))
               (setq l (cdr l))
               (setq hostaddr (tcp-addr-to-str (stream-remote-host stream)))
               (process-run-function "HTTP Handler" f stream port hostaddr))
              (t (setq l (cdr l)))))))

(defun tcp-shutdown ()
  (dolist (process *all-processes*)
    (when (equalp (process-name process) "TCP Loop")
      (process-kill process))))

(defun tcp-stream-shutdown ()
  (dolist (x ccl::*open-opentransport-streams*) (close x)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; new
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun tcp-server-thing (f port &optional (n 30))
  (let (streams)
    (do ((i 1 (1+ i)))
        ((> i n))
        (setq streams (cons (open-tcp-stream nil port) streams)))
    (do ((l streams) (stream) (hostaddr))
        (nil)
        (cond ((null l) (setq l streams))
              ((listen (car l))
               (setq stream (car l))
               (rplaca l (open-tcp-stream nil port))
               (setq l (cdr l))
               (setq hostaddr (tcp-addr-to-str (stream-remote-host stream)))
               (process-run-function "HTTP Handler" f stream port hostaddr))
              (t (setq l (cdr l)))))))


(defparameter *connection-attempts* 5)

(defun open-connection (host port)
  (do ((i 1 (1+ i)) (stream))
      ((> i *connection-attempts*) 
       (problem "Connection to ~A:~A failed." host port))
      (if (setq stream (ignore-errors (open-tcp-stream host port)))
          (return stream))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Stream operations for tcp connections.
;;; Also, remember the following stream subroutine:
;;;   (clear-input stream)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defparameter *trace* nil)

(defun send (msg stream)
  (when *trace*
    (fresh-line *trace*) (prin1 msg *trace*) (force-output *trace*))
  (let ((*print-pretty* nil)) (prin1 msg stream))
  (write-char #\return stream)
  (finish-output stream)
  'done)

(defun receive (stream)
  (do ((msg (accept stream) (accept stream)))
      (msg msg)))

(defun accept (stream)
  (let (msg)
    (when (listen stream)
      (setq msg (read stream))
      (when *trace* (fresh-line *trace*)
            (prin1 msg *trace*) (force-output *trace*))
      msg)))

(defun ip-to-number (a b c d)
  (+ (* (expt 256 3) a)
     (* (expt 256 2) b)
     (* (expt 256 1) c)
     d))

(defun number-to-ip (n)
  (list (floor n (expt 256 3))
        (floor (mod n (expt 256 3)) (expt 256 2))
        (floor (mod n (expt 256 2)) 256)
        (mod n 256)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Mailers
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun mail (from to subject body server)
  (with-open-stream (s (open-tcp-stream server 25))
    (sendmsg "helo sequel" s)
    (sendmsg (format nil "mail from: ~a" from)  s)
    (sendmsg (format nil "rcpt to: ~a" to) s)
    (sendmsg "data" s)
    (sendmsg (format nil "Subject: ~a" subject) s)
    (sendmsg (format nil "Errors-To: ~a" from) s)
    (sendmsg (format nil "Reply-To: ~a"  from) s)
    (sendmsg (format nil "~a" body)  s)
    (sendmsg "." s)
    (sleep 1)
    (clear-input s)
    (close s :abort t)
    'done))

(defun sendmsg (msg stream)
  (format stream "~a~a~a" msg #\Return #\linefeed)
  (finish-output stream)
  t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
