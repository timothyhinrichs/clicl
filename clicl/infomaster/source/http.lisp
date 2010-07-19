;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; (c) Copyright 1999-2006 by Michael Genesereth.  All rights reserved.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(eval-when (compile load eval)
  (proclaim '(special *receiver* *agent* *manager*)))

(defparameter *lock* (make-lock))
(defparameter *logfile* nil)
(defparameter *remote* "0.0.0.0")
(defparameter *browser* 'generic)
(defparameter *client* 'anonymous)
(defparameter *password* "anonymous")
(defparameter *cookies* nil)
(defparameter *max-line-size* 1000000)

(defparameter *http-status-codes*
    '(;; 1xx: Informational
      
      ;; 2xx: Success
      (200 . "OK")
      (201 . "Created")
      (202 . "Accepted")
      (203 . "Provisional information")
      (204 . "No content")
      (205 . "Reset content")

      ;; 3xx: Redirection
      (300 . "Multiple choices")
      (301 . "Moved permanently")
      (302 . "Moved temporarily")
      (303 . "Method")
      (304 . "Not modified")

      ;; 4xx: Client error
      (400 . "Bad request")
      (401 . "Unauthorized")
      (402 . "Payment required")
      (403 . "Forbidden")
      (404 . "Not found")
      (405 . "Method not allowed")
      (406 . "None acceptable")
      (407 . "Proxy authentication required")
      (408 . "Request timeout")
      (409 . "Conflict")
      (410 . "Gone")

      ;; 5xx: Server error
      (500 . "Internal server error")
      (501 . "Not implemented")
      (502 . "Bad gateway")
      (503 . "Service unavailable")
      (504 . "Gateway timeout")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; http-handler
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun http-handler (s port &optional (ip 0))
  (declare (type stream s))
  (declare (ignore port))
  (with-open-stream (s s)
    (ignore-errors (process-http s ip))
    (ignore-errors (finish-output s))
    (ignore-errors (close s))))

(defun process-http (s *remote*)
  (let (start end firstline postline command path protocol
        type contentlength dir file query
        (*client* *client*) (*password* *password*)
        *cookies* (*browser* 'generic) (*receiver* *agent*))
    (setq start (get-universal-time))
    (unless (setq firstline (get-http-line s))
      (format s "HTTP/1.0 400 Bad request") (crlf s)
      (return-from process-http))
    (multiple-value-setq (command path protocol) (parsefirstline firstline))
    (unless command
      (format s "HTTP/1.0 400 Bad request") (crlf s)
      (return-from process-http) )
    (cond (protocol (multiple-value-setq (type contentlength) (parseheader s)))
          (t (setq type 'x-www-form-urlencoded contentlength *max-line-size*)))
    (cond ((eq type 'acl) (acl-handler s (read-content s contentlength)))
          ((eq type 'sql) (sql-handler s (read-content s contentlength)))
          ((eq type 'soap) (soap-handler s (read-content s contentlength)))
          ((eq type 'form-data)
           (multiple-value-setq (dir file query) (parsepath path))
           (html-handler s dir file (readmultipart s contentlength) type))
          (t (multiple-value-setq (dir file query) (parsepath path))
             (if (string= command "POST")
               (setq postline (get-http-line s contentlength))
               (setq postline query))
             (html-handler s dir file postline nil)
             (when *logfile*
               (setq end (get-universal-time))
               (loghttp start end *remote* *browser* *client* firstline postline))))))

(defun parsefirstline (string)
  "Returns: (1) Command, (2) Path, (3) Protocol"
  (declare (type string string))
  (let (space-pos space2-pos)
    (cond ((not (setq space-pos (position #\space string)))
           (values nil nil nil))
          ((setq space2-pos (position #\space string :start (1+ space-pos)))
           (values (subseq string 0 space-pos)
	           (subseq string (1+ space-pos) space2-pos)
	           (subseq string (1+ space2-pos))))
          (t (values (subseq string 0 space-pos)
                     (subseq string (1+ space-pos))
                     nil)))))

(defun parseheader (s)
  (do ((line) (pos) (type) (len *max-line-size*))
      (nil)
      (setq line (string-left-trim '(#\space #\linefeed #\return) (get-http-line s)))
      (cond ((string= line "") (return (values type len)))
            ((and (startstringp "Authorization" line)
                  (setq line (base64-to-string (subseq line 21)))
                  (setq pos (position #\: line)))
             (setq *client* (read-user-string (subseq line 0 pos)))
             (setq *password* (subseq line (1+ pos)))
             (when (or (null *client*) (null *password*))
               (setq *client* 'anonymous *password* "anonymous")))
            ((startstringp "Webauthproxy" line)
             (setq *client* (read-user-string (subseq line 14 (1- (length line)))))
             (setq *password* "webauth"))
            ((startstringp "Receiver" line)
	     (setq *receiver* (internalize (subseq line 10))))
            ((startstringp "User-Agent" line)
             (setq *browser* (parsebrowser (subseq line 12))))
            ((startstringp "Cookie" line)
             (setq *cookies* (parsecookies (subseq line 8))))
            ((startstringp "Content-type" line)
             (when (setq pos (position #\/ line))
               (setq type (read-value-string (subseq line (1+ pos))))))
            ((startstringp "Content-length" line)
             (setq line (subseq line 16))
	     (when (not (find-if-not #'digit-char-p line))
	       (setq len (read-from-string line)))))))

(defun parsebrowser (s)
  (cond ((substringp "MSIE 6" s) 'explorer)
        ((substringp "MSIE 7" s) 'explorer)
        ((substringp "Safari/3" s) 'safari)
        ((substringp "Safari/4" s) 'safari)
        ((substringp "Firefox" s) 'firefox)
        ((substringp "Netscape/8" s) 'netscape)
        ((substringp "Camino" s) 'camino)
        ((substringp "Opera/9" s) 'opera)
        ((substringp "Flock" s) 'flock)
        ((substringp "Konqueror/3.5" s) 'konqueror)
        (t 'generic)))

(defun parsecookies (line)
  (do ((start 0) (end (length line)) (eq) (sc) (nl))
      ((>= start end) (nreverse nl))
      (cond ((whitep (elt line start)) (setq start (1+ start)))
            ((setq eq (position #\= line :start start))
             (setq sc (or (position #\; line :start start) end))
             (when (< eq sc)
               (setq nl (cons (cons (decode-url-chrs (subseq line start eq))
                                    (decode-url-chrs (subseq line (1+ eq) sc)))
                                     nl)))
             (setq start (1+ sc)))
            (t (setq start end)))))

(defun parsepath (path)
  "Returns: (1) Directory, (2) File, (3) Query"
  (declare (type string path))
  (let (q-pos slash-pos dir file query)
    (cond ((setq q-pos (position #\? path))
	   (setq query (subseq path (1+ q-pos)))
	   (setq path (subseq path 0 q-pos)))
          (t (setq query "")))
    (cond ((setq slash-pos (position #\/ path :from-end t))
           (setq dir (subseq path 0 (1+ slash-pos)))
	   (setq file (subseq path (1+ slash-pos))))
          (t (setq dir "/" file path)))
    (cond (q-pos)
          ((string-equal file ""))
          ((string-equal file "favicon.ico") (setq file "Favicon"))
          (t (setq query (strappend "Object=" file) file "Fastinspectpage")))
    ;(setq dir (decode-url-chrs dir))
    ;(setq file (decode-url-chrs file))
    ;(setq query (decode-url-chrs query))
    (values dir file query)))

(defparameter *messagebreak* "



")

(defun readmultipart (s contentlength)
  (let (input)
    (setq input
          (with-output-to-string (str)
            (do ((i 1 (1+ i)))
                ((> i contentlength) )
                (write-char (read-char s) str))))
    (parsemultidata input)))

(defun parsemultidata (body)
  (let (boundary beg end postlines)
    (setq boundary (with-input-from-string (s body) (read-line s)))
    (setq beg 1 end 1)
    (loop (setq beg (search *messagebreak* body :start2 end))
          (when (not (integerp beg)) (return (nreverse postlines)))
          (setq end (search boundary body :start2 beg))
          (when (not (integerp end)) (return (nreverse postlines)))
          (setq postlines (acons "Foo" (subseq body (+ beg 4) (- end 2)) postlines)))))

(defun loghttp (start end remote browser client first post)
  (with-lock-grabbed (*lock*)
    (with-open-file
      (log *logfile* :direction :io :if-exists :append :if-does-not-exist :create)
      (prin1 start log) (write-char #\tab log)
      (prin1 end log) (write-char #\tab log)
      (prin1 remote log) (write-char #\tab log)
      (prin1 browser log) (write-char #\tab log)
      (prin1 client log) (write-char #\tab log)
      (prin1 first log) (write-char #\tab log)
      (prin1 post log) (terpri log))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Reader stuff
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun get-http-line (s &optional (max-chars *max-line-size*) (timeout 10000))
  (declare (type stream s))
  (declare (type (integer 0 *) max-chars))
  (declare (optimize speed (safety 1) (space 0) (debug 0)))
  (with-output-to-string (out)
    (do ((i 1) (time) (chr nil (read-char-no-hang s nil 'eof)))
        ((or (eq chr 'eof) (eql chr #\linefeed) (> i max-chars)))
        (cond ((null chr)
	       (unless time (setq time (get-universal-time)))
	       (when (> (get-universal-time) (+ timeout time))
		     (print "Stream time out on read" *terminal-io*)
		     (error "Stream time out on read")))
	      ((graphic-char-p chr) (write-char chr out)
               (setq i (1+ i) time nil))
	      (t (setq i (1+ i) time nil))))))

(defun read-content (s len &optional (timeout 10000))
  (with-output-to-string (out)
    (do ((i 1) (time) (chr nil (read-char-no-hang s nil 'eof)))
        ((or (eq chr 'eof) (> i len)))
        (cond ((null chr) 
	       (unless time (setq time (get-universal-time)))
	       (when (> (get-universal-time) (+ timeout time)) 
		     (print "Stream time out on read" *terminal-io*)
		     (error "Stream time out on read")))
              ((graphic-char-p chr) (write-char chr out) (setq i (1+ i))
	       (setq time nil))
              (t (setq i (1+ i) time nil))))))

(defun decode-url-chrs (str)
  (with-output-to-string (s)
    (do ((i 0 (1+ i)) (n (length str)) (c))
        ((>= i n))
        (setq c (elt str i))
        (cond ((eql c #\+) (write-char #\space s))
              ((eql c #\%)
               (write-char (code-char (readcode str i)) s)
               (setq i (+ i 2)))
              (t (write-char c s))))))

(defun readcode (str i)
  (let ((*read-base* 16))
    (read-from-string str nil 0 :start (1+ i) :end (+ i 3))))

(defun internalize (y)
  (let (pos short)
    (setq pos (position #\@ y :test #'char=))
    (cond ((and pos (> pos 0))
           (setq short (read-value-string (subseq y 0 pos)))
           (if (findp `(NameIsGlobal ,short no) *manager*)
             short
             (read-value-string y)))
          (t (read-value-string y)))))

(defun externalize (x)
  (setq x (name x))
  (if (findp `(nameisglobal ,x no) *manager*)
    (symbol-name (universalize x))
    (symbol-name x)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Password stuff
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun password (n)
  (mod (- (* (* 7 (- (mod n 3) (mod n 5)) n) (* n n))
          (* 983 n)
          (integer-length n))
       1000000))

(defun pwd (s)
  (setq s (string-downcase s))
  (do ((i (length s) (1- i)) (n 0))
      ((= i 0) (princ-to-string (password n)))
      (setq n (+ (expt 8 (char-code (elt s (1- i)))) n))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; base64 for encryption
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun base64-to-string (s) (number-to-string (base64-to-number s)))

(defun string-to-base64 (s) (number-to-base64 (string-to-number s)))

(defun string-to-number (s)
  (do ((i 0 (1+ i)) (n (length s)) (out 0))
      ((= i n) out)
      (setq out (+ out (* (char-code (elt s (- n i 1))) (expt 256 i))))))

(defun number-to-string (n)
  (reverse (with-output-to-string (out)
             (do ((n n (floor n 256)))
                 ((zerop n))
                 (unless (zerop (mod n 256))
                   (write-char (code-char (mod n 256)) out))))))

(defun base64-to-number (s)
  (do ((i 0 (1+ i)) (n (length s)) (out 0))
      ((= i n) out)
      (setq out (+ out (* (base64-code (elt s (- n i 1))) (expt 64 i))))))

(defun number-to-base64 (n)
  (reverse (with-output-to-string (out)
             (do ((n n (floor n 64)))
                 ((zerop n))
                 (write-char (code-base64 (mod n 64)) out)))))

(defun base64-code (c)
  (setq c (char-code c))
  (cond ((and (>= c 65) (<= c 90)) (- c 65))
        ((and (>= c 97) (<= c 122)) (- c 71))
        ((and (>= c 48) (<= c 57)) (+ c 4))
        ((= c 43) 62)
        ((= c 47) 63)
        (t 0)))

(defun code-base64 (c)
  (cond ((< c 26) (code-char (+ c 65)))
        ((< c 52) (code-char (+ c 71)))
        ((< c 62) (code-char (- c 4)))
        ((= c 62) #\+)
        ((= c 63) #\/)
        (t #\=)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; output
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun output-html-prolog (s status-code)
  (format s "HTTP/1.0 ~D ~A"
	  status-code (rest (assoc status-code *http-status-codes*))) (crlf s)
  (format s "Content-type: text/html") (crlf s)
  (crlf s))  

(defun output-xml-prolog (s dum)
  (format s "HTTP/1.0 200 OK") (crlf s)
  (format s "Content-type: text/xml") (crlf s)
  (crlf s)
  (format s "<?xml version=\"1.0\"?>") (crlf s)
  (format s "<?xml-stylesheet type=\"text/xsl\" href=\"~A\"?>" dum) (crlf s)) 

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
