;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; (c) Copyright 1999-2006 by Michael Genesereth.  All rights reserved.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(eval-when (compile load eval) (proclaim '(special *http-status-codes*)))

(defmethod process (s (file (eql 'doc)) postlines)
  (let (pathname)
    (setq pathname (macify (getf-post "Path" postlines)))
    (cond ((null pathname) (http-problem s "No file specified."))
          ((probe-file pathname) (serve-document s pathname))
          (t (http-problem s "Document not found.")))))

(defun serve-document (stream pathname)
  (with-open-file
    (istream pathname :direction :input :element-type 'unsigned-byte) 
    (let (len wd major minor)
      (setq len (file-length istream))
      (setq wd (file-write-date istream))
      (multiple-value-setq (major minor)
        (content-type-from-file-type (pathname-type pathname)))
      (output-prolog-detailed stream 200 
        :nocookie t
        :content-type (list (string-downcase major) (string-downcase minor))
        :date (http-date-from-utime wd)
        :content-length len)
      (force-output stream)
      (do ((i 1 (1+ i)) (byte (read-byte istream nil :eof) (read-byte istream nil :eof)))
          ((eq :eof byte) 'done)
          (when (> i 1024) (force-output stream) (setq i 1))
          (write-char (code-char byte) stream)))))

(defvar *content-type-alist* nil)

(defmacro defcontent-type (major-type minor-type description icon &rest types)
  `(loop for type in ',types
       do (let ((existing (assoc type *content-type-alist*
				 :test #'string-equal)))
	    (setq *content-type-alist*
	      (cons (list type ',major-type ',minor-type ,description ,icon)
		    (remove existing *content-type-alist*))))))

(defcontent-type :video :msvideo "AVI video" "avi-movie.gif" "avi")
(defcontent-type :application :unknown "Unknown binary file" "binary.gif" 
		 :unknown)
(defcontent-type :application :binary "Binary file" "binary.gif" 
		 "fasl" "pfasl" "sbin" "bin" "o")
(defcontent-type :application :binary "Compiled Java class file" "binary.gif" 
		 "class")
(defcontent-type :application :bryce "Bryce 3D model" "bryce-icon.gif" "br3")
(defcontent-type :application :dos "DOS/Windows/NT executable" 
		 "dos-prompt.gif" "exe")
(defcontent-type :application :generic "File" "generic.gif" :generic)
(defcontent-type :image :unknown "Image" "image.gif" "bmp")
(defcontent-type :application :kif "KIF format knowledge content" 
		 "kif-icon.gif" "kif")
(defcontent-type :application :lisp "Lisp source file" "lisp-icon.gif" 
		 "lisp" "lsp" "cl")
(defcontent-type :video :unknown "Video" "movie.gif" )
(defcontent-type :video :mpeg "MPEG video" "mpeg-movie.gif" "mpg" "mpeg" "mpe")
(defcontent-type :application :msaccess "Access database" "msaccess-icon.gif"
		 "mdb")
(defcontent-type :application :msexcel "Excel spreadsheet" "msexcel-icon.gif"
		 "xls")
(defcontent-type :application :csv "Comma-separated values" "csv-icon.gif"
		 "csv")
(defcontent-type :application :tab "Tab-delimited values" "tab-icon.gif"
		 "tab")
(defcontent-type :application :mspowerpoint "Powerpoint presentation" 
		 "mspowerpoint-icon.gif" "ppt")
(defcontent-type :application :msword "MS Word document" "msword-icon.gif"
		 "doc")
(defcontent-type :video :quicktime "Quicktime video" "new-quicktime-movie.gif"
		 "qt" "mov")
(defcontent-type :application :pdf "Portable Document Format (PDF) file" 
		 "pdfi-icon.gif" "pdf")
(defcontent-type :application :photoshop "Photoshop file" "photoshop-icon.gif"
		 "psd")
(defcontent-type :application :postscript "Postscript document" "ps-icon.gif"
		 "ps")
(defcontent-type :application :raydream "Raydream studio model" 
		 "raydream-studio-icon.gif" "rd4" "rd5" "rds")
(defcontent-type :application :uuencode "UUencoded file" "uu.gif" "uu" "uue")
(defcontent-type :application :tar "tar archive" "compressed-folder.gif" "tar")
(defcontent-type :application :zip "ZIP archive" "compressed-folder.gif" "zip")
(defcontent-type :image :gif "Compuserve GIF image" "gif-image.gif" "gif")
(defcontent-type :image :jpeg "JPEG image" "jpeg-image.gif" "jpg" "jpeg")
(defcontent-type :text :plain "Plain text file" "text.gif" "txt" "text")
(defcontent-type :text :html "Hypertext Markup Language (HTML) file" "html.gif"
		 "html" "htm")
(defcontent-type :text :xml "eXtensible Markup Language (XML) file" "html.gif"
		 "xml")
(defcontent-type :text :xml "XML Stylesheet Language (XSL) file" "html.gif"
		 "xsl")
(defcontent-type :application :javascript "Javascript source file" 
		 "javascript-icon.gif" "js")

(defun content-type-from-file-type (type)
  (let ((entry (assoc type *content-type-alist* :test #'string-equal)))
    (if entry
	(values (second entry) (third entry))
      (values :application :unknown))))

(defun http-date-from-utime (utime)
  (multiple-value-bind (second minute hour day month year dow)
      (decode-universal-time utime)
    (let ((*print-case* :capitalize))
      (format nil "~A, ~2D ~A ~D ~2,'0,D:~2,'0,D:~2,'0,D" 
	      (nth dow *day-names*)
	      day
	      (nth (- month 1) *short-month-names*) 
	      year 
	      hour minute second))))

(defun utime-from-http-date (string)
  (let ((space-pos (position #\space string :test #'char=)))
    (let ((day-end-pos (position #\space string :test #'char= 
				 :start (+ 1 space-pos))))
      (let ((month-end-pos (position #\space string :test #'char= 
				     :start (+ 1 day-end-pos)))
	    (day (parse-integer string :start (+ space-pos 1)
				:end day-end-pos)))
	(let ((year-end-pos (position #\space string :test #'char= 
				      :start (+ 1 month-end-pos)))
	      (month (+ 1 (position 
			   (read-from-string string t nil
						    :start day-end-pos
						    :end month-end-pos)
			   *short-month-names*))))
	  (let ((year (parse-integer string :start (+ 1 month-end-pos)
				     :end year-end-pos))
		(hour-end-pos (position #\: string :test #'char= 
					:start (+ 1 year-end-pos))))
	    (let ((hour (parse-integer string :start (+ 1 year-end-pos)
				       :end hour-end-pos))
		  (minute-end-pos (position #\: string :test #'char= 
					    :start (+ 1 hour-end-pos))))
	      (let ((minute (parse-integer string :start (+ 1 hour-end-pos)
					   :end minute-end-pos))
		    (second-end-pos (position #\space string :test #'char= 
					      :start (+ 1 minute-end-pos))))
		(let ((second (parse-integer
			       string :start (+ 1 minute-end-pos)
			       :end second-end-pos)))
		  (encode-universal-time
		   second minute hour day month year))))))))))

(defun parse-url-components-into-pathname (string)
  (let ((last-/-pos (position #\/ string :test #'char= :from-end t)))
    (let ((dir-part (if last-/-pos
			(subseq string 0 (+ 1 last-/-pos))
		      ""))
	  (rest (if last-/-pos (subseq string (+ last-/-pos 1)) string)))
      (let ((dotpos (position #\. rest :test #'char=))
	    (dirs (loop with start = 0
		      for /pos = (position #\/ dir-part :test #'char= 
					   :start start)
		      for dir = (and /pos (subseq dir-part start /pos))
		      while /pos
		      when (not (equal "" dir))
		      collect dir
		      do (setq start (+ 1 /pos)))))
	(let ((name (if dotpos 
			(if (zerop dotpos)
			    nil
			  (subseq rest 0 dotpos))
		      rest))
	      (type-part (if dotpos
			     (subseq rest (+ dotpos 1))
			   nil)))
	  (let ((args-pos (min (or (position #\& type-part :test #'char=)
				   most-positive-fixnum)
			       (or (position #\? type-part :test #'char=)
				   most-positive-fixnum))))
	    (let ((type (if (= args-pos most-positive-fixnum)
			    type-part
			  (subseq type-part 0 args-pos)))
		  (args (if (= args-pos most-positive-fixnum)
			    nil
			  (subseq type-part args-pos))))
	      (values dirs name type args))))))))

(defun output-prolog-detailed (s status-code &rest plist)
  (format s "HTTP/1.0 ~D ~A"
	  status-code (rest (assoc status-code *http-status-codes*)))
  (crlf s)
  (when (not (getf plist :date))
    (format s "Date: ~A" (http-date-from-utime (get-universal-time)))
    (crlf s))
  (let ((*print-case* :capitalize))
    (loop for (key value) on plist by #'cddr
	when (not (eq :nocookie key))
	do (format s "~A: " key)
	   (if (consp value)
	       (loop for x in value
		   for first-p = t then nil
		   when (not first-p) do (princ "/" s)
		   do (format s "~A" x))
	     (format s "~A" value))
	   (crlf s)))
  (crlf s))

(defun type-icon-for-pathname (path)
  (let ((entry (or (assoc (pathname-type path) *content-type-alist*
			  :test #'string-equal)
		   (assoc :unknown *content-type-alist*))))
    (values (fifth entry) (fourth entry))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
