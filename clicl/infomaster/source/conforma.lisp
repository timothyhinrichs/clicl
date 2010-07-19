;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; (c) Copyright 1999-2008 by Michael Genesereth.  All rights reserved.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; conforma.lisp
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(eval-when (compile load eval)
  (proclaim '(special *interface* *bgcolor* *border*)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Performatives
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; slowfindpage
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defparameter *slowfindpage* "slowfindpage")

(defmethod process (s (command (eql 'slowfindpage)) postlines)
  (let (class structure)
    (cond ((null postlines) (http-problem s "Bad request."))
	  ((null (cdr postlines))
	   (setq class (read-user-string (cdar postlines)))
           (setq structure (fastsearchstructure class))
           (output-slowfindpage s structure))
          ((string-equal (getf-post "Command" postlines) "Display")
           (setq structure (searchconvert (parsestructure postlines)))
           (process-fastlookpage s structure 1 *count*))
          (t (http-problem s "Bad request.")))))

(defun parsestructure (postlines)
  (let (object class constraints)
    (setq object (read-user-string (cdr (pop postlines))))
    (setq class (read-user-string (cdr (pop postlines))))
    (do ((l postlines (cdr l)) (slot) (value) (nl))
	((or (null l) (equalp (caar l) "Command"))
	 (setq constraints (nreverse nl)))
        (setq slot (read-user-string (caar l)))
        (setq value (read-user-string (cdar l)))
        (if value (setq nl (cons (list slot value) nl))
	    (setq nl (cons (list slot) nl))))
    (list* object class constraints)))

(defun searchconvert (structure)
  (do ((l (cddr structure) (cdr l)) (nl))
      ((null l) (list* (car structure) (cadr structure) (nreverse nl)))
      (cond ((null (cdar l)) (setq nl (cons (car l) nl)))
            ((triplep 'searchstyle (caar l) 'fastselector *manager*)
             (setq nl (cons (list (caar l) (parseconvertfastselector (caar l) (cadar l))) nl)))
            ((triplep 'searchstyle (caar l) 'fancyselector *manager*)
             (setq nl (cons (list (caar l) (parseconvertfastselector (caar l) (cadar l))) nl)))
            ((triplep 'searchstyle (caar l) 'stringfield *manager*)
             (setq nl (cons (list (caar l) `(?* string (substring ,(cadar l)))) nl)))
            ((triplep 'searchstyle (caar l) 'text *manager*)
             (setq nl (cons (list (caar l) `(?* string (substring ,(cadar l)))) nl)))
            (t (setq nl (cons (car l) nl))))))

(defun parseconvertfastselector (slot value)
  (do ((l (results 'expander slot *interface*) (cdr l)) (ns value))
      ((null l) ns)
      (when (object (car l) value *gui*)
        (dolist (expander l) (setq ns `(?* thing ,(list expander ns)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun output-slowfindpage (s structure)
  (format-html s) (crlf s)
  (format-head s)
  (format s "<title>Find</title>") (crlf s)
  (finish-head s) (crlf s)
  (format-body s *bgcolor*) (crlf s)
  (output-header s)
  (format-border s) (crlf s)
  (output-slowfind s structure)
  (finish-border s) (crlf s)
  (output-footer s)
  (finish-body s) (crlf s)
  (finish-html s) (crlf s)
  'done)

(defun output-slowfind (s structure)
  (format s "<form name='form1' action='~A?'>" *slowfindpage*)
  (format s "Find every <b>~A</b> with the following properties."
	  (prettyname (cadr structure)))
  (format-hidden s "Object" (stringize (car structure))) (crlf s)
  (format-hidden s "Class" (stringize (cadr structure))) (crlf s)
  (format s "<br/>")
  (format s "<table cellspacing='3'>")
  (do ((l (cddr structure)) (slot) (values) (style) (label))
      ((null l))
      (setq slot (caar l))
      (setq style (find-searchstyle slot))
      (setq label (find-searchlabel slot))
      (multiple-value-setq (values l) (collectentries slot l))
      (format s "<tr><th align='left' valign='top'>")
      (output-slotlink s slot)
      (format s "</th><td>")
      (output-slow-cells s slot values style)
      (format s "</td>")
      (when label (format s "<td>~A</td>" label))
      (format s "</tr>")
      (crlf s))
  (format s "</table>") (crlf s)
  (output-slowfind-display s (cadr structure))
  (format s "</form>")
  'done)

(defmethod output-slowfind-display (s class)
  (declare (ignore class))
  (format s "<input type='submit' name='Command' value='Display'/>"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; slowinspectpage
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; slowcreatepage
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defparameter *slowcreatepage* "slowcreatepage")

(defmethod process (s (command (eql 'slowcreatepage)) postlines)
  (let (class structure)
    (cond ((null postlines) (http-problem s "Bad request."))
	  ((null (cdr postlines))
	   (setq class (read-user-string (cdar postlines)))
	   (process-slowcreatepage-start s class))
          ((string-equal (getf-post "Command" postlines) "Create")
           (setq structure (parsestructure postlines))
           (process-slowcreatepage-create s (car structure) (cadr structure) (cddr structure)))
          (t (http-problem s "Bad request.")))))

(defmethod process-slowcreatepage-start (s class)
  (output-slowcreatepage s (createitem (newinstance class) class)))

(defmethod process-slowcreatepage-create (s object class constraints)
  (let (errors)
    (cond ((setq errors (checkcreation object class constraints))
           (http-problems s errors))
	  (t (defineobject object class constraints *gui*)
	     (output-slowcreatepage-success s object)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun output-slowcreatepage (s structure)
  (format-html s) (crlf s)
  (format-head s) (crlf s)
  (format s "<title>Create</title>") (crlf s)
  (finish-head s) (crlf s)
  (force-output s)
  (format-body s *bgcolor*) (crlf s)
  (output-header s)
  (format-border s) (crlf s)
  (output-slowcreate s structure)
  (finish-border s) (crlf s)
  (output-footer s)
  (finish-body s) (crlf s)
  (finish-html s) (crlf s)
  'done)

(defun output-slowcreate (s structure)
  (format s "<form name='form1' action='~A?'>" *slowcreatepage*) (crlf s)
  (format s "Create ")
  (cond ((triplep 'noupdate (cadr structure) 'handle *interface*)
	 (format s "a new <b>~A</b>." (prettyname (cadr structure))))
	(t (format-id s (stringize (car structure)))
	   (format s " as ~A <b>~A</b>." (article (cadr structure)) (prettyname (cadr structure)))))
  (format-hidden s "Object" (stringize (car structure))) (crlf s)
  (format-hidden s "Class" (stringize (cadr structure))) (crlf s)
  (format s "<br/>")
  (format s "<table cellspacing='3' border='0'>")
  (do ((l (cddr structure)) (slot) (style) (label) (values))
      ((null l))
      (setq slot (caar l))
      (setq style (find-createstyle slot))
      (setq label (find-createlabel slot))
      (multiple-value-setq (values l) (collectvalues slot l))
      (format s "<tr>")
      (format s "<th align='left' valign='top'>")
      (output-slotlink s slot)
      (format s "</th>")
      (format s "<td>")
      (output-slow-cells s slot values style)
      (format s "</td>")
      (when label (format s "<td>~A</td>" label))
      (format s "</tr>")
      (crlf s))
  (format s "</table>") (crlf s)
  (output-slowcreate-create s (cadr structure))
  (format s "</form>") (crlf s)
  'done)

(defmethod output-slowcreate-create (s class)
  (declare (ignore class))
  (format s "<input type='submit' name='Command' value='Create'/>"))

(defun output-slowcreatepage-success (s object)
  (format-html s) (crlf s)
  (format s "<head>")
  (format s "<title>Create ~A</title>" (prettify object)) (crlf s)
  (format s "</head>") (crlf s)
  (format s "<body leftmargin='0' topmargin='0' marginwidth='0' marginheight='0' bgcolor='~A' onLoad='~A'>"
          *bgcolor* (format nil "location.replace(\"slowinspectpage?Object=~A\")" object)) (crlf s)
  (output-header s)
  (format-border s) (crlf s)
  (format s "Creation successful.")
  (finish-border s) (crlf s)
  (output-footer s)
  (finish-body s) (crlf s)
  (finish-html s) (crlf s)
  'done)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; slowchangepage
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defparameter *slowchangepage* "slowchangepage")

(defmethod process (s (command (eql 'slowchangepage)) postlines)
  (let (object class structure)
    (cond ((null postlines) (http-problem s "Bad request."))
	  ((null (cdr postlines))
	   (setq object (read-user-string (cdr (pop postlines))))
	   (setq class (classify object *gui*))
	   (process-slowchangepage-start s object class (changeitem object class)))
	  (t (setq structure (changestructure postlines))
	     (process-slowchangepage-record s (car structure) (cadr structure) (cddr structure))))))

(defmethod process-slowchangepage-start (s object class structure)
  (declare (ignore object class))
  (output-slowchangepage s structure))

(defmethod process-slowchangepage-record (s object class constraints)
  (let (errors)
    (cond ((setq errors (checkchange object class constraints))
           (http-problems s errors))
	  (t (defineobject object class constraints *gui*)
	     (output-slowchangepage-success s object)))))

(defun changestructure (postlines)
  (let (object class constraints)
    (setq object (read-user-string (cdr (pop postlines))))
    (setq class (read-user-string (cdr (pop postlines))))
    (do ((l postlines (cdr l)) (slot) (value) (style) (nl))
	((or (null l) (equalp (caar l) "Command"))
	 (setq constraints (nreverse nl)))
        (setq slot (read-user-string (caar l)))
        (setq style (result 'changestyle slot *manager*))
        (if (find style '(stringfield text textarea password))
	    (setq value (cdar l))
	    (setq value (read-user-string (cdar l))))
        (if value (setq nl (cons (list slot value) nl))
	    (setq nl (cons (list slot) nl))))
    (list* object class constraints)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun output-slowchangepage (s structure)
  (format-html s) (crlf s)
  (format-head s) (crlf s)
  (format s "<title>Change</title>") (crlf s)
  (finish-head s) (crlf s)
  (force-output s)
  (format-body s *bgcolor*) (crlf s)
  (output-header s)
  (format-border s) (crlf s)
  (output-slowchange s structure)
  (finish-border s) (crlf s)
  (output-footer s)
  (finish-body s) (crlf s)
  (finish-html s) (crlf s)
  'done)

(defun output-slowchange (s structure)
  (format s "<form name='form1' action='~A?'>" *slowchangepage*) (crlf s)
  (output-handle s (car structure))
  (format s " is ~A ~A." (article (cadr structure)) (prettify (cadr structure)))
  (format-hidden s "Object" (stringize (car structure))) (crlf s)
  (format-hidden s "Class" (stringize (cadr structure))) (crlf s)
  (format s "<br/>") (crlf s)
  (format s "<table cellspacing='3'>")
  (do ((l (cddr structure)) (slot) (style) (label) (values))
      ((null l))
      (setq slot (caar l))
      (setq style (find-changestyle slot))
      (setq label (find-changelabel slot))
      (multiple-value-setq (values l) (collectvalues slot l))
      (format s "<tr>")
      (format s "<th align='left' valign='top'>")
      (output-slotlink s slot)
      (format s "</th>")
      (format s "<td>")
      (output-slow-cells s slot values style)
      (format s "</td>")
      (when label (format s "<td>~A</td>" label))
      (format s "</tr>")
      (crlf s))
  (format s "</table>") (crlf s)
  (output-slowchange-record s (cadr structure)) (crlf s)
  (format s "</form>") (crlf s)
  'done)

(defmethod output-slowchange-record (s class)
  (declare (ignore class))
  (format s "<input type='submit' name='Command' value='Record'/>"))

(defun output-slowchangepage-success (s object)
  (format-html s) (crlf s)
  (format s "<head>")
  (format s "<title>Change ~A</title>" (prettify object)) (crlf s)
  (format s "</head>") (crlf s)
  (format s "<body leftmargin='0' topmargin='0' marginwidth='0' marginheight='0' bgcolor='~A' onLoad='~A'>"
          *bgcolor* (format nil "location.replace(\"slowinspectpage?Object=~A\")" object)) (crlf s)
  (output-header s)
  (format-border s) (crlf s)
  (format s "Change successful.")
  (finish-border s) (crlf s)
  (output-footer s)
  (finish-body s) (crlf s)
  (finish-html s) (crlf s)
  'done)  

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; slowcopypage
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod process (s (command (eql 'slowcopypage)) postlines)
  (let (object class)
    (cond ((null (cdr postlines)) (http-problem s "Bad request."))
          (t (setq object (read-user-string (cdr (pop postlines))))
             (setq class (classify object *gui*))
             (process-slowcopypage-copy s object class)))))

(defmethod process-slowcopypage-copy (s object class)
  (output-slowcreatepage s (createcopy object (newinstance class) class)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; slowdeletepage
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod process (s (command (eql 'slowdeletepage)) postlines)
  (let (object class)
    (cond ((null (cdr postlines)) (http-problem s "Bad request."))
          (t (setq object (read-user-string (cdr (pop postlines))))
             (setq class (classify object *gui*))
             (process-slowdeletepage-delete s object class)))))

(defun process-slowdeletepage-delete (s object class)
  (declare (ignore class))
  (let (facts result)
    (setq facts (facts object *gui*))
    (setq result (prorequest `(update ,(maksand (mapcar #'maknot facts)))))
    (cond ((errorp result) (output-problems s result))
          (t (html-message s "Object deleted.")))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Styles
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod output-slow-cells (s slot values (style (eql 'checkbox)))
  (let (options)
    (when (setq options (findalternatives slot))
      (output-coldcheckbox s slot options values))))

(defmethod output-slow-cells (s slot values (style (eql 'combobox)))
  (output-slow-deadcells s slot values style))

(defmethod output-slow-cells (s slot values (style (eql 'dateinput)))
  (output-slow-deadcells s slot values style))

(defmethod output-slow-cells (s slot values (style (eql 'datestyle)))
  (output-slow-deadcells s slot values style))

(defmethod output-slow-cells (s slot values (style (eql 'dollarinput)))
  (output-slow-deadcells s slot values style))

(defmethod output-slow-cells (s slot values (style (eql 'dollarstyle)))
  (output-slow-deadcells s slot values style))

(defmethod output-slow-cells (s slot values (style (eql 'emailstyle)))
  (output-slow-deadcells s slot values style))

(defmethod output-slow-cells (s slot values (style (eql 'fastselector)))
  (output-slow-deadcells s slot values style))

(defmethod output-slow-cells (s slot values (style (eql 'fancyselector)))
  (output-slow-deadcells s slot values style))

(defmethod output-slow-cells (s slot values (style (eql 'glyph)))
  (output-slow-deadcells s slot values style))

(defmethod output-slow-cells (s slot values (style (eql 'htmlstyle)))
  (output-slow-deadcells s slot values style))

(defmethod output-slow-cells (s slot values (style (eql 'imagestyle)))
  (output-slow-deadcells s slot values style))

(defmethod output-slow-cells (s slot values (style (eql 'menu)))
  (let (options)
    (when (setq options (findalternatives slot))
      (output-oldmenu s slot options values))))

(defmethod output-slow-cells (s slot values (style (eql 'password)))
  (output-slow-deadcells s slot values style))

(defmethod output-slow-cells (s slot values (style (eql 'prettystyle)))
  (output-slow-deadcells s slot values style))

(defmethod output-slow-cells (s slot values (style (eql 'radiobutton)))
  (output-slow-deadcells s slot values style))

(defmethod output-slow-cells (s slot values (style (eql 'selector)))
  (output-slow-deadcells s slot values style))

(defmethod output-slow-cells (s slot values (style (eql 'stringfield)))
  (output-slow-deadcells s slot values style))

(defmethod output-slow-cells (s slot values (style (eql 'subframe)))
  (output-slow-deadcells s slot values style))

(defmethod output-slow-cells (s slot values (style (eql 'tabulator)))
  (output-slow-deadcells s slot values style))

(defmethod output-slow-cells (s slot values (style (eql 'text)))
  (output-slow-deadcells s slot values style))

(defmethod output-slow-cells (s slot values (style (eql 'textarea)))
  (output-slow-deadcells s slot values style))

(defmethod output-slow-cells (s slot values (style (eql 'typein)))
  (output-slow-deadcells s slot values style))

(defmethod output-slow-cells (s slot values (style (eql 'urlstyle)))
  (output-slow-deadcells s slot values style))

(defmethod output-slow-cells (s slot values style)
  (output-slow-deadcells s slot values style))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod output-slow-deadcells (s slot values style)
  (format s "<table cellpadding='0' cellspacing='0'>")
  (format s "<tr><td>")
  (output-slow-cell s slot (car values) style) (crlf s)
  (format s "</td></tr>")
  (dolist (value (cdr values))
    (format s "<tr><td>")
    (output-slow-cell s slot value style) (crlf s)
    (format s "</td></tr>"))
  (format s "</table>")
  (crlf s))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod output-slow-cell (s slot value (style (eql 'checkbox)))
  (let (options)
    (when (setq options (findalternatives slot))
      (output-coldcheckbox s slot options (list value)))))

(defmethod output-slow-cell (s slot value (style (eql 'combobox)))
  (let (options)
    (when (setq options (findalternatives slot))
      (setq options (cons 'unknown options))
      (output-oldcombobox s slot options (prettyname value) 30))))

(defmethod output-slow-cell (s slot value (style (eql 'dateinput)))
  (output-olddateinput s slot value))

(defmethod output-slow-cell (s slot value (style (eql 'datestyle)))
  (output-hidden s slot value)
  (output-simple s value))

(defmethod output-slow-cell (s slot value (style (eql 'dollarinput)))
  (output-olddollarinput s slot value 6))

(defmethod output-slow-cell (s slot value (style (eql 'dollarstyle)))
  (output-hidden s slot value)
  (when (realp value) (format s "$~$" value)))

(defmethod output-slow-cell (s slot value (style (eql 'emailstyle)))
  (output-hidden s slot value)
  (when value (format s "<a href='mailto:~A'><font color='red'>~A</font></a>" value value)))

(defmethod output-slow-cell (s slot value (style (eql 'fastselector)))
  (output-oldfastselector s slot value))

(defmethod output-slow-cell (s slot value (style (eql 'fancyselector)))
  (output-oldfancyselector s slot value))

(defmethod output-slow-cell (s slot value (style (eql 'glyph)))
  (output-hidden s slot value)
  (output-handle s value))

(defmethod output-slow-cell (s slot value (style (eql 'htmlstyle)))
  (output-hidden s slot value)
  (when value (format s "~A" value)))

(defmethod output-slow-cell (s slot value (style (eql 'imagestyle)))
  (output-hidden s slot value)
  (when value (format s "<img src='~A'/>" value)))

(defmethod output-slow-cell (s slot value (style (eql 'menu)))
  (let (options)
    (when (setq options (findalternatives slot))
      (output-oldmenu s slot options (list value)))))

(defmethod output-slow-cell (s slot value (style (eql 'password)))
  (output-coldpassword s slot value 40))

(defmethod output-slow-cell (s slot value (style (eql 'prettystyle)))
  (output-hidden s slot value)
  (output-simple s value))

(defmethod output-slow-cell (s slot value (style (eql 'radiobutton)))
  (let (options)
    (when (setq options (findalternatives slot))
      (setq options (cons 'unknown options))
      (output-oldradiobutton s slot options value))))

(defmethod output-slow-cell (s slot value (style (eql 'selector)))
  (let (options)
    (when (setq options (findalternatives slot))
      (setq options (cons 'unknown options))
      (output-coldselector s slot options value))))

(defmethod output-slow-cell (s slot value (style (eql 'stringfield)))
  (output-coldstringfield s slot value 40))

(defmethod output-slow-cell (s slot value (style (eql 'subframe)))
  (output-hidden s slot value))

(defmethod output-slow-cell (s slot value (style (eql 'tabulator)))
  (output-hidden s slot value))

(defmethod output-slow-cell (s slot value (style (eql 'text)))
  (output-coldstringfield s slot value 40))

(defmethod output-slow-cell (s slot value (style (eql 'textarea)))
  (output-coldstringfield s slot value 40))

(defmethod output-slow-cell (s slot value (style (eql 'typein)))
  (output-coldtypein s slot value 40))

(defmethod output-slow-cell (s slot value (style (eql 'urlstyle)))
  (output-hidden s slot value)
  (when value (format s "<a href='~A'>~A</a>" value (htmlify value))))

(defmethod output-slow-cell (s slot value style)
  (declare (ignore style))
  (output-hidden s slot value))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
