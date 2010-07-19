;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; (c) Copyright 1999-2008 by Michael Genesereth.  All rights reserved.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; proforma.lisp
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(eval-when (compile load eval)
  (proclaim '(special *home* *interface* *bgcolor* *border*)))

(defparameter *performative* "toplevel")
(defparameter *buttons* 0)
(defparameter *count* 20)
(defparameter *debug* nil)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Performatives
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; fastfieldpage
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod process (s (command (eql 'fastfieldpage)) postlines)
  (cond ((null postlines) (process-rootclasspage s))
        (t (process-fastfieldpage s (read-value-string (cdar postlines))))))

(defun process-rootclasspage (s)
  (format-html s) (crlf s)
  (output-head s "Fastfieldpage") (crlf s)
  (format-body s *bgcolor*) (crlf s)
  (output-header s)
  (format-border s)
  (output-classes s 'thing (find-subclasses 'thing))
  (finish-border s)
  (output-footer s)
  (finish-body s) (crlf s)
  (finish-html s) (crlf s)
  'done)

(defun process-fastfieldpage (s class)
  (format-html s) (crlf s)
  (output-head s "Fastfieldpage") (crlf s)
  (format-body s *bgcolor*) (crlf s)
  (output-header s)
  (format-border s)
  (output-classes s class (find-subclasses class))
  (finish-border s)
  (output-footer s)
  (finish-body s) (crlf s)
  (finish-html s) (crlf s)
  'done)

(defmethod output-classes (s class classes)
  (let (count)
    (format s "<br/>
<center>
<table width='800'>
<tr>")
    (setq count (length classes))
    (cond ((> count 18)
           (do ((i 1 (1+ i)))
               ((> i 4))
               (format s "<td width='25%' valign='top'>")
               (cond ((= i 1)
                      (format s "<span style='font-size:16px; font-weight:bold; color:#004488'>~A</span><br/>"
                              (pluralize class)))
                     (t (format s "<font size='4'>&nbsp;</font>")))
               (format s "<div style='margin-left: 15px'>")
               (do ((j (ceiling count 4) (1- j)))
                   ((or (= j 0) (null classes)))
                   (format s "<a href='fastlookpage?class=~A'>~A</a><br/>"
                           (car classes) (pluralize (car classes)))
                   (setq classes (cdr classes)))
               (format s "</div>")
               (format s "</td>")))
          (t (format s "<td width='25%' valign='top'>")
             (format s "<span style='font-size:16px; font-weight:bold; color:#004488'>~A</span><br/>"
                              (pluralize class))
             (format s "<div style='margin-left: 15px'>")
             (dolist (class classes)
               (format s "<a href='fastlookpage?class=~A'>~A</a><br/>"
                       class (pluralize class)))
             (format s "</div>")
             (format s "</td>")))
    (format s "</tr>
</table>
</center>
<br/>")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; fastfindpage
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod process (s (command (eql 'fastfindpage)) postlines)
  (let (dum structure (*performative* "fastfindpage"))
    (cond ((setq dum (getf-post "Class" postlines))
           (setq structure `(? ,(read-user-string dum)))
           (output-fastfindpage s structure))
          ((string-equal (getf-post "Command" postlines) "Display")
           (setq structure (read-user-string (cdr (pop postlines))))
           (process-fastlookpage s structure 1 *count*))
          (t (http-problem s "Bad request.")))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun output-fastfindpage (s structure)
  (format-html s) (crlf s)
  (format-head s)
  (format s "<title>Find</title>") (crlf s)
  (format-javascript s) (crlf s)
  (finish-head s) (crlf s)
  (format-body s *bgcolor*) (crlf s)
  (output-header s)
  (format-border s) (crlf s)
  (output-fastfind s structure)
  (finish-border s) (crlf s)
  (output-footer s)
  (finish-body s) (crlf s)
  (finish-html s) (crlf s)
  'done)

(defun output-fastfind (s structure)
  (let ((*buttons* 0))
    (format s "<form name='form1' action='~A?'>" *performative*)
    (format s "Find every <b>")
    (format s "<a href='~A?Class=~A'>~A</a>"
            *performative* (stringize (cadr structure)) (prettyname (cadr structure)))
    (format s "</b> with the following properties.")
    (format-hidden s "Object" (stringize (car structure))) (crlf s)
    (format-hidden s "Class" (stringize (cadr structure))) (crlf s)
    (format s "<br/>")
    (format s "<table cellspacing='3'>")
    (do ((l (queryable-slots (cadr structure)) (cdr l)) (values) (style) (label))
        ((null l))
        (setq style (find-searchstyle (car l)))
        (setq label (find-searchlabel (car l)))
        (setq values (getslotvals (car l) (cddr structure)))
        (format s "<tr><th align='left' valign='top'>")
        (output-slotlink s (car l))
        (format s "</th><td>")
        (output-fastfind-cells s (car l) values style)
        (format s "</td>")
        (when label (format s "<td>~A</td>" label))
        (format s "</tr>")
        (crlf s))
    (format s "</table>") (crlf s)
    (output-fastfind-display s (cadr structure))
    (format s "</form>")
    'done))

(defun output-fastfind-display (s class)
  (declare (ignore class))
  (format s "<input type='button' name='Command' value='Display'
                    onClick='postDisplay(this.form)'/>"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod output-fastfind-cells (s slot values (style (eql 'checkbox)))
  (let (options)
    (setq options (findalternatives slot))
    (format s "<table cellpadding='0' cellspacing='0'>")
    (format s "<tr><td>")
    (when options (output-coldcheckbox s slot options values))
    (format s "</td></tr>") (crlf s)
    (format s "</table>")
    (crlf s)))

(defmethod output-fastfind-cells (s slot values (style (eql 'combobox)))
  (output-fastfind-deadcells s slot values style))

(defmethod output-fastfind-cells (s slot values (style (eql 'dateinput)))
  (output-fastfind-livecells s slot values style))

(defmethod output-fastfind-cells (s slot values (style (eql 'datestyle)))
  (output-fastfind-deadcells s slot values style))

(defmethod output-fastfind-cells (s slot values (style (eql 'dollarinput)))
  (output-fastfind-livecells s slot values style))

(defmethod output-fastfind-cells (s slot values (style (eql 'dollarstyle)))
  (output-fastfind-deadcells s slot values style))

(defmethod output-fastfind-cells (s slot values (style (eql 'emailstyle)))
  (output-fastfind-deadcells s slot values style))

(defmethod output-fastfind-cells (s slot values (style (eql 'fastselector)))
  (output-fastfind-deadcells s slot values style))

(defmethod output-fastfind-cells (s slot values (style (eql 'fancyselector)))
  (output-fastfind-deadcells s slot values style))

(defmethod output-fastfind-cells (s slot values (style (eql 'glyph)))
  (output-fastfind-deadcells s slot values style))

(defmethod output-fastfind-cells (s slot values (style (eql 'htmlstyle)))
  (output-fastfind-deadcells s slot values style))

(defmethod output-fastfind-cells (s slot values (style (eql 'imagestyle)))
  (output-fastfind-deadcells s slot values style))

(defmethod output-fastfind-cells (s slot values (style (eql 'menu)))
  (let (options)
    (setq options (findalternatives slot))
    (format s "<table cellpadding='0' cellspacing='0'>")
    (format s "<tr><td>")
    (when options (output-oldmenu s slot options values))
    (format s "</td></tr>") (crlf s)
    (format s "</table>")
    (crlf s)))

(defmethod output-fastfind-cells (s slot values (style (eql 'password)))
  (output-fastfind-livecells s slot values style))

(defmethod output-fastfind-cells (s slot values (style (eql 'prettystyle)))
  (output-fastfind-deadcells s slot values style))

(defmethod output-fastfind-cells (s slot values (style (eql 'radiobutton)))
  (output-fastfind-deadcells s slot values style))

(defmethod output-fastfind-cells (s slot values (style (eql 'selector)))
  (output-fastfind-livecells s slot values style))

(defmethod output-fastfind-cells (s slot values (style (eql 'stringfield)))
  (output-fastfind-livecells s slot values style))

(defmethod output-fastfind-cells (s slot values (style (eql 'text)))
  (output-fastfind-livecells s slot values style))

(defmethod output-fastfind-cells (s slot values (style (eql 'textarea)))
  (output-fastfind-livecells s slot values style))

(defmethod output-fastfind-cells (s slot values (style (eql 'typein)))
  (output-fastfind-livecells s slot values style))

(defmethod output-fastfind-cells (s slot values (style (eql 'urlstyle)))
  (output-fastfind-deadcells s slot values style))

(defmethod output-fastfind-cells (s slot values style)
  (output-fastfind-deadcells s slot values style))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun output-fastfind-deadcells (s slot values style)
  (format s "<table cellpadding='0' cellspacing='0'>")
  (dolist (value values)
    (format s "<tr><td>")
    (output-fastfind-cell s slot value style) (crlf s)
    (format s "</td></tr>"))
  (format s "</table>") (crlf s))

(defun output-fastfind-livecells (s slot values style)
  (format s "<table cellpadding='0' cellspacing='0'>")
  (dolist (value values)
    (format s "<tr><td>")
    (output-fastfind-cell s slot value style) (crlf s)
    (format s "</td></tr>"))
  (when (or (null values) (find-multivalued slot))
    (format s "<tr><td>")
    (output-fastfind-cell s slot nil style) (crlf s)
    (format s "</td></tr>"))
  (when (find-multivalued slot)
    (format s "<tr id='~A' qualifier='skip' style='display:None'>" (stringize slot))
    (format s "<td>")
    (output-fastfind-cell s slot nil style)
    (format s "</td>")
    (format s "</tr>"))
  (format s "</table>") (crlf s))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod output-fastfind-cell (s slot value (style (eql 'checkbox)))
  (let (options)
    (when (setq options (findalternatives slot))
      (output-coldcheckbox s slot (cons nil options) value))))

(defmethod output-fastfind-cell (s slot value (style (eql 'combobox)))
  (let (options)
    (when (setq options (findalternatives slot))
      (output-oldcombobox s slot (cons 'unknown options) (prettyname value) 30))))

(defmethod output-fastfind-cell (s slot value (style (eql 'dateinput)))
  (output-olddateinput s slot value))

(defmethod output-fastfind-cell (s slot value (style (eql 'datestyle)))
  (when value
    (output-hidden s slot value)
    (output-simple s value)))

(defmethod output-fastfind-cell (s slot value (style (eql 'dollarinput)))
  (output-olddollarinput s slot value 6))

(defmethod output-fastfind-cell (s slot value (style (eql 'dollarstyle)))
  (when (realp value)
    (output-hidden s slot value)
    (format s "$~$" value)))

(defmethod output-fastfind-cell (s slot value (style (eql 'emailstyle)))
  (when value
    (output-hidden s slot value)
    (format s "<a href='mailto:~A'><font color='red'>~A</font></a>" value value)))

(defmethod output-fastfind-cell (s slot value (style (eql 'fastselector)))
  (output-oldfastselector s slot value))

(defmethod output-fastfind-cell (s slot value (style (eql 'fancyselector)))
  (output-oldfancyselector s slot value))

(defmethod output-fastfind-cell (s slot value (style (eql 'glyph)))
  (when value
    (output-hidden s slot value)
    (output-handle s value)))

(defmethod output-fastfind-cell (s slot value (style (eql 'htmlstyle)))
  (when value
    (output-hidden s slot value)
    (format s "~A" value)))

(defmethod output-fastfind-cell (s slot value (style (eql 'imagestyle)))
  (when value
    (output-hidden s slot value)
    (format s "<img src='~A'/>" value)))

(defmethod output-fastfind-cell (s slot value (style (eql 'password)))
  (if (find-multivalued slot)
      (output-warmpassword s slot value 40)
      (output-coldpassword s slot value 40)))

(defmethod output-fastfind-cell (s slot value (style (eql 'prettystyle)))
  (when value
    (output-hidden s slot value)
    (output-simple s value)))

(defmethod output-fastfind-cell (s slot value (style (eql 'radiobutton)))
  (let (options)
    (cond ((setq options (findalternatives slot))
           (setq options (cons nil options))
           (output-oldradiobutton s slot options value))
          (t (format-hidden s (stringize slot) "")))))

(defmethod output-fastfind-cell (s slot value (style (eql 'selector)))
  (let (options)
    (when (setq options (findalternatives slot))
      (setq options (cons 'unknown options))
      (if (find-multivalued slot)
	  (output-warmselector s slot options value)
          (output-coldselector s slot options value)))))

(defmethod output-fastfind-cell (s slot value (style (eql 'stringfield)))
  (if (find-multivalued slot)
      (output-warmstringfield s slot value 40)
      (output-coldstringfield s slot value 40)))

(defmethod output-fastfind-cell (s slot value (style (eql 'text)))
  (if (find-multivalued slot)
      (output-warmstringfield s slot value 40)
      (output-coldstringfield s slot value 40)))

(defmethod output-fastfind-cell (s slot value (style (eql 'textarea)))
  (if (find-multivalued slot)
      (output-warmstringfield s slot value 40)
      (output-coldstringfield s slot value 40)))

(defmethod output-fastfind-cell (s slot value (style (eql 'typein)))
  (if (find-multivalued slot)
      (output-warmtypein s slot value 40)
      (output-coldtypein s slot value 40)))

(defmethod output-fastfind-cell (s slot value (style (eql 'urlstyle)))
  (when value
    (output-hidden s slot value)
    (format s "<a href='~A'>Link</a>" value)))

(defmethod output-fastfind-cell (s slot value style)
  (declare (ignore style))
  (when value (output-hidden s slot value)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; fastseekpage
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod process (s (command (eql 'fastseekpage)) postlines)
  (let (dum structure (*performative* "fastseekpage"))
    (cond ((setq dum (getf-post "Class" postlines))
           (setq structure (fastsearchstructure (read-user-string dum)))
           (output-fastseekpage s structure))
          ((string-equal (getf-post "Command" postlines) "Refresh")
           (setq structure (read-user-string (cdr (pop postlines))))
           (output-fastseekpage s structure))
          ((string-equal (getf-post "Command" postlines) "Display")
           (setq structure (read-user-string (cdr (pop postlines))))
           (process-fastlookpage s structure 1 *count*))
          (t (http-problem s "Bad request.")))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun output-fastseekpage (s structure)
  (format-html s) (crlf s)
  (format s "<head>")
  (format s "<title>Seek</title>") (crlf s)
  (format-javascript s) (crlf s)
  (format s "</head>") (crlf s)
  (format-body s *bgcolor*) (crlf s)
  (output-header s)
  (format-border s) (crlf s)
  (output-fastseek s structure)
  (finish-border s) (crlf s)
  (output-footer s)
  (finish-body s) (crlf s)
  (finish-html s) (crlf s)
  'done)

(defun output-fastseek (s structure)
  (let ((*buttons* 0))
    (format s "<form name='form1' action='~A?'>" *performative*) (crlf s)
    (format s "Find every <b>")
    (format s "<a href='~A?Class=~A'>~A</a>"
            *performative* (stringize (cadr structure)) (prettyname (cadr structure)))
    (format s "</b> with the following properties.")
    (format-hidden s "Object" (stringize (car structure))) (crlf s)
    (format-hidden s "Class" (stringize (cadr structure))) (crlf s)
    (format s "<br/>")
    (format s "<table cellspacing='3'>")
    (do ((l (queryable-slots (cadr structure)) (cdr l)) (values) (style) (label))
        ((null l))
        (setq style (find-searchstyle (car l)))
        (setq label (find-searchlabel (car l)))
        (setq values (getslotvals (car l) (cddr structure)))
        (format s "<tr><th align='left' valign='top'>")
        (output-slotlink s (car l))
        (format s "</th><td>")
        (output-fastview-cells s (car l) values structure style)
        (format s "</td>")
        (when label (format s "<td>~A</td>" label))
        (format s "</tr>")
        (crlf s))
    (format s "</table>") (crlf s)
    (output-fastseek-display s (cadr structure))
    (format s "</form>")
    'done))

(defun output-fastseek-display (s class)
  (declare (ignore class))
  (format s "<input type='button' name='Command' value='Display'
                    onClick='postDisplay(this.form)'/>"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; fastlookpage
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod process (s (command (eql 'fastlookpage)) postlines)
  (let (dum start end (*performative* "fastlookpage"))
    (cond ((setq dum (getf-post "Class" postlines))
           (setq dum `(? ,(read-user-string dum)))
           (setq start (or (read-value-string (getf-post "Start" postlines)) 1))
           (setq end (or (read-value-string (getf-post "End" postlines)) *count*))
           (process-fastlookpage s dum start end))
          ((setq dum (getf-post "Structure" postlines))
           (setq start (or (read-value-string (getf-post "Start" postlines)) 1))
           (setq end (or (read-value-string (getf-post "End" postlines)) *count*))
           (process-fastlookpage s (read-user-string dum) start end))
          (t (http-problem s "Bad request.")))))

(defun process-fastlookpage (s structure start end)
  (format-html s) (crlf s)
  (format-head s)
  (format s "<title>Look</title>")
  (finish-head s) (crlf s)
  (format-body s *bgcolor*) (crlf s)
  (output-header s)
  (format-margin s)
  (process-fastlook s structure start end)
  (finish-margin s)
  (output-footer s)
  (finish-body s) (crlf s)
  (finish-html s) (crlf s))

(defun process-fastlook (s structure start end)
  (let (objects sorter slots results count)
    (setq objects (findinstances (viewconvert structure) *gui*))
    (when (setq sorter (find-sorter (cadr structure)))
      (setq objects (sortem objects sorter 'ascending)))
    (multiple-value-setq (objects count start end) (trim objects start end))
    (setq slots (displayable-slots (cadr structure)))
    (setq results (prorequest `(ask-table ,objects ,slots)))
    (output-fastlook s structure objects slots results count start end)))

(defun output-fastlook (s structure objects slots results count start end)
  (format s "<center>")
  (cond ((= count 0)
         (format s "<table>")
         (format s "<tr><td align='center'>There are no answers.</td></tr>")
         (format s "<tr><td align='center'>")
         (output-fastlook-create s (cadr structure) structure)
         (format s "</td></tr></table>"))
        ((= count 1)
         (format s "<table>")
         (format s "<tr><td align='center'>There is 1 answer.</td></tr>")
         (format s "<tr><td>")
         (output-fastlook-inner s structure objects slots results)
         (format s "</td></tr>")
         (format s "<tr><td align='center'>")
         (output-fastlook-create s (cadr structure) structure)
         (format s "</td></tr></table>"))
        ((and (= start 1) (geqp end count))
         (format s "<table>")
         (format s "<tr><td align='center'>There are ~D answers.</td></tr>" count)
         (format s "<tr><td>")
         (output-fastlook-inner s structure objects slots results)
         (format s "</td></tr>")
         (format s "<tr><td align='center'>")
         (output-fastlook-create s (cadr structure) structure)
         (format s "</td></tr></table>"))
        (t (format s "<table>")
           (format s "<tr><td align='center'>There are ~D answers.  The following table shows answers ~A through ~A.</td></tr>"
                   count start end)
           (format s "<tr><td>")
           (output-fastlook-inner s structure objects slots results)
           (format s "</td></tr><tr><td align='center'>")
           (multiple-value-setq (start end) (kerchunk count start end))
           (output-fastlook-create s (cadr structure) structure)
           (output-fastlook-display s (cadr structure) structure start end)
           (format s "</td></tr></table>")))
  (format s "</center>") (crlf s))

(defun output-fastlook-inner (s structure items slots results)
  (let (class nohandle)
    (setq class (cadr structure))
    (setq nohandle (findp `(nodisplay ,class handle) *interface*))
    (format s "<table cellspacing='3' bgcolor='~A' border='~A'>" *bgcolor* *border*) (crlf s)
    (format s "<tr>")
    (unless nohandle
      (format s "<th>")
      (format s (iconify class))
      (format s "</th>"))
    (dolist (slot slots)
      (format s "<th>")
      (format s (iconify slot))
      (format s "</th>")
      (crlf s))
    (format s "</tr>") (crlf s)
    (do ((l items (cdr l)) (m results (cdr m)) (flag nil (not flag)))
        ((null l))
        (if flag (format s "<tr>") (format s "<tr bgcolor='#EEEEEE'>"))
        (unless nohandle
          (format s "<td>")
          (output-handle s (car l))
          (format s "</td>"))
        (do ((n (car m) (cdr n)) (slots slots (cdr slots)) (style) (vals)) 
            ((null n))
            (setq style (find-comparestyle (car slots)))
            (setq vals (car n))
            (setq vals (remove 'unknown vals))
            (if (every #'(lambda (val) (numberp val)) vals)
              (format s "<td align='right'>")
              (format s "<td>"))
            (output-cells s (car slots) vals style)
            (format s "</td>"))
        (format s "</tr>")
        (crlf s))
    (format s "</table>") (crlf s)))

(defmethod output-fastlook-create (s class structure)
  (declare (ignore class))
  (unless (findp `(or (nocommand ,*gui* create) (nocreate ,*gui* ,(cadr structure))) *interface*)
    (format s "<form action='fastcreatepage?' method='post'>" (stringize structure))
    (format-hidden s "Class" (stringize (cadr structure)))
    (format-button s "Command" "Create")
    (format s " a new ~A." (prettify (cadr structure)))
    (format s "</form>") (crlf s)))

(defmethod output-fastlook-display (s class structure start end)
  (declare (ignore class))
  (format s "<form action='~A?' method='post'>" *performative*)
  (format-hidden s "Structure" (htmlify (prin1-to-string structure)))
  (format-button s "Command" "Display")
  (format s "answers ")
  (format-text s "Start" (princ-to-string start) 5)
  (format s " through ")
  (format-text s "End" (princ-to-string end) 5)
  (format s "</form>") (crlf s))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; fastshowpage
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod process (s (command (eql 'fastshowpage)) postlines)
  (let (dum start end (*performative* "fastshowpage"))
    (cond ((setq dum (getf-post "Class" postlines))
           (setq dum `(? ,(read-user-string dum)))
           (setq start (or (read-value-string (getf-post "Start" postlines)) 1))
           (setq end (or (read-value-string (getf-post "End" postlines)) *count*))
           (process-fastshowpage s dum start end))
          ((setq dum (getf-post "Structure" postlines))
           (setq start (or (read-value-string (getf-post "Start" postlines)) 1))
           (setq end (or (read-value-string (getf-post "End" postlines)) *count*))
           (process-fastshowpage s (read-user-string dum) start end))
          (t (http-problem s "Bad request.")))))

(defun process-fastshowpage (s structure start end)
  (format-html s) (crlf s)
  (format-head s)
  (format s "<title>Show</title>") (crlf s)
  (format-javascript s) (crlf s)
  (finish-head s) (crlf s)
  (format-body s *bgcolor*) (crlf s)
  (output-header s)
  (format-border s)
  (output-fastshow-query s structure)
  (format s "<hr width='800'/>") (crlf s)
  (process-fastlook s structure start end)
  (finish-border s)
  (output-footer s)
  (finish-body s) (crlf s)
  (finish-html s) (crlf s))

(defun output-fastshow-query (s structure)
  (let ((*buttons* 0))
    (format s "<form name='form1' action='~A?'>" *performative*)
    (format s "Find every <b>")
    (format s "<a href='~A?Class=~A'>~A</a>"
            *performative* (stringize (cadr structure)) (prettyname (cadr structure)))
    (format s "</b> with the following properties.")
    (format-hidden s "Object" (stringize (car structure))) (crlf s)
    (format-hidden s "Class" (stringize (cadr structure))) (crlf s)
    (format s "<br/>")
    (format s "<table cellspacing='3'>")
    (do ((l (queryable-slots (cadr structure)) (cdr l)) (values) (style) (label))
        ((null l))
        (setq style (find-searchstyle (car l)))
        (setq label (find-searchlabel (car l)))
        (setq values (getslotvals (car l) (cddr structure)))
        (format s "<tr><th align='left' valign='top'>")
        (output-slotlink s (car l))
        (format s "</th><td>")
        (output-fastshow-cells s (car l) values style)
        (format s "</td>")
        (when label (format s "<td>~A</td>" label))
        (format s "</tr>")
        (crlf s))
    (format s "</table>") (crlf s)
    (format s "</form>")
    'done))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod output-fastshow-cells (s slot values (style (eql 'checkbox)))
  (let (options)
    (setq options (findalternatives slot))
    (when options (output-newcheckbox s slot options values))
    (crlf s)))

(defmethod output-fastshow-cells (s slot values (style (eql 'combobox)))
  (output-fastshow-deadcells s slot values style))

(defmethod output-fastshow-cells (s slot values (style (eql 'dateinput)))
  (output-fastshow-livecells s slot values style))

(defmethod output-fastshow-cells (s slot values (style (eql 'datestyle)))
  (output-fastshow-deadcells s slot values style))

(defmethod output-fastshow-cells (s slot values (style (eql 'dollarinput)))
  (output-fastshow-livecells s slot values style))

(defmethod output-fastshow-cells (s slot values (style (eql 'dollarstyle)))
  (output-fastshow-deadcells s slot values style))

(defmethod output-fastshow-cells (s slot values (style (eql 'emailstyle)))
  (output-fastshow-deadcells s slot values style))

(defmethod output-fastshow-cells (s slot values (style (eql 'fastselector)))
  (output-fastshow-deadcells s slot values style))

(defmethod output-fastshow-cells (s slot values (style (eql 'fancyselector)))
  (output-fastshow-deadcells s slot values style))
 
(defmethod output-fastshow-cells (s slot values (style (eql 'glyph)))
  (output-fastshow-deadcells s slot values style))

(defmethod output-fastshow-cells (s slot values (style (eql 'htmlstyle)))
  (output-fastshow-deadcells s slot values style))

(defmethod output-fastshow-cells (s slot values (style (eql 'imagestyle)))
  (output-fastshow-deadcells s slot values style))

(defmethod output-fastshow-cells (s slot values (style (eql 'menu)))
  (let (options)
    (setq options (findalternatives slot))
    (when options (output-newmenu s slot options values))
    (crlf s)))

(defmethod output-fastshow-cells (s slot values (style (eql 'password)))
  (output-fastshow-livecells s slot values style))

(defmethod output-fastshow-cells (s slot values (style (eql 'prettystyle)))
  (output-fastshow-deadcells s slot values style))

(defmethod output-fastshow-cells (s slot values (style (eql 'radiobutton)))
  (output-fastshow-deadcells s slot values style))

(defmethod output-fastshow-cells (s slot values (style (eql 'selector)))
  (output-fastshow-livecells s slot values style))

(defmethod output-fastshow-cells (s slot values (style (eql 'stringfield)))
  (output-fastshow-livecells s slot values style))

(defmethod output-fastshow-cells (s slot values (style (eql 'subframe)))
  (output-fastshow-livecells s slot values style))

(defmethod output-fastshow-cells (s slot values (style (eql 'tabulator)))
  (let (class slots results)
    (setq class (find-range slot))
    (setq slots (queryable-slots class))
    (setq results (prorequest `(ask-table ,values ,slots)))
    (format s "<div>")
    (output-fastupdate-inner s slot class values slots results)
    (format s "</div>")))

(defmethod output-fastshow-cells (s slot values (style (eql 'text)))
  (output-fastshow-livecells s slot values style))

(defmethod output-fastshow-cells (s slot values (style (eql 'textarea)))
  (output-fastshow-livecells s slot values style))

(defmethod output-fastshow-cells (s slot values (style (eql 'typein)))
  (output-fastshow-livecells s slot values style))

(defmethod output-fastshow-cells (s slot values (style (eql 'urlstyle)))
  (output-fastshow-deadcells s slot values style))

(defmethod output-fastshow-cells (s slot values style)
  (output-fastshow-deadcells s slot values style))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun output-fastshow-deadcells (s slot values style)
  (format s "<table cellpadding='0' cellspacing='0'>")
  (dolist (value values)
    (format s "<tr><td>")
    (output-fastshow-cell s slot value style) (crlf s)
    (format s "</td></tr>"))
  (format s "</table>") (crlf s))

(defun output-fastshow-livecells (s slot values style)
  (format s "<table cellpadding='0' cellspacing='0'>")
  (dolist (value values)
    (format s "<tr><td>")
    (output-fastshow-cell s slot value style) (crlf s)
    (format s "</td></tr>"))
  (when (or (null values) (find-multivalued slot))
    (format s "<tr><td>")
    (output-fastshow-cell s slot nil style) (crlf s)
    (format s "</td></tr>"))
  (format s "</table>") (crlf s)
  'done)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod output-fastshow-cell (s slot value (style (eql 'checkbox)))
  (let (options)
    (setq options (findalternatives slot))
    (when options (output-newcheckbox s slot options (list value)))
    (crlf s)))

(defmethod output-fastshow-cell (s slot value (style (eql 'combobox)))
  (let (options)
    (when (setq options (findalternatives slot))
      (setq options (cons 'unknown options))
      (output-newcombobox s slot options (prettyname value) 20))))

(defmethod output-fastshow-cell (s slot value (style (eql 'dateinput)))
  (output-newdateinput s slot value))

(defmethod output-fastshow-cell (s slot value (style (eql 'datestyle)))
  (output-hidden s slot value)
  (if value (output-simple s value) (format s "~A" (iconify slot))))

(defmethod output-fastshow-cell (s slot value (style (eql 'dollarinput)))
  (output-newdollarinput s slot value 6))

(defmethod output-fastshow-cell (s slot value (style (eql 'dollarstyle)))
  (output-hidden s slot value)
  (if (realp value) (format s "$~$" value) (format s "~A" (iconify slot))))

(defmethod output-fastshow-cell (s slot value (style (eql 'emailstyle)))
  (output-hidden s slot value)
  (if value (format s "<a href='mailto:~A'><font color='red'>~A</font></a>" value value)
      (format s "~A" (iconify slot))))

(defmethod output-fastshow-cell (s slot value (style (eql 'fastselector)))
  (let (options)
    (when (setq options (findalternatives slot))
      (setq options (cons 'unknown options))
      (output-newselector s slot options value))))

(defmethod output-fastshow-cell (s slot value (style (eql 'fancyselector)))
  (let (options)
    (when (setq options (findalternatives slot))
      (setq options (cons 'unknown options))
      (output-newselector s slot options value))))

(defmethod output-fastshow-cell (s slot value (style (eql 'glyph)))
  (output-hidden s slot value)
  (format s "~A" (iconify slot)))

(defmethod output-fastshow-cell (s slot value (style (eql 'htmlstyle)))
  (output-hidden s slot value)
  (if value (format s "~A" value) (format s "~A" (iconify slot))))

(defmethod output-fastshow-cell (s slot value (style (eql 'imagestyle)))
  (output-hidden s slot value)
  (if value (format s "<img src='~A'/>" value) (format s "~A" (iconify slot))))

(defmethod output-fastshow-cell (s slot value (style (eql 'menu)))
  (let (options)
    (setq options (findalternatives slot))
    (when options (output-newmenu s slot options (list value)))
    (crlf s)))

(defmethod output-fastshow-cell (s slot value (style (eql 'password)))
  (format-newpassword s (stringize slot) (prettyname value) 20))

(defmethod output-fastshow-cell (s slot value (style (eql 'prettystyle)))
  (output-hidden s slot value)
  (format s "~A" (iconify slot)))

(defmethod output-fastshow-cell (s slot value (style (eql 'radiobutton)))
  (let (options)
    (cond ((setq options (findalternatives slot))
           (setq options (cons nil options))
           (output-newradiobutton s slot options value))
          (t (format-hidden s (stringize slot) "")))))

(defmethod output-fastshow-cell (s slot value (style (eql 'selector)))
  (let (options)
    (when (setq options (findalternatives slot))
      (setq options (cons 'unknown options))
      (output-newselector s slot options value))))

(defmethod output-fastshow-cell (s slot value (style (eql 'stringfield)))
  (format-newstringfield s (stringize slot) (prettyname value) 20))

(defmethod output-fastshow-cell (s slot value (style (eql 'subframe)))
  (let (class)
    (setq class (find-range slot))
    (format s "<div slot='~A' id='~A' category='~A' style='border:groove; width:640'>"
            slot value class)
    (format s "<span>~A</span>" (prettyname value))
    (format s "<br/>")
    (output-fastshow-query s (changeitem value class))
    (format s "</div>")))

(defmethod output-fastshow-cell (s slot value (style (eql 'tabulator)))
  (let (class slots results)
    (setq class (find-range slot))
    (setq slots (queryable-slots class))
    (setq results (prorequest `(ask-table ,(list value) ,slots)))
    (format s "<div>")
    (output-fastupdate-inner s slot class (list value) slots results)
    (format s "</div>")))

(defmethod output-fastshow-cell (s slot value (style (eql 'text)))
  (output-fastshow-cell s slot value 'stringfield))

(defmethod output-fastshow-cell (s slot value (style (eql 'textarea)))
  (output-fastshow-cell s slot value 'stringfield))

(defmethod output-fastshow-cell (s slot value (style (eql 'typein)))
  (format-newtypein s (stringize slot) (stringize value) 20))

(defmethod output-fastshow-cell (s slot value (style (eql 'urlstyle)))
  (output-hidden s slot value)
  (if value (format s "<a href='~A'>~A</a>" value (htmlify value))
      (format s "~A" (iconify slot))))

(defmethod output-fastshow-cell (s slot value style)
  (declare (ignore style))
  (output-hidden s slot value)
  (format s "~A" (iconify slot)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; fastviewpage
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod process (s (command (eql 'fastviewpage)) postlines)
  (let (dum start end (*performative* "fastviewpage"))
    (cond ((setq dum (getf-post "Class" postlines))
           (setq dum (fastsearchstructure (read-user-string dum)))
           (setq start (or (read-value-string (getf-post "Start" postlines)) 1))
           (setq end (or (read-value-string (getf-post "End" postlines)) *count*))
           (process-fastviewpage s dum start end))
          ((setq dum (getf-post "Structure" postlines))
           (setq start (or (read-value-string (getf-post "Start" postlines)) 1))
           (setq end (or (read-value-string (getf-post "End" postlines)) *count*))
           (process-fastviewpage s (read-user-string dum) start end))
          (t (http-problem s "Bad request.")))))

(defun process-fastviewpage (s structure start end)
  (format-html s) (crlf s)
  (format-head s) (crlf s)
  (format s "<title>View</title>") (crlf s)
  (format-javascript s) (crlf s)
  (finish-head s) (crlf s)
  (format-body s *bgcolor*) (crlf s)
  (output-header s)
  (format-border s)
  (output-fastview-query s structure)
  (format s "<hr width='800'/>") (crlf s)
  (process-fastlook s structure start end)
  (finish-border s)
  (output-footer s)
  (finish-body s) (crlf s)
  (finish-html s) (crlf s))

(defun output-fastview-query (s structure)
  (let ((*buttons* 0))
    (format s "<form name='form1' action='~A?'>" *performative*)
    (format s "Find every <b>")
    (format s "<a href='~A?Class=~A'>~A</a>"
            *performative* (stringize (cadr structure)) (prettyname (cadr structure)))
    (format s "</b> with the following properties.")
    (format-hidden s "Object" (stringize (car structure))) (crlf s)
    (format-hidden s "Class" (stringize (cadr structure))) (crlf s)
    (format s "<br/>")
    (format s "<table cellspacing='3'>")
    (do ((l (queryable-slots (cadr structure)) (cdr l)) (values) (style) (label))
        ((null l))
        (setq style (find-searchstyle (car l)))
        (setq label (find-searchlabel (car l)))
        (setq values (getslotvals (car l) (cddr structure)))
        (format s "<tr><th align='left' valign='top'>")
        (output-slotlink s (car l))
        (format s "</th><td>")
        (output-fastview-cells s (car l) values structure style)
        (format s "</td>")
        (when label (format s "<td>~A</td>" label))
        (format s "</tr>")
        (crlf s))
    (format s "</table>") (crlf s)
    (format s "</form>")
    'done))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod output-fastview-cells (s slot values structure (style (eql 'checkbox)))
  (let (options)
    (setq options (fastoptions slot structure))
    (when options (output-newcheckbox s slot options values))
    (crlf s)))

(defmethod output-fastview-cells (s slot values structure (style (eql 'combobox)))
  (output-fastview-deadcells s slot values structure style))

(defmethod output-fastview-cells (s slot values structure (style (eql 'dateinput)))
  (output-fastview-livecells s slot values structure style))

(defmethod output-fastview-cells (s slot values structure (style (eql 'datestyle)))
  (output-fastview-deadcells s slot values structure style))

(defmethod output-fastview-cells (s slot values structure (style (eql 'dollarinput)))
  (output-fastview-livecells s slot values structure style))

(defmethod output-fastview-cells (s slot values structure (style (eql 'dollarstyle)))
  (output-fastview-deadcells s slot values structure style))

(defmethod output-fastview-cells (s slot values structure (style (eql 'emailstyle)))
  (output-fastview-deadcells s slot values structure style))

(defmethod output-fastview-cells (s slot values structure (style (eql 'fastselector)))
  (output-fastview-deadcells s slot values structure style))

(defmethod output-fastview-cells (s slot values structure (style (eql 'fancyselector)))
  (output-fastview-deadcells s slot values structure style))

(defmethod output-fastview-cells (s slot values structure (style (eql 'glyph)))
  (output-fastview-deadcells s slot values structure style))

(defmethod output-fastview-cells (s slot values structure (style (eql 'htmlstyle)))
  (output-fastview-deadcells s slot values structure style))

(defmethod output-fastview-cells (s slot values structure (style (eql 'imagestyle)))
  (output-fastview-deadcells s slot values structure style))

(defmethod output-fastview-cells (s slot values structure (style (eql 'menu)))
  (let (options)
    (setq options (fastoptions slot structure))
    (when options (output-newmenu s slot options values))
    (crlf s)))

(defmethod output-fastview-cells (s slot values structure (style (eql 'password)))
  (output-fastview-livecells s slot values structure style))

(defmethod output-fastview-cells (s slot values structure (style (eql 'prettystyle)))
  (output-fastview-deadcells s slot values structure style))

(defmethod output-fastview-cells (s slot values structure (style (eql 'radiobutton)))
  (output-fastview-livecells s slot values structure style))

(defmethod output-fastview-cells (s slot values structure (style (eql 'selector)))
  (output-fastview-livecells s slot values structure style))

(defmethod output-fastview-cells (s slot values structure (style (eql 'selector)))
  (output-multiselector s slot values structure))

(defmethod output-fastview-cells (s slot values structure (style (eql 'stringfield)))
  (output-fastview-livecells s slot values structure style))

(defmethod output-fastview-cells (s slot values structure (style (eql 'subframe)))
  (output-fastview-livecells s slot values structure style))

(defmethod output-fastview-cells (s slot values structure (style (eql 'tabulator)))
  (declare (ignore structure))
  (let (class slots results)
    (setq class (find-range slot))
    (setq slots (queryable-slots class))
    (setq results (prorequest `(ask-table ,values ,slots)))
    (format s "<div>")
    (output-fastupdate-inner s slot class values slots results)
    (format s "</div>")))

(defmethod output-fastview-cells (s slot values structure (style (eql 'text)))
  (output-fastview-livecells s slot values structure style))

(defmethod output-fastview-cells (s slot values structure (style (eql 'textarea)))
  (output-fastview-livecells s slot values structure style))

(defmethod output-fastview-cells (s slot values structure (style (eql 'typein)))
  (output-fastview-livecells s slot values structure style))

(defmethod output-fastview-cells (s slot values structure (style (eql 'urlstyle)))
  (output-fastview-deadcells s slot values structure style))

(defmethod output-fastview-cells (s slot values structure style)
  (output-fastview-deadcells s slot values structure style))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun output-fastview-deadcells (s slot values structure style)
  (format s "<table cellpadding='0' cellspacing='0'>")
  (dolist (value values)
    (format s "<tr><td>")
    (output-fastview-cell s slot value structure style) (crlf s)
    (format s "</td></tr>"))
  (format s "</table>") (crlf s))

(defun output-fastview-livecells (s slot values structure style)
  (format s "<table cellpadding='0' cellspacing='0'>")
  (dolist (value values)
    (format s "<tr><td>")
    (output-fastview-cell s slot value structure style) (crlf s)
    (format s "</td></tr>"))
  (when (or (null values) (find-multivalued slot))
    (format s "<tr><td>")
    (output-fastview-cell s slot nil structure style) (crlf s)
    (format s "</td></tr>"))
  (format s "</table>") (crlf s)
  'done)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod output-fastview-cell (s slot value structure (style (eql 'checkbox)))
  (let (options)
    (setq options (fastoptions slot structure))
    (when options (output-newcheckbox s slot options (list value)))
    (crlf s)))

(defmethod output-fastview-cell (s slot value structure (style (eql 'combobox)))
  (let (options)
    (when (setq options (fastoptions slot structure))
      (setq options (cons 'unknown options))
      (output-newcombobox s slot options (prettyname value) 20))))

(defmethod output-fastview-cell (s slot value structure (style (eql 'dateinput)))
  (declare (ignore structure))
  (output-newdateinput s slot value))

(defmethod output-fastview-cell (s slot value structure (style (eql 'datestyle)))
  (declare (ignore structure))
  (output-hidden s slot value)
  (when value (output-simple s value)))

(defmethod output-fastview-cell (s slot value structure (style (eql 'dollarinput)))
  (declare (ignore structure))
  (output-newdollarinput s slot value 6))

(defmethod output-fastview-cell (s slot value structure (style (eql 'dollarstyle)))
  (declare (ignore structure))
  (output-hidden s slot value)
  (when (realp value) (format s "$~$" value)))

(defmethod output-fastview-cell (s slot value structure (style (eql 'emailstyle)))
  (declare (ignore structure))
  (output-hidden s slot value)
  (when value (format s "<a href='mailto:~A'><font color='red'>~A</font></a>" value value)))

(defmethod output-fastview-cell (s slot value structure (style (eql 'fastselector)))
  (let (options)
    (setq options (cons 'unknown (fastoptions slot structure)))
    (output-newselector s slot options value)))

(defmethod output-fastview-cell (s slot value structure (style (eql 'fancyselector)))
  (let (options)
    (setq options (cons 'unknown (fastoptions slot structure)))
    (output-newselector s slot options value)))

(defmethod output-fastview-cell (s slot value structure (style (eql 'glyph)))
  (declare (ignore structure))
  (output-hidden s slot value))

(defmethod output-fastview-cell (s slot value structure (style (eql 'htmlstyle)))
  (declare (ignore structure))
  (output-hidden s slot value)
  (when value (format s "~A" value)))

(defmethod output-fastview-cell (s slot value structure (style (eql 'imagestyle)))
  (declare (ignore structure))
  (output-hidden s slot value)
  (when value (format s "<img src='~A'/>" value)))

(defmethod output-fastview-cell (s slot value structure (style (eql 'menu)))
  (let (options)
    (setq options (fastoptions slot structure))
    (when options (output-newmenu s slot options (list value)))
    (crlf s)))

(defmethod output-fastview-cell (s slot value structure (style (eql 'password)))
  (declare (ignore structure))
  (format-newpassword s (stringize slot) (prettyname value) 20))

(defmethod output-fastview-cell (s slot value structure (style (eql 'prettystyle)))
  (declare (ignore structure))
  (output-hidden s slot value))

(defmethod output-fastview-cell (s slot value structure (style (eql 'radiobutton)))
  (let (options)
    (cond ((setq options (fastoptions slot structure))
           (setq options (cons nil options))
           (output-newradiobutton s slot options value))
          (t (format-hidden s (stringize slot) "")))))

(defmethod output-fastview-cell (s slot value structure (style (eql 'selector)))
  (let (options)
    (cond ((setq options (fastoptions slot structure))
	   (output-newselector s slot (cons 'unknown options) value))
	  (t (output-disselector s "None")))))

(defmethod output-fastview-cell (s slot value structure (style (eql 'stringfield)))
  (declare (ignore structure))
  (format-newstringfield s (stringize slot) (prettyname value) 20))

(defmethod output-fastview-cell (s slot value structure (style (eql 'subframe)))
  (declare (ignore structure))
  (let (class)
    (setq class (find-range slot))
    (format s "<div slot='~A' id='~A' category='~A' style='border:groove; width:640'>"
            slot value class)
    (format s "<span>~A</span>" (prettyname value))
    (format s "<br/>")
    (output-fastview-query s (changeitem value class))
    (format s "</div>")))

(defmethod output-fastview-cell (s slot value structure (style (eql 'tabulator)))
  (declare (ignore structure))
  (let (class slots results)
    (setq class (find-range slot))
    (setq slots (queryable-slots class))
    (setq results (prorequest `(ask-table ,(list value) ,slots)))
    (format s "<div>")
    (output-fastupdate-inner s slot class (list value) slots results)
    (format s "</div>")))

(defmethod output-fastview-cell (s slot value structure (style (eql 'text)))
  (declare (ignore structure))
  (format-newstringfield s (stringize slot) (prettyname value) 20))

(defmethod output-fastview-cell (s slot value structure (style (eql 'textarea)))
  (declare (ignore structure))
  (format-newstringfield s (stringize slot) (prettyname value) 20))

(defmethod output-fastview-cell (s slot value structure (style (eql 'typein)))
  (declare (ignore structure))
  (format-newtypein s (stringize slot) (stringize value) 20))

(defmethod output-fastview-cell (s slot value structure (style (eql 'urlstyle)))
  (declare (ignore structure))
  (output-hidden s slot value)
  (when value (format s "<a href='~A'>~A</a>" value (htmlify value))))

(defmethod output-fastview-cell (s slot value structure style)
  (declare (ignore structure style))
  (output-hidden s slot value))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; fastdisplaypage
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod process (s (command (eql 'fastdisplaypage)) postlines)
  (let (dum start end (*performative* "fastdisplaypage"))
    (cond ((setq dum (getf-post "Class" postlines))
           (setq dum (fastsearchstructure (read-user-string dum)))
           (setq start (or (read-value-string (getf-post "Start" postlines)) 1))
           (setq end (or (read-value-string (getf-post "End" postlines)) *count*))
           (process-fastdisplaypage s dum start end))
          ((setq dum (getf-post "Structure" postlines))
           (setq start (or (read-value-string (getf-post "Start" postlines)) 1))
           (setq end (or (read-value-string (getf-post "End" postlines)) *count*))
           (process-fastdisplaypage s (read-user-string dum) start end))
          (t (http-problem s "Bad request.")))))

(defun process-fastdisplaypage (s structure start end)
  (format-html s) (crlf s)
  (format-head s) (crlf s)
  (format s "<title>Display</title>") (crlf s)
  (format-javascript s) (crlf s)
  (finish-head s) (crlf s)
  (format-body s *bgcolor*) (crlf s)
  (output-header s)
  (format-border s)
  (process-fastdisplay s structure start end)
  (finish-border s)
  (output-footer s)
  (finish-body s) (crlf s)
  (finish-html s) (crlf s)
  'done)

(defun process-fastdisplay (s structure start end)
  (let (objects sorter count)
    (setq objects (findinstances (viewconvert structure) *gui*))
    (when (setq sorter (find-sorter (cadr structure)))
      (setq objects (sortem objects sorter 'ascending)))
    (multiple-value-setq (objects count start end) (trim objects start end))
    (output-display s objects (cadr structure) count start end)
    (multiple-value-setq (start end) (kerchunk count start end))
    (output-fastdisplay-display s (cadr structure) structure start end)))

(defmethod output-fastdisplay-create (s class structure)
  (declare (ignore class))
  (unless (findp `(or (nocommand ,*gui* create) (nocreate ,*gui* ,(cadr structure))) *interface*)
    (format s "<form action='fastcreatepage?' method='post'>" (stringize structure))
    (format-hidden s "Class" (stringize (cadr structure)))
    (format-button s "Command" "Create")
    (format s " a new ~A." (prettify (cadr structure)))
    (format s "</form>") (crlf s)))

(defmethod output-fastdisplay-display (s class structure start end)
  (declare (ignore class))
  (format s "<form action='~A?' method='post'>" *performative*)
  (format-hidden s "Structure" (htmlify (prin1-to-string structure)))
  (format-button s "Command" "Display")
  (format s "answers ")
  (format-text s "Start" (princ-to-string start) 5)
  (format s " through ")
  (format-text s "End" (princ-to-string end) 5)
  (format s "</form>") (crlf s))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod output-display (s items class count start end)
  (format s "<center>")
  (format s "<br/>")
  (cond ((= count 1) (format s "<b>There is 1 answer.</b>"))
        ((<= count *count*)
         (format s "<b>There are ~A items matching your search.</b>" count))
        (t (format s "<b>There are ~A items matching your search.</b>" count)
           (format s "<br/>")
           (format s "The following table shows items ~A - ~A." start end)))
  (format s "</center>")
  (format s "<center>")
  (format s "<table cellpadding='4' border='0'>")
  (do ((l items) (count start))
      ((or (null l) (> count end)))
      (format s "<tr valign='top'>")
      (do ((i 1 (1+ i)))
          ((> i 4))
          (format s "<td width='25%' align='center'>")
          (when l (output-item s (car l) class)
                (setq l (cdr l) count (1+ count)))
          (format s "</td>"))
      (format s "</tr>") (crlf s))       
  (format s "</table>")
  (format s "</center>")
  'done)

(defmethod output-item (s item class)
  (let (structure thumb)
    (setq structure (makedisplaystructure item class))
    (setq thumb (thumbify item class))
    (format s "~A" thumb)
    (format s "<br/>")
    (format s "<font face='verdana' size='2'>")
    (format s "<B>")
    (output-handle s item)
    (format s "</B>")
    (format s "<table cellspacing='2' style='font-family:verdana; font-size:11px'>")
    (do ((l (cddr structure)) (slot) (values))
        ((null l))
        (setq slot (caar l))
        (multiple-value-setq (values l) (collectvalues slot l))
        (output-item-cell s slot values))
    (format s "</table>")
    (format s "</font>")
    (format s "<br/>")
    (format s "<br/>")))

(defun makedisplaystructure (object class)
  (let (values dum)
    (dolist (slot (displayable-slots class))
      (setq dum (request `(ask-all (,slot ?x) (,slot ,object ?x)) nil *gui*))
      (if dum (setq values (nreconc dum values)) (setq values (cons (list slot) values))))
    (cons object (cons class (nreverse values)))))

(defmethod thumbify (item class)
  (declare (ignore class))
  (let (thumb image)
    (setq thumb (result 'thumb item *gui*))
    (setq image (result 'image item *gui*))
    (cond ((and thumb image)
	   (format nil "<a href='~A'><img src='~A'/></a>" image thumb))
	  ((and thumb (not image)) (format nil "<img src='~A'/>" thumb))
	  ((and (not thumb) image)
	   (format nil "<a href='~A'><img src='~A' height='120'/></a>" image image))
	  (t (format nil "<div height='120' width='160'>&nbsp;</div>")))))

(defun output-item-cell (s slot values)
  (let (style)
    (format s "<tr><th align='left' valign='top'>")
    (output-slotlink s slot)
    (format s "</th><td>")
    (setq style (find-comparestyle slot))
    (when values (output-handle-in-style s (car values) style))
    (dolist (value (cdr values))
      (format s ", ")
      (output-handle-in-style s value style))
    (format s "</td></tr>")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; fastgallerypage
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod process (s (command (eql 'fastgallerypage)) postlines)
  (let (dum start end (*performative* "fastgallerypage"))
    (cond ((setq dum (getf-post "Class" postlines))
           (setq dum (fastsearchstructure (read-user-string dum)))
           (setq start (or (read-value-string (getf-post "Start" postlines)) 1))
           (setq end (or (read-value-string (getf-post "End" postlines)) *count*))
           (process-fastgallerypage s dum start end))
          ((setq dum (getf-post "Structure" postlines))
           (setq start (or (read-value-string (getf-post "Start" postlines)) 1))
           (setq end (or (read-value-string (getf-post "End" postlines)) *count*))
           (process-fastgallerypage s (read-user-string dum) start end))
          (t (http-problem s "Bad request.")))))

(defun process-fastgallerypage (s structure start end)
  (format-html s) (crlf s)
  (format-head s) (crlf s)
  (format s "<title>Gallery</title>") (crlf s)
  (format-javascript s) (crlf s)
  (finish-head s) (crlf s)
  (format-body s *bgcolor*) (crlf s)
  (output-header s)
  (format-border s)
  (output-fastshow-query s structure)
  (format s "<hr width='800'/>") (crlf s)
  (process-fastdisplay s structure start end)
  (finish-border s)
  (output-footer s)
  (finish-body s) (crlf s)
  (finish-html s) (crlf s)
  'done)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; fasttableaupage
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod process (s (command (eql 'fasttableaupage)) postlines)
  (let (dum start end (*performative* "fasttableaupage"))
    (cond ((setq dum (getf-post "Class" postlines))
           (setq dum (fastsearchstructure (read-user-string dum)))
           (setq start (or (read-value-string (getf-post "Start" postlines)) 1))
           (setq end (or (read-value-string (getf-post "End" postlines)) *count*))
           (process-fasttableaupage s dum start end))
          ((setq dum (getf-post "Structure" postlines))
           (setq start (or (read-value-string (getf-post "Start" postlines)) 1))
           (setq end (or (read-value-string (getf-post "End" postlines)) *count*))
           (process-fasttableaupage s (read-user-string dum) start end))
          (t (http-problem s "Bad request.")))))

(defun process-fasttableaupage (s structure start end)
  (format-html s) (crlf s)
  (format-head s) (crlf s)
  (format s "<title>Tableau</title>") (crlf s)
  (format-javascript s) (crlf s)
  (finish-head s) (crlf s)
  (format-body s *bgcolor*) (crlf s)
  (output-header s)
  (format-border s)
  (output-fastview-query s structure)
  (format s "<hr width='800'/>") (crlf s)
  (process-fastdisplay s structure start end)
  (finish-border s)
  (output-footer s)
  (finish-body s) (crlf s)
  (finish-html s) (crlf s)
  'done)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; fastinspectpage
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod process (s (command (eql 'fastinspectpage)) postlines)
  (let (object class)
    (cond ((null postlines) (http-problem s "Bad request."))
          ((null (cdr postlines))
	   (setq object (read-value-string (cdr (pop postlines))))
	   (setq class (classify object *gui*))
	   (output-fastinspectpage s object class))
	  (t (http-problem s "Bad request.")))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod output-fastinspectpage (s object class)
  (format-html s) (crlf s)
  (format-head s) (crlf s)
  (output-title s (stringappend "Inspect " (prettify object))) (crlf s)
  (finish-head s) (crlf s)
  (format-body s *bgcolor*) (crlf s)
  (output-header s) (crlf s)
  (format-margin s) (crlf s)
  (output-fastinspect s object class)
  (finish-margin s) (crlf s)
  (output-footer s) (crlf s)
  (finish-body s) (crlf s)
  (finish-html s) (crlf s))

(defmethod output-fastinspect (s object class)
  (let (structure)
    (setq structure (inspectitem object class))   
    (format s "<B>~A</B> is " (prettyname object))
    (format s (article class))
    (format s " <B>" )
    (output-classlink s class)
    (format s "</B>.")
    (force-output s)
    (format s "<table cellspacing='8'>")
    (do ((l (cddr structure)) (slot) (values) (style) (label))
        ((null l))
      (setq slot (caar l))
      (setq style (find-inspectstyle slot))
      (setq label (find-inspectlabel slot))
      (multiple-value-setq (values l) (collectvalues slot l))
      (format s "<tr><th align='left' valign='top'>")
      (output-slotlink s slot)
      (format s "</th><td>")
      (output-cells s slot values style)
      (format s "</td>")
      (when label (format s "<td>~A</td>" label))
      (format s "</tr>"))
    (format s "</table>")
    (when (changeablep object class *gui*)
      (format s "<dl>")
      (format s "<dt>")
      (output-fastinspect-change s object class)
      (format s "</dt>")
      (format s "<dt>")
      (output-fastinspect-copy s object class)
      (format s "</dt>")
      (format s "<dt>")
      (output-fastinspect-delete s object class)
      (format s "</dt>")
      (format s "</dl>"))))

(defmethod output-fastinspect-change (s object class)
  (format s "<input type='button' value='Change'
                    onClick='location.replace(\"fastchangepage?Object=~A\")'\> this ~A."
          object (prettify class)))

(defmethod output-fastinspect-copy (s object class)
  (format s "<form action='fastcopypage?' method='post'>")
  (format-hidden s "Object" (stringize object))
  (format-button s "Command" " Copy")
  (format s " this ~A.</dt>" (prettify class))
  (format s "</form>"))

(defmethod output-fastinspect-delete (s object class)
  (format s "<form action='fastdeletepage?' method='post'>")
  (format-hidden s "Object" (stringize object))
  (format-button s "Command" "Delete")
  (format s " this ~A.</dt>" (prettify class))
  (format s "</form>"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod output-cells (s slot values (style (eql 'checkbox)))
  (let (options)
    (when (setq options (findalternatives slot))
      (output-discheckbox s slot options values))))

(defmethod output-cells (s slot values (style (eql 'combobox)))
  (output-deadcells s slot values style))

(defmethod output-cells (s slot values (style (eql 'dateinput)))
  (output-deadcells s slot values style))

(defmethod output-cells (s slot values (style (eql 'datestyle)))
  (when values (output-cell s slot (car values) style))
  (dolist (value (cdr values))
    (format s ", ")
    (output-cell s slot value style)))

(defmethod output-cells (s slot values (style (eql 'dollarinput)))
  (output-deadcells s slot values style))

(defmethod output-cells (s slot values (style (eql 'dollarstyle)))
  (when values (output-cell s slot (car values) style))
  (dolist (value (cdr values))
    (format s ", ")
    (output-cell s slot value style)))

(defmethod output-cells (s slot values (style (eql 'emailstyle)))
  (when values (output-cell s slot (car values) style))
  (dolist (value (cdr values))
    (format s ", ")
    (output-cell s slot value style)))

(defmethod output-cells (s slot values (style (eql 'fastselector)))
  (output-deadcells s slot values style))

(defmethod output-cells (s slot values (style (eql 'fancyselector)))
  (output-deadcells s slot values style))

(defmethod output-cells (s slot values (style (eql 'glyph)))
  (when values (output-cell s slot (car values) style))
  (dolist (value (cdr values))
    (format s ", ")
    (output-cell s slot value style)))

(defmethod output-cells (s slot values (style (eql 'htmlstyle)))
  (when values (output-cell s slot (car values) style))
  (dolist (value (cdr values))
    (format s ", ")
    (output-cell s slot value style)))

(defmethod output-cells (s slot values (style (eql 'imagestyle)))
  (when values (output-cell s slot (car values) style))
  (dolist (value (cdr values))
    (format s ", ")
    (output-cell s slot value style)))

(defmethod output-cells (s slot values (style (eql 'menu)))
  (let (options)
    (when (setq options (findalternatives slot))
      (output-dismenu s slot options values))))

(defmethod output-cells (s slot values (style (eql 'password)))
  (output-deadcells s slot values style))

(defmethod output-cells (s slot values (style (eql 'prettystyle)))
  (when values (output-cell s slot (car values) style))
  (dolist (value (cdr values))
    (format s ", ")
    (output-cell s slot value style)))

(defmethod output-cells (s slot values (style (eql 'radiobutton)))
  (output-deadcells s slot values style))

(defmethod output-cells (s slot values (style (eql 'selector)))
  (output-deadcells s slot values style))

(defmethod output-cells (s slot values (style (eql 'stringfield)))
  (output-deadcells s slot values style))

(defmethod output-cells (s slot values (style (eql 'subframe)))
  (output-deadcells s slot values style))

(defmethod output-cells (s slot values (style (eql 'text)))
  (output-deadcells s slot values style))

(defmethod output-cells (s slot values (style (eql 'textarea)))
  (output-deadcells s slot values style))

(defmethod output-cells (s slot values (style (eql 'typein)))
  (output-deadcells s slot values style))

(defmethod output-cells (s slot values (style (eql 'tabulator)))
  (let (class slots sorter results)
    (setq class (find-range slot))
    (setq slots (displayable-slots class))
    (when (setq sorter (find-sorter (find-range slot)))
      (setq values (sortem values sorter 'ascending)))
    (setq results (prorequest `(ask-table ,values ,slots)))
    (format s "<div>")
    (output-fastlook-inner s `(? ,class) values slots results)
    (format s "</div>")))

(defmethod output-cells (s slot values (style (eql 'urlstyle)))
  (when values (output-cell s slot (car values) style))
  (dolist (value (cdr values))
    (format s ", ")
    (output-cell s slot value style)))

(defmethod output-cells (s slot values style)
  (output-deadcells s slot values style))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun output-deadcells (s slot values style)
  (format s "<table cellpadding='0' cellspacing='0'>")
  (format s "<tr><td>")
  (output-cell s slot (car values) style) (crlf s)
  (format s "</td></tr>")
  (dolist (value (cdr values))
    (format s "<tr><td>")
    (output-cell s slot value style) (crlf s)
    (format s "</td></tr>"))
  (format s "</table>"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod output-cell (s slot value (style (eql 'checkbox)))
  (let (options)
    (when (setq options (findalternatives slot))
      (output-discheckbox s slot options (list value)))))

(defmethod output-cell (s slot value (style (eql 'combobox)))
  (declare (ignore slot))
  (format-distypein s (stringize value) 30))

(defmethod output-cell (s slot value (style (eql 'dateinput)))
  (output-disdateinput s slot value))

(defmethod output-cell (s slot value (style (eql 'datestyle)))
  (declare (ignore slot))
  (output-simple s value))

(defmethod output-cell (s slot value (style (eql 'dollarinput)))
  (output-disdollarinput s slot value 6))

(defmethod output-cell (s slot value (style (eql 'dollarstyle)))
  (declare (ignore slot))
  (when (realp value) (format s "$~$" value)))

(defmethod output-cell (s slot value (style (eql 'emailstyle)))
  (declare (ignore slot))
  (format s "<a href='mailto:~A'><font color='red'>~A</font></A>" value value))

(defmethod output-cell (s slot value (style (eql 'fastselector)))
  (declare (ignore slot))
  (output-disselector s value))

(defmethod output-cell (s slot value (style (eql 'fancyselector)))
  (declare (ignore slot))
  (format-distypein s (stringize value) 30))

(defmethod output-cell (s slot value (style (eql 'glyph)))
  (declare (ignore slot))
  (cond ((simplep value) (output-simple s value))
        (t (output-anchor s value))))

(defmethod output-cell (s slot value (style (eql 'htmlstyle)))
  (declare (ignore slot))
  (format s "~A" value))

(defmethod output-cell (s slot value (style (eql 'imagestyle)))
  (declare (ignore slot))
  (format s "<img src='~A'/>" value))

(defmethod output-cell (s slot value (style (eql 'menu)))
  (let (options)
    (when (setq options (findalternatives slot))
      (output-dismenu s slot options (list value)))))

(defmethod output-cell (s slot value (style (eql 'password)))
  (declare (ignore slot))
  (format-dispassword s (prettyname value) 20))

(defmethod output-cell (s slot value (style (eql 'prettystyle)))
  (declare (ignore slot))
  (output-simple s value))

(defmethod output-cell (s slot value (style (eql 'radiobutton)))
  (let (options)
    (when (setq options (findalternatives slot))
      (setq options (cons 'unknown options))
      (output-disradiobutton s options value))))

(defmethod output-cell (s slot value (style (eql 'selector)))
  (declare (ignore slot))
  (output-disselector s value))

(defmethod output-cell (s slot value (style (eql 'stringfield)))
  (declare (ignore slot))
  (format-distypein s (prettyname value) 20))

(defmethod output-cell (s slot value (style (eql 'subframe)))
  (declare (ignore slot))
  (format s "<div style='border:groove'>")
  (output-fastinspect s value (classify value *gui*))
  (format s "</div>"))

(defmethod output-cell (s slot value (style (eql 'tabulator)))
  (let (class slots results)
    (setq class (find-range slot))
    (setq slots (displayable-slots class))
    (setq results (prorequest `(ask-table ,(list value) ,slots)))
    (format s "<div>")
    (output-fastlook-inner s `(? ,class) (list value) slots results)
    (format s "</div>")))

(defmethod output-cell (s slot value (style (eql 'text)))
  (declare (ignore slot))
  (format-distypein s (prettyname value) 20))

(defmethod output-cell (s slot value (style (eql 'textarea)))
  (declare (ignore slot))
  (format-distypein s (prettyname value) 20))

(defmethod output-cell (s slot value (style (eql 'typein)))
  (declare (ignore slot))
  (format-distypein s (stringize value) 20))

(defmethod output-cell (s slot value (style (eql 'urlstyle)))
  (declare (ignore slot))
  (format s "<a href='~A'><font color='red'>Link</font></A>" value ))

(defmethod output-cell (s slot value style)
  (declare (ignore style))
  (output-cell s slot value 'glyph))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; fastcreatepage
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod process (s (command (eql 'fastcreatepage)) postlines)
  (let (class structure (*performative* "fastcreatepage"))
    (cond ((null postlines) (http-problem s "Bad request."))
          ((getf-post "Class" postlines)
           (setq class (read-value-string (cdr (pop postlines))))
           (process-fastcreatepage s class))
          ((string-equal (getf-post "Command" postlines) "Refresh")
           (setq structure (read-user-string (cdr (pop postlines))))
           (output-fastcreatepage s structure))
          ((string-equal (getf-post "Command" postlines) "Create")
           (setq structure (read-user-string (cdr (pop postlines))))
           (process-fastcreatepage-create s (car structure) (cadr structure) (cddr structure)))
          (t (http-problem s "Bad request.")))))

(defmethod process-fastcreatepage (s class)
  (output-fastcreatepage s (createitem (newinstance class) class)))

(defmethod process-fastcreatepage-create (s object class constraints)
  (let (result)
    (cond ((setq result (checkcreation object class constraints))
           (http-problems s result))
	  (t (defineobject object class constraints *gui*)
	     (output-fastcreatepage-success s object)))))

(defmethod checkcreation (object class constraints)
  (declare (ignore class constraints))
  (let (errors)
    (unless (eq (classify object *gui*) 'thing)
      (setq errors (cons "Object already exists." errors)))
    (when errors
      (setq errors (cons "Press the Back button, correct the error(s), and resubmit." errors)))
    (nreverse errors)))

(defun getslotval (slot constraints)
  (cadr (assoc slot constraints :test #'eq)))

(defun getslotvals (slot constraints)
  (do ((l constraints (cdr l)) (nl))
      ((null l) (nreverse nl))
      (when (and (eq (caar l) slot) (cdar l)) (setq nl (cons (cadar l) nl)))))

(defun output-fastcreatepage-success (s object)
  (format-html s) (crlf s)
  (format s "<head>")
  (format s "<title>Create ~A</title>" (prettify object)) (crlf s)
  (format s "</head>") (crlf s)
  (format s "<body leftmargin='0' topmargin='0' marginwidth='0' marginheight='0' bgcolor='~A' onLoad='~A'>"
          *bgcolor* (format nil "location.replace(\"fastinspectpage?Object=~A\")" object)) (crlf s)
  (output-header s)
  (format-border s) (crlf s)
  (format s "Creation successful.")
  (finish-border s) (crlf s)
  (output-footer s)
  (finish-body s) (crlf s)
  (finish-html s) (crlf s)
  'done)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun output-fastcreatepage (s structure)
  (format-html s) (crlf s)
  (format-head s) (crlf s)
  (output-title s (stringappend "Create " (prettify (cadr structure)))) (crlf s)
  (format-javascript s) (crlf s)
  (finish-head s) (crlf s)
  (force-output s)
  (format-body s *bgcolor* "hidePopup()") (crlf s)
  (output-header s)
  (format-border s) (crlf s)
  (output-fastcreate s structure)
  (finish-border s) (crlf s)
  (output-footer s)
  (finish-body s) (crlf s)
  (finish-html s) (crlf s)
  'done)

(defun output-fastcreate (s structure)
  (let ((*buttons* 0))
    (format s "<form name='form1' action='~A?'>" *performative*) (crlf s)
    (format s "<div id='~A' category='~A'>"
	    (stringize (car structure)) (stringize (cadr structure))) (crlf s)
    (format s "Create ")
    (cond ((triplep 'noupdate (cadr structure) 'handle *interface*)
           (format s "a new <b>~A</b>." (prettyname (cadr structure))))
          (t (format-id s (stringize (car structure)))
             (format s " as ~A <b>~A</b>." (article (cadr structure)) (prettyname (cadr structure)))))
    (format s "<br/>") (crlf s)
    (output-fastcreate-inner s structure)
    (output-fastcreate-create s (cadr structure)) (crlf s)
    (format s "</div>") (crlf s)
    (format s "</form>") (crlf s)))

(defun format-id (s value)
  (format s "<input type='text' name='Object' value='~A' size='30' qualifier='skip'
                  onChange='this.parentNode.id=this.value'/>" value))

(defun output-fastcreate-inner (s structure)
  (format s "<table cellspacing='3'>")
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
      (cond ((and (null (cdr values)) (not (find-multivalued slot)))
             (format s "<table cellpadding='0' cellspacing='0'>")
             (format s "<tr><td width='32'>&nbsp;</td><td>")
             (output-fastchange-cell s slot (car values) style)
             (format s "</td></tr>")
             (format s "</table>"))
            (t (output-fastchange-cells s slot values style)))
      (format s "</td>")
      (when label (format s "<td>~A</td>" label))
      (format s "</tr>")
      (crlf s))
  (format s "</table>") (crlf s))

(defmethod output-fastcreate-create (s class)
  (declare (ignore class))
  (when *debug*
    (format s "<input type='button' name='Command' value='Show' onClick='showstructure(this)'/>"))
  (format s "<input type='button' name='Command' value='Create' onClick='create(this)'/>"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; fastchangepage
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod process (s (command (eql 'fastchangepage)) postlines)
  (let (dum class structure (*performative* "fastchangepage"))
    (cond ((null postlines) (http-problem s "Bad request."))
          ((null (cdr postlines))
           (setq dum (read-value-string (cdr (pop postlines))))
           (setq class (classify dum *gui*))
           (process-fastchangepage s dum class (changeitem dum class)))
          ((string-equal (getf-post "Command" postlines) "Refresh")
           (setq structure (read-user-string (cdr (pop postlines))))
           (output-fastcreatepage s structure))
          ((string-equal (getf-post "Command" postlines) "Record")
           (setq structure (read-user-string (cdr (pop postlines))))
	   (process-fastchangepage-record s (car structure) (cadr structure) (cddr structure)))
          (t (http-problem s "Bad request.")))))

(defmethod process-fastchangepage (s object class structure)
  (declare (ignore object class))
  (output-fastchangepage s structure))

(defmethod process-fastchangepage-record (s object class constraints)
  (defineobject object class constraints *gui*)
  (output-fastchangepage-success s object))

(defmethod checkchange (object class constraints)
  (declare (ignore object class constraints))
  nil)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun output-fastchangepage (s structure)
  (format-html s) (crlf s)
  (format-head s) (crlf s)
  (output-title s (stringappend "Change " (prettify (car structure)))) (crlf s)
  (format-javascript s) (crlf s)
  (finish-head s) (crlf s)
  (force-output s)
  (format-body s *bgcolor* "hidePopup()") (crlf s)
  (output-header s)
  (format-border s) (crlf s)
  (output-fastchange s structure)
  (finish-border s) (crlf s)
  (output-footer s)
  (finish-body s) (crlf s)
  (finish-html s) (crlf s)
  'done)

(defun output-fastchange (s structure)
  (let ((*buttons* 0))
    (format s "<form name='form1' action='~A?'>" *performative*) (crlf s)
    (format s "<div id='~A' category='~A'>" (car structure) (cadr structure)) (crlf s)
    (output-handle s (car structure))
    (format s " is ~A ~A." (article (cadr structure)) (prettify (cadr structure)))
    (format s "<br/>") (crlf s)
    (output-fastchange-inner s structure)
    (output-fastchange-record s (cadr structure)) (crlf s)
    (format s "</div>") (crlf s)
    (format s "</form>") (crlf s)))

(defun output-fastchange-inner (s structure)
  (format s "<table cellspacing='3'>")
  (do ((l (cddr structure)) (slot) (style) (label)
       (controlslot) (controlvalues) (values))
      ((null l))
      (setq slot (caar l))
      (setq style (find-changestyle slot))
      (setq label (find-changelabel slot))
      (setq controlslot (result 'controlslot slot *manager*))
      (setq controlvalues (result 'controlvalue slot *manager*))
      (multiple-value-setq (values l) (collectvalues slot l))
      (if (null controlslot) (format s "<tr>")
          (format s "<tr controlslot='~A' controlvalues='~A'"
                  (stringize controlslot)
                  (stringizem controlvalues)))
      (format s "<th align='left' valign='top'>")
      (output-slotlink s slot)
      (format s "</th>")
      (format s "<td>")
      (cond ((and (null (cdr values)) (not (find-multivalued slot)))
             (format s "<table cellpadding='0' cellspacing='0'>")
             (format s "<tr><td width='32'>&nbsp;</td><td>")
             (output-fastchange-cell s slot (car values) style)
             (format s "</td></tr>")
             (format s "</table>"))
            (t (output-fastchange-cells s slot values style)))
      (format s "</td>")
      (when label (format s "<td>~A</td>" label))
      (format s "</tr>")
      (crlf s))
  (format s "</table>") (crlf s))

(defmethod output-fastchange-record (s class)
  (declare (ignore class))
  (when *debug*
    (format s "<input type='button' name='Command' value='Show' onClick='showstructure(this)'/>"))
  (format s "<input type='button' name='Command' value='Record' onClick='record(this)'/>"))

(defun output-fastchangepage-success (s object)
  (format-html s) (crlf s)
  (format s "<head>")
  (format s "<title>Change ~A</title>" (prettify object)) (crlf s)
  (format s "</head>") (crlf s)
  (format s "<body leftmargin='0' topmargin='0' marginwidth='0' marginheight='0' bgcolor='~A' onLoad='~A'>"
          *bgcolor* (format nil "location.replace(\"fastinspectpage?Object=~A\")" object)) (crlf s)
  (output-header s)
  (format-border s) (crlf s)
  (format s "Change successful.")
  (finish-border s) (crlf s)
  (output-footer s)
  (finish-body s) (crlf s)
  (finish-html s) (crlf s)
  'done)  

(defun stringizem (items)
  (let (ns)
    (cond ((null items) "")
          (t (setq ns (stringize (car items)))
             (dolist (item (cdr items))
               (setq ns (strappend ns "&" (stringize item))))
             ns))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod output-fastchange-cells (s slot values (style (eql 'checkbox)))
  (let (options)
    (setq options (findalternatives slot))
    (format s "<table cellpadding='0' cellspacing='0'>")
    (format s "<tr><td width='32' valign='top'>")
    (output-empty-button s)
    (output-empty-button s)
    (format s "</td><td>")
    (when options (output-coldcheckbox s slot options values))
    (format s "</td></tr>") (crlf s)
    (format s "</table>")
    (crlf s)))

(defmethod output-fastchange-cells (s slot values (style (eql 'combobox)))
  (output-fastchange-deadcells s slot values style))

(defmethod output-fastchange-cells (s slot values (style (eql 'dateinput)))
  (output-fastchange-livecells s slot values style))

(defmethod output-fastchange-cells (s slot values (style (eql 'datestyle)))
  (output-fastchange-deadcells s slot values style))

(defmethod output-fastchange-cells (s slot values (style (eql 'dollarinput)))
  (output-fastchange-livecells s slot values style))

(defmethod output-fastchange-cells (s slot values (style (eql 'dollarstyle)))
  (output-fastchange-deadcells s slot values style))

(defmethod output-fastchange-cells (s slot values (style (eql 'emailstyle)))
  (output-fastchange-deadcells s slot values style))

(defmethod output-fastchange-cells (s slot values (style (eql 'fastselector)))
  (output-fastchange-deadcells s slot values style))

(defmethod output-fastchange-cells (s slot values (style (eql 'fancyselector)))
  (output-fastchange-deadcells s slot values style))

(defmethod output-fastchange-cells (s slot values (style (eql 'glyph)))
  (output-fastchange-deadcells s slot values style))

(defmethod output-fastchange-cells (s slot values (style (eql 'htmlstyle)))
  (output-fastchange-deadcells s slot values style))

(defmethod output-fastchange-cells (s slot values (style (eql 'imagestyle)))
  (output-fastchange-deadcells s slot values style))

(defmethod output-fastchange-cells (s slot values (style (eql 'menu)))
  (let (options)
    (setq options (findalternatives slot))
    (format s "<table cellpadding='0' cellspacing='0'>")
    (format s "<tr><td width='32' valign='top'>")
    (output-empty-button s)
    (output-empty-button s)
    (format s "</td><td>")
    (when options (output-oldmenu s slot options values))
    (format s "</td></tr>") (crlf s)
    (format s "</table>")
    (crlf s)))

(defmethod output-fastchange-cells (s slot values (style (eql 'password)))
  (output-fastchange-livecells s slot values style))

(defmethod output-fastchange-cells (s slot values (style (eql 'prettystyle)))
  (output-fastchange-deadcells s slot values style))

(defmethod output-fastchange-cells (s slot values (style (eql 'radiobutton)))
  (output-fastchange-deadcells s slot values style))

(defmethod output-fastchange-cells (s slot values (style (eql 'selector)))
  (output-fastchange-livecells s slot values style))

(defmethod output-fastchange-cells (s slot values (style (eql 'stringfield)))
  (output-fastchange-livecells s slot values style))

(defmethod output-fastchange-cells (s slot values (style (eql 'subframe)))
  (format s "<table cellpadding='0' cellspacing='0'>")

  (format s "<tr id='~A' qualifier='skip' style='display:None'>" slot)
  (format s "<td width='32' valign='top'>")
  (output-additem s slot)
  (output-remitem s)
  (format s "</td><td>")
  (output-fastchange-cell s slot nil style)
  (format s "</td>")
  (format s "</tr>")

  (dolist (value values)
    (format s "<tr><td width='32' valign='top'>")
    (output-additem s slot)
    (output-remitem s)
    (format s "</td><td>")
    (output-fastchange-cell s slot value style) (crlf s)
    (format s "</td></tr>"))

  (format s "<tr>")
  (format s "<td width='32'>")
  (output-additem s slot)
  (output-empty-button s)
  (format s "</td>")
  (format s "<td></td>")
  (format s "</tr>")

  (format s "</table>")
  (crlf s))

(defmethod output-fastchange-cells (s slot values (style (eql 'tabulator)))
  (let (class slots results)
    (setq class (find-range slot))
    (setq slots (createable-slots class))
    (setq results (prorequest `(ask-table ,values ,slots)))
    (format s "<div>")
    (output-fastupdate-inner s slot class values slots results)
    (format s "</div>")))

(defmethod output-fastchange-cells (s slot values (style (eql 'text)))
  (output-fastchange-livecells s slot values style))

(defmethod output-fastchange-cells (s slot values (style (eql 'textarea)))
  (output-fastchange-livecells s slot values style))

(defmethod output-fastchange-cells (s slot values (style (eql 'typein)))
  (output-fastchange-livecells s slot values style))

(defmethod output-fastchange-cells (s slot values (style (eql 'urlstyle)))
  (output-fastchange-deadcells s slot values style))

(defmethod output-fastchange-cells (s slot values style)
  (output-fastchange-deadcells s slot values style))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod output-fastchange-cell (s slot value (style (eql 'checkbox)))
  (let (options)
    (setq options (findalternatives slot))
    (format s "<table cellpadding='0' cellspacing='0'>")
    (format s "<tr><td width='32' valign='top'>")
    (output-empty-button s)
    (output-empty-button s)
    (format s "</td><td>")
    (when options (output-coldcheckbox s slot options (list value)))
    (format s "</td></tr>") (crlf s)
    (format s "</table>")
    (crlf s)))

(defmethod output-fastchange-cell (s slot value (style (eql 'combobox)))
  (let (options)
    (when (setq options (findalternatives slot))
      (setq options (cons nil options))
      (output-oldcombobox s slot options (prettyname value) 40))))

(defmethod output-fastchange-cell (s slot value (style (eql 'dateinput)))
  (output-olddateinput s slot value))

(defmethod output-fastchange-cell (s slot value (style (eql 'datestyle)))
  (when value
    (output-hidden s (stringize slot) (stringize value))
    (output-simple s value)))

(defmethod output-fastchange-cell (s slot value (style (eql 'dollarinput)))
  (output-olddollarinput s slot value 6))

(defmethod output-fastchange-cell (s slot value (style (eql 'dollarstyle)))
  (when value
    (output-hidden s slot value)
    (format s "$~$" value)))

(defmethod output-fastchange-cell (s slot value (style (eql 'emailstyle)))
  (when value
    (output-hidden s slot value)
    (format s "<a href='mailto:~A'><font color='red'>~A</font></a>" value value)))

(defmethod output-fastchange-cell (s slot value (style (eql 'fastselector)))
  (output-oldfastselector s slot value))

(defmethod output-fastchange-cell (s slot value (style (eql 'fancyselector)))
  (output-oldfancyselector s slot value))

(defmethod output-fastchange-cell (s slot value (style (eql 'glyph)))
  (when value
    (output-hidden s slot value)
    (output-handle s value)))

(defmethod output-fastchange-cell (s slot value (style (eql 'htmlstyle)))
  (when value
    (output-hidden s slot value)
    (format s "~A" value)))

(defmethod output-fastchange-cell (s slot value (style (eql 'imagestyle)))
  (when value
    (output-hidden s slot value)
    (format s "<img src='~A'/>" value)))

(defmethod output-fastchange-cell (s slot value (style (eql 'menu)))
  (let (options)
    (setq options (findalternatives slot))
    (format s "<table cellpadding='0' cellspacing='0'>")
    (format s "<tr><td width='32' valign='top'>")
    (output-empty-button s)
    (output-empty-button s)
    (format s "</td><td>")
    (when options (output-oldmenu s slot options (list value)))
    (format s "</td></tr>") (crlf s)
    (format s "</table>")
    (crlf s)))

(defmethod output-fastchange-cell (s slot value (style (eql 'password)))
  (format-password s slot (prettyname value) 40))

(defmethod output-fastchange-cell (s slot value (style (eql 'prettystyle)))
  (when value
    (output-hidden s slot value)
    (output-simple s value)))

(defmethod output-fastchange-cell (s slot value (style (eql 'radiobutton)))
  (let (options)
    (when (setq options (findalternatives slot))
      (setq options (cons nil options))
      (output-oldradiobutton s slot options value))))

(defmethod output-fastchange-cell (s slot value (style (eql 'selector)))
  (output-fastselector s slot (cons 'unknown (findalternatives slot)) value))

(defmethod output-fastchange-cell (s slot value (style (eql 'stringfield)))
  (output-coldstringfield s slot value 40))

(defmethod output-fastchange-cell (s slot value (style (eql 'subframe)))
  (let (class)
    (setq class (find-range slot))
    (format s "<div slot='~A' id='~A' category='~A' style='border:groove; width:640'>"
            slot value class)
    (format s "<span>~A</span>" (prettyname value))
    (format s "<br/>")
    (output-fastchange-inner s (changeitem value class))
    (format s "</div>")))

(defmethod output-fastchange-cell (s slot value (style (eql 'tabulator)))
  (let (class slots results)
    (setq class (find-range slot))
    (setq slots (createable-slots class))
    (setq results (prorequest `(ask-table ,(list value) ,slots)))
    (format s "<div>")
    (output-fastupdate-inner s slot class (list value) slots results)
    (format s "</div>")))

(defmethod output-fastchange-cell (s slot value (style (eql 'text)))
  (output-coldstringfield s slot value 40))

(defmethod output-fastchange-cell (s slot value (style (eql 'textarea)))
  (format-textarea s slot (prettyname value) 4 80))

(defmethod output-fastchange-cell (s slot value (style (eql 'typein)))
  (output-coldtypein s slot value 40))

(defmethod output-fastchange-cell (s slot value (style (eql 'urlstyle)))
  (when value
    (output-hidden s slot value)
    (format s "<a href='~A'>~A</a>" value (htmlify value))))

(defmethod output-fastchange-cell (s slot value style)
  (declare (ignore style))
  (output-hidden s slot value))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod output-fastchange-deadcells (s slot values style)
  (format s "<table cellpadding='0' cellspacing='0'>")
  (format s "<tr><td width='32' valign='top'>")
  (output-empty-button s)
  (output-empty-button s)
  (format s "</td><td>")
  (output-fastchange-cell s slot (car values) style) (crlf s)
  (format s "</td></tr>")
  (dolist (value (cdr values))
    (format s "<tr><td width='32' valign='top'>")
    (output-empty-button s)
    (output-empty-button s)
    (format s "</td><td>")
    (output-fastchange-cell s slot value style) (crlf s)
    (format s "</td></tr>"))
  (format s "</table>")
  (crlf s))

(defun output-fastchange-livecells (s slot values style)
  (format s "<table cellpadding='0' cellspacing='0'>")

  (format s "<tr id='~A' qualifier='skip' style='display:None'>" (stringize slot))
  (format s "<td width='32' valign='top'>")
  (output-addslot s slot)
  (output-remslot s)
  (format s "</td><td>")
  (output-fastchange-cell s slot nil style)
  (format s "</td>")
  (format s "</tr>")

  (dolist (value values)
    (format s "<tr><td width='32' valign='top'>")
    (output-addslot s slot)
    (output-remslot s)
    (format s "</td><td>")
    (output-fastchange-cell s slot value style) (crlf s)
    (format s "</td></tr>"))

  (format s "<tr>")
  (format s "<td width='32'>")
  (output-addslot s slot)
  (output-empty-button s)
  (format s "</td>")
  (format s "<td></td>")
  (format s "</tr>")

  (format s "</table>")
  (crlf s))

(defun output-fastupdate-inner (s slot class items slots results)
  (let (nohandle)
    (setq nohandle (findp `(nodisplay ,class handle) *interface*))
    (format s "<div style='margin-left:36px; cursor:pointer; color:#666666' onClick='toggletable(this)'>Show</div>")
    (format s "<table style='display:none' cellspacing='2' bgcolor='~A' border='0'>" "#bbbbbb") (crlf s)
    (format s "<tr bgcolor='#eeeeee'>")
    (format s "<td>")
    ;(output-addrow s class)
    ;(output-hidetable s)
    ;(output-empty-button s)
    (format s "</td>")
    (unless nohandle
      (format s "<th>")
      (format s (iconify class))
      (format s "</th>"))
    (dolist (slot slots)
      (format s "<th>")
      (format s (iconify slot))
      (format s "</th>")
      (crlf s))
    (format s "</tr>") (crlf s)
    (do ((l items (cdr l)) (m results (cdr m)))
        ((null l))
        (format s "<tr slot='~A' id='~A' category='~A' bgcolor='~A'>"
                slot (car l) class *bgcolor*)
        (format s "<td>")
        (output-addrow s class)
        (output-remrow s)
        (format s "</td>")
        (unless nohandle
          (format s "<td>")
          (output-handle s (car l))
          (format s "</td>"))
        (do ((n (car m) (cdr n)) (slots slots (cdr slots)) (style)) 
            ((null n))
            (setq style (find-changestyle (car slots)))
            (format s "<td>")
            (cond ((and (null (cdar n)) (not (find-multivalued (car slots))))
                   (output-fastchange-cell s (car slots) (caar n) style))
                  (t (output-fastchange-cells s (car slots) (car n) style)))
            (format s "</td>"))
        (format s "</tr>")
        (crlf s))
    (output-fastupdate-original s nohandle slot class slots) (crlf s)
    (format s "<tr bgcolor='#eeeeee'>")
    (format s "<td width='32'>")
    (output-addrow s class)
    (output-empty-button s)
    (format s "</td>")
    (format s "<td colspan='4'></td>")
    (format s "</tr>") (crlf s)
    (format s "</table>") (crlf s)))

(defun output-fastupdate-original (s nohandle slot class slots)
  (format s "<tr slot='~A' id='~A' category='~A' qualifier='skip' style='display:None' bgcolor='~A'>"
          slot class class *bgcolor*)
  (format s "<td width='32'>")
  (output-addrow s class)
  (output-remrow s)
  (format s "</td>")
  (unless nohandle
    (format s "<td>")
    (output-handle s nil)
    (format s "</td>"))
  (do ((slots slots (cdr slots)) (style)) 
      ((null slots))
      (setq style (find-changestyle (car slots)))
      (format s "<td>")
      (cond ((not (find-multivalued (car slots)))
             (output-fastchange-cell s (car slots) nil style))
            (t (output-fastchange-cells s (car slots) nil style)))
      (format s "</td>"))
  (format s "</tr>"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun output-fastselector (s slot options value)
  (format s "<select name='~A' onClick='showchange(this)'>" (stringize slot))
  (dolist (option options)
    (cond ((equalp option value)
           (format s "<option value='~A' selected='true'>~A</option>"
                   (stringize option) (prettyname option)) (crlf s))
          (t (format s "<option value='~A'>~A</option>"
                     (stringize option) (prettyname option)) (crlf s))))
  (format s "</select>"))

(defun output-addslot (s x)
  (format s "<image src='~Ainfomaster/images/add.gif' border='0' onClick='addslot(\"~A\",this)'/>"
          *home* (stringize x)))

(defun output-remslot (s)
  (format s "<image src='~Ainfomaster/images/delete.gif' border='0' onClick='remslot(this)'/>"
          *home*))

(defun output-additem (s x)
  (format s "<image src='~Ainfomaster/images/add.gif' border='0' onClick='additem(\"~A\",this)'/>"
          *home* x))

(defun output-remitem (s)
  (format s "<image src='~Ainfomaster/images/delete.gif' border='0' onClick='remitem(this)'/>" *home*))

(defun output-hidetable (s)
  (format s "<image src='~Ainfomaster/images/magnifier.gif' border='0' onClick='hidetable(this)'/>"
          *home*))

(defun output-addrow (s class)
  (format s "<image src='~Ainfomaster/images/add.gif' border='0' onClick='addrow(\"~A\",this)'/>"
          *home* class))

(defun output-remrow (s)
  (format s "<image src='~Ainfomaster/images/delete.gif' border='0' onClick='remrow(this)'/>" *home*))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; fastcopypage
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod process (s (command (eql 'fastcopypage)) postlines)
  (let (object class)
    (cond ((null (cdr postlines)) (http-problem s "Bad request."))
          (t (setq object (read-user-string (cdr (pop postlines))))
             (setq class (classify object *gui*))
             (process-fastcopypage-copy s object class)))))

(defmethod process-fastcopypage-copy (s object class)
  (output-fastcreatepage s (createcopy object (newinstance class) class)))

(defun createcopy (object new class)
  (let (values dum)
    (dolist (slot (createable-slots class))
      (setq dum (request `(ask-all (,slot ?x) (,slot ,object ?x)) *client* *agent*))
      (if dum (setq values (nreconc dum values)) (setq values (cons (list slot) values))))
    (cons new (cons class (nreverse values)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; fastdeletepage
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod process (s (command (eql 'fastdeletepage)) postlines)
  (let (object class)
    (cond ((null (cdr postlines)) (http-problem s "Bad request."))
          (t (setq object (read-user-string (cdr (pop postlines))))
	     (setq class (classify object *gui*))
             (process-fastdeletepage-delete s object class)))))

(defmethod process-fastdeletepage-delete (s object class)
  (declare (ignore class))
  (let (facts result)
    (setq facts (facts object *gui*))
    (setq result (prorequest `(update ,(maksand (mapcar #'maknot facts)))))
    (cond ((errorp result) (output-problems s result))
          (t (html-message s "Object deleted.")))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Widgets
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; combobox
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun output-oldcombobox (s slot options value size)
  (when (eq value 'unknown) (setq value ""))
  (format s "<input type='text' name='~A' value='~A' size='~A' autocomplete='off'
                    onFocus='showMenu(this.nextSibling)' onClick='cancelBubble(event)'
                    onkeydown='return HandleKeyDown(event)'/>"
          (stringize slot) value size)
  (output-oldcombobox-div s options)
  (incf *buttons*)
  (format s "<iframe id='iframe~A' style='display:none;position:absolute;z-index:2;filter:mask();' frameborder='0'></iframe>"
          *buttons*) (crlf s))

(defun output-oldcombobox-div (s options)
  (format s "<div style='border:1px solid gray; background-color:#fff; position:absolute; overflow:auto; z-index:3; cursor:pointer; font-size:100%; display:none'>") (crlf s)
  (do ((l options (cdr l)) (i 0 (1+ i)))
      ((null l))
      (format s "<div onMouseOver='comboSelect(this)'
                      onClick='cbClick(this,\"~A\",event)'
                      style='padding:2px;'>~A</div>"
              (prettyname (car l)) (prettyname (car l)))
      (crlf s))
  (format s "</div>") (crlf s))

(defun output-newcombobox (s name options value size)
  (when (eq value 'unknown) (setq value ""))
  (format s "<input type='text' name='~A' value='~A' size='~A' autocomplete='off'
                    onFocus='showMenu(this.nextSibling)' onClick='cancelBubble(event)'
                    onchange='postReplace(form1)'
                    onkeydown='return HandleKeyDown(event)'/>"
          name value size)
  (output-newcombobox-div s options)
  (incf *buttons*)
  (format s "<iframe id='iframe~A' style='display:none;position:absolute;z-index:2;filter:mask();' frameborder='0'></iframe>"
          *buttons*) (crlf s))

(defun output-newcombobox-div (s options)
  (format s "<div style='border:1px solid gray; background-color:#fff; position:absolute; overflow:auto; z-index:3; cursor:pointer; font-size:100%; display:none'>") (crlf s)
  (do ((l options (cdr l)) (i 0 (1+ i)))
      ((null l))
      (format s "<div onMouseOver='comboSelect(this)'
                      onClick='cbClick(this,\"~A\",event); postReplace(form1); false'
                      style='padding:2px;'>~A</div>"
              (prettyname (car l)) (prettyname (car l)))
      (crlf s))
  (format s "</div>") (crlf s))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; checkbox
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun output-discheckbox (s slot options values)
  (format s "<table>")
  (dolist (option options)
    (format s "<tr><td>")
    (cond ((find option values :test #'equalp)
           (format s "<input type='checkbox' qualifier='skip' name='~A' value='~A' checked='true' disabled='true'/>"
                   (stringize slot) (stringize option)))
          (t (format s "<input type='checkbox' qualifier='skip' name='~A' value='~A' disabled='true'/>"
                     (stringize slot) (stringize option))))
    (format s "~A" (prettyname option))
    (format s "</td></tr>"))
  (format s "</table>"))

(defun output-coldcheckbox (s slot options values)
  (format s "<table>")
  (dolist (option options)
    (format s "<tr><td>")
    (cond ((find option values :test #'equalp)
           (format s "<input type='checkbox' name='~A' value='~A' checked='true'/>"
                   (stringize slot) (stringize option)))
          (t (format s "<input type='checkbox' name='~A' value='~A'/>"
                     (stringize slot) (stringize option))))
    (format s "~A" (prettyname option))
    (format s "</td></tr>"))
  (format s "</table>"))

(defun output-newcheckbox (s slot options values)
  (format s "<table>")
  (dolist (option options)
    (format s "<tr><td>")
    (cond ((find option values :test #'equalp)
           (format s "<input type='checkbox' name='~A' value='~A' checked='true'
                          onClick='postReplace(this.form)'/>"
                   (stringize slot) (stringize option)))
          (t (format s "<input type='checkbox' name='~A' value='~A'
                          onClick='postReplace(this.form)'/>"
                     (stringize slot) (stringize option))))
    (format s "~A" (prettyname option))
    (format s "</td></tr>"))
  (format s "</table>"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; dateinput
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun output-disdateinput (s name value)
  (when (or (null value) (eq value 'unknown)) (setq value ""))
  (format s "<input type='text' qualifier='skip' name='~A' value='~A' size='10' maxlength='10' disabled='true'/>"
          name value))

(defun output-olddateinput (s name value)
  (when (or (null value) (eq value 'unknown)) (setq value ""))
  (format s "<input type='text' name='~A' value='~A' size='10' maxlength='10' autocomplete='off'
                    onFocus='showCal(this.nextSibling,this.value)' onClick='cancelBubble(event)'
                    onkeydown='return HandleKeyDown(event)'/>"
          name value)
  (output-dateinput-div s "false")
  (incf *buttons*)
  (format s "<iframe id='iframe~A' style='display:none;position:absolute;z-index:2;filter:mask();' frameborder='0'></iframe>"
          *buttons*) (crlf s))

(defun output-newdateinput (s name value)
  (when (or (null value) (eq value 'unknown)) (setq value ""))
  (format s "<input type='text' name='~A' value='~A' size='10' maxlength='10' autocomplete='off'
                    onFocus='showCal(this.nextSibling,this.value)' onClick='cancelBubble(event)'
                    onChange='this.value=normalizedate(this.value); postReplace(form1)'
                    onkeydown='return HandleKeyDown(event)'/>"
          name value)
  (output-dateinput-div s "true")
  (incf *buttons*)
  (format s "<iframe id='iframe~A' style='display:none;position:absolute;z-index:2;filter:mask();' frameborder='0'></iframe>"
          *buttons*) (crlf s))

(defun output-dateinput-div (s flag)
  (format s "<div style='background-color:#FFF; position:absolute; overflow:auto; z-index:3; cursor:pointer; font-size:100%; display:none; -moz-user-select:none; -khtml-user-select:none; user-select:none;'
                  onselectstart='return false;'>")
  (format s "<input type='hidden' qualifier='skip'/>")
  (format s "<input type='hidden' qualifier='skip'/>")
  (format s "<input type='hidden' qualifier='skip'/>")
  (format s "<input type='hidden' qualifier='skip'/>")
  (format s "<table style='margin-top:7px;background:#fff;text-align:center;font-family:Verdana;background:#fff;border-bottom:1px #A2BBDD solid;font-size:70%;cursor:pointer' cellpadding='4' cellspacing='0'>")
  (format s "<tr style='cursor:default;background:#c3d9ff;color:#112ABB;font-weight:bold;vertical-align:middle'>")
  (format s "<td style='padding:2px;font-size:125%; padding-bottom:6px;text-align:right;cursor:pointer' onclick='prevMonth(this,event)'>&laquo;</td>")
  (format s "<td style='padding:2px;font:bold 100% Verdana,Sans-serif;padding-bottom:4px' colspan='5'><span></span><span></span></td>") (crlf s)
  (format s "<td style='padding:2px;font-size:125%; padding-bottom:6px;text-align:left;cursor:pointer' onclick='nextMonth(this,event)'>&raquo;</td>") (crlf s)
  (format s "</tr>") (crlf s)
  (format s "<tr style='background:#c3d9ff;cursor:default'>") (crlf s)
  (format s "<td>S</td><td>M</td><td>T</td><td>W</td><td>T</td><td>F</td><td>S</td>") (crlf s)
  (format s "</tr>") (crlf s)
  (format s "<tr>") (crlf s)
  (format s "<td onclick='calClick(this.id,event,~A)'></td>" flag) (crlf s)
  (dotimes (i 5)
    (format s "<td onclick='calClick(this.id,event,~A)'></td>" flag)  (crlf s))
  (format s "<td onclick='calClick(this.id,event,~A)'></td>" flag)  (crlf s)
  (format s "</tr>")  (crlf s)
  (dotimes (j 6)
    (format s "<tr>")  (crlf s)
    (format s "<td onclick='calClick(this.id,event,~A)'></td>" flag)  (crlf s)
    (dotimes (i 5)
      (format s "<td onclick='calClick(this.id,event,~A)'></td>" flag)  (crlf s))
    (format s "<td onclick='calClick(this.id,event,~A)'></td>" flag)  (crlf s)
    (format s "</tr>")  (crlf s))
  (format s "</table>")  (crlf s)
  (format s "</div>")  (crlf s))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; dollarinput
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun output-disdollarinput (s name value size)
  (format s "$<input type='text' qualifier='skip' name='~A' value='~A' size='~A' maxlength='~A' disabled='true'/>"
          name (cond ((integerp value) (stringize value))
		     ((realp value) (format nil "~$" value))
		     ((stringp value) value)
		     (t ""))
          size size))

(defun output-olddollarinput (s name value size)
  (format s "$<input type='text' name='~A' value='~A' size='~A' maxlength='~A'
                     onKeyPress='return oldCheckNumber(event,this.value)'/>"
          name (cond ((integerp value) (stringize value))
		     ((realp value) (format nil "~$" value))
		     ((stringp value) value)
		     (t ""))
          size size))

(defun output-newdollarinput (s name value size)
  (format s "$<input type='text' name='~A' value='~A' size='~A' maxlength='~A'
                     onKeyPress='return newCheckNumber(event,this.value)'
                     onBlur='postReplace(this.form)'/>"
          name (cond ((integerp value) (stringize value))
		     ((realp value) (format nil "~$" value))
		     ((stringp value) value)
		     (t ""))
          size size))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; multiselector
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun output-multiselector (s slot values structure)
  (let (options)
    (format s "<table cellpadding='0' cellspacing='0'>")
    (dolist (value values)
      (setq options (slowoptions slot value structure))
      (setq options (pruneoptions options value values))
      (format s "<tr><td>")
      (output-newselector s slot (cons 'unknown options) value)
      (format s "</td></tr>"))
    (cond ((null values)
           (format s "<tr><td>")
           (if (setq options (difference* (sortoptions slot structure) values))
	       (output-newselector s slot (cons 'unknown options) nil)
               (output-disselector s "None"))
           (format s "</td></tr>"))
          ((find-multivalued slot)
           (when (setq options (difference* (sortoptions slot structure) values))
             (format s "<tr><td>")
             (output-newselector s slot (cons 'unknown options) nil))
             (format s "</td></tr>")))
    (format s "</table>") (crlf s)
    'done))

(defun output-newmultiselector (s slot values structure)
  (let (options)
    (format s "<table cellpadding='0' cellspacing='0'>")
    (dolist (value values)
      (format s "<tr><td>")
      (output-newselector s slot (list 'unknown value) value)
      (format s "</td></tr>"))
    (cond ((null values)
           (format s "<tr><td>")
           (if (setq options (difference* (sortoptions slot structure) values))
	       (output-newselector s slot (cons 'unknown options) nil)
               (output-disselector s "None"))
           (format s "</td></tr>"))
          ((find-multivalued slot)
           (when (setq options (difference* (sortoptions slot structure) values))
             (format s "<tr><td>")
             (output-newselector s slot (cons 'unknown options) nil))
             (format s "</td></tr>")))
    (format s "</table>") (crlf s)
    'done))

(defun sortoptions (slot structure)
  (sort (findoptions slot structure *gui*) #'lessp))

(defun pruneoptions (options value values)
  (do ((l options (cdr l)) (nl))
      ((null l) (nreverse nl))
      (when (or (equalp (car l) value) (not (find (car l) values :test #'equalp)))
        (setq nl (cons (car l) nl)))))

(defmethod fastoptions (slot structure)
  (cond ((results 'option slot *interface*))
        (t (setq structure (screen slot (viewconvert structure)))
	   (sort (findoptions slot structure *gui*) #'lessp))))

(defun screen (slot structure)
  (do ((l (cddr structure) (cdr l)) (ol))
      ((null l) (list* (car structure) (cadr structure) (nreverse ol)))
      (unless (eq (caar l) slot) (setq ol (cons (car l) ol)))))

(defun slowoptions (slot value structure)
  (cond ((results 'option slot *interface*))
        (t (setq structure (slowscreen slot value (viewconvert structure)))
	   (sort (findoptions slot structure *gui*) #'lessp))))

(defun slowscreen (slot value structure)
  (do ((l (cddr structure) (cdr l)) (ol))
      ((null l) (list* (car structure) (cadr structure) (nreverse ol)))
      (unless (and (eq (caar l) slot) (equalp (cadar l) value))
	 (setq ol (cons (car l) ol)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; fastselector
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod process (s (command (eql 'fastselect)) postlines)
  (let (slot value)
    (setq slot (read-user-string (cdr (pop postlines))))
    (setq value (read-user-string (cdr (pop postlines))))
    (output-fastselector-select s slot value)))

(defun output-oldfastselector (s slot value)
  (let (context options)
    (cond ((or (null value) (eq value 'unknown))
           (format-fastselector-select s slot)
           (output-fastselector-options s slot nil (findalternatives slot))
           (finish-fastselector-select s))
          (t (setq context (nreverse (cons value (nreverse (findcontext slot value)))))
             (setq options (findcomponents slot context value))
             (format-fastselector-select s slot)
             (output-fastselector-options s slot context options)
             (finish-fastselector-select s)))))

(defun output-newfastselector (s slot value)
  (let (context options)
    (cond ((or (null value) (eq value 'unknown))
           (format-fastselector-select s slot)
           (output-fastselector-options s slot nil (findalternatives slot))
           (finish-fastselector-select s))
          (t (setq context (nreverse (cons value (nreverse (findcontext slot value)))))
             (setq options (findcomponents slot context value))
             (format-fastselector-select s slot)
             (output-fastselector-options s slot context options)
             (finish-fastselector-select s)))))

(defun output-weirdfastselector (s slot value structure)
  (let (context options)
    (cond ((or (null value) (eq value 'unknown))
           (format-fastselector-select s slot)
           (output-fastselector-options s slot nil (findalternatives slot))
           (finish-fastselector-select s))
          (t (setq context (nreverse (cons value (nreverse (findcontext slot value)))))
             (setq options (findcomponentoptions slot context structure))
             (format-fastselector-select s slot)
             (output-fastselector-options s slot context options)
             (finish-fastselector-select s)))))

(defun output-fastselector-select (s slot value)
  (let (context options)
    (cond ((or (null value) (equalp value 'unknown))
           (output-fastselector-options s slot nil (findalternatives slot)))
          (t (setq context (nreverse (cons value (nreverse (findcontext slot value)))))
             (setq options (find-components slot value context))
             (output-fastselector-options s slot context options)))))

(defun format-fastselector-select (s slot)
  (format s "<select id='~A' name='~A'
                     onChange='fastselect(\"~A\",this.options[this.selectedIndex].value)'>"
          (stringize slot) (stringize slot) (stringize slot)) (crlf s))

(defun output-fastselector-options (s slot context options)
  (format s "<option value=' '>~A</option>" (stringappend "Any " (iconify slot))) (crlf s)
  (do ((l context (cdr l)))
      ((null (cdr l))
       (when l (format s "<option value='~A' selected>~A</option>"
                       (car l) (prettyname (car l))) (crlf s)))
      (format s "<option value='~A'>~A</option>"
              (stringize (car l)) (prettyname (car l)))
      (crlf s))
  (format s "<option value=' '></option>") (crlf s)
  (dolist (option options)
    (format s "<option value='~A'>~A</option>"
            (stringize option) (prettyname option))
    (crlf s)))

(defun finish-fastselector-select (s)
  (format s "</select>"))

(defun findcontext (slot value)
  (do ((l (nreverse (results 'expander slot *interface*)) (cdr l)) (dum) (nl))
      ((null l) nl)
      (when (setq dum (result (car l) value *gui*))
        (setq nl (cons dum nl) value dum))))

(defun findcomponents (slot context value)
  (do ((l context (cdr l)) (m (results 'expander slot *interface*) (cdr m)))
      ((null l))
      (when (and (equalp (car l) value) m)
        (return (sort (objects (car m) value *gui*) #'lessp)))))

(defun findcomponentoptions (slot context structure)
  (let (aporels)
    (setq aporels (results 'expander slot *interface*))
    (do ((l context (cdr l)))
        ((null (cdr l)))
        (setq aporels (cdr aporels)))
    (when aporels
      (do ((l (findinstances (viewconvert structure) *gui*) (cdr l)) (dum) (nl))
          ((null l) (sort (nreverse (uniquify nl)) #'lessp))
          (when (setq dum (transresult slot (car l) (cdr aporels) *gui*))
            (setq nl (cons dum nl)))))))

(defun transresult (slot object aporels source)
  (let (value)
    (setq value (result slot object source))
    (do ((l aporels (cdr l)))
        ((null l) value)
        (setq value (result (car l) value source)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; fancyselector
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod process (s (command (eql 'fancyselect)) postlines)
  (let (context slot old new options)
    (setq slot (read-user-string (cdr (pop postlines))))
    (setq old (read-user-string (cdr (pop postlines))))
    (setq context (read-user-string (cdr (pop postlines))))
    (setq new (read-user-string (cdr (pop postlines))))
    (setq context (nreverse (cons new (nreverse context))))
    (setq options (find-components slot new context))
    (cond ((setq options (find-components slot new context))
           (output-fancyselector-options s slot old context options))
          (t (format s " ")))))

(defmethod process (s (command (eql 'fancydeselect)) postlines)
  (let (context slot old new options)
    (setq slot (read-user-string (cdr (pop postlines))))
    (setq old (read-user-string (cdr (pop postlines))))
    (setq context (read-user-string (cdr (pop postlines))))
    (setq new (read-user-string (cdr (pop postlines))))
    (do ((l context (cdr l)))
        ((null l))
        (when (eq (car l) new) (rplacd l nil) (return t)))
    (cond ((setq options (find-components slot new context))
           (output-fancyselector-options s slot old context options))
          (t (format s " ")))))

(defun output-oldfancyselector (s slot value)
  (format s "<input type='text' name='~A' value='~A' disabled='true'/>"
          (stringize slot) (prettyname value))
  (format s "<span style='color:#ff0000; cursor:pointer' onClick='fancyselect(\"~A\",\"~A\",\"()\",\"~A\")'>Select</span>"
          (stringize slot) (stringize value) (stringize (result 'option slot *manager*)))
  (format s "<div id='~A'></div>" (stringize slot)))

(defun output-newfancyselector (s slot value)
  (format s "<input type='text' name='~A' value='~A' disabled='true'
                    onChange='postReplace(this.form)'/>"
          (stringize slot) (prettyname value))
  (format s "<span style='color:#ff0000; cursor:pointer' onClick='fancyselect(\"~A\",\"~A\",\"()\",\"~A\")'>Select</span>"
          (stringize slot) (stringize value) (stringize (result 'option slot *manager*)))
  (format s "<div id='~A'></div>" (stringize slot)))

(defun output-fancyselector-options (s slot old context options)
  (format s "<table><tr><td><center>")
  (format s "<select name='Up' qualifier='skip' onChange='fancydeselect(\"~A\",\"~A\",\"~A\",this.options[this.selectedIndex].value)'>"
          (stringize slot) (stringize old) (stringize context))
  (format s "<option value=''></option>")
  (do ((l context (cdr l)))
      ((null (cdr l))
       (format s "<option value='~A' selected>~A</option>" (car l) (prettyname (car l)))
       (crlf s))
      (format s "<option value='~A'>~A</option>" (car l) (prettyname (car l)))
      (crlf s))
  (format s "</select>")
  (format s "<br/>")
  (when options
    (format s "<select name='Down' qualifier='skip' multiple onChange='fancyselect(\"~A\",\"~A\",\"~A\",this.options[this.selectedIndex].value)'>"
            (stringize slot) (stringize old) (stringize context))
    (dolist (option options)
      (format s "<option value='~A'>~A</option>"
              (stringize option) (prettyname option))
      (crlf s))
    (format s "</select>")
    (format s "<br/>")
    (format s "<input type='button' value='Cancel' onClick='fancycancel(\"~A\",\"~A\")'/>"
            (stringize slot) (stringize old))
    (format s "</center></td></tr></table>")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; hidden
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun output-hidden (s slot value)
  (format s "<input type='hidden' name='~A' value='~A'/>"
	  (stringize slot) (stringize value)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; menu
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun output-dismenu (s slot options values)
  (format s "<select name='~A' qualifier='skip' disabled='true' multiple='true' size='~A'>"
          (stringize slot) (menu-size (length options)))
    (dolist (option options)
      (cond ((find option values :test #'equalp)
             (format s "<option value='~A' selected='true'>~A</option>"
                     (stringize option) (prettyname option)) (crlf s))
            (t (format s "<option value='~A'>~A</option>"
                       (stringize option) (prettyname option)) (crlf s))))
  (format s "</select>"))

(defun output-oldmenu (s slot options values)
  (format-hidden s (stringize slot) "")
  (when options
    (format s "<select name='~A' multiple='true' size='~D'>"
            (stringize slot) (menu-size (length options)))
    (dolist (option options)
      (cond ((find option values :test #'equalp)
             (format s "<option value='~A' selected='true'>~A</option>"
                     (stringize option) (prettyname option)) (crlf s))
            (t (format s "<option value='~A'>~A</option>"
                       (stringize option) (prettyname option)) (crlf s))))
    (format s "</select>")))

(defun output-newmenu (s slot options values)
  (format-hidden s (stringize slot) "")
  (when options
    (format s "<select name='~A' multiple='true' size='~D'
                       onChange='postReplace(this.form)'>"
            (stringize slot) (menu-size (length options)))
    (dolist (option options)
      (cond ((find option values :test #'equalp)
             (format s "<option value='~A' selected='true'>~A</option>"
                     (stringize option) (prettyname option)) (crlf s))
            (t (format s "<option value='~A'>~A</option>"
                       (stringize option) (prettyname option)) (crlf s))))
    (format s "</select>")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; password
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun format-dispassword (s value size)
  (format s "<input type='password' value='~A' size='~A' qualifier='skip' disabled='true'/>"
          value size))

(defun output-coldpassword (s slot value size)
  (format s "<input type='password' name='~A' value='~A' size='~A' qualifier='string'/>"
          (stringize slot) (htmlify value) size))

(defun output-warmpassword (s slot value size)
  (format s "<input type='password' name='~A' value='~A' size='~A' qualifier='string'/>"
          (stringize slot) (htmlify value) size))

(defun format-newpassword (s name value size)
  (format s "<input type='password' name='~A' value='~A' size='~A' qualifier='string'
                    onKeyPress='return textEdit(event)'
                    onBlur='textBlur(event)'/>"
          name value size))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; radiobutton
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun output-disradiobutton (s options value)
  (format s "<table>")
  (dolist (option options)
    (format s "<tr><td>")
    (if (equalp option value)
        (format s "<input type='radio' qualifier='skip' disabled='true' checked='true'/>")
        (format s "<input type='radio' qualifier='skip' disabled='true'/>"))
    (format s "~A" (prettyname option))
    (format s "</td></tr>"))
  (format s "</table>"))

(defun output-oldradiobutton (s slot options value)
  (format s "<table>")
  (dolist (option options)
    (format s "<tr><td>")
    (if (equalp option value)
        (format s "<input type='radio' name='~A' value='~A' checked='true'/>"
                (stringize slot) (stringize option))
        (format s "<input type='radio' name='~A' value='~A'/>"
                (stringize slot) (stringize option)))
    (format s "~A" (prettyname option))
    (format s "</td></tr>"))
  (format s "</table>"))

(defun output-newradiobutton (s slot options value)
  (format s "<table>")
  (dolist (option options)
    (format s "<tr><td>")
    (if (equalp option value)
        (format s "<input type='radio' name='~A' value='~A' checked='true'
                          onClick='postReplace(this.form)'/>"
                (stringize slot) (stringize option))
        (format s "<input type='radio' name='~A' value='~A'
                          onClick='postReplace(this.form)'/>"
                (stringize slot) (stringize option)))
    (format s "~A" (prettyname option))
    (format s "</td></tr>"))
  (format s "</table>"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; selector
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun output-disselector (s value)
  (format s "<select qualifier='skip' disabled='true'>")
  (format s "<option selected='true'>~A</option>" (prettyname value))
  (format s "</select>"))

(defun output-coldselector (s slot options value)
  (format s "<select name='~A'>" (stringize slot))
  (dolist (option options)
    (cond ((equalp option value)
           (format s "<option value='~A' selected='true'>~A</option>"
                   (stringize option) (prettyname option)) (crlf s))
          (t (format s "<option value='~A'>~A</option>"
                     (stringize option) (prettyname option)) (crlf s))))
  (format s "</select>"))

(defun output-warmselector (s slot options value)
  (format s "<select name='~A' onChange='adjust(\"~A\",this)'>"
	  (stringize slot) (stringize slot))
  (dolist (option options)
    (cond ((equalp option value)
           (format s "<option value='~A' selected='true'>~A</option>"
                   (stringize option) (prettyname option)) (crlf s))
          (t (format s "<option value='~A'>~A</option>"
                     (stringize option) (prettyname option)) (crlf s))))
  (format s "</select>"))

(defun output-newselector (s slot options value)
  (format s "<select name='~A' onChange='postReplace(this.form)'>"
          (stringize slot))
  (dolist (option options)
    (cond ((equalp option value)
           (format s "<option value='~A' selected='true'>~A</option>"
                   (stringize option) (prettyname option)) (crlf s))
          (t (format s "<option value='~A'>~A</option>"
                     (stringize option) (prettyname option)) (crlf s))))
  (format s "</select>"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; stringfield
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun output-coldstringfield (s slot value size)
  (format s "<input type='text' name='~A' value='~A' size='~A' qualifier='string'/>"
          (stringize slot) (prettify value) size))

(defun output-warmstringfield (s slot value size)
  (format s "<input type='text' name='~A' value='~A' size='~A' qualifier='string'
	            onChange='adjust(\"~A\",this)'/>"
          (stringize slot) (prettify value) size (stringize slot)))

(defun format-newstringfield (s name value size)
  (format s "<input type='text' qualifier='string' name='~A'value='~A' size='~A'
                    onKeyPress='return textEdit(event)'
                    onBlur='textBlur(event)'/>"
          name value size))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; typein
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun format-distypein (s value size)
  (format s "<input type='text' value='~A' size='~A' qualifier='skip' disabled='true'/>"
          value size))

(defun output-coldtypein (s slot value size)
  (format s "<input type='text' name='~A' value='~A' size='~A'/>"
          (stringize slot) (stringize value) size))

(defun output-warmtypein (s slot value size)
  (format s "<input type='text' name='~A' value='~A' size='~A'
	            onChange='adjust(\"~A\",this)'/>"
          (stringize slot) (stringize value) size (stringize slot)))

(defun format-newtypein (s name value size)
  (format s "<input type='text' name='~A' value='~A' size='~A'
                    onKeyPress='return textEdit(event)'
                    onBlur='textBlur(event)'/>"
          name value size))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Miscellaneous Subroutines
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defparameter *ticker* 0)

(defun fastsearchstructure (class)
  (do ((l (queryable-slots class) (cdr l)) (nl))
      ((null l) (list* '? class (nreverse nl)))
      (setq nl (cons (list (car l)) nl))))

(defun fastcomparestructure (class)
  (do ((l (displayable-slots class) (cdr l)) (nl))
      ((null l) (list* '? class (nreverse nl)))
      (setq nl (cons (list (car l)) nl))))

(defmethod inspectitem (object class)
  (let (values dum)
    (dolist (slot (inspectable-slots class))
      (setq dum (request `(ask-all (,slot ?x) (,slot ,object ?x)) *client* *agent*))
      (if dum (setq values (nreconc dum values)) (setq values (cons (list slot) values))))
    (if class (cons object (cons class (nreverse values))) object)))

(defun createitem (object class)
  (let (values dum)
    (dolist (slot (createable-slots class))
      (setq dum (find-createdefault slot))
      (if dum (setq values (cons (list slot dum) values))
          (setq values (cons (list slot) values))))
    (if class (cons object (cons class (nreverse values))) object)))

(defun changeitem (object class)
  (let (values dum)
    (dolist (slot (modifiable-slots class))
      (setq dum (request `(ask-all (,slot ?x) (,slot ,object ?x)) *client* *agent*))
      (if dum (setq values (nreconc dum values)) (setq values (cons (list slot) values))))
    (if class (cons object (cons class (nreverse values))) object)))

(defun fastquerystructure (structure)
  (do ((l (queryable-slots (cadr structure)) (cdr l)) (dum) (nl))
      ((null l) (list* (car structure) (cadr structure) (nreverse nl)))
      (cond ((setq dum (assoc (car l) (cddr structure)))
             (setq nl (cons dum nl)))
            (t (setq nl (cons (list (car l)) nl))))))

(defun fastcompletestructure (structure)
  (do ((l (displayable-slots (cadr structure)) (cdr l)) (dum) (nl))
      ((null l) (list* (car structure) (cadr structure) (nreverse nl)))
      (cond ((setq dum (assoc (car l) (cddr structure)))
             (setq nl (cons dum nl)))
            (t (setq nl (cons (list (car l)) nl))))))

(defmethod newinstance (class)
  (setq *ticker* (rem (1+ *ticker*) 10))
  (intern (strappend (symbol-name class) "."
                     (princ-to-string (get-universal-time))
                     (princ-to-string *ticker*))))

(defun viewconvert (structure)
  (do ((l (cddr structure) (cdr l)) (nl))
      ((null l) (list* (car structure) (cadr structure) (nreverse nl)))
      (cond ((null (cdar l)) (setq nl (cons (car l) nl)))
            ((triplep 'searchstyle (caar l) 'fastselector *manager*)
             (setq nl (cons (list (caar l) (viewconvertfastselector (caar l) (cadar l))) nl)))
            ((triplep 'searchstyle (caar l) 'fancyselector *manager*)
             (setq nl (cons (list (caar l) (viewconvertfastselector (caar l) (cadar l))) nl)))
            ((triplep 'searchstyle (caar l) 'stringfield *manager*)
             (setq nl (cons (list (caar l) `(?* string (substring ,(cadar l)))) nl)))
            ((triplep 'searchstyle (caar l) 'text *manager*)
             (setq nl (cons (list (caar l) `(?* string (substring ,(cadar l)))) nl)))
            (t (setq nl (cons (car l) nl))))))

(defun viewconvertfastselector (slot value)
  (do ((l (results 'expander slot *interface*) (cdr l)) (ns value))
      ((null l) ns)
      (when (object (car l) value *gui*)
        (dolist (expander l) (setq ns `(?* thing ,(list expander ns)))))))

(defun collectentries (slot items)
  (do ((l items (cdr l)) (nl))
      ((null l) (values (nreverse nl) nil))
      (if (eq (caar l) slot) (setq nl (cons (cadar l) nl))
          (return (values (nreverse nl) l)))))

(defun collectvalues (slot items)
  (do ((l items (cdr l)) (nl))
      ((null l) (values (nreverse nl) nil))
      (cond ((not (eq (caar l) slot)) (return (values (nreverse nl) l)))
            ((cdar l) (setq nl (cons (cadar l) nl))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun format-javascript (s)
  (format s "<script type='text/javascript' src='~Ainfomaster/javascript/proforma.js'></script>"
	  *home*))

(defun format-border (s)
  (format s "<div style='margin-left:10px; margin-right:10px; margin-top:10px; margin-bottom:10px' onClick='hidePopup()'>"))

(defun finish-border (s)
  (format s "</div>"))

(defun output-another-button (s buttons multivalued)
  (if multivalued (output-snow-button s buttons)
      (output-empty-button s)))

(defun output-removal-button (s buttons partial)
  (if partial (output-trash-button s buttons)
      (output-empty-button s)))

(defun output-snow-button (s x)
  (format s "<input type='image' src='~Ainfomaster/images/new.gif' name='~A' border='0'/>"
	  *home* (stringize x)))

(defun output-trash-button (s x)
  (format s "<input type='image' src='~Ainfomaster/images/smalltrash.gif' name='~A' border='0'/>"
	  *home* (stringize x)))

(defun output-empty-button (s)
  (format s "<span style='height:16px; width:16px'>&nbsp;</span>"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun sortem (objects sorter order)
  (let (values)
    (cond ((eq sorter 'handle)
           (sort objects (if (eq order 'ascending) #'lessp #'greaterp)))
          (t (setq values (request `(ask-all (?x ?y) (and (oneof ?x . ,objects)
                                                          ,(list sorter '?x '?y)))
                                   *client* *agent*))
             (setq order (if (eq order 'ascending) #'lessp #'greaterp))
             (mapcar #'car (sort (extracts objects values) order :key #'cadr))))))

(defun extracts (items answers)
  (setq answers (cons nil answers))
  (do ((l items (cdr l)) (nl))
      ((null l) (nreverse nl))
      (do ((m answers) (nm))
          ((null (cdr m)) (setq nl (cons (cons (car l) (nreverse nm)) nl)))
          (cond ((eql (car l) (caadr m))
                 (setq nm (cons (cadadr m) nm))
                 (rplacd m (cddr m)))
                (t (setq m (cdr m)))))))

(defun trim (results start end)
  (let (count)
    (setq count (length results))
    (cond ((not (integerp start)) (setq start 1))
          ((< start 1) (setq start 1))
          ((> start count) (setq start 1)))
    (cond ((not (integerp end)) (setq end count))
          ((< end 1) (setq end (min count *count*)))
          ((> end count) (setq end count)))
    (when (< end start) (setq end start))
    (cond ((and (= start 1) (= end count)))
          ((> start count) (setq results nil))
          (t (setq results (subseq results (1- start) end))))
    (values results count start end)))

(defun kerchunk (count start end)
  (cond ((and (= start 1) (= end count)))
        ((> count end) (setq start (1+ end) end (min count (+ end *count*))))
        (t (setq start 1 end (min count *count*))))
  (values start end))

(defun keechunk (start end)
  (cond ((= start 1) (values start end))
        (t (values (- start 20) (- start 1)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun differentiator (x)
  (nreverse (difftree x nil)))

(defun difftree (x nl)
  (let (fact)
    (cond ((atom x) nl)
          ((prorequest `(ask-if ,(setq fact (makpred (car x) (cadr x) *gui*))))
           (diffslots x nl))
          (t (diffslots x (adjoiner fact nl))))))

(defun diffslots (x nl)
  (do ((l (cddr x)) (slot) (vals))
      ((null l))
      (setq slot (caar l))
      (multiple-value-setq (vals l) (diffcollect slot l))
      (setq nl (diffslot slot (car x) vals nl)))
  (do ((l (cddr x) (cdr l)))
      ((null l) nl)
      (cond ((null (cdar l)))
            ((atom (cadar l)))
            ((eq (caadar l) 'oneof))
            ((eq (caadar l) 'taxonomy))
            ((eq (caadar l) 'between))
            (t (setq nl (difftree (cadar l) nl))))))

(defun diffcollect (slot slots)
  (do ((l slots (cdr l)) (nl))
      ((or (null l) (not (eq (caar l) slot))) (values (nreverse nl) l))
      (cond ((null (cdar l)))
            ((atom (cadar l)) (setq nl (cons (cadar l) nl)))
            ((eq (caadar l) 'oneof))
            ((eq (caadar l) 'taxonomy) (setq nl (cons (car (last (cadar l))) nl)))
            ((eq (caadar l) 'between))
            (t (setq nl (cons (caadar l) nl))))))

(defun diffslot (slot object vals nl)
  (do ((l (prorequest `(ask-all ?y ,(list slot object '?y))) (cdr l)))
      ((null l))
      (cond ((equal (car l) (car vals)) (setq vals (cdr vals)))
            (t (setq nl (adjoiner `(not ,(list slot object (car l))) nl)))))
  (do ((l vals (cdr l)))
      ((null l) nl)
      (setq nl (adjoiner (list slot object (car l)) nl))))

(defun errorp (x)
  (cond ((atom x) (stringp x))
        (t (some #'stringp x))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun find-sorter (class)
  (result 'sorter class *interface*))

(defun find-multivalued (slot)
  (findp `(unique ,slot no) *interface*))

(defun find-partial (slot)
  (findp `(total ,slot no) *interface*))

(defun find-createstyle (slot)
  (result 'createstyle slot *interface*))

(defun find-changestyle (slot)
  (result 'changestyle slot *interface*))

(defun find-searchstyle (slot)
  (result 'searchstyle slot *interface*))

(defun find-comparestyle (slot)
  (result 'comparestyle slot *interface*))

(defun find-inspectstyle (slot)
  (result 'inspectstyle slot *interface*))

(defun find-createlabel (slot)
  (or (result 'createlabel slot *interface*) ""))

(defun find-changelabel (slot)
  (or (result 'changelabel slot *interface*) ""))

(defun find-searchlabel (slot)
  (or (result 'searchlabel slot *interface*) ""))

(defun find-comparelabel (slot)
  (or (result 'comparelabel slot *interface*) ""))

(defun find-inspectlabel (slot)
  (or (result 'inspectlabel slot *interface*) ""))

(defun changeablep (object class gui)
  (and (not (findp `(nocommand ,gui change) *interface*))
       (not (findp `(nochange ,gui ,class) *interface*))
       object))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
