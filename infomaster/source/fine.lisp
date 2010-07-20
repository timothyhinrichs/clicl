;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; (c) Copyright 1999-2008 by Michael Genesereth.  All rights reserved.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; proforma.lisp
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(eval-when (compile load eval)
  (proclaim '(special *home* *imageprefix* *interface* *bgcolor* *border*)))

(defparameter *buttons* 0)
(defparameter *count* 20)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Performatives
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; finefieldpage
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod process (s (command (eql 'finefieldpage)) postlines)
  (cond ((null postlines) (process-rootclasspage s))
        (t (process-finefieldpage s (read-value-string (cdar postlines))))))

(defun process-rootclasspage (s)
  (format-html s) (crlf s)
  (output-head s "Finefieldpage") (crlf s)
  (format-body s *bgcolor*) (crlf s)
  (output-header s)
  (format-border s)
  (output-classes s 'thing (find-subclasses 'thing))
  (finish-border s)
  (output-footer s)
  (finish-body s) (crlf s)
  (finish-html s) (crlf s)
  'done)

(defun process-finefieldpage (s class)
  (format-html s) (crlf s)
  (output-head s "Finefieldpage") (crlf s)
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
                   (format s "<a href='finelookpage?class=~A'>~A</a><br/>"
                           (car classes) (pluralize (car classes)))
                   (setq classes (cdr classes)))
               (format s "</div>")
               (format s "</td>")))
          (t (format s "<td width='25%' valign='top'>")
             (format s "<span style='font-size:16px; font-weight:bold; color:#004488'>~A</span><br/>"
                              (pluralize class))
             (format s "<div style='margin-left: 15px'>")
             (dolist (class classes)
               (format s "<a href='finelookpage?class=~A'>~A</a><br/>"
                       class (pluralize class)))
             (format s "</div>")
             (format s "</td>")))
    (format s "</tr>
</table>
</center>
<br/>")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; finelookpage
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod process (s (command (eql 'finelookpage)) postlines)
  (let (dum start end)
    (cond ((setq dum (getf-post "Class" postlines))
           (setq dum (fastcomparestructure (read-user-string dum)))
           (setq start (or (read-value-string (getf-post "Start" postlines)) 1))
           (setq end (or (read-value-string (getf-post "End" postlines)) *count*))
           (process-finelookpage s dum start end))
          ((setq dum (getf-post "Structure" postlines))
           (setq start (or (read-value-string (getf-post "Start" postlines)) 1))
           (setq end (or (read-value-string (getf-post "End" postlines)) *count*))
           (process-finelookpage s (read-user-string dum) start end))
          (t (http-problem s "Bad request.")))))

(defun process-finelookpage (s structure start end)
  (format-html s) (crlf s)
  (format-head s)
  (format s "<title>Finelookpage</title>")
  (finish-head s) (crlf s)
  (format-body s *bgcolor*) (crlf s)
  (output-header s)
  (format-border s)
  (process-finelook s structure start end)
  (finish-border s)
  (output-footer s)
  (finish-body s) (crlf s)
  (finish-html s) (crlf s))

(defun process-finelook (s structure start end)
  (let (objects sorter attributes results count)
    (setq objects (findinstances (viewconvert structure) *gui*))
    (when (setq sorter (find-sorter (cadr structure)))
      (setq objects (sortem objects sorter 'ascending)))
    (multiple-value-setq (objects count start end) (trim objects start end))
    (setq attributes (displayable-slots (cadr structure)))
    (setq results (prorequest `(ask-table ,objects ,attributes)))
    (output-finelook s structure objects attributes results count start end)))

(defun output-finelook (s structure objects slots results count start end)
  (format s "<center>")
  (format s "<br/>")
  (cond ((= count 0)
         (format s "<table>")
         (format s "<tr><td align='center'>There are no answers.</td></tr>")
         (format s "<tr><td>")
         (output-finelook-create s (cadr structure) structure)
         (format s "</td></tr></table>"))
        ((= count 1)
         (format s "<table>")
         (format s "<tr><td align='center'>There is 1 answer.</td></tr>")
         (format s "<tr><td>")
         (output-finelook-inner s structure objects slots results)
         (format s "</td></tr>")
         (format s "<tr><td>")
         (output-finelook-create s (cadr structure) structure)
         (format s "</td></tr></table>"))
        ((and (= start 1) (geqp end count))
         (format s "<table>")
         (format s "<tr><td align='center'>There are ~D answers.</td></tr>" count)
         (format s "<tr><td>")
         (output-finelook-inner s structure objects slots results)
         (format s "</td></tr>")
         (format s "<tr><td>")
         (output-finelook-create s (cadr structure) structure)
         (format s "</td></tr></table>"))
        (t (format s "<table>")
           (format s "<tr><td align='center'>There are ~D answers.  The following table shows answers ~A through ~A.</td></tr>"
                   count start end)
           (format s "<tr><td>")
           (output-finelook-inner s structure objects slots results)
           (format s "</td></tr><tr><td>")
           (multiple-value-setq (start end) (kerchunk count start end))
           (output-finelook-create s (cadr structure) structure)
           (output-finelook-display s (cadr structure) structure start end)
           (format s "</td></tr></table>")))
  (format s "</center>") (crlf s))

(defun output-finelook-inner (s structure items slots results)
  (let (class nohandle)
    (setq class (cadr structure))
    (setq nohandle (findp `(nodisplay ,class handle) *interface*))
    (format s "<table cellspacing='2' bgcolor='~A' border='~A'>" *bgcolor* *border*) (crlf s)
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
            (when vals (output-handle-in-style s (car vals) style))
            (dolist (val (cdr vals))
              (format s ", ")
              (output-handle-in-style s val style))
            (format s "<br/></td>"))
        (format s "</tr>")
        (crlf s))
    (format s "</table>") (crlf s)))

(defmethod output-finelook-create (s class structure)
  (declare (ignore class))
  (unless (findp `(or (nocommand ,*gui* create) (nocreate ,*gui* ,(cadr structure))) *interface*)
    (format s "<form action='fastcreatepage?' method='post'>" (stringize structure))
    (format-hidden s "Class" (stringize (cadr structure)))
    (format-button s "Command" "Create")
    (format s " a new ~A." (prettify (cadr structure)))
    (format s "</form>") (crlf s)))

(defmethod output-finelook-display (s class structure start end)
  (declare (ignore class))
  (format s "<form action='finelookpage?' method='post'>")
  (format-hidden s "Structure" (htmlify (prin1-to-string structure)))
  (format-button s "Command" "Display")
  (format s "answers ")
  (format-text s "Start" (princ-to-string start) 5)
  (format s " through ")
  (format-text s "End" (princ-to-string end) 5)
  (format s "</form>") (crlf s))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; fineviewpage
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defparameter *fineviewpage* "fineviewpage")

(defmethod process (s (command (eql 'fineviewpage)) postlines)
  (let (dum start end)
    (cond ((setq dum (getf-post "Class" postlines))
           (setq dum (fastcomparestructure (read-user-string dum)))
           (setq start (or (read-value-string (getf-post "Start" postlines)) 1))
           (setq end (or (read-value-string (getf-post "End" postlines)) *count*))
           (process-fineviewpage s dum start end))
          ((setq dum (getf-post "Structure" postlines))
           (setq start (or (read-value-string (getf-post "Start" postlines)) 1))
           (setq end (or (read-value-string (getf-post "End" postlines)) *count*))
           (process-fineviewpage s (read-user-string dum) start end))
          (t (http-problem s "Bad request.")))))

(defun process-fineviewpage (s structure start end)
  (format-html s) (crlf s)
  (format-head s)
  (format s "<title>fineviewpage</title>") (crlf s)
  (format s (fineviewscript)) (crlf s)
  (format s (stylesheet)) (crlf s)
  (format s (modalscript)) (crlf s)
  (finish-head s) (crlf s)
  (format-body s *bgcolor*) (crlf s)
  (output-header s)
  (format-border s)
  (process-fineview s structure start end)
  (finish-border s)
  (output-footer s)
  (finish-body s) (crlf s)
  (finish-html s) (crlf s))

(defun process-fineview (s structure start end)
  (let (objects sorter attributes results count)
    (setq objects (findinstances (viewconvert structure) *gui*))
    (when (setq sorter (find-sorter (cadr structure)))
      (setq objects (sortem objects sorter 'ascending)))
    (multiple-value-setq (objects count start end) (trim objects start end))
    (setq attributes (displayable-slots (cadr structure)))
    (setq results (prorequest `(ask-table ,objects ,attributes)))
    (output-fineview s structure objects attributes results count start end)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun output-fineview (s structure objects slots results count start end)
  (format s "<center>")
  (format s "<br/>")
  (cond ((= count 0)
         (format s "<table>")
         (format s "<tr><td align='center'>There are no answers.</td></tr>")
         (unless (emptystructurep structure)
           (format s "<tr><td>")
           (output-fineview-inner s structure objects slots results)
           (format s "</td></tr>"))
         (format s "<tr><td>")
         (output-fineview-create s (cadr structure) structure)
         (format s "</td></tr></table>"))
        ((= count 1)
         (format s "<table>")
         (format s "<tr><td align='center'>There is 1 answer.</td></tr>")
         (format s "<tr><td>")
         (output-fineview-inner s structure objects slots results)
         (format s "</td></tr>")
         (format s "<tr><td>")
         (output-fineview-create s (cadr structure) structure)
         (format s "</td></tr></table>"))
        ((and (= start 1) (geqp end count))
         (format s "<table>")
         (format s "<tr><td align='center'>There are ~D answers.</td></tr>" count)
         (format s "<tr><td>")
         (output-fineview-inner s structure objects slots results)
         (format s "</td></tr>")
         (format s "<tr><td>")
         (output-fineview-create s (cadr structure) structure)
         (format s "</td></tr></table>"))
        (t (format s "<table>")
           (format s "<tr><td align='center'>There are ~D answers.  The following table shows answers ~A through ~A.</td></tr>"
                   count start end)
           (format s "<tr><td>")
           (output-fineview-inner s structure objects slots results)
           (format s "</td></tr><tr><td>")
           (multiple-value-setq (start end) (kerchunk count start end))
           (output-fineview-create s (cadr structure) structure)
           (output-fineview-display s (cadr structure) structure start end)
           (format s "</td></tr></table>")))
  (format s "</center>") (crlf s))

(defun output-fineview-inner (s structure items slots results)
  (let (class nohandle (*buttons* 0))
    (setq class (cadr structure))
    (setq nohandle (findp `(nodisplay ,class handle) *interface*))
    (format s "<form name='form1' action='~A?' method='post'>" *fineviewpage*) (crlf s)
    (format-hidden s "Object" (stringize (car structure))) (crlf s)
    (format-hidden s "Class" (stringize (cadr structure))) (crlf s)
    (format s "<table bgcolor='~A' border='~A'>" *bgcolor* *border*) (crlf s)
    (format s "<tr>")
    (unless nohandle
      (format s "<th>")
      (format s "<span style='cursor:pointer; text-decoration:underline; color:#000000'
                       onClick='postReplace(window.document.form1)'>")
      (format s (iconify class))
      (format s "</span>")
      (format s "</th>"))
    (do ((l (cddr structure)) (slot) (values))
        ((null l))
        (setq slot (caar l))
        (multiple-value-setq (values l) (collectentries slot l))
        (format s "<th>")
        (output-fineview-cells s slot values (find-searchstyle slot))
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
            (when vals (output-handle-in-style s (car vals) style))
            (dolist (val (cdr vals))
              (format s ", ")
              (output-handle-in-style s val style))
            (format s "<br/></td>"))
        (format s "</tr>")
        (crlf s))
    (format s "</table>") (crlf s)
    (format s "</form>") (crlf s)))

(defmethod output-fineview-cells (s slot values (style (eql 'menu)))
  (let (options)
    (setq options (findalternatives slot))
    (incf *buttons*)
    (when options (output-newmenu s slot options values))
    (crlf s)))

(defmethod output-fineview-cells (s slot values (style (eql 'selector)))
  (output-fineview-multicells s slot values style))

(defmethod output-fineview-cells (s slot values (style (eql 'fineselector)))
  (incf *buttons*)
  (output-fineview-cell s slot (car values) style))

(defmethod output-fineview-cells (s slot values (style (eql 'fancyselector)))
  (incf *buttons*)
  (output-fineview-cell s slot (car values) style))

(defmethod output-fineview-cells (s slot values (style (eql 'combobox)))
  (incf *buttons*)
  (output-fineview-cell s slot (car values) style))

(defmethod output-fineview-cells (s slot values (style (eql 'checkbox)))
  (let (options)
    (setq options (findalternatives slot))
    (incf *buttons*)
    (when options (output-newcheckbox s slot options values))
    (crlf s)))

(defmethod output-fineview-cells (s slot values (style (eql 'radiobutton)))
  (incf *buttons*)
  (output-fineview-cell s slot (car values) style))

(defmethod output-fineview-cells (s slot values (style (eql 'typein)))
  (output-fineview-multicells s slot values style))

(defmethod output-fineview-cells (s slot values (style (eql 'stringfield)))
  (output-fineview-multicells s slot values style))

(defmethod output-fineview-cells (s slot values (style (eql 'text)))
  (output-fineview-multicells s slot values style))

(defmethod output-fineview-cells (s slot values (style (eql 'textarea)))
  (output-fineview-multicells s slot values style))

(defmethod output-fineview-cells (s slot values (style (eql 'password)))
  (output-fineview-multicells s slot values style))

(defmethod output-fineview-cells (s slot values (style (eql 'dateinput)))
  (output-fineview-multicells s slot values style))

(defmethod output-fineview-cells (s slot values (style (eql 'datestyle)))
  (incf *buttons*)
  (output-fineview-cell s slot (car values) style))

(defmethod output-fineview-cells (s slot values (style (eql 'dollarinput)))
  (output-fineview-multicells s slot values style))

(defmethod output-fineview-cells (s slot values (style (eql 'dollarstyle)))
  (incf *buttons*)
  (output-fineview-cell s slot (car values) style))

(defmethod output-fineview-cells (s slot values (style (eql 'emailstyle)))
  (incf *buttons*)
  (output-fineview-cell s slot (car values) style))

(defmethod output-fineview-cells (s slot values (style (eql 'htmlstyle)))
  (incf *buttons*)
  (output-fineview-cell s slot (car values) style))

(defmethod output-fineview-cells (s slot values (style (eql 'imagestyle)))
  (incf *buttons*)
  (output-fineview-cell s slot (car values) style))

(defmethod output-fineview-cells (s slot values (style (eql 'urlstyle)))
  (incf *buttons*)
  (output-fineview-cell s slot (car values) style))

(defmethod output-fineview-cells (s slot values (style (eql 'glyph)))
  (incf *buttons*)
  (output-fineview-cell s slot (car values) style))

(defmethod output-fineview-cells (s slot values (style (eql 'prettystyle)))
  (incf *buttons*)
  (output-fineview-cell s slot (car values) style))

(defmethod output-fineview-cells (s slot values style)
  (incf *buttons*)
  (output-fineview-cell s slot (car values) style))

(defun output-fineview-multicells (s slot values style)
  (let (multivalued multiple)
    (setq multivalued (find-multivalued slot))
    (setq multiple (cdr values))
    (format s "<table cellpadding='0' cellspacing='0'>")
    (incf *buttons*)
    (format s "<tr><td valign='center'>")
    (when multivalued (output-add s *buttons*))
    (when multiple (output-delete s *buttons*))
    (format s "</td><td valign='center'>")
    (output-fineview-cell s slot (car values) style) (crlf s)
    (format s "</td></tr>")
    (dolist (value (cdr values))
      (incf *buttons*)
      (format s "<tr><td valign='center'>")
      (when multivalued (output-add s *buttons*))
      (when multiple (output-delete s *buttons*))
      (format s "</td><td>")
      (output-fineview-cell s slot value style) (crlf s)
      (format s "</td></tr>"))
    (format s "</table>")
    (crlf s)))

(defmethod output-fineview-cell (s slot value (style (eql 'selector)))
  (declare (ignore class))
  (let (options)
    (when (setq options (findalternatives slot))
      (setq options (cons 'unknown options))
      (output-newselector s slot options value))))

(defmethod output-fineview-cell (s slot value (style (eql 'fineselector)))
  (output-newfastselector s slot value))

(defmethod output-fineview-cell (s slot value (style (eql 'fancyselector)))
  (output-newfancyselector s slot value))

(defmethod output-fineview-cell (s slot value (style (eql 'combobox)))
  (let (options)
    (when (setq options (findalternatives slot))
      (setq options (cons 'unknown options))
      (output-newcombobox s slot options (prettyname value) 20))))

(defmethod output-fineview-cell (s slot value (style (eql 'radiobutton)))
  (let (options)
    (cond ((setq options (findalternatives slot))
           (setq options (cons nil options))
           (output-newradiobutton s slot options value))
          (t (format-hidden s (stringize slot) "")))))

(defmethod output-fineview-cell (s slot value (style (eql 'typein)))
  (format-newtypein s (stringize slot) (stringize value) 20))

(defmethod output-fineview-cell (s slot value (style (eql 'stringfield)))
  (format-newstringfield s (stringize slot) (prettyname value) 20))

(defmethod output-fineview-cell (s slot value (style (eql 'text)))
  (output-fineview-cell s slot value 'stringfield))

(defmethod output-fineview-cell (s slot value (style (eql 'textarea)))
  (output-fineview-cell s slot value 'stringfield))

(defmethod output-fineview-cell (s slot value (style (eql 'password)))
  (format-newpassword s (stringize slot) (prettyname value) 20))

(defmethod output-fineview-cell (s slot value (style (eql 'dateinput)))
  (declare (ignore class structure))
  (output-newdateinput s slot value))

(defmethod output-fineview-cell (s slot value (style (eql 'datestyle)))
  (format-hidden s (stringize slot) (stringize value))
  (if value (output-simple s value) (format s "~A" (iconify slot))))

(defmethod output-fineview-cell (s slot value (style (eql 'dollarinput)))
  (output-newdollarinput s slot value 6))

(defmethod output-fineview-cell (s slot value (style (eql 'dollarstyle)))
  (format-hidden s (stringize slot) (stringize value))
  (if (realp value) (format s "$~$" value) (format s "~A" (iconify slot))))

(defmethod output-fineview-cell (s slot value (style (eql 'emailstyle)))
  (format-hidden s (stringize slot) (stringize value))
  (if value (format s "<a href='mailto:~A'><font color='red'>~A</font></a>" value value)
      (format s "~A" (iconify slot))))

(defmethod output-fineview-cell (s slot value (style (eql 'htmlstyle)))
  (format-hidden s (stringize slot) (stringize value))
  (if value (format s "~A" value) (format s "~A" (iconify slot))))

(defmethod output-fineview-cell (s slot value (style (eql 'imagestyle)))
  (format-hidden s (stringize slot) (stringize value))
  (if value (format s "<img src='~A'/>" value) (format s "~A" (iconify slot))))

(defmethod output-fineview-cell (s slot value (style (eql 'urlstyle)))
  (declare (ignore class structure))
  (format-hidden s (stringize slot) (stringize value))
  (if value (format s "<a href='~A'>~A</a>" value (htmlify value))
      (format s "~A" (iconify slot))))

(defmethod output-fineview-cell (s slot value (style (eql 'glyph)))
  (format-hidden s (stringize slot) (stringize value))
  (format s "~A" (iconify slot)))

(defmethod output-fineview-cell (s slot value (style (eql 'prettystyle)))
  (format-hidden s (stringize slot) (stringize value))
  (format s "~A" (iconify slot)))

(defmethod output-fineview-cell (s slot value style)
  (declare (ignore style))
  (format-hidden s (stringize slot) (stringize value))
  (format s "~A" (iconify slot)))

(defun output-fineview-create (s class structure)
  (declare (ignore class))
  (unless (findp `(or (nocommand ,*gui* create) (nocreate ,*gui* ,(cadr structure))) *interface*)
    (format s "<form action='fastcreatepage?' method='post'>" (stringize structure))
    (format-hidden s "Class" (stringize (cadr structure)))
    (format-button s "Command" "Create")
    (format s " a new ~A." (prettify (cadr structure)))
    (format s "</form>") (crlf s)))

(defun output-fineview-display (s class structure start end)
  (declare (ignore class))
  (format s "<form action='fineviewpage?' method='post'>")
  (format-hidden s "Structure" (htmlify (prin1-to-string structure)))
  (format-button s "Command" "Display")
  (format s "answers ")
  (format-text s "Start" (princ-to-string start) 5)
  (format s " through ")
  (format-text s "End" (princ-to-string end) 5)
  (format s "</form>") (crlf s))

(defun newprettify (slot x)
  (cond ((eq x 'unknown) (stringappend "Any " (iconify slot)))
        (t (prettyname x))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun fineviewscript ()
  "<script>

var http_slot = false;
var http_old = false;

function fineselect (slot,value)
 {http_slot = slot;
  postSelect('fineselect?','Slot=' + slot + '&Value=' + value)}

function postSelect (url,args)
 {http_result = false;
  if (window.XMLHttpRequest)
     {http_result = new XMLHttpRequest();
      if (http_result.overrideMimeType)
         {http_result.overrideMimeType('text/xml');}}
  else if (window.ActiveXObject)
          {try {http_result = new ActiveXObject('Msxml2.XMLHTTP')}
           catch (e) {try {http_result = new ActiveXObject('Microsoft.XMLHTTP')}
                      catch (e) {} }}
  http_result.onreadystatechange = alertSelect;
  http_result.open('POST', url, true);
  http_result.send(args);}

function alertSelect()
 {if (http_result.readyState == 4)
     {if (http_result.responseText)
         {var cell = document.getElementById(http_slot);
          cell.innerHTML = http_result.responseText;
          postReplace(form1)}
      else {alert('There was a problem with the request in alertResult.')}}}

function fancyselect (slot,old,context,value)
 {http_slot = slot;
  http_old = value;
  postFancy('fancyselect?','Slot=' + slot + '&Old=' + old + '&Context=' + context + '&Value=' + value)}

function fancydeselect (slot,old,context,value)
 {http_slot = slot;
  http_old = value;
  postFancy('fancydeselect?','Slot=' + slot + '&Old=' + old + '&Context=' + context + '&Value=' + value)}

function fancycancel (slot,old)
 {document.getElementById(slot).innerHTML=''}

function postFancy (url,args)
 {http_result = false;
  if (window.XMLHttpRequest)
     {http_result = new XMLHttpRequest();
      if (http_result.overrideMimeType)
         {http_result.overrideMimeType('text/xml');}}
  else if (window.ActiveXObject)
          {try {http_result = new ActiveXObject('Msxml2.XMLHTTP')}
           catch (e) {try {http_result = new ActiveXObject('Microsoft.XMLHTTP')}
                      catch (e) {} }}
  http_result.onreadystatechange = alertFancy;
  http_result.open('POST', url, true);
  http_result.send(args);}

function alertFancy()
 {if (http_result.readyState == 4)
     {if (http_result.responseText)
         {var elem = document.getElementById(http_slot);
          if (http_result.responseText == ' ')
             {var inp = document.getElementsByName(http_slot)[0];
              inp.value=http_old;
              postReplace(inp.form)};
          elem.innerHTML = http_result.responseText}
      else {alert('There was a problem with the request in alertResult.')}}}

</script>")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; finechangepage
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defparameter *finechangepage* "finechangepage")

(defmethod process (s (command (eql 'finechangepage)) postlines)
  (let (dum class structure)
    (cond ((null postlines) (http-problem s "Bad request."))
          ((null (cdr postlines))
           (setq dum (read-value-string (cdr (pop postlines))))
           (setq class (classify dum *gui*))
           (process-finechangepage s dum class (changeitem dum class)))
          ((string-equal (getf-post "Command" postlines) "Refresh")
           (setq structure (read-user-string (cdr (pop postlines))))
           (output-fastcreatepage s structure))
          ((string-equal (getf-post "Command" postlines) "Record")
           (setq structure (read-user-string (cdr (pop postlines))))
           (updatemodel structure *repository*)
           (http-problem s "Okay"))
          (t (http-problem s "Bad request.")))))

(defmethod process-finechangepage (s object class structure)
  (declare (ignore object class))
  (output-finechangepage s structure))

(defmethod process-finechangepage-record (s object class constraints)
  (let (delta result)
    (cond ((setq result (checkchange object class constraints))
           (http-problems s result))
          ((setq delta (differentiator (list* object class constraints)))
           (setq result (prorequest (cons 'update delta)))
           (cond ((errorp result) (http-problems s result))
                 (t (output-finechange-success s object))))
          (t (output-finechange-success s object)))))  

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun output-finechangepage (s structure)
  (format-html s) (crlf s)
  (format s "<head>")
  (format s "<title>Change ~A</title>" (prettify (car structure))) (crlf s)
  (format s "<script type='text/javascript' src='~Ajavascript/modal.js'></script>" *home*) (crlf s)
  (format s "<script type='text/javascript' src='~Ajavascript/finechange.js'></script>" *home*) (crlf s)
  (format s "<script type='text/javascript' src='~Ajavascript/patcher.js'></script>" *home*) (crlf s)
  (format s "</head>") (crlf s)
  (force-output s)
  (format-body s *bgcolor* "hidePopup()") (crlf s)
  (output-header s)
  (format-border s) (crlf s)
  (output-finechange s structure)
  (finish-border s) (crlf s)
  (output-footer s)
  (finish-body s) (crlf s)
  (finish-html s) (crlf s)
  'done)

(defun output-finechange (s structure)
  (let ((*buttons* 0))
    (format s "<div id='~A' class='~A'>" (car structure) (cadr structure)) (crlf s)
    (output-handle s (car structure))
    (format s " is ~A ~A." (article (cadr structure)) (prettify (cadr structure)))
    (format s "<br/>") (crlf s)
    (output-finechange-inner s structure)
    (output-finechange-record s (cadr structure)) (crlf s)
    (format s "</div>")))

(defun output-finechange-inner (s structure)
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
      (cond ((and (null (cdr values)) (not (find-multivalued slot)))
             (format s "<table cellpadding='0' cellspacing='0'><tr><td width='32'>&nbsp;</td><td>")
             (output-finechange-cell s slot (car values) style)
             (format s "</td></tr></table>"))
            (t (output-finechange-cells s slot values style)))
      (format s "</td>")
      (when label (format s "<td>~A</td>" label))
      (format s "</tr>")
      (crlf s))
  (format s "</table>") (crlf s))

(defun output-finechange-inner (s structure)
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
             (output-finechange-cell s slot (car values) style)
             (format s "</td></tr>")
             (format s "</table>"))
            (t (output-finechange-cells s slot values style)))
      (format s "</td>")
      (when label (format s "<td>~A</td>" label))
      (format s "</tr>")
      (crlf s))
  (format s "</table>") (crlf s))

(defun stringizem (items)
  (let (ns)
    (cond ((null items) "")
          (t (setq ns (stringize (car items)))
             (dolist (item (cdr items))
               (setq ns (strappend ns "&" (stringize item))))
             ns))))

(defmethod output-finechange-cells (s slot values (style (eql 'menu)))
  (let (options)
    (setq options (findalternatives slot))
    (format s "<table cellpadding='0' cellspacing='0'>")
    (incf *buttons*)
    (format s "<tr><td width='32' valign='top'>")
    (output-empty-button s)
    (output-empty-button s)
    (format s "</td><td>")
    (when options (output-oldmenu s slot options values))
    (format s "</td></tr>") (crlf s)
    (format s "</table>")
    (crlf s)))

(defmethod output-finechange-cells (s slot values (style (eql 'selector)))
  (output-finechange-multicells s slot values style))

(defmethod output-finechange-cells (s slot values (style (eql 'fineselector)))
  (output-finechange-unicells s slot values style))

(defmethod output-finechange-cells (s slot values (style (eql 'fancyselector)))
  (output-finechange-unicells s slot values style))

(defmethod output-finechange-cells (s slot values (style (eql 'combobox)))
  (output-finechange-unicells s slot values style))

(defmethod output-finechange-cells (s slot values (style (eql 'checkbox)))
  (let (options)
    (setq options (findalternatives slot))
    (format s "<table cellpadding='0' cellspacing='0'>")
    (format s "<tr><td width='32' valign='top'>")
    (output-empty-button s)
    (output-empty-button s)
    (format s "</td><td>")
    (when options (output-oldcheckbox s slot options values))
    (format s "</td></tr>") (crlf s)
    (format s "</table>")
    (crlf s)))

(defmethod output-finechange-cells (s slot values (style (eql 'radiobutton)))
  (output-finechange-unicells s slot values style))

(defmethod output-finechange-cells (s slot values (style (eql 'typein)))
  (output-finechange-multicells s slot values style))

(defmethod output-finechange-cells (s slot values (style (eql 'stringfield)))
  (output-finechange-multicells s slot values style))

(defmethod output-finechange-cells (s slot values (style (eql 'text)))
  (output-finechange-multicells s slot values style))

(defmethod output-finechange-cells (s slot values (style (eql 'textarea)))
  (output-finechange-multicells s slot values style))

(defmethod output-finechange-cells (s slot values (style (eql 'password)))
  (output-finechange-multicells s slot values style))

(defmethod output-finechange-cells (s slot values (style (eql 'dollarinput)))
  (output-finechange-multicells s slot values style))

(defmethod output-finechange-cells (s slot values style)
  (output-finechange-unicells s slot values style))

(defmethod output-finechange-cell (s slot value (style (eql 'selector)))
  (output-fineselector s slot (cons 'unknown (findalternatives slot)) value))

(defmethod output-finechange-cell (s slot value (style (eql 'fineselector)))
  (output-oldfastselector s slot value))

(defmethod output-finechange-cell (s slot value (style (eql 'fancyselector)))
  (output-oldfancyselector s slot value))

(defmethod output-finechange-cell (s slot value (style (eql 'combobox)))
  (let (options)
    (when (setq options (findalternatives slot))
      (setq options (cons 'unknown options))
      (output-oldcombobox s slot options (prettyname value) 40))))

(defmethod output-finechange-cell (s slot value (style (eql 'radiobutton)))
  (let (options)
    (when (setq options (findalternatives slot))
      (setq options (cons nil options))
      (output-oldradiobutton s slot options value))))

(defmethod output-finechange-cell (s slot value (style (eql 'typein)))
  (format-oldtypein s (stringize slot) (stringize value) 40))

(defmethod output-finechange-cell (s slot value (style (eql 'stringfield)))
  (format-oldstringfield s (stringize slot) (prettyname value) 10))

(defmethod output-finechange-cell (s slot value (style (eql 'text)))
  (format-text s slot (prettyname value) 40))

(defmethod output-finechange-cell (s slot value (style (eql 'textarea)))
  (format-textarea s slot (prettyname value) 4 80))

(defmethod output-finechange-cell (s slot value (style (eql 'password)))
  (format-password s slot (prettyname value) 40))

(defmethod output-finechange-cell (s slot value (style (eql 'dateinput)))
  (output-olddateinput s slot value))

(defmethod output-finechange-cell (s slot value (style (eql 'datestyle)))
  (declare (ignore slot))
  (output-simple s value))

(defmethod output-finechange-cell (s slot value (style (eql 'dollarinput)))
  (output-olddollarinput s slot value 6))

(defmethod output-finechange-cell (s slot value (style (eql 'dollarstyle)))
  (declare (ignore slot))
  (when (realp value) (format s "$~$" value)))

(defmethod output-finechange-cell (s slot value (style (eql 'emailstyle)))
  (declare (ignore slot))
  (format s "<a href='mailto:~A'><font color='red'>~A</font></a>" value value))

(defmethod output-finechange-cell (s slot value (style (eql 'htmlstyle)))
  (declare (ignore slot))
  (format s "~A" value))

(defmethod output-finechange-cell (s slot value (style (eql 'imagestyle)))
  (declare (ignore slot))
  (format s "<img src='~A'/>" value))

(defmethod output-finechange-cell (s slot value (style (eql 'urlstyle)))
  (format-hidden s slot value)
  (format s "<a href='~A'>~A</a>" value (htmlify value)))

(defmethod output-finechange-cell (s slot value (style (eql 'glyph)))
  (format-hidden s (stringize slot) (stringize value))
  (output-handle s value))

(defmethod output-finechange-cell (s slot value (style (eql 'prettystyle)))
  (format-hidden s (stringize slot) (stringize value))
  (output-simple s value))

(defmethod output-finechange-cell (s slot value style)
  (declare (ignore style))
  (format-hidden s (stringize slot) (stringize value)))



(defmethod output-finechange-cells (s slot values (style (eql 'tabulator)))
  (let (class slots results)
    (setq class (find-range slot))
    (setq slots (createable-slots class))
    (setq results (prorequest `(ask-table ,values ,slots)))
    (format s "<div>")
    (output-fineupdate-inner s slot class values slots results)
    (format s "</div>")))


(defmethod output-finechange-cells (s slot values (style (eql 'subframe)))
  (format s "<table cellpadding='0' cellspacing='0'>")
  (incf *buttons*)

  (format s "<tr id='~A' status='skip' style='display:None'>" slot)
  (format s "<td width='32' valign='top'>")
  (output-additem s slot)
  (output-remitem s)
  (format s "</td><td>")
  (output-finechange-cell s slot nil style)
  (format s "</td>")
  (format s "</tr>")

  (dolist (value values)
    (incf *buttons*)
    (format s "<tr><td width='32' valign='top'>")
    (output-additem s slot)
    (output-remitem s)
    (format s "</td><td>")
    (output-finechange-cell s slot value style) (crlf s)
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

(defmethod output-finechange-cell (s slot value (style (eql 'subframe)))
  (let (class)
    (setq class (find-range slot))
    (format s "<div slot='~A' id='~A' class='~A' style='border:groove; width:640'>"
            slot value class)
    (format s "<span>~A</span>" (prettyname value))
    (format s "<br/>")
    (output-finechange-inner s (changeitem value class))
    (format s "</div>")))

(defmethod output-finechange-unicells (s slot values style)
  (format s "<table cellpadding='0' cellspacing='0'>")
  (incf *buttons*)
  (format s "<tr><td width='32' valign='top'>")
  (output-empty-button s)
  (output-empty-button s)
  (format s "</td><td>")
  (output-finechange-cell s slot (car values) style) (crlf s)
  (format s "</td></tr>")
  (dolist (value (cdr values))
    (incf *buttons*)
    (format s "<tr><td width='32' valign='top'>")
    (output-empty-button s)
    (output-empty-button s)
    (format s "</td><td>")
    (output-finechange-cell s slot value style) (crlf s)
    (format s "</td></tr>"))
  (format s "</table>")
  (crlf s))

(defun output-finechange-multicells (s slot values style)
  (format s "<table cellpadding='0' cellspacing='0'>")
  (incf *buttons*)

  (format s "<tr id='~A' status='skip' style='display:None'>" slot)
  (format s "<td width='32' valign='top'>")
  (output-addslot s slot)
  (output-remslot  s slot)
  (format s "</td><td>")
  (output-finechange-cell s slot nil style)
  (format s "</td>")
  (format s "</tr>")

  (dolist (value values)
    (incf *buttons*)
    (format s "<tr><td width='32' valign='top'>")
    (output-addslot s slot)
    (output-remslot  s slot)
    (format s "</td><td>")
    (output-finechange-cell s slot value style) (crlf s)
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

(defmethod output-finechange-record (s class)
  (declare (ignore class))
  (format s "<input type='button' name='Command' value='Show' onClick='showstructure(this)'/>")
  (format s "<input type='button' name='Command' value='Record' onClick='record(this)'/>"))

(defun output-finechange-success (s object)
  (format-html s) (crlf s)
  (format s "<head>")
  (format s "<title>Change ~A</title>" (prettify object)) (crlf s)
  (format s "</head>") (crlf s)
  (format s "<body leftmargin='0' topmargin='0' marginwidth='0' marginheight='0' bgcolor='~A' onLoad='~A'>"
          *bgcolor* (format nil "location.replace(\"fineinspectpage?Object=~A\")" object)) (crlf s)
  (output-header s)
  (format-border s) (crlf s)
  (format s "Change successful.")
  (finish-border s) (crlf s)
  (output-footer s)
  (finish-body s) (crlf s)
  (finish-html s) (crlf s)
  'done)

(defun output-fineselector (s slot options value)
  (format s "<select name='~A' onClick='showchange(this)'>"
          (stringize slot) *buttons*)
  (dolist (option options)
    (cond ((equalp option value)
           (format s "<option value='~A' selected='true'>~A</option>"
                   (stringize option) (prettyname option)) (crlf s))
          (t (format s "<option value='~A'>~A</option>"
                     (stringize option) (prettyname option)) (crlf s))))
  (format s "</select>"))

(defun output-addslot (s x)
  (format s "<image src='~Aimages/add.gif' border='0' onClick='addslot(\"~A\",this)'/>"
          *home* x))

(defun output-remslot (s x)
  (format s "<image src='~Aimages/delete.gif' border='0' onClick='remslot(this)'/>"
          *home* (addressify x)))

(defun output-additem (s x)
  (format s "<image src='~Aimages/add.gif' border='0' onClick='additem(\"~A\",this)'/>"
          *home* x))

(defun output-remitem (s)
  (format s "<image src='~Aimages/delete.gif' border='0' onClick='remitem(this)'/>" *home*))

(defun output-hidetable (s)
  (format s "<image src='~Aimages/magnifier.gif' border='0' onClick='hidetable(this)'/>"
          *home*))

(defun output-addrow (s class)
  (format s "<image src='~Aimages/add.gif' border='0' onClick='addrow(\"~A\",this)'/>"
          *home* class))

(defun output-remrow (s)
  (format s "<image src='~Aimages/delete.gif' border='0' onClick='remrow(this)'/>" *home*))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod output-cells (s slot values (style (eql 'tabulator)))
  (let (class slots results)
    (setq class (find-range slot))
    (setq slots (displayable-slots class))
    (setq results (prorequest `(ask-table ,values ,slots)))
    (format s "<div>")
    (output-fastlook-inner s `(? ,class) values slots results)
    (format s "</div>")))

(defmethod output-cell (s slot value (style (eql 'subframe)))
  (declare (ignore slot))
  (format s "<div style='border:groove'>")
  (output-fastinspect s value (classify value *repository*))
  (format s "</div>"))

;;;;

(defun output-fineupdate-inner (s slot class items slots results)
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
        (format s "<tr slot='~A' id='~A' class='~A' bgcolor='~A'>"
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
                   (output-finechange-cell s (car slots) (caar n) style))
                  (t (output-finechange-cells s (car slots) (car n) style)))
            (format s "</td>"))
        (format s "</tr>")
        (crlf s))
    (output-fineupdate-original s nohandle slot class slots) (crlf s)
    (format s "<tr bgcolor='#eeeeee'>")
    (format s "<td width='32'>")
    (output-addrow s class)
    (output-empty-button s)
    (format s "</td>")
    (format s "<td colspan='4'></td>")
    (format s "</tr>") (crlf s)
    (format s "</table>") (crlf s)))

(defun output-fineupdate-original (s nohandle slot class slots)
  (format s "<tr slot='~A' id='~A' class='~A' status='skip' style='display:None' bgcolor='~A'>"
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
             (output-finechange-cell s (car slots) nil style))
            (t (output-finechange-cells s (car slots) nil style)))
      (format s "</td>"))
  (format s "</tr>"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun updatemodel (structure source)
  (killmodel (car structure) source)
  (savemodel structure source))

(defun savemodel (structure source)
  (insert `(model.instance ,(car structure)) source)
  (dolist (pair (cddr structure))
    (cond ((and (eq (car pair) 'model.shoe) (cdr pair))
           (insert `(model.shoe ,(car structure) ,(caadr pair)) source)
           (saveshoe (cadr pair) source))
          ((cdr pair)
           (insert (list (first pair) (car structure) (second pair)) source))))
  structure)

(defun saveshoe (structure source)
  (insert `(shoe.instance ,(car structure)) source)
  (dolist (pair (cddr structure))
    (cond ((and (eq (car pair) 'shoe.gtin) (cdr pair))
           (insert `(shoe.gtin ,(car structure) ,(caadr pair)) source)
           (savegtin (cadr pair) source))
          ((cdr pair)
           (insert (list (first pair) (car structure) (second pair)) source))))
  structure)

(defun savegtin (structure source)
  (insert `(gtin.instance ,(car structure)) source)
  (dolist (pair (cddr structure))
    (insert (list (first pair) (car structure) (second pair)) source))
  structure)

(defun killmodel (model source)
  (dolist (shoe (results 'model.shoe model source))
    (dolist (gtin (results 'shoe.gtin shoe source)) (kill gtin source))
    (kill shoe source))
  (kill model source))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod defineobject (obj class th constraints)
  (kill '(?r obj @l) th #'matchp)
  (insert (makpred obj class th) th)
  (dolist (pair constraints) (insert (list (car pair) obj (cadr pair)) th))
  obj)

#|
(defineobject 'model.type 'attributetable *manager*
  '((domain        model)
    (range         type)
    (total         yes)
    (unique        yes)
    (createstyle   selector)
    (changestyle   selector)
    (searchstyle   selector)
    (comparestyle  prettystyle)
    (inspectstyle  prettystyle)
    (controlslot   model.style)
    (controlvalue  athletic)
    (option        crosstraining)
    (option        hiking)
    (option        running)
    (prettyname    "Type")))

(defineobject 'model.length 'attributetable *manager*
  '((domain        model)
    (range         length)
    (total         yes)
    (unique        yes)
    (createstyle   selector)
    (changestyle   selector)
    (searchstyle   selector)
    (comparestyle  prettystyle)
    (inspectstyle  prettystyle)
    (controlslot   model.style)
    (controlvalue  boot)
    (option        overknee)
    (option        kneehigh)
    (option        midcalf)
    (prettyname    "Length")))
|#
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
