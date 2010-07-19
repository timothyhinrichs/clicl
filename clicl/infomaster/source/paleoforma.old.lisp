;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; (c) Copyright 1999-2007 by Michael Genesereth.  All rights reserved.
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
                   (format s "<a href='fastshowpage?class=~A'>~A</a><br/>"
                           (car classes) (pluralize (car classes)))
                   (setq classes (cdr classes)))
               (format s "</div>")
               (format s "</td>")))
          (t (format s "<td width='25%' valign='top'>")
             (format s "<span style='font-size:16px; font-weight:bold; color:#004488'>~A</span><br/>"
                              (pluralize class))
             (format s "<div style='margin-left: 15px'>")
             (dolist (class classes)
               (format s "<a href='fastshowpage?class=~A'>~A</a><br/>"
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

(defparameter *fastfindpage* "fastfindpage")

(defmethod process (s (command (eql 'fastfindpage)) postlines)
  (let (dum structure)
    (cond ((setq dum (getf-post "Class" postlines))
           (setq structure (fastsearchstructure (read-user-string dum)))
           (output-fastfindpage s structure))
          ((string-equal (getf-post "Command" postlines) "Refresh")
           (setq structure (read-user-string (cdr (pop postlines))))
           (output-fastfindpage s structure))
          ((string-equal (getf-post "Command" postlines) "Display")
           (setq structure (read-user-string (cdr (pop postlines))))
           (process-fastlookpage s structure 1 *count*))
          (t (http-problem s "Bad request.")))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun output-fastfindpage (s structure)
  (format-html s) (crlf s)
  (format-head s)
  (format s "<title>Fastfindpage</title>") (crlf s)
  (format s (stylesheet)) (crlf s)
  (format s (modalscript)) (crlf s)
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
    (format s "<form name='form1' action='~A?'>" *fastfindpage*)
    (format s "Find every <b>~A</b> with the following properties."
            (prettyname (cadr structure)))
    (format-hidden s "Oject" (stringize (car structure))) (crlf s)
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
        (output-fastfind-cells s slot values style)
        (format s "</td>")
        (when label (format s "<td>~A</td>" label))
        (format s "</tr>")
        (crlf s))
    (format s "</table>") (crlf s)
    (output-fastfind-display s (cadr structure))
    (format s "</form>")
    'done))

(defmethod output-fastfind-cells (s slot values (style (eql 'menu)))
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

(defmethod output-fastfind-cells (s slot values (style (eql 'selector)))
  (output-fastfind-multicells s slot values style))

(defmethod output-fastfind-cells (s slot values (style (eql 'fastselector)))
  (output-fastfind-unicells s slot values style))

(defmethod output-fastfind-cells (s slot values (style (eql 'fancyselector)))
  (output-fastfind-unicells s slot values style))

(defmethod output-fastfind-cells (s slot values (style (eql 'combobox)))
  (output-fastfind-unicells s slot values style))

(defmethod output-fastfind-cells (s slot values (style (eql 'checkbox)))
  (let (options)
    (setq options (findalternatives slot))
    (format s "<table cellpadding='0' cellspacing='0'>")
    (incf *buttons*)
    (format s "<tr><td width='32' valign='top'>")
    (output-empty-button s)
    (output-empty-button s)
    (format s "</td><td>")
    (when options (output-oldcheckbox s slot options values))
    (format s "</td></tr>") (crlf s)
    (format s "</table>")
    (crlf s)))

(defmethod output-fastfind-cells (s slot values (style (eql 'radiobutton)))
  (output-fastfind-unicells s slot values style))

(defmethod output-fastfind-cells (s slot values (style (eql 'typein)))
  (output-fastfind-multicells s slot values style))

(defmethod output-fastfind-cells (s slot values (style (eql 'stringfield)))
  (output-fastfind-multicells s slot values style))

(defmethod output-fastfind-cells (s slot values (style (eql 'text)))
  (output-fastfind-multicells s slot values style))

(defmethod output-fastfind-cells (s slot values (style (eql 'textarea)))
  (output-fastfind-multicells s slot values style))

(defmethod output-fastfind-cells (s slot values (style (eql 'password)))
  (output-fastfind-multicells s slot values style))

(defmethod output-fastfind-cells (s slot values (style (eql 'dateinput)))
  (output-fastfind-multicells s slot values style))

(defmethod output-fastfind-cells (s slot values (style (eql 'datestyle)))
  (output-fastfind-unicells s slot values style))

(defmethod output-fastfind-cells (s slot values (style (eql 'dollarinput)))
  (output-fastfind-multicells s slot values style))

(defmethod output-fastfind-cells (s slot values (style (eql 'dollarstyle)))
  (output-fastfind-unicells s slot values style))

(defmethod output-fastfind-cells (s slot values (style (eql 'emailstyle)))
  (output-fastfind-unicells s slot values style))

(defmethod output-fastfind-cells (s slot values (style (eql 'htmlstyle)))
  (output-fastfind-unicells s slot values style))

(defmethod output-fastfind-cells (s slot values (style (eql 'imagestyle)))
  (output-fastfind-unicells s slot values style))

(defmethod output-fastfind-cells (s slot values (style (eql 'urlstyle)))
  (output-fastfind-unicells s slot values style))

(defmethod output-fastfind-cells (s slot values (style (eql 'glyph)))
  (output-fastfind-unicells s slot values style))

(defmethod output-fastfind-cells (s slot values (style (eql 'prettystyle)))
  (output-fastfind-unicells s slot values style))

(defmethod output-fastfind-cells (s slot values style)
  (output-fastfind-unicells s slot values style))

(defmethod output-fastfind-unicells (s slot values style)
  (format s "<table cellpadding='0' cellspacing='0'>")
  (incf *buttons*)
  (format s "<tr><td width='32' valign='top'>")
  (output-empty-button s)
  (output-empty-button s)
  (format s "</td><td>")
  (output-fastfind-cell s slot (car values) style) (crlf s)
  (format s "</td></tr>")
  (dolist (value (cdr values))
    (incf *buttons*)
    (format s "<tr><td width='32' valign='top'>")
    (output-empty-button s)
    (output-empty-button s)
    (format s "</td><td>")
    (output-fastfind-cell s slot value style) (crlf s)
    (format s "</td></tr>"))
  (format s "</table>")
  (crlf s))

(defun output-fastfind-multicells (s slot values style)
  (let (multivalued multiple)
    (setq multivalued (find-multivalued slot))
    (setq multiple (cdr values))
    (format s "<table cellpadding='0' cellspacing='0'>")
    (incf *buttons*)
    (format s "<tr><td width='32' valign='top'>")
    (if multivalued (output-add s *buttons*) (output-empty-button s))
    (if multiple (output-delete s *buttons*) (output-empty-button s))
    (format s "</td><td>")
    (output-fastfind-cell s slot (car values) style) (crlf s)
    (format s "</td></tr>")
    (dolist (value (cdr values))
      (incf *buttons*)
      (format s "<tr><td width='32' valign='top'>")
      (if multivalued (output-add s *buttons*) (output-empty-button s))
      (if multiple (output-delete s *buttons*) (output-empty-button s))
      (format s "</td><td>")
      (output-fastfind-cell s slot value style) (crlf s)
      (format s "</td></tr>"))
    (format s "</table>")
    (crlf s)))

(defmethod output-fastfind-cell (s slot value (style (eql 'selector)))
  (let (options)
    (when (setq options (findalternatives slot))
      (setq options (cons 'unknown options))
      (output-oldselector s slot options value))))

(defmethod output-fastfind-cell (s slot value (style (eql 'fastselector)))
  (output-oldfastselector s slot value))

(defmethod output-fastfind-cell (s slot value (style (eql 'fancyselector)))
  (output-oldfancyselector s slot value))

(defmethod output-fastfind-cell (s slot value (style (eql 'combobox)))
  (let (options)
    (when (setq options (findalternatives slot))
      (setq options (cons 'unknown options))
      (output-oldcombobox s slot options (prettyname value) 30))))

(defmethod output-fastfind-cell (s slot value (style (eql 'checkbox)))
  (output-fastfind-cell s slot value 'radiobutton))

(defmethod output-fastfind-cell (s slot value (style (eql 'radiobutton)))
  (let (options)
    (cond ((setq options (findalternatives slot))
           (setq options (cons nil options))
           (output-oldradiobutton s slot options value))
          (t (format-hidden s (stringize slot) "")))))

(defmethod output-fastfind-cell (s slot value (style (eql 'typein)))
  (format-oldtypein s (stringize slot) (stringize value) 40))

(defmethod output-fastfind-cell (s slot value (style (eql 'stringfield)))
  (format-oldstringfield s (stringize slot) (prettyname value) 40))

(defmethod output-fastfind-cell (s slot value (style (eql 'text)))
  (output-fastfind-cell s slot value 'stringfield))

(defmethod output-fastfind-cell (s slot value (style (eql 'textarea)))
  (output-fastfind-cell s slot value 'stringfield))

(defmethod output-fastfind-cell (s slot value (style (eql 'password)))
  (format-oldpassword s (stringize slot) (prettyname value) 40))

(defmethod output-fastfind-cell (s slot value (style (eql 'dateinput)))
  (output-olddateinput s slot value))

(defmethod output-fastfind-cell (s slot value (style (eql 'datestyle)))
  (format-hidden s (stringize slot) (stringize value))
  (output-simple s value))

(defmethod output-fastfind-cell (s slot value (style (eql 'dollarinput)))
  (output-olddollarinput s slot value 6))

(defmethod output-fastfind-cell (s slot value (style (eql 'dollarstyle)))
  (format-hidden s (stringize slot) (stringize value))
  (when (realp value) (format s "$~$" value)))

(defmethod output-fastfind-cell (s slot value (style (eql 'emailstyle)))
  (format-hidden s (stringize slot) (stringize value))
  (when value (format s "<a href='mailto:~A'><font color='red'>~A</font></a>" value value)))

(defmethod output-fastfind-cell (s slot value (style (eql 'htmlstyle)))
  (format-hidden s (stringize slot) (stringize value))
  (when value (format s "~A" value)))

(defmethod output-fastfind-cell (s slot value (style (eql 'imagestyle)))
  (format-hidden s (stringize slot) (stringize value))
  (when value (format s "<img src='~A'/>" value)))

(defmethod output-fastfind-cell (s slot value (style (eql 'urlstyle)))
  (format-hidden s (stringize slot) (stringize value))
  (when value (format s "<a href='~A'>~A</a>" value (htmlify value))))

(defmethod output-fastfind-cell (s slot value (style (eql 'glyph)))
  (format-hidden s (stringize slot) (stringize value))
  (output-handle s value))

(defmethod output-fastfind-cell (s slot value (style (eql 'prettystyle)))
  (format-hidden s (stringize slot) (stringize value))
  (output-simple s value))

(defmethod output-fastfind-cell (s slot value style)
  (declare (ignore style))
  (format-hidden s (stringize slot) (stringize value)))

(defun output-fastfind-display (s class)
  (declare (ignore class))
  (format s "<input type='button' name='Command' value='Display'
                    onClick='postDisplay(this.form)'/>"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun fastfindscript ()
  "<script type='text/javascript'>

var http_slot = false;
var http_old = false;

function fastselect (slot,value)
 {http_slot = slot;
  postSelect('fastoldselect?','Slot=' + slot + '&Value=' + value)}

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
          cell.innerHTML = http_result.responseText}
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
             {document.getElementsByName(http_slot)[0].value=http_old};
          elem.innerHTML = http_result.responseText}
      else {alert('There was a problem with the request in alertResult.')}}}

</script>")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; fastseekpage
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defparameter *fastseekpage* "fastseekpage")

(defmethod process (s (command (eql 'fastseekpage)) postlines)
  (let (dum structure)
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
  (format s "<title>Fastfindpage</title>") (crlf s)
  (format s (fastseekscript)) (crlf s)
  (format s (stylesheet)) (crlf s)
  (format s (modalscript)) (crlf s)
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
    (format s "<form name='form1' action='~A?'>" *fastseekpage*) (crlf s)
    (format s "Find every <b>~A</b> with the following properties."
            (prettyname (cadr structure)))
    (format-hidden s "Oject" (stringize (car structure))) (crlf s)
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
        (output-fastseek-cells s slot values structure style)
        (format s "</td>")
        (when label (format s "<td>~A</td>" label))
        (format s "</tr>")
        (crlf s))
    (format s "</table>") (crlf s)
    (output-fastseek-display s (cadr structure))
    (format s "</form>")
    'done))

(defmethod output-fastseek-cells (s slot values structure (style (eql 'menu)))
  (let (options)
    (setq options (fastoptions slot structure))
    (format s "<table cellpadding='0' cellspacing='0'>")
    (incf *buttons*)
    (format s "<tr><td width='32' valign='top'>")
    (output-empty-button s)
    (output-empty-button s)
    (format s "</td><td>")
    (when options (output-newmenu s slot options values))
    (format s "</td></tr>") (crlf s)
    (format s "</table>")
    (crlf s)))

(defmethod output-fastseek-cells (s slot values structure (style (eql 'selector)))
  (output-fastseek-multicells s slot values structure style))

(defmethod output-fastseek-cells (s slot values structure (style (eql 'fastselector)))
  (output-fastseek-unicells s slot values structure style))

(defmethod output-fastseek-cells (s slot values structure (style (eql 'fancyselector)))
  (output-fastseek-unicells s slot values structure style))

(defmethod output-fastseek-cells (s slot values structure (style (eql 'combobox)))
  (output-fastseek-unicells s slot values structure style))

(defmethod output-fastseek-cells (s slot values structure (style (eql 'checkbox)))
  (let (options)
    (setq options (fastoptions slot structure))
    (format s "<table cellpadding='0' cellspacing='0'>")
    (incf *buttons*)
    (format s "<tr><td width='32' valign='top'>")
    (output-empty-button s)
    (output-empty-button s)
    (format s "</td><td>")
    (when options (output-newcheckbox s slot options values))
    (format s "</td></tr>") (crlf s)
    (format s "</table>")
    (crlf s)))

(defmethod output-fastseek-cells (s slot values structure (style (eql 'radiobutton)))
  (output-fastseek-unicells s slot values structure style))

(defmethod output-fastseek-cells (s slot values structure (style (eql 'typein)))
  (output-fastseek-multicells s slot values structure style))

(defmethod output-fastseek-cells (s slot values structure (style (eql 'stringfield)))
  (output-fastseek-multicells s slot values structure style))

(defmethod output-fastseek-cells (s slot values structure (style (eql 'text)))
  (output-fastseek-multicells s slot values structure style))

(defmethod output-fastseek-cells (s slot values structure (style (eql 'textarea)))
  (output-fastseek-multicells s slot values structure style))

(defmethod output-fastseek-cells (s slot values structure (style (eql 'password)))
  (output-fastseek-multicells s slot values structure style))

(defmethod output-fastseek-cells (s slot values structure (style (eql 'dateinput)))
  (output-fastseek-multicells s slot values structure style))

(defmethod output-fastseek-cells (s slot values structure (style (eql 'datestyle)))
  (output-fastseek-unicells s slot values structure style))

(defmethod output-fastseek-cells (s slot values structure (style (eql 'dollarinput)))
  (output-fastseek-multicells s slot values structure style))

(defmethod output-fastseek-cells (s slot values structure (style (eql 'dollarstyle)))
  (output-fastseek-unicells s slot values structure style))

(defmethod output-fastseek-cells (s slot values structure (style (eql 'emailstyle)))
  (output-fastseek-unicells s slot values structure style))

(defmethod output-fastseek-cells (s slot values structure (style (eql 'htmlstyle)))
  (output-fastseek-unicells s slot values structure style))

(defmethod output-fastseek-cells (s slot values structure (style (eql 'imagestyle)))
  (output-fastseek-unicells s slot values structure style))

(defmethod output-fastseek-cells (s slot values structure (style (eql 'urlstyle)))
  (output-fastseek-unicells s slot values structure style))

(defmethod output-fastseek-cells (s slot values structure (style (eql 'glyph)))
  (output-fastseek-unicells s slot values structure style))

(defmethod output-fastseek-cells (s slot values structure (style (eql 'prettystyle)))
  (output-fastseek-unicells s slot values structure style))

(defmethod output-fastseek-cells (s slot values structure style)
  (output-fastseek-unicells s slot values structure style))

(defmethod output-fastseek-unicells (s slot values structure style)
  (format s "<table cellpadding='0' cellspacing='0'>")
  (incf *buttons*)
  (format s "<tr><td width='32' valign='top'>")
  (output-empty-button s)
  (output-empty-button s)
  (format s "</td><td>")
  (output-fastseek-cell s slot (car values) structure style) (crlf s)
  (format s "</td></tr>")
  (dolist (value (cdr values))
    (incf *buttons*)
    (format s "<tr><td width='32' valign='top'>")
    (output-empty-button s)
    (output-empty-button s)
    (format s "</td><td>")
    (output-fastseek-cell s slot value structure style) (crlf s)
    (format s "</td></tr>"))
  (format s "</table>")
  (crlf s))

(defun output-fastseek-multicells (s slot values structure style)
  (let (multivalued multiple)
    (setq multivalued (find-multivalued slot))
    (setq multiple (cdr values))
    (format s "<table cellpadding='0' cellspacing='0'>")
    (incf *buttons*)
    (format s "<tr><td width='32' valign='top'>")
    (if multivalued (output-add s *buttons*) (output-empty-button s))
    (if multiple (output-delete s *buttons*) (output-empty-button s))
    (format s "</td><td>")
    (output-fastseek-cell s slot (car values) structure style) (crlf s)
    (format s "</td></tr>")
    (dolist (value (cdr values))
      (incf *buttons*)
      (format s "<tr><td width='32' valign='top'>")
      (if multivalued (output-add s *buttons*) (output-empty-button s))
      (if multiple (output-delete s *buttons*) (output-empty-button s))
      (format s "</td><td>")
      (output-fastseek-cell s slot value structure style) (crlf s)
      (format s "</td></tr>"))
    (format s "</table>")
    (crlf s)))

(defmethod output-fastseek-cell (s slot value structure (style (eql 'menu)))
  (output-fastseek-cell s slot value structure 'selector))

(defmethod output-fastseek-cell (s slot value structure (style (eql 'selector)))
  (let (options)
    (cond ((setq options (fastoptions slot structure))
           (setq options (cons 'unknown options))
           (output-newselector s slot options value))
          (t (format-hidden s (stringize slot) "")))))

(defmethod output-fastseek-cell (s slot value structure (style (eql 'fastselector)))
  (output-weirdfastselector s slot value structure))

(defmethod output-fastseek-cell (s slot value structure (style (eql 'fancyselector)))
 (declare (ignore structure))
  (format s "<div id='~A'>" (stringize slot))
  (output-newfancyselector s slot value)
  (format s "</div>"))

(defmethod output-fastseek-cell (s slot value structure (style (eql 'combobox)))
  (let (options)
    (when (setq options (fastoptions slot structure))
      (setq options (cons 'unknown options))
      (output-newcombobox s slot options (prettyname value) 30))))

(defmethod output-fastseek-cell (s slot value structure (style (eql 'checkbox)))
  (output-fastseek-cell s slot value structure 'radiobutton))

(defmethod output-fastseek-cell (s slot value structure (style (eql 'radiobutton)))
  (let (options)
    (cond ((setq options (fastoptions slot structure))
           (setq options (cons nil options))
           (output-newradiobutton s slot options value))
          (t (format-hidden s (stringize slot) "")))))

(defmethod output-fastseek-cell (s slot value structure (style (eql 'typein)))
  (declare (ignore structure))
  (format-newtypein s (stringize slot) (stringize value) 40))

(defmethod output-fastseek-cell (s slot value structure (style (eql 'stringfield)))
  (declare (ignore structure))
  (format-newstringfield s (stringize slot) (prettyname value) 40))

(defmethod output-fastseek-cell (s slot value structure (style (eql 'text)))
  (output-fastseek-cell s slot value structure 'stringfield))

(defmethod output-fastseek-cell (s slot value structure (style (eql 'textarea)))
  (output-fastseek-cell s slot value structure 'stringfield))

(defmethod output-fastseek-cell (s slot value structure (style (eql 'password)))
  (declare (ignore structure))
  (format-newpassword s (stringize slot) (prettyname value) 40))

(defmethod output-fastseek-cell (s slot value structure (style (eql 'dateinput)))
  (declare (ignore structure))
  (output-newdateinput s slot value))

(defmethod output-fastseek-cell (s slot value structure (style (eql 'datestyle)))
  (declare (ignore structure))
  (format-hidden s (stringize slot) (stringize value))
  (output-simple s value))

(defmethod output-fastseek-cell (s slot value structure (style (eql 'dollarinput)))
  (declare (ignore structure))
  (output-newdollarinput s slot value 6))

(defmethod output-fastseek-cell (s slot value structure (style (eql 'dollarstyle)))
  (declare (ignore structure))
  (format-hidden s (stringize slot) (stringize value))
  (when (realp value) (format s "$~$" value)))

(defmethod output-fastseek-cell (s slot value structure (style (eql 'emailstyle)))
  (declare (ignore structure))
  (format-hidden s (stringize slot) (stringize value))
  (when value (format s "<a href='mailto:~A'><font color='red'>~A</font></a>" value value)))

(defmethod output-fastseek-cell (s slot value structure (style (eql 'htmlstyle)))
  (declare (ignore structure))
  (format-hidden s (stringize slot) (stringize value))
  (when value (format s "~A" value)))

(defmethod output-fastseek-cell (s slot value structure (style (eql 'imagestyle)))
  (declare (ignore structure))
  (format-hidden s (stringize slot) (stringize value))
  (when value (format s "<img src='~A'/>" value)))

(defmethod output-fastseek-cell (s slot value structure (style (eql 'urlstyle)))
  (declare (ignore structure))
  (format-hidden s (stringize slot) (stringize value))
  (format s "<a href='~A'>~A</a>" value (htmlify value)))

(defmethod output-fastseek-cell (s slot value structure (style (eql 'glyph)))
  (declare (ignore structure))
  (format-hidden s (stringize slot) (stringize value))
  (output-handle s value))

(defmethod output-fastseek-cell (s slot value structure (style (eql 'prettystyle)))
  (declare (ignore structure))
  (format-hidden s (stringize slot) (stringize value))
  (output-simple s value))

(defmethod output-fastseek-cell (s slot value structure style)
  (declare (ignore structure style))
  (format-hidden s (stringize slot) (stringize value)))

(defun output-fastseek-display (s class)
  (declare (ignore class))
  (format s "<input type='button' name='Command' value='Display'
                    onClick='postDisplay(this.form)'/>"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun fastseekscript ()
  "<script>

var http_slot = false;
var http_old = false;

function fastselect (slot,value)
 {http_slot = slot;
  postSelect('fastselect?','Slot=' + slot + '&Value=' + value)}

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
;;; fastlookpage
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod process (s (command (eql 'fastlookpage)) postlines)
  (let (dum start end)
    (cond ((setq dum (getf-post "Class" postlines))
           (setq dum (fastcomparestructure (read-user-string dum)))
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
  (format s "<title>Fastlookpage</title>")
  (finish-head s) (crlf s)
  (format-body s *bgcolor*) (crlf s)
  (output-header s)
  (format-border s)
  (process-fastlook s structure start end)
  (finish-border s)
  (output-footer s)
  (finish-body s) (crlf s)
  (finish-html s) (crlf s))

(defun process-fastlook (s structure start end)
  (let (objects sorter attributes results count)
    (setq objects (findinstances (viewconvert structure) *gui*))
    (when (setq sorter (find-sorter (cadr structure)))
      (setq objects (sortem objects sorter 'ascending)))
    (multiple-value-setq (objects count start end) (trim objects start end))
    (setq attributes (displayable-slots (cadr structure)))
    (setq results (prorequest `(ask-table ,objects ,attributes)))
    (output-fastlook s structure objects attributes results count start end)))

(defun output-fastlook (s structure objects slots results count start end)
  (format s "<center>")
  (format s "<br/>")
  (cond ((= count 0)
         (format s "<table>")
         (format s "<tr><td align='center'>There are no answers.</td></tr>")
         (format s "<tr><td>")
         (output-fastlook-create s (cadr structure) structure)
         (format s "</td></tr></table>"))
        ((= count 1)
         (format s "<table>")
         (format s "<tr><td align='center'>There is 1 answer.</td></tr>")
         (format s "<tr><td>")
         (output-fastlook-inner s structure objects slots results)
         (format s "</td></tr>")
         (format s "<tr><td>")
         (output-fastlook-create s (cadr structure) structure)
         (format s "</td></tr></table>"))
        ((and (= start 1) (geqp end count))
         (format s "<table>")
         (format s "<tr><td align='center'>There are ~D answers.</td></tr>" count)
         (format s "<tr><td>")
         (output-fastlook-inner s structure objects slots results)
         (format s "</td></tr>")
         (format s "<tr><td>")
         (output-fastlook-create s (cadr structure) structure)
         (format s "</td></tr></table>"))
        (t (format s "<table>")
           (format s "<tr><td align='center'>There are ~D answers.  The following table shows answers ~A through ~A.</td></tr>"
                   count start end)
           (format s "<tr><td>")
           (output-fastlook-inner s structure objects slots results)
           (format s "</td></tr><tr><td>")
           (multiple-value-setq (start end) (kerchunk count start end))
           (output-fastlook-create s (cadr structure) structure)
           (output-fastlook-display s (cadr structure) structure start end)
           (format s "</td></tr></table>")))
  (format s "</center>") (crlf s))

(defun output-fastlook-inner (s structure items slots results)
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
  (format s "<form action='fastlookpage?' method='post'>")
  (format-hidden s "Structure" (htmlify (prin1-to-string structure)))
  (format-button s "Command" "Display")
  (format s "answers ")
  (format-text s "Start" (princ-to-string start) 5)
  (format s " through ")
  (format-text s "End" (princ-to-string end) 5)
  (format s "</form>") (crlf s))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; fastviewpage
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defparameter *fastviewpage* "fastviewpage")

(defmethod process (s (command (eql 'fastviewpage)) postlines)
  (let (dum start end)
    (cond ((setq dum (getf-post "Class" postlines))
           (setq dum (fastcomparestructure (read-user-string dum)))
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
  (format-head s)
  (format s "<title>Fastviewpage</title>") (crlf s)
  (format s (fastviewscript)) (crlf s)
  (format s (stylesheet)) (crlf s)
  (format s (modalscript)) (crlf s)
  (finish-head s) (crlf s)
  (format-body s *bgcolor*) (crlf s)
  (output-header s)
  (format-border s)
  (process-fastview s structure start end)
  (finish-border s)
  (output-footer s)
  (finish-body s) (crlf s)
  (finish-html s) (crlf s))

(defun process-fastview (s structure start end)
  (let (objects sorter attributes results count)
    (setq objects (findinstances (viewconvert structure) *gui*))
    (when (setq sorter (find-sorter (cadr structure)))
      (setq objects (sortem objects sorter 'ascending)))
    (multiple-value-setq (objects count start end) (trim objects start end))
    (setq attributes (displayable-slots (cadr structure)))
    (setq results (prorequest `(ask-table ,objects ,attributes)))
    (output-fastview s structure objects attributes results count start end)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun output-fastview (s structure objects slots results count start end)
  (format s "<center>")
  (format s "<br/>")
  (cond ((= count 0)
         (format s "<table>")
         (format s "<tr><td align='center'>There are no answers.</td></tr>")
         (unless (emptystructurep structure)
           (format s "<tr><td>")
           (output-fastview-inner s structure objects slots results)
           (format s "</td></tr>"))
         (format s "<tr><td>")
         (output-fastview-create s (cadr structure) structure)
         (format s "</td></tr></table>"))
        ((= count 1)
         (format s "<table>")
         (format s "<tr><td align='center'>There is 1 answer.</td></tr>")
         (format s "<tr><td>")
         (output-fastview-inner s structure objects slots results)
         (format s "</td></tr>")
         (format s "<tr><td>")
         (output-fastview-create s (cadr structure) structure)
         (format s "</td></tr></table>"))
        ((and (= start 1) (geqp end count))
         (format s "<table>")
         (format s "<tr><td align='center'>There are ~D answers.</td></tr>" count)
         (format s "<tr><td>")
         (output-fastview-inner s structure objects slots results)
         (format s "</td></tr>")
         (format s "<tr><td>")
         (output-fastview-create s (cadr structure) structure)
         (format s "</td></tr></table>"))
        (t (format s "<table>")
           (format s "<tr><td align='center'>There are ~D answers.  The following table shows answers ~A through ~A.</td></tr>"
                   count start end)
           (format s "<tr><td>")
           (output-fastview-inner s structure objects slots results)
           (format s "</td></tr><tr><td>")
           (multiple-value-setq (start end) (kerchunk count start end))
           (output-fastview-create s (cadr structure) structure)
           (output-fastview-display s (cadr structure) structure start end)
           (format s "</td></tr></table>")))
  (format s "</center>") (crlf s))

(defun output-fastview-inner (s structure items slots results)
  (let (class nohandle (*buttons* 0))
    (setq class (cadr structure))
    (setq nohandle (findp `(nodisplay ,class handle) *interface*))
    (format s "<form name='form1' action='~A?' method='post'>" *fastviewpage*) (crlf s)
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
        (output-fastview-cells s slot values (find-searchstyle slot))
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

(defmethod output-fastview-cells (s slot values (style (eql 'menu)))
  (let (options)
    (setq options (findalternatives slot))
    (incf *buttons*)
    (when options (output-newmenu s slot options values))
    (crlf s)))

(defmethod output-fastview-cells (s slot values (style (eql 'selector)))
  (output-fastview-multicells s slot values style))

(defmethod output-fastview-cells (s slot values (style (eql 'fastselector)))
  (incf *buttons*)
  (output-fastview-cell s slot (car values) style))

(defmethod output-fastview-cells (s slot values (style (eql 'fancyselector)))
  (incf *buttons*)
  (output-fastview-cell s slot (car values) style))

(defmethod output-fastview-cells (s slot values (style (eql 'combobox)))
  (incf *buttons*)
  (output-fastview-cell s slot (car values) style))

(defmethod output-fastview-cells (s slot values (style (eql 'checkbox)))
  (let (options)
    (setq options (findalternatives slot))
    (incf *buttons*)
    (when options (output-newcheckbox s slot options values))
    (crlf s)))

(defmethod output-fastview-cells (s slot values (style (eql 'radiobutton)))
  (incf *buttons*)
  (output-fastview-cell s slot (car values) style))

(defmethod output-fastview-cells (s slot values (style (eql 'typein)))
  (output-fastview-multicells s slot values style))

(defmethod output-fastview-cells (s slot values (style (eql 'stringfield)))
  (output-fastview-multicells s slot values style))

(defmethod output-fastview-cells (s slot values (style (eql 'text)))
  (output-fastview-multicells s slot values style))

(defmethod output-fastview-cells (s slot values (style (eql 'textarea)))
  (output-fastview-multicells s slot values style))

(defmethod output-fastview-cells (s slot values (style (eql 'password)))
  (output-fastview-multicells s slot values style))

(defmethod output-fastview-cells (s slot values (style (eql 'dateinput)))
  (output-fastview-multicells s slot values style))

(defmethod output-fastview-cells (s slot values (style (eql 'datestyle)))
  (incf *buttons*)
  (output-fastview-cell s slot (car values) style))

(defmethod output-fastview-cells (s slot values (style (eql 'dollarinput)))
  (output-fastview-multicells s slot values style))

(defmethod output-fastview-cells (s slot values (style (eql 'dollarstyle)))
  (incf *buttons*)
  (output-fastview-cell s slot (car values) style))

(defmethod output-fastview-cells (s slot values (style (eql 'emailstyle)))
  (incf *buttons*)
  (output-fastview-cell s slot (car values) style))

(defmethod output-fastview-cells (s slot values (style (eql 'htmlstyle)))
  (incf *buttons*)
  (output-fastview-cell s slot (car values) style))

(defmethod output-fastview-cells (s slot values (style (eql 'imagestyle)))
  (incf *buttons*)
  (output-fastview-cell s slot (car values) style))

(defmethod output-fastview-cells (s slot values (style (eql 'urlstyle)))
  (incf *buttons*)
  (output-fastview-cell s slot (car values) style))

(defmethod output-fastview-cells (s slot values (style (eql 'glyph)))
  (incf *buttons*)
  (output-fastview-cell s slot (car values) style))

(defmethod output-fastview-cells (s slot values (style (eql 'prettystyle)))
  (incf *buttons*)
  (output-fastview-cell s slot (car values) style))

(defmethod output-fastview-cells (s slot values style)
  (incf *buttons*)
  (output-fastview-cell s slot (car values) style))

(defun output-fastview-multicells (s slot values style)
  (let (multivalued multiple)
    (setq multivalued (find-multivalued slot))
    (setq multiple (cdr values))
    (format s "<table cellpadding='0' cellspacing='0'>")
    (incf *buttons*)
    (format s "<tr><td valign='center'>")
    (when multivalued (output-add s *buttons*))
    (when multiple (output-delete s *buttons*))
    (format s "</td><td valign='center'>")
    (output-fastview-cell s slot (car values) style) (crlf s)
    (format s "</td></tr>")
    (dolist (value (cdr values))
      (incf *buttons*)
      (format s "<tr><td valign='center'>")
      (when multivalued (output-add s *buttons*))
      (when multiple (output-delete s *buttons*))
      (format s "</td><td>")
      (output-fastview-cell s slot value style) (crlf s)
      (format s "</td></tr>"))
    (format s "</table>")
    (crlf s)))

(defmethod output-fastview-cell (s slot value (style (eql 'selector)))
  (declare (ignore class))
  (let (options)
    (when (setq options (findalternatives slot))
      (setq options (cons 'unknown options))
      (output-newselector s slot options value))))

(defmethod output-fastview-cell (s slot value (style (eql 'fastselector)))
  (output-newfastselector s slot value))

(defmethod output-fastview-cell (s slot value (style (eql 'fancyselector)))
  (output-newfancyselector s slot value))

(defmethod output-fastview-cell (s slot value (style (eql 'combobox)))
  (let (options)
    (when (setq options (findalternatives slot))
      (setq options (cons 'unknown options))
      (output-newcombobox s slot options (prettyname value) 20))))

(defmethod output-fastview-cell (s slot value (style (eql 'radiobutton)))
  (let (options)
    (cond ((setq options (findalternatives slot))
           (setq options (cons nil options))
           (output-newradiobutton s slot options value))
          (t (format-hidden s (stringize slot) "")))))

(defmethod output-fastview-cell (s slot value (style (eql 'typein)))
  (format-newtypein s (stringize slot) (stringize value) 20))

(defmethod output-fastview-cell (s slot value (style (eql 'stringfield)))
  (format-newstringfield s (stringize slot) (prettyname value) 20))

(defmethod output-fastview-cell (s slot value (style (eql 'text)))
  (output-fastview-cell s slot value 'stringfield))

(defmethod output-fastview-cell (s slot value (style (eql 'textarea)))
  (output-fastview-cell s slot value 'stringfield))

(defmethod output-fastview-cell (s slot value (style (eql 'password)))
  (format-newpassword s (stringize slot) (prettyname value) 20))

(defmethod output-fastview-cell (s slot value (style (eql 'dateinput)))
  (declare (ignore class structure))
  (output-newdateinput s slot value))

(defmethod output-fastview-cell (s slot value (style (eql 'datestyle)))
  (format-hidden s (stringize slot) (stringize value))
  (if value (output-simple s value) (format s "~A" (iconify slot))))

(defmethod output-fastview-cell (s slot value (style (eql 'dollarinput)))
  (output-newdollarinput s slot value 6))

(defmethod output-fastview-cell (s slot value (style (eql 'dollarstyle)))
  (format-hidden s (stringize slot) (stringize value))
  (if (realp value) (format s "$~$" value) (format s "~A" (iconify slot))))

(defmethod output-fastview-cell (s slot value (style (eql 'emailstyle)))
  (format-hidden s (stringize slot) (stringize value))
  (if value (format s "<a href='mailto:~A'><font color='red'>~A</font></a>" value value)
      (format s "~A" (iconify slot))))

(defmethod output-fastview-cell (s slot value (style (eql 'htmlstyle)))
  (format-hidden s (stringize slot) (stringize value))
  (if value (format s "~A" value) (format s "~A" (iconify slot))))

(defmethod output-fastview-cell (s slot value (style (eql 'imagestyle)))
  (format-hidden s (stringize slot) (stringize value))
  (if value (format s "<img src='~A'/>" value) (format s "~A" (iconify slot))))

(defmethod output-fastview-cell (s slot value (style (eql 'urlstyle)))
  (declare (ignore class structure))
  (format-hidden s (stringize slot) (stringize value))
  (if value (format s "<a href='~A'>~A</a>" value (htmlify value))
      (format s "~A" (iconify slot))))

(defmethod output-fastview-cell (s slot value (style (eql 'glyph)))
  (format-hidden s (stringize slot) (stringize value))
  (format s "~A" (iconify slot)))

(defmethod output-fastview-cell (s slot value (style (eql 'prettystyle)))
  (format-hidden s (stringize slot) (stringize value))
  (format s "~A" (iconify slot)))

(defmethod output-fastview-cell (s slot value style)
  (declare (ignore style))
  (format-hidden s (stringize slot) (stringize value))
  (format s "~A" (iconify slot)))

(defun output-fastview-create (s class structure)
  (declare (ignore class))
  (unless (findp `(or (nocommand ,*gui* create) (nocreate ,*gui* ,(cadr structure))) *interface*)
    (format s "<form action='fastcreatepage?' method='post'>" (stringize structure))
    (format-hidden s "Class" (stringize (cadr structure)))
    (format-button s "Command" "Create")
    (format s " a new ~A." (prettify (cadr structure)))
    (format s "</form>") (crlf s)))

(defun output-fastview-display (s class structure start end)
  (declare (ignore class))
  (format s "<form action='fastviewpage?' method='post'>")
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

(defun fastviewscript ()
  "<script>

var http_slot = false;
var http_old = false;

function fastselect (slot,value)
 {http_slot = slot;
  postSelect('fastselect?','Slot=' + slot + '&Value=' + value)}

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
;;; fastshowpage
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defparameter *fastshowpage* "fastshowpage")

(defmethod process (s (command (eql 'fastshowpage)) postlines)
  (let (dum start end)
    (cond ((setq dum (getf-post "Class" postlines))
           (setq dum (fastcomparestructure (read-user-string dum)))
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
  (format-head s) (crlf s)
  (format s "<title>Fastshowpage</title>") (crlf s)
  (format s (fastshowscript)) (crlf s)
  (format s (stylesheet)) (crlf s)
  (format s (modalscript)) (crlf s)
  (finish-head s) (crlf s)
  (format-body s *bgcolor*) (crlf s)
  (output-header s)
  (format-border s)
  (process-fastshow s structure start end)
  (finish-border s)
  (output-footer s)
  (finish-body s) (crlf s)
  (finish-html s) (crlf s))

(defun process-fastshow (s structure start end)
  (let (objects sorter attributes results count)
    (setq objects (findinstances (viewconvert structure) *gui*))
    (when (setq sorter (find-sorter (cadr structure)))
      (setq objects (sortem objects sorter 'ascending)))
    (multiple-value-setq (objects count start end) (trim objects start end))
    (setq attributes (displayable-slots (cadr structure)))
    (setq results (prorequest `(ask-table ,objects ,attributes)))
    (output-fastshow s structure objects attributes results count start end)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun output-fastshow (s structure objects slots results count start end)
  (format s "<center>")
  (format s "<br/>")
  (cond ((= count 0)
         (format s "<table>")
         (format s "<tr><td align='center'>There are no answers.</td></tr>") (crlf s)
         (unless (emptystructurep structure)
           (format s "<tr><td>")
           (output-fastshow-inner s structure objects slots results)
           (format s "</td></tr>"))
         (format s "<tr><td>")
         (output-fastshow-create s (cadr structure) structure)
         (format s "</td></tr></table>"))
        ((= count 1)
         (format s "<table>")
         (format s "<tr><td align='center'>There is 1 answer.</td></tr>")
         (format s "<tr><td>")
         (output-fastshow-inner s structure objects slots results)
         (format s "</td></tr>")
         (format s "<tr><td>")
         (output-fastshow-create s (cadr structure) structure)
         (format s "</td></tr></table>"))
        ((and (= start 1) (geqp end count))
         (format s "<table>")
         (format s "<tr><td align='center'>There are ~D answers." count)
         (format s "<tr><td>")
         (output-fastshow-inner s structure objects slots results)
         (format s "</td></tr>")
         (format s "<tr><td>")
         (output-fastshow-create s (cadr structure) structure)
         (format s "</td></tr></table>"))
        (t (format s "<table>")
           (format s "<tr><td align='center'>There are ~D answers.  The following table shows answers ~A through ~A.</td></tr>"
                   count start end)
           (format s "<tr><td>")
           (output-fastshow-inner s structure objects slots results)
           (format s "</td></tr><tr><td>")
           (multiple-value-setq (start end) (kerchunk count start end))
           (output-fastshow-create s (cadr structure) structure)
           (output-fastshow-display s (cadr structure) structure start end)
           (format s "</td></tr></table>")))
  (format s "</center>") (crlf s))

(defun output-fastshow-inner (s structure items slots results)
  (let (class nohandle (*buttons* 0))
    (setq class (cadr structure))
    (setq nohandle (findp `(nodisplay ,class handle) *interface*))
    (format s "<form name='form1' action='~A?' method='post'>" *fastshowpage*) (crlf s)
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
        (output-fastshow-cells s slot values structure (find-searchstyle slot))
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

(defmethod output-fastshow-cells (s slot values structure (style (eql 'menu)))
  (let (options)
    (setq options (fastoptions slot structure))
    (incf *buttons*)
    (when options (output-newmenu s slot options values))
    (crlf s)))

(defmethod output-fastshow-cells (s slot values structure (style (eql 'selector)))
  (output-fastshow-multicells s slot values structure style))

(defmethod output-fastshow-cells (s slot values structure (style (eql 'fastselector)))
  (incf *buttons*)
  (output-fastshow-cell s slot (car values) structure style))

(defmethod output-fastshow-cells (s slot values structure (style (eql 'fancyselector)))
  (incf *buttons*)
  (output-fastshow-cell s slot (car values) structure style))

(defmethod output-fastshow-cells (s slot values structure (style (eql 'combobox)))
  (incf *buttons*)
  (output-fastshow-cell s slot (car values) structure style))

(defmethod output-fastshow-cells (s slot values structure (style (eql 'checkbox)))
  (let (options)
    (setq options (fastoptions slot structure))
    (incf *buttons*)
    (when options (output-newcheckbox s slot options values))
    (crlf s)))

(defmethod output-fastshow-cells (s slot values structure (style (eql 'radiobutton)))
  (incf *buttons*)
  (output-fastshow-cell s slot (car values) structure style))

(defmethod output-fastshow-cells (s slot values structure (style (eql 'typein)))
  (output-fastshow-multicells s slot values structure style))

(defmethod output-fastshow-cells (s slot values structure (style (eql 'stringfield)))
  (output-fastshow-multicells s slot values structure style))

(defmethod output-fastshow-cells (s slot values structure (style (eql 'text)))
  (output-fastshow-multicells s slot values structure style))

(defmethod output-fastshow-cells (s slot values structure (style (eql 'textarea)))
  (output-fastshow-multicells s slot values structure style))

(defmethod output-fastshow-cells (s slot values structure (style (eql 'password)))
  (output-fastshow-multicells s slot values structure style))

(defmethod output-fastshow-cells (s slot values structure (style (eql 'dateinput)))
  (output-fastshow-multicells s slot values structure style))

(defmethod output-fastshow-cells (s slot values structure (style (eql 'datestyle)))
  (incf *buttons*)
  (output-fastshow-cell s slot (car values) structure style))

(defmethod output-fastshow-cells (s slot values structure (style (eql 'dollarinput)))
  (output-fastshow-multicells s slot values structure style))

(defmethod output-fastshow-cells (s slot values structure (style (eql 'dollarstyle)))
  (incf *buttons*)
  (output-fastshow-cell s slot (car values) structure style))

(defmethod output-fastshow-cells (s slot values structure (style (eql 'emailstyle)))
  (incf *buttons*)
  (output-fastshow-cell s slot (car values) structure style))

(defmethod output-fastshow-cells (s slot values structure (style (eql 'htmlstyle)))
  (incf *buttons*)
  (output-fastshow-cell s slot (car values) structure style))

(defmethod output-fastshow-cells (s slot values structure (style (eql 'imagestyle)))
  (incf *buttons*)
  (output-fastshow-cell s slot (car values) structure style))

(defmethod output-fastshow-cells (s slot values structure (style (eql 'urlstyle)))
  (incf *buttons*)
  (output-fastshow-cell s slot (car values) structure style))

(defmethod output-fastshow-cells (s slot values structure (style (eql 'glyph)))
  (incf *buttons*)
  (output-fastshow-cell s slot (car values) structure style))

(defmethod output-fastshow-cells (s slot values structure (style (eql 'prettystyle)))
  (incf *buttons*)
  (output-fastshow-cell s slot (car values) structure style))

(defmethod output-fastshow-cells (s slot values structure style)
  (incf *buttons*)
  (output-fastshow-cell s slot (car values) structure style))

(defun output-fastshow-multicells (s slot values structure style)
  (let (multivalued multiple)
    (setq multivalued (find-multivalued slot))
    (setq multiple (cdr values))
    (format s "<table cellpadding='0' cellspacing='0'>")
    (incf *buttons*)
    (format s "<tr><td valign='center'>")
    (when multivalued (output-add s *buttons*))
    (when multiple (output-delete s *buttons*))
    (format s "</td><td valign='center'>")
    (output-fastshow-cell s slot (car values) structure style) (crlf s)
    (format s "</td></tr>")
    (dolist (value (cdr values))
      (incf *buttons*)
      (format s "<tr><td valign='center'>")
      (when multivalued (output-add s *buttons*))
      (when multiple (output-delete s *buttons*))
      (format s "</td><td>")
      (output-fastshow-cell s slot value structure style) (crlf s)
      (format s "</td></tr>"))
    (format s "</table>")
    (crlf s)))

(defmethod output-fastshow-cell (s slot value structure (style (eql 'selector)))
  (let (options)
    (setq options (cons 'unknown (fastoptions slot structure)))
    (output-newselector s slot options value)))

(defmethod output-fastshow-cell (s slot value structure (style (eql 'fastselector)))
  (output-weirdfastselector s slot value structure))

(defmethod output-fastshow-cell (s slot value structure (style (eql 'fancyselector)))
  ;(output-newfancyselector s slot value)
  (output-weirdfastselector s slot value structure))

(defmethod output-fastshow-cell (s slot value structure (style (eql 'combobox)))
  (let (options)
    (when (setq options (fastoptions slot structure))
      (setq options (cons 'unknown options))
      (output-newcombobox s slot options (prettyname value) 30))))

(defmethod output-fastshow-cell (s slot value structure (style (eql 'radiobutton)))
  (let (options)
    (cond ((setq options (fastoptions slot structure))
           (setq options (cons nil options))
           (output-newradiobutton s slot options value))
          (t (format-hidden s (stringize slot) "")))))

(defmethod output-fastshow-cell (s slot value structure (style (eql 'typein)))
  (declare (ignore structure))
  (format-newtypein s (stringize slot) (stringize value) 20))

(defmethod output-fastshow-cell (s slot value structure (style (eql 'stringfield)))
  (declare (ignore structure))
  (format-newstringfield s (stringize slot) (prettyname value) 20))

(defmethod output-fastshow-cell (s slot value structure (style (eql 'text)))
  (output-fastshow-cell s slot value structure 'stringfield))

(defmethod output-fastshow-cell (s slot value structure (style (eql 'textarea)))
  (output-fastshow-cell s slot value structure 'stringfield))

(defmethod output-fastshow-cell (s slot value structure (style (eql 'password)))
  (declare (ignore structure))
  (format-newpassword s (stringize slot) (prettyname value) 20))

(defmethod output-fastshow-cell (s slot value structure (style (eql 'dateinput)))
  (declare (ignore structure))
  (output-newdateinput s slot value))

(defmethod output-fastshow-cell (s slot value structure (style (eql 'datestyle)))
  (declare (ignore structure))
  (format-hidden s (stringize slot) (stringize value))
  (if value (output-simple s value) (format s "~A" (iconify slot))))

(defmethod output-fastshow-cell (s slot value structure (style (eql 'dollarinput)))
  (declare (ignore structure))
  (output-newdollarinput s slot value 6))

(defmethod output-fastshow-cell (s slot value structure (style (eql 'dollarstyle)))
  (declare (ignore structure))
  (format-hidden s (stringize slot) (stringize value))
  (if (realp value) (format s "$~$" value) (format s "~A" (iconify slot))))

(defmethod output-fastshow-cell (s slot value structure (style (eql 'emailstyle)))
  (declare (ignore structure))
  (format-hidden s (stringize slot) (stringize value))
  (if value (format s "<a href='mailto:~A'><font color='red'>~A</font></a>" value value)
      (format s "~A" (iconify slot))))

(defmethod output-fastshow-cell (s slot value structure (style (eql 'htmlstyle)))
  (declare (ignore structure))
  (format-hidden s (stringize slot) (stringize value))
  (if value (format s "~A" value) (format s "~A" (iconify slot))))

(defmethod output-fastshow-cell (s slot value structure (style (eql 'imagestyle)))
  (declare (ignore structure))
  (format-hidden s (stringize slot) (stringize value))
  (if value (format s "<img src='~A'/>" value) (format s "~A" (iconify slot))))

(defmethod output-fastshow-cell (s slot value structure (style (eql 'urlstyle)))
  (declare (ignore structure))
  (format-hidden s (stringize slot) (stringize value))
  (if value (format s "<a href='~A'>~A</a>" value (htmlify value)) (format s "~A" (iconify slot))))

(defmethod output-fastshow-cell (s slot value structure (style (eql 'glyph)))
  (declare (ignore structure))
  (format-hidden s (stringize slot) (stringize value))
  (format s "~A" (iconify slot)))

(defmethod output-fastshow-cell (s slot value structure (style (eql 'prettystyle)))
  (declare (ignore structure))
  (format-hidden s (stringize slot) (stringize value))
  (format s "~A" (iconify slot)))

(defmethod output-fastshow-cell (s slot value structure style)
  (declare (ignore structure style))
  (format-hidden s (stringize slot) (stringize value))
  (format s "~A" (iconify slot)))

(defmethod output-fastshow-create (s class structure)
  (declare (ignore structure))
  (unless (findp `(or (nocommand ,*gui* create) (nocreate ,*gui* ,class)) *interface*)
    (format s "<form action='fastcreatepage?' method='post'>")
    (format-hidden s "Class" (stringize class))
    (format-button s "Command" "Create")
    (format s " a new ~A." (prettify class))
    (format s "</form>") (crlf s)))

(defmethod output-fastshow-display (s class structure start end)
  (declare (ignore class))
  (format s "<form action='fastshowpage?' method='post'>")
  (format-hidden s "Structure" (htmlify (prin1-to-string structure)))
  (format-button s "Command" "Display")
  (format s "answers ")
  (format-text s "Start" (princ-to-string start) 5)
  (format s " through ")
  (format-text s "End" (princ-to-string end) 5)
  (format s "</form>") (crlf s))

(defmethod fastoptions (slot structure)
  (cond ((results 'option slot *interface*))
        ((sort (findoptions slot (viewconvert structure) *gui*) #'lessp))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; fastshowscript
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun fastshowscript ()
  "<script>

var http_slot = false;
var http_old = false;

function fastselect (slot,value)
 {http_slot = slot;
  postSelect('fastselect?','Slot=' + slot + '&Value=' + value)}

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
;;; fastdisplaypage
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; fastgallerypage
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defparameter *fastgallerypage* "fastgallerypage")

(defmethod process (s (command (eql 'fastgallerypage)) postlines)
  (let (dum start end)
    (cond ((setq dum (getf-post "Class" postlines))
           (setq dum (fastcomparestructure (read-user-string dum)))
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
  (format s "<title>Fastgallerypage</title>") (crlf s)
  (format s (fastgalleryscript)) (crlf s)
  (format s (stylesheet)) (crlf s)
  (format s (modalscript)) (crlf s)
  (finish-head s) (crlf s)
  (format-body s *bgcolor*) (crlf s)
  (output-header s)
  (format-border s)
  (format s "<table width='100%' cellpadding='0' cellspacing='0' bgcolor='#EEEEEE'>")
  (format s "<tr><td height='80' align='center' valign='bottom'>")
  (format s "<p>Select desired features.  Click on images for more information.  Click <span style='cursor:pointer; font-weight:bold; text-decoration:underline;' onClick='location.reload(true)'>here</span> to start over.</p>")
  (output-fastgallery-structure s structure)
  (format s "</td></tr>")
  (format s "<tr><td height='2' bgcolor='#006699' nowrap><img src='~A/images/blank.gif' width='1' height='2'></td></tr>" *home*)
  (format s "</table>")
  (process-fastgallery s structure start end)
  (finish-border s)
  (output-footer s)
  (finish-body s) (crlf s)
  (finish-html s) (crlf s)
  'done)

(defun process-fastgallery (s structure start end)
  (let (objects sorter count)
    (setq objects (findinstances (viewconvert structure) *gui*))
    (when (setq sorter (find-sorter (cadr structure)))
      (setq objects (sortem objects sorter 'ascending)))
    (multiple-value-setq (objects count start end) (trim objects start end))
    (output-gallery s objects (cadr structure) count start end)
    (multiple-value-setq (start end) (kerchunk count start end))
    (output-fastgallery-display s (cadr structure) structure start end)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun output-fastgallery-structure (s structure)
  (format s "<form name='form1' action='~A?'>" *fastgallerypage*) (crlf s)
  (format-hidden s "Object" (stringize (car structure)))
  (format-hidden s "Class" (stringize (cadr structure)))
  (format s "<table cellpadding='0' cellspacing='2'>")
  (format s "<tr>")
  (do ((l (cddr structure)) (slot) (values))
      ((null l))
      (setq slot (caar l))
      (multiple-value-setq (values l) (collectentries slot l))
      (format s "<th>")
      (output-fastgallery-cells s slot values structure (find-searchstyle slot))
      (format s "</th>")
      (crlf s))
  (format s "</tr>")
  (format s "</table>")
  (format s "</form>"))

(defmethod output-fastgallery-cells (s slot values structure (style (eql 'menu)))
  (let (options)
    (setq options (fastoptions slot structure))
    (incf *buttons*)
    (when options (output-newmenu s slot options values))
    (crlf s)))

(defmethod output-fastgallery-cells (s slot values structure (style (eql 'selector)))
  (output-fastgallery-multicells s slot values structure style))

(defmethod output-fastgallery-cells (s slot values structure (style (eql 'fastselector)))
  (incf *buttons*)
  (output-fastgallery-cell s slot (car values) structure style))

(defmethod output-fastgallery-cells (s slot values structure (style (eql 'fancyselector)))
  (incf *buttons*)
  (output-fastgallery-cell s slot (car values) structure style))

(defmethod output-fastgallery-cells (s slot values structure (style (eql 'combobox)))
  (incf *buttons*)
  (output-fastgallery-cell s slot (car values) structure style))

(defmethod output-fastgallery-cells (s slot values structure (style (eql 'checkbox)))
  (let (options)
    (setq options (fastoptions slot structure))
    (incf *buttons*)
    (when options (output-newcheckbox s slot options values))
    (crlf s)))

(defmethod output-fastgallery-cells (s slot values structure (style (eql 'radiobutton)))
  (incf *buttons*)
  (output-fastgallery-cell s slot (car values) structure style))

(defmethod output-fastgallery-cells (s slot values structure (style (eql 'typein)))
  (output-fastgallery-multicells s slot values structure style))

(defmethod output-fastgallery-cells (s slot values structure (style (eql 'stringfield)))
  (output-fastgallery-multicells s slot values structure style))

(defmethod output-fastgallery-cells (s slot values structure (style (eql 'text)))
  (output-fastgallery-multicells s slot values structure style))

(defmethod output-fastgallery-cells (s slot values structure (style (eql 'textarea)))
  (output-fastgallery-multicells s slot values structure style))

(defmethod output-fastgallery-cells (s slot values structure (style (eql 'password)))
  (output-fastgallery-multicells s slot values structure style))

(defmethod output-fastgallery-cells (s slot values structure (style (eql 'dateinput)))
  (output-fastgallery-multicells s slot values structure style))

(defmethod output-fastgallery-cells (s slot values structure (style (eql 'datestyle)))
  (incf *buttons*)
  (output-fastgallery-cell s slot (car values) structure style))

(defmethod output-fastgallery-cells (s slot values structure (style (eql 'dollarinput)))
  (output-fastgallery-multicells s slot values structure style))

(defmethod output-fastgallery-cells (s slot values structure (style (eql 'dollarstyle)))
  (incf *buttons*)
  (output-fastgallery-cell s slot (car values) structure style))

(defmethod output-fastgallery-cells (s slot values structure (style (eql 'emailstyle)))
  (incf *buttons*)
  (output-fastgallery-cell s slot (car values) structure style))

(defmethod output-fastgallery-cells (s slot values structure (style (eql 'htmlstyle)))
  (incf *buttons*)
  (output-fastgallery-cell s slot (car values) structure style))

(defmethod output-fastgallery-cells (s slot values structure (style (eql 'imagestyle)))
  (incf *buttons*)
  (output-fastgallery-cell s slot (car values) structure style))

(defmethod output-fastgallery-cells (s slot values structure (style (eql 'urlstyle)))
  (incf *buttons*)
  (output-fastgallery-cell s slot (car values) structure style))

(defmethod output-fastgallery-cells (s slot values structure (style (eql 'glyph)))
  (incf *buttons*)
  (output-fastgallery-cell s slot (car values) structure style))

(defmethod output-fastgallery-cells (s slot values structure (style (eql 'prettystyle)))
  (incf *buttons*)
  (output-fastgallery-cell s slot (car values) structure style))

(defmethod output-fastgallery-cells (s slot values structure style)
  (incf *buttons*)
  (output-fastgallery-cell s slot (car values) structure style))

(defun output-fastgallery-multicells (s slot values structure style)
  (let (multivalued multiple)
    (setq multivalued (find-multivalued slot))
    (setq multiple (cdr values))
    (format s "<table cellpadding='0' cellspacing='0'>")
    (incf *buttons*)
    (format s "<tr><td valign='center'>")
    (when multivalued (output-add s *buttons*))
    (when multiple (output-delete s *buttons*))
    (format s "</td><td valign='center'>")
    (output-fastgallery-cell s slot (car values) structure style) (crlf s)
    (format s "</td></tr>")
    (dolist (value (cdr values))
      (incf *buttons*)
      (format s "<tr><td valign='center'>")
      (when multivalued (output-add s *buttons*))
      (when multiple (output-delete s *buttons*))
      (format s "</td><td>")
      (output-fastgallery-cell s slot value structure style) (crlf s)
      (format s "</td></tr>"))
    (format s "</table>")
    (crlf s)))

(defmethod output-fastgallery-cell (s slot value structure (style (eql 'selector)))
  (let (options)
    (setq options (cons 'unknown (fastoptions slot structure)))
    (output-newselector s slot options value)))

(defmethod output-fastgallery-cell (s slot value structure (style (eql 'fastselector)))
  (output-weirdfastselector s slot value structure))

(defmethod output-fastgallery-cell (s slot value structure (style (eql 'fancyselector)))
  ;(output-newfancyselector s slot value)
  (output-weirdfastselector s slot value structure))

(defmethod output-fastgallery-cell (s slot value structure (style (eql 'combobox)))
  (let (options)
    (when (setq options (fastoptions slot structure))
      (setq options (cons 'unknown options))
      (output-newcombobox s slot options (prettyname value) 30))))

(defmethod output-fastgallery-cell (s slot value structure (style (eql 'radiobutton)))
  (let (options)
    (cond ((setq options (fastoptions slot structure))
           (setq options (cons nil options))
           (output-newradiobutton s slot options value))
          (t (format-hidden s (stringize slot) "")))))

(defmethod output-fastgallery-cell (s slot value structure (style (eql 'typein)))
  (declare (ignore structure))
  (format-newtypein s (stringize slot) (stringize value) 20))

(defmethod output-fastgallery-cell (s slot value structure (style (eql 'stringfield)))
  (declare (ignore structure))
  (format-newstringfield s (stringize slot) (prettyname value) 20))

(defmethod output-fastgallery-cell (s slot value structure (style (eql 'text)))
  (output-fastgallery-cell s slot value structure 'stringfield))

(defmethod output-fastgallery-cell (s slot value structure (style (eql 'textarea)))
  (output-fastgallery-cell s slot value structure 'stringfield))

(defmethod output-fastgallery-cell (s slot value structure (style (eql 'password)))
  (declare (ignore structure))
  (format-newpassword s (stringize slot) (prettyname value) 20))

(defmethod output-fastgallery-cell (s slot value structure (style (eql 'dateinput)))
  (declare (ignore structure))
  (output-newdateinput s slot value))

(defmethod output-fastgallery-cell (s slot value structure (style (eql 'datestyle)))
  (declare (ignore structure))
  (format-hidden s (stringize slot) (stringize value))
  (when value (output-simple s value)))

(defmethod output-fastgallery-cell (s slot value structure (style (eql 'dollarinput)))
  (declare (ignore structure))
  (output-newdollarinput s slot value 6))

(defmethod output-fastgallery-cell (s slot value structure (style (eql 'dollarstyle)))
  (declare (ignore structure))
  (format-hidden s (stringize slot) (stringize value))
  (when (realp value) (format s "$~$" value)))

(defmethod output-fastgallery-cell (s slot value structure (style (eql 'emailstyle)))
  (declare (ignore structure))
  (format-hidden s (stringize slot) (stringize value))
  (when value (format s "<a href='mailto:~A'><font color='red'>~A</font></a>" value value)))

(defmethod output-fastgallery-cell (s slot value structure (style (eql 'htmlstyle)))
  (declare (ignore structure))
  (format-hidden s (stringize slot) (stringize value))
  (when value (format s "~A" value)))

(defmethod output-fastgallery-cell (s slot value structure (style (eql 'imagestyle)))
  (declare (ignore structure))
  (format-hidden s (stringize slot) (stringize value))
  (when value (format s "<img src='~A'/>" value)))

(defmethod output-fastgallery-cell (s slot value structure (style (eql 'urlstyle)))
  (declare (ignore structure))
  (format-hidden s (stringize slot) (stringize value))
  (when value (format s "<a href='~A'>~A</a>" value (htmlify value))))

(defmethod output-fastgallery-cell (s slot value structure (style (eql 'glyph)))
  (declare (ignore structure))
  (format-hidden s (stringize slot) (stringize value)))

(defmethod output-fastgallery-cell (s slot value structure (style (eql 'prettystyle)))
  (declare (ignore structure))
  (format-hidden s (stringize slot) (stringize value)))

(defmethod output-fastgallery-cell (s slot value structure style)
  (declare (ignore structure style))
  (format-hidden s (stringize slot) (stringize value)))

(defmethod output-fastgallery-create (s class structure)
  (declare (ignore class))
  (unless (findp `(or (nocommand ,*gui* create) (nocreate ,*gui* ,(cadr structure))) *interface*)
    (format s "<form action='fastcreatepage?' method='post'>" (stringize structure))
    (format-hidden s "Class" (stringize (cadr structure)))
    (format-button s "Command" "Create")
    (format s " a new ~A." (prettify (cadr structure)))
    (format s "</form>") (crlf s)))

(defmethod output-fastgallery-display (s class structure start end)
  (declare (ignore class))
  (format s "<form action='fastgallerypage?' method='post'>")
  (format-hidden s "Structure" (htmlify (prin1-to-string structure)))
  (format-button s "Command" "Display")
  (format s "answers ")
  (format-text s "Start" (princ-to-string start) 5)
  (format s " through ")
  (format-text s "End" (princ-to-string end) 5)
  (format s "</form>") (crlf s))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod output-gallery (s items class count start end)
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
  (format s "<table width='90%' cellpadding='4' border='0'>")
  (do ((l items) (count start))
      ((or (null l) (> count end)))
      (format s "<tr valign='top'>")
      (do ((i 1 (1+ i)))
          ((> i 4))
          (format s "<td width='25%'>")
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
    (format s "<a href='fastinspectpage?object=~A'>~A</a>" (stringize item) thumb)
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
  (let (thumb)
    (cond ((setq thumb (result 'thumb item *gui*))
           (stringappend "<img src='" thumb "' height='120'>"))
          (t "<div height='120' width='160'>&nbsp;</div>"))))

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

(defun fastgalleryscript ()
  "<script>

var http_slot = false;
var http_old = false;

function fastselect (slot,value)
 {http_slot = slot;
  postSelect('fastselect?','Slot=' + slot + '&Value=' + value)}

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
;;; fastinspectpage
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod process (s (command (eql 'fastinspectpage)) postlines)
  (let (object class)
    (cond ((null postlines) (http-problem s "Bad request."))
          (t (setq object (read-value-string (cdr (pop postlines))))
             (setq class (classify object *gui*))
             (process-fastinspectpage s object class)))))

(defmethod process-fastinspectpage (s object class)
  (format-html s) (crlf s)
  (output-head s "Fastinspectpage") (crlf s)
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

(defmethod output-cells (s slot values style)
  (format s "<table cellpadding='0' cellspacing='0'>")
  (format s "<tr><td>")
  (output-cell s slot (car values) style) (crlf s)
  (format s "</td></tr>")
  (dolist (value (cdr values))
    (format s "<tr><td>")
    (output-cell s slot value style) (crlf s)
    (format s "</td></tr>"))
  (format s "</table>"))

(defmethod output-cells (s slot values (style (eql 'menu)))
  (let (options)
    (when (setq options (findalternatives slot))
      (output-dismenu s slot options values))))

(defmethod output-cells (s slot values (style (eql 'checkbox)))
  (let (options)
    (when (setq options (findalternatives slot))
      (output-discheckbox s slot options values))))

(defmethod output-cells (s slot values (style (eql 'datestyle)))
  (when values (output-cell s slot (car values) style))
  (dolist (value (cdr values))
    (format s ", ")
    (output-cell s slot value style)))

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

(defmethod output-cells (s slot values (style (eql 'urlstyle)))
  (when values (output-cell s slot (car values) style))
  (dolist (value (cdr values))
    (format s ", ")
    (output-cell s slot value style)))

(defmethod output-cells (s slot values (style (eql 'glyph)))
  (when values (output-cell s slot (car values) style))
  (dolist (value (cdr values))
    (format s ", ")
    (output-cell s slot value style)))

(defmethod output-cells (s slot values (style (eql 'prettystyle)))
  (when values (output-cell s slot (car values) style))
  (dolist (value (cdr values))
    (format s ", ")
    (output-cell s slot value style)))

(defmethod output-cell (s slot value (style (eql 'selector)))
  (declare (ignore slot))
  (output-disselector s value))

(defmethod output-cell (s slot value (style (eql 'fastselector)))
  (declare (ignore slot))
  (output-disselector s value))

(defmethod output-cell (s slot value (style (eql 'fancyselector)))
  (declare (ignore slot))
  (format-distypein s (stringize value) 30))

(defmethod output-cell (s slot value (style (eql 'combobox)))
  (declare (ignore slot))
  (format-distypein s (stringize value) 30))

(defmethod output-cell (s slot value (style (eql 'radiobutton)))
  (let (options)
    (when (setq options (findalternatives slot))
      (setq options (cons 'unknown options))
      (output-disradiobutton s slot options value))))

(defmethod output-cell (s slot value (style (eql 'typein)))
  (declare (ignore slot))
  (format-distypein s (stringize value) 40))

(defmethod output-cell (s slot value (style (eql 'stringfield)))
  (declare (ignore slot))
  (format-distypein s (prettyname value) 40))

(defmethod output-cell (s slot value (style (eql 'text)))
  (declare (ignore slot))
  (format-distypein s (prettyname value) 40))

(defmethod output-cell (s slot value (style (eql 'textarea)))
  (declare (ignore slot))
  (format-distypein s (prettyname value) 40))

(defmethod output-cell (s slot value (style (eql 'password)))
  (declare (ignore slot))
  (format-dispassword s (prettyname value) 40))

(defmethod output-cell (s slot value (style (eql 'dateinput)))
  (output-newdateinput s slot value))

(defmethod output-cell (s slot value (style (eql 'datestyle)))
  (declare (ignore slot))
  (output-simple s value))

(defmethod output-cell (s slot value (style (eql 'dollarinput)))
  (output-newdollarinput s slot value 6))

(defmethod output-cell (s slot value (style (eql 'dollarstyle)))
  (declare (ignore slot))
  (when (realp value) (format s "$~$" value)))

(defmethod output-cell (s slot value (style (eql 'emailstyle)))
  (declare (ignore slot))
  (format s "<a href='mailto:~A'><font color='red'>~A</font></A>" value value))

(defmethod output-cell (s slot value (style (eql 'htmlstyle)))
  (declare (ignore slot))
  (format s "~A" value))

(defmethod output-cell (s slot value (style (eql 'imagestyle)))
  (declare (ignore slot))
  (format s "<img src='~A'/>" value))

(defmethod output-cell (s slot value (style (eql 'urlstyle)))
  (declare (ignore slot))
  (format s "<a href='~A'><font color='red'>~A</font></A>" value value))

(defmethod output-cell (s slot value (style (eql 'glyph)))
  (declare (ignore slot))
  (cond ((simplep value) (output-simple s value))
        (t (output-anchor s value))))

(defmethod output-cell (s slot value (style (eql 'prettystyle)))
  (declare (ignore slot))
  (output-simple s value))

(defmethod output-cell (s slot value style)
  (declare (ignore style))
  (output-cell s slot value 'glyph))

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
;;; fastcreatepage
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defparameter *fastcreatepage* "fastcreatepage")

(defmethod process (s (command (eql 'fastcreatepage)) postlines)
  (let (dum structure)
    (cond ((null postlines) (http-problem s "Bad request."))
          ((getf-post "Class" postlines)
           (setq dum (read-value-string (cdr (pop postlines))))
           (process-fastcreatepage s dum))
          ((string-equal (getf-post "Command" postlines) "Refresh")
           (setq structure (read-user-string (cdr (pop postlines))))
           (output-fastcreatepage s structure))
          ((string-equal (getf-post "Command" postlines) "Record")
           (setq structure (read-user-string (cdr (pop postlines))))
           (process-fastcreate-record s (car structure) (cadr structure) (cddr structure)))
          (t (http-problem s "Bad request.")))))

(defmethod process-fastcreatepage (s class)
  (output-fastcreatepage s (createitem (newinstance class) class)))

(defmethod process-fastcreate-record (s object class constraints)
  (let (delta result)
    (cond ((setq result (checkcreation object class constraints))
           (http-problems s result))
          ((setq delta (differentiator (list* object class constraints)))
           (setq result (prorequest (cons 'update delta)))
           (cond ((errorp result) (http-problems s result))
                 (t (output-fastcreate-success s object)))))))

(defmethod checkcreation (object class constraints)
  (declare (ignore class constraints))
  (let (errors)
    (unless (eq (classify object *gui*) 'thing)
      (setq errors (cons "Object already exists." errors)))
    (when errors
      (setq errors (cons "Press the Back button, correct the error(s), and resubmit." errors)))
    (nreverse errors)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun output-fastcreatepage (s structure)
  (format-html s) (crlf s)
  (format s "<head>")
  (format s "<title>Create ~A</title>" (prettify (cadr structure))) (crlf s)
  (format s (fastcreatescript)) (crlf s)
  (format s (stylesheet)) (crlf s)
  (format s (modalscript)) (crlf s)
  (format s "</head>") (crlf s)
  (format-body s *bgcolor*) (crlf s)
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
    (format s "<form name='form1' action='~A?'>" *fastcreatepage*) (crlf s)
    (format s "Create ")
    (cond ((triplep 'noupdate (cadr structure) 'handle *interface*)
           (format-hidden s "Object" (stringize (car structure)))
           (format s "a new <b>~A</b>." (prettyname (cadr structure))))
          (t (format-text s "Object" (stringize (car structure)) 30)
             (format s " as ~A <b>~A</b>." (article (cadr structure)) (prettyname (cadr structure)))))
    (format-hidden s "Class" (stringize (cadr structure)))
    (format s "<br/>")
    (format s "<table cellspacing='3'>")
    (do ((l (cddr structure)) (slot) (values) (style) (label))
        ((null l))
        (setq slot (caar l))
        (setq style (find-createstyle slot))
        (setq label (find-createlabel slot))
        (multiple-value-setq (values l) (collectentries slot l))
        (format s "<tr><th align='left' valign='top'>")
        (output-slotlink s slot)
        (format s "</th><td>")
        (output-fastcreate-cells s slot values style)
        (format s "</td>")
        (when label (format s "<td>~A</td>" label))
        (format s "</tr>")
        (crlf s))
    (format s "</table>") (crlf s)
    (output-fastcreate-create s (cadr structure)) (crlf s)
    (format s "</form>")))

(defmethod output-fastcreate-cells (s slot values (style (eql 'menu)))
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

(defmethod output-fastcreate-cells (s slot values (style (eql 'selector)))
  (output-fastcreate-multicells s slot values style))

(defmethod output-fastcreate-cells (s slot values (style (eql 'fastselector)))
  (output-fastcreate-unicells s slot values style))

(defmethod output-fastcreate-cells (s slot values (style (eql 'fancyselector)))
  (output-fastcreate-unicells s slot values style))

(defmethod output-fastcreate-cells (s slot values (style (eql 'combobox)))
  (output-fastcreate-unicells s slot values style))

(defmethod output-fastcreate-cells (s slot values (style (eql 'checkbox)))
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

(defmethod output-fastcreate-cells (s slot values (style (eql 'radiobutton)))
  (output-fastcreate-unicells s slot values style))

(defmethod output-fastcreate-cells (s slot values (style (eql 'typein)))
  (output-fastcreate-multicells s slot values style))

(defmethod output-fastcreate-cells (s slot values (style (eql 'stringfield)))
  (output-fastcreate-multicells s slot values style))

(defmethod output-fastcreate-cells (s slot values (style (eql 'text)))
  (output-fastcreate-multicells s slot values style))

(defmethod output-fastcreate-cells (s slot values (style (eql 'textarea)))
  (output-fastcreate-multicells s slot values style))

(defmethod output-fastcreate-cells (s slot values (style (eql 'password)))
  (output-fastcreate-multicells s slot values style))

(defmethod output-fastcreate-cells (s slot values (style (eql 'dollarinput)))
  (output-fastcreate-multicells s slot values style))

(defmethod output-fastcreate-cells (s slot values style)
  (output-fastcreate-unicells s slot values style))

(defmethod output-fastcreate-cell (s slot value (style (eql 'selector)))
  (output-oldselector s slot (cons 'unknown (findalternatives slot)) value))

(defmethod output-fastcreate-cell (s slot value (style (eql 'fastselector)))
  (output-oldfastselector s slot value))

(defmethod output-fastcreate-cell (s slot value (style (eql 'fancyselector)))
  (output-oldfancyselector s slot value))

(defmethod output-fastcreate-cell (s slot value (style (eql 'combobox)))
  (let (options)
    (when (setq options (findalternatives slot))
      (setq options (cons 'unknown options))
      (output-oldcombobox s slot options (prettyname value) 40))))

(defmethod output-fastcreate-cell (s slot value (style (eql 'radiobutton)))
  (let (options)
    (when (setq options (findalternatives slot))
      (setq options (cons nil options))
      (output-oldradiobutton s slot options value))))

(defmethod output-fastcreate-cell (s slot value (style (eql 'typein)))
  (format-oldtypein s (stringize slot) (stringize value) 40))

(defmethod output-fastcreate-cell (s slot value (style (eql 'stringfield)))
  (format-oldstringfield s (stringize slot) (prettyname value) 40))

(defmethod output-fastcreate-cell (s slot value (style (eql 'text)))
  (format-text s slot (prettyname value) 40))

(defmethod output-fastcreate-cell (s slot value (style (eql 'textarea)))
  (format-oldstringfield s (stringize slot) (prettyname value) 40))

(defmethod output-fastcreate-cell (s slot value (style (eql 'password)))
  (format-password s slot (prettyname value) 40))

(defmethod output-fastcreate-cell (s slot value (style (eql 'dateinput)))
  (output-olddateinput s slot value))

(defmethod output-fastcreate-cell (s slot value (style (eql 'datestyle)))
  (declare (ignore slot))
  (output-simple s value))

(defmethod output-fastcreate-cell (s slot value (style (eql 'dollarinput)))
  (output-olddollarinput s slot value 6))

(defmethod output-fastcreate-cell (s slot value (style (eql 'dollarstyle)))
  (declare (ignore slot))
  (when (realp value) (format s "$~$" value)))

(defmethod output-fastcreate-cell (s slot value (style (eql 'emailstyle)))
  (declare (ignore slot))
  (format s "<a href='mailto:~A'><font color='red'>~A</font></a>" value value))

(defmethod output-fastcreate-cell (s slot value (style (eql 'htmlstyle)))
  (declare (ignore slot))
  (format s "~A" value))

(defmethod output-fastcreate-cell (s slot value (style (eql 'imagestyle)))
  (declare (ignore slot))
  (format s "<img src='~A'/>" value))

(defmethod output-fastcreate-cell (s slot value (style (eql 'urlstyle)))
  (format-hidden s slot value)
  (format s "<a href='~A'>~A</a>" value (htmlify value)))

(defmethod output-fastcreate-cell (s slot value (style (eql 'glyph)))
  (format-hidden s (stringize slot) (stringize value))
  (output-handle s value))

(defmethod output-fastcreate-cell (s slot value (style (eql 'prettystyle)))
  (format-hidden s (stringize slot) (stringize value))
  (output-simple s value))

(defmethod output-fastcreate-cell (s slot value style)
  (declare (ignore style))
  (format-hidden s (stringize slot) (stringize value)))

(defun output-fastcreate-success (s object)
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

(defmethod output-fastcreate-unicells (s slot values style)
  (format s "<table cellpadding='0' cellspacing='0'>")
  (incf *buttons*)
  (format s "<tr><td width='32' valign='top'>")
  (output-empty-button s)
  (output-empty-button s)
  (format s "</td><td>")
  (output-fastcreate-cell s slot (car values) style) (crlf s)
  (format s "</td></tr>")
  (dolist (value (cdr values))
    (incf *buttons*)
    (format s "<tr><td width='32' valign='top'>")
    (output-empty-button s)
    (output-empty-button s)
    (format s "</td><td>")
    (output-fastcreate-cell s slot value style) (crlf s)
    (format s "</td></tr>"))
  (format s "</table>")
  (crlf s))

(defun output-fastcreate-multicells (s slot values style)
  (let (multivalued multiple)
    (setq multivalued (find-multivalued slot))
    (setq multiple (cdr values))
    (format s "<table cellpadding='0' cellspacing='0'>")
    (incf *buttons*)
    (format s "<tr><td width='32' valign='top'>")
    (if multivalued (output-add s *buttons*) (output-empty-button s))
    (if multiple (output-delete s *buttons*) (output-empty-button s))
    (format s "</td><td>")
    (output-fastcreate-cell s slot (car values) style) (crlf s)
    (format s "</td></tr>")
    (dolist (value (cdr values))
      (incf *buttons*)
      (format s "<tr><td width='32' valign='top'>")
      (if multivalued (output-add s *buttons*) (output-empty-button s))
      (if multiple (output-delete s *buttons*) (output-empty-button s))
      (format s "</td><td>")
      (output-fastcreate-cell s slot value style) (crlf s)
      (format s "</td></tr>"))
    (format s "</table>")
    (crlf s)))

(defmethod output-fastcreate-create (s class)
  (declare (ignore class))
  (format s "<input type='button' name='Command' value='Create'
                    onClick='postRecord(form1)'/>"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun fastcreatescript ()
  "<script>

var http_slot = false;
var http_old = false;

function fastselect (slot,value)
 {http_slot = slot;
  postSelect('fastselect?','Slot=' + slot + '&Value=' + value)}

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
          cell.innerHTML = http_result.responseText}
      else {alert('There was a problem with the request in alertResult.')}}}

function fancyselect (slot,old,context,value)
 {http_slot = slot;
  http_old = value;
  postFancyselect('fancyselect?','Slot=' + slot + '&Old=' + old + '&Context=' + context + '&Value=' + value)}

function fancydeselect (slot,old,context,value)
 {http_slot = slot;
  http_old = value;
  postFancyselect('fancydeselect?','Slot=' + slot + '&Old=' + old + '&Context=' + context + '&Value=' + value)}

function fancycancel (slot,old)
 {document.getElementById(slot).innerHTML=''}

function postFancyselect (url,args)
 {http_result = false;
  if (window.XMLHttpRequest)
     {http_result = new XMLHttpRequest();
      if (http_result.overrideMimeType)
         {http_result.overrideMimeType('text/xml');}}
  else if (window.ActiveXObject)
          {try {http_result = new ActiveXObject('Msxml2.XMLHTTP')}
           catch (e) {try {http_result = new ActiveXObject('Microsoft.XMLHTTP')}
                      catch (e) {} }}
  http_result.onreadystatechange = alertFancyselect;
  http_result.open('POST', url, true);
  http_result.send(args);}

function alertFancyselect()
 {if (http_result.readyState == 4)
     {if (http_result.responseText)
         {var elem = document.getElementById(http_slot);
          if (http_result.responseText == ' ')
             {document.getElementsByName(http_slot)[0].value=http_old};
          elem.innerHTML = http_result.responseText}
      else {alert('There was a problem with the request in alertResult.')}}}

function postRecord (fobj)
 {window.location = fobj.action + 'Structure=' + newFormStructure(fobj) + '&Command=Record'}

</script>")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; fastchangepage
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defparameter *fastchangepage* "fastchangepage")

(defmethod process (s (command (eql 'fastchangepage)) postlines)
  (let (dum class structure)
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
  (let (delta result)
    (cond ((setq result (checkchange object class constraints))
           (http-problems s result))
          ((setq delta (differentiator (list* object class constraints)))
           (setq result (prorequest (cons 'update delta)))
           (cond ((errorp result) (http-problems s result))
                 (t (output-fastchange-success s object))))
          (t (output-fastchange-success s object)))))

(defmethod checkchange (object class constraints)
  (declare (ignore object class constraints))
  (let (errors)
    (when errors
      (setq errors (cons "Press the Back button, correct the error(s), and resubmit." errors)))
    (nreverse errors)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun output-fastchangepage (s structure)
  (format-html s) (crlf s)
  (format s "<head>")
  (format s "<title>Change ~A</title>" (prettify (car structure))) (crlf s)
  (format s (fastchangescript)) (crlf s)
  (format s (stylesheet)) (crlf s)
  (format s (modalscript)) (crlf s)
  (format s "</head>") (crlf s)
  (format-body s *bgcolor*) (crlf s)
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
    (format s "<form name='form1' action='~A?'>" *fastchangepage*) (crlf s)
    (output-handle s (car structure))
    (format s " is ~A ~A." (article (cadr structure)) (prettify (cadr structure)))
    (format-hidden s "Object" (stringize (car structure)))
    (format-hidden s "Class" (stringize (cadr structure)))
    (format s "<br/>")
    (output-fastchange-inner s structure)
    (output-fastchange-record s (cadr structure)) (crlf s)
    (format s "</form>")))

(defun output-fastchange-inner (s structure)
  (format s "<table cellspacing='3'>")
  (do ((l (cddr structure)) (slot) (values) (style) (label))
      ((null l))
      (setq slot (caar l))
      (setq style (find-createstyle slot))
      (setq label (find-createlabel slot))
      (multiple-value-setq (values l) (collectentries slot l))
      (format s "<tr><th align='left' valign='top'>")
      (output-slotlink s slot)
      (format s "</th><td>")
      (output-fastchange-cells s slot values style)
      (format s "</td>")
      (when label (format s "<td>~A</td>" label))
      (format s "</tr>")
      (crlf s))
  (format s "</table>") (crlf s))

(defmethod output-fastchange-cells (s slot values (style (eql 'menu)))
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

(defmethod output-fastchange-cells (s slot values (style (eql 'selector)))
  (output-fastchange-multicells s slot values style))

(defmethod output-fastchange-cells (s slot values (style (eql 'fastselector)))
  (output-fastchange-unicells s slot values style))

(defmethod output-fastchange-cells (s slot values (style (eql 'fancyselector)))
  (output-fastchange-unicells s slot values style))

(defmethod output-fastchange-cells (s slot values (style (eql 'combobox)))
  (output-fastchange-unicells s slot values style))

(defmethod output-fastchange-cells (s slot values (style (eql 'checkbox)))
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

(defmethod output-fastchange-cells (s slot values (style (eql 'radiobutton)))
  (output-fastchange-unicells s slot values style))

(defmethod output-fastchange-cells (s slot values (style (eql 'typein)))
  (output-fastchange-multicells s slot values style))

(defmethod output-fastchange-cells (s slot values (style (eql 'stringfield)))
  (output-fastchange-multicells s slot values style))

(defmethod output-fastchange-cells (s slot values (style (eql 'text)))
  (output-fastchange-multicells s slot values style))

(defmethod output-fastchange-cells (s slot values (style (eql 'textarea)))
  (output-fastchange-multicells s slot values style))

(defmethod output-fastchange-cells (s slot values (style (eql 'password)))
  (output-fastchange-multicells s slot values style))

(defmethod output-fastchange-cells (s slot values (style (eql 'dollarinput)))
  (output-fastchange-multicells s slot values style))

(defmethod output-fastchange-cells (s slot values style)
  (output-fastchange-unicells s slot values style))

(defmethod output-fastchange-cell (s slot value (style (eql 'selector)))
  (output-oldselector s slot (cons 'unknown (findalternatives slot)) value))

(defmethod output-fastchange-cell (s slot value (style (eql 'fastselector)))
  (output-oldfastselector s slot value))

(defmethod output-fastchange-cell (s slot value (style (eql 'fancyselector)))
  (output-oldfancyselector s slot value))

(defmethod output-fastchange-cell (s slot value (style (eql 'combobox)))
  (let (options)
    (when (setq options (findalternatives slot))
      (setq options (cons 'unknown options))
      (output-oldcombobox s slot options (prettyname value) 40))))

(defmethod output-fastchange-cell (s slot value (style (eql 'radiobutton)))
  (let (options)
    (when (setq options (findalternatives slot))
      (setq options (cons nil options))
      (output-oldradiobutton s slot options value))))

(defmethod output-fastchange-cell (s slot value (style (eql 'typein)))
  (format-oldtypein s (stringize slot) (stringize value) 40))

(defmethod output-fastchange-cell (s slot value (style (eql 'stringfield)))
  (format-oldstringfield s (stringize slot) (prettyname value) 40))

(defmethod output-fastchange-cell (s slot value (style (eql 'text)))
  (format-text s slot (prettyname value) 40))

(defmethod output-fastchange-cell (s slot value (style (eql 'textarea)))
  (format-textarea s slot (prettyname value) 4 40))

(defmethod output-fastchange-cell (s slot value (style (eql 'password)))
  (format-password s slot (prettyname value) 40))

(defmethod output-fastchange-cell (s slot value (style (eql 'dateinput)))
  (output-olddateinput s slot value))

(defmethod output-fastchange-cell (s slot value (style (eql 'datestyle)))
  (declare (ignore slot))
  (output-simple s value))

(defmethod output-fastchange-cell (s slot value (style (eql 'dollarinput)))
  (output-olddollarinput s slot value 6))

(defmethod output-fastchange-cell (s slot value (style (eql 'dollarstyle)))
  (declare (ignore slot))
  (when (realp value) (format s "$~$" value)))

(defmethod output-fastchange-cell (s slot value (style (eql 'emailstyle)))
  (declare (ignore slot))
  (format s "<a href='mailto:~A'><font color='red'>~A</font></a>" value value))

(defmethod output-fastchange-cell (s slot value (style (eql 'htmlstyle)))
  (declare (ignore slot))
  (format s "~A" value))

(defmethod output-fastchange-cell (s slot value (style (eql 'imagestyle)))
  (declare (ignore slot))
  (format s "<img src='~A'/>" value))

(defmethod output-fastchange-cell (s slot value (style (eql 'urlstyle)))
  (format-hidden s slot value)
  (format s "<a href='~A'>~A</a>" value (htmlify value)))

(defmethod output-fastchange-cell (s slot value (style (eql 'glyph)))
  (format-hidden s (stringize slot) (stringize value))
  (output-handle s value))

(defmethod output-fastchange-cell (s slot value (style (eql 'prettystyle)))
  (format-hidden s (stringize slot) (stringize value))
  (output-simple s value))

(defmethod output-fastchange-cell (s slot value style)
  (declare (ignore style))
  (format-hidden s (stringize slot) (stringize value)))


(defmethod output-fastchange-unicells (s slot values style)
  (format s "<table cellpadding='0' cellspacing='0'>")
  (incf *buttons*)
  (format s "<tr><td width='32' valign='top'>")
  (output-empty-button s)
  (output-empty-button s)
  (format s "</td><td>")
  (output-fastchange-cell s slot (car values) style) (crlf s)
  (format s "</td></tr>")
  (dolist (value (cdr values))
    (incf *buttons*)
    (format s "<tr><td width='32' valign='top'>")
    (output-empty-button s)
    (output-empty-button s)
    (format s "</td><td>")
    (output-fastchange-cell s slot value style) (crlf s)
    (format s "</td></tr>"))
  (format s "</table>")
  (crlf s))

(defun output-fastchange-multicells (s slot values style)
  (let (multivalued multiple)
    (setq multivalued (find-multivalued slot))
    (setq multiple (cdr values))
    (format s "<table cellpadding='0' cellspacing='0'>")
    (incf *buttons*)
    (format s "<tr><td width='32' valign='top'>")
    (if multivalued (output-add s *buttons*) (output-empty-button s))
    (if multiple (output-delete s *buttons*) (output-empty-button s))
    (format s "</td><td>")
    (output-fastchange-cell s slot (car values) style) (crlf s)
    (format s "</td></tr>")
    (dolist (value (cdr values))
      (incf *buttons*)
      (format s "<tr><td width='32' valign='top'>")
      (if multivalued (output-add s *buttons*) (output-empty-button s))
      (if multiple (output-delete s *buttons*) (output-empty-button s))
      (format s "</td><td>")
      (output-fastchange-cell s slot value style) (crlf s)
      (format s "</td></tr>"))
    (format s "</table>")
    (crlf s)))

(defmethod output-fastchange-record (s class)
  (declare (ignore class))
  (format s "<input type='button' name='Command' value='Record'
                    onClick='postRecord(form1)'/>"))

(defun output-fastchange-success (s object)
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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun fastchangescript ()
  "<script>

var http_slot = false;
var http_old = false;

function fastselect (slot,value)
 {http_slot = slot;
  postSelect('fastselect?','Slot=' + slot + '&Value=' + value)}

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
          cell.innerHTML = http_result.responseText}
      else {alert('There was a problem with the request in alertResult.')}}}

function fancyselect (slot,old,context,value)
 {http_slot = slot;
  http_old = value;
  postFancyselect('fancyselect?','Slot=' + slot + '&Old=' + old + '&Context=' + context + '&Value=' + value)}

function fancydeselect (slot,old,context,value)
 {http_slot = slot;
  http_old = value;
  postFancyselect('fancydeselect?','Slot=' + slot + '&Old=' + old + '&Context=' + context + '&Value=' + value)}

function fancycancel (slot,old)
 {document.getElementById(slot).innerHTML=''}

function postFancyselect (url,args)
 {http_result = false;
  if (window.XMLHttpRequest)
     {http_result = new XMLHttpRequest();
      if (http_result.overrideMimeType)
         {http_result.overrideMimeType('text/xml');}}
  else if (window.ActiveXObject)
          {try {http_result = new ActiveXObject('Msxml2.XMLHTTP')}
           catch (e) {try {http_result = new ActiveXObject('Microsoft.XMLHTTP')}
                      catch (e) {} }}
  http_result.onreadystatechange = alertFancyselect;
  http_result.open('POST', url, true);
  http_result.send(args);}

function alertFancyselect()
 {if (http_result.readyState == 4)
     {if (http_result.responseText)
         {var elem = document.getElementById(http_slot);
          if (http_result.responseText == ' ')
             {document.getElementsByName(http_slot)[0].value=http_old};
          elem.innerHTML = http_result.responseText}
      else {alert('There was a problem with the request in alertResult.')}}}

function postRecord (fobj)
 {window.location = fobj.action + 'Structure=' + newFormStructure(fobj) + '&Command=Record'}

   </script>")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; fastcopypage
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod process (s (command (eql 'fastcopypage)) postlines)
  (let (object class)
    (cond ((null (cdr postlines)) (http-problem s "Bad request."))
          (t (setq object (read-user-string (cdr (pop postlines))))
             (setq class (classify object *gui*))
             (process-fastcopypage s object class)))))

(defmethod process-fastcopypage (s object class)
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
  (let (object)
    (cond ((null (cdr postlines)) (http-problem s "Bad request."))
          (t (setq object (read-user-string (cdr (pop postlines))))
             (process-fastdeletepage s object)))))

(defun process-fastdeletepage (s object)
  (let (facts result)
    (setq facts (facts object *gui*))
    (setq result (prorequest `(update ,(maksand (mapcar #'maknot facts)))))
    (cond ((errorp result) (output-problems s result))
          (t (html-message s "Object deleted.")))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Widgets
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; menu
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun output-dismenu (s slot options values)
  (format s "<select name='~A' disabled='true' multiple='true' size='~A'>"
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
;;; selector
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun output-disselector (s value)
  (format s "<select disabled='true'>")
  (format s "<option value='~A' selected='true'>~A</option>"
          (stringize value) (prettyname value))
  (format s "</select>"))

(defun output-oldselector (s slot options value)
  (format s "<select name='~A' index='~A'>" (stringize slot) *buttons*)
  (dolist (option options)
    (cond ((equalp option value)
           (format s "<option value='~A' selected='true'>~A</option>"
                   (stringize option) (prettyname option)) (crlf s))
          (t (format s "<option value='~A'>~A</option>"
                     (stringize option) (prettyname option)) (crlf s))))
  (format s "</select>"))

(defun output-newselector (s slot options value)
  (format s "<select name='~A' index='~A' onChange='postReplace(this.form)'>"
          (stringize slot) *buttons*)
  (dolist (option options)
    (cond ((equalp option value)
           (format s "<option value='~A' selected='true'>~A</option>"
                   (stringize option) (newprettify slot option)) (crlf s))
          (t (format s "<option value='~A'>~A</option>"
                     (stringize option) (newprettify slot option)) (crlf s))))
  (format s "</select>"))

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
  (format s "<select id='~A' name='~A' index='~A'
                     onChange='fastselect(\"~A\",this.options[this.selectedIndex].value)'>"
          (stringize slot) (stringize slot) *buttons* (stringize slot)) (crlf s))

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
  (format s "<select name='Up' status='skip' onChange='fancydeselect(\"~A\",\"~A\",\"~A\",this.options[this.selectedIndex].value)'>"
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
    (format s "<select name='Down' status='skip' multiple onChange='fancyselect(\"~A\",\"~A\",\"~A\",this.options[this.selectedIndex].value)'>"
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
;;; combobox
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun output-oldcombobox (s slot options value size)
  (when (eq value 'unknown) (setq value ""))
  (format s "<input type='text' name='~A' value='~A' size='~A' autocomplete='off'
                    onFocus='showMenu(this.nextSibling)' onClick='cancelBubble(event)'
                    onkeydown='return HandleKeyDown(event)'/>"
          (stringize slot) value size)
  (output-oldcombobox-div s options)
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
           (format s "<input type='checkbox' name='~A' value='~A' checked='true' disabled='true'/>"
                   (stringize slot) (stringize option)))
          (t (format s "<input type='checkbox' name='~A' value='~A' disabled='true'/>"
                     (stringize slot) (stringize option))))
    (format s "~A" (prettyname option))
    (format s "</td></tr>"))
  (format s "</table>"))

(defun output-oldcheckbox (s slot options values)
  (format-hidden s (stringize slot) "")
  (format s "<table>")
  (dolist (option options)
    (incf *buttons*)
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
  (format-hidden s (stringize slot) "")
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
;;; radiobutton
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun output-disradiobutton (s slot options value)
  (format s "<table>")
  (dolist (option options)
    (format s "<tr><td>")
    (if (equalp option value)
        (format s "<input type='radio' name='~A' value='~A' disabled='true' checked='true'/>"
                (stringize slot) (stringize option))
        (format s "<input type='radio' name='~A' value='~A' disabled='true'/>"
                (stringize slot) (stringize option)))
    (format s "~A" (prettyname option))
    (format s "</td></tr>"))
  (format s "</table>"))

(defun output-oldradiobutton (s slot options value)
  (format s "<table>")
  (dolist (option options)
    (format s "<tr><td>")
    (if (equalp option value)
        (format s "<input type='radio' name='~A' index='~A' value='~A' checked='true'/>"
                (stringize slot) *buttons* (stringize option))
        (format s "<input type='radio' name='~A' index='~A' value='~A'/>"
                (stringize slot) *buttons* (stringize option)))
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
;;; typein
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun format-distypein (s value size)
  (format s "<input type='text' value='~A' size='~A' disabled='true'/>"
          value size))

(defun format-oldtypein (s name value size)
  (format s "<input type='text' name='~A' index='~A' value='~A' size='~A'/>"
          name *buttons* value size))

(defun format-newtypein (s name value size)
  (format s "<input type='text' name='~A' index='~A' value='~A' size='~A'
                    onKeyPress='return textEdit(event)'
                    onBlur='textBlur(event)'/>"
          name *buttons* value size))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; stringfield
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun format-oldstringfield (s name value size)
  (format s "<input type='text' status='string' name='~A' index='~A' value='~A' size='~A'/>"
          name *buttons* value size))

(defun format-newstringfield (s name value size)
  (format s "<input type='text' status='string' name='~A' index='~A' value='~A' size='~A'
                    onKeyPress='return textEdit(event)'
                    onBlur='textBlur(event)'/>"
          name *buttons* value size))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; password
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun format-dispassword (s value size)
  (format s "<input type='password' value='~A' size='~A' disabled='true'/>"
          value size))

(defun format-oldpassword (s name value size)
  (format s "<input type='password' status='string' name='~A' index='~A' value='~A' size='~A'/>"
          name *buttons* value size))

(defun format-newpassword (s name value size)
  (format s "<input type='password' status='string' name='~A' index='~A' value='~A' size='~A'
                    onKeyPress='return textEdit(event)'
                    onBlur='textBlur(event)'/>"
          name *buttons* value size))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; dateinput
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun output-olddateinput (s name value)
  (when (or (null value) (eq value 'unknown)) (setq value ""))
  (format s "<input type='text' name='~A' value='~A' size='10' maxlength='10' autocomplete='off'
                    onFocus='showCal(this.nextSibling,this.value)' onClick='cancelBubble(event)'
                    onkeydown='return HandleKeyDown(event)'/>"
          name value)
  (output-dateinput-div s)
  (format s "<iframe id='iframe~A' style='display:none;position:absolute;z-index:2;filter:mask();' frameborder='0'></iframe>"
          *buttons*) (crlf s))

(defun output-newdateinput (s name value)
  (when (or (null value) (eq value 'unknown)) (setq value ""))
  (format s "<input type='text' name='~A' value='~A' size='10' maxlength='10' autocomplete='off'
                    onFocus='showCal(this.nextSibling,this.value)' onClick='cancelBubble(event)'
                    onChange='postReplace(form1)'
                    onkeydown='return HandleKeyDown(event)'/>"
          name value)
  (output-dateinput-div s)
  (format s "<iframe id='iframe~A' style='display:none;position:absolute;z-index:2;filter:mask();' frameborder='0'></iframe>"
          *buttons*) (crlf s))

(defun output-dateinput-div (s)
  (format s "<div style='background-color:#FFF; position:absolute; overflow:auto; z-index:3; cursor:pointer; font-size:100%; display:none; -moz-user-select:none; -khtml-user-select:none; user-select:none;'
                  onselectstart='return false;'>")
  (format s "<input type='hidden' status='skip'/>")
  (format s "<input type='hidden' status='skip'/>")
  (format s "<input type='hidden' status='skip'/>")
  (format s "<input type='hidden' status='skip'/>")
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
  (format s "<td onclick='calClick(this.id,event)'></td>") (crlf s)
  (dotimes (i 5)
    (format s "<td onclick='calClick(this.id,event)'></td>")  (crlf s))
  (format s "<td onclick='calClick(this.id,event)'></td>")  (crlf s)
  (format s "</tr>")  (crlf s)
  (dotimes (j 6)
    (format s "<tr>")  (crlf s)
    (format s "<td onclick='calClick(this.id,event)'></td>")  (crlf s)
    (dotimes (i 5)
      (format s "<td onclick='calClick(this.id,event)'></td>")  (crlf s))
    (format s "<td onclick='calClick(this.id,event)'></td>")  (crlf s)
    (format s "</tr>")  (crlf s))
  (format s "</table>")  (crlf s)
  (format s "</div>")  (crlf s))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; dollarinput
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun output-olddollarinput (s name value size)
  (format s "$<input type='text' name='~A' index='~A' value='~A' size='~A' maxlength='~A'
                     onKeyPress='return oldCheckNumber(event,this.value)'/>"
          name *buttons*
          (cond ((integerp value) (stringize value))
                ((realp value) (format nil "~$" value))
                ((stringp value) value)
                (t ""))
          size size))

(defun output-newdollarinput (s name value size)
  (format s "$<input type='text' name='~A' index='~A' value='~A' size='~A' maxlength='~A'
                     onKeyPress='return newCheckNumber(event,this.value)'
                     onBlur='postReplace(this.form)'/>"
          name *buttons*
          (cond ((integerp value) (stringize value))
                ((realp value) (format nil "~$" value))
                ((stringp value) value)
                (t ""))
          size size))

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

(defun format-border (s)
  (format s "<div style='margin-left:10px; margin-right:10px; margin-top:10px; margin-bottom:10px' onClick='hidePopup()'>"))

(defun finish-border (s)
  (format s "</div>"))

(defun output-add (s x)
  (format s "<image src='~A/images/add.gif' border='0' onClick='postAnother(form1,\"~A\")'/>"
          *home* (addressify x)))

(defun output-delete (s x)
  (format s "<image src='~A/images/delete.gif' border='0' onClick='postRemoval(form1,\"~A\")'/>"
          *home* (addressify x)))

(defun output-another-button (s buttons multivalued)
  (if multivalued (output-snow-button s buttons)
      (output-empty-button s)))

(defun output-removal-button (s buttons partial)
  (if partial (output-trash-button s buttons)
      (output-empty-button s)))

(defun output-snow-button (s x)
  (format s "<input type='image' src='~A/images/new.gif' name='~A' border='0'/>" *home* (addressify x)))

(defun output-trash-button (s x)
  (format s "<input type='image' src='~A/images/smalltrash.gif' name='~A' border='0'/>" *home* (addressify x)))

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
          ((> end count) (setq end (min count (+ start (1- *count*))))))
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

(defun adjoiner (x l)
  (adjoin x l :test #'equalp))

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
;;; stylesheet
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun stylesheet ()
  (format nil "<style type='text/css'>
.calleft {border-left:1px #A2BBDD solid;background:#eee}
.calright {border-right:1px #A2BBDD solid;background:#eee}
.caltop {border-top:1px #A2BBDD solid}
</style>"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; modalscript
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun modalscript ()
  "<script type='text/javascript'>
var gMenu = null;
var gCal = null;
var gColorPicker = null;
var gSelectedItem = null;
var gSelectedCell = null;
var gIsGecko = false;
var gIsOpera = false;
var gIsSafari = null;
var gIsBrowserKnown = false;
var preloadImage = new Image();

var gMonths = [
  {name: 'January', days: 31},
  {name: 'February', days: 28},
  {name: 'March', days: 31},
  {name: 'April', days: 30},
  {name: 'May', days: 31},
  {name: 'June', days: 30},
  {name: 'July', days: 31},
  {name: 'August', days: 31},
  {name: 'September', days: 30},
  {name: 'October', days: 31},
  {name: 'November', days: 30},
  {name: 'December', days: 31}
];

var textchange = false;
var http_result = false;

function postAnother (fobj,index)
 {postRequest(fobj.action,'Structure=' + anotherStructure(fobj,index) + '&Command=Refresh')}

function postRemoval (fobj,index)
 {postRequest(fobj.action,'Structure=' + removalStructure(fobj,index) + '&Command=Refresh')}

function postReplace (fobj)
 {postRequest(fobj.action,'Structure=' + newFormStructure(fobj) + '&Command=Refresh')}

function postDisplay (fobj)
 {window.location = fobj.action + 'Structure=' + newFormStructure(fobj) + '&Command=Display'}

function postRequest (url,args)
 {http_result = false;
  if (window.XMLHttpRequest)
     {http_result = new XMLHttpRequest();
      if (http_result.overrideMimeType)
         {http_result.overrideMimeType('text/xml');}}
  else if (window.ActiveXObject)
          {try {http_result = new ActiveXObject('Msxml2.XMLHTTP')}
           catch (e) {try {http_result = new ActiveXObject('Microsoft.XMLHTTP')}
                      catch (e) {} }};
  http_result.onreadystatechange = alertRequest;
  http_result.open('POST', url, true);
  http_result.send(args)}

function alertRequest()
 {if (http_result.readyState == 4)
     {if (http_result.responseText)
         {document.documentElement.innerHTML=http_result.responseText}
      else {alert('There was a problem with the request in alertResult.')}}}

function newFormStructure (fobj)
 {var str = '';
  str += '(' + fobj.elements[0].value;
  str += ' ' + fobj.elements[1].value;
  for (var i = 2; i < fobj.elements.length; i++)
      {if (fobj.elements[i].status == 'skip') {str = str}
       else if (fobj.elements[i].type == 'select-one')
               {var val = fobj.elements[i].options[fobj.elements[i].selectedIndex].value;
                str += ' ' + listify(fobj.elements[i].name, val)}
       else if (fobj.elements[i].type == 'select-multiple')
               {for (var j = 0; j < fobj.elements[i].options.length; j++)
                    {if (fobj.elements[i].options[j].selected)
                        {var val = fobj.elements[i].options[j].value;
                         str += ' ' + listify(fobj.elements[i].name, val)}}}
       else if (fobj.elements[i].type == 'radio')
               {if (fobj.elements[i].checked)
                   {str += ' ' + listify(fobj.elements[i].name, escape(fobj.elements[i].value))}}
       else if (fobj.elements[i].type == 'checkbox')
               {if (fobj.elements[i].checked)
                   {str += ' ' + listify(fobj.elements[i].name, escape(fobj.elements[i].value))}}
       else if (fobj.elements[i].type == 'button')
               {str = str}
       else if (fobj.elements[i].status == 'string')
               {if (fobj.elements[i].value == '')
                   {str +=  ' ' + listify(fobj.elements[i].name, '')}
                else {str += ' ' + listify(fobj.elements[i].name, stringize(fobj.elements[i].value))}}
       else {str += ' ' + listify(fobj.elements[i].name, escape(fobj.elements[i].value))}}
  str = str + ')';
  return str}

function anotherStructure (fobj,ind)
 {var str = '';
  str += '(' + fobj.elements[0].value;
  str += ' ' + fobj.elements[1].value;
  for (var i = 2; i < fobj.elements.length; i++)
      {if (fobj.elements[i].status == 'skip') {str = str}
       else if (fobj.elements[i].type == 'select-one')
               {var val = fobj.elements[i].options[fobj.elements[i].selectedIndex].value;
                str += ' ' + listify(fobj.elements[i].name,val);
                if (fobj.elements[i].getAttribute('index') == ind)
                   {str += ' ' + listify(fobj.elements[i].name, '')}}
       else if (fobj.elements[i].type == 'select-multiple')
               {for (var j = 0; j < fobj.elements[i].options.length; j++)
                    {if (fobj.elements[i].options[j].selected)
                        {var val = fobj.elements[i].options[j].value;
                         str += ' ' + listify(fobj.elements[i].name, val)}}}
       else if (fobj.elements[i].type == 'radio')
               {if (fobj.elements[i].checked)
                   {str += ' ' + listify(fobj.elements[i].name, escape(fobj.elements[i].value))}}
       else if (fobj.elements[i].type == 'checkbox')
               {if (fobj.elements[i].checked)
                   {str += ' ' + listify(fobj.elements[i].name, escape(fobj.elements[i].value))}}
       else if (fobj.elements[i].type == 'button')
               {str = str}
       else if (fobj.elements[i].status == 'string')
               {if (fobj.elements[i].value == '')
                   {str +=  ' ' + listify(fobj.elements[i].name, '')}
                else {str += ' ' + listify(fobj.elements[i].name, stringize(fobj.elements[i].value))};
                if (fobj.elements[i].getAttribute('index') == ind)
                   {str += ' ' + listify(fobj.elements[i].name, '')}}
       else {str += ' ' + listify(fobj.elements[i].name, escape(fobj.elements[i].value));
             if (fobj.elements[i].getAttribute('index') == ind)
                 {str += ' ' + listify(fobj.elements[i].name, '')}}}
  str = str + ')';
  return str}

function removalStructure (fobj,ind)
 {var str = '';
  str += '(' + fobj.elements[0].value;
  str += ' ' + fobj.elements[1].value;
  for (var i = 2; i < fobj.elements.length; i++)
      {if (fobj.elements[i].status == 'skip') {str = str}
       else if (fobj.elements[i].index == ind) {str = str}
       else if (fobj.elements[i].type == 'button') {str = str}
       else if (fobj.elements[i].type == 'select-one')
               {if (fobj.elements[i].getAttribute('index') != ind)
                   {var val = fobj.elements[i].options[fobj.elements[i].selectedIndex].value;
                    str += ' ' + listify(fobj.elements[i].name, val)}}
       else if (fobj.elements[i].type == 'select-multiple')
               {for (var j = 0; j < fobj.elements[i].options.length; j++)
                    {if (fobj.elements[i].options[j].selected)
                        {var val = fobj.elements[i].options[j].value;
                         str += ' ' + listify(fobj.elements[i].name, val)}}}
       else if (fobj.elements[i].type == 'radio')
               {if (fobj.elements[i].checked)
                   {str += ' ' + listify(fobj.elements[i].name, escape(fobj.elements[i].value))}}
       else if (fobj.elements[i].type == 'checkbox')
               {if (fobj.elements[i].checked)
                   {str += ' ' + listify(fobj.elements[i].name, escape(fobj.elements[i].value))}}
       else if (fobj.elements[i].status == 'string')
               {if (fobj.elements[i].getAttribute('index') != ind && fobj.elements[i].value == '')
                   {str +=  ' ' + listify(fobj.elements[i].name, '')}
                else {str += ' ' + listify(fobj.elements[i].name, stringize(fobj.elements[i].value))}}
       else if (fobj.elements[i].getAttribute('index') != ind)
               {str += ' ' + listify(fobj.elements[i].name, escape(fobj.elements[i].value))}}
  str = str + ')';
  return str}

function stringize (str)
  {str = str.replace(/\\\\/g,'\\\\\\\\');
   str = str.replace(/\"/g,'\\\\\\\"');
   return '\"' + escape(str) + '\"'}

function listify (x, y)
  {return '(' + x + ' ' + y + ')'}

function textEdit(e)
 {var keynum;
  if (window.event) {keynum = e.keyCode}
  else if (e.which) {keynum = e.which};
  if (keynum == 13)
     {textchange = false;
      postReplace(form1);
      return false}
  else {textchange = true;
        return true}}

function textBlur(e)
 {if (textchange == true)
     {textchange = false;
      postReplace(form1)}}

function oldCheckNumber (evt,value)
 {var code = GetCharCode(evt);
  if (IsMetaKeyDown(evt) || IsAltKeyDown(evt) || IsCtrlKeyDown(evt)) {return true}
  if ((code==null) || (code==0) || (code==8) || (code==9) || (code==27) || (code >= 37 && code <= 40) || (code > 255)) {return true}
  else if (code >= 48 && code <= 57) {return true}
  else if (((code == 190) || (code == 46)) && value.indexOf('.') == -1) {return true}
  return false}

function newCheckNumber (evt,value)
 {var code = GetCharCode(evt);
  if (IsMetaKeyDown(evt) || IsAltKeyDown(evt) || IsCtrlKeyDown(evt)) {return true}
  if (code == 13) {postReplace(form1); return false};
  if ((code==null) || (code==0) || (code==8) || (code==9) || (code==27) || (code >= 37 && code <= 40) || (code > 255)) {return true}
  else if (code >= 48 && code <= 57) {return true}
  else if (((code == 190) || (code == 46)) && value.indexOf('.') == -1) {return true}
  return false}

function DetermineBrowser()
 {if(gIsBrowserKnown) return;
  gIsBrowserKnown=true;
  var ua=navigator.userAgent.toLowerCase();
  gIsGecko=((ua.indexOf('gecko')!=-1)&&(ua.indexOf('opera')==-1));
  gIsOpera=(ua.indexOf('opera')!=-1);
  gIsSafari=(ua.indexOf('safari')!=-1)}

function IsGecko ()
 {DetermineBrowser();
  return gIsGecko}

function IsSafari ()
 {DetermineBrowser();
  return gIsSafari}

function Min (a,b)
 {if (a < b) return a;
  return b}

function IsMetaKeyDown (evt)
 {if (!evt) evt=window.event;
  if (evt) {return evt.metaKey}
  return false}

function IsShiftKeyDown (evt)
 {if (!evt) evt=window.event;
  if (evt) {return evt.shiftKey}
  return false}

function IsAltKeyDown (evt)
 {if (!evt) evt=window.event;
  if (evt) {return evt.altKey}
  return false}

function IsCtrlKeyDown (evt)
 {if (!evt) evt=window.event;
  if (evt) {return evt.ctrlKey}
  return false}

function GetCharCode (evt)
 {if (!evt) evt=window.event;
  if (evt)
     {if (evt.keyCode) {return evt.keyCode}
      else {return evt.which}}
  return 0}

function HandleKeyDown (evt)
 {var code = GetCharCode(evt);
  if (code == 9 || code == 0) {hidePopup()}
  return true}

function numChildNodes (elt)
 {var nodes = elt.childNodes;
  var n = 0;
  for (i=0; i < nodes.length; i=i+1)
      {if(nodes[i].nodeType == 1) {n = n+1}}
  return n}

function showMenu (n)
 {hidePopup();
  gMenu = n;
  var menu = document.getElementById('cbmenu' + gMenu);
  var numElts = numChildNodes(menu);
  if (numElts > 0)
     {var f = document.getElementById('iframe' + n);
      menu.style.display='block';
      var box = EltBox('box' + n);
      f.style.left = menu.style.left = box.left;
      f.style.top = menu.style.top = box.bottom;
      f.style.width = menu.style.width = box.right-box.left;
      var h = (box.bottom - box.top) * Min(6,numElts) + (IsSafari() ? 15 : 0);
      f.style.height = menu.style.height = h;
      f.style.display = 'block'}}

function hideMenu ()
 {if (gMenu != null)
     {document.getElementById('cbmenu' + gMenu).style.display='none';
      document.getElementById('iframe' + gMenu).style.display='none';
      gMenu = null}
  comboDeselect()}

function comboSelect (elt)
 {if (elt != gSelectedItem)
     {comboDeselect();
      gSelectedItem = elt;
      elt = document.getElementById(elt);
      elt.style.color='white';
      elt.style.backgroundColor='#316AC5'}}

function comboDeselect ()
 {if (gSelectedItem)
     {var elt = document.getElementById(gSelectedItem);
      elt.style.color='';
      elt.style.backgroundColor='';
      gSelectedItem = null}}

function selectElement (v)
 { var menu = document.getElementById('cbmenu' + gMenu);
   menu.scrollTop = 0;
   var nodes = menu.childNodes;
   for(i=0; i < nodes.length; i=i+1) {
     if(nodes[i].nodeType == 1 && nodes[i].firstChild.nodeValue == v) {
       comboSelect(nodes[i].id);
       menu.scrollTop = nodes[i].offsetTop}}}

function parseDate(d)
 {d = d.split('/');
   if(d.length != 3 || d[0].length == 0 || d[1].length == 0 || d[0].length > 2 || d[1].length > 2 || d[2].length != 4) return null;
   d[0] = parseInt(d[0]);
   d[1] = parseInt(d[1]);
   d[2] = parseInt(d[2]);
   if (isNaN(d[0]) || isNaN(d[1]) || isNaN(d[2])) return null;
   if (d[0] < 1 || d[0] > 12 || d[1] < 1 || d[1] > 31) return null;
   return new Date(d[2],d[0]-1,d[1])}

function showCal (n,date)
 {hidePopup();
  gCal = n;
  var cal = document.getElementById('cal' + n);
  var f = document.getElementById('iframe' + n);
  cal.style.display='block';
  var box = EltBox('box' + n);
  f.style.left = cal.style.left = box.left;
  f.style.top = cal.style.top = box.bottom;
  f.style.width = cal.offsetWidth;
  f.style.height = cal.offsetHeight;
  f.style.display = 'block';
  date = parseDate(date);
  if(!date) date = new Date();
  var month = date.getMonth()+1;
  var year = date.getFullYear();
  var day = date.getDate();
  selectCell(year,month,day)}

function hideCal ()
 {if (typeof gCal == 'string')
     {document.getElementById('cal' + gCal).style.display='none';
      document.getElementById('iframe' + gCal).style.display='none';
      gCal = null}}

function prevMonth(evt)
 {selectCell(parseInt(toElem('cal' + gCal + '_prevyear').value), parseInt(toElem('cal' + gCal + '_prevmonth').value), 0);
  cancelBubble(evt)}

function nextMonth(evt)
 {selectCell(parseInt(toElem('cal' + gCal + '_nextyear').value), parseInt(toElem('cal' + gCal + '_nextmonth').value), 0);
  cancelBubble(evt)}

function selectCell (year,month,day)
 {var monthIndex = month-1;
   gMonths[1].days = ((year % 4 == 0 && year % 100 != 0) || year % 400 == 0) ? 29 : 28;
   var prevYear = (month == 1) ? year - 1 : year;
   var nextYear = (month == 12) ? year + 1 : year;
   var prevMonth = (month == 1) ? 12 : month - 1;
   var nextMonth = month % 12 + 1;
   var firstDay = new Date ();
   firstDay.setFullYear(year,monthIndex,1);
   firstDay = firstDay.getDay();
   var monthName = gMonths[monthIndex].name;
   var numDays = gMonths[monthIndex].days;
   var prevDays = gMonths[prevMonth-1].days;
   var numPrevDays = (firstDay > 4) ? firstDay : firstDay + 7;
   toElem('cal' + gCal + '_month').innerHTML = monthName;
   toElem('cal' + gCal + '_year').innerHTML = year;
   toElem('cal' + gCal + '_prevmonth').value = prevMonth;
   toElem('cal' + gCal + '_prevyear').value = prevYear;
   toElem('cal' + gCal + '_nextmonth').value = nextMonth;
   toElem('cal' + gCal + '_nextyear').value = nextYear;
   var table = toElem('cal' + gCal + '_table');
   var d = prevDays - numPrevDays + 1;
   var m = prevMonth;
   var y = prevYear;
   for(var r = 2; r < 9; r++) {
     var row = table.rows[r];
     for (var c = 0; c < 7; c++) {
        var cell = row.cells[c];
        cell.innerHTML = d;
        cell.id = m + '/' + d + '/' + y;
        cell.style.fontWeight = (m == month) ? 'bold' : '';
        if (d == day && m == month && y == year) {
           cell.style.backgroundColor = (c == 0 || c == 6) ? '#9bd' : '#ace';
        } else {
           cell.style.backgroundColor = ''
        }
        d++;
        if (m == prevMonth && d > prevDays) {
           m = month;
           d = 1;
           y = year;
        } else if (m == month && d > numDays) {
           m = nextMonth;
           d = 1;
           y = nextYear}}}}

function calClicked (val,evt)
 {document.getElementById('box' + gCal).value=val;
  postReplace(form1);
  hidePopup();
  cancelBubble(evt)}

function cancelBubble(evt)
 {if(!evt) evt=window.event;
  if (evt)
     {if (evt.stopPropagation) evt.stopPropagation();
      if (evt.preventDefault) evt.preventDefault();
     //   if (evt.preventBubble) evt.preventBubble();
      evt.returnValue=false;
      evt.cancelBubble=true}
    return false}

function getElementBy(ele_str)
 {var elem=document.getElementById(ele_str);
  if (elem) return elem;
  var elt=document.getElementsByName(ele_str);
  if (elt.length>0) return elt[0];
  return null}

function toElem(ele)
 {if(!ele) return ele;
  if(typeof ele=='string')
    {return getElementBy(ele)}
  else {return ele}}

function EltBox(ele)
 {ele=toElem(ele);
    var box=new Object;
    var lft=0;
    var top=0;
    for (var e=ele; e; e=e.offsetParent)
        {lft += e.offsetLeft;
         top += e.offsetTop;
         if (!IsGecko())
            {lft+=e.clientLeft;
             top+=e.clientTop}}
    box.left = lft;
    box.top = top;
    box.right = lft + ele.offsetWidth;
    box.bottom = top + ele.offsetHeight;
    return box}

function hidePopup()
 {hideMenu();
  hideCal()}

</script>")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
