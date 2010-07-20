;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; fastdisplaypage
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defparameter *fastdisplaypage* "fastdisplaypage")

(defmethod process (s (command (eql 'fastdisplaypage)) postlines)
  (let (dum start end)
    (cond ((setq dum (getf-post "Class" postlines))
           (setq dum (fastcomparestructure (read-user-string dum)))
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
  (format s "<title>fastdisplaypage</title>") (crlf s)
  (format s (fastdisplayscript)) (crlf s)
  (format s (stylesheet)) (crlf s)
  (format s (modalscript)) (crlf s)
  (finish-head s) (crlf s)
  (format-body s *bgcolor*) (crlf s)
  (output-header s)
  (format-border s)
  (process-fastdisplay s structure start end)
  (finish-border s)
  (output-footer s)
  (finish-body s) (crlf s)
  (finish-html s) (crlf s))

(defun process-fastdisplay (s structure start end)
  (let (objects sorter attributes results count)
    (setq objects (findinstances (viewconvert structure) *gui*))
    (when (setq sorter (find-sorter (cadr structure)))
      (setq objects (sortem objects sorter 'ascending)))
    (multiple-value-setq (objects count start end) (trim objects start end))
    (setq attributes (displayable-slots (cadr structure)))
    (setq results (prorequest `(ask-table ,objects ,attributes)))
    (output-fastdisplay s structure objects attributes results count start end)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun output-fastdisplay (s structure objects slots results count start end)
  (format s "<center>")
  (format s "<br/>")
  (cond ((= count 0)
         (format s "<table>")
         (format s "<tr><td align='center'>There are no answers.</td></tr>") (crlf s)
         (unless (emptystructurep structure)
           (format s "<tr><td>")
           (output-fastdisplay-inner s structure objects slots results)
           (format s "</td></tr>"))
         (format s "<tr><td>")
         (output-fastdisplay-create s (cadr structure) structure)
         (format s "</td></tr></table>"))
        ((= count 1)
         (format s "<table>")
         (format s "<tr><td align='center'>There is 1 answer.</td></tr>")
         (format s "<tr><td>")
         (output-fastdisplay-inner s structure objects slots results)
         (format s "</td></tr>")
         (format s "<tr><td>")
         (output-fastdisplay-create s (cadr structure) structure)
         (format s "</td></tr></table>"))
        ((and (= start 1) (geqp end count))
         (format s "<table>")
         (format s "<tr><td align='center'>There are ~D answers." count)
         (format s "<tr><td>")
         (output-fastdisplay-inner s structure objects slots results)
         (format s "</td></tr>")
         (format s "<tr><td>")
         (output-fastdisplay-create s (cadr structure) structure)
         (format s "</td></tr></table>"))
        (t (format s "<table>")
           (format s "<tr><td align='center'>There are ~D answers.  The following table shows answers ~A through ~A.</td></tr>"
                   count start end)
           (format s "<tr><td>")
           (output-fastdisplay-inner s structure objects slots results)
           (format s "</td></tr><tr><td>")
           (multiple-value-setq (start end) (kerchunk count start end))
           (output-fastdisplay-create s (cadr structure) structure)
           (output-fastdisplay-display s (cadr structure) structure start end)
           (format s "</td></tr></table>")))
  (format s "</center>") (crlf s))

(defun output-fastdisplay-inner (s structure items slots results)
  (let (class nohandle (*buttons* 0))
    (setq class (cadr structure))
    (setq nohandle (findp `(nodisplay ,class handle) *interface*))
    (format s "<form name='form1' action='~A?' method='post'>" *fastdisplaypage*) (crlf s)
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
        (output-fastdisplay-cells s slot values structure (find-searchstyle slot))
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

(defmethod output-fastdisplay-create (s class structure)
  (declare (ignore structure))
  (unless (findp `(or (nocommand ,*gui* create) (nocreate ,*gui* ,class)) *interface*)
    (format s "<form action='fastcreatepage?' method='post'>")
    (format-hidden s "Class" (stringize class))
    (format-button s "Command" "Create")
    (format s " a new ~A." (prettify class))
    (format s "</form>") (crlf s)))

(defmethod output-fastdisplay-display (s class structure start end)
  (declare (ignore class))
  (format s "<form action='fastdisplaypage?' method='post'>")
  (format-hidden s "Structure" (htmlify (prin1-to-string structure)))
  (format-button s "Command" "Display")
  (format s "answers ")
  (format-text s "Start" (princ-to-string start) 5)
  (format s " through ")
  (format-text s "End" (princ-to-string end) 5)
  (format s "</form>") (crlf s))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod output-fastdisplay-cells (s slot values structure (style (eql 'menu)))
  (let (options)
    (setq options (fastoptions slot structure))
    (incf *buttons*)
    (when options (output-newmenu s slot options values))
    (crlf s)))

(defmethod output-fastdisplay-cells (s slot values structure (style (eql 'selector)))
  (output-fastdisplay-multicells s slot values structure style))

(defmethod output-fastdisplay-cells (s slot values structure (style (eql 'fastselector)))
  (incf *buttons*)
  (output-fastdisplay-cell s slot (car values) structure style))

(defmethod output-fastdisplay-cells (s slot values structure (style (eql 'fancyselector)))
  (incf *buttons*)
  (output-fastdisplay-cell s slot (car values) structure style))

(defmethod output-fastdisplay-cells (s slot values structure (style (eql 'combobox)))
  (incf *buttons*)
  (output-fastdisplay-cell s slot (car values) structure style))

(defmethod output-fastdisplay-cells (s slot values structure (style (eql 'checkbox)))
  (let (options)
    (setq options (fastoptions slot structure))
    (incf *buttons*)
    (when options (output-newcheckbox s slot options values))
    (crlf s)))

(defmethod output-fastdisplay-cells (s slot values structure (style (eql 'radiobutton)))
  (incf *buttons*)
  (output-fastdisplay-cell s slot (car values) structure style))

(defmethod output-fastdisplay-cells (s slot values structure (style (eql 'typein)))
  (output-fastdisplay-multicells s slot values structure style))

(defmethod output-fastdisplay-cells (s slot values structure (style (eql 'stringfield)))
  (output-fastdisplay-multicells s slot values structure style))

(defmethod output-fastdisplay-cells (s slot values structure (style (eql 'text)))
  (output-fastdisplay-multicells s slot values structure style))

(defmethod output-fastdisplay-cells (s slot values structure (style (eql 'textarea)))
  (output-fastdisplay-multicells s slot values structure style))

(defmethod output-fastdisplay-cells (s slot values structure (style (eql 'password)))
  (output-fastdisplay-multicells s slot values structure style))

(defmethod output-fastdisplay-cells (s slot values structure (style (eql 'dateinput)))
  (output-fastdisplay-multicells s slot values structure style))

(defmethod output-fastdisplay-cells (s slot values structure (style (eql 'datestyle)))
  (incf *buttons*)
  (output-fastdisplay-cell s slot (car values) structure style))

(defmethod output-fastdisplay-cells (s slot values structure (style (eql 'dollarinput)))
  (output-fastdisplay-multicells s slot values structure style))

(defmethod output-fastdisplay-cells (s slot values structure (style (eql 'dollarstyle)))
  (incf *buttons*)
  (output-fastdisplay-cell s slot (car values) structure style))

(defmethod output-fastdisplay-cells (s slot values structure (style (eql 'emailstyle)))
  (incf *buttons*)
  (output-fastdisplay-cell s slot (car values) structure style))

(defmethod output-fastdisplay-cells (s slot values structure (style (eql 'htmlstyle)))
  (incf *buttons*)
  (output-fastdisplay-cell s slot (car values) structure style))

(defmethod output-fastdisplay-cells (s slot values structure (style (eql 'imagestyle)))
  (incf *buttons*)
  (output-fastdisplay-cell s slot (car values) structure style))

(defmethod output-fastdisplay-cells (s slot values structure (style (eql 'urlstyle)))
  (incf *buttons*)
  (output-fastdisplay-cell s slot (car values) structure style))

(defmethod output-fastdisplay-cells (s slot values structure (style (eql 'glyph)))
  (incf *buttons*)
  (output-fastdisplay-cell s slot (car values) structure style))

(defmethod output-fastdisplay-cells (s slot values structure (style (eql 'prettystyle)))
  (incf *buttons*)
  (output-fastdisplay-cell s slot (car values) structure style))

(defmethod output-fastdisplay-cells (s slot values structure style)
  (incf *buttons*)
  (output-fastdisplay-cell s slot (car values) structure style))

(defun output-fastdisplay-multicells (s slot values structure style)
  (let (multivalued multiple)
    (setq multivalued (find-multivalued slot))
    (setq multiple (cdr values))
    (format s "<table cellpadding='0' cellspacing='0'>")
    (incf *buttons*)
    (format s "<tr><td valign='center'>")
    (when multivalued (output-add s *buttons*))
    (when multiple (output-delete s *buttons*))
    (format s "</td><td valign='center'>")
    (output-fastdisplay-cell s slot (car values) structure style) (crlf s)
    (format s "</td></tr>")
    (dolist (value (cdr values))
      (incf *buttons*)
      (format s "<tr><td valign='center'>")
      (when multivalued (output-add s *buttons*))
      (when multiple (output-delete s *buttons*))
      (format s "</td><td>")
      (output-fastdisplay-cell s slot value structure style) (crlf s)
      (format s "</td></tr>"))
    (format s "</table>")
    (crlf s)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod output-fastdisplay-cell (s slot value structure (style (eql 'selector)))
  (let (options)
    (setq options (cons 'unknown (fastoptions slot structure)))
    (output-newselector s slot options value)))

(defmethod output-fastdisplay-cell (s slot value structure (style (eql 'fastselector)))
  (output-weirdfastselector s slot value structure))

(defmethod output-fastdisplay-cell (s slot value structure (style (eql 'fancyselector)))
  ;(output-newfancyselector s slot value)
  (output-weirdfastselector s slot value structure))

(defmethod output-fastdisplay-cell (s slot value structure (style (eql 'combobox)))
  (let (options)
    (when (setq options (fastoptions slot structure))
      (setq options (cons 'unknown options))
      (output-newcombobox s slot options (prettyname value) 30))))

(defmethod output-fastdisplay-cell (s slot value structure (style (eql 'radiobutton)))
  (let (options)
    (cond ((setq options (fastoptions slot structure))
           (setq options (cons nil options))
           (output-newradiobutton s slot options value))
          (t (format-hidden s (stringize slot) "")))))

(defmethod output-fastdisplay-cell (s slot value structure (style (eql 'typein)))
  (declare (ignore structure))
  (format-newtypein s (stringize slot) (stringize value) 20))

(defmethod output-fastdisplay-cell (s slot value structure (style (eql 'stringfield)))
  (declare (ignore structure))
  (format-newstringfield s (stringize slot) (prettyname value) 20))

(defmethod output-fastdisplay-cell (s slot value structure (style (eql 'text)))
  (output-fastdisplay-cell s slot value structure 'stringfield))

(defmethod output-fastdisplay-cell (s slot value structure (style (eql 'textarea)))
  (output-fastdisplay-cell s slot value structure 'stringfield))

(defmethod output-fastdisplay-cell (s slot value structure (style (eql 'password)))
  (declare (ignore structure))
  (format-newpassword s (stringize slot) (prettyname value) 20))

(defmethod output-fastdisplay-cell (s slot value structure (style (eql 'dateinput)))
  (declare (ignore structure))
  (output-newdateinput s slot value))

(defmethod output-fastdisplay-cell (s slot value structure (style (eql 'datestyle)))
  (declare (ignore structure))
  (format-hidden s (stringize slot) (stringize value))
  (if value (output-simple s value) (format s "~A" (iconify slot))))

(defmethod output-fastdisplay-cell (s slot value structure (style (eql 'dollarinput)))
  (declare (ignore structure))
  (output-newdollarinput s slot value 6))

(defmethod output-fastdisplay-cell (s slot value structure (style (eql 'dollarstyle)))
  (declare (ignore structure))
  (format-hidden s (stringize slot) (stringize value))
  (if (realp value) (format s "$~$" value) (format s "~A" (iconify slot))))

(defmethod output-fastdisplay-cell (s slot value structure (style (eql 'emailstyle)))
  (declare (ignore structure))
  (format-hidden s (stringize slot) (stringize value))
  (if value (format s "<a href='mailto:~A'><font color='red'>~A</font></a>" value value)
      (format s "~A" (iconify slot))))

(defmethod output-fastdisplay-cell (s slot value structure (style (eql 'htmlstyle)))
  (declare (ignore structure))
  (format-hidden s (stringize slot) (stringize value))
  (if value (format s "~A" value) (format s "~A" (iconify slot))))

(defmethod output-fastdisplay-cell (s slot value structure (style (eql 'imagestyle)))
  (declare (ignore structure))
  (format-hidden s (stringize slot) (stringize value))
  (if value (format s "<img src='~A'/>" value) (format s "~A" (iconify slot))))

(defmethod output-fastdisplay-cell (s slot value structure (style (eql 'urlstyle)))
  (declare (ignore structure))
  (format-hidden s (stringize slot) (stringize value))
  (if value (format s "<a href='~A'>~A</a>" value (htmlify value)) (format s "~A" (iconify slot))))

(defmethod output-fastdisplay-cell (s slot value structure (style (eql 'glyph)))
  (declare (ignore structure))
  (format-hidden s (stringize slot) (stringize value))
  (format s "~A" (iconify slot)))

(defmethod output-fastdisplay-cell (s slot value structure (style (eql 'prettystyle)))
  (declare (ignore structure))
  (format-hidden s (stringize slot) (stringize value))
  (format s "~A" (iconify slot)))

(defmethod output-fastdisplay-cell (s slot value structure style)
  (declare (ignore structure style))
  (format-hidden s (stringize slot) (stringize value))
  (format s "~A" (iconify slot)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod fastoptions (slot structure)
  (cond ((results 'option slot *interface*))
        ((sort (findoptions slot (viewconvert structure) *gui*) #'lessp))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; fastdisplayscript
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun fastdisplayscript ()
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
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
