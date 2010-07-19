;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;
;; infomaster bug fixes
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;

;;wasn't outputing prolog
(defun html-message (s info)
  (output-prolog s 200)
  (output-header s "Message")
  (format s "<p>~A</p>" info)
  (output-footer s))


;;;;;;;;deal with favicon.ico
;; If you do a trace request, you will notice that the browser is constantly
asking for a favicon.
;; This was incorrectly treated as a request for an inspect page for
favico.ico before.
;; Note that images/favico.ico should exist (I have provided a default one
in the images directory)

(defun parse-path (path)
  "Returns: (1) Directory, (2) File, (3) Query"
  (declare (type string path))
  (when (equalp "/favicon.ico" path)
        (setq path (stringappend *home* "images/favicon.ico")))
  (let (q-pos slash-pos dir file query)
    (cond ((setq q-pos (position #\? path))
       (setq query (subseq path (1+ q-pos)))
       (setq path (subseq path 0 q-pos)))
          (t (setq query "")))
    (cond ((setq slash-pos (position #\/ path :from-end t))
           (setq dir (subseq path 0 (1+ slash-pos)))
       (setq file (subseq path (1+ slash-pos))))
          (t (setq dir "/" file path)))
    (unless (or q-pos (string-equal file ""))
      (setq query (strappend "Object=" file) file "Inspect"))
    (setq dir (decode-url-chrs dir))
    (setq file (decode-url-chrs file))
    (setq query (decode-url-chrs query))
    (values dir file query)))

(defcontent-type :application :ico "Icon" "favicon.ico" "ico")


;; added a force-output, since output was hanging on large examines
(defun output-examine (s name)
  (let ((obj (read-user-string name)))
    (output-prolog s 200)
    (output-header s "Examine ~A" (prettify obj))
    (format s "<pre>")
    (dolist (sentence (request `(ask-about ,obj) *client* *agent*))
      (print-kif s sentence)
      (format s "~%~%")
      (force-output s))
    (format s "</pre>")
    (format s "<hr/>")
    (format s "<form action='examine?' method='post'>")
    (format-hidden s "Object" (stringify obj))
    (format-button s "Command" "Edit")
    (format s " these sentences.")
    (format s "</form>")
    (output-footer s)))

;;url needs to be encoded
;;for example, consider the object _uri_http%58//www.adam.gzella
;;its encoding is _uri_http%2558%2F%2Fwww.adam.gzella
(defun prettify-kif (kif)
  (let (*print-length* *print-level*)
    (cond ((stringp kif) (stringify kif))
          ((varp kif) (string-downcase (prin1-to-string kif)))
          ((and kif (symbolp kif))
           (setq kif (string-downcase (prin1-to-string kif)))
           (format nil "<a href='examine?Object=~A'>~A</a>" (urlify kif)
kif))  ;;encode url
          ((atom kif) (prin1-to-string kif))
          (t (mapcar #'prettify-kif kif)))))

;; there was a problem with URLs being decoded twice.  The problem was that,
for example, 
;; _uri_http%2558%2F%2Fwww.adam.gzella would be decoded once as
;; _uri_http%58//www.adam.gzella and then again as
;; _uri_http://www.adam.gzella
;; below is my fix for that problem.

;;fix - don't re-decode the URL.  This is already done in parse-path.
(defun parse-post-data (line)
  (declare (type string line))
  (loop
    with start = 0
    while (and line (not (= start (length line))))
    for eq-pos = (position #\= line :start start)
    for amp-pos = (position #\& line :start start)
    when eq-pos
    collect (cons (subseq line start eq-pos)      ;;don't re-decode the URL
                  (subseq line (1+ eq-pos) amp-pos))
    do (if amp-pos (setq start (1+ amp-pos)) (setq start (length line)))))

;; but then we need to decode the url for POST data
(defun process-http (s *remote*)
  (declare (type stream s))
  (let ((firstline "") (postline "") (start) (end)
        (contentlength *max-line-size*) (type 'x-www-form-urlencoded)
        (*client* *client*) (*password* *password*)
        *cookies* *browser* (*receiver* *agent*)
        dir file query)
    (setq start (get-universal-time))
    (unless (setq firstline (get-http-line s))
      (http-problem s "Error reading first line")
      (return-from process-http) )
    (multiple-value-bind (command path protocol) (parse-request firstline)
      (unless command
    (http-problem s (format nil "Can't parse `~A'" firstline))
    (return-from process-http) )
      (when protocol (multiple-value-setq (type contentlength) (parse-header
s)))
      (cond ;((eq type 'form-data)
            ; (multiple-value-setq (dir file query) (parse-path path))
            ; (post-handler s dir file (readmultipart s contentlength)))
            ((eq type 'acl)
             (acl-handler s (read-content s contentlength)))
            ((eq type 'sql)
             (sql-handler s (read-content s contentlength)))
            ((eq type 'soap)
             (soap-handler s (read-content s contentlength)))
            (t (multiple-value-setq (dir file query) (parse-path path))
               (if (string= command "POST")
                 (setq postline (decode-url-chrs (get-http-line s
contentlength))) ;; added decode
                 (setq postline query))
               (html-handler s dir file postline)
               (when *logfile*
                 (setq end (get-universal-time))
                 (loghttp start end *remote* *browser* *client* firstline
postline)))))))


;;the html was so broken (in the form element) that in some browsers the
form wouldn't submit
(defun output-inspect-page (s object class structure)
  (output-prolog s 200)
  (output-header s "Inspect ~A" (prettify object))
  (format s "<form action='inspect?' method='post'>")
  (format-hidden s "Structure" (htmlify (prin1-to-string structure)))
  (output-inspect-structure s structure)
  (format s "<table width='100%'><tr><td width='25%' valign='top'>")
  (when (changeablep object class *gui*)
    (format s "<dl>")
    (format s "<dt>")
    (format-button s "Command" "Change")
    (format s " this ~A.</dt>" (prettify class))
    (format s "<dt>")
    (format-button s "Command" " Copy")
    (format s " this ~A.</dt>" (prettify class))
    (format s "<dt>")
    (format-button s "Command" "Delete")
    (format s " this ~A.</dt>" (prettify class))
    (format s "</dl>"))
  (unless (findp `(nocommand ,*gui* inspector) *interface*)
    (format s "</td><td width='25%' valign='top'>")
    (format-button s "Command" "Textual Inspector"))
  (unless (findp `(nocommand ,*gui* convert) *interface*)
    (format s "</td><td width='25%' valign='top'>")
    (format-button s "Command" "Convert to KIF")
    (format s "<br/>")
    (format-button s "Command" "Convert to iKIF")
    (format s "<br/>")
    (format-button s "Command" "Convert to XML"))
  (format s "</td><td width='25%' valign='top'>")
  (unless (findp `(nocommand ,*gui* memory) *interface*)
    (format s "<dl><dt>")
    (format-top-button s "Command" "Save")
    (format-top-button s "Command" "Drop")
    (format-top-button s "Command" "Pipe")
    (format s "</dt><dt>")
    (format s "<select name='Target'>")
    (dolist (option (getbaskets))
      (format s "<option>~A</option>" (stringify option)))
    (format s "</select>")
    (format s "</dt></dl>"))
  (format s "</td><td><br/></td></tr></table>")
  (format s "</form>")
  (output-footer s))


;;infomaster wasn't properly handling sidelines.  I had to change (source p)
to (newsource p).
(defun facilitateask (x p receiver)
  (let (library residues (*ancestry* 1))
    (setq library (get-rulebase receiver))
    (decludes 'infotheory)
    (empty 'infotheory)
    (includes 'infotheory library)
    (dolist (x (contrapositives `(<= (answer ,x) ,p))) (insert x
'infotheory))
    (when (setq residues (fullresidues `(answer ,x) 'infotheory #'sidelinep
#'specialp))
      (setq p (decolonize (maksor residues)))
      (if *collapse* (setq p (collapse x p)))
      ;(if *collapse* (setq p (raisin x p)))
      (newsource p))))  ;;source p


;;;
;;; new datestyle using calendar popup
;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;
;;; calendar
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;

;; this command is new.  Note that pages/calendar.html must exist
(defmethod process (s (command (eql 'calendar)) postlines)
  (declare (ignore postlines))
  (serve-document s (stringappend *homedir* (macify (stringappend *home*
"pages/calendar.html")))))

;; javsascript can't handle numbers for names.  Therefore, added id field so
that calendar
;; javascript will work.
(defun format-text (s name value size)
  (if (string= value "")
      (format s "<input type='text' name='~A' size='~A' id='id~A'/>" name
size name)
      (format s "<input type='text' name='~A' value='~A' size='~A'
id='id~A'/>"
              name value size name)))

(defun output-change-datestyle (s slot value)
  (format-hidden s "Start" (stringify *cells*))
  (format-hidden s "Style" "Datestyle")
  (format-hidden s "Slot" (stringify slot))
  (format-text s (stringify *cells*) (stringify value) 14)
  (format s "&nbsp;&nbsp;<a
href=\"javascript:window.open('calendar?dateField=id~A&today=' +
document.form1.id~A.value, 'cal',
'width=275,height=200,resizable=1,status=1,menubar=0,scrollbars=0,fullscreen
=0').focus();\"><img alt='Show calendar' src='~Aimages/calendar.gif'
border='0' /></a>" (stringify *cells*) (stringify *cells*) *home*)
  (format-hidden s "End" ""))

(defun output-search-datestyle (s slot value)
  (format-hidden s "Start" (stringify *cells*))
  (format-hidden s "Style" "Datestyle")
  (format-hidden s "Slot" (stringify slot))
  (format-text s (stringify *cells*) (stringify value) 14)
  (format s "&nbsp;&nbsp;<a
href=\"javascript:window.open('calendar?dateField=id~A&today=' +
document.form1.id~A.value, 'cal',
'width=275,height=200,resizable=1,status=1,menubar=0,scrollbars=0,fullscreen
=0').focus();\"><img alt='Show calendar' src='~Aimages/calendar.gif'
border='0' /></a>" (stringify *cells*) (stringify *cells*) *home*)
  (format-hidden s "End" ""))

(defun output-refind-datestyle (s slot value)
  (format-hidden s "Start" (stringify *cells*))
  (format-hidden s "Style" "Datestyle")
  (format-hidden s "Slot" (stringify slot))
  (format-text s (stringify *cells*) (stringify value) 14)
  (format s "&nbsp;&nbsp;<a
href=\"javascript:window.open('calendar?submit=yes&dateField=id~A&today=' +
document.form1.id~A.value, 'cal',
'width=275,height=200,resizable=1,status=1,menubar=0,scrollbars=0,fullscreen
=0').focus();\"><img alt='Show calendar' src='~Aimages/calendar.gif'
border='0' /></a>" (stringify *cells*) (stringify *cells*) *home*)
  (format-hidden s "End" ""))

(defun output-argument-datestyle (s slot value)
  (format-hidden s "Start" (stringify *cells*))
  (format-hidden s "Style" "Datestyle")
  (format-hidden s "Slot" (stringify slot))
  (format-text s (stringify *cells*) (stringify value) 14)
  (format s "&nbsp;&nbsp;<a
href=\"javascript:window.open('calendar?dateField=id~A&today=' +
document.form1.id~A.value, 'cal',
'width=275,height=200,resizable=1,status=1,menubar=0,scrollbars=0,fullscreen
=0').focus();\"><img alt='Show calendar' src='~Aimages/calendar.gif'
border='0' /></a>" (stringify *cells*) (stringify *cells*) *home*)
  (format-hidden s "End" ""))

;;added datestyle
(defun output-refind-cell (s style slot value class)
  (cond ((eq style 'multichoicelist) (output-refind-menu s slot value))
        ((eq style 'dropdownlist) (output-refind-selector s slot value
class))
        ((eq style 'hierarchicalselector) (output-refind-multiselector s
slot value))
        ((eq style 'checkbox) (output-refind-checkbox s slot value))
        ((eq style 'radiobutton) (output-refind-radiobutton s slot value))
        ((eq style 'interval) (output-refind-interval s slot value))
        ((eq style 'textarea) (output-refind-textarea s slot value))
        ((eq style 'stringfield) (output-refind-stringfield s slot value))
        ((eq style 'text) (output-refind-text s slot value))
        ((eq style 'password) (output-refind-password s slot value))
        ((eq style 'urlstyle) (output-refind-text s slot value))
        ((eq style 'datestyle) (output-refind-datestyle s slot value))
        ((eq style 'glyph) (output-refind-glyph s slot value))
        (t (output-refind-typein s slot value))))

;;replace old datestyle parser
(defun parsedatestyle (postlines)
  (let (slot value)
    (pop postlines)
    (pop postlines)
    (setq slot (read-value-string (cdr (pop postlines))))
    (setq value (cdr (pop postlines)))
    (pop postlines)
    (cond ((string= value "") (setq value (list slot)))
          (t (setq value (list slot (readas value (find-range slot))))))
    (values value postlines)))

;;added datestyle / fixed interval / stringfield
(defun output-search-cell (s style slot value class)
  (cond ((eq style 'multichoicelist) (output-search-menu s slot value))
        ((eq style 'dropdownlist) (output-search-selector s slot value
class))
        ((eq style 'hierarchicalselector) (output-search-multiselector s
slot value))
        ((eq style 'checkbox) (output-search-checkbox s slot value))
        ((eq style 'radiobutton) (output-search-radiobutton s slot value))
        ((eq style 'interval) (output-search-interval s slot value))
        ((eq style 'textarea) (output-search-textarea s slot value))
        ((eq style 'stringfield) (output-search-stringfield s slot value))
        ((eq style 'text) (output-search-text s slot value))
        ((eq style 'urlstyle) (output-search-text s slot value))
        ((eq style 'password) (output-search-password s slot value))
        ((eq style 'datestyle) (output-search-datestyle s slot value))
        ((eq style 'glyph) (output-search-glyph s slot value))
        (t (output-search-typein s slot value))))

;;didn't exist before
(defun output-search-interval (s slot value)
  (let (min max)
    (cond ((and (listp value) (eq 'between (car value)))
           (setq min (cadr value) max (caddr value)))
          (t (setq min 'unknown max 'unknown)))
    (format-hidden s "Start" (stringify *cells*))
    (format-hidden s "Style" "Interval")
    (format-hidden s "Slot" (stringify slot))
    (format s "At least ")
    (format-text s "Min" (stringify min) 10)
    (format s " and at most ")
    (format-text s "Max" (stringify max) 10)
    (format-hidden s "End" "")))

;;didn't exist before
(defun output-search-stringfield (s slot value)
  (let (match)
    (cond ((and (listp value) (eq 'substring (car value)))
             (setq value (cadr value) match nil))
          ((eq 'unknown value) (setq value "" match nil))
          (t (setq match t)))
    (format-hidden s "Start" (stringify *cells*))
    (format-hidden s "Style" "Stringfield")
    (format-hidden s "Slot" (stringify slot))
    (format-text s (stringify *cells*) value 30)
    (format-checkbox s "Match" "Exact?" match)
    (format-hidden s "End" "")))

;;the cond was completely broken
(defun output-refind-stringfield (s slot value)
  (let (match)
    (cond ((and (listp value) (eq 'substring (car value)))
             (setq value (cadr value) match nil))
          ((eq 'unknown value) (setq value "" match nil))
          (t (setq match t)))
    (format-hidden s "Start" (stringify *cells*))
    (format-hidden s "Style" "Stringfield")
    (format-hidden s "Slot" (stringify slot))
    (format-text s (stringify *cells*) value 30)
    (format-checkbox s "Match" "Exact?" match)
    (format-hidden s "End" "")))

;;now handles multiple selections
(defun output-refind-menu (s slot value)
  (format-hidden s "Start" (stringify *cells*))
  (format-hidden s "Style" "Menu")
  (format-hidden s "Slot" (stringify slot))
  (cond ((null value) (setq value '(unknown)))
        ((eq value 'unknown) (setq value '(unknown)))
        ((atom value) (setq value (list value)))
        (t (setq value (cdr value))))
  (output-menu-result s *cells* (find-values slot) value)
  (format-hidden s "End" ""))

;;now handles multiple selections
(defun output-refind-checkbox (s slot value)
  (format-hidden s "Start" (stringify *cells*))
  (format-hidden s "Style" "Checkbox")
  (format-hidden s "Slot" (stringify slot))
  (cond ((null value) (setq value '(unknown)))
        ((eq value 'unknown) (setq value '(unknown)))
        ((atom value) (setq value (list value)))
        (t (setq value (cdr value))))
  (output-checkbox-result s *cells* (find-values slot) value)
  (format-hidden s "End" ""))

;;now handles multiple selections
(defun output-search-menu (s slot value)
  (format-hidden s "Start" (stringify *cells*))
  (format-hidden s "Style" "Menu")
  (format-hidden s "Slot" (stringify slot))
  (cond ((null value) (setq value '(unknown)))
        ((eq value 'unknown) (setq value '(unknown)))
        ((atom value) (setq value (list value)))
        (t (setq value (cdr value))))
  (output-menu s *cells* (find-values slot) value)
  (format-hidden s "End" ""))

;;now handles multiple selections
(defun output-search-checkbox (s slot value)
  (format-hidden s "Start" (stringify *cells*))
  (format-hidden s "Style" "Checkbox")
  (format-hidden s "Slot" (stringify slot))
  (cond ((null value) (setq value '(unknown)))
        ((eq value 'unknown) (setq value '(unknown)))
        ((atom value) (setq value (list value)))
        (t (setq value (cdr value))))
  (output-checkboxes s *cells* (find-values slot) value)
  (format-hidden s "End" ""))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;
;; subframes
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;

;; There is no point on having a subframe button if the class doesn't have
;; any searchable slots (for example, strings).  This just leads to user
confusion,
;; since you get a subframe that says "a String that satisfies the following
conditions"
;; with no following conditions.  When I first saw this I was really
confused.  So,
;; I created a function which first makes sure that the subframe has some
searchable
;; attribute.
;;
;; Also, the trash button was being used instead of the removal button for
subframes.

(defun queryable-slot (class)
   (findx '?s `(and (attribute ,class ?s) (not (searchstyle ?s noshow)))
*interface*))

(defun output-refind-structure (s x)
  (let (flat)
    (setq flat (findp `(nocommand ,*gui* subframe) *interface*))
    (format-hidden s "Start" "")
    (format-hidden s "Object" (stringify (car x)))
    (format-hidden s "Class" (stringify (cadr x)))
    (format s "<p><table cellspacing='3'>")
    (do ((l (cddr x) (cdr l)) (multivalued) (multiple) (style) (flag))
        ((null l))
      (format s "<tr><th align='left' valign='top'>")
      (unless (eq (caar l) flag)
        (setq multivalued (find-multivalued (caar l)))
        (setq style (find-searchstyle (caar l)))
        (output-slotlink s (caar l)))
      (setq multiple (or (eq (caar l) flag) (eq (caar l) (caadr l))))
      (setq flag (caar l))
      (cond ((null (cdar l))
             (format s "</th><td valign='top'>")
             (output-another-button s (incf *buttons*) multivalued)
             (output-removal-button s (incf *buttons*) multiple)
             (if (or flat (not (queryable-slot (find-range (caar l)))))
                (incf *buttons*) (output-plus-button s (incf *buttons*)))
             (format s "</td><td>")
             (output-refind-cell s style (caar l) 'unknown (cadr x))
             (incf *cells*))
            ((or (atom (cadar l)) (find (caadar l) '(oneof taxonomy between
substring)))
             (format s "</th><td valign='top'>")
             (output-another-button s (incf *buttons*) multivalued)
             (output-removal-button s (incf *buttons*) multiple)
             (if (or flat (not (queryable-slot (find-range (caar l)))))
                (incf *buttons*) (output-plus-button s (incf *buttons*)))
             (format s "</td><td>")
             (output-refind-cell s style (caar l) (cadar l) (cadr x))
             (incf *cells*))
            (t (format s "</th><td valign='top'>")
               (output-snow-button s (incf *buttons*))
               ;(output-trash-button s (incf *buttons*))
               (output-removal-button s (incf *buttons*) multiple) ;;patched
mak
               (output-minus-button s (incf *buttons*))
               (format s "</TD><TD>")
               (format s "<table border='1'><tr><td>")
               (format-hidden s "Start" "")
               (format-hidden s "Style" "Subframe")
               (format-hidden s "Slot" (stringify (caar l)))
               (format s "~A <b>" (article (cadr (cadar l))))
               (output-classlink s (cadr (cadar l)))
               (format s "</b> that satisfies the following criteria<br/>")
               (output-refind-structure s (cadar l))
               (format s "</td></tr></table>")))
      (format s "</td></tr>")
      (crlf s))
    (format s "</table>")
    (format-hidden s "End" "")))

;; same fixes here
(defun output-search-structure (s x)
  (let (flat)
    (setq flat (findp `(nocommand ,*gui* subframe) *interface*))
    (format-hidden s "Start" "")
    (format-hidden s "Object" (stringify (car x)))
    (format-hidden s "Class" (stringify (cadr x)))
    (format s "<p><table cellspacing='3'>")
    (do ((l (cddr x) (cdr l)) (multivalued) (multiple) (style) (flag))
        ((null l))
      (format s "<tr><th align='left' valign='top'>")
      (unless (eq (caar l) flag)
        (setq multivalued (find-multivalued (caar l)))
        (setq style (find-searchstyle (caar l)))
        (output-slotlink s (caar l)))
      (setq multiple (or (eq (caar l) flag) (eq (caar l) (caadr l))))
      (setq flag (caar l))
      (cond ((null (cdar l))
             (format s "</th><td valign='top'>")
             (output-another-button s (incf *buttons*) multivalued)
             (output-removal-button s (incf *buttons*) multiple)
             (if (or flat (not (queryable-slot (find-range (caar l)))))
;;patched mak
                 (incf *buttons*) (output-plus-button s (incf *buttons*)))
             (format s "</td><td>")
             (output-search-cell s style (caar l) 'unknown (cadr x))
             (incf *cells*))
            ((or (atom (cadar l)) (find (caadar l) '(oneof taxonomy between
substring)))
             (format s "</th><td valign='top'>")
             (output-another-button s (incf *buttons*) multivalued)
             (output-removal-button s (incf *buttons*) multiple)
             (if (or flat (not (queryable-slot (find-range (caar l)))))
;;patched mak
                 (incf *buttons*) (output-plus-button s (incf *buttons*)))
             (format s "</td><td>")
             (output-search-cell s style (caar l) (cadar l) (cadr x))
             (incf *cells*))
            (t (format s "</th><td valign='top'>")
               (output-snow-button s (incf *buttons*))
               ;(output-trash-button s (incf *buttons*))
               (output-removal-button s (incf *buttons*) multiple) ;;patched
mak
               (output-minus-button s (incf *buttons*))
               (format s "</td><td>")
               (format s "<table border="1"><tr><td>")
               (format-hidden s "Start" "")
               (format-hidden s "Style" "Subframe")
               (format-hidden s "Slot" (stringify (caar l)))
               (format s "~A <b>" (article (cadr (cadar l))))
               (output-classlink s (cadr (cadar l)))
               (format s "</b> that satisfies the following criteria<br/>")
               (output-search-structure s (cadar l))
               (format s "</td></tr></table>")))
      (format s "</td></tr>")
      (crlf s))
    (format s "</table>")
    (format-hidden s "End" "")))

;; some missing content types

(defcontent-type :text :xsl "XML Stylesheet Language (XSL) file" "html.gif"
         "xsl")

(defcontent-type :application :javascript "Javascript source file"
         "javascript-icon.gif" "js")

;;not a bug fix, but a nice shorthand
(defun http-servers (port &optional (n 10))
  (tcp-servers #'http-handler port n))