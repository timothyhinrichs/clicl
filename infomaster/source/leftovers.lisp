;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; (c) Copyright 1999-2007 by Michael Genesereth.  All rights reserved.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; leftovers.lisp
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(eval-when (compile load eval)
  (proclaim '(special *sender* *receiver* *client*
                      *gui* *agent* *interface* *home* *homedir* *trace*
                      *bgcolor* *border* *cells* *buttons*)))

(eval-when (compile load eval)
  (proclaim '(special *client* *library* *warehouse* *manager*)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; interface
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defparameter *relations*
  '(same distinct
    < > =< >= + - * /
    convertfromstring
    makestring
    stralphanumeric
    stringappend
    strcapitalize
    strcharpos
    strdowncase
    strelement
    strgreater
    string
    strlength
    strless
    strposition
    strmatch
    strmatchall
    strmatchany
    strmatchphrase
    strsubseq
    strsubstitute
    strupcase
    substring))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; operations
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod process (s (command (eql 'operations)) postlines)
  (declare (ignore postlines))
  (serve-document s (stringappend *homedir* (macify "/pages/operations.html"))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; apropos
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod process (s (command (eql 'apropos)) postlines)
  (let (query answers)
    (setq query (getf-post "String" postlines))
    (cond ((string= query "")
           (html-message s (format nil "Please specify a substring to retrieve.")))
          ((setq answers (aprothing query))
           (cond ((null (cdr answers)) (output-inspect s (caar answers) (cadar answers)))
                 (t (output-selection s answers))))
          (t (html-message s (format-warning nil "No matching item of known type."))))))

(defun output-apropos (s)
  (unless (findp `(nocommand ,*gui* apropos) *manager*)
    (format s "<BR>
<CENTER>
<FORM action=\"Keysearch?\">
<TABLE cellpadding=\"1\" cellspacing=\"0\" border=\"0\">
<TR height=20>
<TD bgcolor=\"#E7C73B\">&nbsp;</TD>
<TD bgcolor=\"#E7C73B\" align=\"RIGHT\" class=\"panelHeader\" nowrap>&nbsp;Search for&nbsp;</TD>
<TD bgcolor=\"#E7C73B\"><input type=\"TEXT\" size=\"25\" name=\"String\"></TD>
<TD bgcolor=\"#E7C73B\" align=\"LEFT\" nowrap=\"1\">&nbsp;<input type=\"submit\" value=\" Go \" class=\"panelText\">&nbsp;</TD>
<TD bgcolor=\"#E7C73B\">&nbsp;</TD></TR>
</FORM>
</TABLE>
</CENTER>
<BR>")))

(defun aprothing (s)
  (let (items)
    (setq items (remove-if #'varp (apropos-list (string-upcase s))))
    (setq items (request `(ask-all (?x ?y) (and (oneof ?x . ,items) ,(makisa '?x '?y *gui*)))
                         *client* *gui*))
    (do ((l items (cdr l)) (nl))
        ((null l) (nreverse nl))
        (unless (eq (caar l) (caar nl)) (setq nl (cons (car l) nl))))))

(defun output-selection (s answers)
  (format-html s) (crlf s)
  (output-head s "Choose Object") (crlf s)
  (format-body s *bgcolor*) (crlf s)
  (output-header s)
  (setq answers (sort answers #'lessp :key #'car))
  (format s "<P>Select an item from the following list.")
  (format s "<P><TABLE>")
  (dolist (answer answers)
    (format s "<TR><TD><A HREF=\"VIEW?Object=~A&Class=~A\">~A</A></TD><TD>~A</TD></TR>"
            (addressify (car answer)) (addressify (cadr answer))
            (prettify (car answer)) (prettify (cadr answer))))
  (format s "</TABLE>")
  (output-footer s)
  (finish-body s) (crlf s)
  (finish-html s) (crlf s))

(defun output-inspect (s obj class)
  (output-fastinspectpage s obj class))

(defun strreplace (s1 s2 s3)
  (let (start)
    (cond ((setq start (search s2 s3 :test #'char-equal))
           (strappend (subseq s3 0 start) s1
                      (strreplace s1 s2 (subseq s3 (+ start (length s2))))))
          (t s3))))

(defun vowelwordp (x)
  (and (symbolp x)
       (find (elt (symbol-name x) 0) '(#\A #\a #\E #\e #\I #\i #\O #\o #\U #\u))))

(defun article (x)
  (if (vowelwordp x) "an" "a"))

(defun output-single-cell (s slot value)
  (format s "<TR><TH ALIGN=LEFT VALIGN=TOP>")
  (output-slotlink s slot)
  (format s "</TH><TD>")
  (output-value s value)   ;;; (output-view s value (find-range slot))
  (format s "</TD></TR>"))

(defun viewablep (user obj)
  (or (not *security*)
      (request `(ask-if (viewable ,user ,obj)) *client* *security*)))

(defun editablep (user obj)
  (or (not *security*)
      (request `(ask-if (editable ,user ,obj)) *client* *security*)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; tuples
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod process (s (command (eql 'tuples)) postlines)
  (let (relation arity results selections)
    (format-html s) (crlf s)
    (output-head s "Save Tuples") (crlf s)
    (format-body s *bgcolor*) (crlf s)
    (output-header s)
    (multiple-value-setq (selections postlines) (getslots "Command" postlines))
    (pop postlines)
    (setq selections (mapcar #'read-value-string selections))
    (setq relation (read-value-string (cdr (pop postlines))))
    (setq arity (read-value-string (cdr (pop postlines))))
    (setq results (read-user-string (cdr (pop postlines))))
    (with-lock-grabbed (*lock*)
      (do ((i 1 (1+ i)) (rl results (cdr rl)))
          ((null rl))
          (when (equal i (car selections))
            (setq selections (cdr selections))
            (save (cons relation (car rl)) *warehouse*)))
      (save `(specialty ,(name *warehouse*) ,relation) *manager*)
      (save `(isa ,relation relation) *interface*)
      (save `(arity ,relation ,arity) *interface*)
      (save `(rootrelation warehouse ,relation) *interface*))
    (format s "<P>Rows saved in Warehouse.<P>")
    (output-footer s)
    (finish-body s) (crlf s)
    (finish-html s) (crlf s)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Add
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun save-viewable (owner obj)
  (when *security*
    (request `(tell (viewable ,owner ,obj)) *client* *security*)))

(defun drop-viewable (owner obj)
  (when *security*
    (request `(untell (viewable ,owner ,obj)) *client* *security*)))

(defun save-editable (owner obj)
  (when *security*
    (request `(tell (editable ,owner ,obj)) *client* *security*)))

(defun drop-editable (owner obj)
  (when *security*
    (request `(untell (editable ,owner ,obj)) *client* *security*)))

(defun output-open-button (s slot n)
  (when (subframep slot)
    (format s "<INPUT TYPE=SUBMIT NAME=\"~A\" VALUE=\"~A\">"
            (strappend "Command." (stringize n)) "  ")))

(defun output-close-button (s n)
  (format-button s (strappend "Command." (stringize n)) "  "))

(defun output-slot-button (s slot)
  (let (range)
    (when (setq range (findx '?c `(and (range ,slot ?c) (attribute ?c ?s)) *interface*))
      (format-button s (strappend "Command.Find." (stringize range)) "  "))))

(defun output-create-button (s slot)
  (let (range)
    (when (setq range (findx '?c `(and (range ,slot ?c) (attribute ?c ?s)) *interface*))
      (format-button s (strappend "Command.Create." (stringize range)) "  "))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ask
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod process (s (file (eql 'ask)) postlines)
  (cond ((null postlines) (process-ask-start s))
        ((string= (getf-post "Command" postlines) "Trace Execution")
         (process-ask-execution s postlines))
        (t (process-ask-answer s postlines))))

(defun process-ask-start (s)
  (format-html s) (crlf s)
  (output-head s "Ask") (crlf s)
  (format-body s *bgcolor*) (crlf s)
  (output-header s)
  (format s "<FORM ACTION=Ask? METHOD=POST>~%")
  (format s "<TEXTAREA NAME=\"Aspect\" ROWS=1 COLS=76></TEXTAREA><P><BR>")
  (format s "<TEXTAREA NAME=\"Query\" ROWS=10 COLS=76></TEXTAREA><P>")
  (format s "<HR>~%")
  (format s "<TABLE WIDTH=100%>~%")
  (format s "<TR><TD WIDTH=50% VALIGN=\"TOP\">~%")
  (unless (findp `(nocommand ,*gui* submit) *interface*)
    (format-button s "Command" "   Submit   ")
    (format s "this ACL request"))
  (format s "</TD><TD>")
  (unless (findp `(nocommand ,*gui* execution) *interface*)
    (format-button s "Command" "Trace Execution"))
  (format s "</TD><TD>")
  (format s "</TABLE>")
  (format s "</FORM>")
  (output-footer s)
  (finish-body s) (crlf s)
  (finish-html s) (crlf s))

(defun process-ask-execution (*stream* postlines)
  (let (aspect query result)
    (setq aspect (read-user-string (cdar postlines)))
    (setq query (maksand (read-sentences (cdadr postlines))))
    (format-html *stream*) (crlf *stream*)
    (output-head *stream* "Ask Execution Trace") (crlf *stream*)
    (format-body *stream* *bgcolor*) (crlf *stream*)
    (output-header *stream*)
    (force-output *stream*)
    (format *stream* "<XMP>~%")
    (let ((*trace-device* *stream*) (traceexpressions))
      (trace-expression '?x)
      (setq result (ignore-errors (prorequest `(ask-all ,aspect ,query))))
      (untrace-expression '?x))
    (format *stream* "</XMP>~%")
    (format *stream* "<HR>")
    (format *stream* "<XMP>~%")
    (print-acl *stream* result)
    (format *stream* "</XMP>~%")
    (output-footer *stream*)
    (finish-body *stream*) (crlf *stream*)
    (finish-html *stream*) (crlf *stream*)))

(defun process-ask-answer (s postlines)
  (let (aspect query)
    (setq aspect (read-user-string (cdar postlines)))
    (setq query (maksand (read-sentences (cdadr postlines))))
    (format-html s) (crlf s)
    (output-head s "Ask Result") (crlf s)
    (format-body s *bgcolor*) (crlf s)
    (output-header s)
    (force-output s)
    (format s "<XMP>~%")
    (print-acl s (ignore-errors (prorequest `(ask-all ,aspect ,query))))
    (format s "</XMP>~%")
    (output-footer s)
    (finish-body s) (crlf s)
    (finish-html s) (crlf s)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; xform
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod process (s (file (eql 'xform)) postlines)
  (cond ((null postlines) (process-xform-start s))
        (t (process-xform-submit s postlines))))

(defun process-xform-start (s)
  (format-html s) (crlf s)
  (output-head s "Ask") (crlf s)
  (format-body s *bgcolor*) (crlf s)
  (output-header s)
  (format s "<FORM ACTION=xform? METHOD=POST>~%")
  (format s "<TEXTAREA NAME=\"Condition\" ROWS=10 COLS=76></TEXTAREA><P><BR>")
  (format s "<TEXTAREA NAME=\"Conclusion\" ROWS=10 COLS=76></TEXTAREA><P>")
  (format s "<HR>~%")
  (format s "<TABLE WIDTH=100%>~%")
  (format s "<TR><TD WIDTH=50% VALIGN=\"TOP\">~%")
  (unless (findp `(nocommand ,*gui* submit) *interface*)
    (format-button s "Command" "   Submit   ")
    (format s "this ACL request"))
  (format s "</TD><TD>")
  (unless (findp `(nocommand ,*gui* execution) *interface*)
    (format-button s "Command" "Trace Execution"))
  (format s "</TD><TD>")
  (format s "</TABLE>")
  (format s "</FORM>")
  (output-footer s)
  (finish-body s) (crlf s)
  (finish-html s) (crlf s))

(defun process-xform-submit (s postlines)
  (let (condition conclusion)
    (setq condition (maksand (read-sentences (cdar postlines))))
    (setq conclusion (maksand (read-sentences (cdadr postlines))))
    (format-html s) (crlf s)
    (format-html s) (crlf s)
    (output-head s "Ask Result") (crlf s)
    (format-body s *bgcolor*) (crlf s)
    (output-header s)
    (format-body s *bgcolor*) (crlf s)
    (output-header s)
    (force-output s)
    (format s "<XMP>~%")
    (print-acl s (ignore-errors (prorequest `(update (==> ,condition ,conclusion)))))
    (format s "</XMP>~%")
    (output-footer s)
    (finish-body s) (crlf s)
    (finish-html s) (crlf s)))

(defun read-sentences (s)
  (ignore-errors
   (with-input-from-string (s s)
     (do ((sentence (read s nil) (read s nil)) (nl))
         ((null sentence) (nreverse nl))
       (setq nl (cons sentence nl))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ACL
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defparameter *stream* nil)

(defmethod process (*stream* (command (eql 'acl)) postlines)
  (let (command)
    (cond ((null postlines) (output-acl *stream*))
          ((and (setq command (getf-post "Command" postlines)) nil))
          ((string= command "Show Communication")
           (communication-acl *stream* postlines))
          ((string= command "Show Inference")
           (execution-acl *stream* postlines))
          (t (answer-acl *stream* postlines)))))

(defun output-acl (s)
  (format-html s) (crlf s)
  (output-head s "ACL Request") (crlf s)
  (format-body s *bgcolor*) (crlf s)
  (output-header s)
  (format s "<FORM ACTION=ACL? METHOD=POST>~%")
  (format s "<TEXTAREA NAME=\"Message\" ROWS=10 COLS=76></TEXTAREA><P>~%")
  (format s "<HR>~%")
  (format s "<TABLE WIDTH=100%>~%")
  (format s "<TR><TD>")
  (format-button s "Command" "   Submit   ")
  (format s "</TD></TR><TR><TD>")
  (format-button s "Command" "Show Communication")
  (format s "</TD></TR><TR><TD>")
  (format-button s "Command" "Show Inference")
  (format s "</TD></TR>")
  (format s "</TABLE>")
  (format s "</FORM>")
  (output-footer s)
  (finish-body s) (crlf s)
  (finish-html s) (crlf s))

(defun generate-acl (s x)
  (format-html s) (crlf s)
  (output-head s "ACL Request") (crlf s)
  (format-body s *bgcolor*) (crlf s)
  (output-header s)
  (format s "<FORM ACTION=ACL? METHOD=POST>~%")
  (format s "<TEXTAREA NAME=\"Message\" ROWS=10 COLS=70>")
  (print-acl s x)
  (format s "</TEXTAREA><P>~%")
  (format s "<HR>~%")
  (format s "<TABLE WIDTH=100%>~%")
  (format s "<TR><TD>")
  (format-button s "Command" "   Submit   ")
  (format s "</TD></TR><TR><TD>")
  (format-button s "Command" "Show Communication")
  (format s "</TD></TR><TR><TD>")
  (format-button s "Command" "Show Inference")
  (format s "</TD></TR>")
  (format s "</TABLE>")
  (format s "</FORM>")
  (output-footer s)
  (finish-body s) (crlf s)
  (finish-html s) (crlf s))

(defun answer-acl (s postlines)
  (let ((message (read-from-post-data "Message" postlines)))
    (format-html s) (crlf s)
    (output-head s "ACL Result") (crlf s)
    (format-body s *bgcolor*) (crlf s)
    (output-header s)
    (force-output s)
    (format s "<XMP>~%")
    (let ((*standard-output* *stream*)
          (*trace-output* *stream*)
          (*error-output* *stream*)
          (*trace-device* *stream*))
      (print-acl s (ignore-errors (prorequest message))))
    (format s "</XMP>~%")
    (output-footer s)
    (finish-body s) (crlf s)
    (finish-html s) (crlf s)))

(defun execution-acl (s postlines)
  (let ((message (read-from-post-data "Message" postlines)))
    (format-html s) (crlf s)
    (output-head s "ACL Execution Trace") (crlf s)
    (format-body s *bgcolor*) (crlf s)
    (output-header s)
    (force-output s)
    (format s "<XMP>~%")
    (let ((*trace-device* s) (traceexpressions))
      (trace-expression '?x)
      (setq message (prorequest message))
      (untrace-expression '?x))
    (format s "</XMP>~%")
    (format s "<HR>")
    (format s "<XMP>~%")
    (print-acl s message)
    (format s "</XMP>~%")
    (output-footer s)
    (finish-body s) (crlf s)
    (finish-html s) (crlf s)))

(defun communication-acl (s postlines)
  (let ((message (read-from-post-data "Message" postlines)))
    (format-html s) (crlf s)
    (output-head s "ACL Communication Trace") (crlf s)
    (format-body s *bgcolor*) (crlf s)
    (output-header s)
    (force-output s)
    (format s "<XMP>~%")
    (let ((*wiretap* s) (*print-pretty* t) (*print-length* 20))
      (prorequest message))
    (format s "</XMP>~%")
    (output-footer s)
    (finish-body s) (crlf s)
    (finish-html s) (crlf s)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; SQL
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod process (s (command (eql 'sql)) postlines)
  (cond ((null postlines) (output-sql s))
        (t (answer-sql s postlines))))

(defun output-sql (s)
  (format-html s) (crlf s)
  (output-head s "SQL Request") (crlf s)
  (format-body s *bgcolor*) (crlf s)
  (output-header s)
  (format s "<FORM ACTION=SQL? METHOD=POST>")
  (format s "<TEXTAREA NAME=\"Message\" ROWS=10 COLS=76></TEXTAREA>")
  (format s "<P><INPUT TYPE=SUBMIT VALUE=Submit> this SQL request")
  (format s "</FORM>")
  (output-footer s)
  (finish-body s) (crlf s)
  (finish-html s) (crlf s))

(defun answer-sql (s postlines)
  (let ((message (acl (getf-post "Message" postlines))))
    (format-html s) (crlf s)
    (output-head s "SQL Result") (crlf s)
    (format-body s *bgcolor*) (crlf s)
    (output-header s)
    (force-output s)
    (print-table s (request message *client* *agent*))
    (output-footer s)
    (finish-body s) (crlf s)
    (finish-html s) (crlf s)))

(defun print-table (s table)
  (format s "<P><CENTER><TABLE BORDER>")
  (dolist (row table)
    (format s "<TR>")
    (dolist (col row)
      (format s "<TD>")
      (output-value s col)
      (format s "</TD>"))
    (format s "</TR>"))
  (format s "</TABLE></CENTER><P>"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Miscellaneous
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun newrefs (value refs)
  (cond ((null refs) (list value))
        ((equalp value (car refs)) (list (car refs)))
        (t (cons (car refs) (newrefs value (cdr refs))))))

(defun find-createdefault-string (relation)
  (let ((default (find-createdefault relation)))
    (if default (prettify default) "")))

(defun find-createdefault (relation)
  (deval (findx '?v `(createdefault ,relation ?v) *interface*)))

(defun find-createdefaults (relation)
  (mapcar #'deval (finds '?v `(createdefault ,relation ?v) *interface*)))


(defun find-searchdefault-string (relation)
  (let ((default (find-searchdefault relation)))
    (if default (prettify default) "")))

(defun find-searchdefault (relation)
  (deval (findx '?v `(searchdefault ,relation ?v) *interface*)))

(defun find-searchdefaults (relation)
  (mapcar #'deval (finds '?v `(searchdefault ,relation ?v) *interface*)))


(defun find-input-string (relation)
  (let ((default (find-input relation)))
    (if default (prettify default) "")))

(defun find-input (relation)
  (deval (findx '?v `(createdefault ,relation ?v) *interface*)))

(defun find-inputs (relation)
  (mapcar #'deval (finds '?v `(createdefault ,relation ?v) *interface*)))

(defun find-default-string (relation)
  (let ((default (find-default relation)))
    (if default (prettify default) "")))

(defun find-default (relation)
  (deval (findx '?v `(searchdefault ,relation ?v) *interface*)))

(defun find-defaults (relation)
  (mapcar #'deval (finds '?v `(searchdefault ,relation ?v) *interface*)))


(defun queryable-slots (class)
  (remove-if #'(lambda (s) (findp `(searchstyle ,s noshow) *interface*))
             (finds '?s `(attribute ,class ?s) *interface*)))

(defun displayable-slots (class)
  (remove-if #'(lambda (s) (findp `(comparestyle ,s noshow) *interface*))
             (finds '?s `(attribute ,class ?s) *interface*)))

(defun sortable-slots (class)
  (remove-if #'(lambda (s) (findp `(nosort ,class ,s) *interface*))
             (finds '?s `(attribute ,class ?s) *interface*)))

(defun inspectable-slots (class)
  (remove-if #'(lambda (s) (findp `(inspectstyle ,s noshow) *interface*))
             (finds '?s `(attribute ,class ?s) *interface*)))

(defun createable-slots (class)
  (remove-if #'(lambda (s) (findp `(or (createstyle ,s noshow)
                                       (noupdate ,*gui* ,s)) *interface*))
             (finds '?s `(attribute ,class ?s) *interface*)))

(defun modifiable-slots (class)
  (remove-if #'(lambda (s) (findp `(changestyle ,s noshow) *interface*))
             (finds '?s `(attribute ,class ?s) *interface*)))

(defun subframep (slot)
  (findp `(and (range ,slot ?c) (attribute ?c ?s) (unprovable (nosearch ?c ?s)))
         *interface*))

(defun arity-of-relation (relation)
  (let (arity)
    (setq arity (findx '?x `(arity ,relation ?x) *interface*))
    (cond ((not (integerp arity)) 0)
          ((= arity -1) 0)
          (t arity))))


(defparameter *var-count* 0)
(defparameter *postlines* nil)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Edit
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod process (s (file (eql 'edit)) postlines)
  (let (command)
    (cond ((null postlines) (process-edit-start s))
          ((and (setq command (getf-post "Command" postlines)) nil))
          ((null command) (output-examine s (cdar postlines)))
          ((string-equal command "Edit") (output-editor s postlines))
          (t (output-edit s postlines)))))

(defun process-edit-start (s)
  (format-html s) (crlf s)
  (output-head s "Retrieve") (crlf s)
  (format-body s *bgcolor*) (crlf s)
  (output-header s)
  (format s "Please enter a word contained in the sentences you wish to edit:<BR>")
  (format s "<FORM ACTION=Edit? METHOD=POST>")
  (format-text s "Object" "" 40)
  (format-button s "Command" "Edit")
  (format s "</FORM>~%")
  (output-footer s)
  (finish-body s) (crlf s)
  (finish-html s) (crlf s))

(defun process-edit-edit (s name)
  (let ((obj (read-value-string name)))
    (format-html s) (crlf s)
    (output-head s (format nil "Examine ~A" (prettify obj))) (crlf s)
    (format-body s *bgcolor*) (crlf s)
    (output-header s)
    (format s "<PRE>")
    (dolist (sentence (request `(ask-about ,obj) *client* *agent*))
      (print-kif s sentence)
      (format s "~%~%"))
    (format s "</PRE>")
    (format s "<HR>")
    (format s "<FORM ACTION=EXAMINE? METHOD=POST>")
    (format-hidden s "Object" (stringize obj))
    (format-button s "Command" "Edit")
    (format s " these sentences.")
    (format s "</FORM>")
    (output-footer s)
    (finish-body s) (crlf s)
    (finish-html s) (crlf s)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Examine
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod process (s (file (eql 'examine)) postlines)
  (let (command)
    (cond ((null postlines) (output-examine-start s))
          ((and (setq command (getf-post "Command" postlines)) nil))
          ((null command) (output-examine s (cdar postlines)))
          ((string-equal command "Examine") (output-examine s (cdar postlines)))
          ((string-equal command "Edit") (output-editor s postlines))
          (t (output-edit s postlines)))))

(defun output-examine-start (s)
  (format-html s) (crlf s)
  (output-head s "Retrieve") (crlf s)
  (format-body s *bgcolor*) (crlf s)
  (output-header s)
  (format s "Please enter a word contained in the sentences you wish to examine:<BR>")
  (format s "<FORM ACTION=EXAMINE? METHOD=POST>")
  (format-text s "Object" "" 40)
  (format-button s "Command" "Examine")
  (format s "</FORM>~%")
  (output-footer s)
  (finish-body s) (crlf s)
  (finish-html s) (crlf s))

(defun output-examine (s name)
  (let ((obj (read-user-string name)))
    (format-html s) (crlf s)
    (output-head s (format nil "Examine ~A" (prettify obj))) (crlf s)
    (format-body s *bgcolor*) (crlf s)
    (output-header s)
    (format s "<PRE>")
    (dolist (sentence (facts obj *agent*))
      (print-kif s sentence)
      (format s "~%~%"))
    (format s "</PRE>")
    (format s "<HR>")
    (format s "<FORM ACTION=EXAMINE? METHOD=POST>")
    (format-hidden s "Object" (stringize obj))
    (format-button s "Command" "Edit")
    (format s " these sentences.")
    (format s "</FORM>")
    (output-footer s)
    (finish-body s) (crlf s)
    (finish-html s) (crlf s)))

(defun output-editor (s postlines)
  (let* ((obj (read-value-string (cdar postlines)))
         (sentences (facts obj *agent*)))
    (output-edits s obj sentences)))

(defun output-edits (s obj sentences)
    (format-html s) (crlf s)
    (output-head s (format nil "Edit ~A" (prettify obj))) (crlf s)
    (format-body s *bgcolor*) (crlf s)
    (output-header s)
  (format s "<FORM ACTION=EXAMINE? METHOD=POST>")
  (format-hidden s "Object" (stringize obj))
  (format-hidden s "Defaults" (stringize-kif sentences))
  (format s "<TEXTAREA NAME=\"Sentences\" ROWS=20 COLS=70>")
  (dolist (sentence sentences)
    (print-string s sentence)
    (format s "~%~%"))
  (format s "</TEXTAREA>")
  (format s "<HR>")
  (format-button s "Command" "Update")
  (format s " these sentences.")
  (format s "</FORM>~%")
  (output-footer s)
  (finish-body s) (crlf s)
  (finish-html s) (crlf s))

(defun output-edit (s postlines)
  (let (object defaults sentences error negatives)
    (setq object (read-value-string (cdr (pop postlines))))
    (setq defaults (read-from-string (cdr (pop postlines))))
    (multiple-value-setq (sentences error) (read-sentences (cdar postlines)))
    (cond (error (http-problem s "Syntax error."))
          (t (setq negatives (set-difference defaults sentences :test #'samep))
             (dolist (negative negatives) (prorequest `(untell ,negative)))
             (prorequest `(update . ,sentences))
             (output-edits s object sentences)))))

(defun print-kif (s kif)
  (cond ((atom kif) (format s "~A" (prettify-kif kif)))
        ((null (cddr kif)) (format s "~A" (prettify-kif kif)))
        ((eq '<= (car kif))
         (format s "(<= ~A" (prettify-kif (cadr kif)))
         (dolist (literal (cddr kif))
           (format s "~%    ~A" (prettify-kif literal)))
         (format s ")"))
        ((eq '=> (car kif))
         (format s "(=> ~A" (prettify-kif (cadr kif)))
         (dolist (literal (cddr kif))
           (format s "~%    ~A" (prettify-kif literal)))
         (format s ")"))
        ((eq 'defun (car kif))
         (format s "(defun ~A ~A"
                 (prettify-kif (cadr kif)) (prettify-kif (caddr kif)))
         (dolist (literal (cdddr kif))
           (format s "~%    ~A" (prettify-kif literal)))
         (format s ")"))
        (t (format s "~A" (prettify-kif kif)))))

(defun prettify-kif (kif)
  (let (*print-length* *print-level*)
    (cond ((stringp kif) (stringize kif))
          ((varp kif) (string-downcase (prin1-to-string kif)))
          ((and kif (symbolp kif))
           (setq kif (string-downcase (prin1-to-string kif)))
           (format nil "<A HREF=\"EXAMINE?Object=~A\">~A</A>" kif kif))
          ((atom kif) (prin1-to-string kif))
          (t (mapcar #'prettify-kif kif)))))

(defun print-string (s kif)
  (cond ((atom kif) (format s "~A" (stringize-kif kif)))
        ((null (cddr kif)) (format s "~A" (stringize-kif kif)))
        ((eq '<= (car kif))
         (format s "(<= ~A" (stringize-kif (cadr kif)))
         (dolist (literal (cddr kif))
           (format s "~%    ~A" (stringize-kif literal)))
         (format s ")"))
        ((eq '=> (car kif))
         (format s "(=> ~A" (stringize-kif (cadr kif)))
         (dolist (literal (cddr kif))
           (format s "~%    ~A" (stringize-kif literal)))
         (format s ")"))
        ((eq 'defun (car kif))
         (format s "(defun ~A ~A"
                 (stringize-kif (cadr kif)) (stringize-kif (caddr kif)))
         (dolist (literal (cdddr kif))
           (format s "~%    ~A" (stringize-kif literal)))
         (format s ")"))
        ((eq 'defmethod (car kif))
         (format s "(defun ~A ~A"
                 (stringize-kif (cadr kif)) (stringize-kif (caddr kif)))
         (dolist (literal (cdddr kif))
           (format s "~%    ~A" (stringize-kif literal)))
         (format s ")"))
        (t (format s "~A" (stringize-kif kif)))))

(defun stringize-kif (kif)
  (let ((*print-case* :downcase))
    (htmlify (prin1-to-string kif))))



(defun print-acl (s acl)
  (let ((*print-case* :downcase) (*print-pretty* t))
    (format s "~S" acl)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
