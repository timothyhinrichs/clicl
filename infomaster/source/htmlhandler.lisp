;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; (c) Copyright 1999-2008 by Michael Genesereth.  All rights reserved.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; htmlhandler.lisp
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(eval-when (compile load eval)
  (proclaim '(special *imageprefix* *remote* *browser* *client* *password*
                      *buttons* *cells* *manager*)))

(defparameter *gui* 'standard)
(defparameter *agent* nil)
(defparameter *interface* nil)
(defparameter *security* nil)
(defparameter *home* "/docserver/")
(defparameter *homedir* "/")
(defparameter *bgcolor* "WHITE")
(defparameter *border* "0")
(defparameter *icons* t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; html-handler
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun html-handler (s dir file postline type)
  (cond ((eq *security* 'newhandler) (html-newhandler s dir file postline type))
        (t (html-oldhandler s dir file (parsepostdata postline)))))

(defun html-oldhandler (s dir file postlines)
  (let (*gui* *security* *interface* *agent*)
    (cond ((string= dir "/") (setq *gui* 'standard))
          ((startstringp "/docserver/" dir)
           (setq dir (subseq dir 10))
           (setq *gui* 'docserver))
          (t (setq *gui* (string-left-trim '(#\/) dir))
             (setq *gui* (string-right-trim '(#\/) *gui*))
             (setq *gui* (read-from-string *gui*))))
    (setq *interface* (find-metadata *gui*))
    (setq *security* (find-security *gui*))
    (setq *agent* *gui*)
    (cond ((eq *gui* 'docserver)
	   (serve-document s (stringappend *homedir* dir (cdar postlines))))
          ((not *security*) (htmleval s (read-value-string file) postlines))
          ((not (accessiblep *gui* *remote*))
           (http-problem s "This interface is not accessible from your IP address."))
          ((not (chkpwd *client* *password*))
           (format s "HTTP/1.0 401 Unauthorized") (crlf s)
           (format s "WWW-Authenticate: Basic realm=\"Infomaster\"") (crlf s)
           (format s "Set-Cookie: ~A=~D; path=/" (addressify *gui*) (get-universal-time)) (crlf s)
           (crlf s)
           (html-message s "Bad password."))
          (t (htmleval s (read-value-string file) postlines)))))

(defun html-newhandler (s dir file postline type)
  (let (*gui* *security* *interface* *agent* user command postlines)
    (cond ((string= dir "/") (setq *gui* 'standard))
          ((startstringp "/docserver/" dir)
           (setq dir (subseq dir 10))
           (setq *gui* 'docserver))
          (t (setq *gui* (string-left-trim '(#\/) dir))
             (setq *gui* (string-right-trim '(#\/) *gui*))
             (setq *gui* (read-from-string *gui*))))
    (setq *interface* (find-metadata *gui*))
    (setq *security* (or (find-security *gui*) *gui*))
    (setq *agent* *gui*)
    (when (setq user (assoc "user" *cookies* :test #'equal))
      (setq *client* (read-user-string (cdr user)))
      (setq *password* (cdr (assoc "nonce" *cookies* :test #'equal))))
    (unless (chknonce *client* *password*) (setq *client* 'anonymous))
    (cond ((equalp file "") (setq command 'toplevel))
          (t (setq command (read-value-string file))))
    (setq postlines (parsepostdata postline))
    (if (eq type 'form-data) (setq postlines postline) (setq postlines (parsepostdata postline)))
    (cond ((eq *gui* 'docserver)
	   (serve-document s (stringappend *homedir* dir (cdar postlines))))
          ((eq command 'signin) (process s 'signin postlines))
          ((eq command 'signout) (process s 'signout postlines))
          ((eq command 'signup) (process s 'signup postlines))
          ((eq command 'forgotpassword) (process s 'forgotpassword postlines))
          ((not (accessiblep *gui* *remote*))
           (http-problem s "This interface is not accessible from your IP address."))
          ((chkpermissionp *client* command) (htmleval s command postlines))
          ((eq *client* 'anonymous)
           (setq file (stringappend dir file "?" postline))
           (setq file (stringappend "signin?location=" (urlify file)))
           (redirect s file))
          (t (http-problem s "Not authorized.")))))

(defun parsepostdata (line)
  (do ((start 0) (eqpos) (amppos) (nl))
      ((= start (length line)) (nreverse nl))
      (setq eqpos (position #\= line :start start))
      (setq amppos (position #\& line :start start))
      (when eqpos
        (setq nl (cons (cons (decode-url-chrs (subseq line start eqpos))
                             (decode-url-chrs (subseq line (1+ eqpos) amppos)))
                       nl)))
      (if amppos (setq start (1+ amppos)) (setq start (length line)))))

(defun accessiblep (interface ip)
  (triplep 'accessible interface ip *security*))

(defun chkpwd (client pwd)
  (triplep 'pwd client pwd *security*))

(defvar *content* nil "allows communication between process-cookies and process")
(defmethod htmleval (s command postlines)
  (let (*content*)  ; for thread safety
    (output-prolog s 200 *cookies* (process-cookies command postlines))
    (process s command postlines)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; process
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod process-cookies (command postlines)
  (declare (ignore command postlines))
  *cookies*)

(defmethod process (s command postlines)
  (cond ((null command) (process s 'toplevel postlines))
        (t (process-execute s command postlines))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; favicon
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod process (s (command (eql 'favicon)) postlines)
  (declare (ignore postlines))
  (format s " ")
  'done)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; toplevel
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod process (s (command (eql 'toplevel)) postlines)
  (declare (ignore postlines))
  (let (page)
    (setq page (find-frontpage *gui*))
    (cond (page (princ page s) 'done)
          (t (html-message s "Hello.")))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; execute
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod process (s (command (eql 'run)) postlines)
  (cond ((null postlines) (http-problem s "No action specified."))
        ((null (cdr postlines))
         (process-execute-start s (read-user-string (cdar postlines))))))

(defun process-execute (s action postlines)
  (cond ((null postlines) (process-execute-start s action))
        ((string= (getf-post "Command" postlines) "Submit")
         (process-execute-answer s action postlines))
        (t (process-execute-revision s action postlines))))

(defun process-execute-start (s action)
  (let ((*cells* 1))
    (output-action-page s action (create-action-args action))))

(defun process-execute-answer (s action postlines)
  (let (kif)
    (format-html s) (crlf s)
    (output-head s "Result") (crlf s)
    (format-body s *bgcolor*) (crlf s)
    (output-header s)
    (setq kif (convert-template postlines))
    (format s "~A" (apply action kif))
    (output-footer s)
    (finish-body s) (crlf s)
    (finish-html s) (crlf s)))

(defun process-execute-revision (s action postlines)
  (let ((*cells* 1))
    (output-action-page s action (convert-action-args postlines))))

(defun create-action-args (action)
  (do ((args (find-arguments action) (cdr args)) (dum) (nl))
      ((null args) (nreverse nl))
      (cond ((setq dum (find-argumentdefault (car args)))
             (setq nl (cons (list (car args) dum) nl)))
            (t (setq nl (cons (list (car args)) nl))))))

(defun convert-action-args (postlines)
  (do ((slot) (style) (value) (nl))
      ((or (null postlines) (not (string= (caar postlines) "Start")))
       (nreverse nl))
      (pop postlines)
      (setq style (read-value-string (cdr (pop postlines))))
      (setq slot (read-value-string (cdr (pop postlines))))
      (cond ((find style '(stringfield text textarea password))
             (setq value (cdr (pop postlines))))
            (t (setq value (readas (cdr (pop postlines)) (find-range slot)))))
      (setq nl (cons (list slot value) nl))
      (pop postlines)))

(defun convert-template (postlines)
  (do ((slot) (style) (value) (nl))
      ((or (null postlines) (not (string= (caar postlines) "Start")))
       (nreverse nl))
      (pop postlines)
      (setq style (read-value-string (cdr (pop postlines))))
      (setq slot (read-value-string (cdr (pop postlines))))
      (cond ((find style '(stringfield text textarea password))
             (setq value (cdr (pop postlines))))
            (t (setq value (readas (cdr (pop postlines)) (find-range slot)))))
      (setq nl (cons value nl))
      (pop postlines)))

(defun output-action-page (s action args)
  (format-html s) (crlf s)
  (output-head s (format nil "Execute ~A" (prettify action))) (crlf s)
  (format-body s *bgcolor*) (crlf s)
  (output-header s)
  (format s "<form action='~A?' method='post'>" (addressify action))
  (output-action-structure s action args)
  (format-button s "Command" "Submit")
  (format s "</form>")
  (output-footer s)
  (finish-body s) (crlf s)
  (finish-html s) (crlf s))

(defun output-action-structure (s action args)
  (format s "<p>Run action <b>~A</b> on the following arguments.</p>" (prettify action))
  (format s "<p><table cellspacing=\"3\">")
  (do ((l args (cdr l)) (style))
      ((null l))
      (setq style (find-argumentstyle (caar l)))
      (format s "<tr><th align=\"left\" valign=\"top\">")
      (output-slotlink s (caar l))
      (format s "</th><td valign=\"top\">")
      (format s "</td><td>")
      (output-argument-cell s style (caar l) (or (cadar l) 'unknown))
      (format s "</td>")
      (format s "</tr>")
      (crlf s))
  (format s "</table>")
  (format-hidden s "End" ""))

(defun output-argument-cell (s style slot value)
  (cond ((eq style 'multichoicelist) (output-argument-menu s slot value))
        ((eq style 'menu) (output-argument-menu s slot value))
        ((eq style 'dropdownlist) (output-argument-selector s slot value))
        ((eq style 'selector) (output-argument-selector s slot value))
        ((eq style 'hierarchicalselector) (output-argument-multiselector s slot value))
        ((eq style 'checkbox) (output-argument-checkbox s slot value))
        ((eq style 'radiobutton) (output-argument-radiobutton s slot value))
        ;((eq style 'datestyle) (output-argument-datestyle s slot value))
        ((eq style 'subframe) (output-argument-typein s slot value))
        ((eq style 'interval) (output-argument-typein s slot value))
        ((eq style 'textarea) (output-argument-textarea s slot value))
        ((eq style 'stringfield) (output-argument-text s slot value))
        ((eq style 'text) (output-argument-text s slot value))
        ((eq style 'filestyle) (output-argument-filestyle s slot value))
        ((eq style 'urlstyle) (output-argument-text s slot value))
        ((eq style 'emailstyle) (output-argument-text s slot value))
        ((eq style 'password) (output-argument-password s slot value))
        ((eq style 'glyph) (output-argument-glyph s slot value))
        (t (output-argument-typein s slot value))))

(defun output-argument-menu (s slot value)
  (format-hidden s "Start" (stringize *cells*))
  (format-hidden s "Style" "Menu")
  (format-hidden s "Slot" (stringize slot))
  (output-menu s *cells* (find-choices slot) (list value))
  (format-hidden s "End" ""))

(defun output-argument-selector (s slot value)
  (format-hidden s "Start" (stringize *cells*))
  (format-hidden s "Style" "Selector")
  (format-hidden s "Slot" (stringize slot))
  (output-selector s *cells* (find-choices slot) value)
  (format-hidden s "End" ""))

(defun output-argument-multiselector (s slot value)
  (let (references options)
    (format-hidden s "Start" (stringize *cells*))
    (format-hidden s "Style" "Multiselector")
    (format-hidden s "Slot" (stringize slot))
    (cond ((atom value) (setq references (find-references slot value)))
          (t (setq references (cdr value) value (car (last value)))))
    (cond ((null references) (setq options (find-choices slot)))
          (t (setq options (find-components slot (car (last references)) references))))
    (dolist (ref references) (format-hidden s "Reference" (stringize ref)))
    (output-selector s *cells* (nconc references (cons 'unknown options)) value)
    (format-button s "Command" "Expand")
    (format-hidden s "End" "")))

(defun output-argument-checkbox (s slot value)
  (format-hidden s "Start" (stringize *cells*))
  (format-hidden s "Style" "Checkbox")
  (format-hidden s "Slot" (stringize slot))
  (output-checkboxes s *cells* (find-choices slot) (list value))
  (format-hidden s "End" ""))

(defun output-argument-radiobutton (s slot value)
  (format-hidden s "Start" (stringize *cells*))
  (format-hidden s "Style" "Radiobutton")
  (format-hidden s "Slot" (stringize slot))
  (output-radiobuttons s *cells* (find-choices slot) value)
  (format-hidden s "End" ""))
#|
(defun output-argument-datestyle (s slot value)
  (let (month date year)
    (if (datep value)
      (multiple-value-setq (month date year) (deconstructdate value))
      (setq month 'unknown date 'unknown year 'unknown))
    (format-hidden s "Start" (stringize *cells*))
    (format-hidden s "Style" "Datestyle")
    (format-hidden s "Slot" (stringize slot))
    (output-selector s *cells* *months* month)
    (output-selector s *cells* *dates* date)
    (output-selector s *cells* *years* year)
    (format-hidden s "End" "")))
|#
(defun output-argument-textarea (s slot value)
  (when (eq value 'unknown) (setq value ""))
  (format-hidden s "Start" (stringize *cells*))
  (format-hidden s "Style" "Textarea")
  (format-hidden s "Slot" (stringize slot))
  (format-textarea s (stringize *cells*) (htmlify value) 8 60)
  (format-hidden s "End" ""))

(defun output-argument-text (s slot value)
  (when (eq value 'unknown) (setq value ""))
  (format-hidden s "Start" (stringize *cells*))
  (format-hidden s "Style" "Text")
  (format-hidden s "Slot" (stringize slot))
  (format-text s (stringize *cells*) (htmlify value) 40)
  (format-hidden s "End" ""))

(defun output-argument-filestyle (s slot value)
  (when (eq value 'unknown) (setq value ""))
  (format-hidden s "Start" (stringize *cells*))
  (format-hidden s "Style" "Text")
  (format-hidden s "Slot" (stringize slot))
  (format s "<INPUT TYPE=FILE ACCEPT=\"*\" SIZE=\"40\" NAME=\"Filecontent\" VALUE=\"\">")
  (format-hidden s "End" ""))

(defun output-argument-password (s slot value)
  (format-hidden s "Start" (stringize *cells*))
  (format-hidden s "Style" "Password")
  (format-hidden s "Slot" (stringize slot))
  (format-password s (stringize *cells*) (htmlify value) 40)
  (format-hidden s "End" ""))

(defun output-argument-typein (s slot value)
  (format-hidden s "Start" (stringize *cells*))
  (format-hidden s "Style" "Typein")
  (format-hidden s "Slot" (stringize slot))
  (format-text s (stringize *cells*) (stringize value) 40)
  (format-hidden s "End" ""))

(defun output-argument-glyph (s slot value)
  (format-hidden s "Start" (stringize *cells*))
  (format-hidden s "Style" "Glyph")
  (format-hidden s "Slot" (stringize slot))
  (format-hidden s "Value" (stringize value))
  (output-value s value)
  (format-hidden s "End" ""))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; output routines
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; head
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun output-head (s str)
  (format s "<head><title>")
  (format s str)
  (format s "</title></head>"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; title
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun output-title (s str)
  (format s "<title>")
  (format s "~A" str)
  (format s "</title>"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; header and footer
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun output-header (s)
  (format s (find-header *gui*)))

(defun output-footer (s)
  (format s (find-footer *gui*)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; forms
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun output-menu (s slot options values)
  (when options
    (format s "<select name='~A' size='~D' multiple='true'>"
            (stringize slot) (menu-size (length options)))
    (dolist (option options)
      (cond ((find option values :test #'equalp)
             (format s "<option value='~A' selected='true'>~A</option>"
                     (stringize option) (prettyname option)) (crlf s))
            (t (format s "<option value='~A'>~A</option>"
                       (stringize option) (prettyname option)) (crlf s))))
    (format s "</select>")))

(defun output-selector (s slot options value &optional action)
  (if action
      (format s "<select name='~A' onChange='~A'>" (stringize slot) action)
      (format s "<select name='~A'>" (stringize slot)))
  (dolist (option options)
    (cond ((equalp option value)
           (format s "<option value='~A' selected='true'>~A</option>"
                   (stringize option) (prettyname option)) (crlf s))
          (t (format s "<option value='~A'>~A</option>"
                     (stringize option) (prettyname option)) (crlf s))))
  (format s "</select>"))

(defun output-checkboxes (s slot options values)
  (format s "<dl>")
  (dolist (option options)
    (format s "<dt>")
    (format-checkbox s (stringize slot) (stringize option)
                     (find option values :test #'equalp)))
  (format s "</dl>"))

(defun output-radiobuttons (s slot options value)
  (when options
    (format s "<dl>")
    (format s "<dt>")
    (format-radiobutton s (stringize slot) (stringize (car options))
                        (equal (car options) value))
    (dolist (option (cdr options))
      (format s "<dt>")
      (format-radiobutton s (stringize slot) (stringize option)
                          (equal option value)))
    (format s "</dl>")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; format routines
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; html
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun format-html (s)
  (format s "<html>"))

(defun finish-html (s)
  (format s "</html>"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; head
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun format-head (s)
  (format s "<head>"))

(defun finish-head (s)
  (format s "</head>"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; title
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun format-title (s)
  (format s "<title>"))

(defun finish-title (s)
  (format s "</title>"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; body
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun format-body (s color &optional action)
  (cond ((null action)
         (format s "<body leftmargin='0' topmargin='0' marginwidth='0' marginheight='0' bgcolor='~A'>" color))
        (t (format s "<body leftmargin='0' topmargin='0' marginwidth='0' marginheight='0' bgcolor='~A' onLoad='~A'>" color action))))

(defun finish-body (s)
  (format s "</body>"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; margin
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defparameter *width* nil)
(defparameter *height* nil)

(defun format-margin (s)
  (format s "<div style='margin-left:10px; margin-right:10px; margin-top:10px; margin-bottom:10px'>"))

(defun finish-margin (s)
  (format s "</div>"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; forms
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun format-form (s)
  (format s "<form>"))

(defun finish-form (s)
  (format s "</form>"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; modalities
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun format-stringfield (s name value size &optional (max ""))
  (format s "<input type='text' status='string' name='~A' value='~A' size='~A' maxlength='~A'/>"
          name value size max))

(defun format-typein (s name value size &optional (max ""))
  (format s "<input type='text' name='~A' value='~A' size='~A' maxlength='~A'/>"
          name value size max))

(defun format-text (s name value size &optional (max ""))
  (format s "<input type='text' name='~A' value='~A' size='~A' maxlength='~A'/>"
          name value size max))

(defun format-textarea (s name text rows cols)
  (format s "<textarea name='~A' rows='~A' cols='~A' wrap='soft'/>" name rows cols)
  (format s "~A" (prettyname text))
  (format s "</textarea>"))

(defun format-checkboxes (s slot options values)
  (format s "<dl>")
  (dolist (option options)
    (format s "<dt>")
    (format-checkbox s (stringize slot) (stringize option)
                     (find option values :test #'equalp)))
  (format s "</dl>"))

(defun format-checkbox (s name value checked)
  (if checked
    (format s "<input type='checkbox' name='~A' value='~A' checked='1'/>~A"
            name value value)
    (format s "<input type='checkbox' name='~A' value='~A'/>~A"
            name value value)))

(defun format-radiobuttons (s slot options value)
  (setq slot (stringize slot))
  (when options
    (format s "<dl>")
    (format s "<dt>")
    (format-radiobutton s slot (stringize (car options))
                        (equal (car options) value))
    (dolist (option (cdr options))
      (format s "<dt>")
      (format-radiobutton s slot (stringize option) (equal option value)))
    (format s "</dl>")))

(defun format-radiobutton (s name value checked)
  (if checked
      (format s "<input type='radio' name='~A' value='~A' checked='1'/>~A"
              name value value)
      (format s "<input type='radio' name='~A' value='~A'/>~A"
              name value value)))

(defun format-password (s name value size &optional (maxlength ""))
  (format s "<input type='password' name='~A' value='~A' size='~A' maxlength='~A'/>"
          name value size maxlength))

(defun format-hidden (s name value)
  (format s "<input type='hidden' name='~A' value='~A'/>" name value))

(defun format-button (s name value)
  (format s "<input type='submit' name='~A' value='~A'/>" name value))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Miscellaneous
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun output-prolog (s &optional (status-code 200) (oldcookies) (newcookies))
  (output-html-prolog s status-code oldcookies newcookies))

(defun output-break (s)
  (format s "<br/>"))

(defun format-message (s text)
  (format s "<p><table><tr><td bgcolor=\"#99ff99\">~A</td></tr></table></p>" text))

(defun format-warning (s text)
  (format s "<p><table><tr><td bgcolor=\"yellow\">~A</td></tr></table></p>" text))

(defun format-error (s text)
  (format s "<p><table><tr><td bgcolor=\"#ff9999\">~A</td></tr></table></p>" text))

#|
(defun output-classlink (s concept)
  (unless (findp `(nocommand ,*gui* metalevel) *manager*)
    (format s "<a href=\"/~A/change?Object=~A\" target=\"_blank\"><img src=\"~Aimages/pencil.gif\" border=\"0\"/></A>"
            (addressify (name *interface*)) (addressify concept) *home*)
    (format s "<a href=\"/~A/qbeview?Object=~A\" target=\"_blank\"><img src=\"~Aimages/hammer.gif\" border=\"0\"/></A>"
            (addressify *gui*) (addressify concept) *home*))
  (format s (iconify concept)))
|#

(defun output-classlink (s concept)
  (format s (iconify concept)))

#|
(defun output-slotlink (s concept)
  (unless (findp `(nocommand ,*gui* metalevel) *manager*)
    (format s "<a href=\"/~A/change?Object=~A\" target=\"_blank\"><img src=\"~Aimages/pencil.gif\" border=\"0\"/></A>"
            (addressify (name *interface*)) (addressify concept) *home*)
    (format s "<a href=\"/~A/qbeview?Object=~A\" target=\"_blank\"><img src=\"~Aimages/hammer.gif\" border=\"0\"/></A>"
            (addressify *gui*) (addressify concept) *home*))
  (format s (iconify concept)))
|#

(defun output-slotlink (s concept)
  (format s (iconify concept)))

(defun output-value (s obj)
  (cond ((not (symbolp obj)) (format s "~A" (prettify obj)))
        (t (format s "<A HREF='fastinspectpage?Object=~A' target=\"_top\"><font color='black'>~A</font></A>"
                 (addressify obj) (iconify obj)))))

(defun output-enumvalue (s obj)
  (cond ((not (symbolp obj)) (format s "~A" (prettify obj)))
        (t (format s "<A HREF=\"fastinspectpage?Object=~A\" target=\"_blank\">~A</A>"
                 (addressify obj) (iconify obj)))))

(defun output-multiples (s values)
  (unless (null values)
    (output-value s (car values))
    (dolist (value (cdr values))
      (format s ", ")
      (output-value s value))))

(defun pluralize (obj)
  (let (icon)
    (cond ((varp obj) (prettify obj))
          ((and *icons* (setq icon (findx '?x `(pluralname ,obj ?x) *interface*)))
           (cleanse icon))
          ((and *icons* (setq icon (findx '?x `(prettyname ,obj ?x) *interface*)))
           (cleanse (stringappend icon "s")))
          (t (stringappend (prettify obj) "s")))))

(defparameter *namer* nil)

(defun iconify (obj)
  (let (icon)
    (cond ((and *icons* (setq icon (find-icon obj))) (cleanse icon))
          ((and *namer* (setq icon (find-prettyname obj))) (cleanse icon))
          (t (prettify obj)))))

(defun find-icon (x)
  (unless (varp x) (findx '?x `(prettyname ,x ?x) *interface*)))

(defun find-prettyname (x)
  (cond ((null *namer*) nil)
        ((varp x) nil)
        ((symbolp x) (result *namer* x *gui*))))

(defun shortify (obj)
  (let (icon)
    (cond ((and *icons* (setq icon (find-shortname obj))) icon)
          (t (prettify obj)))))

(defun find-shortname (x)
  (unless (varp x) (findx '?x `(shortname ,x ?x) *interface*)))

(defun html-message (s info)
  (format-html s) (crlf s)
  (output-head s "Message") (crlf s)
  (format-body s *bgcolor*) (crlf s)
  (output-header s)
  (format s "<p>~A</p>" info)
  (output-footer s)
  (finish-body s) (crlf s)
  (finish-html s) (crlf s))

(defun http-problem (s info)
  (format-html s) (crlf s)
  (output-head s "Error") (crlf s)
  (format-body s *bgcolor*) (crlf s)
  (output-header s)
  (format s "<p style='margin-left: 10px; margin-right: 10px'>~A</p>" info)
  (output-footer s)
  (finish-body s) (crlf s)
  (finish-html s) (crlf s))

(defun http-problems (s items)
  (format-html s) (crlf s)
  (output-head s "Error") (crlf s)
  (format-body s *bgcolor*) (crlf s)
  (output-header s)
  (format s "<div style='margin-left: 10px; margin-right: 10px; color:#FF0000'>")
  (dolist (item items) (format s "<p>~A</p>" item))
  (format s "</div>")
  (output-footer s)
  (finish-body s) (crlf s)
  (finish-html s) (crlf s))

(defun output-problems (s items)
  (format-html s) (crlf s)
  (output-head s "Error") (crlf s)
  (format-body s *bgcolor*) (crlf s)
  (output-header s) (crlf s)
  (format-margin s) (crlf s)
  (dolist (item items) (format s "<p>~A</p>" item))
  (finish-margin s) (crlf s)
  (output-footer s)
  (finish-body s) (crlf s)
  (finish-html s) (crlf s))

(defun readas (str type)
  (cond ((eq type 'string) str)
        ((eq type 'date) (parsedate str))
        ;((eq type 'time) (parsetime str))
        ;(t (read-user-string (substitute #\- #\space str)))
        (t (read-user-string str))))

(defun hyphenize (str)
  (with-output-to-string (out)
    (do ((i (find-nonwhite str 0 (length str))) (n (length str)))
        ((>= i n) nil)
        (cond ((not (whitep (elt str i)))
               (write-char (elt str i) out) (setq i (1+ i)))
              ((< (setq i (find-nonwhite str i n)) n)
               (write-char #\- out))
              (t (return t))))))

(defun find-nonwhite (str pos len)
  (do ((i pos (1+ i)))
      ((>= i len) len)
      (unless (whitep (elt str i)) (return i))))

(defun menu-size (num-of-items)
  (min num-of-items 6))

(defun read-from-post-data (name post-lines)
  (declare (type string name))
  (declare (type list post-lines))
  (let ((data (getf-post name post-lines)))
    (when (stringp data)
      (read-user-string data) )))


(defun getf-post (item postlines)
  (cdr (assoc item postlines :test #'string-equal)))

(defun getslots (slotname pl)
  (do ((l pl (cdr l)) (slots))
      ((or (null l) (string= (caar l) slotname)) (values (nreverse slots) l))
      (setq slots (cons (caar l) slots))))

(defun getvals (slotname pl)
  (do ((l pl (cdr l)) (values))
      ((or (null l) (not (string= (caar l) slotname))) (values (nreverse values) l))
      (if (not (string= (cdar l) "")) (setq values (cons (cdar l) values)))))

(defun getvalues (slotname pl)
  (do ((l pl (cdr l)) (values))
      ((or (null l) (not (string= (caar l) slotname))) (values (nreverse values) l))
      (unless (string= (cdar l) "")
        (setq values (cons (read-value-string (cdar l)) values)))))

(defun popvals (slotname pl)
  (do ((l pl (cdr l)))
      ((or (null l) (not (string= (caar l) slotname))) l)))

(defun getallvals (slotname pl)
  (do ((l pl (cdr l)) (nl))
      ((null l) (nreverse nl))
      (when (string-equal (caar l) slotname) (setq nl (cons (cdar l) nl)))))

(defun getf-revname (postlines)
  (let ((dum (assoc "Command" postlines :test #'startstringp)))
    (when dum (subseq (car dum) 8))))

(defun getf-expname (postlines)
  (let ((dum (assoc "Expand" postlines :test #'startstringp)))
    (when dum (subseq (car dum) 7))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; cleanse
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun cleanse (str)
  (let (beg end result)
    (cond ((and (setq beg (search "<DISPLAY>" str :test #'string-equal))
                (setq end (search "</DISPLAY>" str :start2 beg :test #'string-equal)))
           (setq result (prorequest (read-from-string (subseq str (+ beg 9) end))))
           (setq result (with-output-to-string (s) (display s result)))
           (cleanse (strappend (subseq str 0 beg) result (subseq str (+ end 10)))))
          ((and (setq beg (search "<EXPRESS>" str :test #'string-equal))
                (setq end (search "</EXPRESS>" str :start2 beg :test #'string-equal)))
           (setq result (prorequest (read-from-string (subseq str (+ beg 9) end))))
           (setq result (with-output-to-string (s) (express s result)))
           (cleanse (strappend (subseq str 0 beg) result (subseq str (+ end 10)))))
          (t str))))

(defun display (s x)
  (cond ((null x))
        ((atom x) (output-value s x))
        ((atom (car x)) (output-multiples s x))
        (t (display-array s x))))

(defun display-array (s array)
  (format s "<TABLE BORDER>")
  (do ((l array (cdr l)))
      ((null l))
      (format s "<TR>")
      (do ((m (car l) (cdr m)))
          ((null m))
          (format s "<TD>")
          (display s (car m))
          (format s "</TD>"))
      (format s "</TR>"))
  (format s "</TABLE>"))

(defun express (s x)
  (cond ((null x))
        ((atom x) (express-value s x))
        (t (express-multiples s x))))

(defun express-value (s obj)
  (format s (stringize obj)))

(defun express-multiples (s values)
  (unless (null values)
    (express-value s (car values))
    (dolist (value (cdr values))
      (format s ", ")
      (express-value s value))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Character conversion
;;; 
;;; Three types of conversion:
;;;   stringification
;;;   slashification
;;;   htmlification or urlification
;;;
;;; Assumption:
;;;   Numbers are stringified with princ-to-string
;;;   Numbers need not be backslashed; backslashing has no effect
;;;   Numbers need not be htmlified nor urlified
;;;
;;; Assumption:
;;;   Strings are already strings and are not prettified in any way
;;;   Strings MAY contain quotes and backslashes, necessitating backslashing
;;;   Strings MAY contain special chars < > & ", necessitating htmlify or urlify
;;;
;;; Assumption:
;;;   Symbols are converted with symbol-name and always prettified
;;;   Symbols have read property, i.e. (read-from-string (symbol-name x)) = x
;;;   i.e. no cases like |a| or |10:00| and so forth
;;;   therefore, need not be backslashed; backslashing has no effect
;;;   Symbol names MAY contain special chars, necessitating htmlify or urlify
;;;
;;; Assumption:
;;;   Nothing else should occur; if it does, it prints as an empty string
;;;
;;; There are two types of output:
;;;   urls
;;;   html
;;;
;;; There are two types of output for each of these:
;;;   viewing -- backslashing necessary
;;;   editing -- no backslashing
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; output routines
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun output-handle (s item)
  (cond ((simplep item) (output-simple s item))
        (t (output-anchor s item))))

(defun output-handle-in-style (s value style)
  (cond ((eq style 'imagestyle) (format s "<img src='~A'/>" value))
        ((eq style 'urlstyle) (format s "<a href='~A'><font color='red'>~A</font></A>" value value))
        ((eq style 'emailstyle) (format s "<a href='mailto:~A'><font color='red'>~A</font></A>" value value))
        ((eq style 'htmlstyle) (format s value))
        ((eq style 'dollarstyle) (format s "$~$" value))
        ((eq style 'prettystyle) (output-simple s value))
        ((simplep value) (output-simple s value))
        (t (output-anchor s value))))

(defun output-value-in-style (s value style)
  (when (listp value) (setq value (car value)))
  (case style
    (imagestyle (format s "<img src='~A'/>" value))
    (urlstyle (format s "<a href='~A'><font color='red'>~A</font></A>" value value))
    (emailstyle (format s "<a href='mailto:~A'><font color='red'>~A</font></A>" value value))
    (htmlstyle (format s value))
    (dollarstyle (format s "$~$" value))
    (prettystyle (output-simple s value))
    (t (output-handle s value))))

(defun output-simple (s item)
  (format s "~A" (prettyname item)))

#|
(defun output-anchor (s item)
  (format s "<span style='cursor:pointer; text-decoration:underline;' onClick='doclick(\"~A\",event)'>~A</span>"
          (addressify item) (prettyname item)))
|#

(defun output-anchor (s item)
  (format s "<a href='fastinspectpage?Object=~A'>~A</a>"
          (addressify item) (prettyname item)))

(defun simplep (item)
  (or (not (symbolp item)) (startstringp "unknown" (symbol-name item)) (varp item)))

(defun output-pretty-value (s value)
  (format s "~A" (iconify value)))

;;; should it be fasthandle?
;;; should prettynames be cleansed?

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun output-kif (s kif)
  (format s "<PRE>~%")
  (format s "~A~%" (kif-to-html-string kif))
  (format s "</PRE>")
  (format s "~%") )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; expression conversion
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun kif-to-html-string (kif)
  (let ((*print-pretty* t))
    (htmlify
     (format nil "~A" (downcase-except-strings kif)))))

(defun downcase-except-strings (obj)
  (cond ((stringp obj) (concatenate 'string (string #\") obj (string #\")))
        ((symbolp obj) (string-downcase (symbol-name obj)))
        ((listp obj) (mapcar #'downcase-except-strings obj))
        (t obj)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; symbol-conversion
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; prettify
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun prettify (x)
  (cond ((numberp x) (princ-to-string x))
        ((characterp x) (htmlify (princ-to-string x)))
        ((stringp x) (if (html-string? x) x (htmlify x)))
        ((find x '(unknown unknown1 unknown2 unknown3)) "")
        ((null x) "")
        ((symbolp x) (htmlify (format nil "~:(~A~)" (symbol-name x))))
        (t "")))

(defparameter *underscores* t)

(defun make-pretty-string (sym)
  (if *underscores* (format nil "~:(~A~)" (symbol-name sym))
      (delete #\_ (format nil "~:(~A~)" (symbol-name sym)))))

(defun html-string? (str)
  (declare (type string str))
  (and (not (equalp str "")) (char= (elt str 0) #\<)))

(defun htmlp (str)
  (declare (type string str))
  (and (not (equalp str "")) (char= (elt str 0) #\<)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; prettyname
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun prettyname (item)
  (let (name)
    (cond ((numberp item) (princ-to-string item))
          ((characterp item) (htmlify (princ-to-string item)))
          ((stringp item) (if (htmlp item) item (htmlify item)))
          ((symbolp item)
           (cond ((null item) "")
                 ((startstringp "unknown" (symbol-name item)) "")
                 ((varp item)
                  (htmlify (format nil "~:(~A~)" (symbol-name item))))
                 ((setq name (result 'prettyname item *gui*))
                  (htmlify name))
                 (t (htmlify (format nil "~:(~A~)" (symbol-name item))))))
          (t ""))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; stringize
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun stringize (item)
  (cond ((numberp item) (prin1-to-string item))
	((characterp item) (htmlify (princ-to-string item)))
	((stringp item) (htmlify (prin1-to-string item)))
	((symbolp item)
	 (cond ((null item) "")
	       ((startstringp "unknown" (symbol-name item)) "")
	       (t (htmlify (format nil "~:(~A~)" (symbol-name item))))))
	(t "")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; addressify
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun addressify (item)
  (let (name)
    (cond ((numberp item) (prin1-to-string item))
          ((characterp item) (htmlify (princ-to-string item)))
          ((stringp item) (if (htmlp item) item (htmlify (prin1-to-string item))))
          ((symbolp item)
           (cond ((null item) "")
                 ((startstringp "unknown" (symbol-name item)) "")
                 ((varp item)
                  (htmlify (format nil "~:(~A~)" (symbol-name item))))
                 ((setq name (symbol-name item))
                  (htmlify name))
                 (t (htmlify (format nil "~:(~A~)" (symbol-name item))))))
          (t ""))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; string conversion
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; urlify
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun urlify (s)
  (unless (stringp s) (setq s (princ-to-string s)))
  (with-output-to-string (o)
    (do ((i 0 (1+ i)) (c) (n (length s)))
        ((= i n) o)
        (setq c (elt s i))
        (cond ((alphanumericp c) (write-char c o))
              ((find c '(#\$ #\- #\_ #\. #\+) :test #'char=) (write-char c o))
              (t (format o "%~:@(~2,'0x~)" (char-code c)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; htmlify
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun htmlify (str)
  (with-output-to-string (s)
    (do ((i 0 (1+ i)) (n (length str)) (c))
        ((= i n))
        (setq c (elt str i))
        (cond ((eql c #\") (format s "&quot;"))
              ((eql c #\') (format s "&#39;"))
              ((eql c #\&) (format s "&amp;"))
              ((eql c #\>) (format s "&gt;"))
              ((eql c #\<) (format s "&lt;"))
              (t (write-char c s))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; javascript stuff
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun javify (str)
  (with-output-to-string (s)
    (do ((i 0 (1+ i)) (c) (n (length str)))
        ((= i n) s)
        (setq c (elt str i))
        (cond ((char= c #\') (write-char #\\ s) (write-char #\' s))
              ((char= c #\") (write-char #\\ s) (write-char #\" s))
              (t  (write-char c s))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; cleanstring
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun cleanstring (str)
  (setq str (string-trim " '\"" str))
  (with-output-to-string (s)
    (do ((i 0 (1+ i)) (c) (flag) (n (length str)))
        ((= i n) s)
        (setq c (elt str i))
        (cond ((char= c #\'))
              ((char= c #\"))
              ((char= c #\space) (setq flag t))
              (flag (setq flag nil) (write-char #\space s) (write-char c s))
              (t (write-char c s))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; file stuff
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun macify (pathname)
  (substitute #\: #\/ (subseq pathname 1)))

(defun filify (s)
  (when (stringp s)
    (make-filename (filename-directory s) (filename-file s) (filename-type s))))

(defun filename-directory (s)
  (do ((old 0) (pos 0)
       (dir (list (if (char= (elt s 0) #\/) :absolute :relative))))
      ((not (setq pos (position #\/ s :start old))) (nreverse dir))
      (unless (= pos old) (setq dir (cons (subseq s old pos) dir)))
      (setq old (1+ pos))))

(defun filename-file (s)
  (let (left right)
    (setq left (or (position #\/ s :from-end t) 0))
    (setq right (position #\. s :from-end t))
    (subseq s (1+ left) right)))

(defun filename-type (s)
  (let (pos)
    (setq pos (position #\. s :from-end t))
    (subseq s (1+ pos))))

;;; always relative to homedir - ignore leading "/"

(defun make-filename (directory file type)
  (when (eq (car directory) :relative)
    (setq directory (append (pathname-directory *homedir*) (cdr directory))))
  (namestring (make-pathname :directory directory :name file :type type)))

#|
(defun make-filename (directory file type)
  (setq directory (append (pathname-directory *homedir*) (cdr directory)))
  (namestring (make-pathname :directory directory :name file :type type)))
|#

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; miscellaneous
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun tellgui (p)
  (request `(tell ,p) *client* *gui*))

(defun untellgui (p)
  (request `(untell ,p) *client* *gui*))

(defun testeval (msg)
  (request msg *client* *gui*))

(defun sorteval (msg)
  (sort (request msg *client* *gui*) #'lessp))

(defun proclassify (x)
  (classify x *gui*))

(defun prorequest (msg)
  (let ((log))
    (if (and (listp msg) (find (car msg) '(tell untell update))
             (setq log (findx '?log `(logfile ,*gui* ?log) *interface*)))
        (logmessage `(request ',msg ',(name *client*) ',(name *agent*)) log))
    (request msg *client* *agent*)))

(defun promessage (msg)
  (let ((log))
    (if (and (listp msg) (find (car msg) '(tell untell update))
             (setq log (findx '?log `(logfile ,*gui* ?log) *interface*)))
        (logmessage `(message ',msg ',(name *client*) ',(name *agent*)) log))
    (message msg *client* *agent*)))

(defun logmessage (msg fn)
  (with-open-file
    (log fn :direction :output :if-exists :append :if-does-not-exist :create)
    (prin1 msg log)
    (terpri log)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; manager information
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun find-metadata (x)
  (or (referent (findx '?x `(metadata ,x ?x) *manager*)) *manager*))

(defun find-security (x)
  (findx '?x `(security ,x ?x) *manager*))

(defun find-stylesheet (interface)
  (findx '?x `(stylesheet ,interface ?x) *interface*))

(defun find-frontpage (interface)
  (let (dir name)
    (cond ((setq name (filify (findx '?x `(frontpage ,interface ?x) *interface*))))
          (t (setq dir (append (pathname-directory *homedir*)
			       (list "pages" (symbol-name interface))))
             (setq name (make-pathname :directory dir
                                       :name "frontpage" :type "html"))))
    (when (probe-file name) (read-from-file name))))

(defun find-header (interface)
  (let (dir name)
    (cond ((setq name (filify (findx '?x `(header ,interface ?x) *interface*))))
          (t (setq dir (append (pathname-directory *homedir*)
                               (list "pages" (symbol-name interface))))
             (setq name (make-pathname :directory dir
                                       :name "header" :type "html"))))
    (if (probe-file name) (read-from-file name) "")))

(defun find-footer (interface)
  (let (dir name)
    (cond ((setq name (filify (findx '?x `(footer ,interface ?x) *interface*))))
          (t (setq dir (append (pathname-directory *homedir*)
                                (list "pages" (symbol-name interface))))
             (setq name (make-pathname :directory dir
                                       :name "footer" :type "html"))))
    (if (probe-file name) (read-from-file name) "")))

(defun find-searchpage (class)
  (findx '?x `(searchpage ,class ?x) *interface*))

(defun find-createpage (class)
  (findx '?x `(createpage ,class ?x) *interface*))

(defun find-inspectpage (class)
  (findx '?x `(inspectpage ,class ?x) *interface*))

(defun find-modifypage (class)
  (findx '?x `(modifypage ,class ?x) *interface*))

(defun find-arguments (action)
  (finds '?x `(argument ,action ?x) *interface*))

(defun find-argumentstyle (slot)
  (or (findx '?t `(argumentstyle ,slot ?t) *interface*)
      (findx '?t `(changestyle ,slot ?t) *interface*)))

(defun find-argumenttype (slot)
  (or (findx '?t `(argumenttype ,slot ?t) *interface*)
      (findx '?t `(range ,slot ?t) *interface*)))

(defun find-argumentdefault (relation)
  (deval (findx '?v `(argumentdefault ,relation ?v) *interface*)))

(defun find-choices (relation)
  (let (class choices)
    (cond ((finds '?v `(choice ,relation ?v) *interface*))
          ((and (setq class (find-argumenttype relation))
                (setq choices (request `(ask-all ?x ,(makpred '?x class *gui*))
                                       *client* *gui*)))
           (sort choices #'lessp)))))

(defun find-references (slot value)
  (let (aporels (*var-count* 0))
    (setq aporels (finds '?x `(expander ,slot ?x) *interface*))
    (do ((l (nreverse aporels) (cdr l)) (vl) (nl))
        ((null l) (prorequest `(ask-one ,vl ,(maksand (nreverse nl)))))
        (setq nl (cons (list (car l) value (setq value (genvar))) nl))
        (setq vl (cons value vl)))))

(defun find-components (slot value references)
  (let (aporels)
    (setq aporels (finds '?x `(expander ,slot ?x) *interface*))
    (do ((l references (cdr l)))
        ((null l))
        (cond ((equalp (car l) value) (return (find-args (car aporels) value)))
              ((cdr aporels) (setq aporels (cdr aporels)))))))

(defun find-args (relation value)
  (sort (request `(ask-all ?x ,(list relation '?x value)) *client* *gui*) #'lessp))

(defun find-updatelabel (slot)
  (or (findx '?x `(updatelabel ,slot ?x) *interface*) ""))

(defun find-values (slot)
  (let (options)
    (cond ((results 'option slot *interface*))
          ((setq options (request `(ask-all ?y ,(list slot '?x '?y))
                                       *client* *gui*))
           (sort options #'lessp)))))

(defun find-possibilities (slot class)
  (let (options)
    (cond ((setq options (results 'option slot *interface*))
           (cons 'unknown options))
          ((setq options (request `(ask-all ?y (and ,(makpred '?x class *gui*) ,(list slot '?x '?y)))
                                  *client* *gui*))
           (cons 'unknown (sort options #'lessp)))
          (t (list 'unknown)))))

(defun find-options (slot)
  (let (class options)
    (cond ((results 'option slot *interface*))
          ((and (setq class (find-range slot))
                (setq options (request `(ask-all ?x ,(makpred '?x class *gui*))
                                       *client* *gui*)))
           (sort options #'lessp)))))

(defun find-alternatives (relation)
  (cons 'unknown (findalternatives relation)))

(defun findalternatives (slot)
  (let (class options)
    (cond ((results 'option slot *interface*))
          ((and (setq class (find-range slot))
                (setq options (request `(ask-all ?x ,(makpred '?x class *gui*))
                                       *client* *gui*)))
           (sort options #'lessp)))))

(defun deval (val)
  (cond ((atom val) val)
        (t (ignore-errors (eval val)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
