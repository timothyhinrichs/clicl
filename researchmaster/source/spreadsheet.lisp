(in-package :common-lisp-user)

; NOTE: server-side Lisp generation is likely broken since I turned off shortnames via making them equal to their prettynames.

(defun starttcp1 (port)
  (process-run-function "tcp-handlers" #'tcp-handlers #'http-handler port))

(defun starttcpbroken (port)
  (process-run-function "tcp-handlers" #'(lambda () (ignore-errors (tcp-handlers #'http-handler port)))))

(defun starttcp (port)
  (process-run-function "tcp-handlers" #'(lambda () (ignore-errors (tcp-servers #'http-handler port)))))

(defun starttcp2 (port)
  (ignore-errors (tcp-handlers #'http-handler port)))

(defun starttcp3 (port)
  (tcp-handlers #'http-handler port))

(defun stoptcp ()
  (let (titles)
    (setq titles '("tcp handler" "tcp-handler"))
    (dolist (p (all-processes))
      (when (some #'(lambda (x) (search x (process-name p) :test #'char-equal)) titles)
	(process-kill p)))))

(defun find-all-lisp-funcs (filename)
  (let ((s (read-file filename)))
    (mapcarnot #'(lambda (x) (if (and (listp x) (or (eq (car x) 'defun) (eq (car x) 'defmethod)))
				 (second x)
				 nil))
	       s)))

(defvar *tmp*)
(defparameter *webformlog* nil)
; note: ought to make this a per-form option.  Just need to add it as an option and replace the global var with the form far.
;   On second thought, maybe not.  It's an implementation detail, not a behavioral detail.
(defparameter *ws-use-value-shortnames* nil)  
(defparameter *ws-assign* '<- "symbol for assignment in datalog")

; TO INSTALL a new built-in
; Options
; 1) Extend basic language
;    *ws-builtins*: add new parameter.
;    tojavascript: translate Lisp version of built-in into Javascript 
;       (Also, add implementations of the built-ins to the Lisp server stuff.  Can't remember where to do that right now.)
; 2) Extend PHP function implementation
;     add function definition to PHP implementation.  Search for *corejs* to find appropriate file.
;         (Somewhat dangerous b/c may need to upgrade PHP file to fix bugs and all changes will be lost. 
;     add function name with appropriate case to *ws-php-builtin-names* and recompute *ws-php-builtin-prefixed-names*
; 3) Unimplemented idea: add a macro replacement step for one-to-one rewritings.  So if we want to include ===, the easiest approach
;      is to add a macro that replaces === with =.

#| TODO
Output GUI
-----------
2) Use drop-down list that displays text-decoration but always displays the selected value as unadorned.
3) Allow other kinds of widgets, e.g. radio.
4) Multiple valued cells should be possible, broadening the class of widgets to include check boxes.
5) Enforce the requirement that certain cells must be assigned values.  (On submit?)
6) Undo, conflict-reduce undo, conflict-free undo   and redos
7) Auto solve button (using CSP-solver?)
8) Explanations as to why a cell is red (original constraints used to produce proofs)
9) Save/load functionality to halt progress in large form.

Implementation
---------------
Compiler-speed/termination
1) Resolution: restrict resolution to only apply to widget predicates.  (Does this always terminate?)

Expressiveness
1) Allow user to express constraints as tables of cells: compile to monadic cells.
2) Allow procedural attachments in constraints (to handle text inputs).  Need to fix js match code.
3) Multi-valued cells

Input GUI
----------
2) Do away with Type definitions -- fold into Constraints and detect complete definitions?
3) Syntax/input method for constraints (use Austen's javascript?)

|#

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; SOAP interface ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defclass plato (interface)
  ((name :initarg :name :accessor name :initform nil)))

(defmethod create (obj (type (eql 'plato)))
  (unless (and (boundp obj) (typep (symbol-value obj) 'plato))
    (set obj (make-instance 'plato :name obj))))

(defmethod destroy (obj (type (eql 'plato)))
  (when (and (symbolp obj) (boundp obj)
             (eq (type-of (symbol-value obj)) 'plato))
    ;(empty (symbol-value obj))
    (makunbound obj)))


(defmethod request (msg sender (receiver plato))
  (declare (ignore sender))
  (let (form fields)
    (multiple-value-setq (form fields) (lispxml2form msg))
    (if (not form) 
	(values "" t)  ; signal error
	(values (htmlform2xml (compile-websheet form) fields) nil))))

(defun makekifxmlsafe (p)
  (cond ((atom p) p)
	((eq (car p) '<=) (cons 'reduct (mapcar #'makekifxmlsafe (cdr p))))
	((eq (car p) '=>) (cons 'implies (mapcar #'makekifxmlsafe (cdr p))))
	((eq (car p) '<=>) (cons 'iff (mapcar #'makekifxmlsafe (cdr p))))
	((member (car p) '(forall exists and or not)) (cons (car p) (mapcar #'makekifxmlsafe (cdr p))))
	(t p)))

(defun xmlsafekif2kif (p)
  (cond ((atom p) p)
	((eq (car p) 'reduct) (cons '<= (mapcar #'makekifxmlsafe (cdr p))))
	((eq (car p) 'implies) (cons '=> (mapcar #'makekifxmlsafe (cdr p))))
	((eq (car p) 'iff) (cons '<=> (mapcar #'makekifxmlsafe (cdr p))))
	((member (car p) '(forall exists and or not)) (cons (car p) (mapcar #'makekifxmlsafe (cdr p))))
	(t p)))

(defun xmlsafeform2form (p)
  "(XMLSAFEFORM2FORM P) given a line describing a component of a form (that is XML safe)
   transform the line so that it is in the original form description language."
  (cond ((atom p) p)
	((member (car p) '(constraints definitions)) 
	 (list (car p) (quotify (mapcar #'xmlsafekif2kif (second (second p))))))
	(t p)))

(defun lispxml2form (xml)
  "(LISPXML2FORM XML) takes a lisp version of XML and extracts (1) the form field
   and (2) the list of all field elements"
  (cond ((atom xml) (values nil nil))
	((atom (car xml)) (values nil nil))
	((eq (caar xml) 'form) (values (mapcar #'xmlsafeform2form (read-sentences (second xml))) nil))
	((eq (caar xml) 'field) (values nil (list (read-user-string (second xml)))))
	(t
	 (let ((f nil) (ds nil) form field)
	   (dolist (x (cdr xml) (values f (nreverse ds)))
	     (multiple-value-setq (form field) (lispxml2form x))
	     (when (and (not f) form) (setq f form))
	     (when field (setq ds (union ds field))))))))

; (defstruct htmlform cssincludes jsincludes javascript widgets errors options onload)
(defun htmlform2xml (form fields)
  (with-output-to-string (buf)
    (let ((all (member '? fields)))
      (format buf "<platoform>~%")
      (when (or all (member 'cssincludes fields)) 
	(format buf "<cssincludes>~A</cssincludes>~%" (list2xml (htmlform-cssincludes form) "cssinclude")))
      (when (or all (member 'jsincludes fields))
	(format buf "<jsincludes>~A</jsincludes>~%" (list2xml (htmlform-jsincludes form) "jsinclude")))
      (when (or all (member 'javascript fields))
	(format buf "<javascript>~A</javascript>~%" (htmlform-javascript form)))
      (when (or all (member 'widgets fields))
	(format buf "<widgets>~A</widgets>~%" (widgets2xml (htmlform-widgets form))))
      (when (or all  (member 'errors fields))
	(format buf "<errors>~A</errors>~%" (list2xml (htmlform-errors form) "error")))
      (when (or all (member 'options fields))
	(format buf "<options>~A</options>~%" (options2xml (htmlform-options form))))
      (when (or all (member 'onload fields))
	(format buf "<onload>~A</onload>~%" (htmlform-onload form)))
      (when (or all (member 'submitprep fields))
	(format buf "<submitprep>~A</submitprep>~%" (htmlform-submitprep form)))
      (when (or all (member 'action fields))
	(format buf "<action>~A</action>~%" (htmlform-action form)))
      (when (or all (member 'name fields))
	(format buf "<name>~A</name>~%" (htmlform-name form)))
      (when (or all (member 'hidden fields))
	(format buf "<hidden>~A</hidden>~%" (htmlform-submitprep form)))
      (when (or all (member 'server fields))
	(format buf "<server>~A</server>~%" (htmlform-server form)))
      (format buf "</platoform>")
      buf)))

(defun list2xml (list tag)
  (with-output-to-string (buf)
    (dolist (l list buf)
      (format buf "<~A>~A</~A>" tag l tag))))

(defun widgets2xml (widgets)
  (with-output-to-string (buf)
    (dolist (w widgets buf)
      (format buf "<widget>")
      (format buf "<description>~A</description>" (htmlwidget-description w))
      (format buf "<required>~A</required>" (if (htmlwidget-required w) "true" "false"))
      (format buf "<html>~A</html>" (htmlwidget-html w))
      (format buf "</widget>"))))

(defun options2xml (options)
  (with-output-to-string (buf)
    (dolist (o options buf)
      (format buf "<option>")
      (format buf "<key>~(~A~)</key>" (first o))
      (format buf "<value>~(~A~)</value>" (if (booleanp (second o)) 
					      (if (second o) "true" "false") (second o)))
      (format buf "</option>"))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Webserver interface ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod process (s (file (eql 'openwebsheet)) postlines)
  (declare (ignore postlines))
  (open-websheet s nil))
  
(defmethod process (s (file (eql 'editwebsheet)) postlines)
  (cond ((getf-post "formstructure" postlines) ; loading formstructure directly
         (edit-websheet s (read-sentences (getf-post "formstructure" postlines))))
	((getf-post "file" postlines)  ; example stored on server
	 (let (pathname)
	   (setq pathname (filify (getf-post "file" postlines)))
	   (cond ((null pathname) (http-problem s "No file specified."))
		 ((probe-file pathname) 
		  (edit-websheet s (read-file pathname)))
		 (t (html-websheet-problem s "Document not found.")))))
        (t (edit-websheet s '((option allowconflicts true)
			      (option debug false)
			      (option casesensitive true))))))

(defmethod process (s (file (eql 'compilewebsheet)) postlines)
  (cond ((getf-post "file" postlines)  ; example stored on server
	 (let (pathname)
	   (setq pathname (filify (getf-post "file" postlines)))
	   (cond ((null pathname) (html-websheet-problem s "No file specified."))
		 ((probe-file pathname) 
		  (output-htmlform s (compile-websheet (read-file pathname))))
		 (t (html-websheet-problem s "Document not found.")))))
        ((equalp (getf-post "command" postlines) "compile") ; GUI compile
         (output-htmlform s (compile-websheet (parse-formstructure postlines))))
	((equalp (getf-post "command" postlines) "dump") ; GUI dump
	 (open-websheet s (parse-formstructure postlines)))
	(t (html-websheet-problem s "No compilation source specified."))))

(defun save-websheet (ws name &optional (prefix "/Users/thinrich/Research/code/researchmaster/wsgallery/"))
  "(SAVE-WEBSHEET WS NAME PREFIX) compiles websheet WS (either a filename given as a string or
   a theory) and stores the result on disk.  The HTML is stored in PREFIXNAME.html, and the Lisp server-side
   error-checking code is stored in PREFIXNAME.lisp"
  (when (and (not (stringp ws)) (not (listp ws))) (return-from save-websheet 'unsupported))
  (let (h 
	(*interface* (find-metadata 'plato))
	(*gui* 'plato))
    ; turn filename into theory
    (when (stringp ws) 
      (when (not (probe-file ws)) 
	(setq ws (filify ws))
	(when (not (probe-file ws)) (return-from save-websheet 'unsupported)))
      (setq ws (read-file ws)))
    ; compile theory and output
    (setq h (compile-websheet ws))
    (with-open-file (f (tostring (list prefix (tolower name) ".html")) 
		       :direction :output :if-does-not-exist :create :if-exists :supersede)
      (output-htmlform f h))
    (with-open-file (f (tostring (list prefix (tolower name) ".lisp"))
		       :direction :output :if-does-not-exist :create :if-exists :supersede)
      (dolist (c (htmlform-server h))
	(format f "~S~%" c)))))

(defun open-websheet (s th)
  "(OPEN-WEBSHEET S TH) displays page where user can paste declarative description of a web form.
   When that page is submitted, open the form in the form description editor."
  (html-websheet s "Open Webform" #'(lambda (s) (output-open-websheet s th))))

(defun edit-websheet (s th)
  "(EDIT-WEBSHEET S TH) Given a declarative description of a web form, output GUI for editing
   that description."
  (html-websheet s "Webform Editor" #'(lambda (s) (output-websheet-editor s th))))

(defun html-websheet-problem (s msg)
  "(HTML-WEBSHEET-PROBLEM S MSG) Informs the user of a problem."
  (html-websheet s "Problem" #'(lambda (s) (princ msg s))))

(defun html-websheet-message (s msg)
  "(HTML-WEBSHEET-MESSAGE S MSG) Gives the user a message."
  (html-websheet s "Message" #'(lambda (s) (princ msg s))))

(defun html-websheet (s title contentfunc)
  "(HTML-WEBSHEET S TITLE CONTENTFUNC) outputs the HTML for a websheet page
   to stream S using title TITLE and builds content by invoking CONTENTFUNC
   on stream S."
  (format-html s) (crlf s)
  (format-head s)
  (format s "<title>~A</title>~%" title)
  (output-websheet-styles s)
  (finish-head s)
  (format-body s *bgcolor*) (crlf s)
  (output-header s)
  (funcall contentfunc s)
  (output-footer s)
  (finish-body s) (crlf s)
  (finish-html s) (crlf s))

(defun output-open-websheet (s th)
  (format s "<div class=\"bodytext\">~%")
  (format s "<h2>Open Websheet</h2>~%")
  (format s "<form name=\"myform\" method=\"post\" action=\"editwebsheet?\">")
  (format s "<p><input type=\"submit\" value=\"Open\">")
  (format s "<p>In the box below, paste your Plato dump.")
  (format s "<p><textarea name=\"formstructure\" id=\"formstructure\" rows=\"20\" cols=\"100\">")
  (mapc #'(lambda (x) (print x s)) th)
  (format s "</textarea>~%")
  (format s "<p><input type=\"submit\" value=\"Open\">")
  (format s "</form>~%")
  (format s "</div>~%"))

(defparameter *ws-avail-options* '(allowconflicts debug casesensitive))
(defun output-websheet-editor (s th)
  "(OUTPUT-WEBSHEET-EDITOR S TH) outputs a graphical interface for editing the declarative
   description TH of a web form."
  (let (cnt tmp)
    (output-websheet-editor-script s)
    (format s "<form method='post' action='compilewebsheet?'>") (crlf s)
    (format s "<p><input type='submit' name='command' value='Compile'>")
    (format s "<input type='submit' name='command' value='Dump'>") (crlf s)
					; Display information
    (format s "<h2>1. Widgets</h2>") (crlf s)
    (format s "<table class='input'>") (crlf s)
    (format s "<tr><th></th><th>Name</th><th>Style</th><th>Unique</th><th>Required</th>") 
    (format s "<th>Include<br>undefined</th><th>Typename</th><th>Init Val</th><th>Description</th></tr>") 
    (crlf s)
    (setq cnt 1)
    (setq tmp (remove-if-not #'(lambda (x) (eq (relation x) 'widget)) th))
    (unless tmp (push '(widget :name "" :desc "") tmp))
    (dolist (v tmp)
      (setq v (apply #'make-widget (cdr v)))  ; so we have an object instead of a list
      (format s "<tr valign='top'>") (crlf s)
      (format s "   <td>") (crlf s)
      (output-plusminus-widget s "addWidget" "del2par")
      (format s "   </td>") (crlf s)
      (format s "   <td><input type='text' id='widget~Aname' name='widget~Aname' size='15' " cnt cnt)
      (format s "value='~A'></td>" (widget-name v)) (crlf s)
      (format s "   <td><select id='widget~Astyle' name='widget~Astyle'>" cnt cnt) (crlf s)
      (format s "       <option~A>selector</option>" 
	      (if (eq (widget-style v) 'selector) " selected" "")) (crlf s)
      (format s "       <option~A>checkbox</option>"
	      (if (eq (widget-style v) 'checkbox) " selected" "")) (crlf s)
      (format s "       <option~A>radio</option>" 
	      (if (eq (widget-style v) 'radio) " selected" "")) (crlf s)
      (format s "       <option~A>textbox</option>"
	      (if (eq (widget-style v) 'textbox) " selected" "")) (crlf s)
      (format s "       </select></td>") (crlf s)
      (format s "   <td align='center'><input type='checkbox' id='widget~Aunique' name='widget~Aunique'" 
	      cnt cnt)
      (format s " ~A></td>" (if (widget-unique v) "checked" ""))    
      (format s "   <td align='center'><input type='checkbox' id='widget~Areq' name='widget~Areq'" cnt cnt)
      (format s " ~A></td>" (if (widget-req v) "checked" ""))    
      (format s "   <td align='center'><input type='checkbox' id='widget~Aincundef'" cnt)
      (format s " name='widget~Aincundef' ~A></td>" cnt (if (widget-incundef v) "checked" ""))   
      (format s "   <td><input type='text' id='widget~Atype' name='widget~Atype' size='15' " cnt cnt)
      (format s "value='~A'></td>" (widget-typename v)) (crlf s)
      (format s "   <td><input type='text' id='widget~Ainit' name='widget~Ainit' size='15' " cnt cnt)
      (format s "value='")
      (when (listp (widget-init v)) (toinfixlist s (cdr (widget-init v))))
      (format s "'></td>") (crlf s)
      (format s "   <td><input type='text' id='widget~Adesc' name='widget~Adesc' size='40' " cnt cnt)
      (format s "value='~A'></td>" (widget-desc v)) (crlf s)
      (format s "</tr>") (crlf s)
      (setq cnt (1+ cnt)))
    (format s "</table>") (crlf s)
#|
					; Queries
    (format s "<p><br><h2>2. Queries</h2>") (crlf s)
    (format s "<table class='input'>") (crlf s)
    (format s "<tr><th></th><th>Name</th><th>Typename</th></tr>") (crlf s)
    (setq tmp (remove-if-not #'(lambda (x) (eq (relation x) 'query)) th))
    (unless tmp (push `(query "" "") tmp))
    (dolist (v tmp)
      (format s "<tr valign='top'>") (crlf s)
      (format s "   <td>") (crlf s)
      (output-plusminus-widget s "addQuery" "del2par")
      (format s "   </td>") (crlf s)
      (format s "   <td><input type='text' id='query~Aname' name='query~Aname' size='15' " cnt cnt) (crlf s)
      (format s "         value='~A'></td>" (second v)) (crlf s)
      (format s "   <td><input type='text' id='query~Atype' name='query~Atype' size='30' " cnt cnt) (crlf s)
      (format s "         value='~A'></td>" (third v)) (crlf s)
      (format s "</tr>") (crlf s)
      (setq cnt (1+ cnt)))
    (format s "</table>") (crlf s)
|#
					; Schema with types
    (format s "<p><br><h2>2. Types</h2>") (crlf s)
    (format s "<table class='input'>") (crlf s)
    (format s "<tr><th></th><th>Name</th><th>Comma-separated List</th></tr>") (crlf s)
    (setq tmp (remove-if-not #'(lambda (x) (eq (relation x) 'type)) th))
    (unless tmp (push `(type "" (listof)) tmp))
    (dolist (v tmp)
      (format s "<tr valign='top'>") (crlf s)
      (format s "   <td>") (crlf s)
      (output-plusminus-widget s "addPred" "del2par")
      (format s "   </td>") (crlf s)
      (format s "   <td><input type='text' id='pred~Aname' name='pred~Aname' size='15' " cnt cnt) (crlf s)
      (format s "         value='~A'></td>" (second v)) (crlf s)
;      (format s "   <td><input type='radio' id='pred~Atype' name='pred~Atype'~A value='string'>String " 
;	      cnt cnt (if (eq (third v) 'string) " checked" "")) (crlf s)
;
 ;     (format s "       <input type='radio' id='pred~Atype' name='pred~Atype'~A value='boolean'>Boolean " 
;	      cnt cnt (if (eq (third v) 'boolean) " checked" "")) (crlf s)
 ;     (format s "       <input type='radio' id='pred~Atype' name='pred~Atype'~A value='enum'>Enum: " 
;	      cnt cnt (if (listp (third v)) " checked" "")) (crlf s)
      (format s "   <td><input type='text' id='pred~Aenum' name='pred~Aenum' size='50' " cnt cnt) (crlf s)
      (format s "         value='")
      (when (listp (third v)) (toinfixlist s (cdr (third v))))
      (format s "'></td>") (crlf s)
      (format s "</tr>") (crlf s)
      (setq cnt (1+ cnt)))
    (format s "</table>") (crlf s)

					; Constraints and Definitions
    (format s "<p><br><h2>3. Constraints (<span class='language'>FHL</span>)</h2>") (crlf s)
    (format s "Note: all constants must be comprised only of alphabetic characters.<br>") (crlf s)
    (format s "<textarea cols='60' rows='10' id='constraints' name='constraints'>") (crlf s)
    (mapcar #'(lambda (x) (prin1 x s) (crlf s))
	    (mapcan #'(lambda (x) (second (second x))) 
		    (remove-if-not #'(lambda (x) (eq (relation x) 'constraints)) th)))(crlf s)
    (format s "</textarea>")    (crlf s)
    (format s "<p><br><h2>4. Extensional Definitions (<span class='language'>datalog</span>)</h2>") 
    (crlf s)
    (format s "Note: all constants must be comprised only of alphabetic characters.<br>") (crlf s)
    (format s "<textarea cols='60' rows='10' id='definitions' name='definitions'>") (crlf s)
    (mapcar #'(lambda (x) (prin1 x s) (crlf s))
	    (mapcan #'(lambda (x) (second (second x))) 
		    (remove-if-not #'(lambda (x) (eq (relation x) 'definitions)) th))) (crlf s)
    (format s "</textarea>") (crlf s)		    

					; Options
    (format s "<p><br><h2>5. Options</h2>") (crlf s)
    (dolist (v *ws-avail-options*)
      (format s "   <input type='checkbox' id='~A' name='~A' " v v)
      (format s "~A>~(~A~)<br>" (if (viewfindx '?x `(option ,v ?x) th) "checked" "") v))

					; Form name
    (setq tmp (viewfindx '?x '(formname ?x) th))
    (format s "<p><br><h2>6. Form Name</h2>") (crlf s)
    (format s "Form Name <input type='textbox' id='formname' name='formname' value='~A'><br>"
	    (if tmp tmp ""))

    (format s "<p><input type='submit' name='command' value='Compile'>")
    (format s "<input type='submit' name='command' value='Dump'>")
    (format s "<script type='text/javascript'>cellcounter = ~A;</script>" cnt) (crlf s)    
    (format s "</form>") (crlf s)))

(defun output-plusminus-widget (s addfunc remfunc)
  (output-plus-widget s addfunc)
  (output-minus-widget s remfunc))

(defun output-plus-widget (s func &optional (id nil))
  (unless (search "(" func) (setq func (format nil "~A(this)" func)))
  (format s "   <img src='/docserver/infomaster/images/add.gif'") (crlf s)
  (format s "      border='0' onClick=\"~A\"~A>" func (if id (format nil "id='~A'" id) "")) 
  (crlf s))

(defun output-minus-widget (s func &optional (id nil))
  (unless (search "(" func) (setq func (format nil "~A(this)" func)))
  (format s "   <img src='/docserver/infomaster/images/delete.gif'") (crlf s)
  (format s "      border='0' onClick=\"~A\"~A>" func (if id (format nil "id='~A'" id) "")) 
  (crlf s))


(defun output-websheet-editor-script (s)  
(format s "
<script type='text/javascript'>
var cellcounter = 2;
/* Leave in case we need later.
function createQueryRow (row)
 {var copy = row.cloneNode(true);
  var name = copy.childNodes[3].childNodes[0];
  var type = copy.childNodes[5].childNodes[0];

  name.id = 'query' + cellcounter + 'name';
  name.name = 'query' + cellcounter + 'name';
  type.id = 'query' + cellcounter + 'type';
  type.name = 'query' + cellcounter + 'type';

  name.value = '';
  type.value = '';

  copy.style.display = '';
  cellcounter++;  
  return copy; }
*/ 
function createPredRow (row)
 {var copy = row.cloneNode(true);
  var name = copy.childNodes[3].childNodes[0];
  var typestring = copy.childNodes[5].childNodes[0];
  var typenumber = copy.childNodes[5].childNodes[2];
  var typeenum = copy.childNodes[5].childNodes[4];
  var enumvs = copy.childNodes[5].childNodes[6];

  name.id = 'pred' + cellcounter + 'name';
  name.name = 'pred' + cellcounter + 'name';
  typestring.id = 'pred' + cellcounter + 'type';
  typestring.name = 'pred' + cellcounter + 'type';
  typenumber.id = 'pred' + cellcounter + 'type';
  typenumber.name = 'pred' + cellcounter + 'type';
  typeenum.id = 'pred' + cellcounter + 'type';
  typeenum.name = 'pred' + cellcounter + 'type';
  enumvs.id = 'pred' + cellcounter + 'enum';
  enumvs.name = 'pred' + cellcounter + 'enum';

  name.value = '';
  typestring.checked = false;
  typenumber.checked = false;
  typeenum.checked = true;
  enumvs.value = '';

  copy.style.display = '';
  cellcounter++;  
  return copy; }

function createWidgetRow (row)
 {var copy = row.cloneNode(true);
  var name = copy.childNodes[3].childNodes[0];
  var style = copy.childNodes[5].childNodes[0];
  var unique = copy.childNodes[7].childNodes[0];
  var req = copy.childNodes[9].childNodes[0];
  var undef = copy.childNodes[11].childNodes[0];
  var type = copy.childNodes[13].childNodes[0];
  var init = copy.childNodes[15].childNodes[0];
  var desc = copy.childNodes[17].childNodes[0];	

  name.id = 'widget' + cellcounter + 'name';
  name.name = 'widget' + cellcounter + 'name';
  style.id = 'widget' + cellcounter + 'style';
  style.name = 'widget' + cellcounter + 'style';
  unique.id = 'widget' + cellcounter + 'unique';
  unique.name = 'widget' + cellcounter + 'unique';
  req.id = 'widget' + cellcounter + 'req';
  req.name = 'widget' + cellcounter + 'req';
  undef.id = 'widget' + cellcounter + 'incundef';
  undef.name = 'widget' + cellcounter + 'incundef';
  init.id = 'widget' + cellcounter + 'init';
  init.name = 'widget' + cellcounter + 'init';
  type.id = 'widget' + cellcounter + 'type';
  type.name = 'widget' + cellcounter + 'type';
  init.id = 'widget' + cellcounter + 'init';
  init.name = 'widget' + cellcounter + 'init';
  desc.id = 'widget' + cellcounter + 'desc';
  desc.name = 'widget' + cellcounter + 'desc';

  name.value = '';
  style.selectedIndex = 0;
  unique.checked = true;
  req.checked = false;
  undef.checked = true;
  type.value = '';
  init.value = '';
  desc.value = '';

  copy.style.display = '';
  cellcounter++;  
  return copy; }

function insAfter(ins, obj) {
  if (obj.nextSibling != undefined) {
  	obj.parentNode.insertBefore(ins, obj.nextSibling);
  } else {
  	obj.parentNode.appendChild(ins); 
  }
  return true}

function insBefore(ins, obj) {
  obj.parentNode.insertBefore(ins, obj); }

function del2par (obj)
 {var node = obj.parentNode.parentNode;
  node.parentNode.removeChild(node);
  return true}

function addPred (obj) { 
	return insAfter(createPredRow(obj.parentNode.parentNode),obj.parentNode.parentNode); }

function addWidget (obj) { 
	return insAfter(createWidgetRow(obj.parentNode.parentNode),obj.parentNode.parentNode); }
	
	
</script>
") (crlf s) (crlf s))

(defun toinfixlist (s l)
  (cond ((null l))
	((listp l)
	 (format s "~A" (car l))
	 (dolist (v (cdr l))
	   (format s ", ~A" v)))))

(defstruct htmlform cssincludes jsincludes javascript widgets errors options onload submitprep action name hidden server)
(defstruct htmlwidget description required html name)

(defparameter *extracss* '("/docserver/infoserver/examples/researchmaster/style/main.css"
			   "/docserver/infoserver/examples/researchmaster/style/jack.css"))
(defparameter *extrajs* nil)
(defparameter *corecss* '("/docserver/infoserver/examples/researchmaster/style/websheet.css"))
(defparameter *corejs* '("/docserver/infoserver/examples/researchmaster/javascript/browser.js"
			 "/docserver/infoserver/examples/researchmaster/javascript/util.js"
			 "/docserver/infoserver/examples/researchmaster/javascript/spreadsheet.js"
			 "/docserver/infoserver/examples/researchmaster/javascript/ds.js"
			 "/docserver/infoserver/examples/researchmaster/javascript/phpjs.tlh.namespaced.min.js"))  ; http://phpjs.org


(defun output-htmlform (s html &optional (table t))
  "(OUTPUT-HTMLFORM S TH) given a declarative description of a web form, return an HTMLFORM."
  (format-html s) (crlf s)
  (format-head s)
  (format s "<title>~A</title>~%" (htmlform-name html))
  (mapc #'(lambda (x) (output-htmlform-style x s)) *extracss*)
  (mapc #'(lambda (x) (output-htmlform-style x s)) (htmlform-cssincludes html))
  (mapc #'(lambda (x) (output-htmlform-js x s)) (htmlform-jsincludes html))
  (when (htmlform-javascript html)
    (format s "<script type=\"text/javascript\">~%")
    (format s "~A" (htmlform-javascript html))
    (format s "function getBigBoxName (x) { return x + 'BIGBOX'; }~%")
    (format s "~&</script>~%"))
  (finish-head s)
  (if (htmlform-onload html) (format-body s *bgcolor* (htmlform-onload html)) (format-body s *bgcolor*)) (crlf s)
  (output-header s)
  (cond ((htmlform-errors html) 
	 (dolist (msg (htmlform-errors html))
	   (format s "<h2>Error: ~A</h2>~%" msg)))
	(t
	 (format s "<div class=\"autoform\">~%")
	 (format s "<span id=\"status\"></span>")
	 (format s "<center>~%")
	 (format s "<form method=\"post\" action=\"~A\" onsubmit=\"return ~A\">~%" 
		 (htmlform-action html) (htmlform-submitprep html))
	 (cond (table
		(format s "<table>~%")
		(dolist (w (htmlform-widgets html))
		  (format s "<tr id='~(~A~)BIGBOX'><td class=\"description\"><span class=\"description\">~A</span></td>~%"
			  (htmlwidget-name w) (htmlwidget-description w))
		  (format s "<td class=\"required\"><span class=\"required\">~A</span></td>~%" 
			  (if (htmlwidget-required w) "*" ""))
		  (format s "<td class=\"data\">~A</td></tr>~%"
			  (htmlwidget-html w)))
		(format s "</table>~%"))
	       (t
		(format s "<center><table><tr><td>~%")
		(dolist (w (htmlform-widgets html))
		  (format s "<div class='row' id='~(~A~)BIGBOX'>" (htmlwidget-name w))
		  (format s "<div class='inputtd'>~A</div>" (htmlwidget-html w))
		  (format s "<div class='requiredtd'>~A</div>" (if (htmlwidget-required w) "*" "&nbsp;"))
		  (format s "<div class='descriptiontd'>~A</div>" (htmlwidget-description w))
		  (format s "<div class='righttd'></div>")  ; to terminate "row"
		  (format s "</div>~%"))
		(format s "</td></tr></table></center>")))
	 (format s "<p><input type=\"submit\" value=\"Submit\">")
	 (dolist (h (htmlform-hidden html))
	   (princ h s))
	 (when (member 'debug (htmlform-options html) :key #'car) 
	   (output-websheet-datalogtest s))
	 (finish-form s)))
  (format s "</center></div>")  ;</div></div>~%")))
  (output-footer s)
  (finish-body s) (crlf s)
  (finish-html s) (crlf s))

(defun output-htmlform-style (style s)
  (format s "<link rel=\"stylesheet\" type=\"text/css\" href=\"~A\">~%" style))

(defun output-htmlform-js (style s)
  (format s "<script type=\"text/javascript\" src=\"~A\"></script>~%" style))

(defun output-websheet-styles (s)
  (mapc #'(lambda (x) (output-htmlform-style x s)) *extracss*)
  (mapc #'(lambda (x) (output-htmlform-style x s)) *corecss*)
  (mapc #'(lambda (x) (output-htmlform-js x s)) *extrajs*)
  (mapc #'(lambda (x) (output-htmlform-js x s)) *corejs*))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Parsing autoform theories ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun parse-formstructure (postlines)
  "(PARSE-FORMSTRUCTURE) given postlines, dump the form structure."
  (let (widgets types constraints definitions options worder torder tmp tmp2 formname )
    (setq widgets (make-hash-table))
    (setq types (make-hash-table))
    (setq worder nil torder nil)
    (dolist (v postlines)
      (cond ((equalp (car v) "formname") (setq formname (read-user-string (cdr v))))
	    ((equalp (car v) "constraints") (setq constraints (read-sentences (cdr v))))
	    ((equalp (car v) "definitions") (setq definitions (read-sentences (cdr v))))
	    ((equalp (car v) "command") nil)
	    ((setq tmp (extract-num (car v) "widget")) 
	     (when (not (member tmp worder)) (push tmp worder))
	     (parse-formstructure-addwidget tmp 
					  (extract-field (car v) "widget") 
					  (cdr v)
					  widgets
					  postlines))
	    ((setq tmp (extract-num (car v) "pred")) 
	     (when (not (member tmp torder)) (push tmp torder))
	     (parse-formstructure-addpred tmp 
					  (extract-field (car v) "pred") 
					  (cdr v)
					  types
					  postlines))
	    (t (push (list (read-user-string (car v)) (equalp (cdr v) "on")) options))))
    ; collect widgets (in proper order)
    (setq tmp nil)
    (dolist (v worder)
      (push (cons 'widget (gethash v widgets)) tmp))
    ; collect type info
    (setq tmp2 nil)
    (dolist (v torder)
      (push (cons 'type (gethash v types)) tmp2))

    ; return theory
    (list* `(formname ,formname)
	   `(constraints ,(quotify constraints))
	   `(definitions ,(quotify definitions))
	   (nconc (mapcar #'(lambda (x) (cons 'option x)) options)
		  tmp
		  tmp2))))

(defun parse-formstructure-addquery (num key val h postlines)
  (declare (ignore postlines))
  (let (tmp)
    (setq tmp (gethash num h))
    (unless tmp (setq tmp (list nil nil)))
    (cond ((eq key 'name)
	   (setf (first tmp) (tosymbol val)))
	  ((eq key 'type)
	   (setf (second tmp) (tosymbol val))))
    (setf (gethash num h) tmp)))

(defun parse-formstructure-addpred (num key val h postlines)
  (declare (ignore postlines))
  (let (tmp)
    (setq tmp (gethash num h))
    (unless tmp (setq tmp (list nil nil)))
    (cond ((eq key 'name)
	   (setf (first tmp) (tosymbol val)))
	  ((eq key 'enum)
	   (setf (second tmp) (cons 'listof (websheet-parse-commalist val)))))
;	  ((eq key 'type)
;	   (setq val (tosymbol val))
;	   (if (eq val 'enum) 
;	       (setq tmp2 (cons 'listof (websheet-parse-commalist 
;					 (getf-post (tostring (list "pred" num "enum")) postlines))))
;	       (setq tmp2 val))
;	   (setf (second tmp) tmp2)))
    (setf (gethash num h) tmp)))

(defun parse-formstructure-addwidget (num key val h postlines)
  (declare (ignore postlines))
  (let (tmp)
    (setq tmp (gethash num h))
    (unless tmp (setq tmp (list :name nil :req nil :init nil 
				:desc nil :style nil :incundef nil :unique nil :typename nil)))
    (cond ((eq key 'name) (setf (second tmp) (tosymbol val)))
	  ((eq key 'req) (setf (fourth tmp) (equalp val "on")))
	  ((eq key 'init) (if (equal val "") 
			      (setf (sixth tmp) nil) 
			      (setf (sixth tmp) (cons 'listof (websheet-parse-commalist val)))))
	  ((eq key 'desc) (setf (eighth tmp) val))
	  ((eq key 'style) (setf (tenth tmp) (tosymbol val)))
	  ((eq key 'incundef) (setf (nth 11 tmp) (equalp val "on")))
	  ((eq key 'unique) (setf (nth 13 tmp) (equalp val "on")))
	  ((eq key 'type) (setf (nth 15 tmp) (tosymbol val))))
    (setf (gethash num h) tmp)))

(defun extract-num (str thing)
  (let (pos l)
    (setq pos (extract-field-position str))
    (setq l (length thing))
    (if (and pos (equalp (subseq str 0 l) thing)) (tosymbol (subseq str l (1+ pos))) nil)))

(defun extract-field-position (str)
  (let (pos1 pos2)
    (setq pos1 (position-if #'digit-char-p str))
    (cond ((not pos1) nil)
	  (t
	   (setq pos2 (position-if #'digit-char-p str :start (1+ pos1)))
	   (if pos2 pos2 pos1)))))

(defun extract-field (str thing) 
  (let (pos l)
    (setq pos (extract-field-position str))
    (setq l (length thing))
    (if (and pos (equalp (subseq str 0 l) thing)) (tosymbol (subseq str (1+ pos))) nil)))

(defun websheet-parse-commalist (str)
  (let (names ws)
    (setq ws (list #\Space #\Tab #\Newline #\Linefeed #\Return))
    (setq names (mapcar #'(lambda (x) (string-trim ws x)) (split-string str (list #\,))))
    (filter #'(lambda (x) (> (length x) 0)) names)))

(defun websheet-parse-type (str dict)
  (let (names v res ws)
    (setq ws (list #\Space #\Tab #\Newline #\Linefeed #\Return))
    (setq names (mapcar #'(lambda (x) (string-trim ws x)) (split-string str (list #\Newline))))
    (setq names (filter #'(lambda (x) (> (length x) 0)) names))
    (setq res nil)
    (dolist (n names)
      (setq v (dictionary-lookup n dict))
      (cond (v (push v res))
	    (t (setq v (gentemp "o"))
	       (push (make-value :symbol v 
				 :prettyname n) res)
	       (dictionary-save n (car res) dict))))
    (nreverse res)))

#|
(defun find-options (postlines)
  (let (results)
    (setq results nil)
    (dolist (v postlines)
      (case (read-user-string (car v))
	(allowconflicts (push (cons 'allowconflicts 'true) results))
	(debug (push (cons 'debug 'true) results))))
    results))
|#


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;; Load plato theory into internal data structure ;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defparameter *ws-vocab-macro*
  (list `(,*ws-assign* = relation)))
(defun ws-macrorepl (p)
  (let (fa ra)
    (setq fa (mapcarnot #'(lambda (x) (if (eq (third x) 'function) (cons (first x) (second x)) nil)) *ws-vocab-macro*))
    (setq ra (mapcarnot #'(lambda (x) (if (eq (third x) 'relation) (cons (first x) (second x)) nil)) *ws-vocab-macro*))
  (subrel ra (subfun fa p))))

(defparameter *ws-builtins* 
  (list    
   (make-parameter :symbol '= :arity 2 :type 'relation)
   (make-parameter :symbol 'lt :arity 2 :type 'relation)
   (make-parameter :symbol 'gt :arity 2 :type 'relation)
   (make-parameter :symbol 'lte :arity 2 :type 'relation)
   (make-parameter :symbol 'gte :arity 2 :type 'relation)
   (make-parameter :symbol '+ :arity 2 :type 'function)
   (make-parameter :symbol '- :arity 2 :type 'function)
   (make-parameter :symbol '* :arity 2 :type 'function)
   (make-parameter :symbol '/ :arity 2 :type 'function)
   (make-parameter :symbol '% :arity 2 :type 'function)
   (make-parameter :symbol 'in :arity 2 :type 'relation)
   (make-parameter :symbol 'tonum :arity 1 :type 'function)
   (make-parameter :symbol 'tostr :arity 1 :type 'function)
   (make-parameter :symbol 'tobool :arity 1 :type 'function)
   (make-parameter :symbol *ws-assign* :arity 2 :type 'relation)  ; assignment (semantic synonym for =)
   ))
(defun ws-builtins () 
  (nconc (mapcar #'(lambda (x) (make-pred :parameter x 
					  :kind :builtin)) 
		 *ws-builtins*)
	 (mapcar #'(lambda (x) (make-pred :parameter (make-parameter :symbol (tosymbol x) :type 'function) 
					  :kind :builtin))
		 *ws-php-builtin-names*)))

(defparameter *ws-php-namespace* "PHP"
  "The prefix that is added to PHP functions in our implementation of those functions.")
; to recreate list....
;egrep -o ',([0-9a-zA-Z])*:function' /Users/thinrich/Research/code/clicl/researchmaster/javascript/phpjs.tlh.namespaced.min.js | sed -e 's/^,\([0-9a-zA-Z]*\):function$/"\1"/' | tr '\n' ' '
(defparameter *ws-php-builtin-names* '("abs" "acos" "acosh" "addcslashes" "addslashes" "arsort" "asin" "asinh" "asort" "atan" "atan2" "atanh" "bin2hex" "bindec" "ceil" "checkdate" "chop" "chr" "compact" "cos" "cosh" "count" "crc32" "date" "D" "j" "l" "N" "S" "w" "z" "W" "F" "m" "M" "n" "t" "L" "o" "Y" "y" "a" "A" "B" "g" "G" "h" "H" "i" "s" "u" "e" "I" "O" "P" "T" "Z" "c" "r" "U" "decbin" "dechex" "decoct" "deg2rad" "doubleval" "echo" "end" "exp" "explode" "expm1" "floatval" "floor" "fmod" "getdate" "getenv" "getrandmax" "gettype" "hexdec" "htmlentities" "htmlspecialchars" "hypot" "implode" "intval" "ip2long" "isset" "join" "krsort" "ksort" "lcfirst" "levenshtein" "localeconv" "log" "log10" "log1p" "long2ip" "ltrim" "max" "microtime" "min" "mktime" "natcasesort" "natsort" "nl2br" "octdec" "ord" "pi" "pow" "printf" "quotemeta" "rad2deg" "rand" "range" "rawurldecode" "rawurlencode" "reset" "round" "rsort" "rtrim" "serialize" "setcookie" "setlocale" "setrawcookie" "settype" "sha1" "shuffle" "sin" "sinh" "sizeof" "sort" "soundex" "split" "sprintf" "sqrt" "strcasecmp" "strchr" "strcmp" "strcspn" "stripos" "stripslashes" "stristr" "strlen" "strnatcasecmp" "strnatcmp" "strncasecmp" "strncmp" "strpbrk" "strpos" "strrchr" "strrev" "strripos" "strrpos" "strspn" "strstr" "strtok" "strtolower" "strtotime" "strtoupper" "strtr" "strval" "substr" "tan" "tanh" "time" "trim" "uasort" "ucfirst" "ucwords" "uksort" "unserialize" "urldecode" "urlencode" "usort" "vprintf" "vsprintf" "wordwrap"))
; DANGER: make sure this parameter is created *after* *ws-php-builtin-names* is created
(defparameter *ws-php-builtin-prefixed-names* 
  (mapcar #'(lambda (x) (tostring *ws-php-namespace* "." x)) *ws-php-builtin-names*)
  "The list of all php builtins with the namespace prefixed")

(defstruct dictionary (index (make-hash-table :test #'equal)))
(defun dictionary-lookup (n dict) 
  (gethash n (dictionary-index dict)))
;  (cdr (assoc n (dictionary-index dict) :test test)))
(defun dictionary-save (n v dict) 
  (setf (gethash n (dictionary-index dict)) v))
;  (setf (dictionary-index dict) (acons n v (dictionary-index dict))))

(defstruct webform name preds univ constraints definitions options submit)
(defvar *predkinds* '(:widget :extensional :intensional :builtin :query))
(defstruct pred parameter (kind :extensional) (display nil))
(defstruct widget name (init "") (typename "") desc (req nil) (style 'selector) (unique t) (incundef t))
(defstruct value symbol (shortname "") (prettyname ""))

; handy routines
(defun webform-widgets (wf) 
  (mapcar #'pred-display 
	  (remove-if-not #'(lambda (x) (eq (pred-kind x) :widget)) (webform-preds wf))))
(defun webform-option (opt webform) (cdr (assoc opt (webform-options webform))))
(defun pred-name (pred) (parameter-symbol (pred-parameter pred)))
(defun pred-unique (pred) (if (widget-p (pred-display pred)) 
			      (widget-unique (pred-display pred)) nil))
(defun pred-univ (pred) (parameter-univ (pred-parameter pred)))

(defvar *infinitetypes* '(string number regexp))
(defvar *builtintypes* (append *infinitetypes* '(boolean)))

(define-condition unknown-symbol (error) ((comment :initarg :comment :accessor comment)))

; some of what follows would be simpler if we didn't have data replicated inside of widgets.
;   but to change that, we need to edit the HTML/Javascript construction code.  For now, we leave it ugly.
(defvar *booleantrue* "true")
(defvar *booleanfalse* "false")
(defparameter *boolean* (list *booleanfalse* *booleantrue*))  ; false first makes it the default
(defun mak-undefinedvalue (x) (make-value :symbol 'undefined :shortname 'undefined :prettyname x))
(defvar *undefinedvalue* (mak-undefinedvalue ""))
(defvar *tmp3*)
(defun load-formstructure (th)
  "(LOAD-FORMSTRUCTURE TH) given a dump (as output by dump-formstructure),
   create the internal WEBFORM object that the code that follows operates on."
  (setq th (define-prologtheory (make-instance 'prologtheory) "" (contents th)))
  (let (w types preds constraints definitions widgets tmp univ options idb edb ts
	  wnames valuecomp booluniv name vocab)
    (setq w (make-webform :name "myform"))

    ; divide theory
    (setq name (viewfindx '?x '(formname ?x) th))
    (setq types (viewfinds '(type @x) '(type @x) th))
    (setq widgets (viewfinds '(widget @x) '(widget @x) th))
    (setq definitions (viewfinds '(definitions @x) '(definitions @x) th))
    (setq constraints (viewfinds '(constraints @x) '(constraints @x) th))
    (setq options (viewfinds '(option @x) '(option @x) th))

    (when name (setf (webform-name w) name))

    ; collapse multiple constraint/definition sets
    (setq constraints (mapcan #'(lambda (x) (second (second x))) constraints))
    (setq definitions (mapcan #'(lambda (x) (second (second x))) definitions))
    ; macro replacement
    (setq constraints (mapcar #'ws-macrorepl constraints))
    (setq definitions (mapcar #'ws-macrorepl definitions))
    
    ; case sensitivity for values
    (if (viewfindp '(option casesensitive t) options) (setq valuecomp #'equal) (setq valuecomp #'equalp))

    ; make widgets into objects
    (setq widgets (mapcar #'(lambda (w) (apply #'make-widget (cdr w))) widgets))
    (setq wnames (mapcar #'widget-name widgets))

    ; construct universe of objects
    (setq univ (mapcan #'(lambda (x) (if (listp (third x)) (copy-list (cdr (third x))) nil)) types))
    (setq univ (nconc (objs (maksand constraints)) univ))
    (setq univ (nconc (objs (maksand definitions)) univ))
    (setq univ (nconc (delete nil (mapcan #'(lambda (x) (copy-list (cdr (widget-init x)))) widgets)) univ))
    (setq univ (append *boolean* univ))
    (setq univ (append '("" "1") univ))   ; default values for string and number
    (setq univ (delete-duplicates univ :test valuecomp))
    (setq univ (mapcar #'(lambda (x) (make-value :symbol (gentemp "o") :prettyname x)) univ))
    (setf (webform-univ w) univ)
    (addshortnames w)

    ; theories and options
    ;  change propositional predicates to monadics
    (setf (webform-constraints w) 
	  (websheet-theory-prettynames-to-symbols 
	   (mapcar #'(lambda (x) (bool2monad x wnames)) constraints) w :test valuecomp))
    (setf (webform-definitions w) 
	  (websheet-theory-prettynames-to-symbols definitions w :test valuecomp))
    (setf (webform-options w) (mapcar #'cdr options))
    (mapc #'(lambda (x) (cond ((eq (second x) t) (setf (second x) 'true))
			      ((eq (second x) nil) (setf (second x) 'false))))
	  (webform-options w))

    ; predicates
    ;  build parameters from types
    ;  parameter universe is either a list of actual values or the name of an infinite type
    (setq booluniv (ws-vals2objs *boolean* univ valuecomp))
    (setq ts (list (make-parameter :symbol 'boolean :type 'relation :arity 1 :univ booluniv)))
    (dolist (x types)
      (when (second x)
	(cond ((eq (third x) 'boolean) (setq tmp booluniv))
	      ((atom (third x)) (setq tmp (third x)))
	      (t (setq tmp (ws-vals2objs (cdr (third x)) univ valuecomp))))
	(push (make-parameter :symbol (second x) :type 'relation :arity 1 :univ tmp) ts)))
    (setq types (nreverse ts))

    ; adjust widgets
    (mapc #'(lambda (x) (ws-fixwidget x univ types valuecomp)) widgets)

    ;;; Build predicate list ;;;
    ; Widgets: those declared to be widgets
    ; IDB: those appearing in the head of some rule in Definitions
    ; EDB: those appearing only as data in Definitions
    ; Builtins: the pre-defined relations/functions
    ; Queries: all the remaining predicates in the constraints 
    ;   (all functions are Builtins)
    (setq preds nil)

    ; Builtins
    (push (make-pred :parameter (make-parameter :symbol (mak-univ) :arity 1 :type 'relation :univ univ)
		     :kind :extensional) preds)
    (setq preds (nconc (ws-builtins) preds))

    ; Widgets stay :widget
    (dolist (p widgets)
      (push (make-pred :parameter (make-parameter :symbol (widget-name p) 
						  :arity 1 :type 'relation
						  :univ (widget-typename p))
		       :kind :widget :display p) preds))
    ; Types become :EDB
    (dolist (p types)
     (cond ((listp (parameter-univ p)) 
	    (push (make-pred :parameter p :kind :extensional) preds))
	   ((member (parameter-univ p) *infinitetypes*) 
	    (push (make-pred :parameter p :kind :builtin) preds))
	   (t (assert nil nil (format nil "Unknown named type ~A for typename ~A~%" 
				      (parameter-univ p) (parameter-symbol p))))))

    ; IDB/EDB
    (multiple-value-setq (edb idb) (split #'datap (webform-definitions w)))
    (setq idb (preds (maksand idb)))
    (setq edb (set-difference (preds (maksand edb)) idb :test #'param-equal))
    (dolist (p idb) (push (make-pred :parameter p :kind :intensional) preds))
    (dolist (p edb) (push (make-pred :parameter p :kind :extensional) preds))
 
    ; Queries
    (dolist (p (set-difference (preds (maksand constraints))
			       (mapcar #'pred-parameter preds)
			       :test #'param-equal))
      (push (make-pred :parameter p :kind :query) preds))
    (setf (webform-preds w) (nreverse preds))

    ; check that functions that appear in constraints are part of our pred list.
    ;   (All relations are accounted for, by the definitions of :query.)
    ;  Note that we don't have arities recorded for all builtins, so we're just comparing names.
    (when constraints  ; if constraints empty, true appears which isn't a built-in relation
      (setq vocab (mapcar #'parameter-symbol (delete-if #'isobject (get-vocabulary (maksand constraints)))))
      (setq preds (mapcar #'(lambda (x) (parameter-symbol (pred-parameter x))) (webform-preds w)))
      (when (set-difference vocab preds) (error 'unknown-symbol :comment vocab)))
    w))

(defun ws-fixwidget (w univ types valuecomp)
  "(WS-FIXWIDGET W) adjusts the given widget W to meet assumptions of compiler."
  (let (type)
    ; if typename not supplied or unknown, set to univ
    (setq type (find (widget-typename w) types :key #'parameter-symbol))
    (unless (or type (member (widget-typename w) *builtintypes*))
      (setf (widget-typename w) (mak-univ))) 

    ; set display so that the typename, unique, and undef fields can all be satisfied
    (cond ((find (widget-typename w) *infinitetypes*) (setf (widget-style w) 'textbox))
	  ((and (widget-unique w) (widget-incundef w))
	   (when (eq (widget-style w) 'checkbox) (setf (widget-style w) 'radio)))
	  ((and (widget-unique w) (not (widget-incundef w)))
	   (when (and (eq (widget-style w) 'checkbox) (not (= (ws-widget-univ-size w types) 2)))
	     (setf (widget-style w) 'radio)))
	  ((and (not (widget-unique w)) (widget-incundef w))
	   (when (eq (widget-style w) 'radio)
	     (setf (widget-style w) 'checkbox)))
	  ((and (not (widget-unique w)) (not (widget-incundef w)))
	   (when (eq (widget-style w) 'radio)
	     (setf (widget-style w) 'checkbox))))

    ; ensure there's a reasonable initial value for the widget
    (setf (widget-init w) (ws-compute-initval w univ types valuecomp))))

(defun ws-widget-univ-size (w types)
  (cond ((find (widget-typename w) *infinitetypes*) 'infinite)
	((eq (widget-typename w) 'boolean) 2)
	(t (let (type)
	     (setq type (find (widget-typename w) types :key #'parameter-symbol))
	     (cond ((not type) nil)
		   ((eq (parameter-univ type) :unknown) nil)
		   (t (length (parameter-univ type))))))))
	
(defun ws-vals2objs (vals univ valuecomp) 
  (mapcar #'(lambda (y) (find y univ :key #'value-prettyname :test valuecomp)) vals))

(defun ws-compute-initval (w univ types valuecomp)
  "(WS-COMPUTE-INITVAL W UNIV TYPES VALUECOMP) choose initial value for widget W.
   Return value is either NIL indicating undefined, (listof) indicating the empty set,
   or (listof v1 ... vn) where vi is a value struct for each i.  Assumes (widget-init w)
   is either NIL or a list beginning with LISTOF.  If (not (widget-incundef w)) then will not return NIL."
  (assert (or (null (widget-init w))
	      (and (listp (widget-init w)) (eq (first (widget-init w)) 'listof)))
	  nil "COMPUTE-INITVAL requires widget-init either be NIL or a list starting with LISTOF")
  ; turn list of simple values into (possibly smaller but type-safe) list of value objects
  (let (user type)
    (setq user (ws-vals2objs (cdr (widget-init w)) univ valuecomp))
    (setq type (widget-typename w))
    (unless (member type *infinitetypes*)
      (setq type (find type types :key #'parameter-symbol))
      (when type (setq type (parameter-univ type)))
      (setq user (intersection user type :test #'equalp)))
    (when (and (widget-unique w) user) (setq user (list (first user))))
    (when (widget-init w) (setq user (cons 'listof user)))
    ; either return user's values (always possible if incundef is true) or construct new list
    ;(print type)
    ;(print user)
    (cond ((or user (widget-incundef w)) user)
	  ((listp type) (list 'listof (first type)))
	  ((eq type 'string) (cons 'listof (ws-vals2objs (list "") univ valuecomp)))
	  ((eq type 'boolean) (cons 'listof (ws-vals2objs (list *booleanfalse*) univ valuecomp)))
	  ((eq type 'number) (cons 'listof (ws-vals2objs (list "1") univ valuecomp)))
	  (t (assert nil nil (format nil "Couldn't compute initial value for ~A" (widget-name w)))))))


(defun addshortnames (struct)
  "(ADDSHORTNAMES STRUCT) adds shortnames for all values in the universe."
  (let ((i 0))
    (dolist (v (webform-univ struct) struct)
      (if *ws-use-value-shortnames* 
	  (setf (value-shortname v)  i)
	  (setf (value-shortname v) (value-prettyname v)))
      (setq i (1+ i)))))

(defun bool2monad (p widgets) 
  (cond ((atom p) (if (member p widgets) (list p *booleantrue*) p))
	((member (first p) '(=> <= <=> or and not)) 
	 (cons (first p) (mapcar #'(lambda (x) (bool2monad x widgets)) (cdr p))))
	((member (car p) '(forall exists))
	 (list* (first p) (second p) (bool2monad (third p) widgets)))
	((cdr p) p)
	((member (first p) widgets) (list (first p) *booleantrue*))
	(t p)))
   
; swap all user object constants with symbols
; assumes predicates are disjoint from objects
(defun websheet-theory-prettynames-to-symbols (th webform &key (test #'equal))
  (let (alist)
    (setq alist (mapcar #'(lambda (x) (cons (value-prettyname x) (value-symbol x))) 
		  (webform-univ webform)))
    (mapcar #'(lambda (x) (subobj alist x :test test)) (contents th))))
 

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Webform Compiler ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-condition inconsistent-constraints (error) ((comment :initarg :comment :accessor comment)))
(define-condition cyclic-constraints (error) ((comment :initarg :comment :accessor comment)))
(define-condition timeout (error) ((comment :initarg :comment :accessor comment)))

(defun make-stringbuffer () (make-array 0 :element-type 'character :adjustable t :fill-pointer 0))
(defvar *tmp4*)
(defun compile-websheet (th)
  "(COMPILE-WEBSHEET TH) given a declarative description of a web form, return an HTMLFORM."
  (setq *tmp* nil *tmp2* nil *tmp3* nil *tmp4* nil)  ; for commandline debugging
  (let (struct code buf result)
    (setq result (make-htmlform))
    (when *webformlog* (logmessage (contents th) *webformlog*))
    (handler-case (setq struct (load-formstructure th))
      (unknown-symbol (msg) 
	(setf (htmlform-errors result) (list (format nil "Unknown symbol(s): ~A" (comment msg))))
	(return-from compile-websheet result)))
    (setq *tmp* struct)

    ; compile websheet to code
    ; call construct-websheet-intcode-safe, except then exception handling doesn't work
    (handler-case (setq code (construct-websheet-intcode struct))  
      (inconsistent-constraints () 
	(setf (htmlform-errors result) (list "Inconsistent constraints")) 
	(return-from compile-websheet result))
      (timeout () 
	(setf (htmlform-errors result) (list "Permitted resources exhausted")) 
	(return-from compile-websheet result))
      (cyclic-constraints () 
	(setf (htmlform-errors result) (list "Cyclic constraints")) 
	(return-from compile-websheet result)))
    (setq *tmp4* code)

    ; translate code to client-side and server-side code
    (setq buf (make-stringbuffer))
    (intcode-to-client code buf)
    (setf (htmlform-javascript result) buf)
    ;(setq buf (make-stringbuffer))
    ;(intcode-to-server code buf)
    (setf (htmlform-server result) (intcode-tp code))

    ; fill in remaining values for rendering form on client
    (setf (htmlform-onload result) "init()")
    (setf (htmlform-submitprep result) "submitprep()")
    (setf (htmlform-name result) (webform-name struct))
    (setf (htmlform-action result) (format nil "/plato/commitform?"))
    (dolist (r (webform-widgets struct))
      (setq code (compile-websheet-widget r struct))
      (when code (push code (htmlform-widgets result))))
    (push (format nil "<input type=\"hidden\" name=\"formname\" id=\"formname\" value=\"~A\">" 
		  (webform-name struct))
	  (htmlform-hidden result))
    (push (format nil "<input type=\"hidden\" name=\"time\" id=\"time\">")
	  (htmlform-hidden result))
    (setf (htmlform-widgets result) (nreverse (htmlform-widgets result)))
    (setf (htmlform-options result) (webform-options struct))
    (setf (htmlform-cssincludes result) *corecss*)
    (setf (htmlform-jsincludes result) *corejs*)
    result))

(defun compile-websheet-widget (r struct)
  "(COMPILE-WEBSHEET-WIDGET R STRUCT) takes a widget and a webform struct and returns an HTML widget
   representing R."
  (let ((res nil) s)
    (when (widget-name r)
      (setq res (make-htmlwidget))
      (setf (htmlwidget-name res) (widget-name r))
      (setf (htmlwidget-description res) (widget-desc r))
      (setf (htmlwidget-required res) (widget-req r))
      (setq s (make-array 0 :element-type 'character :adjustable t :fill-pointer 0))
      (format s "<span id=\"~Abox\" class=\"databox\" onMouseOver=\"~A\" onMouseOut=\"~A\" >~%" 
	      (websheet-cellname (widget-name r))
	      (format nil "~(~A_mouseover('~A')~)" (widget-style r) (websheet-cellname (widget-name r)))
	      (format nil "~(~A_mouseout('~A')~)" (widget-style r) (websheet-cellname (widget-name r))))
      (cond ((eq (widget-style r) 'selector)
	     (output-websheet-selector  
	      s 
	      (websheet-cellname (widget-name r)) 
	      (pred-univ (find (widget-typename r) (webform-preds struct) :key #'pred-name))
	      (widget-unique r)
	      (widget-incundef r)
	      (widget-init r)
	      (tostring (list "selector_change('" (websheet-cellname (widget-name r)) "')"))
	      (tostring (list "selector_focus('" (websheet-cellname (widget-name r)) "')"))))
	    ((eq (widget-style r) 'textbox)
	     (output-websheet-textbox  
	      s 
	      (websheet-cellname (widget-name r)) 
	      (widget-unique r)
	      (widget-incundef r)
	      (widget-init r)
	      (tostring (list "textbox_change('" (websheet-cellname (widget-name r)) "')"))
	      (tostring (list "textbox_focus('" (websheet-cellname (widget-name r)) "')"))))
	   ((eq (widget-style r) 'radio)
	     (output-websheet-radio 
	      s
	      (websheet-cellname (widget-name r)) 
	      (pred-univ (find (widget-typename r) (webform-preds struct) :key #'pred-name))
	      (widget-unique r)
	      (widget-incundef r)
	      (widget-init r)
	      (tostring (list "radio_change('" (websheet-cellname (widget-name r)) "')"))
	      (tostring (list "radio_focus('" (websheet-cellname (widget-name r)) "')"))))
	   ((eq (widget-style r) 'checkbox)
	     (output-websheet-checkbox 
	      s 
	      (websheet-cellname (widget-name r)) 
	      (pred-univ (find (widget-typename r) (webform-preds struct) :key #'pred-name))
	      (widget-unique r)
	      (widget-incundef r)
	      (widget-init r)
	      (tostring (list "checkbox_change('" (websheet-cellname (widget-name r)) "')"))
	      (tostring (list "checkbox_focus('" (websheet-cellname (widget-name r)) "')")))))
      (format s "</span>")
      (setf (htmlwidget-html res) s)
      res)))

; For now, on multi-select widgets, the empty set signals undefined.  In other words,
;   the user can choose any set except the empty set.
;   Later, we should add an option that forces the empty set to mean the empty set.
;   If both incundef and emptyset must be expressible, we need another widget.
;   Programmatically, this means that (listof) and NIL are treated as the same initial value.

(defun output-websheet-selector (s nameid options unique incundef values changeaction focus)
  (when (and unique incundef) (setq options (cons *undefinedvalue* options)))
  (output-ws-selector s nameid options (cdr values) changeaction focus nil (not unique)))

(defun output-ws-selector (s nameid options values changeaction focus disabled multi)
  (format s "<select name='~A' onChange=\"~A\" onFocus=\"~A\"~A~A>" 
	  (websheet-cellname nameid) changeaction focus (if disabled "disabled" "")
	  (if multi " multiple" ""))
  (dolist (option options)
    (format s "<option value='~A'~A>~A</option>"
	    (js-thing (value-shortname option))
	    (if (member option values :test #'equalp) " selected='true'" "")
	    (value-prettyname option)) (crlf s))
  (format s "</select>"))

(defun output-websheet-textbox (s nameid unique incundef values changeaction focus)
  (setq values (cdr values))
  (if unique
      (output-ws-textbox s nameid incundef (first values) changeaction focus nil)
      (output-websheet-multi s nameid incundef values changeaction focus #'output-ws-textbox)))

(defun output-ws-textbox (s nameid incundef value changeaction focus disabled)
  (declare (ignore incundef))
  (format s "<input type='text' name='~A' onChange=\"~A\" onFocus=\"~A\" value='~A'~A>"
	  (websheet-cellname nameid) 
	  changeaction
	  focus 
	  (if value (value-prettyname value) "")
	  (if disabled " disabled" "")))

(defun output-websheet-multi (s nameid incundef values changeaction focus widgetfunc)
  (let (ghost att bottom)
    (setq ghost (format nil "~A_ghost" nameid))
    (setq att (format nil " style='display:none; background-color:white;' id='~A'" ghost))
    (setq bottom (format nil "~A_bottom" nameid))
    (format s "<table border='0'>")
    (dolist (v (cons t values))
      (format s "<tr~A>~%<td>" (if (eq v 't) att nil))
      (output-plusminus-widget s (format nil "addTableRow('~A',this)" ghost)
			       (format nil "remWidgetSlot('~A',this)" (websheet-cellname nameid)))
      (format s "~&</td><td>~%")
      (funcall widgetfunc s nameid incundef (if (eq v 't) nil v) changeaction focus (eq v 't))
      (format s "~&</td></tr>~%"))
    (format s "<tr style='background-color:white'><td align='left' colspan='2'>")
    (output-plus-widget s (format nil "addTableRow('~A',this)" ghost) bottom)
    (format s "</td></tr></table>")))

; checkbox does not handle undefined value or multiple values, but we should never get here if either is the case
(defun output-websheet-checkbox (s nameid options unique incundef values changeaction focus)
  (declare (ignore incundef))
  (setq values (cdr values))
  (assert (or (not unique) (not (cddr values))) nil 
	  (format nil "Widget ~A cannot be a checkbox because it is unique and not boolean." nameid))
  (if unique
      (output-ws-checkbox s nameid (first options) (equalp (first values) (first options))
			  changeaction focus nil)
      (dolist (o options)
	(output-ws-checkbox s nameid o (member o values :test #'equalp) changeaction focus nil)
	(format s "~%"))))

(defun output-ws-checkbox (s nameid value checked changeaction focus disabled)
  (format s "<input type='checkbox' name='~A' value='~A' onChange=\"~A\" onFocus=\"~A\"~A~A>~A"
	  (websheet-cellname nameid) 
	  (js-thing (value-shortname value))
	  changeaction
	  focus
	  (if checked " checked" "")
	  (if disabled " disabled" "")
	  (if (member (value-prettyname value) *boolean* :test #'equalp) "" (value-prettyname value))))

(defun output-websheet-radio (s nameid options unique incundef values changeaction focus)
  (declare (ignore unique))
  (setq values (cdr values))
  (when incundef
    (setq options (cons (mak-undefinedvalue "Undefined") options)))
  (dolist (o options)
    (output-ws-radio s nameid o (member o values :test #'equalp) changeaction focus nil)
    (format s "~%")))

(defun output-ws-radio (s nameid value checked changeaction focus disabled)
  (format s "<input type='radio' name='~A' value='~A' onChange=\"~A\" onFocus=\"~A\"~A~A>~A "
	  (websheet-cellname nameid)
	  (js-thing (value-shortname value))
	  changeaction
	  focus
	  (if checked " checked" "")
	  (if disabled " disabled" "")
	  (value-prettyname value)))



(defun output-websheet-datalogtest (s)
(format s "
<p>
<hr width='90%'>
<h2>Testing</h2>
<p>Messages</p>
<textarea cols='30' rows='5' id='msg' name='msg'>
</textarea>

<p>Eval<br>
<textarea cols='30' rows='5' id='evalbox' name='evalbox'>
</textarea><br>
<input type='button' value='Eval' onclick=\"alerteval('evalbox');\">

"))

#|
<p>Query<br>
<!-- cell(q,V1) & univ(X0) & ~~assoc(X0,V1) -->
<textarea cols='30' rows='5' id='query' name='query'>
</textarea>

<p>Theory<br>
<textarea cols='30' rows='5' id='logic' name='logic'>
")
(output-websheet-javascript-datalog struct s)
(format s "
</textarea>

<p>
<input type='button' value='Findp' onclick=\"queryp('query', 'logic')\">
<input type='button' value='Findx' onclick=\"queryx('query', 'logic')\">
<input type='button' value='Finds' onclick=\"querys('query', 'logic')\">
<input type='button' value='Proofx' onclick=\"queryproofx('query', 'logic')\">
<input type='button' value='Proofs' onclick=\"queryproofs('query', 'logic')\">
<input type='button' value='Conflicts' onclick=\"conflicts('query')\">

")) |#

(defun websheet-cellname (p) (format nil "~(~A~)" p))
(defun websheet-valuename (v) 
  (cond ((and v (not (eq v 'unknown))) 
	 (format nil "~(~A~)" (toalpha (tostring v))))
	(t "")))


(defun toalpha (s)
  "(TOALPHA S) takes a string s and returns a version of s using only letters."
  (reduce #'(lambda (x y) (stringappend x "_" y)) 
	  s
	  :key #'(lambda (x) (case x
			       (#\0 "zero")
			       (#\1 "one")
			       (#\2 "two")
			       (#\3 "three")
			       (#\4 "four")
			       (#\5 "five")
			       (#\6 "six")
			       (#\7 "seven")
			       (#\8 "eight")
			       (#\9 "nine")
			       (otherwise x)))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Logic to Javascript/Lisp/etc. Compiler ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defparameter *prefixeq* 'tlheq)
(defstruct datalog extensional intensional components completepreds)
(defvar *tmp2*)
(defstruct intcode tp support objs)

; wrap complex code with run-time
(defun construct-websheet-intcode-safe (struct)
  (run-time 120 #'construct-websheet-intcode struct)
  (when (eq *run-time-result* 'timeout)
    (error 'timeout))
  *run-time-result*)

(defun construct-websheet-intcode (webform)
  (setq *tmp* webform)
  (let (datalog code falist)
    (setq datalog (construct-websheet-datalog webform))
    ; prepend built-in function names to simulate namespaces
    (setq falist (mapcar #'(lambda (x y) (cons (tosymbol x) (tosymbol y)))
			 *ws-php-builtin-names* *ws-php-builtin-prefixed-names*))
    (setf (datalog-extensional datalog) (mapcar #'(lambda (x) (subfun falist x)) (datalog-extensional datalog)))
    (setf (datalog-intensional datalog) (mapcar #'(lambda (x) (subfun falist x)) (datalog-intensional datalog)))
    ; change objects to their short-values (the values the intermediate code operates on directly for =)
    (setq falist (mapcar #'(lambda (x) (cons (value-symbol x) (value-shortname x))) (webform-univ webform)))
    (setf (datalog-extensional datalog) (mapcar #'(lambda (x) (subobj falist x)) (datalog-extensional datalog)))
    (setf (datalog-intensional datalog) (mapcar #'(lambda (x) (subobj falist x)) (datalog-intensional datalog)))
    ; compile datalog to intermediate code
    (setq *tmp2* datalog)
    (setq code (make-intcode))
    (setf (intcode-tp code) (compile-ext (append (datalog-extensional datalog) (datalog-intensional datalog))
					 (webform-preds webform)))
    (setf (intcode-support code) (construct-websheet-code-support datalog webform))
    code))

; The following two functions expect an intcode struct.
(defun intcode-to-server (code s)
  (intcode-to-lisp (intcode-tp code) s))

(defun intcode-to-client (code s)
  (intcode-to-javascript (intcode-to-client-extra code) s)
  (intcode-to-javascript (intcode-support code) s)
  (intcode-to-javascript (intcode-tp code) s))

(defun intcode-to-client-extra (code)
  (declare (ignore code))
  (list 
   (mak-vardecl* `((,*ws-php-namespace* ,(mak-class "PHP_JS"))))))

; The following two functions expect a list of intcode statements
(defun intcode-to-lisp (code s)
  (format s "~S" code)
  nil)

(defun intcode-to-javascript (code s)
  (dolist (c code)
    (tojavascript c (signifier c) 0 s)
    (crlf s) (crlf s))
  nil)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Logic to Datalog Compiler ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun construct-websheet-datalog (struct)
  "(CONSTRUCT-WEBSHEET-DATALOG STRUCT) computes datalog implementing constraints for
   webform STRUCT.  Returns a list of (<=> (IMPL 'P) (or (and ...) ...)), one for
   each widget and query predicate. The incoming constraints are allowed to include built-in 
   functions; the resulting datalog flattens those functions, e.g. instead of the atom 
   p(f(x,y),z), we get p(w,z) ^ w=f(x,y)."
  (let (constraints lpreds cpreds comp dpreds qpreds wpreds)
    (setq constraints (add-uva (webform-constraints struct) struct))
    (multiple-value-setq (wpreds lpreds) 
      (split #'(lambda (x) (eq (pred-kind x) :widget)) (webform-preds struct)))
    ;(setq mpreds (remove-if #'(lambda (x) (widget-unique (pred-display x))) wpreds))
    (multiple-value-setq (qpreds cpreds) 
      (split #'(lambda (x) (eq (pred-kind x) :query)) lpreds))
    (setq dpreds (remove-if #'(lambda (x) (eq (pred-kind x) :builtin)) cpreds))

    (setq wpreds (mapcar #'pred-parameter wpreds)) ; widgets
    ;(setq mpreds (mapcar #'pred-parameter mpreds)) ; non-unique (multi) widgets
    (setq lpreds (mapcar #'pred-parameter lpreds))
    (setq qpreds (mapcar #'pred-parameter qpreds)) ; query-only
    (setq cpreds (mapcar #'pred-parameter cpreds)) ; complete
    (setq dpreds (mapcar #'pred-parameter dpreds)) ; preds with definitions in defs
#|
    (format t "~&wpreds: ~A~%lpreds: ~A~%qpreds: ~A~%cpreds: ~A~%dpreds: ~A~%"
	    (mapcar #'parameter-symbol wpreds)
	    ;(mapcar #'parameter-symbol mpreds)
	    (mapcar #'parameter-symbol lpreds)
	    (mapcar #'parameter-symbol qpreds)
	    (mapcar #'parameter-symbol cpreds)
	    (mapcar #'parameter-symbol dpreds))
|#

    (when (and (intersection cpreds (preds constraints) :key #'parameter-symbol)
	       (agraph-cyclicp (syntactic-finiteness-graph (propositional-clauses (maksand constraints)))))
      (error 'cyclic-constraints))

    ; compute components before compiling constraints
    (setq comp (agraph-connected-components 
		(undirected-dependency-graph constraints lpreds :test #'param-equal) 
		:test #'param-equal))

    (setq constraints (fhlc-webform-multi constraints cpreds qpreds))
    (when (member '(or) constraints :test #'equal)
      (error 'inconsistent-constraints))

    (setq cpreds (mapcar #'parameter-symbol cpreds))
    (setq comp (maptree #'parameter-symbol comp))

    (make-datalog :extensional (nconc (materialize (webform-definitions struct) dpreds) 
				      (create-type-data (webform-preds struct)))
		  :intensional constraints
		  :components comp
		  :completepreds cpreds)))

(defun ws-datalog-ordering (p bound preds typehash univ)
  "(WS-DATALOG-ORDERING P BOUND PREDS TYPEHASH) takes a biconditional P: (<=[>] LIT [(or] RULEBOD1 ... RULEBODN[)])
   where each RULEBODI is a conjunction of literals, a list of variables BOUND that are bound,
   the list of webform predicates (including relations/functions), a TYPEHASH that maps predicate names
   to type names, and the predicate name for the universe.  
   Returns a list of datalog queries: one for each RULEBOD1, made safe and efficient for left-to-right eval."
  (let (disjs builtins)
    (setq builtins (mapcarnot #'(lambda (x) (if (eq (pred-kind x) :builtin) (pred-parameter x) nil)) preds))
    (setq disjs (drop-ors (drop-exists (third p))))
    ; order each conjunction, taking binding list into account
    (setq disjs (mapcar #'(lambda (x) (maksand (body (order-datalog-rule (list* '<= (head p) (drop-ands x))
									 :hashedtypes typehash :defaulttype univ 
									 :attach builtins :bound bound :assign *ws-assign*))))
			disjs))
    ; check for degenerate cases--should never need this, but just in case.
    (setq disjs (delete 'false disjs))
    (if (member 'true disjs) '(true) disjs)))

#|  Call for non-uva version, sort of
(defun fhlc-webform-nonuva (th cpreds)
    (fhlc th 'complete 'valid :dca nil :una nil
	  :options `((transformation . allpreds) 
		     (fullyfactored . t)
		     (predicates . ,cpreds))))
|#

(defun fhlc-webform-uva (th cpreds qpreds)
  (let (pos neg origsize hornsize)
    (assert (eq (get-theory-type th) 'quantifier-free)
	    nil "FHL-WEBFORM-UVA requires a quantifier-free theory")

    ; compute all prime clausal consequences using resolution and perform some simplification
    (setq th (funcall *resolution-closure* (clauses (maksand (contents th))) *limit* cpreds))
    (when (member '(or) th :test #'equal)
      (return-from fhlc-webform-uva '((or))))
    (setq th (mapcar #'herbrand-simplify th))
    (when (member 'false th)
      (return-from fhlc-webform-uva '((or))))
    (setq th (remove-if #'(lambda (x) (eq x 'true)) th))

    ; drop all non-Horn rules and fully factor the rest
    (setq origsize (length th))
    (setq th (delete-if-not #'(lambda (x) (horn-clausep x cpreds)) th))
    (setq hornsize (length th))
    (setq th (mapcarnot #'(lambda (x) (fully-factor-all x cpreds)) th))
    (format t "Resolution closure: ~A, Horn rules: ~A.  Percentage Horn: ~D~%" 
	    origsize hornsize (* 100 (* 1.0 (/ hornsize origsize))))
 
    ; compute set of query predicates and turn clauses into clausesets
    (setq th (mapcar #'drop-ors th))

    ; split definite Horn and purely negative clauses (while ignoring cpreds)
    (multiple-value-setq (pos neg) 
      (split #'(lambda (x) 
		 (some #'(lambda (y) (and (positive-literalp y) 
					  (not (member (relation y) cpreds :key #'parameter-symbol)))) x)) 
		 th))

    ; construct positive and negative rules
    (setq pos (fhl-webform-uva-positive pos cpreds qpreds))
    (setq neg (fhl-webform-uva-negative neg cpreds qpreds))
    (nconc pos neg)))

(defun fhlc-webform-multi (th cpreds qpreds)
  (let (origsize ressize)
    (assert (eq (get-theory-type th) 'quantifier-free)
	    nil "FHL-WEBFORM-NO-EQ requires a quantifier-free theory")

    ; compute all prime clausal consequences using resolution and perform some simplification.
    ; flattening functions so that all built-in functions appear inside = (a built-in relation).
    (setq th (mapcar #'(lambda (x) (flatten-functions (nnf x))) (contents th)))
    (setq origsize (length th))
    (print th)
    (setq th (funcall *resolution-closure* (clauses (maksand th)) *limit* cpreds))
    (when (member '(or) th :test #'equal)
      (return-from fhlc-webform-multi '((or))))
    (setq th (mapcar #'herbrand-simplify th))
    (when (member 'false th)
      (return-from fhlc-webform-multi '((or))))
    (setq th (remove-if #'(lambda (x) (eq x 'true)) th))
    (print th)
    (setq ressize (length th))
    (when (> ressize 0)
      (format t "Orig: ~A, Resolution closure: ~A, Axiomatization percentage: ~D~%" 
	      origsize ressize (* 100 (* 1.0 (/ origsize ressize)))))

    ; fully factor all clauses and turn into clausesets
    ;  Removed since it seems unsound for multi-valued widgets
    ;(setq th (mapcarnot #'(lambda (x) (fully-factor-all x (union cpreds multipreds :test #'param-equal))) th))

    ; construct all rules
    (fhl-webform-uva-impl-queries 
     (mapcan #'(lambda (x) (contrapositives* x (remove-if #'isfunction cpreds))) th) qpreds)))

(defun fully-factor-all (lits &optional (ignorepreds nil))
  "(FULLY-FACTOR-ALL LITS) takes a list of literals LITS. Suppose p occurs in LITS and is not in IGNOREPREDS. 
   If there is any predicate p occurring both positively and negatively, returns nil.
   If there is no most general substitution s that when applied to lits produces a literal set
   with one occurrence of each predicate, returns nil.  Otherwise, returns lits after applying
   such a substitution.  "
  (let ((mgu truth) h)
    (setq h (split-by-preds lits ignorepreds))
    (when (not h) (return-from fully-factor-all nil))
    (with-hash-table-iterator (my-iterator h)
      (loop
	 (multiple-value-bind (entry-p key value) (my-iterator)
	   (declare (ignore key))
	   (cond (entry-p
		  (setq mgu (mgun value mgu))
		  (when (not mgu) (return nil)))
		 (t (return (remove-duplicates (plug lits mgu) :test #'equal)))))))))

(defun split-by-preds (lits ignorepreds)
  (let ((h (make-hash-table)) (signs (make-hash-table)) tmp)
    (dolist (l lits h)
      (unless (member (relation l) ignorepreds :key #'parameter-symbol)
	(cond ((setq tmp (gethash (relation l) signs))
	       (unless (samesigns tmp l) (return-from split-by-preds nil))
	       (setf (gethash (relation l) h) (cons l (gethash (relation l) h))))
	      (t
	       (setf (gethash (relation l) signs) l)
	       (setf (gethash (relation l) h) (list l))))))))

(defun fhl-webform-uva-positive (clausesets &optional (ignorepreds nil) (qpreds nil))
  "(FHL-WEBFORM-UVA-POSITIVE CLAUSESETS) returns a list of sentences of the form
   (<=> (impl '(p @x) @x) (psi @x)) for each p occurring positively in clausesets except for ignorepreds.  
   CLAUSESETS consists of define horn clausesets, ignoring IGNOREPREDS.  QPREDS defines
   the set of predicates that should never appear in the body of rules because they are only for
   queries."
  (fhl-webform-uva-impl-queries 
   (mapcar #'(lambda (x) (to-horn-rule (brf (maksor x)) ignorepreds)) clausesets)
   qpreds))

(defun fhl-webform-uva-negative (clausesets &optional (ignorepreds nil) (qpreds nil)) 
  "(FHL-WEBFORM-UVA-NEGATIVE CLAUSESETS) returns a list of sentences of the form
   (<=> (impl '(neg (p @x)) @x) (psi @x)) for each p occurring (negatively) in clausesets 
   except for ignorepreds.  
   CLAUSESETS consists of purely negatively clausesets, ignoring IGNOREPREDS.  QPREDS defines
   the set of predicates that should never appear in the body of rules because they are only for
   queries."
  (fhl-webform-uva-impl-queries 
   (mapcan #'(lambda (x) (contrapositives* (maksor x) ignorepreds)) clausesets)
   qpreds))
  
(defun fhl-webform-uva-impl-queries (rules qpreds)
  "(FHL-WEBFORM-UVA-IMPL-QUERIES PREDS RULES QPREDS) takes a list of rules and a set of predicates
   which should only appear in rule heads.
   It returns a list of sentences of the form (<=> (impl '(not (p @x)) @x) (psi @x)), which except
   for impl is the predicate completion for p."
  (let (vars (result nil) rs spreds)
    ; throw out rules with qpreds in body
    (setq rules (remove-if #'(lambda (x) (intersection qpreds (preds (maksand (body x))) 
						       :test #'param-equal)) rules))
    ; grab signed predicates occurring in the heads
    (setq spreds (mapunion #'(lambda (x) (list (signed-relation (head x)))) rules :test #'equal))
    ; index rules
    (setq rules (define-theory (make-instance 'metheory) "" rules))
    ; construct an impl sentence for each signed predicate
    (dolist (sp spreds result)
      (setq rs (indexps sp rules))
      (setq vars (head-args (length (args (head (first rs))))))
      (setq rs (mapcar #'(lambda (r) (similarize-head r vars)) rs))
      (push (list '<=> 
		  (makimpl (head (first rs)) vars) 
		  (equantify-except (maksor (mapcar #'(lambda (imp) (maksand (body imp))) rs))
				    vars))
	    result))))

(defun add-uva (th struct)
  (let ((n nil))
    ; add unique value constraints as required
    (dolist (r (webform-widgets struct))
      (when (and (widget-name r) (widget-unique r)) ; (not (eq (widget-style r) 'checkbox)))  ???
	(push `(<= (= ?x ?y) (,(widget-name r) ?x) (,(widget-name r) ?y)) n)))
    (nconc n th)))

(defun ws-sym2short (th webform)
  (nsublis (mapcar #'(lambda (x) (cons (value-symbol x) (value-shortname x))) 
		   (webform-univ webform))
	   (contents th)
	   :test #'eq))

(defun create-type-data (preds)
  (mapcan #'(lambda (p) (if (listp (pred-univ p))
			    (mapcar #'(lambda (val) 
					(list (pred-name p) (value-symbol val)))
				    (pred-univ p))
			    nil))
	  preds))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;; Lisp-like code constructors ;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun mak-class (class &rest args) (list* 'make-class (quotify class) args))
(defun maks-class (class args) (list* 'make-class (quotify class) args))
(defun mak-collection (class &rest args) (list* 'make-class (quotify class) args))
(defun maks-collection (class args) (list* 'make-class (quotify class) args))
(defun mak-for (var exp body) (list* 'dolist `(,var ,exp) (proglist body)))
(defun mak-collectioniter (var exp body) (list* 'with-collection-iterator `(,var ,exp) (proglist body)))
(defun mak-orderedcollectioniter (var exp body) (list* 'with-orderedcollection-iterator `(,var ,exp) (proglist body)))
(defun mak-vardecl (vars body) (if vars (list* 'let vars (proglist body)) body))
(defun mak-vardecl* (vars) 
  (maks-block (mapcar #'(lambda (x) (if (atom x) (list 'defvar x) (cons 'defvar x))) vars)))
(defun mak-return (value &optional (fname nil)) `(normreturn ,fname ,value))
(defun mak-when (con val) (list* 'when con (proglist val)))
(defun mak-if (con then else) (list 'if con then else))
(defun mak-while (exp val) (list* 'while exp (proglist val)))
(defun mak-neg (exp) (list 'not exp))
(defun maks-and (l) (maksand l))
(defun mak-funcall (fn &rest args) (list* 'funcall `(function ,fn) args))
(defun mak-apply (fn &rest args) (apply #'mak-funcall fn (apply #'list* args)))
(defun mak-function (name args body) (list* 'defun name args (proglist body)))
(defun mak-anonfunc (args body) (list 'function (list* 'lambda args (proglist body))))
(defun mak-funcname (f) (list 'functionname f))
(defun mak-funcref (f) (list 'function f))
(defun mak-declare (thing) (list 'declare thing))
(defun mak-ignore (thing) (list 'ignore thing))
(defun maks-ignore (things) (cons 'ignore things))
(defun mak-objproperty (obj prop) (list 'objproperty obj prop))
(defun mak-method (obj funcall) (list 'method obj funcall))
(defun mak-eq (e1 e2) (list 'eq e1 e2))
(defun mak-quote (e) (quotify e))
(defun mak-assign (var exp) (list 'setq var exp))
(defun mak-member (seq thing) (list 'collection-member seq thing))
(defun mak-element (thing key) (list 'collection-element thing key))
(defun mak-block (&rest ps) (maks-block ps))
(defun maks-block (l) (if (cdr l) (cons 'progn l) (car l)))
(defun is-block (p) (and (listp p) (eq (car p) 'progn)))
(defun unmak-block (p) (cdr p))
(defun mak-cellvalue () 'cellvalue)
(defun mak-cellvalues () 'cellvalues)
(defun mak-hascellvalue () 'hascellvalue)
(defun mak-univ () 'univ)
(defun mak-arrayassign (arr ind val) (list 'array-assign arr ind val))
(defun proglist (body) 
  (if (not body) body (if (is-block body) (flatten-block body) (list body))))
(defun mak-bool (v) (if v (mak-true) (mak-false)))
(defun mak-true () ''true)
(defun mak-false () ''false)
(defun mak-undefined () ''undefined)

(defun flatten-block (p)
  (cond ((is-block p) (mapcan #'flatten-block (unmak-block p)))
	(t (list p))))

;(defun maks-progn (forms) (if (cdr forms) (cons 'progn forms) (car forms)))
;(defun newlispvar () (gentemp "v"))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Websheet Lisp-like code support ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun construct-websheet-code-support (datalog struct)
  (let (comp pretty ugly init negps posps globalvars extern submitprep)
    (setq globalvars (construct-websheet-code-globals))
    (setq comp (construct-websheet-code-components2 (datalog-components datalog)))
    ;(setq types (construct-websheet-code-types2 struct))
    (setq pretty (construct-websheet-code-prettynames struct))
    (setq ugly (construct-websheet-code-uglynames struct))
    (setq negps (mapcar #'(lambda (x) (signed-relation (impl-sent (head x)))) 
			(datalog-intensional datalog)))
    (multiple-value-setq (negps posps) (split #'negative-literalp negps))
    (setq negps (mapcar #'drop-not negps))
    (setq init (construct-websheet-code-init2 struct negps posps (datalog-components datalog)))
    (setq extern (construct-websheet-code-externalinit))
    (setq submitprep (construct-websheet-code-submitprep struct))
    (list globalvars comp pretty ugly init extern submitprep)))

(defun construct-websheet-code-externalinit () (mak-function 'external_init nil nil))
(defun construct-websheet-code-globals () (mak-block (mak-vardecl* `(univ (useshortnames ,*ws-use-value-shortnames*)))))
(defun construct-websheet-code-init2 (struct negdefs posdefs comp)
  (let (hasneg haspos mycomp body tmp)
    (dolist (o (webform-options struct))
      (push `(setq ,(first o) ,(second o)) body))
    (push `(setq valuecount ,(length (webform-univ struct))) body)
    (push '(funcall init_index) body)
    (push '(setq univ (dictionary-get datastore 'univ)) body)
    (push (mak-vardecl* (list (list 'mycellarray (mak-class 'array)))) body)
    (setq tmp 0)
    (dolist (r (webform-widgets struct))
      (when (widget-name r)
	(setq hasneg (member (widget-name r) negdefs))
	(setq haspos (member (widget-name r) posdefs))
	(setq mycomp (car (find (widget-name r) comp :test #'(lambda (x y) (member x y)))))
	(if mycomp 
	    (setq mycomp (tosymbol (list "component_" (websheet-cellname mycomp))))
	    (setq mycomp (construct-component (list (websheet-cellname (widget-name r))))))
	(push (mak-arrayassign 'mycellarray tmp (construct-websheet-code-init2-widget r hasneg haspos mycomp))
	      body)
	(setq tmp (1+ tmp))))
    (push (mak-funcall 'initspread 'mycellarray) body)
    (mak-function 'init nil (maks-block (nreverse body)))))

(defun construct-websheet-code-init2-widget (w hasneg haspos compname)
  (mak-class 
   'cell
   (tostring (websheet-cellname (widget-name w))) 
   (if (member (widget-typename w) *infinitetypes*) 
       (mak-quote (widget-typename w))
       `(funcall dictionary-get datastore ,(quotify (widget-typename w))))
   (mak-quote (widget-style w))
   (if hasneg (mak-queryver (widget-name w) 'neg 's) 'undefined) 
   (if haspos (mak-queryver (widget-name w) 'pos 's) 'undefined) 
   (if hasneg (mak-queryver (widget-name w) 'neg 'supps) 'undefined)
   (if haspos (mak-queryver (widget-name w) 'pos 'supps) 'undefined)
   (if hasneg (mak-queryver (widget-name w) 'neg 'suppx) 'undefined)
   (if haspos (mak-queryver (widget-name w) 'pos 'suppx) 'undefined)
   compname
   (mak-bool (widget-incundef w))
   (if (widget-init w)
       (maks-collection 'set (mapcar #'value-shortname (cdr (widget-init w))))
       'undefined)
   (mak-bool (widget-unique w))))

(defun construct-websheet-code-submitprep (struct)
  (let (body)
    (dolist (w (webform-widgets struct))
      (push (mak-when (maknot (mak-method 
			       (mak-objproperty (mak-funcall 'findcell (mak-quote (widget-name w))) 
						'conflictset)
			       (mak-funcall 'empty)))
		      (mak-method 's (mak-funcall 'adjoin (mak-quote (widget-name w))))) body))
    (push (mak-when (maknot (mak-method 's (mak-funcall 'empty)))
		    (mak-block (mak-funcall 'alert "Errors remain.  You must fix them before submitting.")
			       (mak-return (mak-bool nil) 'submitprep))) body)
    (push (mak-funcall 'clear_forced_gui) body)
    (push (mak-return (mak-bool t) 'submitprep) body)
    (mak-function 'submitprep nil 
		  (mak-vardecl (list (list 's (mak-collection 'set))) (maks-block (nreverse body))))))

; returns a single statement intended for global scope
(defun construct-websheet-code-components2 (comps)
    (mak-vardecl* 
     (mapcar #'(lambda (x) (list (tosymbol (list "component_" (car x))) 
				 (construct-component (mapcar #'quotify x)))) comps)))

; returns a single statement intended for global scope
(defun construct-websheet-code-types2 (struct)
  ; write out one type: var type_p = new Array(val1,val2,...,valn)
  (mak-vardecl*
   (mapcar #'(lambda (w) (list (pred-name w)
			       (construct-type (mapcar #'quotify
						       (mapcar #'tosymbol
							       (mapcar #'value-shortname
								       (pred-univ w)))))))
	   (remove-if-not #'(lambda (x) (listp (pred-univ x)))
			  (webform-preds struct)))))
  
; returns a single, global statment
(defun construct-websheet-code-prettynames (struct)
  (let ((body nil))
    (push (mak-arrayassign 'prettynames 'undefined "") body)  ; useful for filling textboxes with undefined value
    (dolist (p (webform-univ struct))
      (push (mak-arrayassign 
	     'prettynames 
	     (if (stringp (value-shortname p)) (value-shortname p) (mak-quote (value-shortname p)))
	     (if (stringp (value-prettyname p)) (value-prettyname p) (mak-quote (value-prettyname p))))
	    body))
    (mak-vardecl (list (list 'prettynames (mak-class 'hash))) (maks-block (nreverse body)))))

; returns a single, global statement
(defun construct-websheet-code-uglynames (struct)
  (let ((body nil))
    ;(push (mak-arrayassign 'uglynames "" 'undefined) body)
    (dolist (p (webform-univ struct))
      (push (mak-arrayassign 
	     'uglynames
	     (if (stringp (value-prettyname p)) (value-prettyname p) (mak-quote (value-prettyname p)))
	     (if (stringp (value-shortname p)) (value-shortname p) (mak-quote (value-shortname p))))
	    body))
    (mak-vardecl (list (list 'uglynames (mak-class 'hash))) (maks-block (nreverse body)))))

; helper routines
(defun construct-component (elems) (maks-collection 'set elems))
(defun construct-type (elems) (maks-collection 'hashbag elems))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Datalog to Lisp Compiler ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun compile-ext (th preds)
  "(COMPILE-EXT TH PREDS) takes a datalog theory TH and a set of webform PRED objects. 
   Assumes each IDB predicate is defined in terms of EDBs and widgets.
   Outputs code that implements all signed IDB predicates."
  (let (data code extpreds typehash)
    ; force theory to satisfy compiler's assumptions
    (setq th (mapcar #'strip-impl (mapcar #'force-nice-vars (contents th))))
    (setq extpreds (remove-if-not #'(lambda (x) (eq (pred-kind x) :extensional)) preds))
    ; create hash-table for widget types (used in conjunct ordering)
    (setq typehash (compile-ext-typehash preds))
    ; compile data and complex sentences differently
    (multiple-value-setq (data th) (split #'atomicp th))
    (setq code (mapcan #'(lambda (x) (compile-ext-p x preds typehash)) th))
    (setq code (nconc (mapcar #'def-enum extpreds) code))
    (setq code (nconc (mapcar #'def-check extpreds) code))
    (push (def-init data) code)
    (nconc (def-globals) code)))

(defun compile-ext-typehash (preds)
  "(COMPILE-EXT-TYPEHASH PREDS) create hash of types for preds.  infinite types are left out."
  (let (typehash)
    (setq typehash (make-hash-table))
    (dolist (w (remove-if-not #'(lambda (x) (eq (pred-kind x) :widget)) preds))
      ;(format t "(find ~A in ~S)~%" (widget-typename (pred-display w)) preds)
      (unless (or (member (widget-typename (pred-display w)) *infinitetypes*)
		  (= (parameter-arity (pred-parameter w)) 0))
		  ;(and (setq tmp (find (widget-typename (pred-display w)) preds 
			;		      :key #'pred-name))
		       ;(member (parameter-univ (pred-parameter tmp)) *infinitetypes*)))
	(setf (gethash (pred-name w) typehash)
	      (cons (list (pred-name w) '?x) 
		    (list (widget-typename (pred-display w)) '?x)))))
    typehash))

(defun strip-impl (p)
  (cond ((atom p) p)
	((implp p)
	 (cond ((atomicp p) (second p))
	       ((member (first p) '(and or not => <= <=>)) 
		(list* (first p) (impl-sent (second p)) (cddr p)))
	       ((member (first p) '(forall exists))
		(list (first p) (second p) (impl-sent (third p)) (fourth p)))
	       (t p)))
	(t p)))

(defun force-nice-vars (p)
  "(FORCE-NICE-VARS P) takes the sentence P and ensures that every variable name can be
   devariabled to produce a valid LISP variable.  We should really make sure there
   are no accidental variable captures, but instead we use a gensym-like approach."
  (cond ((atom p) (if (varp p) (variable (tolispvar (devariable p))) p))
	(t (cons (car p) (mapcar #'force-nice-vars (cdr p))))))

(defun mak-query (sym sign) (tosymbol (list sign "_" sym)))
(defun mak-queryver (sym sign suff) (tosymbol (list sign suff "_" sym)))
(defun mak-queryadorned (sym sign adorn) 
  (tosymbol (list (mak-query sym sign) "_" adorn)))
(defun mak-enum (pred) (tosymbol (list 'enum "_" pred)))
(defun mak-check (pred) (tosymbol (list 'check "_" pred)))

(defun compile-ext-p (p preds typehash)
  (let ((res nil) (sr (signed-relation (head p))) pred sign)
    (setq pred (find (relation sr) preds 
		     :test #'(lambda (x y) (and (eq x (pred-name y)) (isrelation (pred-parameter y))))))
    (if (negative-literalp sr) (setq sign 'neg) (setq sign 'pos))
    (push (construct-x pred sign) res)
    (push (construct-s pred sign) res)
    (push (construct-suppx pred sign) res)
    (push (construct-supps pred sign) res)
    (push (construct-query pred sign) res)
    ; infinite types only support bound queries
    ;(if (member (relation (cdr (gethash r typehash))) *infinitetypes*)
	;(push (compile-ext-p-adorn p (args (head p)) preds typehash) res)
    (dolist (s (subsets (args (head p))) (nreverse res))
      (push (compile-ext-p-adorn p s preds typehash) res))))

(defun construct-x (pred sign) 
  (construct-wrap pred sign 'x (mak-collection 'pair (mak-true) 'newval) '(newval)))
(defun construct-s (pred sign) 
  (construct-wrap pred sign 's 
		  (mak-collection 'pair (mak-false) 
				  `(collection-adjoin sofar newval ,(mak-funcref 'collection-equal)))
		  
		  '(sofar newval)))
(defun construct-suppx (pred sign) 
  (construct-wrap pred sign 'suppx 
		  (mak-collection 'pair (mak-true) (mak-collection 'pair 'newval 'support))
		  '(newval support)))
(defun construct-supps (pred sign) 
  (construct-wrap pred sign 'supps 
		  (mak-collection 'pair 
				  (mak-false) 
				  `(collection-adjoin sofar ,(mak-collection 'pair 'newval 'support) 
						      ,(mak-funcref 'collection-equal)))
		  '(newval support sofar)))

(defun construct-wrap (pred sign suffix returnform returnfromvars)
  (let ((args (mapcar #'devariable (head-args (parameter-arity (pred-parameter pred))))) unusedvars)
    (setq unusedvars (set-difference '(newval support sofar) returnfromvars))
    (mak-function (mak-queryver (pred-name pred) sign suffix) 
		  args 
		  (mak-return (mak-apply (mak-query (pred-name pred) sign)
					 (mak-anonfunc '(newval support sofar) 
						       (mak-block 
							(mak-declare (maks-ignore unusedvars))
							(mak-return returnform)))
					 args)))))

(defun construct-query (pred sign)
  (let ((args (mapcar #'devariable (head-args (parameter-arity (pred-parameter pred)))))
	(fname (mak-query (pred-name pred) sign)))
    (mak-function fname
		  (cons 'onsuccess args)
		  (construct-adornswitch args pred sign nil args fname))))

(defun construct-adornswitch (argsleft pred sign sofar args fname)
  (let (tmp)
    (cond ((null argsleft)
	   (setq tmp (mak-queryadorned (pred-name pred) sign (reverse sofar)))
	   (mak-if (mak-funcall 'fboundp (mak-funcname tmp))
		   (mak-return (mak-apply tmp 'onsuccess args) fname)
		   (mak-return (mak-undefined) fname)))
	  (t
	   (mak-if (mak-funcall 'varp (car argsleft))
		   (construct-adornswitch (cdr argsleft) pred sign (cons 'f sofar) args fname)
		   (construct-adornswitch (cdr argsleft) pred sign (cons 'b sofar) args fname))))))

(defun compile-ext-p-adorn (p bound preds typehash)
  "(COMPILE-EXT-P-ADORN P BOUND PREDS TYPEHASH) takes a sentence P of the form 
   (<=> [not ](p ?x1 ... ?xn) (exists blah (or (and blah) blah blah))), where each conjunction is
   a list of literals, and a list of variables bound in the head.
   Returns a function that finds a single tuple of variables true of p.  BOUND is the set of
   variables that are assumed to be bound on entry."
  (let (fname vars newvars disjs)
    (setq vars (vars (second p)))
    (setq newvars (maptimes #'(lambda () (tosymbol (gentemp "?nsh"))) (length vars)))
    (setq fname (construct-funcname (relation (second p)) (negative-literalp (head p)) bound vars))
    (setq disjs (ws-datalog-ordering p bound preds typehash (mak-univ)))
    (setq *tmp3* disjs)
    (setq disjs (group-by disjs #'(lambda (x) (find-widget-preds x preds)) :test #'equal))
    
    (mak-function  
     fname
     (cons 'onsuccess (mapcar #'(lambda (x) (devariable x)) newvars))
     (mak-vardecl 
      (nconc (list (list 'sofar (mak-collection 'set)) 'tmp) 
	     (mapcar #'(lambda (x y) (list (devariable x) (devariable y))) vars newvars))
      (maks-block (nconc (mapcar #'(lambda (x) 
				     (compile-ext-p-adorn-group x vars newvars fname bound preds))
				 disjs)
			 (list (mak-return 'sofar fname))))))))

(defun include-in-list (lit list)
  (if (and (listp list) (listp (car list)))
      (cons lit list)
      (list lit list)))

; Leave in case need later, but we're dealing with the infinite types of textfields differently.
;   We're using the active domain on the spreadsheet.
(defun handle-inftypes (list inftypes)
  "(HANDLE-INFTYPES LIST INFTYPES) removes all positive safety literals that are infinite, and 
   turns negative literals with infinite types into positive literals.  Sounds strange, but
   if a safety literal is added with an infinite type, it is because the variables in 
   the negative literal with that type did not belong to any finite-domain literal.  Since
   there are infinitely many values for that literal (and all other such literals), the
   existential query being asked by that collection of literals is always true.  Normally
   we would just drop all such literals, but in the webform setting where we want 
   to compute proofs to understand which cells were responsible for causing conflicts,
   we need to retain the fact that those negative literals were part of this query.
   If we make them positive (a) we preserve the predicates occurring in the query and
   (b) we know the query will only be evaluated when those predicates have values.
   Only handle monadic predicates with infinite types."
  (let (inflits vs res vars)
    (setq inflits (remove-if-not #'(lambda (x) (and (positive-literalp x) (member (relation x) inftypes)))
				 list))
    (setq vars (mapunion #'vars inflits))
    (setq res nil)
    (dolist (l list)
      (cond ((and (positive-literalp l) (member (relation l) inftypes)))
	    ((and (negative-literalp l) (not (cdr (setq vs (vars l)))) (member (first vs) vars))
	     (push (list (relation l) (newindvar)) res))
	    (t (push l res))))
    (nreverse res)))


(defun construct-funcname (symb negp bound vars)
  (let (charfunc)
    (setq charfunc nil)
    (dolist (v vars)
      (if (member v bound) (push 'b charfunc) (push 'f charfunc)))
    (setq charfunc (nreverse charfunc))
    (if negp
	(mak-queryadorned symb 'neg charfunc)
	(mak-queryadorned symb 'pos charfunc))))

(defun compile-ext-p-adorn-group (disjgroup vars newvars fname bound preds)
  (let (body)
    (setq body (maks-block (mapcar #'(lambda (y)
				       (nconc
					(compile-ext-and vars y fname bound preds (car disjgroup))
					(mapcar #'(lambda (y z) `(setq ,(devariable y) ,(devariable z))) 
						vars newvars)))
				    (cdr disjgroup))))
    (if (car disjgroup)
	(mak-when (construct-widget-def-guard (car disjgroup)) body)
	body)))

(defun tolispvar (symb)
  (cond ((numberp symb) (tosymbol (list "tlh" symb)))
	(t symb)))

(defun find-widget-preds (p preds)
  "(FIND-WIDGET-PREDS) P PREDS) Given a sentence P and a list of pred objects, finds
   those preds occurring in P that are of kind :widget.  Returns list of sorted
   predicate names."
  (let (widgets)
    (setq widgets (mapcarnot #'(lambda (x) (find x preds :test #'(lambda (y z) (param-equal y (pred-parameter z))))) 
			     (preds p)))
    (setq widgets (delete-if-not #'(lambda (x) (eq (pred-kind x) :widget)) widgets))
    (sort (mapcar #'pred-name widgets) #'string< :key #'tostring)))

(defun compile-ext-and (thing p fname bound preds predgroup)
  "(compile-ext-AND P) takes a conjunction of literals P and the variables
   for which we want return values VARS.  Constructs code that finds
   a single value for VARS, assuming all literals in P are extensionally
   defined."
  ; make p a list of literals.
  (if (listp p) 
      (if (eq (car p) 'and) (setq p (cdr p)) (setq p (list p)))
      (setq p (list p)))    
  ; build initial binding list
  (setq bound (nreverse (cons truth (mapcar #'(lambda (x) 
						(cons x (devariable x))) 
					    bound))))
  ; build loops
  (compile-ext-and-aux thing p bound fname preds predgroup))


(defun compile-ext-and-aux (inputvars lits binding fname preds predgroup)
  ;(declare (notinline compile-ext-and-aux))
  (cond ((null lits)      
	 (maks-block `((setq tmp (funcall onsuccess 
					  ,(maks-collection 'expr (plug inputvars binding))
					  ,(maks-collection 'set (mapcar #'quotify predgroup))
					  sofar))
		       (if (eq (collection-first tmp) ,(mak-true))
			   (return-from ,fname (collection-second tmp))
			   (setq sofar (collection-second tmp))))))
	(t
	 (let (lit vs its newbdg rec letbdg pred litparam)
	   (setq lit (car lits))
	   (setq lit (quote-lit-args lit #'(lambda (x) (not (varp x)))))
	   (setq lit (plug lit binding))
	   (setq vs (vars lit))			    
	   (setq its (mapcar #'devariable vs))
	   (setq newbdg (nconc (mapcar #'cons vs its) binding))
	   (setq lit (plug lit newbdg))
	   (setq letbdg (mapcar #'(lambda (x y) 
				    (list x (quotify y))) its vs))
	   (setq letbdg (remove-if #'(lambda (x) 
				       (member (second (second x)) inputvars)) 
				   letbdg))
	   (setq rec (compile-ext-and-aux inputvars
					  (cdr lits)
					  newbdg
					  fname
					  preds
					  predgroup))
	   (setq litparam (pred lit))
	   (setq pred (find-if #'(lambda (x) (param-equal litparam (pred-parameter x))) preds))
	   (assert pred nil (format nil "Unknown predicate: ~A." (parameter-symbol litparam)))

	   ; built-ins and negative literals must be lookups, except for assignment
	   (when (or (eq (pred-kind pred) :builtin) (negative-literalp lit))
	     (let ((bindingok (if (and (positive-literalp lit) (eq (pred-name pred) *ws-assign*))
				  (equal vs (list (second (car lits))))
				  (null vs))))
	       (assert bindingok nil (format nil "Bad conjunct ordering for lit ~A with vars ~A" lit vs))))

	   ; positive widgets and extensions may be either lookups or constructions
	   ; unique-valued widget enumerations have a special form
	   (cond ((null vs)   ; no variables: just a lookup
		  (cond ((eq (pred-kind pred) :widget)
			 (if (pred-unique pred)
			     (setq rec (mak-when (construct-widgettest lit) rec))  
			     (setq rec (mak-when (construct-widgettests lit) rec))))
			((eq (pred-kind pred) :builtin)
			 (setq rec (mak-when (construct-builtin-test lit) rec)))
			(t
			 (setq rec (mak-when (construct-test lit) rec)))))
		 ((eq (pred-kind pred) :widget)  ; widget enumeration
		  (if (pred-unique pred)
		      (setq rec (construct-widgetset (car its) lit rec))
		      (setq rec (construct-collectioniter its lit rec t))))
		 ((eq (pred-name pred) *ws-assign*)
		  (setq rec (mak-block (construct-assignment lit) rec)))
		 (t (setq rec (construct-collectioniter its lit rec))))
	   (mak-vardecl letbdg rec)))))

(defun construct-collectioniter (vars atom body &optional (widget nil))
  "(CONSTRUCT-COLLECTIONITER VARS ATOM BODY WIDGET) creates lisp code
   that iterates the variable list VARS over the datalog atom ATOM
   and executes BODY for each iteration.  WIDGET controls whether or not
   ATOM refers to a widget (thus requiring a different function-call to 
   lookup the values)." 
  (let ((v (tosymbol (gentemp "arh"))) (assign nil) (i 0) init)
    (if widget
	(setq init (construct-widgetenum atom))
	(setq init (construct-enum atom))) 
    (dolist (a vars)
      (if widget
	  (push (mak-assign a (mak-element v 'key)) assign)  ; so that widget data is stored as simple set
	  (push (mak-assign a (mak-element (mak-element v 'key) i)) assign))
      (setq i (1+ i)))
    
    (mak-vardecl (list (list v init)) 
		 (mak-collectioniter 'key v (apply #'mak-block (nconc (nreverse assign)
								      (list body)))))))
 
(defun quote-lit-args (lit test) 
  (cond ((atom lit) (if (funcall test lit) (quotify lit) lit))
	((eq (car lit) 'not) (maknot (quote-lit-args (second lit) test)))
	(t (cons (car lit) (mapcar #'(lambda (x) (quote-lit-args x test)) 
				    (cdr lit))))))

(defun construct-enum (atom) 
  (cons (mak-enum (car atom)) (cdr atom)))
(defun construct-widgetenum (atom) 
  (list (mak-cellvalues) (mak-quote (relation atom))))
(defun construct-test (lit) 
  (cond ((negative-literalp lit) (mak-neg (construct-test (second lit))))
	(t (cons (mak-check (car lit)) (cdr lit)))))
(defun construct-widgettest (lit)
  (cond ((negative-literalp lit) (mak-neg (construct-widgettest (second lit))))
	(t (if (and (listp lit) (cdr lit))   ; cell with an argument
	       (mak-eq (list (mak-cellvalue) (mak-quote (first lit))) (second lit))
	       (list (mak-cellvalue) (mak-quote (relation lit)))))))
(defun construct-widgettests (lit)
  (cond ((negative-literalp lit) (mak-neg (construct-widgettests (second lit))))
	(t (mak-member (list (mak-cellvalues) (mak-quote (car lit)))
		       (cadr lit)))))
(defun construct-widgetset (var atom body)
  (mak-block (mak-assign var (mak-funcall (mak-cellvalue) 
					  (mak-quote (relation atom))))
	     body))

; because all built-in functions appear in an equality statement, e.g. (= ?x (* ?z ?w)), they are always compiled into
;    the Lisp atom (eq ?x (* ?z ?w)).  This works as long as all of the functions are built-ins.  (If we wanted to allow
;    the user to define his own functions, we would want to be able to use those functions to generate as well as validate,
;    which we can't do under this scheme.)

(defun construct-assignment (lit)
  (assert (positive-literalp lit) nil (format nil "Assignment lit cannot be negative: ~A" lit))
  (assert (listp lit) nil (format nil "Assignment lit must be a list: ~A" lit))
  (cons 'setq (mapcar #'construct-builtin-eval (cdr lit))))

(defun construct-builtin-eval (term)
  term)

(defun construct-builtin-test (lit)
  (cond ((negative-literalp lit) (mak-neg (construct-builtin-test (second lit))))
	((and (listp lit) (eq (car lit) '=)) (cons 'eq (mapcar #'construct-builtin-eval (cdr lit))))
	(t lit)))
#|
(defmethod construct-builtin-test-atom (atom (type (eql '=))) (cons 'eq (cdr atom)))
(defmethod construct-builtin-test-atom (atom (type (eql '+r))) (mak-eq (cons '+ (butlast atom)) (first (last atom))))
(defmethod construct-builtin-test-atom (atom (type (eql '-r))) (mak-eq (cons '- (butlast atom)) (first (last atom))))
(defmethod construct-builtin-test-atom (atom (type (eql '*r))) (mak-eq (cons '* (butlast atom)) (first (last atom))))
(defmethod construct-builtin-test-atom (atom (type (eql '/r))) (mak-eq (cons '/ (butlast atom)) (first (last atom))))
(defmethod construct-builtin-test-atom (atom type) (declare (ignore type)) lit)
|#
#|
(defun construct-for (vars valuesexp body) 
  (let ((assign nil) (i 0))
    (dolist (v vars)
      (push `(setq ,v (ds.element tablepart ,i)) assign)
      (setq i (1+ i)))
    ; should make enum return array, and then instead of nth use aref
    (mak-for 'tablepart valuesexp (apply #'mak-block (nconc (nreverse assign)
							    (list body))))))
|#


(defun construct-widget-def-guard (predsymbs)
  "(CONSTRUCT-WIDGET-DEF-GUARD PREDSYMBS) takes a list of predicate symbols and
   constructs a boolean expression true exactly when all the PREDSYMBS are
   defined."
  (maks-and (mapcar #'(lambda (x) (mak-funcall (mak-hascellvalue) (mak-quote x))) 
			   predsymbs)))

(defun def-enum (predicate)
  (let (pred arity)
    (setq pred (parameter-symbol (pred-parameter predicate)))
    (setq arity (parameter-arity (pred-parameter predicate)))
    (let ((args (head-args arity)))
      (cond ((= arity 0) `(defun ,(mak-enum pred) ,args  
			    (return-from ,(mak-enum pred) ,(mak-collection 'tuple))))
	    ((= arity 1) `(defun ,(mak-enum pred) ,args 
			    ,(mak-declare (maks-ignore args))
			    (return-from ,(mak-enum pred) (dictionary-get datastore (quote ,pred)))))
	    (t   ; the below looks seriously broken, but it is only useful if we're using n-ary preds to enum
	     `(defun ,(mak-enum pred) ,args 
		(let (m res (allres ,(mak-collection 'tuple)) i (ex ,(maks-collection 'expr args)))
		  (with-collection-iterator (d (dictionary-get datastore (quote ,pred)))
		    (setq m (expr-match ex (collection-element d)))
		    (when (not (eq m ,(mak-false))) 
		      (setq res ,(mak-class 'array))
		      (setq i 0)
		      (with-orderedcollection-iterator (e ,(maks-collection 'tuple args))
       			(if (varp e)
			    (array-assign res i (get m e))
			    (array-assign res i e)))		    
		      (setq allres (append allres (collection-toExpr res)))))
		  (return-from ,(mak-enum pred) allres))))))))

(defun def-check (predicate)
  (let (pred arity)
    (setq pred (parameter-symbol (pred-parameter predicate)))
    (setq arity (parameter-arity (pred-parameter predicate)))
    (let ((args (head-args arity)))
      `(defun ,(mak-check pred) ,args 
	 (return-from ,(mak-check pred)
	   (collection-member (dictionary-get datastore (quote ,pred))
			      ,(maks-collection 'expr args)
			      ,(mak-funcref 'collection-equal)))))))

(defun def-globals ()
  (list 
   `(defvar datastore)))

; datastore keeps for each predicate a set of expressions
(defun def-init (data)
  (let (u code)
    (multiple-value-setq (u data) (split #'(lambda (x) (eq (relation x) 'univ)) data))
    (push `(setq datastore ,(mak-collection 'dictionary)) code)
    (setq code (nconc code (def-hashbags u)))
    (setq code (nconc code (def-sets data)))
    (mak-function 'init_index nil (maks-block code))))

(defun def-hashbags (data) (def-collection data 'hashbag))
(defun def-sets (data) (def-collection data 'set))
(defun def-collection (data type)
  (let (code tables)
    (setq tables (group-by data #'car))
    (dolist (c tables)
      (push `(dictionary-set datastore (quote ,(car c)) 
		     ,(maks-collection type 
			     (mapcar #'(lambda (x) 
					 (maks-collection 'expr (mapcar #'quotify (cdr x)))) 
				     (nreverse (cdr c)))))
	    code))
    code))
 
#|
(deftheory p
    (<= (goal ?x) (or (and (p ?x) (r ?x ?y a) (not (s ?y)))
		      (and (p ?x) (v ?x ?y b) (not (s ?y)))
		      (and (t ?x) (r ?x ?y a) (not (s ?y)))))
  ;(<= (goal2 ?x ?y) (and (p ?x) (r ?x ?y a) (not (s ?y))))
  (p a)
  (p b)
  (r a b a)
  (r a c a)
  (r b b a)
  (r b c a)
  (s b))

; for debugging: create predicates for  
(defun setwidgets (th widgetlist uniquelist)
  (declare (ignore uniquelist))
  (let (res preds tmp)
    (setq preds (preds (maksand (contents th))))
    (setq th (split #'(lambda (x) (not (datap x))) (contents th)))
    (dolist (v preds)
      (push (make-pred :parameter v 
		       :kind (if (member (parameter-symbol v) widgetlist)
				:widget :extensional))
;		       :unique (if (find (parameter-symbol v) uniquelist)
;				   t nil))
	    res))
    (dolist (p th (nreverse res))
      (setq tmp (find (relation (head p)) res 
		      :key #'(lambda (x) (parameter-symbol 
					  (pred-parameter x)))))
      (setf (pred-kind tmp) :intensional))))

; (compile-ext 'p (setwidgets 'p '(p s t) '(p)))
; (defun cellvalue (p) (second (car (mydata p))))
; (defun cellvalues (p) (mapcar #'second (mydata p)))

  
|#  

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;; Lisp to Javascript Translator ;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; we're only translating a well-behaved portion of (a portion of) Lisp
;  Most notably, the only Javascript 'return's we insert (which must exist in every
;  javascript function) are those generated by 'return-from' statements.
;  Also, all variables within a function are assumed distinct, (so let environments
;  are collapsed).
; 
; tojavascript outputs javascript to the given stream.  Each function leaves the
;   last line without a CR and does not insert a CR before the first line.
;   Only block-like structures indent intermediate lines; all others make no indentation.  

; forms that combine basic operations and other combinations
(defmethod tojavascript (form (type (eql 'defun)) depth s)
  (js-indent depth s) 
  (format s "function ~A (" (js-funcname (second form)))
  (tojavascript (third form) 'arglist depth s)
  (format s ") { ")
  (when (cdddr form)
    (setq form (maks-block (cdddr form)))
    (when (js-multilinep form) (format s "~%") (js-indent (1+ depth) s)) 
    (tojavascript form (signifier form) (1+ depth) s))
  (format s " }"))

; ignore any declarations
(defmethod tojavascript (form (type (eql 'declare)) depth s)
  (declare (ignore form depth s)))

(defmethod tojavascript (form (type (eql 'let)) depth s)
  (dolist (va (second form))
    (if (atom va) 
	(tojavascript (list 'defvar va) 'defvar depth s)
	(tojavascript (cons 'defvar va) 'defvar depth s))
    (format s "~%")
    (js-indent depth s))
  (setq form (maks-block (cddr form)))
  (tojavascript form (signifier form) depth s))

(defmethod tojavascript (form (type (eql 'progn)) depth s)
  (tojavascript (second form) (signifier (second form)) depth s)
  (dolist (l (cddr form))
    (format s "~%")
    (js-indent depth s)
    (tojavascript l (signifier l) depth s)))
#|
(defmethod tojavascript (form (type (eql 'dolist)) depth s)
  (let (v newform)
    (setq v (first (second form)))
    (setq newform `(do ((,v ,(second (second form)) (ds.cdr ,v)))
		       ((ds.empty ,v))
		     ,(maptree #'(lambda (x) (if (eq x v) `(ds.first ,v) x)) 
			       (maks-block (cddr form)))))
    (tojavascript newform 'do depth s)))

(defmethod tojavascript (form (type (eql 'do)) depth s)
  ; translate to progn block with while loop then convert to javascript
  (setq form
	(maks-block (nconc (tojs-doinit (second form))
			   (list (mak-while (mak-neg (first (third form))) 
					    (maks-block (nconc (cdddr form)
							       (tojs-doinc (second form)))))))))
  (tojavascript form 'progn depth s))

(defun tojs-doinit (varlist)
  (let ((decls nil))
    (dolist (v varlist (nreverse decls))
      (push (list 'defvar (first v) (second v)) decls))))

(defun tojs-doinc (varlist)
  (let ((incs nil))
    (dolist (v varlist (nreverse incs))
      (push (list 'setq (first v) (third v)) incs))))

(defmethod tojavascript (form (type (eql 'while)) depth s)
  (format s "while (")
  (tojavascript (second form) (signifier (second form)) -1 s)
  (format s ") ")
  (setq form (maks-block (cddr form)))
  (when (js-multilinep form) (format s "{~%") (js-indent (1+ depth) s))
  (tojavascript form (signifier form) (1+ depth) s)
  (when (js-multilinep form) (format s "}")))
|#

(defmethod tojavascript (form (type (eql 'with-collection-iterator)) depth s)
  (format s "for (var ")
  (tojavascript (first (second form)) (signifier (first (second form))) -1 s)
  (format s " in ds.data(")
  (tojavascript (second (second form)) (signifier (second (second form))) -1 s)
  (format s ")) ")
  (setq form (maks-block (cddr form)))
  (when (js-multilinep form) (format s "{~%") (js-indent (1+ depth) s))
  (tojavascript form (signifier form) (1+ depth) s)
  (when (js-multilinep form) (format s "}")))

(defmethod tojavascript (form (type (eql 'with-orderedcollection-iterator)) depth s)
  (format s "for (var ")
  (tojavascript (first (second form)) (signifier (first (second form))) -1 s)
  (format s "=0; i< ds.data(")
  (tojavascript (second (second form)) (signifier (second (second form))) -1 s)
  (format s ").length(); ")
  (tojavascript (first (second form)) (signifier (first (second form))) -1 s)
  (format s "++) ")
  (setq form (maks-block (cddr form)))
  (when (js-multilinep form) (format s "{~%") (js-indent (1+ depth) s))
  (tojavascript form (signifier form) (1+ depth) s)
  (when (js-multilinep form) (format s "}")))

(defmethod tojavascript (form (type (eql 'if)) depth s)
  (format s "if (")
  (tojavascript (second form) (signifier (second form)) -1 s)
  (format s ") ")
  (when (js-multilinep (third form)) (format s "{~%") (js-indent (1+ depth) s))
  (tojavascript (third form) (signifier (third form)) (1+ depth) s)
  (when (js-multilinep (third form)) (format s "}"))
  (when (fourth form)
    (format s "~%")
    (js-indent depth s)
    (format s "else ")
    (when (js-multilinep (fourth form)) (format s "{~%") (js-indent (1+ depth) s))
    (tojavascript (fourth form) (signifier (fourth form)) (1+ depth) s)
    (when (js-multilinep (fourth form)) (format s "}"))))

(defmethod tojavascript (form (type (eql 'when)) depth s)
  (tojavascript `(if ,(second form) ,(cons 'progn (cddr form))) 'if depth s))

(defmethod tojavascript (form (type (eql 'lambda)) depth s)
  (format s "function (")
  (tojavascript (second form) 'arglist depth s)
  (format s ") { ")
  (setq form (maks-block (cddr form)))
  (when (js-multilinep form) (format s "~%") (js-indent (1+ depth) s))
  (tojavascript form (signifier form) (1+ depth) s)
  (format s " }"))

(defun js-multilinep (form) (find (car form) '(do dolist if when let progn)))
 
;;; basic operations
(defmethod tojavascript (form (type (eql 'defvar)) depth s)
  ;(js-indent depth s)
  (format s "var ~A" (js-varname (second form)))
  (when (= (length form) 3)  ; notice can't use (third form) in case the last arg is NIL 
    (format s " = ")
    (tojavascript (third form) (signifier (third form)) -1 s))
  (when (> depth -1) (format s ";")))

(defmethod tojavascript (form (type (eql 'return)) depth s)
  ;(js-indent depth s)
  (format s "return ")
  (tojavascript (second form) (signifier (second form)) -1 s)
  (when (> depth -1) (format s ";")))

(defmethod tojavascript (form (type (eql 'setq)) depth s) 
  ;(js-indent depth s)
  (format s "~A = " (js-varname (second form)))
  (tojavascript (third form) (signifier (third form)) -1 s)
  (when (> depth -1) (format s ";")))

(defmethod tojavascript (form (type (eql 'funcall)) depth s) 
  ; assume the second argument to funcall is (function name) or just name, not #'name
  ;(js-indent depth s)
  (setq form (js-rewrite-funcall (if (eq (relation form) 'funcall) (cdr form) form)))
  (tojavascript form (signifier form) depth s))

(defmethod tojavascript (form (type (eql 'objproperty)) depth s) 
  (tojavascript (second form) (signifier (second form)) depth s)
  (format s ".")
  (tojavascript (third form) (signifier (third form)) depth s))

(defmethod tojavascript (form (type (eql 'method)) depth s) 
  (tojavascript (second form) (signifier (second form)) depth s)
  (format s ".")
  (tojavascript (third form) (signifier (third form)) depth s))

(defmethod tojavascript (form (type (eql 'make-instance)) depth s) 
  (tojavascript (cons 'make-class (cdr form)) 'make-class depth s))

(defmethod tojavascript (form (type (eql 'make-class)) depth s) 
  ;(js-indent depth s)
  (format s "new ")
  (cond ((eq (unquote (second form)) 'hash)
	 (tojavascript (cons 'funcall (cons "Object" (cddr form))) 'funcall -1 s))
	((eq (unquote (second form)) 'array)
	 (tojavascript (cons 'funcall (cons "Array" (cddr form))) 'funcall -1 s))
	(t
	 (tojavascript (list* 'funcall (unquote (second form)) (cddr form)) 'funcall -1 s)))
  (when (> depth -1) (format s ";")))

(defmethod tojavascript (form (type (eql 'return-from)) depth s)
  (tojavascript (list 'return (third form)) 'return depth s))

(defmethod tojavascript (form (type (eql 'normreturn)) depth s)
  (tojavascript (list 'return (third form)) 'return depth s))

(defmethod tojavascript (form (type (eql 'array-assign)) depth s)
  (declare (ignore depth))
  (format s "~A[" (js-varname (second form)))
  (tojavascript (third form) (signifier (third form)) -1 s)
  (format s "] = ")
  (tojavascript (fourth form) (signifier (fourth form)) -1 s)
  (format s ";"))

;;;; boolean operations 
(defmethod tojavascript (form (type (eql 'bool)) depth s)
  (declare (ignore depth))
  (cond ((atom form) (tojavascript (list 'eq form 'true) 'eq -1 s))
	((member (car form) '(and or not)) (tojavascript form (car form) -1 s))
	(t (tojavascript (list 'eq form 'true) 'eq -1 s))))

(defmethod tojavascript (form (type (eql 'and)) depth s)
  (tojavascript-infixop "&&" (cdr form) depth s))

(defmethod tojavascript (form (type (eql 'or)) depth s)
  (tojavascript-infixop "||" (cdr form) depth s))

(defun tojavascript-infixop (op args depth s)
  (declare (ignore depth))
  (format s "(")
  (tojavascript (first args) (signifier (first args)) -1 s)
  (dolist (f (cdr args))
    (format s " ~A " op)
    (tojavascript f (signifier f) -1 s))
  (format s ")"))

(defmethod tojavascript (form (type (eql 'not)) depth s)
  (declare (ignore depth))
  (format s "!(")
  (tojavascript (second form) (signifier (second form)) -1 s)
  (format s ")"))

;;;;; infix built-ins

(defmethod tojavascript (form (type (eql 'eq)) depth s) (tojavascript-infixop "===" (cdr form) depth s))

(defmethod tojavascript (form (type (eql 'lt)) depth s) (tojavascript-infixop "<" (cdr form) depth s))
(defmethod tojavascript (form (type (eql 'lte)) depth s) (tojavascript-infixop "<=" (cdr form) depth s))
(defmethod tojavascript (form (type (eql 'gt)) depth s) (tojavascript-infixop ">" (cdr form) depth s))
(defmethod tojavascript (form (type (eql 'gte)) depth s) (tojavascript-infixop ">=" (cdr form) depth s))

(defmethod tojavascript (form (type (eql '+)) depth s) (tojavascript-infixop '+ (cdr form) depth s))
(defmethod tojavascript (form (type (eql '*)) depth s) (tojavascript-infixop '* (cdr form) depth s))
(defmethod tojavascript (form (type (eql '-)) depth s) (tojavascript-infixop '- (cdr form) depth s))
(defmethod tojavascript (form (type (eql '/)) depth s) (tojavascript-infixop '/ (cdr form) depth s))
(defmethod tojavascript (form (type (eql '%)) depth s) (tojavascript-infixop '% (cdr form) depth s))

(defmethod tojavascript (form (type (eql 'in)) depth s) 
  (declare (ignore depth)) 
  (tojavascript (second form) (signifier (second form)) -1 s)
  (format s ".match(")
  (tojavascript (third form) (signifier (third form)) -1 s)
  (format s ")"))

 
;;;;; low-level 
(defmethod tojavascript (form (type (eql 'arglist)) depth s) 
  (declare (ignore depth))
  (unless (null form)
    (tojavascript (car form) (signifier (car form)) -1 s)
    (dolist (i (cdr form))
      (format s ", ")
      (tojavascript i (signifier i) -1 s))))

#|
(defmethod tojavascript (form (type (eql 'mak-array)) depth s)
  (declare (ignore depth))
  (format s "[")
  (tojavascript (cdr form) 'arglist -1 s)
  (format s "]"))
|#

(defmethod tojavascript (form (type (eql 'quote)) depth s)
  (setq form (js-rewrite-quoted form))
  (cond ((atom form) (tojavascript form form depth s))
	((not (eq (first form) 'quote)) (tojavascript form (signifier form) depth s))
	(t
	 (if (atom (second form))
	     (format s "\"~A\"" (js-thing (second form)))
	     (tojavascript (cons 'list (mapcar #'quotify (second form))) 'list -1 s)))))

; (function x) is #'x in Lisp, but (functionname x) is 'x.  In JS, both translated to x.
(defmethod tojavascript (form (type (eql 'function)) depth s)
  (setq form (js-funcname-spec (second form)))
  (if (stringp form)
      (format s "~A" form)
      (tojavascript form (signifier form) depth s)))

(defmethod tojavascript (form (type (eql 'functionname)) depth s)
  (setq form (js-funcname-spec (second form)))
  (if (stringp form)
      (format s "~A" form)
      (tojavascript form (signifier form) depth s)))

(defmethod tojavascript (form type depth s)
  (declare (ignore type))
  (cond ((stringp form) (format s "'~A'" (js-thing form)))
	((atom form) (format s "~A" (js-thing form)))
	(t 
	 (if (listp (first form))
	     (tojavascript (first form) (signifier (first form)) depth s)
	     (format s "~A" (js-funcname (first form))))
	 (format s "(")
	 (tojavascript (cdr form) 'arglist depth s)
	 (format s ")")
	 (when (> depth -1) (format s ";")))))

;;;;; helper functions
(defun tolower (s) (format nil "~(~A~)" s))
(defun js-indent (depth s) (indent depth s (lambda (x) (declare (ignore x)) " ")))
; only applied to atoms
(defun js-funcname (f) 
  (let (fnew)
    (setq f (js-funcname-spec f))
    (cond ((stringp f) f)   ; lookup returned string
	  ((setq fnew (find (tostring f) *ws-php-builtin-prefixed-names* :test #'equalp)) fnew) ; a php built-in: returning proper spelling.
	  (t (tolower (substitute #\_ #\? (substitute #\_ #\- (tostring f))))))))  ; a program function: translate from Lisp to JS syntax
#| New
  (let (fnew fs)
    (setq fs (tostring f))
    (cond ((setq fnew (js-funcname-spec f)) fnew)
	  ((setq fnew (find fs *ws-php-builtin-names* :test #'equalp)) ; php builtin -- need to prefix with phpnamespace
	   (tostring *ws-php-namespace* "." fnew))
	  (t f))))
|#
#| Old
	   (if (stringp fnew)  ; if string leave alone; otherwise, massage
	       fnew
	       (tolower (substitute #\_ #\? (substitute #\_ #\- (tostring fnew))))))
|#

(defun js-funcname-spec (f)
  (case f
    (member "ds.member")
    (collection-member "ds.member")
    (dictionary-get "ds.get")
    (dictionary-set "ds.set")
    (collection-adjoin "ds.adjoin")
    (collection-element "ds.element")
    (collection-first "ds.first")
    (collection-second "ds.second")
    (collection-equal "equalp")
    (tostr "String")
    (tonum "Number")
    (tobool "Boolean")
    (otherwise f)))

(defun js-argname (v) (js-thing v))
(defun js-varname (v) (js-thing v))
(defun js-thing (thing) 
  (cond ((not thing) "false")
	((eq thing 't) "true")
	((varp thing) (js-thing (devariable thing)))
	((stringp thing) thing)
	(t (tolower (tostring thing)))))

(defun js-rewrite-funcall (form)
  (cond ((atom form) form)
	(t
	 (let ((f (first form)))
	   (when (and (listp f) (eq (first f) 'function)) (setq f (second f)))
	   (case f
	     (fboundp (mak-neg (mak-eq (second form) 'undefined)))
	     (otherwise form))))))

(defun js-rewrite-quoted (form)
  (cond ((atom form) form)
	((equal form ''true) 'true)
	((equal form ''false) 'false)
	((equal form ''undefined) 'undefined)
	(t form)))

;;; Testing

(defun tojavascripts (th)
  (dolist (l (contents th))
    (tojavascript l (signifier l) 0 t)))

(defun test-tojavascript (list)
  (let ((resp nil))
    (dolist (l list t)
      (pprint l)
      (format t "~%~%")
      (tojavascript l (signifier l) 0 t)
      (format t "~%")
      (setq resp (read))
      (when (eq resp 'q) (return-from test-tojavascript 'quit)))))

; (test-tojavascript (compile-ext 'p (setwidgets 'p '(p s t) '(p))))


(defun remove-tojs-method (keyword)
  (remove-method #'tojavascript (find-method #'tojavascript nil `(t (eql ,keyword) t t))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Server-side Demo ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; For now, we don't store anything to disk.  We just keep data in main memory.
;   later, we can add logging.
(defvar *webform-db* nil
  "A hash of repositories: one for each collaborative webform.")
(defstruct webformdb name data htmlfile lispfile widgets (committime 0) package) 
(define-condition server-not-installed (error) ((comment :initarg :comment)))

(defun ws-server-register-form (formdb)
  "(WS-server-register-form FORMDB) register webformdb FORMDB so that the server
    keeps track of users' submissions."
  ; ensure our main datastructure is a hash table
  (unless (hash-table-p *webform-db*) (setq *webform-db* (make-hash-table)))
  ; register this form
  (setf (gethash (webformdb-name formdb) *webform-db*) formdb)
  ; install the server-side code
  (ws-server-install-code (read-file (filify (webformdb-lispfile formdb))) 
			  (webformdb-package formdb)))

(defmethod process (s (file (eql 'commitform)) postlines)
  (let (db data checkouttime)
    (setq db (gethash (read-user-string (getf-post "formname" postlines)) *webform-db*))
    (setq data (process-commitform-readdata db postlines))
    (setq checkouttime (read-user-string (getf-post "time" postlines)))
    (cond ((not db) (html-websheet-problem s "Web form not found."))
	  ((not data) (output-initedform s db))
	  ((not checkouttime) (output-initedform s db :msg "No errors found.  Submission successful."))
	  (t (process-commitform s db data checkouttime)))))

(defun process-commitform-readdata (db postlines)
  "(PROCESS-COMMITFORM-READDDATA DB POSTLINES) reads all the widget data out of
   POSTLINES and returns as a set of KIF atoms."
  (let (data)
    (dolist (w (webformdb-widgets db))
      (setq data (nconc (mapcarnot #'(lambda (x) (ws-make-widget-value w x))
				(getallvals (websheet-cellname w) postlines)) data)))
    data))

(defun ws-make-widget-value (widget value)
  (setq value (read-user-string value))
  (if (eq value 'undefined) nil (list widget value)))

(defun process-commitform (s db data checkouttime)
  "(PROCESS-COMMITFORM S DB DATA) given new data DATA and webformdb DB, find errors
  and require user to fix them, or commit union of current and new data."
  ; if newer data available, need to consider union
  (when (not (find-package (webformdb-package db))) (error 'server-not-installed))
  (when (< checkouttime (webformdb-committime db))
    (setq data (union data (contents (webformdb-data db)) :test #'equal)))
  (ws-server-install-data data (webformdb-package db))
  (cond ((ws-server-errorp (mapcar #'parameter-symbol (preds (maksand data))) (webformdb-package db))
	 (output-initedform s db :data data :msg "Conflict detected.  Please fix before re-submission."))
	(t 
	 (setf (webformdb-data db) data)
	 (setf (webformdb-committime db) (get-universal-time))
	 (output-initedform s db :msg "Commit successful"))))

(defun output-initedform (s db &key (data nil) (msg nil))
  "(OUTPUT-INITEDFORM S DB DATA) outputs the form for webformdb DB and initializes
   the values to DATA if it is non-NIL or to the values stored in DB."
  ; once client sends prettynames for data, need to invoke uglyname on each element we're outputting.
  (let (text functext idx values)
    (unless data (setq data (contents (webformdb-data db))))
    (setq functext "function external_init () {  }")
    (setq text (read-any-file (filify (webformdb-htmlfile db))))
    (setq idx (search functext text))
    (princ (subseq text 0 idx) s)
    (format s "function external_init() {~%")
    (dolist (w (webformdb-widgets db))
      (setq values (viewfinds `?x `(,w ?x) data))
      (format s "  findcell('~A').externalInit(new set(" (websheet-cellname w))
      (tojavascript values 'arglist 0 s) 
      (format s "));~%"))
    (format s "  document.getElementById('time').value = ~A;~%" (get-universal-time))
    (format s "  var stat = document.getElementById('status');~%")
    (cond (msg
	   (format s "  if (stat !== undefined) {~%")
	   (format s "    stat.innerHTML = '~A';~%" msg)
	   (format s "    stat.style.visibility = 'visible'; }~%"))
	  (t
	   (format s "  if (stat !== undefined) ~%")
	   (format s "    stat.style.visibility = 'hidden';~%"))) 
    (format s "}~%~%")
    (princ (subseq text (+ idx (length functext))) s)))

(defun ws-server-install-code (code package)
  (let (oldp filename)
    ;(ignore-errors (eval `(delete-package ,package)))
    (setq filename "/tmp/wsservercodeinstall.lisp")
    (setq oldp (package-name *package*))
    (with-open-file (f filename :direction :output 
		       :if-does-not-exist :create :if-exists :supersede)
      (format f "(unless (find-package ~S) (make-package ~S))~%" package package)
      (format f "(in-package ~S)~%" package)
      (format f "(use-package :spreadsheetserver)~%")
      (dolist (c code)
	(format f "~S~%" c))
      (format f "(init_index)~%")
      (format f "(in-package ~S)~%" oldp))
    (load filename)
    (delete-file filename)))

(defun ws-server-install-data (data package) 
  "(WS-SERVER-INSTALL-DATA DATA PACKAGE) is coupled with WS-SERVER-INSTALL-CODE.
   DATA is assumed to be a set of ground atoms where all terms are strings or numbers.
   Executes code to install DATA as the current state of the webform in PACKAGE.
   Assumes WS-SERVER-INSTALL-CODE has already been called on PACKAGE."
  (let (set oldp filename)
    (setq oldp (package-name *package*))
    (setq filename "/tmp/wsserverdatainstall.lisp")
    (with-open-file (f filename :direction :output :if-does-not-exist :create :if-exists :supersede)
      (format f "(in-package ~S)~%" package)
      (format f "(setq *cellvalues* (make-hash-table))~%")
      (dolist (g (group-by data #'relation))
	(setq set (mapcar #'(lambda (x) (cdr x)) (cdr g)))  ; drop off relation
	(setq set (list* 'make-class ''set (mapcar #'car set)))
	(format f "(setf (gethash '~A *cellvalues*) ~S)~%" (relation (first g)) set))
      (format f "(in-package ~S)~%" oldp))
    (load filename)
    (delete-file filename)))

; need to deal with packages correctly
(defun ws-server-find1s (widget sign package)
  "(WS-SERVER-FIND1S WIDGET SIGN PACKAGE) finds all implications of length 1 or more 
   for the WIDGET of the appropriate SIGN where the associated code is located in PACKAGE."
  ; when crossing packages, we need to deal with symbols moving across packages.  
  ;   (a) the function name, while constructed in CL-USER must be translated to the same symbol in PACKAGE
  ;   (b) the results of the function call are strings/numbers, and so they need no translation.
  (let (f res)
    (setq f (format nil "~A" (mak-queryver widget sign 's)))
    (setq res (eval-in-package `(if (fboundp (read-from-string ,f))
				    (funcall (read-from-string "collection-tolistr") 
					     (funcall (read-from-string ,f) (read-from-string "?x")))
				    nil)
			       package))
    (mapcar #'(lambda (x) (if (eq sign 'pos) (cons widget x) (maknot (cons widget x)))) res)))

(defun ws-server-data (widget package)
  "(WS-SERVER-FIND0S WIDGET PACKAGE) finds returns the data stored for WIDGET in PACKAGE.  Note that
   the widget is by definitin false for all data not in the resulting list."
  (let (r)
    (setq r (eval-in-package `(funcall (read-from-string "collection-tolistr")
				       (funcall (read-from-string "cellvalues") 
						(read-from-string ,(tostring widget))))
			     package))
    (mapcar #'(lambda (x) (list widget x)) r)))

(defun eval-in-package (form package)
  (let ((oldp (package-name *package*)))
    (eval `(progn
	     (in-package ,package)
	     (prog1 ,form 
	       (in-package ,oldp))))))

(defun ws-server-errorp (widgetnames package)
  "(WS-SERVER-ERRORP WIDGETNAMES PACKAGE) check if there is an error for any of WIDGETNAMES
   for the web form constraints installed in PACKAGE."
  (let (pos1 neg1 data pos neg both)
    (dolist (p widgetnames nil)
      ; grab data and implications
      (setq data (ws-server-data p package))
      (setq pos1  (ws-server-find1s p 'pos package))
      (setq neg1 (mapcar #'maknot (ws-server-find1s p 'neg package)))
      ; find all positively implied that ARE NOT selected (via set-difference)
      (setq pos (set-difference pos1 data :test #'equal))
      ; find all negatively implied that ARE selected (via intersection)
      (setq neg (intersection neg1 data :test #'equal))
      ; find all both negatively and positively implied
      (setq both (intersection pos1 neg1 :test #'equal))
      ;(format t "~&widget ~A. data:~A, pos:~A, neg:~A~%" p data pos1 neg1)
      (when (or pos neg both) (return t)))))

; untested
#|
(defun ws-find-datalog-compiler-errors (datalog intcode &optional (datum nil))
  (let ((package ':wsdatalogcompilertest) th datath dresults cresults errors widgets)
    ; throw intcode into package that uses spreadsheetserver and check for errors
    (ws-server-install-code intcode package)
    ; throw datalog into prologtheory
    (setq th (define-theory (make-instance 'prologtheory) "" 
	       (append (datalog-extensional datalog)
		       (mapcar #'ws-datalog-rule-to-prolog (datalog-intensional datalog)))))
    (setq widgets (mapcarnot #'(lambda (x) (if (and (listp x) (eq (car x) '<=)) (relation x) nil)) th))
    (setq widgets (remove-duplicates widgets))
    
    ; generate websheet data data1,...,datan, if not supplied
    (when (not datum) (setq datum (ws-datalog-compiler-test-data datalog)))

    (setq datath (define-theory (make-instance 'prologtheory) "" nil))
    (includes datath th)
    (dolist (d datum)
      ; add datai to datalog and compute negs and poss for each predicate
      (empty datath)
      (definemore datath d)
      (setq dresults (mapcan #'(lambda (x) (viewfinds `(,x ?y) `(,x ?y) datath)) widgets))
      (setq dresults (nconc dresults (mapcan #'(lambda (x) (viewfinds `(not (,x ?y)) `(not (,x ?y)) datath)) 
					     widgets)))
      ; add datai to intcode and compute negs and poss for each predicate
      (ws-server-install-data d package)
      (setq cresults (mapcan #'(lambda (x) (ws-server-finds x 'neg package)) widgets))
      (setq cresults (nconc cresults (mapcan #'(lambda (x) (ws-server-finds x 'pos package)) widgets)))

      ; if datalog and intcode differ, accummulate error
      (when (not (seteq cresults dresults))
	(push (format nil "Error on data ~%~A~%   datalog returned ~%~A~%   lisp returned ~%~A~%" d dresults cresults)
	      errors)))
    errors))

; need to implement
(defun ws-datalog-compiler-test-data (datalog) 
  (declare (ignore datalog)) (break "Unimplemented"))
|#

(defun ws-datalog-rule-to-prolog (r)
  (cond ((atom r) r)
	((eq (car r) '<=>)
	 (list* '<= (impl-sent (second r)) (to-orless-list (drop-exists (third r)))))
	(t r)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;; Solve spreadsheet constraints via ASP ;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod process (s (file (eql 'spreadsheetsolve)) postlines)
  (cond ((getf-post "constraints" postlines)
         (process-spreadsheet-solve s postlines))
        (t (http-problem s "Bad request."))))

(defun process-spreadsheet-solve (s postlines)
  (format-html s) (crlf s)
  (format-head s)
  (format s "<title>Spreadsheet Solution</title>~%")
  (format s "<link rel=\"stylesheet\" type=\"text/css\" href=\"/docserver/infoserver/examples/researchmaster/style/main.css\">~%")
  (format s "<link rel=\"stylesheet\" type=\"text/css\" href=\"/docserver/infoserver/examples/researchmaster/style/jack.css\">~%")
  ;(format s "~A" (showhidescript))
  (finish-head s) (crlf s)
  (format-body s *bgcolor*) (crlf s)
  (output-header s)
  (format s "<center><h2>Solution</h2></center>~%")
  (output-spreadsheet-solve s postlines)
  (finish-body s) (crlf s)
  (finish-html s) (crlf s))

(defun output-spreadsheet-solve (s postlines)
  (let (axioms model)
    (setq axioms (read-sentences (getf-post "constraints" postlines)))
    (setq model (rundlv axioms))
    (format s "<div style=\"margin-left: 10px; border: 1px solid blue; width: 90%\">~%")
    (dolist (m model)
      (format s "~A<br>~%" m))
    (format s "</div>")))

(defun rundlv (axioms)
  (let (file 
	mystring 
	(*datalognot* '-) 
	(*real-ops* (cons 'naf *real-ops*)))
    (setq file "/Users/thinrich/Apps/dlv/tlh2")
    (with-open-file (f file :direction :output :if-does-not-exist
		       :create :if-exists :supersede)
      (print-datalog (prep-axioms-for-dlv axioms) f))
    (setq mystring (make-array '(0) :element-type 'base-char
                             :fill-pointer 0 :adjustable t))
    (with-output-to-string (m mystring)
      (print "would run here"))
      ;(run-program "/Users/thinrich/Apps/dlv/dlv" (list file) :output m))
    (parse-dlv-result mystring)))

(defun parse-dlv-result (s)
  (let (model tmp)
    (setq s (substitute #\Space #\Newline s))
    (multiple-value-setq (tmp model) (stringmatches ".*{(.*)}.*" s))
    (setq model (delete-if #'(lambda (x) (equal x ""))
			   (split-on model '(", "))))
    (setq model (mapcar #'(lambda (x) (substitute #\~ #\- x)) model))
    (setq model (mapcar #'readlogic model))
    (values model tmp)))

(defun prep-axioms-for-dlv (axioms)
  ;(assert (every #'quantifier-free-sentencep axioms) nil "Only takes quantifier-free sentences.")
  (let (biconds rest bs v ext)
    (setq ext (factfinds '?x '(extensional ?x) axioms))
    (setq axioms (delete-if #'(lambda (x) (and (listp x) (eq (car x) 'extensional))) axioms))
    (setq v (get-vocabulary (maksand axioms)))
    (multiple-value-setq (biconds rest) (split #'bicondp axioms))
    (setq biconds (mapcar #'orient-bicond biconds))
    (setq bs (nconc ext (mapcar #'relation (mapcar #'second biconds))))
    (setq biconds (mapcan #'bicond-to-dlv biconds))
    (setq rest (mapcan #'(lambda (x) (sentence-to-dlv x bs)) rest))
    (setq ext (mapcar #'(lambda (x) (negative-dlv-closure x v)) ext))
    (setq axioms (list* `(<= (e ?x ?x) (univ ?x))
			`(<= (not (e ?x ?y)) (univ ?x) (univ ?y) (naf (e ?x ?y)))
			(mapcar #'repl= (nconc biconds ext rest))))
    (nconc (mapcar #'(lambda (x) (maksafe-dlv x bs)) axioms) 
	   (mapcar #'(lambda (x) `(univ ,x)) (compute-dca axioms)))))

(defun negative-dlv-closure (reln vocab)
  (let (head)
    (setq vocab (find reln vocab :key #'parameter-symbol))
    (setq head (cons reln (maptimes #'newindvar (parameter-arity vocab))))
    (list* '<= (maknot head) (maknaf head) (mapcar #'(lambda (x) `(univ ,x)) (cdr head)))))
  
(defun repl= (p)
  (cond ((atom p) p)
	((eq (car p) '<=) (cons '<= (mapcar #'repl= (cdr p))))
	((eq (car p) 'not) (maknot (repl= (second p))))
	((eq (car p) 'naf) (maknaf (repl= (second p))))
	((eq (car p) '=) (cons 'e (cdr p)))
	(t p)))

(defun maksafe-dlv (p relns)
  (cond ((atom p) p)
	((and (eq (car p) '<=) (null (cddr p))) p)
	((eq (car p) '<=)
	 (if (set-difference (vars (second p)) (vars (cddr p)))
	     (let (newp)   ; unsafe
	       (setq newp (maksafe-dlv-quick p relns))
	       (if newp newp (maksafe p)))
	     p))
	(t p)))

(defun maksafe-dlv-quick (p relns)
  "If there is a non-naf, non-relns literal in the body whose variables are a subset of
   the head, then swap the two."
  (do ((ps (cddr p) (cdr ps))
       (found nil) (vs (vars (second p)))
       (newp nil))
      ((or (null ps) found) (if (not found) nil newp))
    (cond ((and (not (and (listp (car ps)) (eq (caar ps) 'naf))) 
		(not (member (relation (car ps)) relns))
		(subsetp (vars (car ps)) vs))
	   (setq newp (list* '<= 
			     (maknot (car ps)) 
			     (nconc (nreverse (cons (maknot (second p)) newp))
				    (cdr ps))))
	   (setq found t))
	  (t (setq newp (cons (car ps) newp))))))


(defun sentence-to-dlv (p baserelns)
  ; move the first non-basereln to the head
  (mapcar #'(lambda (x) (contrapositive-wo-in-head x baserelns)) (brfs p)))

(defun contrapositive-wo-in-head (p relns)
  (cond ((atom p) p)
	((and (eq (car p) '<=) (null (cddr p))) p)
	((and (eq (car p) '<=) (not (member (relation p) relns))) p)
	((eq (car p) '<=)
	 (do ((ps (cddr p) (cdr ps))
	      (found nil)
	      (newp nil))
	     ((or (null ps) found) (if (not found) p newp))
	   (cond ((not (member (relation (car ps)) relns))
		  (setq newp (list* '<= 
				    (maknot (car ps)) 
				    (nconc (nreverse (cons (maknot (second p)) newp))
					   (cdr ps))))
		  (setq found t))
		 (t (setq newp (cons (car ps) newp))))))
	(t p)))

(defun bicond-to-dlv (p)
  (let (newps)
    (setq newps (brfs `(<= ,(second p) ,(third p))))
    (cons (list* '<= (maknot (second p)) (maknaf (second p)) 
		 (mapcar #'(lambda (x) `(univ ,x)) (vars (second p))))
	  newps)))

(defun maknaf (p) (list `naf p))

(defun orient-bicond (p)
  (if (and (atomicp (second p)) (nonrepeating-atom (second p)))
      p
      (list '<=> (third p) (second p))))

(defun bicondp (p)
  (and (listp p) 
       (eq (car p) '<=>)
       (or (and (atomicp (second p)) 
		(nonrepeating-atom (second p)))
	   (and (atomicp (third p))
		(nonrepeating-atom (third p))))))

(defun nonrepeating-atom (l)
  (cond ((atom l) t)
	(t (and (every #'varp (cdr l))
		(= (length (cdr l)) (length (remove-duplicates (cdr l))))))))

