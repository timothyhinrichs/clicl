
; logging, so that changes are saved
(defparameter *log* nil)

(defun loadlog (fn th)
  (with-open-file (f fn :direction :input)
    (do ((a (read f nil) (read f nil)))
	((null a) 'done)
        (cond ((atom a) (insert a th))
              ((eq (car a) 'pos) (insert (cadr a) th))
              ((eq (car a) 'neg) (drop (cadr a) th))
              (t (insert a th))))))


(defun dump-repository-data (repository manager dir &optional (lang 'ookif))
  (unless (char= #\/ (elt dir (1- (length dir))))
    (setq dir (concatenate 'string dir "/")))
  (let (classes pred)
    (setq classes (viewfinds '?x '(isa ?x class) manager))
    (dolist (c classes)
      (setq pred (viewfindx '?x `(predicate ,c ?x) manager))
      (when (and (viewfindp `(,pred ?x) repository) (nonview-sourcep pred repository))   ;viewfindp `(extension repository ,pred) manager))
        (dumpclass c repository (concatenate 'string dir (stringify c)) lang)))))

(defun diff-theories (th1 th2 &optional (test #'samep))
  "(DIFF-THEORIES TH1 TH2 TEST) returns any statements in TH1 not in TH2 and vice
   versa according to TEST."
  (let ((results nil))
    (dolist (p (contents th1))
      (when (not (member p (contents th2) :test test))
        (setq results (cons p results))))
    (setq results (cons nil results))
    (dolist (p (contents th2))
      (when (not (member p (contents th1) :test test))
        (setq results (cons p results))))
    (nreverse results)))

(defun loaddata-uniquify (fn target)
  (loadfile-uniquify fn target 'kif 'no))

(defun loadfile-uniquify (fn target lang meta)
  (dolist (sent (read-file-contents fn lang))
    (cond ((atom sent) (save sent target))
          ((find (car sent) '(not unprovable)) (drop (cadr sent) target))
          (t (save sent target))))
  (when (eq meta 'yes) (introspect target))
  'done)


(defun mapdir (function dir)
  "(MAPDIR FUNCTION DIR) applies FUNCTION to every file in DIR.  Returns result."
  (mapcar function (directory dir)))


(defmethod output-fastcreate-cell (s object class slot value (style (eql 'axioms)))
  (declare (ignore object class))
  (format-textarea s slot (stringize value) 4 40))

(defmethod output-fastchange-cell (s slot value (style (eql 'axioms)))
  (format-textarea s slot (stringize value) 4 40))

(defun output-value-in-style (s value style)
  (when (listp value) (setq value (car value)))
  (case style
    (imagestyle (format s "<img src='~A'/>" value))
    (urlstyle (format s "<a href='~A'><font color='red'>~A</font></A>" value value))
    (emailstyle (format s "<a href='mailto:~A'><font color='red'>~A</font></A>" value value))
    (htmlstyle (format s value))
    (dollarstyle (format s "$~$" value))
    (prettystyle (output-simple s value))
    (axioms (mapcar #'(lambda (x) (kif2html x s)) (read-sentences value)))
    (t (output-handle s value))))

(defun output-handle-in-style (s value style)
  (cond ((eq style 'imagestyle) (format s "<img src='~A'/>" value))
        ((eq style 'urlstyle) (format s "<a href='~A'><font color='red'>~A</font></A>" value value))
        ((eq style 'emailstyle) (format s "<a href='mailto:~A'><font color='red'>~A</font></A>" value value))
        ((eq style 'htmlstyle) (format s value))
        ((eq style 'dollarstyle) (format s "$~$" value))
        ((eq style 'prettystyle) (output-simple s value))
        ((eq style 'axioms) (mapcar #'(lambda (x) (kif2html x s)) (read-sentences value)))
        ((simplep value) (output-simple s value))
        (t (output-anchor s value))))


; overloaded to create a new axioms type
#| Old infomaster

(defun output-change-cell (s style slot value)
  (cond ((eq style 'multichoicelist) (output-change-menu s slot value))
        ((eq style 'menu) (output-change-menu s slot value))
        ((eq style 'dropdownlist) (output-change-selector s slot value))
        ((eq style 'selector) (output-change-selector s slot value))
        ((eq style 'hierarchicalselector) (output-change-multiselector s slot value))
        ((eq style 'checkbox) (output-change-checkbox s slot value))
        ((eq style 'radiobutton) (output-change-radiobutton s slot value))
        ((eq style 'datestyle) (output-change-datestyle s slot value))
        ((eq style 'subframe) (output-change-subframe s slot value))
        ((eq style 'interval) (output-change-typein s slot value))
        ((eq style 'textarea) (output-change-textarea s slot value))
        ((eq style 'stringfield) (output-change-text s slot value))
        ((eq style 'text) (output-change-text s slot value))
        ((eq style 'filestyle) (output-change-filestyle s slot value))
        ((eq style 'urlstyle) (output-change-text s slot value))
        ((eq style 'emailstyle) (output-change-text s slot value))
        ((eq style 'password) (output-change-password s slot value))
        ((eq style 'glyph) (output-change-glyph s slot value))
        ((eq style 'axioms) (output-change-axioms s slot value))
        (t (output-change-typein s slot value))))

(defun output-change-axioms (s slot value)
  (when (eq value 'unknown) (setq value ""))
  (format-hidden s "Start" (stringize *cells*))
  (format-hidden s "Style" "Axioms")
  (format-hidden s "Slot" (stringize slot))
  (format-textarea s (stringize *cells*) (htmlify value) 8 60)
  (format-hidden s "End" ""))

(defun output-value-in-style (s value style)
  (when (listp value) (setq value (car value)))
  (case style
    (imagestyle (format s "<IMG SRC=\"~A\"/>" value value))
    (urlstyle (format s "<A HREF=\"~A\">~A</A>" value value))
    (emailstyle (format s "<A HREF=\"mailto:~A\">~A</A>" value value))
    (htmlstyle (format s value))
    (axioms (mapcar #'(lambda (x) (kif2html x s)) (read-sentences value)))
    (t (output-value s value))))

(defun parsevalue (postlines)
  (let ((type (cdadr postlines)))
    (cond ((string= type "Menu") (parsemenu postlines))
          ((string= type "Selector") (parsetypein postlines))
          ((string= type "Multiselector") (parsemultiselector postlines))
          ((string= type "Checkbox") (parsemenu postlines))
          ((string= type "Radiobutton") (parsemenu postlines))
          ((string= type "Interval") (parseinterval postlines))
          ((string= type "Intermenu") (parseother postlines))
          ((string= type "Datestyle") (parsedatestyle postlines))
          ((string= type "Stringfield") (parsestringfield postlines))
          ((string= type "Text") (parsetext postlines))
          ((string= type "Textarea") (parsetext postlines))
          ((string= type "Password") (parsetext postlines))
          ((string= type "Typein") (parsetypein postlines))
          ((string= type "Glyph") (parsetypein postlines))
          ((string= type "Subframe") (parsesubframe postlines))
          ((string= type "Axioms") (parsetext postlines))
          (t (parseother postlines)))))

|#
#|
(defun parseaxioms (postlines)
  (let (slot value)
    (pop postlines)
    (pop postlines)
    (setq slot (read-value-string (cdr (pop postlines))))
    (setq value (kwote (maksand (read-sentences (cdr (pop postlines))))))
    (pop postlines)
    (cond ((string= value "") (setq value (list slot)))
          (t (setq value (list slot value))))
    (values value postlines)))
|#


#|
; overloaded to properly deal with latml front page
(defun find-frontpagetype (interface)
  (findx '?x `(frontpagetype ,interface ?x) *interface*))

(defun process-toplevel (s)
  (let (page type)
    (setq page (find-frontpage *gui*))
    (setq type (find-frontpagetype *gui*))
    (cond ((not page) (process s 'topdoor nil))
          ((eq type 'latml) (output-prolog s 200) (princ (latml2html page)))
          (t (output-prolog s 200) (princ page s)))))
|#

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; View Matrix
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod process (s (file (eql 'displayMatrix)) postlines)
  (declare (ignore postlines))
  (process-displaymatrix s))

(defun process-displaymatrix (s)
  (let (examples techniques)
    (setq examples (prorequest `(ask-all ?x ,(makpred '?x 'example *gui*))))
    (setq techniques (prorequest `(ask-all ?x ,(makpred '?x 'technique *gui*))))

    (setq examples (sort examples #'lessp))
    (setq techniques (sort techniques #'lessp))

    (format-html s) (crlf s)
    (format-head s)
    (format s "<title>Matrix</title>")
    (finish-head s) (crlf s)
    (format-body s *bgcolor*) (crlf s)
    (output-header s)
    (output-displaymatrix s examples techniques)
    (output-footer s)
    (finish-body s) (crlf s)
    (finish-html s) (crlf s)))

;(defun find-prettyname (x) (let ((r (fullfindx '?x `(prettyname ,x ?x) *repository*))) (if r r x)))
(defun find-datashortname (x) (let ((r (fullfindx '?x `(shortname ,x ?x) *repository*))) (if r r x)))

(defun output-displaymatrix (s examples techniques)
  (setq examples (remove-if-not #'(lambda (x) (fullfindp `(matrixshow ,x true) *repository*)) examples))
  (setq techniques (remove-if-not #'(lambda (x) (fullfindp `(matrixshow ,x true) *repository*)) techniques))

  (let (c i (*namer* 'prettyname))
    (multiple-value-setq (c i) 
      (split #'(lambda (x) (fullfindp `(example.complete ,x complete) *repository*)) examples))

    ; first the complete table
    ;(format s "<center>")
    ;(format "<div class=\"subheader\">Complete examples</div>") (crlf s)
    ;(output-displaymatrix-table s c techniques)
    ;(format s "</center>")

    ; then the incomplete table
    (format s "<center>")
    ;(format s "<div class=\"subheader\">Incomplete examples</div>") (crlf s)
    (output-displaymatrix-table s (append c i) techniques)
    (format s "</center>")))

(defun output-displaymatrix-table (s examples techniques)
  (format s "<table class=\"matrix\">") (crlf s)
  (format s "<tr><td class=\"rowhead\"></td>")
  
  (dolist (h techniques)
    (format s "<td class=\"rowhead\"><a href=\"fastinspectpage?object=~A\">~A</a></td>" h (find-datashortname h))
    (crlf s))
  (format s "</tr>")
  
  (dolist (e examples)
    (format s "<tr><td class=\"rowhead\"><a href=\"fastinspectpage?object=~A\">~A</a></td>" e (find-prettyname e))
    (crlf s)
    (dolist (q techniques)
      (format s "<td>")
      (format s "<div class=\"matrixplus\">~A</div>" (if (fullfindp `(example.technique ,e ,q) *repository*) "+" ""))
      (format s "</td>")))
  
  (format s "</table>"))
      
