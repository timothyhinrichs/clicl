;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; (c) Copyright 1993-2000 by Mergent Systems.  All rights reserved.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defparameter *interactive* t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Seek
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod process (s (action (eql 'seek)) postlines)
  (let ((classname (cdar postlines)) (*cells* *cells*) (*var-count* 0))
    (cond ((null (cdr postlines)) (output-seek s classname))
          (t (refresh-seek s classname postlines)))))

(defun output-seek (s classname)
  (let (class page)
    (setq class (read-value-string classname))
    (setq page (seek-findpage class))
    (cond ((not page) (old-seek s classname))
          (t (new-seek s page)))))

(defun seek-findpage (class)
  (findx '?x `(findpage ,class ?x) *interface*))

(defun old-seek (s classname)
  (let ((class (read-value-string classname))
        (aspectname (concatenate 'string "?" classname))
        (objects))
;    (setq objects (request `(ask-all ?x (isa ?x ,class)) *client* *receiver*))
    (setq objects (trues '?x `(image.isa ?x image) *warehouse*))
    (output-prolog s 200)
    (output-header s "Find ~A" (prettify class))
    (format s "<P>Find every <B>")
    (output-doclink s class)
    (format s "</B> that satisfies the following criteria.")
    (format s "<FORM ACTION=seek? METHOD=POST NAME=\"form1\">"
            (addressify class))
    (output-seeker s aspectname class objects)
    (format s "</FORM>")
    (format s "</BODY>")))

(defun output-seeker (s aspectname class objects)
  (format-hidden s "Class" (stringify class))
  (format-hidden s "Predicate" (stringify (find-predicate class)))
  (format-hidden s "Aspect" (htmlify aspectname))
  (format s "<TABLE>")
  (dolist (slot (queryable-slots class)) (output-seek-cell s slot objects))
  (format s "</TABLE>")
  (format-hidden s "Concept-End" ""))

(defun output-seek-cell (s slot objects)
  (let ((style (findx '?t `(searchstyle ,slot ?t) *interface*)))
    (cond ((eq style 'menu) (output-seek-menu-cell s slot objects))
          ((eq style 'selector) (output-seek-selector-cell s slot objects))
          ((eq style 'checkbox) (output-seek-checkbox-cell s slot objects))
          ((eq style 'radiobutton) (output-seek-radiobutton-cell s slot objects))
          (t nil))))

(defun output-seek-menu-cell (s slot objects)
  (format-hidden s "Cell-Start" (stringify (incf *cells*)))
  (format-hidden s "Style" "Menu")
  (format-hidden s "Slot" (stringify slot))
  (format s "<TR><TH ALIGN=LEFT VALIGN=TOP>")
  (output-find-doclink s slot)
  (format s "</TH><TD VALIGN=TOP>")
;  (output-find-button s slot *cells*)
  (format s "</TD><TD>")
  (output-seek-menu s *cells* (seek-options slot objects) (find-inputs slot))
  (format s "</TD><TD VALIGN=TOP>")
  (format s (prettify (find-searchlabel slot)))
  (format s "</TD></TR>")
  (format-hidden s "Cell-End" ""))

(defun output-seek-selector-cell (s slot objects)
  (format-hidden s "Cell-Start" (stringify (incf *cells*)))
  (format-hidden s "Style" "Selector")
  (format-hidden s "Slot" (stringify slot))
  (format s "<TR><TH ALIGN=LEFT>")
  (output-find-doclink s slot)
  (format s "</TH><TD VALIGN=TOP>")
;  (output-open-button s slot *cells*)
  (format s "</TD><TD>")
  (output-seek-selector s *cells* (seek-options slot objects) (find-input slot))
  (format s "</TD><TD>")
  (format s (prettify (find-searchlabel slot)))
  (format s "</TD></TR>")
  (format-hidden s "Cell-End" ""))

(defun output-seek-checkbox-cell (s slot objects)
  (format-hidden s "Cell-Start" (stringify (incf *cells*)))
  (format-hidden s "Style" "Checkbox")
  (format-hidden s "Slot" (stringify slot))
  (format s "<TR><TH ALIGN=LEFT VALIGN=TOP>")
  (output-find-doclink s slot)
  (format s "</TH><TD VALIGN=TOP>")
;  (output-open-button s slot *cells*)
  (format s "</TD><TD>")
  (output-checkboxes s *cells* (find-checkbox-options slot) (find-inputs slot))
  (format s "</TD><TD VALIGN=TOP>")
  (format s (prettify (find-searchlabel slot)))
  (format s "</TD></TR>")
  (format-hidden s "Cell-End" ""))


(defun refresh-seek (s classname postlines)
  (let ((class (read-value-string classname)) aspect kif objects)
    (setq class (read-value-string (cdar postlines)))
    (setq aspect (first-aspect postlines))
    (setq kif (let ((*postlines* postlines)) (convert-frame)))
    (setq objects (request `(ask-all ,aspect ,kif) *client* *agent*))
;    (setq objects (trues '?x `(image.isa ?x image) *warehouse*))
    (output-prolog s 200)
    (output-header s "Find ~A" (prettify class))
    (format s "<P>Find every <B>")
    (output-doclink s class)
    (format s "</B> that satisfies the following criteria.")
    (format s "<FORM ACTION=SEEK? METHOD=POST NAME=\"form1\">" (addressify class))
    (setq postlines (refresh-subframe s postlines objects))
;    (setq postlines (reissue-frame-sorters s class postlines))
;    (reissue-frame-commands s postlines)
    (format s "</FORM>")
    (format s "</BODY>")))

(defun refresh-subframe (s postlines objects)
  (let ((classname (cdr (pop postlines)))
        (predicatename (cdr (pop postlines)))
        (aspectname (cdr (pop postlines))))
    (format-hidden s "Class" (htmlify classname))
    (format-hidden s "Predicate" (htmlify predicatename))
    (format-hidden s "Aspect" (htmlify aspectname))
    (format s "<TABLE>")
    (do ()
        ((or (null postlines) (string= (caar postlines) "Concept-End"))
         (pop postlines))
        (setq postlines (refresh-cell s postlines objects)))
    (format s "</TABLE>")
    (format-hidden s "Concept-End" "")
    postlines))

(defun refresh-cell (s postlines objects)
  (let ((type (cdadr postlines)))
    (cond ((string= type "Menu") (refresh-menu-cell s postlines objects))
          ((string= type "Selector") (refresh-selector-cell s postlines objects))
          ((string= type "Checkbox") (refresh-checkbox-cell s postlines objects))
          ((string= type "Radiobutton") (refresh-radiobutton-cell s postlines objects)))))

(defun refresh-menu-cell (s postlines objects)
  (let (name slot options values)
    (setq name (cdr (pop postlines)))
    (pop postlines)
    (setq slot (read-value-string (cdr (pop postlines))))
    (setq options (seek-options slot objects))
    (do ()
        ((or (null postlines) (not (string= (caar postlines) name)))
         (pop postlines))
        (setq values (cons (read-value-string (cdr (pop postlines))) values)))
    (format-hidden s "Cell-Start" (stringify (incf *cells*)))
    (format-hidden s "Style" "Menu")
    (format-hidden s "Slot" (stringify slot))
    (format s "<TR><TH ALIGN=LEFT VALIGN=TOP>")
    (output-doclink s slot)
    (format s "</TH><TD VALIGN=\"TOP\">")
;    (output-open-button s slot *cells*)
    (format s "</TD><TD>")
    (output-seek-menu s *cells* options values)
    (format s "</TD><TD VALIGN=TOP>")
    (format s (prettify (find-searchlabel slot)))
    (format s "</TD></TR>")
    (format-hidden s "Cell-End" "")
    postlines))

(defun refresh-selector-cell (s postlines objects)
  (let (name slot options values)
    (setq name (cdr (pop postlines)))
    (pop postlines)
    (setq slot (read-value-string (cdr (pop postlines))))
    (setq options (seek-options slot objects))
    (do ()
        ((or (null postlines) (not (string= (caar postlines) name)))
         (pop postlines))
        (setq values (cons (read-value-string (cdr (pop postlines))) values)))
    (format-hidden s "Cell-Start" (stringify (incf *cells*)))
    (format-hidden s "Style" "Selector")
    (format-hidden s "Slot" (stringify slot))
    (format s "<TR><TH ALIGN=LEFT>")
    (output-doclink s slot)
    (format s "</TH><TD VALIGN=\"TOP\">")
;    (output-open-button s slot *cells*)
    (format s "</TD><TD>")
    (output-seek-selector s *cells* options (car values))
    (format s "</TD><TD>")
    (format s (prettify (find-searchlabel slot)))
    (format s "</TD></TR>")
    (format-hidden s "Cell-End" "")
    postlines))

(defun refresh-checkbox-cell (s postlines objects)
  (let (name slot options values)
    (setq name (cdr (pop postlines)))
    (pop postlines)
    (setq slot (read-value-string (cdr (pop postlines))))
    (setq options (seek-options slot objects))
    (do ()
        ((or (null postlines) (not (string= (caar postlines) name)))
         (pop postlines))
        (setq values (cons (read-value-string (cdr (pop postlines))) values)))
    (format-hidden s "Cell-Start" (stringify (incf *cells*)))
    (format-hidden s "Style" "Checkbox")
    (format-hidden s "Slot" (stringify slot))
    (format s "<TR><TH ALIGN=LEFT VALIGN=TOP>")
    (output-doclink s slot)
    (format s "</TH><TD VALIGN=TOP>")
;    (output-open-button s slot *cells*)
    (format s "</TD><TD>")
    (output-seek-checkboxes s *cells* options values)
    (format s "</TD><TD VALIGN=TOP>")
    (format s (prettify (find-searchlabel slot)))
    (format s "</TD></TR>")
    (format-hidden s "Cell-End" "")
    postlines))

(defun refresh-radiobutton-cell (s postlines objects)
  (let (name slot options values)
    (setq name (cdr (pop postlines)))
    (pop postlines)
    (setq slot (read-value-string (cdr (pop postlines))))
    (setq options (seek-options slot objects))
    (do ()
        ((or (null postlines) (not (string= (caar postlines) name)))
         (pop postlines))
        (setq values (cons (read-value-string (cdr (pop postlines))) values)))
    (format-hidden s "Cell-Start" (stringify (incf *cells*)))
    (format-hidden s "Style" "Radiobutton")
    (format-hidden s "Slot" (stringify slot))
    (format s "<TR><TH ALIGN=LEFT VALIGN=TOP>")
    (output-doclink s slot)
    (format s "</TH><TD VALIGN=TOP>")
;    (output-open-button s slot *cells*)
    (format s "</TD><TD>")
    (output-seek-radiobuttons s *cells* options (car values))
    (format s "</TD><TD VALIGN=TOP>")
    (format s (prettify (find-searchlabel slot)))
    (format s "</TD></TR>")
    (format-hidden s "Cell-End" "")
    postlines))


(defun output-seek-menu (s slot options values)
  (format s "<SELECT NAME=\"~A\" SIZE=~D MULTIPLE onChange='window.document.form1.submit()'>"
          (stringify slot) (menu-size (length options)))
  (dolist (option options)
    (if (member option values :test #'equalp)
        (format s "<OPTION SELECTED>~A~%" (stringify option))
        (format s "<OPTION>~A~%" (stringify option))))
  (format s "</SELECT>"))

(defun output-seek-selector (s slot options value)
  (format s "<SELECT NAME=\"~A\" onChange='window.document.form1.submit()'>" (stringify slot))
  (dolist (option options)
    (if (equalp option value)
        (format s "<OPTION SELECTED>~A~%" (stringify option))
        (format s "<OPTION>~A~%" (stringify option))))
  (format s "</SELECT>"))

(defun output-seek-checkboxes (s slot options values)
  (format s "<DL>")
  (dolist (option options)
    (format s "<DT>")
    (format-find-checkbox s (stringify slot) (stringify option)
                     (member option values :test #'equalp)))
  (format s "</DL>"))

(defun output-seek-radiobuttons (s slot options value)
  (when options
    (format s "<DL>")
    (format s "<DT>")
    (format-find-radiobutton s (stringify slot) (stringify (car options))
                        (equal (car options) value))
    (dolist (option (cdr options))
      (format s "<DT>")
      (format-find-radiobutton s (stringify slot) (stringify option)
                          (equal option value)))
    (format s "</DL>")))

(defun seek-options (relation objects)
  (let (class options)
  (cond (*interactive*
         (cons 'unknown
               (request `(ask-all ?y (and (oneof ?x . ,objects)
                                          (,relation ?x ?y)))
                        *client* *receiver*)))
        ((finds '?v `(option ,relation ?v) *interface*))
        ((and (setq class (find-range relation))
              (setq options (request `(ask-all ?x (isa ?x ,class))
                                     *client* *gui*)))
         (cons 'unknown (sort options #'lessp)))
        (t (list 'unknown)))))

(defun seek-options (relation objects)
  (let (options)
    (dolist (object objects)
      (dolist (sentence (indexees object *warehouse*))
        (when (and (eq relation (car sentence)) (eq object (cadr sentence)))
          (setq options (adjoin (caddr sentence) options)))))
    (cons 'unknown (sort options #'lessp))))

#|
(defun addsorted (x items)
  (cond ((null items) (list x))
        ((equalp x (car items)) items)
        (t (do ((l items (cdr l)))
               ((null (cdr l)) (rplacd l (list x))  items)
               (cond ((equalp x (caar l))
|#

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
