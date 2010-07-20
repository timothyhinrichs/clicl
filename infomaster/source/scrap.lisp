;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; (c) Copyright 2000 by Michael R. Genesereth.  All rights reserved.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod materializetable (rel source (target fileserver) &optional (flush 'no))
  (materializer rel source target flush)
  (updatefile target))
un 
(defmethod dematerializetable (rel (target fileserver))
  (dematerializer rel target)
  (updatefile target))

(defmethod setvalue ((slot (eql 'agentcapability)) obj val)
  (let (sentences)
    (when (and (symbolp obj) (boundp obj)
               (typep (setq obj (symbol-value obj)) 'dataserver)
               (not (eq (kind obj) val)))
      (setq sentences (contents obj))
      (empty obj)
      (case (kind obj)
        (compiled
         (setf (kind obj) val)
         (setf (indexing obj) 'relationalindexing)
         (setf (inference obj) 'dataserver))
        (indexed
         (setf (kind obj) val)
         (setf (indexing obj) 'fullindexing)
         (setf (inference obj) 'fastserver))
        (indexedcompiled
         (setf (kind obj) val)
         (setf (indexing obj) 'fullindexing)
         (setf (inference obj) 'dataserver))
        (indexedcompiledrules
         (setf (kind obj) val)
         (setf (indexing obj) 'fullindexing)
         (setf (inference obj) 'findserver))
        (indexedcompiledrulesinversion
         (setf (kind obj) val)
         (setf (indexing obj) 'fullindexing)
         (setf (inference obj) 'fullserver)))
      (define-theory obj "" sentences))))

(defmethod envindexps (p al (th dataserver))
  (case (kind th)
    (compiled (dataenvindexps p al th))
    (indexed (dataenvindexps p al th))
    (indexedcompiled (dataenvindexps p al th))
    (indexedcompiledrules (call-next-method p al th))
    (indexedcompiledrulesinversion (call-next-method p al th))))


(definemore *manager*
  '(
    (isa agentcapability  attributerelation)
    (isa agentcapability  relation)
    (isa agentcapability  thing)
    (superrelation agentcapability  true)
    (arity         agentcapability  2)
    (domain        agentcapability  agent)
    (range         agentcapability  dbagentcapability )
    (singlevalued   agentcapability  yes)
    (alwayshasvalue agentcapability  no)
    (changestyle   agentcapability  dropdownlist)
    (searchstyle   agentcapability  dropdownlist)
    (comparestyle  agentcapability  glyph)
    (inspectstyle  agentcapability  glyph)
    (option        agentcapability  compiled)
    (option        agentcapability  indexed)
    (option        agentcapability  indexedcompiled)
    (option        agentcapability  indexedcompiledrules)
    (option        agentcapability  indexedcompiledrulesinversion)
    (documentation agentcapability  "Kind of storage.")

(isa fileserver class)
    (isa fileserver thing)
    (superclass fileserver agent)
    (attribute  fileserver agentcapability)
    (attribute  fileserver extractmetadata)
    (attribute  fileserver filename)
    (attribute  fileserver format)
    (attribute  fileserver interest)
    (attribute  fileserver specialty)
    (attribute  fileserver rootclass)
    (attribute  fileserver rootrelation)
    (attribute  fileserver editor)
    (attribute  fileserver capability)
    (attribute  fileserver listener)
    (attribute  fileserver nocreate)
    (attribute  fileserver nochange)
    (attribute  fileserver noupdate)
    (attribute  fileserver nocommand)
    (attribute  fileserver frontpage)
    (attribute  fileserver page)
    (attribute  fileserver header)
    (attribute  fileserver footer)
    (attribute  fileserver logfile)
    (attribute  fileserver security)

    (isa filename attributerelation) 
    (isa filename relation) 
    (isa filename thing) 
    (superrelation filename true)
    (arity         filename 2) 
    (domain        filename fileserver) 
    (range         filename string)
    (singlevalued   filename yes)
    (alwayshasvalue filename yes) 
    (changestyle   filename stringfield)
    (searchstyle   filename stringfield)
    (comparestyle  filename glyph)
    (inspectstyle  filename glyph)
    (documentation filename "Name of file served by file server.")

    (isa format attributerelation)
    (isa format relation)
    (isa format thing)
    (superrelation format true)
    (arity         format 2)
    (domain        format fileserver)
    (range         format language)
    (singlevalued   format yes)
    (alwayshasvalue format yes)
    (changestyle   format dropdownlist)
    (searchstyle   format dropdownlist)
    (comparestyle  format glyph)
    (inspectstyle  format glyph)
    (option        format cdf)
    (option        format cdt)
    (option        format ookif)
    (option        format csv)
    (option        format kif)
    (option        format sdt)
    (option        format semanticxml)
    (option        format tdt)
    (option        format vdt)
    (documentation format "Language used by resource.")))

(defun find-extractmetadata (x)
  (findx '?x `(extractmetadata ,x ?x) *manager*))

(defun find-filename (x)
  (findx '?x `(filename ,x ?x) *manager*))

(defun find-format (x)
  (findx '?x `(format ,x ?x) *manager*))

(defmethod setvalue ((slot (eql 'filename)) obj val)
  (declare (ignore val))
  (checkfile (referent obj)))

(defmethod setvalue ((slot (eql 'format)) obj val)
  (declare (ignore val))
  (checkfile (referent obj)))

(defmethod setvalue ((slot (eql 'format)) obj val)
  (declare (ignore val))
  (referent obj))

(defmethod setvalue ((slot (eql 'datarepository)) obj val)
  (when (and (symbolp obj) (boundp obj) (typep (symbol-value obj) 'fileserver))
    (request `(update . ,(contents (referent obj))) obj val)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Fileservers
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass fileserver (dataserver)
  ((filedate :accessor filedate :initarg :filedate :initform 0)))

(defmethod empty ((obj fileserver))
  (setf (filedate obj) 0)
  (call-next-method obj))

(defmethod create (obj (type (eql 'fileserver)))
  (unless (and (boundp obj) (typep (symbol-value obj) 'fileserver))
    (set obj (make-instance 'fileserver :name obj))
    (telladministrator `(isa ,obj agent))))

(defmethod destroy (obj (type (eql 'fileserver)))
  (cond ((and (symbolp obj) (boundp obj)
              (eq (type-of (symbol-value obj)) 'fileserver))
         (empty (symbol-value obj))
         (makunbound obj)
         (untelladministrator `(isa ,obj agent)))))

(defmethod affirm (p sender (receiver fileserver))
  (checkfile receiver)
  (call-next-method p sender receiver)
  (updatefile receiver))

(defmethod affirmall (p q sender (receiver fileserver))
  (checkfile receiver)
  (call-next-method p q sender receiver)
  (updatefile receiver))

(defmethod retract (p sender (receiver fileserver))
  (checkfile receiver)
  (call-next-method p sender receiver)
  (updatefile receiver))

(defmethod retractall (p q sender (receiver fileserver))
  (checkfile receiver)
  (call-next-method p q sender receiver)
  (updatefile receiver))

(defmethod discardall (x sender (receiver fileserver))
  (declare (ignore sender))
  (checkfile receiver)
  (kill x receiver)
  (updatefile receiver))

(defmethod askp (p sender (receiver fileserver))
  (declare (ignore sender))
  (checkfile receiver)
  (findp p receiver))

(defmethod askx (x p sender (receiver fileserver))
  (declare (ignore sender))
  (checkfile receiver)
  (findx x p receiver))

(defmethod asks (x p sender (receiver fileserver))
  (declare (ignore sender))
  (checkfile receiver)
  (finds x p receiver))

(defmethod asktable (ol sl sender (receiver fileserver))
  (declare (ignore sender))
  (checkfile receiver)
  (findtable ol sl))

(defmethod askabout (x sender (receiver fileserver))
  (declare (ignore sender))
  (checkfile receiver)
  (sentences x receiver))

(defun checkit (receiver)
  (checkfile receiver)
  'done)

(defmethod checkloadfile (fn target lang meta)
  (declare (ignore fn target lang meta))
  "Checkloadfile works with local agents only.")

(defmethod checkloadfile (fn (target symbol) lang meta)
  (cond ((and (boundp target) (not (symbolp (symbol-value target))))
         (checkloadfile fn (symbol-value target) lang meta))
        (t (call-next-method fn target lang meta))))

(defmethod checkloadfile (fn (target theory) lang meta)
  (let (sentences)
    (setq sentences (read-file-contents fn lang))
    (setq sentences (finddifferential (contents target) sentences))
    (dolist (sent sentences) (insert sent target))
    (sendem `(update . ,sentences) target)
    (when (eq meta 'dynamic) (introspect target))
    'done))

(defmethod checkloadrelation (rel fn target lang meta)
  (declare (ignore rel fn target lang meta))
  "Checkloadrelation works with local agents only.")

(defmethod checkloadrelation (rel fn (target theory) lang meta)
  (let (sentences)
    (setq sentences (read-file-rows rel fn lang))
    (setq sentences (finddifferential (contents target) sentences))
    (dolist (sent sentences) (insert sent target))
    (sendem `(update . ,sentences) target)
    (when (eq meta 'yes) (introspect target))
    'done))

(defun checkfile (receiver)
  (let (fn old new rel lang meta)
    (setq fn (find-filename (name receiver)))
    (setq lang (find-format (name receiver)))
    (setq meta (find-extractmetadata (name receiver)))
    (when (and fn lang)
      (setq old (filedate receiver) new (file-write-date fn))
      (cond ((equal old new))
            ((find lang '(comma-delimited-text space-delimited-text
                          stroke-delimited-text tab-delimited-text csv html))
             (setq rel (intern (strappend (symbol-name (name receiver)) ".TABLE")))
             (setf (filedate receiver) new)
             (ignore-errors (checkloadrelation rel fn receiver lang meta)))
            (t (setf (filedate receiver) new)
               (ignore-errors (checkloadfile fn receiver lang meta)))))))

(defun updatefile (receiver)
  (let (fn rel lang)
    (setq fn (find-filename (name receiver)))
    (setq lang (find-format (name receiver)))
    (when (and fn lang)
      (cond ((find lang '(comma-delimited-text space-delimited-text
                          stroke-delimited-text tab-delimited-text csv html))
             (setq rel (intern (strappend (symbol-name (name receiver)) ".TABLE")))
             (ignore-errors (dumprelation rel receiver fn lang))
             (setf (filedate receiver) (file-write-date fn)))
            (t (ignore-errors (dumpfile receiver fn lang))
               (setf (filedate receiver) (file-write-date fn)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun output-slotlink (s concept)
  (let (doc mgr)
    (if (setq doc (find-documentation concept))
        (format s "<A HREF=\"DISPLAYTABLE?Relation=~A\" target=\"_top\" onMouseOver='window.status=\"~A\"; return true'>~A</A>"
                (addressify (name *interface*)) (addressify concept) doc (iconify concept))
        (format s "<A HREF=\"DISPLAYTABLE?Relation=~A\" target=\"_top\">~A</A>"
                (addressify concept) (iconify concept)))))

(defun output-classlink (s concept)
  (let (doc)
    (if (setq doc (find-documentation concept))
        (format s "<A HREF=\"DISPLAYCLASS?Class=~A\" target=\"_top\" onMouseOver='window.status=\"~A\"; return true'>~A</A>"
                (addressify concept) doc (prettify concept))
        (format s "<A HREF=\"DISPLAYCLASS?Class=~A\" target=\"_top\">~A</A>"
                (addressify concept) (prettify concept)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Structure-preserving Planner
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#|
(defparameter *filter*  #'basep)

(defun unwind (x p *theory* &optional (*filter* #'basep))
  (let ((*consistency* nil) (alist (environment)))
    (unwindexp x p)))

(defun unwindexp (x p)
  (cond ((atom p) p)
        ((eq 'and (car p)) (unwindand x p))
        ((eq 'or (car p)) (unwindor x p))
        ((and (funcall *filter* (operator p))
              (funcall *test* (setq p (plugstdexp p alist)))) p)
        (t (unwinddb x p))))

(defun unwindand (x p)
  (do ((l (cdr p) (cdr l)) (dum) (nl))
      ((null l) (maksand (nreverse nl)))
      (setq dum (unwindexp x (car l)))
      (cond ((eq dum 'true))
            ((or (null dum) (eq dum 'false)) (return 'false))
            (t (setq nl (cons dum nl))))))

(defun unwindor (x p)
  (do ((l (cdr p) (cdr l)) (dum) (nl))
      ((null l) (maksor (nreverse nl)))
      (setq dum (unwindexp x (car l)))
      (cond ((eq 'true dum) (return 'true))
            ((or (null dum) (eq dum 'false)))
            (t (setq nl (cons dum nl))))))

(defun unwinddb (x p)
  (maksor (residues x p *theory* *filter* #'success)))
|#

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Updating widgets
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun xml-table-to-kif-table (tree)
  (do ((l (cdr tree) (cdr l)) (nl))
      ((null l) (nreverse nl))
      (do ((m (cdar l) (cdr m)) (nm))
          ((null m) (setq nl (cons (nreverse nm) nl)))
          (setq nm (cons (xml-to-kif (cadar m)) nm)))))

(defun xml-to-kif (tree)
  (cond ((atom tree) tree)
        ((eq 'pcdata (car tree)) (cadr tree))
        ((and (listp (car tree)) (eq 'table (caar tree)))
         (xml-table-to-kif-table tree))
        (t (xmlname tree))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Updating widgets
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defparameter *interactive* t)

(defun new-old-frame (s classname)
  (let ((class (read-value-string classname))
        (aspectname (concatenate 'string "?" classname))
        (objects))
    (setq objects (request `(ask-all ?x (isa ?x ,class)) *client* *receiver*))
    (output-prolog s 200)
    (output-header s "Find ~A" (prettify class))
    (format s "<P>Find every <B>")
    (output-doclink s class)
    (format s "</B> that satisfies the following criteria.")
    (format s "<FORM ACTION=FRAME? METHOD=POST TARGET=\"_top\">" (addressify class))
    (new-output-subframe s aspectname class objects)
    (output-frame-sorter s class)
    (output-frame-commands s)
    (format s "</FORM>")
    (output-footer s)))

(defun new-output-subframe (s aspectname class objects)
  (format-hidden s "Class" (stringify class))
  (format-hidden s "Predicate" (stringify (find-predicate class)))
  (format-hidden s "Aspect" (htmlify aspectname))
  (format s "<TABLE>")
  (dolist (slot (queryable-slots class))
    (new-output-query-cell s slot objects))
  (format s "</TABLE>")
  (format-hidden s "Concept-End" ""))

(defun new-output-query-cell (s slot objects)
  (let ((style (findx '?t `(searchstyle ,slot ?t) *interface*)))
    (cond ((eq style 'menu) (new-output-menu-cell s slot objects))
          ((eq style 'selector) (output-selector-cell s slot))
          ((eq style 'multiselector) (output-multiselector-cell s slot))
          ((eq style 'checkbox) (output-checkbox-cell s slot))
          ((eq style 'radiobutton) (output-radiobutton-cell s slot))
          ((eq style 'interval) (output-interval-cell s slot))
          ((eq style 'intermenu) (output-intermenu-cell s slot))
          ((eq style 'stringfield) (output-stringfield-cell s slot))
          ((eq style 'text) (output-text-cell s slot))
          ((eq style 'textarea) (output-text-cell s slot))
          ((eq style 'password))
          ((eq style 'message) (output-message-cell s slot))
          ((eq style 'subframe) (output-subframe-cell s slot))
          (t (output-typein-cell s slot)))))

(defun new-output-menu-cell (s slot objects)
  (format-hidden s "Cell-Start" (stringify (incf *cells*)))
  (format-hidden s "Style" "Menu")
  (format-hidden s "Slot" (stringify slot))
  (format s "<TR><TH ALIGN=LEFT VALIGN=TOP>")
  (output-doclink s slot)
  (format s "</TH><TD VALIGN=TOP>")
  (output-open-button s slot *cells*)
  (format s "</TD><TD>")
  (output-menu s *cells* (new-find-options slot objects) (find-inputs slot))
  (format s "</TD><TD VALIGN=TOP>")
  (format s (prettify (find-searchlabel slot)))
  (format s "</TD></TR>")
  (format-hidden s "Cell-End" ""))

(defun new-find-options (relation objects)
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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defun fullplan (pl al depth cont)
  (setq *inferences* (1+ *inferences*))
  (cond ((>= *inferences* *limit*) (setq *termination* t) nil)
        ((> depth *depth*) (setq *termination* t) nil)
        (t (fullplanexp (car pl) pl al depth cont))))

(defun fullplanexp (p pl al depth cont)
  (cond ((atom p) (fullplanconstant p pl al depth cont))
        ((eq 'not (car p)) (fullplannot (cadr p) pl al depth cont))
        ((eq 'and (car p)) (fullplanand p pl al depth cont))
        ((eq 'or (car p)) (fullplanor p pl al depth cont))
        ((eq 'oneof (car p)) (fullplanoneof p pl al depth cont))
        ((eq 'member (car p)) (fullplanmember p pl al depth cont))
        ((eq 'same (car p)) (fullplansame p pl al depth cont))
        ((eq 'distinct (car p)) (fullplandistinct p pl al depth cont))
	((eq 'ground (car p)) (fullplanground p pl al depth cont))
	((eq 'nonground (car p)) (fullplannonground p pl al depth cont))
	((eq 'primitive (car p)) (fullplanprimitive p pl al depth cont))
	((eq 'nonprimitive (car p)) (fullplannonprimitive p pl al depth cont))
	((eq '== (car p)) (fullplanvalue p pl al depth cont))
	((eq 'value (car p)) (fullplanvalue p pl al depth cont))
        ((eq 'execute (car p)) (fullplanexecute p pl al depth cont))
        ((eq 'evaluate (car p)) (fullplanevaluate p pl al depth cont))
        ((eq 'unprovable (car p)) (fullplanunprovable p pl al depth cont))
        ((eq 'bagofall (car p)) (fullplanbagofall p pl al depth cont))
        ((eq 'strmatch (car p)) (fullplanstrmatch p pl al depth cont))
	((get (car p) 'basicval) (fullplanbasicvalue p pl al depth cont))
        ((get (car p) 'basic) (fullplanbasic p pl al depth cont))
        (t (fullplancallfail p pl al depth cont))))

(defun fullplannot (p pl al depth cont)
  (cond ((atom p) (fullplannotconstant p pl al depth cont))
        ((eq 'not (car p)) (fullplanexp (cadr p) pl al depth cont))
        ((eq 'and (car p)) (fullplannotand p pl al depth cont))
        ((eq 'or (car p)) (fullplannotor p pl al depth cont))
        ((eq 'oneof (car p)) nil)
        ((eq 'same (car p)) nil)
        ((eq 'distinct (car p)) nil)
	((eq 'ground (car p)) nil)
	((eq 'nonground (car p)) nil)
	((eq 'primitive (car p)) nil)
	((eq 'nonprimitive (car p)) nil)
	((eq '== (car p)) (fullplannotvalue p pl al depth cont))
	((eq 'value (car p)) (fullplannotvalue p pl al depth cont))
        ((eq 'execute (car p)) (fullplannotexecute p pl al depth cont))
        ((eq 'evaluate (car p)) (fullplannotevaluate p pl al depth cont))
        ((eq 'unprovable (car p)) (fullplanexp (cadr p) pl al depth cont))
        ((eq 'bagofall (car p)) (fullplannotbagofall p pl al depth cont))
        ((eq 'strmatch (car p)) (fullplannotstrmatch p pl al depth cont))
	((get (car p) 'basicval) (fullplannotbasicvalue p pl al depth cont))
        ((get (car p) 'basic) (fullplannotbasic p pl al depth cont))
        (t (fullplancallfail `(not ,p) pl al depth cont))))


(defun fullplanconstant (p pl al depth cont)
  (cond ((eq 'true p) (fullplanexit pl al depth cont))
        ((eq 'false p) nil)
        (t (fullplancallfail p pl al depth cont))))

(defun fullplannotconstant (p pl al depth cont)
  (cond ((eq 'true p) nil)
        ((eq 'false p) (fullplanexit pl al depth cont))
        (t (fullplancallfail `(not ,p) pl al depth cont))))

(defun fullplanand (p pl al depth cont)
  (cond ((null (cdr p)) (fullplanexit pl al depth cont))
        (t (fullplanexp (cadr p) (append (cdr p) pl) al depth cont))))

(defun fullplannotand (p pl al depth cont)
  (do ((l (cdr p) (cdr l)))
      ((null l))
      (fullplannot (car l) pl al depth cont)))

(defun fullplanor (p pl al depth cont)
  (do ((l (cdr p) (cdr l)))
      ((null l))
      (fullplanexp (car l) pl al depth cont)))

(defun fullplannotor (p pl al depth cont)
  (cond ((null (cdr p)) (fullplanexit pl al depth cont))
        (t (fullplannot (cadr p) al (append (mapcar #'maknot (cddr p)) pl) depth cont))))

(defun fullplanoneof (p pl al depth cont)
  (when (seqvarp (caddr p)) (setq p (plug p al)))
  (do ((l (cddr p) (cdr l)) (bl))
      ((null l) t)
      (if (setq bl (mgwexp (cadr p) (car l) al))
          (fullplanexit pl bl depth cont))))

(defun fullplanmember (p pl al depth cont)
  (do ((l (cdr (plug (caddr p) al)) (cdr l)) (bl))
      ((null l) t)
      (if (setq bl (mgwexp (cadr p) (car l) al))
          (fullplanexit pl bl depth cont))))

(defun fullplansame (p pl al depth cont)
  (let (ol)
    (when (setq ol (unify (cadr p) al (caddr p) al))
      (fullplanexit pl al depth cont)
      (backup ol))))

(defun fullplandistinct (p pl al depth cont)
  (if (unify (cadr p) al (caddr p) al) nil (fullplanexit pl al depth cont)))

(defun fullplanground (p pl al depth cont)
  (setq p (plug p al))
  (if (groundp p) (fullplanexit pl al depth cont)))

(defun fullplannonground (p pl al depth cont)
  (setq p (plug p al))
  (if (groundp p) nil (fullplanexit pl al depth cont)))

(defun fullplanprimitive (p pl al depth cont)
  (setq p (plug p al))
  (if (primitivep p) (fullplanexit pl al depth cont)))

(defun fullplannonprimitive (p pl al depth cont)
  (setq p (plug p al))
  (if (primitivep p) nil (fullplanexit pl al depth cont)))

(defun fullplanvalue (p pl al depth cont)
  (let (x y)
    (setq p (plug p al) x (cadr p) y (caddr p))
    (cond ((atom x) (when (setq al (mgwexp x y al)) (fullplanexit pl al depth cont)))
          ((eq 'bagofall (car x))
           (setq x (cons 'listof (fullplans (cadr x) (caddr x) *theory*)))
           (when (setq al (mgwexp y x al)) (fullplanexit pl al depth cont)))
          ((and (groundp x) (every #'primitivep (cdr x)))
           (setq x (funcall (get (car x) 'basicval) x))
           (when (setq al (mgwexp x y al)) (fullplanexit pl al depth cont))))))

(defun fullplannotvalue (p pl al depth cont)
  (let (x y)
    (setq p (plug p al) x (cadr p) y (caddr p))
    (cond ((atom x) (unless (mgwexp x y al) (fullplanexit pl al depth cont)))
          ((eq 'bagofall (car x))
           (setq x (cons 'listof (fullplans (cadr x) (caddr x) *theory*)))
           (unless (mgwexp y x al)) (fullplanexit pl al depth cont))
          ((and (groundp x) (every #'primitivep (cdr x)))
           (setq x (funcall (get (car x) 'basicval) x))
           (unless (mgwexp x y al) (fullplanexit pl al depth cont))))))

(defun fullplanexecute (p pl al depth cont)
  (let (values)
    (setq p (plug p al))
    (cond ((null (cddr p))
           (if (ignore-errors (eval (cadr p))) (fullplanexit pl al depth cont)))
          ((and (car (setq values (ignore-errors (multiple-value-list (eval (cadr p))))))
                (setq al (matchpexp (cddr p) (mapcar #'quotify values) al)))
           (fullplanexit pl al depth cont)))))
    
(defun fullplannotexecute (p pl al depth cont)
  (let (values)
    (setq p (plug p al))
    (cond ((null (cddr p))
           (if (ignore-errors (not (eval (cadr p)))) (fullplanexit pl al depth cont)))
          ((and (car (setq values (ignore-errors (multiple-value-list (eval (cadr p))))))
                (not (matchpexp (cddr p) (mapcar #'quotify values) al)))
           (fullplanexit pl al depth cont)))))

(defun fullplanevaluate (p pl al depth cont)
  (let (values)
    (setq p (plug p al))
    (cond ((null (cddr p))
           (if (ignore-errors (apply (caadr p) (cdadr p))) (fullplanexit pl al depth cont)))
          ((and (car (setq values (ignore-errors (multiple-value-list (apply (caadr p) (cdadr p))))))
                (setq al (matchpexp (cddr p) values al)))
           (fullplanexit pl al depth cont)))))
    
(defun fullplannotevaluate (p pl al depth cont)
  (let (values)
    (setq p (plug p al))
    (cond ((null (cddr p))
           (if (ignore-errors (not (apply (caadr p) (cdadr p)))) (fullplanexit pl al depth cont)))
          ((and (car (setq values (ignore-errors (multiple-value-list (apply (caadr p) (cdadr p))))))
                (not (matchpexp (cddr p) values al)))
           (fullplanexit pl al depth cont)))))

(defun fullplanunprovable (p pl al depth cont)
  (cond ((fullplanexp (cadr p) pl al depth cont) nil)  ;; fullplanone when ready
        (t (fullplanexit pl al depth cont))))

(defun fullplanbagofall (p pl al depth cont)
  (fullplanvalue `(value ,(butlast p) ,(cadddr p)) pl al depth cont))

(defun fullplannotbagofall (p pl al depth cont)
  (fullplannotvalue `(value ,(butlast p) ,(cadddr p)) pl al depth cont))

(defun fullplanstrmatch (p pl al depth cont)
  (fullplanexp `(execute (strmatches ,(cadr p) ,(caddr p)) ? . ,(cdddr p)) pl al depth cont))

(defun fullplannotstrmatch (p pl al depth cont)
  (fullplannot `(execute (strmatches ,(cadr p) ,(caddr p)) ? . ,(cdddr p)) pl al depth cont))

(defun fullplanbasicvalue (p pl al depth cont)
  (let (x y)
    (setq p (plug p al) x (butlast p) y (car (last p)))
    (if (and (groundp x) (every #'primitivep (cdr x))
             (setq x (funcall (get (car x) 'basicval) x))
             (setq al (mgwexp x y al)))
        (fullplanexit pl al depth cont))))

(defun fullplannotbasicvalue (p pl al depth cont)
  (let (x y)
    (setq p (plug p al) x (butlast p) y (car (last p)))
    (if (and (groundp x) (every #'primitivep (cdr x))
             (setq x (funcall (get (car x) 'basicval) x))
             (not (mgwexp x y al)))
        (fullplanexit pl al depth cont))))

(defun fullplanbasic (p pl al depth cont)
  (setq p (plug p al))
  (when (and (groundp p) (apply (get (car p) 'basic) (cdr p)))
    (fullplanexit pl al depth cont)))

(defun fullplannotbasic (p pl al depth cont)
  (setq p (plug p al))
  (if (and (groundp p) (not (apply (get (car p) 'basic) (cdr p))))
      (fullplanexit pl al depth cont)))


(defun fullplancallfail (p pl al depth cont)
  (let (dum)
    (cond ((funcall *filter* (operator p))
           (setq dum (plugstdexp p al))
           (cond ((redundantp dum *residue*) (fullplanexit pl al depth cont))
                 (t (fullplanassume dum pl al depth cont))))
          (t (fullplanrs p pl al depth cont)))))

(defun redundantp (p res)
  (cond ((basep (operator p)))
        ((null (setq res (remove-if #'(lambda (x) (basep (operator x))) res))) nil)
        ((fullfindx 't `(<= ,p . ,res) *theory*))))

(defun fullplanassume (p pl al depth cont)
  (when (or (not *consistency*)
            (not (or (rebuttalp p *residue*) (rebuttheoryp p *theory*)))
            (consistentp p *residue*))
    (let ((*residue* (cons p *residue*))) (fullplanexit pl al depth cont))))

(defun fullplanrs (p pl al depth cont)
  (cond ((and *ancestry* (fullplanancestor p al cont)) nil)
        ((fullplanreduce p pl al depth cont))
        (*termination* nil)
        (t (fullplandb p pl al depth cont *theory*))))

(defun fullplanancestor (p al cont)
  (do ((l cont (cdr l)))
      ((null l) nil)
      (if (identify (caar l) (cadar l) p al) (return t))))

(defun fullplanreduce (p pl al depth cont)
  (do ((l cont (cdr l)) (ol))
      ((null l))
      (when (setq ol (unify (maknot (caaar l)) (cadar l) p al))
        (if (fullplanexit pl al depth cont) (return t) (backup ol)))))

(defun fullplandb (p pl al depth cont th)
  (cond ((fullplanth p pl al depth cont th))
        (t (do ((l (includees th) (cdr l)))
               ((null l))
               (when (fullplandb p pl al depth cont (car l)) (return t))))))

(defun fullplanth (p pl al depth cont th)
  (do ((l (envindexps p al th) (cdr l)) (bl (environment)) (ol))
      ((null l) nil)
      (cond ((and (listp (car l)) (eq '<= (caar l))
                  (setq ol (unify (cadar l) bl p al)))
             (if (fullplan (cddar l) bl
                           (1+ depth) (cons (list pl al depth) cont))
                 (return t)
                 (backup ol)))
            ((setq ol (unify (car l) bl p al))
             (if (fullplanexit pl al depth cont) (return t) (backup ol))))))

(defun fullplanexit (pl al depth cont)
  (cond ((cdr pl) (fullplanexp (cadr pl) (cdr pl) al depth cont))
        (cont (fullplanexit (caar cont) (cadar cont) (caddar cont) (cdr cont)))
        (t (setq *answers* (cons *residue* *answers*)))))



(defmethod getchanges (p (th agent))
  (if (and (listp p) (eq 'and (car p)))
      (do ((l (cdr p) (cdr l)) (rl) (nl))
          ((null l) (setq p (maksand (nreconc nl (nreverse rl)))))
          (cond ((atom (car l)) (setq nl (cons (car l) nl)))
                ((eq '==> (caar l)) (setq rl (cons (car l) rl)))
                (t (setq nl (cons (car l) nl))))))
  (nreverse (computechanges p th nil)))


(defun sequpdate (nl th)
  (do ((l nl (cdr l)) (dum))
      ((null l) 'done)
      (setq dum (affirm (car l) th)))
  (let (tells)
    (do ((l rules (cdr l)))
        ((null l) (setq tells (nreverse tells)))
        (cond ((atom (car l)) (setq tells (cons (car l) tells)))
              ((eq '==> (caar l))
               (dolist (answer (asks (caddar l) (cadar l) sender receiver))
                 (setq tells (contribute answer tells))))
              (t (setq tells (contribute (car l) tells)))))
    (do ((l tells (cdr l)) (dum) (nl))
        ((null l) 'done)
        (setq dum (affirm (car l) sender receiver))
        (cond ((stringp dum)
               (retractemall nl sender receiver)
               (return dum))
              (t (setq nl (cons (car l) nl)))))))

;;;;

(defun residuecallfail (p al depth stack next done)
  (let (dum)
    (cond ((funcall *filter* (operator p))
           (cond ((resknownrs p al depth stack
                              `(resknowncallfailnext ,depth ,next) done))
                 ((funcall *test* (setq dum (plugstdexp p al)))
                  (residueassume dum next done))
                 (t (profail nil next))))
          (t (residuers p al depth stack
                        `(resknowncallfailnext ,depth ,next) done)))))

(defun resknowncallfailnext (ans depth next)
  (cond ((null ans) (profail nil next))
        ((eql ans depth) (profail nil next))
        (t (profail ans next))))

(defun resknownrs (p al depth stack next done)
  (cond ((and *ancestry* (resknownancestor p al stack)) (profail nil next))
        ((and (numberp *ancestry*) (resknownnumber p al stack 0))
         (residueassumption nil p al next done))
        (*reduction* ;;; (or (atom p) (not (eq 'not (car p)))))
         (resknownrsloop nil p al depth stack stack nil next done))
        (t (resknowndb p al depth stack *theory* next done))))

(defun resknownnumber (p al stack n)
  (let (ol)
    (cond ((numgeqp n *ancestry*))
          ((null stack) nil)
          ((atom p)
           (resknownnumber p al (cdr stack) (if (eq p (caar stack)) (1+ n) n)))
          ((setq ol (unify p al (caar stack) (cdar stack)))
           (prog1 (resknownnumber p al (cdr stack) (1+ n)) (backup ol)))
          (t (resknownnumber p al (cdr stack) n)))))

(defun resknownancestor (p al stack)
  (do ((l stack (cdr l)))
      ((null l) nil)
      (if (identify (caar l) (cdar l) p al) (return t))))

(defun resknownrsloop (ans p al depth stack sl ol next done)
  (backup ol)
  (cond ((not (null ans)) (profail ans next))
        ((null sl) (resknowndb p al depth stack *theory* next done))
        ((setq ol (unify (maknot (caar sl)) (cdar sl) p al))
         (proexit `(resknownrsloop ,p ,al ,depth ,stack ,(cdr sl) ,ol ,next ,done)
                  done))
        (t (resknownrsloop nil p al depth stack (cdr sl) ol next done))))

(defun resknowndb (p al depth stack th next done)
  (resknownth p al depth stack th
          `(resknowndbs ,p ,al ,depth ,stack ,(includees th) ,next ,done) done))

(defun resknowndbs (ans p al depth stack tl next done)
  (cond ((not (null ans)) (profail ans next))
        ((null tl) (profail nil next))
        (t (resknowndb p al depth stack (car tl)
                   `(resknowndbs ,p ,al ,depth ,stack ,(cdr tl) ,next ,done) done))))

(defun resknownth (p al depth stack th next done)
 (resknownths nil p al depth stack (envindexps p al th) (environment) nil next done))

;;; ground test in rule case would save work but it is expensive.
;;; note that subset test not good in rule case cause subgoals may bind vars.
;;; we should allow *saves* to work here when caching problem solved.

(defun resknownths (ans p al depth stack fl bl ol next done)
  (backup ol)
  (cond ((not (null ans)) (profail ans next))
        ((null fl) (profail nil next))
        ((setq ol (unify (car fl) bl p al))
         (if (not (subolp ol (alist bl)))
             (setq next `(resknownths ,p ,al ,depth ,stack ,(cdr fl) ,bl ,ol ,next ,done)))
         (proexit next done))
        (t (resknownths nil p al depth stack (cdr fl) bl nil next done))))

;;;;

(defun newconsequence (antecedent consequent al rl depth stack)
  (do ((l (fullresidues (vars consequent) antecedent *theory* *filter*) (cdr l)))
      ((null l) rl)
      (newconsequencedepth consequent al (makand rl (car l))
                           (1+ depth) (cons (operator consequent) stack))))
(defun fullresidue (*thing* p *theory* &optional (*filter* #'basep) (*test* #'success))
  (car (fullresidues *thing* p *theory* *filter* *test*)))

(defun fullresidues (*thing* p *theory* &optional (*filter* #'basep) (*test* #'success))
  (let (alist *residue* *answers* (*ancestry* t))
    (setq *unifications* 0)
    (setq *inferences* 0)
    (setq *termination* nil)
    (setq p (adjust  *thing* (order *thing* p)))
    (setq alist (environment))
;    (decludes 'epitheory)
;    (empty 'epitheory)
;    (includes 'epitheory *theory*)
;    (setq *theory* 'epitheory)
;    (dolist (x (contrapositives `(<= (answer ,*thing*) ,p))) (save x 'epitheory))
;    (fullres `(answer ,*thing*) nil alist 1 (list (list (list `(answer ,*thing*)) alist 1)))
    (fullres p nil alist 1 nil)
    (nreverse (uniquify *answers*))))

(defun fullres (p pl al depth cont)
  (setq *inferences* (1+ *inferences*))
  (cond ((>= *inferences* *limit*) (setq *termination* t) nil)
        ((> depth *depth*) (setq *termination* t) nil)
        (t (fullresexp p pl al depth cont))))

(defun fullresexp (p pl al depth cont)
  (cond ((atom p) (fullresconstant p pl al depth cont))
        ((eq 'not (car p)) (fullresnot (cadr p) pl al depth cont))
        ((eq 'and (car p)) (fullresand p pl al depth cont))
        ((eq 'or (car p)) (fullresor p pl al depth cont))
        ((eq 'oneof (car p)) (fullresoneof p pl al depth cont))
        ((eq 'member (car p)) (fullresmember p pl al depth cont))
        ((eq 'same (car p)) (fullressame p pl al depth cont))
        ((eq 'distinct (car p)) (fullresdistinct p pl al depth cont))
	((eq 'ground (car p)) (fullresground p pl al depth cont))
	((eq 'nonground (car p)) (fullresnonground p pl al depth cont))
	((eq 'primitive (car p)) (fullresprimitive p pl al depth cont))
	((eq 'nonprimitive (car p)) (fullresnonprimitive p pl al depth cont))
	((eq '== (car p)) (fullresvalue p pl al depth cont))
	((eq 'value (car p)) (fullresvalue p pl al depth cont))
        ((eq 'execute (car p)) (fullresexecute p pl al depth cont))
        ((eq 'evaluate (car p)) (fullresevaluate p pl al depth cont))
        ((eq 'unprovable (car p)) (fullresunprovable p pl al depth cont))
        ((eq 'bagofall (car p)) (fullresbagofall p pl al depth cont))
        ((eq 'strmatch (car p)) (fullresstrmatch p pl al depth cont))
	((get (car p) 'basicval) (fullresbasicvalue p pl al depth cont))
        ((get (car p) 'basic) (fullresbasic p pl al depth cont))
        ((funcall *filter* (operator p)) (fullresassumption p pl al depth cont))
        (t (fullresrs p pl al depth cont))))

(defun fullresassumption (p pl al depth cont)
  (let (dum)
    (cond ((and (groundishp (setq dum (plugstdexp p al)))
                (knownp dum *theory*))
           (fullresexit pl al depth cont))
          ((funcall *test* (setq dum (plugstdexp p al)))
           (fullresassume dum pl al depth cont)))))

(defun fullresassumption (p pl al depth cont)
  (let (dum)
    (cond ((fullknownrs p pl al depth cont))
          ((funcall *test* (setq dum (plugstdexp p al)))
           (fullresassume dum pl al depth cont)))))

(defun fullresassume (p pl al depth cont)
  (when (or (not *consistency*)
            (not (or (rebuttalp p *residue*) (rebuttheoryp p *theory*)))
            (consistentp p *residue*))
    (let ((*residue* (cons p *residue*))) (fullresexit pl al depth cont))))

(defun fullresnot (p pl al depth cont)
  (cond ((atom p) (fullresnotconstant p pl al depth cont))
        ((eq 'not (car p)) (fullresexp (cadr p) pl al depth cont))
        ((eq 'and (car p)) (fullresnotand p pl al depth cont))
        ((eq 'or (car p)) (fullresnotor p pl al depth cont))
        ((eq 'oneof (car p)) nil)
        ((eq 'same (car p)) nil)
        ((eq 'distinct (car p)) nil)
	((eq 'ground (car p)) nil)
	((eq 'nonground (car p)) nil)
	((eq 'primitive (car p)) nil)
	((eq 'nonprimitive (car p)) nil)
	((eq '== (car p)) (fullresnotvalue p pl al depth cont))
	((eq 'value (car p)) (fullresnotvalue p pl al depth cont))
        ((eq 'execute (car p)) (fullresnotexecute p pl al depth cont))
        ((eq 'evaluate (car p)) (fullresnotevaluate p pl al depth cont))
        ((eq 'unprovable (car p)) (fullresexp (cadr p) pl al depth cont))
        ((eq 'bagofall (car p)) (fullresnotbagofall p pl al depth cont))
        ((eq 'strmatch (car p)) (fullresnotstrmatch p pl al depth cont))
	((get (car p) 'basicval) (fullresnotbasicvalue p pl al depth cont))
        ((get (car p) 'basic) (fullresnotbasic p pl al depth cont))
        ((get (car p) 'basic) (fullresnotbasic p pl al depth cont))
        ((funcall *filter* (operator p)) (fullresassumption `(not ,p) pl al depth cont))
        (t (fullresrs `(not ,p) pl al depth cont))))

(defun fullresconstant (p pl al depth cont)
  (cond ((eq 'true p) (fullresexit pl al depth cont))
        ((eq 'false p) nil)
        (t (fullresrs p pl al depth cont))))

(defun fullresnotconstant (p pl al depth cont)
  (cond ((eq 'true p) nil)
        ((eq 'false p) (fullresexit pl al depth cont))
        (t (fullresrs `(not ,p) pl al depth cont))))

(defun fullresand (p pl al depth cont)
  (cond ((null (cdr p)) (fullresexit pl al depth cont))
        (t (fullresexp (cadr p) (append (cdr p) pl) al depth cont))))

(defun fullresnotand (p pl al depth cont)
  (do ((l (cdr p) (cdr l)))
      ((null l))
      (fullresnot (car l) pl al depth cont)))

(defun fullresor (p pl al depth cont)
  (do ((l (cdr p) (cdr l)))
      ((null l))
      (fullresexp (car l) pl al depth cont)))

(defun fullresnotor (p pl al depth cont)
  (cond ((null (cdr p)) (fullresexit pl al depth cont))
        (t (fullresnot (cadr p) al (append (mapcar #'maknot (cddr p)) pl) depth cont))))

(defun fullresoneof (p pl al depth cont)
  (when (seqvarp (caddr p)) (setq p (plug p al)))
  (do ((l (cddr p) (cdr l)) (bl))
      ((null l) t)
      (if (setq bl (mgwexp (cadr p) (car l) al))
          (fullresexit pl bl depth cont))))

(defun fullresmember (p pl al depth cont)
  (do ((l (cdr (plug (caddr p) al)) (cdr l)) (bl))
      ((null l) t)
      (if (setq bl (mgwexp (cadr p) (car l) al))
          (fullresexit pl bl depth cont))))

(defun fullressame (p pl al depth cont)
  (let (ol)
    (when (setq ol (unify (cadr p) al (caddr p) al))
      (fullresexit pl al depth cont)
      (backup ol))))

(defun fullresdistinct (p pl al depth cont)
  (if (unify (cadr p) al (caddr p) al) nil (fullresexit pl al depth cont)))

(defun fullresground (p pl al depth cont)
  (setq p (plug p al))
  (if (groundp p) (fullresexit pl al depth cont)))

(defun fullresnonground (p pl al depth cont)
  (setq p (plug p al))
  (if (groundp p) nil (fullresexit pl al depth cont)))

(defun fullresprimitive (p pl al depth cont)
  (setq p (plug p al))
  (if (primitivep p) (fullresexit pl al depth cont)))

(defun fullresnonprimitive (p pl al depth cont)
  (setq p (plug p al))
  (if (primitivep p) nil (fullresexit pl al depth cont)))

(defun fullresvalue (p pl al depth cont)
  (let (x y)
    (setq p (plug p al) x (cadr p) y (caddr p))
    (cond ((atom x) (when (setq al (mgwexp x y al)) (fullresexit pl al depth cont)))
          ((eq 'bagofall (car x))
           (setq x (cons 'listof (fullfinds (cadr x) (caddr x) *theory*)))
           (when (setq al (mgwexp y x al)) (fullresexit pl al depth cont)))
          ((and (groundp x) (every #'primitivep (cdr x)))
           (setq x (funcall (get (car x) 'basicval) x))
           (when (setq al (mgwexp x y al)) (fullresexit pl al depth cont))))))

(defun fullresnotvalue (p pl al depth cont)
  (let (x y)
    (setq p (plug p al) x (cadr p) y (caddr p))
    (cond ((atom x) (unless (mgwexp x y al) (fullresexit pl al depth cont)))
          ((eq 'bagofall (car x))
           (setq x (cons 'listof (fullfinds (cadr x) (caddr x) *theory*)))
           (unless (mgwexp y x al)) (fullresexit pl al depth cont))
          ((and (groundp x) (every #'primitivep (cdr x)))
           (setq x (funcall (get (car x) 'basicval) x))
           (unless (mgwexp x y al) (fullresexit pl al depth cont))))))

(defun fullresexecute (p pl al depth cont)
  (let (values)
    (setq p (plug p al))
    (cond ((null (cddr p))
           (if (ignore-errors (eval (cadr p))) (fullresexit pl al depth cont)))
          ((and (car (setq values (ignore-errors (multiple-value-list (eval (cadr p))))))
                (setq al (matchpexp (cddr p) (mapcar #'quotify values) al)))
           (fullresexit pl al depth cont)))))
    
(defun fullresnotexecute (p pl al depth cont)
  (let (values)
    (setq p (plug p al))
    (cond ((null (cddr p))
           (if (ignore-errors (not (eval (cadr p)))) (fullresexit pl al depth cont)))
          ((and (car (setq values (ignore-errors (multiple-value-list (eval (cadr p))))))
                (not (matchpexp (cddr p) (mapcar #'quotify values) al)))
           (fullresexit pl al depth cont)))))

(defun fullresevaluate (p pl al depth cont)
  (let (values)
    (setq p (plug p al))
    (cond ((null (cddr p))
           (if (ignore-errors (apply (caadr p) (cdadr p))) (fullresexit pl al depth cont)))
          ((and (car (setq values (ignore-errors (multiple-value-list (apply (caadr p) (cdadr p))))))
                (setq al (matchpexp (cddr p) values al)))
           (fullresexit pl al depth cont)))))
    
(defun fullresnotevaluate (p pl al depth cont)
  (let (values)
    (setq p (plug p al))
    (cond ((null (cddr p))
           (if (ignore-errors (not (apply (caadr p) (cdadr p)))) (fullresexit pl al depth cont)))
          ((and (car (setq values (ignore-errors (multiple-value-list (apply (caadr p) (cdadr p))))))
                (not (matchpexp (cddr p) values al)))
           (fullresexit pl al depth cont)))))

(defun fullresunprovable (p pl al depth cont)
  (cond ((fullres (cadr p) pl al depth cont) nil)  ;; fullfindone when ready
        (t (fullresexit pl al depth cont))))

(defun fullresbagofall (p pl al depth cont)
  (fullresvalue `(value ,(butlast p) ,(cadddr p)) pl al depth cont))

(defun fullresnotbagofall (p pl al depth cont)
  (fullresnotvalue `(value ,(butlast p) ,(cadddr p)) pl al depth cont))

(defun fullresstrmatch (p pl al depth cont)
  (fullresexp `(execute (strmatches ,(cadr p) ,(caddr p)) ? . ,(cdddr p)) pl al depth cont))

(defun fullresnotstrmatch (p pl al depth cont)
  (fullresnot `(execute (strmatches ,(cadr p) ,(caddr p)) ? . ,(cdddr p)) pl al depth cont))

(defun fullresbasicvalue (p pl al depth cont)
  (let (x y)
    (setq p (plug p al) x (butlast p) y (car (last p)))
    (if (and (groundp x) (every #'primitivep (cdr x))
             (setq x (funcall (get (car x) 'basicval) x))
             (setq al (mgwexp x y al)))
        (fullresexit pl al depth cont))))

(defun fullresnotbasicvalue (p pl al depth cont)
  (let (x y)
    (setq p (plug p al) x (butlast p) y (car (last p)))
    (if (and (groundp x) (every #'primitivep (cdr x))
             (setq x (funcall (get (car x) 'basicval) x))
             (not (mgwexp x y al)))
        (fullresexit pl al depth cont))))

(defun fullresbasic (p pl al depth cont)
  (setq p (plug p al))
  (when (and (groundp p) (apply (get (car p) 'basic) (cdr p)))
    (fullresexit pl al depth cont)))

(defun fullresnotbasic (p pl al depth cont)
  (setq p (plug p al))
  (if (and (groundp p) (not (apply (get (car p) 'basic) (cdr p))))
      (fullresexit pl al depth cont)))


(defun fullknownrs (p pl al depth cont)
  (cond ((and *ancestry* (fullknownancestor p al cont)) nil)
        ((fullknownreduce p pl al depth cont))
        (*termination* nil)
        (t (fullknowndb p pl al depth cont *theory*))))

(defun fullknownancestor (p al cont)
  (do ((l cont (cdr l)))
      ((null l) nil)
      (if (identify (caar l) (cadar l) p al) (return t))))

(defun fullknownreduce (p pl al depth cont)
  (do ((l cont (cdr l)) (ol))
      ((null l))
      (when (setq ol (unify (maknot (caaar l)) (cadar l) p al))
        (fullresexit pl al depth cont)
        (backup ol))))

(defun fullknowndb (p pl al depth cont th)
  (fullknownth p pl al depth cont th)
  (do ((l (includees th) (cdr l)))
      ((null l))
      (fullknowndb p pl al depth cont (car l))))

(defun fullknownth (p pl al depth cont th)
  (do ((l (envindexps p al th) (cdr l)) (bl (environment)) (ol))
      ((null l) nil)
      (when (setq ol (unify (car l) bl p al))
        (fullresexit pl al depth cont)
        (backup ol))))


(defun fullresrs (p pl al depth cont)
  (cond ((and *ancestry* (fullresancestor p al cont)) nil)
        ((fullresreduce p pl al depth cont))
        (*termination* nil)
        (t (fullresdb p pl al depth cont *theory*))))

(defun fullresancestor (p al cont)
  (do ((l cont (cdr l)))
      ((null l) nil)
      (if (identify (caar l) (cadar l) p al) (return t))))

(defun fullresreduce (p pl al depth cont)
  (do ((l cont (cdr l)) (ol))
      ((null l))
      (when (setq ol (unify (maknot (caaar l)) (cadar l) p al))
        (fullresexit pl al depth cont)
        (backup ol))))

(defun fullresdb (p pl al depth cont th)
  (fullresth p pl al depth cont th)
  (do ((l (includees th) (cdr l)))
      ((null l))
      (fullresdb p pl al depth cont (car l))))

(defun fullresth (p pl al depth cont th)
  (do ((l (envindexps p al th) (cdr l)) (bl (environment)) (ol))
      ((null l) nil)
      (cond ((and (listp (car l)) (eq '<= (caar l))
                  (setq ol (unify (cadar l) bl p al)))
             (fullres (maksand (cddar l)) (cddar l) bl
                      (1+ depth) (cons (list pl al depth) cont))
             (backup ol))
            ((setq ol (unify (car l) bl p al))
             (fullresexit pl al depth cont)
             (backup ol)))))

(defun fullresexit (pl al depth cont)
  (cond ((cdr pl) (fullallexp (cadr pl) (cdr pl) al depth cont))
        (cont (fullresexit (caar cont) (cadar cont) (caddar cont) (cdr cont)))
        (t (setq *answers* (cons (maksand *residue*) *answers*)))))

;;;;

(defun aclreduce (c)
  (cond ((atom c) (aclreduce-atom c))
        ((eq 'update (car c)) (reduceupdate c))
        ((eq 'tell (car c)) (reducetell (cadr c)))
        ((eq 'untell (car c)) (reduceuntell (cadr c)))
        ((eq 'eliminate (car c)) (aclreduce-eliminate c))
        ((eq 'ask-if (car c)) (aclreduce-ask-if c))
        ((eq 'ask-one (car c)) (aclreduce-ask-one c))
        ((eq 'ask-all (car c)) (aclreduce-ask-all c))
        ((eq 'ask-table (car c)) (aclreduce-ask-table c))
        ((eq 'ask-about (car c)) `(aclabout ',c))
        ((eq 'ask-consistent (car c)) (reduceaskconsistent c))
        ((eq 'quote (car c)) c)
        ((macro-function (car c)) (aclreduce (macroexpand c)))
        ((fboundp (car c)) (aclreduce-call c))
        ((findp `(responds ?a ,(car c)) *manager*) (aclreduce-message c))
        ((findp `(performs ?a ,(car c)) *manager*) (aclreduce-request c))))

(defun reduceaskconsistent (msg)
  (let (errors)
    (decludes 'infotheory)
    (empty 'infotheory)
    (includes 'infotheory *library*)
    (dolist (p (cdr msg)) (newsave p 'infotheory))
    (if (setq errors (finds '?x '(error ?x) 'infotheory)) `',errors 't)))

(defun process-change-change (s postlines)
  (let (structure object class result (*buttons* 0) (*cells* 1))
    (multiple-value-setq (structure postlines) (parsestructure postlines))
    (setq object (car structure))
    (setq class (cadr structure))
    (cond ((and (setq result (prorequest `(ask-consistent . ,(converter structure))))
                (errorp result))
           (output-problems s result))
          ((and (setq result (prorequest `(update ,(maksand (differentiator structure)))))
                (errorp result))
           (output-problems s result))
          (t (setq structure (convert-to-inspect structure))
             (output-inspect-page s object class structure)))))

;;;;

(defun createitem (object class)
  (let (values dum)
    (dolist (slot (createable-slots class))
      (setq dum (find-createdefault slot))
      (if dum (setq values (cons (list slot dum) values))
          (setq values (cons (list slot) values))))
    (if class (cons object (cons class (nreverse values))) object)))

(defun maksearchstructure (object class)
  (let (values dum)
    (dolist (slot (queryable-slots class))
      (setq dum (find-searchdefault slot))
      (if dum (setq values (cons (list slot dum) values))
          (setq values (cons (list slot) values))))
    (if class (cons object (cons class (nreverse values))) object)))







(defun output-menu (s slot options values)
  (when options
    (format s "<SELECT NAME=\"~A\" SIZE=~D MULTIPLE>"
            (stringify slot) (menu-size (length options)))
    (dolist (option options)
      (if (member option values :test #'equalp)
          (format s "<OPTION SELECTED VALUE=~A>~A~%" (stringify option) (iconify option))
          (format s "<OPTION VALUE=~A>~A~%" (stringify option) (iconify option))))
    (format s "</SELECT>")))

(defun output-selector (s slot options value)
  (format s "<SELECT NAME=\"~A\">" (stringify slot))
  (dolist (option options)
    (if (equalp option value)
          (format s "<OPTION SELECTED VALUE=~A>~A~%" (stringify option) (iconify option))
          (format s "<OPTION VALUE=~A>~A~%" (stringify option) (iconify option))))
  (format s "</SELECT>"))

(defun output-selector-result (s slot options value)
  (when options
    (format s "<SELECT NAME=\"~A\" onChange='{window.document.form1.target=\"result\"; window.document.form1.submit()}'>"
            (stringify slot))
    (dolist (option options)
      (if (equalp option value)
           (format s "<OPTION SELECTED VALUE=~A>~A~%" (stringify option) (iconify option))
          (format s "<OPTION VALUE=~A>~A~%" (stringify option) (iconify option))))
    (format s "</SELECT>")))



(defun output-display-table (s class items slots)
  (let (results nohandle nobasket selection)
    (setq results (prorequest `(ask-table ,items ,slots)))
    (setq nohandle (findp `(nodisplay ,class handle) *interface*))
    (setq nobasket (or (null items) (findp `(nocommand ,*gui* memory) *interface*)))
    (unless nobasket (setq selection (find-selection *client* *gui*)))
    (format s "<TABLE BGCOLOR=WHITE BORDER>")
    (format s "<TR>")
    (unless nobasket (format s "<TH></TH>"))
    (unless nohandle
      (format s "<TH>")
      (output-classlink s class)
      (format s "</TH>"))
    (dolist (slot slots)
      (format s "<TH>")
      (output-slotlink s slot)
      (format s "</TH>"))
    (format s "</TR>")
    (do ((l items (cdr l)) (m results (cdr m)) (flag nil (not flag)))
        ((null l))
        (if flag (format s "<TR>") (format s "<TR BGCOLOR=#EEEEEE>"))
        (unless nobasket
          (format s "<TH>")
          (format-checkbox s (prettify (car l)) ""
                           (findp (makpred (car l) selection *gui*) *warehouse*))
          (format s "</TH>"))
        (unless nohandle
          (format s "<TH ALIGN=LEFT>")
          (output-value s (car l))  ;;; (output-view s (car l) class)
          (format s "</TH>"))
        (loop for vals in (car m)
              ;;; for class in classes
	      do (setq vals (remove 'unknown vals))
              (if (every #'(lambda (val) (numberp val)) vals)
                (format s "<TD ALIGN=RIGHT>")
                (format s "<TD>") )
              (loop
                for val in vals
                for first-time = t then nil
                unless first-time
                do (format s ", ")
                do (output-value s val))   ;;; (output-view s val class)
              (format s "<BR></TD>"))
        (format s "</TR>")
        (crlf s))
    (format s "</TABLE>")))



(defun aclreduce-ask-all (x)
  (let (args body)
    (setq args (if (atom (cadr x)) (list (cadr x)) (cadr x)) body (caddr x))
    `(findanswers '((<= (answer . ,args) ,(plan-ask-all args body))))))


#|
(defparameter *updater* (make-instance 'transformer :name 'updater))

(defparameter *ruleserver* (make-instance 'dataserver :name 'ruleserver))

(defparameter *factserver* (make-instance 'dataserver :name 'factserver :inference 'fastserver))

(definemore *manager*
  '((rulebase updater ruleserver)
    (recipient updater factserver)
    (base updater m)
    (base updater n)
    (material updater r)
    (material updater s)
    (specialty factserver m)
    (specialty factserver n)
    (specialty factserver r)))

(define-theory *ruleserver* ""
  '((<= (r ?x ?y ?z) (p ?x ?y) (q ?y ?z))
    (<= (p ?x ?y) (m ?x ?y))
    (<= (q ?x ?y) (n ?x ?y))))

(define-theory *factserver* ""
  '((m a b)
    (n b c)
    (r a b c)))

;;; (revisebackwarddynamic '((m a c) (n c e)) *updater*)
;;; DONE
;;;
;;; (show 'r *factserver*)
;;; ((R A C E))

(define-theory *ruleserver* ""
  '((<= (r ?x ?y) (m ?x ?y))
    (<= (r ?x ?y) (n ?x ?y))))

(define-theory *factserver* ""
  '((m a b)
    (n a b)
    (r a b)))

;;; (revisebackwarddynamic '((m a c) (not (n a b))) *updater*)
;;; DONE
;;;
;;; (show 'r *factserver*)
;;; ((R A B) (R A C))
;;;
;;; (revisebackwarddynamic '((m a c) (not (m a b)) (not (n a b))) *updater*)
;;; DONE
;;;
;;; (show 'r *factserver*)
;;; ((NOT (R A B)) (R A C))

(define-theory *ruleserver* ""
  '((<= (r ?x ?z) (m ?x ?y) (n ?y ?z))))

(define-theory *factserver* ""
  '((m a b)
    (n b c)
    (r a c)))

;;; (revisebackwarddynamic '((n b d) (not (n b c))) *updater*)
;;; DONE
;;;
;;; (show 'r *factserver*)
;;; ((R A D))
|#
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; uses backward rules and computes change rules on the fly
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun revisenewer (facts th)
  (setq facts (reduceviewsnew facts th))
  (ramifyviewsnew facts th))

(defun reduceviewsnew (facts receiver)
  (let (rulebase (*intensions* nil))
    (setq rulebase (find-rulebase (name receiver)))
    (when (and rulebase (symbolp rulebase) (boundp rulebase))
      (setq rulebase (symbol-value rulebase)))
    (getconjuncts (residue t (maksand facts) rulebase #'basicp))))

(defun ramifyviewsnew (facts th)
  (let (rulebase target insertions deletions)
    (setq rulebase (symbol-value (find-rulebase (name th))))
    (setq target (symbol-value (find-target (name th))))
    (multiple-value-setq (insertions deletions) (divide facts))
    (setq deletions (denials deletions target rulebase))
    (when facts (change (maksand facts) target))
    (when deletions (change (maksand deletions) target))
    (setq insertions (vows insertions target rulebase))
    (when insertions (change (maksand insertions) target))
    'done))

;;; extend to savep only
;;; any subgoal trigggers
;;; ensure does not use other rules
;;; is this called before or after atoms in p are recorded.
;;; handle view defs

(defparameter *vows* nil)

(defun vows (facts factbase rulebase)
  (let ((alist (environment)) *vows*)
    (setq *termination* nil)
    (dolist (x facts) (vowsdepth x 1 factbase rulebase))
    (nreverse *vows*)))

(defun vowsdepth (p depth factbase rulebase)
  (cond ((numgreaterp depth *depth*) (setq *termination* t) nil)
        (t (when (materialp (operator p)) (setq *vows* (adjoin p *vows*)))
           (vowsdb p depth factbase rulebase))))

(defun vowsdb (p depth factbase rulebase)
  (vowsth p depth factbase rulebase)
  (do ((l (includees rulebase) (cdr l)))
      ((null l) nil)
      (vowsdb p depth factbase (car l))))

(defun vowsth (p depth factbase rulebase)
  (do ((l (indexps p rulebase) (cdr l)) (ol) (bl (environment)) (rule))
      ((null l))
      (when (and (listp (car l)) (eq (caar l) '<=)
                 (setq ol (unify (caddar l) bl p alist)))
        (setq rule (plugstdexp (car l) bl)) (backup ol)
        (dolist (x (finds (cadr rule) (maksand (cdddr rule)) factbase))
          (vowsdepth x (1+ depth) factbase rulebase)))))

(defparameter *denials* nil)

(defun denials (facts factbase rulebase)
  (let ((alist (environment)) *denials*)
    (setq *termination* nil)
    (dolist (x facts) (denialsdepth x 1 factbase rulebase))
    (nreverse *denials*)))

(defun denialsdepth (p depth factbase rulebase)
  (cond ((numgreaterp depth *depth*) (setq *termination* t) nil)
        (t (when (materialp (operator p)) (setq *denials* (adjoin `(not ,p) *denials*)))
           (denialsdb p depth factbase rulebase))))

(defun denialsdb (p depth factbase rulebase)
  (denialsth p depth factbase rulebase)
  (do ((l (includees rulebase) (cdr l)))
      ((null l) nil)
      (denialsdb p depth factbase (car l))))

(defun denialsth (p depth factbase rulebase)
  (do ((l (indexps p rulebase) (cdr l)) (ol) (bl (environment)) (rule))
      ((null l))
      (when (and (listp (car l)) (eq (caar l) '<=)
                 (setq ol (unify (caddar l) bl p alist)))
        (setq rule (plugstdexp (car l) bl)) (backup ol)
        (dolist (x (finds (cadr rule) (maksand (cdddr rule)) factbase))
          (denialsdepth x (1+ depth) factbase rulebase)))))



(defparameter *updater* (make-instance 'transformer :name 'updater))

(defparameter *ruleserver* (make-instance 'dataserver :name 'ruleserver))

(defparameter *factserver* (make-instance 'dataserver :name 'factserver :inference 'fastserver))

(definemore *manager*
  '((rulebase updater ruleserver)
    (recipient updater factserver)
    (base updater m)
    (base updater n)
    (material updater r)
    (material updater s)
    (specialty factserver m)
    (specialty factserver n)
    (specialty factserver r)))

(define-theory *ruleserver* ""
  '((<= (p ?x ?y) (m ?x ?y))
    (<= (q ?x ?y) (n ?x ?y))
    (<= (r ?x ?y ?z) (m ?x ?y) (n ?y ?z))))

(define-theory *factserver* ""
  '((m a b)
    (n b c)
    (r a b c)))

;;; (revisenewer '((m a c) (n c e)) *updater*)
;;; ((R A C E))




;;; extend to savep only
;;; any subgoal trigggers
;;; ensure does not use other rules
;;; is this called before or after atoms in p are recorded.

(defmethod avow (p *theory*)
  (let ((alist (environment)) (level 0) tracecalls)
    (setq *termination* nil)
    (avowdepth p 1)
    t))

(defun avowdepth (p depth)
  (cond ((numgreaterp depth *depth*) (setq *termination* t) nil)
        (t (avowexp p depth))))

(defun avowexp (p depth)
  (cond ((atom p) (avowexpexit p depth))
        ((eq 'and (car p)) (dolist (x (cdr p)) (avowexp x depth)))
        (t (avowexpexit p depth))))

(defun avowexpexit (p depth)
  (cond ((knownp p *theory* 'samep) nil)
        ((and (listp p) (eq 'execute (car p))) (ignore-errors (eval (cadr p))))
        ((and (listp p) (eq 'evaluate (car p))) (ignore-errors (apply (caadr p) (cdadr p))))
        ((and (savep p) (insert p *theory*) nil))
        (t (avowdb p depth *theory*))))

(defun avowdb (p depth th)
  (avowth p depth th)
  (do ((l (includees th) (cdr l)))
      ((null l) nil)
      (avowdb p depth (car l))))

(defun avowth (p depth th)
  (do ((l (indexps p th) (cdr l)) (ol) (bl (environment)) (rule))
      ((null l))
      (when (and (listp (car l)) (eq (caar l) '<=)
                 (setq ol (unify (caddar l) bl p alist)))
        (setq rule (plugstdexp (car l) bl)) (backup ol)
        (dolist (x (finds (cadr rule) (maksand (cdddr rule)) *theory*))
          (avowdepth x (1+ depth))))))

(deftheory fullserver ""
  (m a b)
  (n b c)
  (r a b c)
  (<= (r ?x ?y ?z) (p ?x ?y) (q ?y ?z))
  (<= (p ?x ?y) (m ?x ?y))
  (<= (q ?x ?y) (n ?x ?y)))



(defmethod newviews (p th *filter* *test*)
  (let ((alist (environment)) (*theory* (make-instance 'theory))
        (*newviews*) (level 0) tracecalls)
    (setq *termination* nil)
    (unwind-protect
      (progn (newsave p *theory*)
             (includes *theory* th)
             (newviewdepth p alist 'true 1 nil))
      (progn (decludes *theory*)
             (empty *theory*)))
    (nreverse *newviews*)))

(defun newsave (p th)
  (cond ((atom p) (save p th))
        ((eq 'and (car p)) (mapc #'(lambda (x) (newsave x th)) (cdr p)))
        (t (save p th))))

(defun newviewdepth (p al res depth stack)
  (cond ((numgreaterp depth *depth*) (setq *termination* t) nil)
        ((find (operator p) (cdr stack)) nil)
        (t (if traceexpressions (tracesave p al))
           (newviewexp p al res depth stack)
           (if traceexpressions (tracedone p al)))))

(defun newviewexp (p al res depth stack)
  (cond ((atom p) (newviewdb p al res depth stack *theory*))
        ((eq 'and (car p)) 
         (mapc #'(lambda (x) (newviewdepth x al res depth stack)) (cdr p)))
        ((funcall *test* (operator p))
         (cond ((eq res 'false))
               ((eq res 'true)
                (setq *newviews*
                      (adjoin (plugstdexp p al) *newviews* :test #'equalp)))
               (t (setq *newviews*
                        (cons (plugstdexp `(==> ,res ,p) al) *newviews*))))
         (newviewdb p al res depth stack *theory*))
        (t (newviewdb p al res depth stack *theory*))))  

(defun newviewdb (p al rl depth stack th)
  (cond ((newviewth p al rl depth stack th))
        (t (do ((l (includees th) (cdr l)))
               ((null l) nil)
               (newviewdb p al rl depth stack (car l))))))

(defun newviewth (p al rl depth stack th)
  (do ((l (envindexps p al th) (cdr l)) (ol) (bl (environment))
       (antecedent) (consequent))
      ((null l))
      (when (and (listp (car l)) (eq (caar l) '=>) (not (null (cddar l)))
                 (setq ol (unify (cadar l) bl p al)))
        (if tracefacts (tracefact (car l)))
        (setq antecedent (plugstdexp (maksand (butlast (cddar l))) bl))
        (setq consequent (plugstdexp (car (last (car l))) bl))
        (newview antecedent consequent al rl depth stack)
        (backup ol))))

(defun newview (antecedent consequent al rl depth stack)
  (do ((l (residues (vars consequent) antecedent *theory* *filter*) (cdr l)))
      ((null l) rl)
      (newviewdepth consequent al (makand rl (car l))
                           (1+ depth) (cons (operator consequent) stack))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; uses backward change rules defined in advance
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun revisebackwardstatic (facts th)
  (setq facts (reduceviewsbackwardstatic facts th))
  (change (maksand facts) th)
  (ramifyviewsbackwardstatic facts th))

(defun reduceviewsbackwardstatic (facts receiver)
  (let (rulebase (*intensions* nil))
    (setq rulebase (find-rulebase (name receiver)))
    (when (and rulebase (symbolp rulebase) (boundp rulebase))
      (setq rulebase (symbol-value rulebase)))
    (getconjuncts (residue t (maksand facts) rulebase #'basicp))))

(defun ramifyviewsbackwardstatic (facts th)
  (let (rulebase target insertions deletions relations conclusions changes)
    (setq rulebase (find-rulebase (name th)))
    (setq target (find-target (name th)))
    (multiple-value-setq (insertions deletions) (divide facts))
    (dolist (p facts) (setq relations (getrelations p relations)))
    (dolist (r relations)
      (setq conclusions (getconclusions r (symbol-value rulebase) conclusions)))
    (dolist (r conclusions)
      (setq changes (nreconc (computedeletions r deletions rulebase target) changes))
      (setq changes (nreconc (computeinsertions r insertions rulebase target) changes)))
    (change (maksand (nreverse (uniquify changes))) target)))

(defun computedeletions (r deletions rulebase factbase)
  (let (plan)
    (setq plan (transformdeletion r (symbol-value rulebase) factbase))
    (setq plan (substdeletions deletions plan))
    (request plan nil factbase)))

(defun computeinsertions (r insertions rulebase factbase)
  (let (plan)
    (setq plan (transforminsertion r (symbol-value rulebase) factbase))
    (setq plan (substinsertions insertions plan))
    (request plan nil factbase)))

(defun transformdeletion (r *library* *target*)
  (let (arity vars p rl (*ancestry* 1))
    (when (setq arity (find-arity r))
      (do ((i 1 (1+ i)) (vl))
          ((> i arity) (setq vars (nreverse vl)))
          (setq vl (cons (decolonize (newindvar)) vl))))
    (cond ((setq rl (residues vars `(delete ,(cons r vars)) *library* #'basicp))
           (setq p (decolonize (maksor rl)))
           (if *collapse* (setq p (collapse vars p)))
           (if *collapse* (setq p (raisin vars p)))
           (if *compress* (setq p (compress p)))
           `(ask-all (not ,(cons r vars)) ,p)))))

(defun transforminsertion (r *library* *target*)
  (let (arity vars p rl (*ancestry* 1))
    (when (setq arity (find-arity r))
      (do ((i 1 (1+ i)) (vl))
          ((> i arity) (setq vars (nreverse vl)))
          (setq vl (cons (decolonize (newindvar)) vl))))
    (cond ((setq rl (residues vars `(insert ,(cons r vars)) *library* #'basicp))
           (setq p (decolonize (maksor rl)))
           (if *collapse* (setq p (collapse vars p)))
           (if *collapse* (setq p (raisin vars p)))
           (if *compress* (setq p (compress p)))
           `(ask-all ,(cons r vars) ,p)))))

(defun substinsertions (insertions plan)
  (cond ((atom plan) plan)
        ((eq (car plan) 'insertion)
         `(oneof ,(cadr plan) . ,(getdata (caadr plan) insertions)))
        ((eq (car plan) 'and)
         (do ((l (cdr plan) (cdr l)) (nl))
             ((null l) (cons 'and (nreverse nl)))
             (setq nl (cons (substinsertions insertions (car l)) nl))))
        ((eq (car plan) 'or)
         (do ((l (cdr plan) (cdr l)) (nl))
             ((null l) (cons 'or (nreverse nl)))
             (setq nl (cons (substinsertions insertions (car l)) nl))))
        ((eq (car plan) 'ask-all)
         `(ask-all ,(cadr plan) ,(substinsertions insertions (caddr plan))))
        (t plan)))

(defun substdeletions (deletions plan)
  (cond ((atom plan) plan)
        ((eq (car plan) 'deletion)
         `(oneof ,(cadr plan) . ,(getdata (caadr plan) deletions)))
        ((eq (car plan) 'and)
         (do ((l (cdr plan) (cdr l)) (nl))
             ((null l) (cons 'and (nreverse nl)))
             (setq nl (cons (substdeletions deletions (car l)) nl))))
        ((eq (car plan) 'or)
         (do ((l (cdr plan) (cdr l)) (nl))
             ((null l) (cons 'or (nreverse nl)))
             (setq nl (cons (substdeletions deletions (car l)) nl))))
        ((eq (car plan) 'ask-all)
         `(ask-all ,(cadr plan) ,(substdeletions deletions (caddr plan))))
        (t plan)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; uses backward change rules defined in advance executed before updates
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun revisebackwardstatic2 (facts th)
  (setq facts (reduceviewsbackwardstatic2 facts th))
  (ramifyviewsbackwardstatic2 facts th))

(defun reduceviewsbackwardstatic2 (facts receiver)
  (let (rulebase (*intensions* nil))
    (setq rulebase (find-rulebase (name receiver)))
    (when (and rulebase (symbolp rulebase) (boundp rulebase))
      (setq rulebase (symbol-value rulebase)))
    (getconjuncts (residue t (maksand facts) rulebase #'basicp))))

(defun ramifyviewsbackwardstatic2 (facts th)
  (let (rulebase target insertions deletions relations conclusions changes)
    (setq rulebase (find-rulebase (name th)))
    (setq target (find-target (name th)))
    (multiple-value-setq (insertions deletions) (divide facts))
    (dolist (p facts) (setq relations (getrelations p relations)))
    (dolist (r relations)
      (setq conclusions (getconclusions r (symbol-value rulebase) conclusions)))
    (setq changes (reverse facts))
    (dolist (r conclusions)
      (setq changes (nreconc (computedeletions r deletions rulebase target) changes))
      (setq changes (nreconc (computeinsertions r insertions rulebase target) changes)))
    (change (maksand (nreverse (uniquify changes))) target)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; uses forward rules defined in advance
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun reviseforwardstatic (facts th)
  (changeviewsforwardstatic (reduceviewsforwardstatic facts th) th))

(defun reduceviewsforwardstatic (facts receiver)
  (let (rulebase (*intensions* nil))
    (setq rulebase (find-rulebase (name receiver)))
    (when (and rulebase (symbolp rulebase) (boundp rulebase))
      (setq rulebase (symbol-value rulebase)))
    (getconjuncts (residue t (maksand facts) rulebase #'basicp))))

(defun changeviewsforwardstatic (facts updater)
  (let (rulebase target positives negatives rules (*intensions* nil))
    (setq rulebase (find-rulebase updater))
    (when (and rulebase (symbolp rulebase) (boundp rulebase))
      (setq rulebase (symbol-value rulebase)))
    (setq target (find-target updater))
    (when (and target (symbolp target) (boundp target))
      (setq target (symbol-value target)))
    (multiple-value-setq (positives negatives) (divide facts))
    (setq rules (computechangerules negatives rulebase))
    (dolist (p (computechangefacts rules target nil)) (drop p target))
    (dolist (p negatives) (drop p target))
    (dolist (p positives) (insert p target))
    (setq rules (computechangerules positives rulebase))
    (dolist (p (computechangefacts rules target nil)) (insert p target))
    'done))



(defun computechangefacts (facts target nl)
  (do ((l facts (cdr l)))
      ((null l) (nreverse nl))
      (setq nl (computechangefactsatom (car l) target nl))))

(defun computechangefactsatom (p target nl)
  (cond ((atom p) (adjoin p nl))
        ((eq '==> (car p))
         (dolist (item (finds (caddr p) (cadr p) target))
           (setq nl (contribute item nl)))
         nl)
        (t (adjoin p nl :test #'equalp))))

(defun computechangerules (facts rulebase)
  (do ((l facts (cdr l)) (nl))
      ((null l) (nreverse nl))
      (setq nl (nreconc (computechangerulesatom (car l) rulebase) nl))))

(defun computechangerulesatom (p rulebase)
  (let ((*intensions* nil))
    (do ((l (newconsequences p rulebase #'basicp #'materialp) (cdr l)) (nl))
        ((null l) (nreverse nl))
        (cond ((and (listp (car l)) (eq 'execute (caar l)))
               (setq nl (cons (cadar l) nl)))
              ((and (listp (car l)) (eq 'evaluate (caar l)))
               (setq nl (cons `(apply ',(caadar l) ',(cdadar l)) nl)))
              ((and (listp (car l)) (eq '=> (caar l)))
               (setq nl (cons (decolonize `(==> ,(cadar l) ,(caddar l))) nl)))
              (t (setq nl (cons (decolonize (car l)) nl)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;  Test
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
#|
(defparameter *updater* (make-instance 'transformer :name 'updater))

(defparameter *ruleserver* (make-instance 'dataserver :name 'ruleserver))

(defparameter *factserver* (make-instance 'dataserver :name 'factserver :inference 'fastserver))

(definemore *manager*
  '((rulebase updater ruleserver)
    (recipient updater factserver)
    (base updater m)
    (base updater n)
    (material updater r)
    (material updater s)
    (specialty factserver m)
    (specialty factserver n)
    (specialty factserver r)))

(define-theory *ruleserver* ""
  '((<= (r ?x ?y ?z) (p ?x ?y) (q ?y ?z))
    (<= (p ?x ?y) (m ?x ?y))
    (<= (q ?x ?y) (n ?x ?y))))

(define-theory *factserver* ""
  '((m a b)
    (n b c)
    (r a b c)))

;;; (ramifyviews '((m a c) (n c e)) *updater*)
;;; ((R A C E))

(define-theory *ruleserver* ""
  '((<= (r ?x ?y) (m ?x ?y))
    (<= (r ?x ?y) (n ?x ?y))))

(define-theory *factserver* ""
  '((m a b)
    (n a b)
    (r a b)))

;;; (ramifyviews '((m a c) (not (n a b))) *updater*)
;;; ((R A C))
;;;
;;; (ramifyviews '((m a c) (not (m a b)) (not (n a b))) *updater*)
;;; ((NOT (R A B)) (R A C))

(define-theory *ruleserver* ""
  '((<= (r ?x ?z) (m ?x ?y) (n ?y ?z))))

(define-theory *factserver* ""
  '((m a b)
    (n b c)
    (r a c)))

;;; (ramifyviews '((n b d) (not (n b c))) *updater*)
;;; ((NOT (R A C)) (R A D))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;  Tests with static rules
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defparameter *updater* (make-instance 'transformer :name 'updater))

(defparameter *ruleserver* (make-instance 'dataserver :name 'ruleserver))

(defparameter *factserver* (make-instance 'dataserver :name 'factserver :inference 'fastserver))

(definemore *manager*
  '((rulebase updater ruleserver)
    (recipient updater factserver)
    (base updater m)
    (base updater n)
    (material updater r)
    (material updater s)
    (specialty factserver m)
    (specialty factserver n)
    (specialty factserver r)
    (specialty factserver s)
    (arity r 3)
    (arity s 2)))

(define-theory *ruleserver* ""
  '((<= (r ?x ?y ?z) (p ?x ?y) (q ?y ?z))
    (<= (p ?x ?y) (m ?x ?y))
    (<= (q ?x ?y) (n ?x ?y))

    (<= (insert (r ?x ?y ?z)) (insertion (m ?x ?y)) (n ?y ?z))
    (<= (insert (r ?x ?y ?z)) (insertion (n ?y ?z)) (m ?x ?y))))

(define-theory *factserver* ""
  '((m a b)
    (n b c)
    (r a b c)))

;;; (revise '((m a c) (n c e)) *updater*)
;;; DONE
;;;
;;; (trues '(?x ?y ?z) '(r ?x ?y ?z) *factserver*)
;;; ((a c d) (b c d) (b c e) (a c e))

(define-theory *ruleserver* ""
  '((<= (r ?x ?y ?z) (m ?x ?y ?z))
    (<= (r ?x ?y ?z) (n ?x ?y ?z))

    (<= (insert (r ?x ?y ?z)) (insertion (m ?x ?y ?z)))
    (<= (insert (r ?x ?y ?z)) (insertion (n ?x ?y ?z)))

    (<= (delete (r ?x ?y ?z)) (deletion (m ?x ?y ?z)) (not (n ?x ?y ?z)))
    (<= (delete (r ?x ?y ?z)) (deletion (n ?x ?y ?z)) (not (m ?x ?y ?z)))))

(define-theory *factserver* ""
  '((m a b c)
    (n a b c)
    (r a b c)))

;;; (revise '((m a c c) (not (n a b c))) *updater*)
;;; DONE
;;;
;;; (show 'r *factserver*)
;;; ((R A C C))
;;;
;;; (revise '((m a c c) (not (m a b c)) (not (n a b c))) *updater*)
;;; DONE
;;;
;;; (show 'r *factserver*)
;;; ((NOT (R A B C)) (R A C C))

(define-theory *ruleserver* ""
  '((<= (s ?x ?z) (m ?x ?y) (n ?y ?z))

    (<= (insert (s ?x ?z)) (insertion (m ?x ?y)) (n ?y ?z))
    (<= (insert (s ?x ?z)) (insertion (n ?y ?z)) (m ?x ?y))

    (<= (delete (s ?x ?z)) (deletion (m ?x ?y)) (s ?x ?z) (unprovable (and (m ?x ?w) (n ?w ?z))))
    (<= (delete (s ?x ?z)) (deletion (n ?y ?z)) (s ?x ?z) (unprovable (and (m ?x ?w) (n ?w ?z))))))

(define-theory *factserver* ""
  '((m a b)
    (n b c)
    (s a c)))

;;; (revise '((n b d) (not (n b c))) *updater*)
;;; DONE
;;;
;;; (show 's *factserver*)
;;; ((NOT (S A C)) (S B D))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;  Test
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(definemore *manager*
  '((base integrator m)
    (base integrator n)
    (material integrator r)
    (specialty warehouse m)
    (specialty warehouse n)
    (interest  warehouse r)))

(definemore *library*
  '((=> (m ?x ?y) (n ?y ?z) (r ?x ?y ?z))
    (=> (n ?y ?z) (m ?x ?y) (r ?x ?y ?z))

    (=> (p ?x ?y) (m ?x ?y))
    (=> (not (p ?x ?y)) (not (m ?x ?y)))
    (=> (q ?x ?y) (n ?x ?y))
    (=> (not (q ?x ?y)) (not (n ?x ?y)))))

(definemore *warehouse*
  '((m b c)
    (n c d)
    (r b c d)))

;;; (request '(update (m a c) (n c e)) nil *integrator*)

;;; (request '(ask-all (?x ?y ?z) (r ?x ?y ?z)) nil *integrator*)
;;; ((a c d) (b c d) (b c e) (a c e))
|#
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; transformer
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; changeviews
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
#|
(defmethod oldchange (p (th transformer))
  (let (updates errors)
    (setq updates (changes p th))
    (dolist (item updates)
      (when (and (listp item) (eq (car item) 'error))
        (setq errors (cons (cadr item) errors))))
    (cond (errors (nreverse errors))
          (t (dolist (sentence updates) (assume sentence th))
             (sendem `(update . ,updates) th)))))

(defmethod oldchanges (p (th agent))
  (nreverse (computechanges p th nil)))

(defun oldcomputechanges (p th nl)
  (cond ((atom p) (adjoin p nl))
        ((eq '==> (car p))
         (dolist (item (finds (caddr p) (cadr p) th))
           (setq nl (contribute item nl)))
         nl)
        ((eq 'and (car p))
         (dolist (item (cdr p)) (setq nl (computechanges item th nl))) nl)
        (t (adjoin p nl :test #'equalp))))
|#
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; reduceviews
;;; start with simple case
;;; now do negatives
;;; then do computation of ground changes
;;; then carry out changes
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
#|
(defparameter *updater* nil)

(defmethod reduceviews (facts (updater transformer))
  (let (rulebase target (*intensions* nil))
    (setq rulebase (find-rulebase updater))
    (when (and rulebase (symbolp rulebase) (boundp rulebase))
      (setq rulebase (symbol-value rulebase)))
    (setq target (find-target updater))
    (when (and target (symbolp target) (boundp target))
      (setq target (symbol-value target)))
    (residues t (maksand facts) rulebase #'basicp)))

(defmethod reduceviews (facts (updater transformer))
  (let (rulebase (*intensions* nil))
    (setq rulebase (find-rulebase updater))
    (when (and rulebase (symbolp rulebase) (boundp rulebase))
      (setq rulebase (symbol-value rulebase)))
    (newconsequences (maksand facts) rulebase #'failure #'basicp)))
|#
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; changeviews
;;; start with simple case
;;; now do negatives
;;; then do computation of ground changes
;;; then carry out changes
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
#|
(defun newertellemall (&rest rules)
  (let (tells)
    (do ((l rules (cdr l)))
        ((null l))
        (cond ((atom (car l)))
              ((eq 'tell (caar l)) (setq tells (cons (car l) tells)))
              ((eq '==> (caar l))
               (dolist (answer (askemall (caddar l) (cadar l)))
                 (setq tells (contribute answer tells))))))
    (setq tells (coalesce tells))
    (do ((l tells (cdr l)) (nl) (dum))
        ((null l) 'done)
        (setq dum (request `(tell ,(maksand (cdar l))) *receiver* (caar l)))
        (cond ((stringp dum)
               (do ((m nl (cdr m)) (untell))
                   ((null m))
                   (setq untell (maksand (nreverse (mapcar #'maknot (cdar m)))))
                   (request `(tell ,untell) *receiver* (caar m)))
               (return dum))
              (t (setq nl (cons (car l) nl)))))))
|#
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;  Old stuff
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#|
(defmethod changeviews (p (th transformer))
  (let (updates errors)
    (setq updates (computeviewchanges p th))
    (dolist (item updates)
      (when (and (listp item) (eq (car item) 'error))
        (setq errors (cons (cadr item) errors))))
    (cond (errors (nreverse errors))
          (t (dolist (sentence updates) (assume sentence th))
             (sendem `(update . ,updates) th)))))

(defmethod computeviewchanges (facts (*receiver* transformer))
  (let (rulebase (*intensions* nil))
    (setq rulebase (find-rulebase *receiver*))
    (when (and rulebase (symbolp rulebase) (boundp rulebase))
      (setq rulebase (symbol-value rulebase)))
    (do ((l (newconsequences (maksand facts) rulebase #'basicp #'materialp) (cdr l)) (nl))
        ((null l) (nreverse nl))
        (cond ((and (listp (car l)) (eq 'execute (caar l)))
               (setq nl (cons (cadar l) nl)))
              ((and (listp (car l)) (eq 'evaluate (caar l)))
               (setq nl (cons `(apply ',(caadar l) ',(cdadar l)) nl)))
              ((and (listp (car l)) (eq '=> (caar l)))
               (setq nl (cons (decolonize `(==> ,(cadar l) ,(caddar l))) nl)))
              (t (setq nl (cons (decolonize (car l)) nl)))))))

(defun computechangerules (rel th *updater*)
  (let (arity pattern (*intensions* nil))
    (setq arity (find-arity rel))
    (do ((i 1 (1+ i)) (nl))
        ((> i arity) (setq pattern (cons rel (nreverse nl))))
        (setq nl (cons (decolonize (newindvar)) nl)))
    (do ((l (newconsequences pattern th #'basicp #'materialp) (cdr l)) (nl))
        ((null l) (nreverse nl))
        (cond ((and (listp (car l)) (eq 'execute (caar l)))
               (setq nl (cons (cadar l) nl)))
              ((and (listp (car l)) (eq 'evaluate (caar l)))
               (setq nl (cons `(apply ',(caadar l) ',(cdadar l)) nl)))
              ((and (listp (car l)) (eq '=> (caar l)))
               (setq nl (cons (decolonize `(==> ,(cadar l) ,(caddar l))) nl)))
              (t (setq nl (cons (decolonize (car l)) nl)))))))

(defmethod changeviews (p (th transformer))
  (let (updates errors)
    (setq updates (computeviewchanges p th))
    (dolist (item updates)
      (when (and (listp item) (eq (car item) 'error))
        (setq errors (cons (cadr item) errors))))
    (cond (errors (nreverse errors))
          (t (dolist (sentence updates) (assume sentence th))
             (sendem `(update . ,updates) th)))))
|#

(defun getdeletionsnew (r bases positives negatives th)
  (do ((l (getnegatives r th) (cdr l)) (nl))
      ((null l) (nreverse nl))
      (do ((m (cddar l) (cdr m)) (om) (new))
          ((null m))
          (cond ((atom (car m)))
                ((find (caar m) bases)
                 (setq new `(oneof ,(car m) . ,(getdata (caar m) positives)))
                 (setq new `(<= ,(cadar l) ,new . ,(revappend om (cdr m))))
                 (setq nl (cons new nl)))
                ((and (eq (caar m) 'not) (find (caadar m) bases))
                 (setq new `(oneof ,(cadar m) . ,(getdata (caadar m) negatives)))
                 (setq new `(<= ,(cadar l) ,new . ,(revappend om (cdr m))))
                 (setq nl (cons new nl))))
          (setq om (cons (car m) om)))))

(defun getinsertions (r bases facts th)
  (do ((l (getpositives r th) (cdr l)) (nl))
      ((null l) (nreverse nl))
      (do ((m (cddar l) (cdr m)) (om) (new))
          ((null m))
          (cond ((atom (car m)))
                ((find (caar m) bases)
                 (setq new `(oneof ,(car m) . ,(getdata (caar m) facts)))
                 (setq new `(<= ,(cadar l) ,new . ,(revappend om (cdr m))))
                 (setq nl (cons new nl))))
          (setq om (cons (car m) om)))))

(defun getdeletions (r bases facts th)
  (do ((l (getnegatives r th) (cdr l)) (nl))
      ((null l) (nreverse nl))
      (do ((m (cddar l) (cdr m)) (om) (new))
          ((null m))
          (cond ((atom (car m)))
                ((and (eq (caar m) 'not) (find (caadar m) bases))
                 (setq new `(oneof ,(cadar m) . ,(getdata (caadar m) facts)))
                 (setq new `(<= ,(cadar l) ,new . ,(revappend om (cdr m))))
                 (setq nl (cons new nl))))
          (setq om (cons (car m) om)))))

(defun getpositives (r th)
  (normalizevars (reductions r th #'basicp #'success)))

(defun getnegatives (r th)
  (normalizevars (nonreductions r th #'basicp #'success)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Old transformer
;;; rats I edited over the original ramifyviewsold; hope i fixed it correctly
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod reviseold (facts (th transformer))
  (ramifyviewsold (reduceviewsold facts th) th))

(defun reduceviewsold (facts receiver)
  (let (rulebase (*intensions* nil))
    (setq rulebase (find-rulebase (name receiver)))
    (when (and rulebase (symbolp rulebase) (boundp rulebase))
      (setq rulebase (symbol-value rulebase)))
    (getconjuncts (residue t (maksand facts) rulebase #'basicp))))

(defun ramifyviewsold (facts th)
  (let (factserver ruleserver insertions deletions bases materials changes)
    (setq factserver (symbol-value (find-target (name th))))
    (setq ruleserver (symbol-value (find-rulebase (name th))))
    (multiple-value-setq (insertions deletions) (divide facts))
    (dolist (p facts) (setq bases (getbases p bases)))
    (dolist (r bases) (setq materials (getmaterials r ruleserver materials)))
    (dolist (r materials)
      (dolist (rule (getdeletionsold r bases deletions ruleserver))
        (setq changes (nreconc (request `(ask-all ,(cadr rule) ,(maksand (cddr rule))) nil factserver) changes))))
    (request `(update . ,facts) th factserver)
    (dolist (r materials)
      (dolist (rule (getinsertionsold r bases insertions ruleserver))
        (setq changes (nreconc (request `(ask-all ,(cadr rule) ,(maksand (cddr rule))) nil factserver) changes))))
    (request `(update . ,(nreverse (uniquify changes))) th factserver)))

(defun getinsertionsold (r relations facts th)
  (do ((l (getdefinitions r th) (cdr l)) (nl))
      ((null l) (nreverse nl))
      (do ((m (cddar l) (cdr m)) (om) (new))
          ((null m))
          (cond ((atom (car m)))
                ((find (caar m) relations)
                 (setq new `(oneof ,(car m) . ,(getdata (caar m) facts)))
                 (setq new `(<= ,(cadar l) ,new . ,(revappend om (cdr m))))
                 (setq nl (cons new nl))))
          (setq om (cons (car m) om)))))

(defun getdeletionsold (r relations facts th)
  (do ((l (getdefinitions r th) (cdr l)) (head) (nl))
      ((null l) (when nl (nreverse (modifyunion (list `(not ,head) '<=) (nreverse nl) nil))))
      (setq head (cadar l))
      (do ((m (cddar l) (cdr m)) (om) (new) (nm))
          ((null m) (setq nl (cons (nreverse (cons (makunprovable (maksand (cddar l))) nm)) nl)))
          (cond ((atom (car m)))
                ((find (caar m) relations)
                 (setq new `(oneof ,(car m) . ,(getdata (caar m) facts)))
                 (setq new (maksand (cons new (revappend om (cdr m)))))
                 (setq nm (cons new nm))))
          (setq om (cons (car m) om)))))

(defun modifyunion (cl xl nl)
  (cond ((null xl) (cons (reverse cl) nl))
        (t (dolist (x (car xl))
             (setq nl (modifyunion (cons x cl) (cdr xl) nl)))
           nl)))

(defun getdefinitions (r th)
  (normalizevars (reductions r th #'basicp #'success)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Old stuff just in case
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun process-search-display (s postlines)
  (let (structure class aspect kif slots objects start end count)
    (multiple-value-setq (structure postlines) (parsestructure postlines))
    (setq class (cadr structure))
    (setq aspect (car structure))
    (setq kif (maksand (reconverter structure)))
    (setq start (or (read-value-string (getf-post "Start" postlines)) 1))
    (setq end (or (read-value-string (getf-post "Solutions" postlines)) 20))
    (setq objects (request `(ask-all ,aspect ,kif) *client* *agent*))
    (multiple-value-setq (objects count start end) (trim objects start end))
    (setq slots (displayable-slots class))
    (output-prolog s 200)
    (output-header s "Result")
    (cond ((= count 0) (display-failure s aspect kif))
          ((and (= count 1) (= (length slots) 1) (uniquep (car slots)))
           (display-class s class slots objects count start end))
          ((>= (1+ (- end start)) count)
           (format s "<CENTER><TABLE><TR><TD>")
           (display-class s class slots objects count start end)
           (format s "</TD></TR><TR><TD>")
           (output-more-commands s class aspect kif nil nil count start end)
           (format s "</TD></TR></TABLE></CENTER>"))
          (t (format s "<CENTER><TABLE><TR><TD>")
             (display-class s class slots objects count start end)
             (format s "</TD></TR><TR><TD>")
             (output-more-commands s class aspect kif nil nil count start end)
             (format s "</TD></TR></TABLE></CENTER>")))
    (output-footer s)))

(defun process-searcher-display (s postlines)
  (let (structure class aspect kif slots objects start end count)
    (multiple-value-setq (structure postlines) (parseobject postlines))
    (setq class (cadr structure))
    (setq aspect (car structure))
    (setq kif (maksand (converter structure)))
    (setq start 1 end 20)
    (setq objects (request `(ask-all ,aspect ,kif) *client* *agent*))
    (multiple-value-setq (objects count start end) (trim objects start end))
    (setq slots (displayable-slots class))
    (output-prolog s 200)
    (output-header s "Result")
    (cond ((= count 0) (display-failure s aspect kif))
          ((and (= count 1) (= (length slots) 1) (uniquep (car slots)))
           (display-class s class slots objects count start end))
          ((>= (1+ (- end start)) count)
           (format s "<CENTER><TABLE><TR><TD>")
           (display-class s class slots objects count start end)
           (format s "</TD></TR><TR><TD>")
           (output-more-commands s class aspect kif nil nil count start end)
           (format s "</TD></TR></TABLE></CENTER>"))
          (t (format s "<CENTER><TABLE><TR><TD>")
             (display-class s class slots objects count start end)
             (format s "</TD></TR><TR><TD>")
             (output-more-commands s class aspect kif nil nil count start end)
             (format s "</TD></TR></TABLE></CENTER>")))
    (output-footer s)))

(defun count-frame (s *postlines*)
  (let (class aspect kif sorter order objects count (*var-count* 0))
    (output-prolog s 200)
    (output-header s "Result")
    (setq class (read-value-string (cdar *postlines*)))
    (setq aspect (first-aspect *postlines*))
    (setq kif (convert-frame))
    (when (setq sorter (convert-sorters))
      (setq order (cadar sorter) sorter (caar sorter)))
    (cond ((viewablep *client* (gensym))
           (setq count (request `(length (ask-all ,aspect ,kif)) *client* *agent*)))
          (t (setq objects (request `(ask-all ,aspect ,kif) *client* *agent*))
             (setq objects (remove-if-not #'(lambda (x) (viewablep *client* x)) objects))
             (setq count (length objects))))
    (cond ((or (not (numberp count)) (= count 0)) (display-failure s aspect kif))
          ((= count 1)
           (format s "<P>There is 1 viewable answer.<P>")
           (output-more-commands s class aspect kif sorter order count 0 0))
          (t (format s "<P>There are ~D viewable answers." count)
             (output-more-commands s class aspect kif sorter order count 0 0)))
    (output-footer s)))


(defun answer-frame (s *postlines*)
  (let (class slots aspect kif sorter order start end objects count (*var-count* 0))
    (output-prolog s 200)
    (output-header s "Result")
    (setq class (read-value-string (cdar *postlines*)))
    (setq aspect (first-aspect *postlines*))
    (setq kif (convert-frame))
    (when (setq sorter (convert-sorters))
      (setq order (cadar sorter) sorter (caar sorter)))
    (setq start (read-value-string (getf-post "Start" *postlines*)))
    (setq end (read-value-string (getf-post "Solutions" *postlines*)))
    (setq objects (request `(ask-all ,aspect ,kif) *client* *agent*))
    (unless (viewablep *client* (gensym))
      (setq objects (remove-if-not #'(lambda (x) (viewablep *client* x)) objects)))
    (multiple-value-setq (objects count start end) (trim objects start end))
    (setq slots (displayable-slots class))
    (cond ((= count 0) (display-failure s aspect kif))
          ((and (= count 1) (= (length slots) 1) (uniquep (car slots)))
           (display-class s class slots objects count start end))
          ((>= (1+ (- end start)) count)
           (format s "<CENTER><TABLE><TR><TD>")
           (display-class s class slots objects count start end)
           (format s "</TD></TR><TR><TD>")
           (output-more-commands s class aspect kif sorter order count start end)
           (format s "</TD></TR></TABLE></CENTER>"))
          (t (format s "<CENTER><TABLE><TR><TD>")
             (display-class s class slots objects count start end)
             (format s "</TD></TR><TR><TD>")
             (output-more-commands s class aspect kif sorter order count start end)
             (format s "</TD></TR></TABLE></CENTER>")))
    (output-footer s)))


(defun rulify-frame (s *postlines*)
  (let (aspect old kif class (*var-count* 0))
    (output-prolog s 200)
    (output-header s "Save Rule")
    (setq aspect (first-aspect *postlines*))
    (setq old (read-value-string (cdar *postlines*)))
    (setq kif (convert-frame))
    (setq class (read-value-string (getf-post "Class" *postlines*)))
    (cond ((not class) (format s "<P>Bad class name.<P>"))
          (t (dolist (rule (rules `(<= (isa ,aspect ,class) ,kif)))
               (save rule *library*))
             (save `(isa ,class class) *interface*)
             (dolist (slot (displayable-slots old))
               (save `(attribute ,class ,slot) *interface*))
             (save `(rootclass ,*gui* ,class) *interface*)
             (format s "<P>Done.<P>")))
    (output-footer s)))

(defun derulify-frame (s *postlines*)
  (let (aspect old kif class (*var-count* 0))
    (output-prolog s 200)
    (output-header s "Save Rule")
    (setq aspect (first-aspect *postlines*))
    (setq old (read-value-string (cdar *postlines*)))
    (setq kif (convert-frame))
    (setq class (read-value-string (cadr (getallvals "Class" *postlines*))))
    (cond ((not class) (format s "<P>Bad class name.<P>"))
          (t (dolist (rule (rules `(<= (isa ,aspect ,class) ,kif)))
               (drop rule *library*))
             (unless (or (classp class) (viewp class *library*))
               (drop `(isa ,class class) *interface*)
               (dolist (slot (displayable-slots old))
                 (drop `(attribute ,class ,slot) *interface*))
               (drop `(rootclass ,*gui* ,class) *interface*))
             (format s "<P>Done.<P>")))
    (output-footer s)))

(defun display-class (s class slots objects count start end)
  (cond ((and (= start 1) (geqp end count)))
        ((= count 1) (format s "<P>There is 1 viewable answer.<P>"))
        (t (format s "<P>There are ~D viewable answers  The following table shows answers ~A through ~A.<P>"
                   count start end)))
  (force-output s)
  (cond ((and (= count 1) slots (null (cdr slots)) (uniquep (car slots)))
         (show-lone-table s (first objects) (first slots)))
        ((find :rows *options*)
         (show-row-table s class objects slots))
        ((find :cols *options*)
         (show-col-table s objects slots))
        ((>= (length slots) (length objects)) (show-col-table s objects slots))
        (t (show-row-table s class objects slots))))

(defun show-lone-table (s object slot)
  (declare (type stream s))
  (declare (type symbol object slot))
  (let (html)
    (setq html (request `(ask-one ?html (,slot ,object ?html)) *client* *agent*))
    (format s "<CENTER>")
    (when (stringp (cadr html)) (format s (cadr html)))
    (format s "</CENTER><P>")))

(defun show-row-table (s class items slots)
  (let ((results (request `(ask-table ,items ,slots) *client* *agent*))
        (nohandle (findp `(nodisplay ,class handle) *interface*))
        (nobasket (or (null items) (findp `(nocommand ,*gui* save) *interface*))))
    (format s "<FORM ACTION=SAVE? METHOD=POST>")
    (format s "<CENTER><TABLE BGCOLOR=WHITE BORDER>")
    (format s "<TR>")
    (unless nobasket (format s "<TH></TH>"))
    (unless nohandle (format s "<TH>ID</TH>"))
    (dolist (slot slots)
      (format s "<TH>")
      (output-slotlink s slot)
      (format s "</TH>"))
    (format s "</TR>")
    (do ((l items (cdr l)) (m results (cdr m)) (flag nil (not flag)))
        ((null l))
        (if flag (format s "<TR>") (format s "<TR BGCOLOR=#EEEEEE>"))
        (unless nobasket
          (format s "<TH>")
          (format-checkbox s (prettify (car l)) "" t)
          (format s "</TH>"))
        (unless nohandle
          (format s "<TH ALIGN=LEFT>")
          (output-value s (car l))  ;;; (output-view s (car l) class)
          (format s "</TH>"))
        (loop for vals in (car m)
              ;;; for class in classes
	      do (setq vals (remove 'unknown vals))
              (if (every #'(lambda (val) (numberp val)) vals)
                (format s "<TD ALIGN=RIGHT>")
                (format s "<TD>") )
              (loop
                for val in vals
                for first-time = t then nil
                unless first-time
                do (format s ", ")
                do (output-value s val))   ;;; (output-view s val class)
              (format s "<BR></TD>"))
        (format s "</TR>")
        (crlf s))
    (format s "</TABLE></CENTER>")
    (force-output s)
    (unless nobasket
      (format s "<P>")
      (format-button s "Command" "Save")
      (format s "selected results in class")
      (format-text s "Class" "" 40)
      (format s "<P>")
      (format-button s "Command" "Drop")
      (format s "selected results from class")
      (format-text s "Class" "" 40)
      (format-hidden s "Items" (format nil "~S" items))
      (format-hidden s "Slots" (format nil "~S" slots))
      (format-hidden s "Results" (htmlify (format nil "~S" results))))
    (format s "</FORM>")))

(defun show-col-table (s items slots)
  (format s "<CENTER><TABLE BGCOLOR=WHITE BORDER>")
  (loop
      initially (format s "<TR><TH>ID</TH>")
      for item in items
      do (format s "<TH><FONT SIZE=-1><A HREF=\"INSPECT?Object=~A\">~A</A></FONT></TH>"
		 (addressify item) (prettify item))
      finally (format s "</TR>"))
  (loop
      with data = (invert-table 
                   (request `(ask-table ,items ,slots) *client* *agent*))
      for slot in slots
      do (format s "<TR><TD>")
         (output-slotlink s slot)
         (format s "</TD>")
         (loop
	    for vals in (car data)
	    do (setq vals (remove 'unknown vals))
	       (if (every #'numberp vals)
		   (format s "<TD ALIGN=\"RIGHT\">")
		   (format s "<TD>") )
	       (loop
		   for val in vals
		   for first-time = t then nil
		   unless first-time
		   do (format s ", ")
		   do (output-value s val))
	       (format s "<BR></TD>"))
         (setq data (cdr data))
	 (format s "</TR>"))
  (format s "</TABLE></CENTER>"))      

(defun display-cost (s)
  (declare (special *account*))
  (format s "<HR>~%")
  (format s "The cost to answer this query: ")
  (if (and (boundp '*account*)
	   (integerp *account*) )
      (format s "$~D.~D <EM>Fee waived</EM>"
	      (truncate (/ *account* 100))
	      (rem *account* 100) )
    (format s "<EM>Unknown</EM>") )
  (format s "<BR>~2%") )

(defun make-pretty-variable (var)
  (unless (symbolp var)
    (return-from make-pretty-variable (make-pretty-string var)) )
  (let ((var-name (symbol-name var)))
    (cond ((string= var-name "") "")
          ((eql (elt var-name 0) #\?)
           (if (ignore-errors (eql (elt var-name 1) #\_))
	     (format nil "<EM>~A</EM>" (make-pretty-string (subseq var-name 2)))
	     (make-pretty-string (subseq var-name 1)) ))
          (t (make-pretty-string var-name)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; convert-frame
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun convert-frame ()
  (do ((class (read-value-string (cdr (pop *postlines*))))
       (predicate (read-value-string (cdr (pop *postlines*))))
       (aspect (read-value-string (cdr (pop *postlines*))))
       (dum) (nl))
      ((or (null *postlines*) (string= (caar *postlines*) "Concept-End"))
       (pop *postlines*)
       (setq nl (cons (makpredicate predicate aspect class) nl))
       (if (rest nl) (cons 'and (nreverse nl)) (car nl)))
      (cond ((string= (caar *postlines*) "Cell-Start")
             (if (setq dum (convert-cell aspect)) (setq nl (cons dum nl))))
            (t (pop *postlines*)))))

(defun convert-cell (aspect)
  (let ((type (cdadr *postlines*)))
    (cond ((string= type "Typein") (convert-typein aspect))
          ((string= type "MultiChoiceList") (convert-menu aspect))
          ((string= type "DropdownList") (convert-menu aspect))
          ((string= type "Multiselector") (convert-multiselector aspect))
          ((string= type "Checkbox") (convert-checkbox aspect))
          ((string= type "Radiobutton") (convert-checkbox aspect))
          ((string= type "Interval") (convert-interval aspect))
          ((string= type "Intermenu") (convert-intermenu aspect))
          ((string= type "Stringfield") (convert-stringfield aspect))
          ((string= type "Text") (convert-text aspect))
          ((string= type "Textarea") (convert-text aspect))
          ((string= type "Password") (convert-text aspect))
          ((string= type "Subframe") (convert-subframe aspect))
          (t (convert-other aspect)))))

(defun convert-typein (aspect)
  (let (slot value)
    (pop *postlines*)
    (pop *postlines*)
    (setq slot (read-value-string (cdr (pop *postlines*))))
    (setq value (cdr (pop *postlines*)))
    (pop *postlines*)
    (cond ((string= value "") nil)
          ((string-equal value "None") `(unprovable ,(list slot aspect '?*)))
          (t (list slot aspect (readas value (find-range slot)))))))

(defun convert-menu (aspect)
  (let (name)
    (setq name (cdr (pop *postlines*)))
    (pop *postlines*)
    (do ((slot (read-value-string (cdr (pop *postlines*)))) (value) (nl))
        ((or (null *postlines*) (not (string= (caar *postlines*) name)))
         (pop *postlines*)
         (if (null (cdr nl)) (car nl) (cons 'or (nreverse nl))))
        (setq value (cdr (pop *postlines*)))
        (cond ((string= value ""))
              ((string= value "None")
               (setq nl (cons `(unprovable ,(list slot aspect '?*)) nl)))
              (t (setq nl (cons (list slot aspect (read-value-string value)) nl)))))))

(defun convert-multiselector (aspect)
  (let (slot references value)
    (pop *postlines*)
    (pop *postlines*)
    (setq slot (read-value-string (cdr (pop *postlines*))))
    (multiple-value-setq (references *postlines*) (getvalues "Reference" *postlines*))
    (setq value (cdr (pop *postlines*)))
    (cond ((string= value "") nil)
          ((string= value "None") `(unprovable ,(list slot aspect '?*)))
          (t (raze slot aspect (read-value-string value) references)))))

(defun convert-checkbox (aspect)
  (let (name)
    (setq name (cdr (pop *postlines*)))
    (pop *postlines*)
    (do ((slot (read-value-string (cdr (pop *postlines*)))) (value) (nl))
        ((or (null *postlines*) (not (string-equal (caar *postlines*) name)))
         (pop *postlines*)
         (if (null (cdr nl)) (car nl) (cons 'and (nreverse nl))))
        (setq value (cdr (pop *postlines*)))
        (cond ((string= value ""))
              ((string= value "None")
               (setq nl (cons `(unprovable ,(list slot aspect '?*)) nl)))
              (t (setq nl (cons (list slot aspect (read-value-string value)) nl)))))))

(defun convert-interval (aspect)
  (let (slot min max type)
    (pop *postlines*)
    (pop *postlines*)
    (setq slot (read-value-string (cdr (pop *postlines*))))
    (setq type (find-range slot))
    (setq min (readas (cdr (pop *postlines*)) type))
    (setq max (readas (cdr (pop *postlines*)) type))
    (makcomparator aspect slot min max (if (eq type 'number) '=< 'leq))))

(defun convert-intermenu (aspect)
  (let ((slot) (min) (max) (var))
    (pop *postlines*)
    (pop *postlines*)
    (setq slot (read-value-string (cdr (pop *postlines*))))
    (setq min (read-value-string (cdr (pop *postlines*))))
    (setq max (read-value-string (cdr (pop *postlines*))))
    (cond ((and min max)
           (setq var (genvar))
	   `(and (,slot ,aspect ,var) (leq ,min ,var) (leq ,var ,max)))
          (min
           (setq var (genvar))
           `(and (,slot ,aspect ,var) (leq ,min ,var)))
          (max
           (setq var (genvar))
           `(and (,slot ,aspect ,var) (leq ,var ,max))))))

(defun convert-stringfield (aspect)
  (let ((slot) (value) (match) (var))
    (pop *postlines*)
    (pop *postlines*)
    (setq slot (read-value-string (cdr (pop *postlines*))))
    (setq value (cdr (pop *postlines*)))
    (unless (string= (caar *postlines*) "Cell-End")
      (setq match (cdr (pop *postlines*))))
    (pop *postlines*)
    (cond ((string= value "") nil)
          ((string= match "Exact?") `(,slot ,aspect ,value))
          (t (setq var (genvar))
             `(and (,slot ,aspect ,var) (substring ,value ,var))))))

(defun convert-text (aspect)
  (let ((slot) (value) (match) (var (genvar)))
    (pop *postlines*)
    (pop *postlines*)
    (setq slot (read-value-string (cdr (pop *postlines*))))
    (setq value (cdr (pop *postlines*)))
    (setq match (cdr (pop *postlines*)))
    (pop *postlines*)
    (cond ((string= value "") nil)
          ((string= match "All Words")
           `(and (,slot ,aspect ,var) (strmatchall ,value ,var)))
          ((string= match "Any Word")
           `(and (,slot ,aspect ,var) (strmatchany ,value ,var)))
          ((string= match "Phrase")
           `(and (,slot ,aspect ,var) (strmatchphrase ,value ,var))))))

(defun convert-subframe (aspect)
  (let (slot value constraint)
    (pop *postlines*)
    (pop *postlines*)
    (setq slot (read-value-string (cdr (pop *postlines*))))
    (setq value (read-value-string (cdaddr *postlines*)))
    (setq constraint (convert-frame))
    (pop *postlines*)
    (if (eq 'and (car constraint))
        `(and (,slot ,aspect ,value) . ,(cdr constraint)))))

(defun convert-other (aspect)
  (declare (ignore aspect))
  (do ((pair (pop *postlines*) (pop *postlines*)))
      ((or (null pair) (string= (car pair) "Cell-End")) nil)))

(defun convert-sorters ()
  (do ((sorter) (order) (nl))
      ((or (null *postlines*) (not (string= (caar *postlines*) "Sorter")))
       (nreverse nl))
      (setq sorter (read-value-string (cdr (pop *postlines*))))
      (setq order (read-value-string (cdr (pop *postlines*))))
      (when sorter (setq nl (cons (list sorter order) nl)))))

(defun transform-frame (s *postlines*)
  (let ((*var-count* 0))
    (output-prolog s 200)
    (output-header s "QBE Query")
    (format s "<FORM ACTION=TABLE? METHOD=POST>")
    (format s "Find ")
    (format-text s "Variables" (cdaddr *postlines*) 32)
    (format s " such that<BR><P>~%")
    (format-hidden s "Begingroup" "And")
    (generate-table s (convert-frame))
    (format-hidden s "Endgroup" "")
    (format s "<P>")
    (reissue-table-commands s *postlines*)
    (format s "</FORM>")
    (output-footer s)))

(defun sql-frame (s *postlines*)
  (let (aspect kif (*var-count* 0))
    (output-prolog s 200)
    (output-header s "SQL Query")
    (setq aspect (read-from-string (getf-post "Aspect" *postlines*)))
    (setq kif (convert-frame))
    (format s "<FORM ACTION=SQL? METHOD=POST>~%")
    (format s "<TEXTAREA NAME=Message ROWS=10 COLS=70>")
    (format s "~A" (sql `(ask-all ,aspect ,kif)))
    (format s "</TEXTAREA><P>~%")
    (format s "<HR>~%")
    (format s "<P><INPUT TYPE=SUBMIT VALUE=Submit> this SQL request")
    (format s "</FORM>")
    (format s "<P>")
    (output-footer s)))

(defun acl-frame (s *postlines*)
  (let (aspect kif sorter acl (*var-count* 0))
    (setq aspect (read-from-string (getf-post "Aspect" *postlines*)))
    (setq kif (convert-frame))
    (setq sorter (convert-sorters))
    (setq acl `(ask-all ,aspect ,kif))
    (when sorter (setq acl `(sortem ,acl ',(caar sorter) ',(cadar sorter))))
    (generate-acl s acl)))

(defun process-save (s postlines)
  (let (items results selections)
    (output-prolog s 200)
    (output-header s "Save")
    (multiple-value-setq (selections postlines) (getslots "Class" postlines))
    (setq selections (mapcar #'read-value-string selections))
    (setq items (read-user-string (getf-post "Items" postlines)))
    (setq results (read-user-string (getf-post "Slots" postlines)))
    (do ((l items (cdr l)) (rl results (cdr rl)))
        ((null l))
        (when (equal (car l) (car selections))
          (setq selections (cdr selections))
          (prorequest `(tell (not (bad ,*client* ,(car l)))))
          (prorequest `(tell (good ,*client* ,(car l))))))
    (format s "<P>Done.<P>")
    (output-footer s)))

(defun process-drop (s postlines)
  (let (items results selections)
    (output-prolog s 200)
    (output-header s "Save")
    (multiple-value-setq (selections postlines) (getslots "Class" postlines))
    (setq selections (mapcar #'read-value-string selections))
    (setq items (read-user-string (getf-post "Items" postlines)))
    (setq results (read-user-string (getf-post "Slots" postlines)))
    (do ((l items (cdr l)) (rl results (cdr rl)))
        ((null l))
        (when (equal (car l) (car selections))
          (setq selections (cdr selections))
          (prorequest `(tell (not (good ,*client* ,(car l)))))
          (prorequest `(tell (bad ,*client* ,(car l))))))
    (format s "<P>Done.<P>")
    (output-footer s)))

(defun reissue-create-cell (s expname postlines)
  (let ((cellname (cdar postlines))
        (style (read-value-string (cdadr postlines))))
    (cond ((eq style 'multichoicelist) (reissue-create-menu s postlines))
          ((eq style 'dropdownlist) (reissue-create-selector s postlines))
          ((eq style 'multiselector)
           (cond ((string= cellname expname) (expand-create-multiselector s postlines))
                 (t (reissue-create-multiselector s postlines))))
          ((eq style 'checkbox) (reissue-create-checkbox s postlines))
          ((eq style 'radiobutton) (reissue-create-checkbox s postlines))
          ((eq style 'stringfield) (reissue-create-text s postlines))
          ((eq style 'text) (reissue-create-text s postlines))
          ((eq style 'textarea) (reissue-create-textarea s postlines))
          ((eq style 'password) (reissue-create-password s postlines))
          (t (reissue-create-typein s postlines)))))

(defun reissue-create-menu (s postlines)
  (let (cell slot values)
    (setq cell (read-value-string (cdr (pop postlines))))
    (pop postlines)
    (setq slot (read-value-string (cdr (pop postlines))))
    (multiple-value-setq (values postlines) (getvalues (stringify cell) postlines))
    (pop postlines)
    (format-hidden s "Start" (stringify cell))
    (format-hidden s "Style" "MultiChoiceList")
    (format-hidden s "Slot" (stringify slot))
    (format s "<TR><TH ALIGN=LEFT VALIGN=TOP>")
    (output-slotlink s slot)
    (format s "</TH><TD VALIGN=TOP>")
    (output-slot-button s slot)
    (format s "</TD><TD VALIGN=TOP>")
    (output-create-button s slot)
    (format s "</TD><TD>")
    (output-menu s cell (find-values slot) values)
    (format s "</TD><TD VALIGN=TOP>")
    (format s (prettify (find-updatelabel slot)))
    (format s "</TD></TR>")
    (format-hidden s "End" "") (crlf s)
    postlines))

(defun reissue-create-selector (s postlines)
  (let (cell slot values)
    (setq cell (read-value-string (cdr (pop postlines))))
    (pop postlines)
    (setq slot (read-value-string (cdr (pop postlines))))
    (multiple-value-setq (values postlines) (getvalues (stringify cell) postlines))
    (pop postlines)
    (format-hidden s "Start" (stringify cell))
    (format-hidden s "Style" "DropdownList")
    (format-hidden s "Slot" (stringify slot))
    (format s "<TR><TH ALIGN=LEFT VALIGN=TOP>")
    (output-slotlink s slot)
    (format s "</TH><TD VALIGN=TOP>")
    (output-slot-button s slot)
    (format s "</TD><TD VALIGN=TOP>")
    (output-create-button s slot)
    (format s "</TD><TD>")
    (output-selector s cell (find-alternatives slot) (car values))
    (format s "</TD><TD>")
    (format s (prettify (find-updatelabel slot)))
    (format s "</TD></TR>")
    (format-hidden s "End" "") (crlf s)
    postlines))

(defun reissue-create-multiselector (s postlines)
  (let (cell slot references options value)
    (setq cell (read-value-string (cdr (pop postlines))))
    (pop postlines)
    (setq slot (read-value-string (cdr (pop postlines))))
    (multiple-value-setq (references postlines) (getvalues "Reference" postlines))
    (setq value (or (read-value-string (cdr (pop postlines))) 'unknown))
    (pop postlines)
    (cond ((null references) (setq options (find-options slot)))
          (t (setq options (find-components slot (car (last references)) references))))
    (format-hidden s "Start" (stringify cell))
    (format-hidden s "Style" "Multiselector")
    (format-hidden s "Slot" (stringify slot))
    (dolist (ref references) (format-hidden s "Reference" (stringify ref)))
    (format s "<TR><TH ALIGN=LEFT>")
    (output-slotlink s slot)
    (format s "</TH><TD>")
    (output-slot-button s slot)
    (format s "</TD><TD>")
    (output-create-button s slot)
    (format s "</TD><TD>")
    (output-selector s cell (nconc references (cons 'unknown options)) value)
    (format-button s (strappend "Expand." (stringify cell)) "Expand")
    (format s "</TD><TD>")
    (format s (prettify (find-updatelabel slot)))
    (format s "</TD></TR>")
    (format-hidden s "End" "") (crlf s)
    postlines))

(defun expand-create-multiselector (s postlines)
  (let (cell slot references options value)
    (setq cell (read-value-string (cdr (pop postlines))))
    (pop postlines)
    (setq slot (read-value-string (cdr (pop postlines))))
    (multiple-value-setq (references postlines) (getvalues "Reference" postlines))
    (setq value (read-value-string (cdr (pop postlines))))
    (unless (string= (caar postlines) "End") (pop postlines))
    (pop postlines)
    (cond (value (setq references (newrefs value references))
                 (setq options (find-components slot value references)))
          (t (setq references nil options (find-options slot))))
    (format-hidden s "Start" (stringify cell))
    (format-hidden s "Style" "Multiselector")
    (format-hidden s "Slot" (stringify slot))
    (dolist (ref references) (format-hidden s "Reference" (stringify ref)))
    (format s "<TR><TH ALIGN=LEFT>")
    (output-slotlink s slot)
    (format s "</TH><TD>")
    (output-slot-button s slot)
    (format s "</TD><TD>")
    (output-create-button s slot)
    (format s "</TD><TD>")
    (output-selector s cell (nconc references (cons 'unknown options)) 'unknown)
    (format-button s (strappend "Expand." (stringify cell)) "Expand")
    (format s "</TD><TD>")
    (format s (prettify (find-updatelabel slot)))
    (format s "</TD></TR>")
    (format-hidden s "End" "") (crlf s)
    postlines))

(defun reissue-create-checkbox (s postlines)
  (let (cell slot values)
    (setq cell (read-value-string (cdr (pop postlines))))
    (pop postlines)
    (setq slot (read-value-string (cdr (pop postlines))))
    (multiple-value-setq (values postlines) (getvalues (stringify cell) postlines))
    (pop postlines)
    (format-hidden s "Start" (stringify cell))
    (format-hidden s "Style" "Checkbox")
    (format-hidden s "Slot" (stringify slot))
    (format s "<TR><TH ALIGN=LEFT VALIGN=TOP>")
    (output-slotlink s slot)
    (format s "</TH><TD VALIGN=TOP>")
    (output-slot-button s slot)
    (format s "</TD><TD VALIGN=TOP>")
    (output-create-button s slot)
    (format s "</TD><TD>")
    (if (uniquep slot)
      (output-radiobuttons s cell (find-values slot) (car values))
      (output-checkboxes s cell (find-values slot) values))
    (format s "</TD><TD VALIGN=TOP>")
    (format s (prettify (find-updatelabel slot)))
    (format s "</TD></TR>")
    (format-hidden s "End" "") (crlf s)
    postlines))

(defun reissue-create-text (s postlines)
  (let (cell slot value)
    (setq cell (read-value-string (cdr (pop postlines))))
    (pop postlines)
    (setq slot (read-value-string (cdr (pop postlines))))
    (setq value (cdr (pop postlines)))
    (pop postlines)
    (format-hidden s "Start" (stringify cell))
    (format-hidden s "Style" "Text")
    (format-hidden s "Slot" (stringify slot))
    (format s "<TR><TH ALIGN=LEFT>")
    (output-slotlink s slot)
    (format s "</TH><TD>")
    (output-slot-button s slot)
    (format s "</TD><TD>")
    (output-create-button s slot)
    (format s "</TD><TD>")
    (format-text s (stringify cell) (htmlify value) 40)
    (format s "</TD><TD>")
    (format s (prettify (find-updatelabel slot)))
    (format s "</TD></TR>")
    (format-hidden s "End" "") (crlf s)
    postlines))

(defun reissue-create-textarea (s postlines)
  (let (cell slot value)
    (setq cell (read-value-string (cdr (pop postlines))))
    (pop postlines)
    (setq slot (read-value-string (cdr (pop postlines))))
    (setq value (cdr (pop postlines)))
    (pop postlines)
    (format-hidden s "Start" (stringify cell))
    (format-hidden s "Style" "Textarea")
    (format-hidden s "Slot" (stringify slot))
    (format s "<TR><TH ALIGN=LEFT VALIGN=TOP>")
    (output-slotlink s slot)
    (format s "</TH><TD VALIGN=TOP>")
    (output-slot-button s slot)
    (format s "</TD><TD VALIGN=TOP>")
    (output-create-button s slot)
    (format s "</TD><TD>")
    (format-textarea s (stringify cell) (htmlify value) 8 60)
    (format s "</TD><TD VALIGN=TOP>")
    (format s (prettify (find-updatelabel slot)))
    (format s "</TD></TR>")
    (format-hidden s "End" "") (crlf s)
    postlines))

(defun reissue-create-password (s postlines)
  (let (cell slot value)
    (setq cell (read-value-string (cdr (pop postlines))))
    (pop postlines)
    (setq slot (read-value-string (cdr (pop postlines))))
    (setq value (cdr (pop postlines)))
    (pop postlines)
    (format-hidden s "Start" (stringify cell))
    (format-hidden s "Style" "Password")
    (format-hidden s "Slot" (stringify slot))
    (format s "<TR><TH ALIGN=LEFT>")
    (output-slotlink s slot)
    (format s "</TH><TD>")
    (output-slot-button s slot)
    (format s "</TD><TD>")
    (output-create-button s slot)
    (format s "</TD><TD>")
    (format-password s (stringify cell) (htmlify value))
    (format s "</TD><TD>")
    (format s (prettify (find-updatelabel slot)))
    (format s "</TD></TR>")
    (format-hidden s "End" "") (crlf s)
    postlines))

(defun reissue-create-typein (s postlines)
  (let (cell slot value)
    (setq cell (read-value-string (cdr (pop postlines))))
    (pop postlines)
    (setq slot (read-value-string (cdr (pop postlines))))
    (setq value (cdr (pop postlines)))
    (pop postlines)
    (format-hidden s "Start" (stringify cell))
    (format-hidden s "Style" "Typein")
    (format-hidden s "Slot" (stringify slot))
    (format s "<TR><TH ALIGN=LEFT>")
    (output-slotlink s slot)
    (format s "</TH><TD>")
    (output-slot-button s slot)
    (format s "</TD><TD>")
    (output-create-button s slot)
    (format s "</TD><TD>")
    (format-text s (stringify cell) (htmlify value) 40)
    (format s "</TD><TD>")
    (format s (prettify (find-updatelabel slot)))
    (format s "</TD></TR>")
    (format-hidden s "End" "") (crlf s)
    postlines))

(defun prettytime (time)
  (let ((hour (floor time 100)) (minute (mod time 100)))
    (cond ((<= hour 12) (format nil "~D:~2,'0D am" hour minute))
          (t (format nil "~D:~2,'0D pm" (- hour 12) minute)))))


(setf (get 'tableau 'basicval) 'make-tableau-from-symbol)

(defun make-tableau-from-symbol (p)
  (make-schedule-from-symbol (cadr p)))

(defun tableau (x) (make-schedule-from-symbol x))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; rooms
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun make-schedule-int-rooms (date resolution)
  (let (reserv-list); (start-time (hour-to-time 8)) (end-time (hour-to-time 21)))
    (setq reserv-list
      (asks '(?reserveid ?st ?et ?room)
            `(and (event.date ?reserveid ,date)
                  (event.classroom ?reserveid ?room)
                  (event.start ?reserveid ?st)
                  (event.end ?reserveid ?et))   ;(leq ,start-time ?et) (leq ?st ,end-time)
            *schedules* *integrator*))
    (main-gen-table-rooms date reserv-list resolution 8 21)))

(defun main-gen-table-rooms (week-of all-data-list resolution start-hour end-hour)
  (let (ids sts ets rooms (start-time (hour-to-time start-hour)))
    (setq ids (mapcar #'first all-data-list))
    (setq sts (mapcar #'second all-data-list))
    (setq sts
	(mapcar #'(lambda (time) (if (lessp time start-time) start-time time))
		sts))
    (setq sts (mapcar #'(lambda (st) (time-ceiling st resolution)) sts))
    (setq ets (mapcar #'third all-data-list))
    (setq ets (mapcar #'(lambda (et) (time-ceiling et resolution)) ets))
    (setq rooms (mapcar #'fourth all-data-list))
    (make-html-schedule-rooms week-of ids sts ets rooms resolution start-hour end-hour)))

(defun make-html-schedule-rooms (start-date ids sts ets rooms resolution start-hour end-hour)
  (let (classrooms)
    (setq classrooms (asks '?x '(classroom.instance ?x) *schedules* *warehouse*))
    (with-output-to-string (ns)
      (format ns "<HTML><TABLE BORDER>")
      (format ns "<TR>")
      (format ns "<TH>~A</TH>"   (format nil "~A<BR>~A/~A" 
                                         (nth 
                                          (day-of (month-of start-date) 
					          (date-of start-date) 
					          (year-of start-date))
                                          *short-day-names*)
                                         (month-of start-date) (date-of start-date)))
      (dolist (room classrooms)
        (format ns "<TH><A HREF=\"/INSPECT?Object=~A\">~A</A></TH>" 
                room (prettify room)))
      (format ns "</TR>")
      (loop
	with filled-box
	with last-time = 0
	for time in (all-times resolution start-hour end-hour)
	do (format ns "<TR>")
           (format ns "<TH>~A</TH>" (prettify time))
           (dolist (current-room  classrooms)
             (setq filled-box nil)
             (loop
               for id in ids
               for st in sts
               for et in ets
               for rm in rooms
               unless filled-box
               do
               (cond ((and (eq current-room rm)
                           (lessp last-time st)
                           (or (eq time st) (greaterp time st)))
                      (print-event ns id (schedule-span st et resolution))
                      (setq filled-box t))
                ((and (equal current-room rm) (lessp st time) (lessp time et))
                 (setq filled-box t))))
             (unless filled-box (format ns "<TD></TD>")))
           (format ns "</TR>")
           (setq last-time time))
      (format ns "</TABLE>"))))

(defun print-event (s handle span)
  (let (title email st et)
    (format s "<TD ALIGN=CENTER ROWSPAN=\"~A\">" span)
    ;; Title
    (setq title
      (or
       (prorequest `(ask-one ?desc (event.title ,handle ?desc)))
       "<EM>Title unknown</EM>" ))
    (format s "<A HREF=\"INSPECT?Object=~A\">~A</A>" handle title)
    
    ;; Time
    (setq st (prorequest `(ask-one ?t (event.start ,handle ?t))))
    (setq et (prorequest `(ask-one ?t (event.end ,handle ?t))))
    (when (and st et)
      (setq st (nbspify (prettify st)))
      (setq et (nbspify (prettify et)))
      (format s "<BR><FONT SIZE=-1>")
      (format s "~A - ~A" st et)
      (format s "</FONT>") )

    ;; Email
    (setq email (prorequest `(ask-one ?e (owner ,handle ?e))))
    (when email
      (format s "<BR>")
      (format s "<EM><A HREF=\"/INSPECT?Object=~A\">" (subseq email 0 (position #\@ email)))
      (format s "~A</A></EM>" (subseq email 0 (position #\@ email))))
    (format s "</TD>") ))

(defun time-int-to-str (time)
  "NNNN (integer) to \"HH:MM am/pm\" (string)"
  (declare (type (integer 0 2500) time))
  (let ((hour (truncate (/ time 100)))
	(minute (mod time 100))
	suffix )
    (cond ((= hour 12) (setq suffix "noon"))
          ((or (= hour 0) (= hour 24))
           (setq hour 12)
           (setq suffix "mid") )
          ((< hour 12) (setq suffix "am"))
          (t (setq hour (- hour 12))
             (setq suffix "pm")))
    (if (= minute 0)
      (format nil "~D&nbsp;~A" hour suffix)
      (format nil "~D:~2,'0D" hour minute))))

(defun strip-slash (x)
  (delete #\/ x) )

(defun date-to-int (x)
  (parse-integer (strip-slash x)) )

(defun 31-days (x) 
  (if (member x '(1 3 5 7 8 10 12)) t))

(defun add-week (x) 
  (let ((date (date-of x)) (month (month-of x)) (year (year-of x)))
    (cond ((eq month 12) (add-week-12 x date))
          ((eq month 2) (add-week-2 x date year))
          ((31-days month) (add-week-31 x date month))
          (t (add-week-30 x date month)))))

(defun add-week-12 (x date)
  (let ((newdt (+ date 6)))
    (if (<= newdt 31) (+ x 60000) 
        (+ 1000000
	   (rem (+ (- x (* date 10000)) 1 (* 10000 (- newdt 31))) 1000000)))))

(defun add-week-2 (x date year)
  (let ((newdt (+ date 6)))
    (if (<= newdt 28)
	(+ x 60000)
      (if (leap-year? year) 
	  (if (<= newdt 29)
	      (+ x 60000)
	    (+ 3000000
	       (rem (+ (- x (* date 10000)) (* 10000 (- newdt 29))) 1000000) ))
	(+ 3000000
	   (rem (+ (- x (* date 10000)) (* 10000 (- newdt 28))) 1000000))))))

(defun add-week-30 (x date month)
  (declare (ignore month))
  (let ((newdt (+ date 6)))
    (if (<= newdt 30)
	(+ x 60000)
      (+ 1000000 (- x (* 10000 date)) (* 10000 (- newdt 30))))))

(defun add-week-31 (x date month)
  (declare (ignore month))
  (let ((newdt (+ date 6)))
    (if (<= newdt 31)
	(+ x 60000)
      (+ 1000000 (- x (* 10000 date)) (* 10000 (- newdt 31))))))

(defun make-schedule-from-symbol (symbol)
  (let (info pos building room date start end resolution interval)
    (setq info (subseq (symbol-name symbol) 9))

    ;; Building
    (setq pos (position #\- info))
    (setq building (subseq info 0 pos))
    (setq info (subseq info (1+ pos)))

    ;; Room
    (setq pos (position #\- info))
    (setq room (subseq info 0 pos))
    (setq info (subseq info (1+ pos)))

    ;; Week
    (setq pos (position #\- info))
    (setq date (intern (subseq info 0 pos)))
    (setq info (subseq info (1+ pos)))
 
    ;; Start hour
    (setq pos (position #\- info))
    (setq start (read-from-string (subseq info 0 pos)))
    (setq info (subseq info (1+ pos)))
    
    ;; End hour
    (setq pos (position #\- info))
    (setq end (read-from-string (subseq info 0 pos)))
    (setq info (subseq info (1+ pos)))

    ;; Resolution
    (setq pos (position #\- info))
    (setq resolution (read-from-string (subseq info 0 pos)))
    (setq info (subseq info (1+ pos)))

    ;; Interval
    (setq interval (intern info))
    (unless (eq interval 'week) (setq interval 'day))
    
    (if (string-equal building "all")
      (make-schedule-int-rooms date resolution)
      (make-schedule-int (intern (stringappend building "-" room)) date start end resolution interval))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; room
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun make-schedule-room (room start-date start-hour end-hour resolution interval)
  (let (reserv-list (start-time (hour-to-time start-hour)) (end-time (hour-to-time end-hour)) date-list)
    (setq reserv-list
      (asks '(?reserveid ?st ?et ?date)
            `(and (event.classroom ?reserveid ,room)
                  (event.date ?reserveid ?date)
                  (event.start ?reserveid ?st)
                  (event.end ?reserveid ?et))
            *schedules* *warehouse*))
    (setq date-list
	  (loop
	    for count from 0 to 6
	    collect (dateint-plus start-date count interval)))
   (setq reserv-list
	  (remove-if
	   #'(lambda (quad)
	       (or (not (find (fourth quad) date-list))
		   (lessp (third quad) start-time)
		   (lessp end-time (second quad)) ))
	   reserv-list))
    (main-gen-table room start-date reserv-list resolution start-hour end-hour interval)))

(defun main-gen-table (room week-of all-data-list resolution start-hour end-hour interval)
  (let (ids sts ets dates (start-time (hour-to-time start-hour)))
    (setq ids (mapcar #'first all-data-list))
    (setq sts (mapcar #'second all-data-list))
    (setq sts
	(mapcar #'(lambda (time) (if (lessp time start-time) start-time time))
		sts))
    (setq sts (mapcar #'(lambda (st) (time-ceiling st resolution)) sts))
    (setq ets (mapcar #'third all-data-list))
    (setq ets (mapcar #'(lambda (et) (time-ceiling et resolution)) ets))
    (setq dates (mapcar #'fourth all-data-list))
    (make-html-schedule room week-of ids sts ets dates resolution start-hour end-hour interval)))

(defun make-html-schedule (room start-date ids sts ets dates resolution start-hour end-hour interval)
  (with-output-to-string (ns)
    (format ns "<HTML><TABLE BORDER>")
    (format ns "<TR>")
    (format ns "<TH>~A</TH>" (prettify room))
    (loop
	for count from 0 to 6
	for dateint = (dateint-plus start-date count interval)
	for day = (day-of (month-of dateint) (date-of dateint) (year-of dateint))
	do (format ns "<TH WIDTH=\"12%\">~:(~A~)<BR>~A/~A</TH>" 
		   (nth day *short-day-names*) (month-of dateint) (date-of dateint)))
    (format ns "</TR>")
    (loop
	with filled-box
	with last-time = -1
	for time in (all-times resolution start-hour end-hour)
	do
	  (format ns "<TR>")
	  (format ns "<TH>~A</TH>" (prettify time))
	  (dotimes (day 7)
	    (setq filled-box nil)
	    (loop
		with dateint = (dateint-plus start-date day interval)
		for id in ids
		for st in sts
		for et in ets
		for date in dates
		unless filled-box
		do
		  (cond ((and (lessp last-time st)
                              (or (eq time st) (greaterp time st))
                              (eq dateint date))
		         (print-event ns id (schedule-span st et resolution))
		         (setq filled-box t))
		        ((and (eq dateint date) (lessp st time) (lessp time et))
		         (setq filled-box t))))
	    (unless filled-box (format ns "<TD></TD>")))
	  (format ns "</TR>") (crlf ns)
	  (setq last-time time))
    (format ns "</TABLE></HTML>")))

;;;;

#|

(defparameter *write-date-cache*
  (make-hash-table :size 1000))

(defun serve-document (stream pathname)
  (multiple-value-bind (dir name type) 
      (parse-url-components-into-pathname pathname)
    (let ((true-pathname
	   (make-pathname
	    :directory (cons :absolute dir)
	    :defaults ""
	    :name name
	    :type type)))
      (if (probe-file true-pathname)
	  (let ((namestring (namestring true-pathname)))
	      (with-open-file
		  (istream true-pathname :direction :input
			   :element-type 'unsigned-byte) 
		(let ((length (file-length istream))
		      (file-write-date 
		       (or (gethash namestring *write-date-cache*)
			   (let ((wd (file-write-date istream)))
			     (setf (gethash namestring *write-date-cache*) wd)
			     wd))))
		  (multiple-value-bind (major-type minor-type)
		      (content-type-from-file-type
		       (pathname-type true-pathname))
		    (output-prolog-detailed
		     stream 200 
		     :nocookie t
		     :content-type 
		     (list (string-downcase major-type)
			   (string-downcase minor-type))
		     :date (http-date-from-utime file-write-date)
		     :content-length length)
		    (loop for byte = (read-byte istream nil :eof)
			for count from 0
			until (eq :eof byte)
			do (let ((char (code-char byte)))
			     (write-char char stream)))
		    (force-output stream)
		    (close stream)))))
	  (progn (output-prolog stream 404)
		     (format stream "Document ~A not found." pathname))))))

(defun serve-document (stream pathname)
  (if (probe-file pathname)
      (with-open-file
        (istream pathname :direction :input :element-type 'unsigned-byte) 
        (let ((length (file-length istream))
              (file-write-date 
               (or (gethash pathname *write-date-cache*)
                   (let ((wd (file-write-date istream)))
                     (setf (gethash pathname *write-date-cache*) wd)
                     wd))))
          (multiple-value-bind (major-type minor-type)
                               (content-type-from-file-type (pathname-type pathname))
            (output-prolog-detailed
             stream 200 
             :nocookie t
             :content-type 
             (list (string-downcase major-type)
                   (string-downcase minor-type))
             :date (http-date-from-utime file-write-date)
             :content-length length)
            (loop for byte = (read-byte istream nil :eof)
                  for count from 0
                  until (eq :eof byte)
                  do (let ((char (code-char byte)))
                       (write-char char stream)))
            (finish-output stream)  ;;; (force-output stream)
            (close stream))))
      (progn (output-prolog stream 404)
             (format stream "Document ~A not found." pathname))))
|#

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Manifold
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass manifold (agent) ())

(defmethod create (obj (type (eql 'manifold)))
  (unless (and (boundp obj) (typep (symbol-value obj) 'manifold))
    (set obj (make-instance 'manifold :name obj))))

(defmethod destroy (obj (type (eql 'manifold)))
  (when (and (symbolp obj) (boundp obj) (typep (symbol-value obj) 'manifold))
    (makunbound obj)))

(defmethod message-handler (*message* *sender* (*receiver* manifold))
  (let (*library* *warehouse* *agents*)
    (setq *library* (get-rulebase *receiver*))
    (setq *warehouse* (get-database *receiver*))
    (setq *agents* (find-agents))
    (manipulate *message* *sender* *receiver*)))

(defun manipulate (msg sender receiver)
  (cond ((atom msg) msg)
        ((eq 'tell (car msg)) (modify (maksand (cdr msg)) sender receiver))
        ((eq 'untell (car msg)) (modify (maknot (cadr msg)) sender receiver))
        ((eq 'update (car msg)) (modify (maksand (cdr msg)) sender receiver))
        ((eq 'eliminate (car msg)) (discardall (maknot (cadr msg)) sender receiver))
        ((eq 'ask-if (car msg)) (askx t (cadr msg) sender receiver))
        ((eq 'ask-one (car msg)) (askx (cadr msg) (caddr msg) sender receiver))
        ((eq 'ask-all (car msg)) (asks (cadr msg) (caddr msg) sender receiver))
        ((eq 'ask-table (car msg)) (asktable (cadr msg) (caddr msg) sender receiver))
        ((eq 'ask-about (car msg)) (askabout (cadr msg) sender receiver))
        ((eq 'quote (car msg)) (cadr msg))
        ((macro-function (car msg)) (facilitate (macroexpand msg) sender receiver))
        ((fboundp (car msg))
         (apply (car msg) (mapcar #'(lambda (x) (facilitate x sender receiver)) (cdr msg))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod save (p (th manifold) &optional (f #'samep))
  (declare (ignore f))
  (cond ((atom p) (save p (find-target (name th))))
        ((eq (car p) '<=) (save p (get-rulebase th)))
        ((eq (car p) '=>) (save p (get-rulebase th)))
        (t (save p (find-target (name th))))))

(defmethod drop (p (th manifold) &optional (f #'samep))
  (declare (ignore f))
  (cond ((atom p) (drop p (find-target (name th))))
        ((eq (car p) '<=) (drop p (get-rulebase th)))
        ((eq (car p) '=>) (drop p (get-rulebase th)))
        (t (drop p (find-target (name th))))))

(defmethod pipe (structure source target sender receiver)
  (declare (ignore structure source target sender receiver))
  nil)

(defmethod pipe (structure source target sender (receiver symbol))
  (cond ((and (boundp receiver) (not (symbolp (symbol-value receiver))))
         (pipe structure source target sender (symbol-value receiver)))
        (t (call-next-method structure source target sender receiver))))

(defmethod pipe (structure source target sender (receiver manifold))
  (let (class aspect kif pipe rule old rulebase)
    (setq class (cadr structure))
    (setq aspect (car structure))
    (setq kif (reconverter structure))
    (setq pipe (gentemp "PIPE."))
    (setq rule `(<= (interested ,target ,aspect ,class) . ,kif))
    (setq rulebase (get-rulebase receiver))
    (setq old (findx '?x `(pipe.rule ?x ,rule) *manager*))
    (cond ((and old (truep rule rulebase)) old)
          (t (save `(isa ,pipe pipe) *manager*)
             (save `(pipe.source ,pipe ,source) *manager*)
             (save `(pipe.target ,pipe ,target) *manager*)
             (save `(pipe.subject ,pipe ,class) *manager*)
             (save `(pipe.owner ,pipe ,sender) *manager*)
             (save `(pipe.rule ,pipe ,rule) *manager*)
             (save rule (get-rulebase receiver))
             pipe))))


(defun process-inspectpage-pipe (s postlines)
  (let (structure target rule pipe)
    (setq structure (read-user-string (cdr (pop postlines))))
    (setq target (read-user-string (getf-post "Target" postlines)))
    (setq rule `(<= (interested ,target ,(car structure) ,(cadr structure))))
    (setq pipe (createpipe *gui* target (cadr structure) *client* rule))
    (save rule *gui*)
    (format-html s) (crlf s)
    (output-head s "Pipe") (crlf s)
    (format-body s *bgcolor*) (crlf s)
    (output-header s)
    (format s "<p>~A created.</p>" (prettify pipe))
    (output-footer s)
    (finish-body s) (crlf s)
    (finish-html s) (crlf s)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; *gamma* is used by fullassertions
;;; it needs to be specialized because of handling of knows in insert  
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod insert (p (th (eql *gamma*)))
  (cond ((atom p) (addcontent p th) (index p p th) p)
        ((eq 'knows (car p)) (save (caddr p) (symbol-value (cadr p))))
        ((and (eq 'not (car p)) (listp (cadr p)) (eq 'knows (caadr p)))
         (drop (caddr (cadr p)) (symbol-value (cadadr p))))
        (t (addcontent p th) (index p p th) p)))

(defmethod modify (p sender (receiver manifold))
  (let (rulebase updates conclusions retractions assertions)
    (setq rulebase (get-rulebase receiver))
    (setq updates (mapcar #'(lambda (x) (relativize x (name sender))) (get-updates p)))
    (setq conclusions (fullramifications (get-negatives updates) rulebase #'failure #'normalmodalp))
    (when (funcall *specialty* (operator p)) (drop p *theory*))
    (dolist (q (nreverse conclusions))
      (cond ((fullprovablep q rulebase))
            ((eq 'knows (car q)) (drop (caddr q) (cadr q)))
            ((eq 'exportable (car q)) (setq retractions (cons q retractions)))))
    (setq assertions (fullassertions (get-positives updates) *gamma* #'failure #'complexmodalp))
    (setq p '(exportable ?a ?p))
    (setq updates (append (trues `(not ,p) p retractions) (trues p p assertions)))
    (eval (assemble updates))))

(defun relativize (p sender)
  (cond ((atom p) `(importable ,sender ,p))
        ((eq (car p) 'not)  `(not (importable ,sender ,(cadr p))))
        (t `(importable ,sender ,p))))

(defun normalmodalp (x)
  (find x '(knows exportable)))

(defun complexmodalp (x)
  (find x '(knows importable exportable)))

(defun get-positives (facts)
  (do ((l facts (cdr l)) (nl))
      ((null l) (nreverse nl))
      (unless (and (listp (car l)) (eq (caar l) 'not)) (setq nl (cons (car l) nl)))))

(defun get-negatives (facts)
  (do ((l facts (cdr l)) (nl))
      ((null l) (nreverse nl))
      (when (and (listp (car l)) (eq (caar l) 'not)) (setq nl (cons (cadar l) nl)))))

(defun assemble (updates)
  (let (al)
    (dolist (p updates)
      (cond ((atom p))
            ((eq 'exportable (car p))
             (setq al (addupdate (cadr p) (caddr p) al)))
            ((and (eq (car p) 'not) (eq (caadr p) 'exportable))
             (setq al (addupdate (cadr (cadr p)) `(not ,(caddr (cadr p))) al)))))
    (do ((l al (cdr l)) (pl))
        ((null l) (maksprogn pl))
        (setq pl (cons `(ask ,(caar l) (update . ,(nreverse (cdar l)))) pl)))))

(defun addupdate (who what al)
  (let (dum)
    (cond ((setq dum (assoc who al))
           (rplacd dum (cons what (cdr dum)))
           al)
          (t (acons who (list what) al)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod askx (x p sender (receiver manifold))
  (declare (ignore sender))
  (let (dum)
    (when (setq dum (manifoldask x p receiver))
      (eval (route (makaskemone x dum))))))

(defmethod asks (x p sender (receiver manifold))
  (declare (ignore sender))
  (let (dum)
    (when (setq dum (manifoldask x p receiver))
      (eval (route (makaskemall x dum))))))

(defmethod asktable (ol sl sender (receiver manifold))
  (declare (ignore sender))
  (do ((l sl (cdr l)) (dum) (nl))
      ((null l) (invert-table (nreverse nl)))
      (setq dum (manifoldask '(?x ?y) `(and (oneof ?x . ,ol) (,(car l) ?x ?y)) receiver))
      (setq nl (cons (extract-values ol (eval (route (makaskemall '(?x ?y) dum)))) nl))))

(defmethod askabout (x sender (receiver manifold))
  (do ((l *agents* (cdr l)) (nl))
      ((null l) nl)
      (cond ((and (eq (car l) sender) (listp *message*)
                  (eq (car *message*) 'ask-about) (equalp (cadr *message*) x)))
            ((eq (car l) receiver))
            (t (setq nl (unionize nl (request `(ask-about ,x) receiver (car l))))))))

(defparameter *manifold* t)

(defun manifoldask (x p receiver)
  (if *manifold* (askplan1 x p receiver) (askplan2 x p receiver)))

(defun askplan1 (x p receiver)
  (let (library residues (*ancestry* 1))
    (setq library (get-rulebase receiver))
    (decludes 'infotheory)
    (empty 'infotheory)
    (includes 'infotheory library)
    (dolist (x (contrapositives `(<= (answer ,x) ,p))) (insert x 'infotheory))
    (when (setq residues (fullresidues `(answer ,x) 'infotheory #'sidelinep #'specialp))
      (setq p (decolonize (maksor residues)))
      (if *collapse* (setq p (collapse x p)))
      ;(if *collapse* (setq p (raisin x p)))
      (newsource p))))

(defun askplan2 (x p receiver)
  (let (library residues (*ancestry* 1))
    ;(setq p (modalize (name receiver) p))
    (setq library (get-rulebase receiver))
    (decludes 'infotheory)
    (empty 'infotheory)
    (includes 'infotheory library)
    (dolist (x (contrapositives `(<= (answer ,x) ,p))) (insert x 'infotheory))
    (when (setq residues (fullresidues `(answer ,x) 'infotheory #'failure #'knowsp))
      (setq p (decolonize (maksor residues)))
      (if *collapse* (setq p (collapse x p)))
      ;(if *collapse* (setq p (raisin x p)))
      (unmodalize p))))

(defun knowsp (x) (eq x 'knows))

(defun modalize (a p)
  (cond ((atom p) `(believes ,a ,p))
        ((eq (car p) 'not) `(not ,(modalize a (cadr p))))
        ((eq (car p) 'unprovable) `(not ,(modalize a (cadr p))))
        ((find (car p) '(and or))
         (cons (car p) (mapcar #'(lambda (x) (modalize a x)) (cdr p))))
        (t `(believes ,a ,p))))

(defun unmodalize (p)
  (cond ((atom p) p)
        ((eq 'knows (car p)) `(ask ,(cadr p) ,(caddr p)))
        ((eq 'not (car p))
         (setq p (unmodalize (cadr p)))
         (cond ((eq 'knows (car p)) `(ask ,(cadr p) (not ,(caddr p))))
               (t `(not ,p))))
        ((eq 'unprovable (car p))
         (setq p (unmodalize (cadr p)))
         (cond ((eq 'knows (car p)) `(ask ,(cadr p) (unprovable ,(caddr p))))
               (t `(unprovable ,p))))
        ((eq 'and (car p)) (unmodalizeand p))
        ((eq 'or (car p)) (unmodalizeor p))
        (t p)))

(defun unmodalize (p)
  (cond ((atom p) p)
        ((eq 'caches (car p)) `(ask ,(cadr p) ,(caddr p)))
        ((eq 'not (car p))
         (setq p (unmodalize (cadr p)))
         (cond ((eq 'caches (car p)) `(ask ,(cadr p) (not ,(caddr p))))
               (t `(not ,p))))
        ((eq 'unprovable (car p))
         (setq p (unmodalize (cadr p)))
         (cond ((eq 'caches (car p)) `(ask ,(cadr p) (unprovable ,(caddr p))))
               (t `(unprovable ,p))))
        ((eq 'and (car p)) (unmodalizeand p))
        ((eq 'or (car p)) (unmodalizeor p))
        (t p)))

(defun unmodalizeand (p)
  (do ((l (mapcar #'unmodalize (cdr p))) (source) (nl))
      ((null l) (if (null (cdr nl)) (car nl) (cons 'and (nreverse nl))))
      (cond ((not (eq 'ask (caar l))) (setq nl (cons (car l) nl) l (cdr l)))
            (t (setq source (cadar l))
               (do ((m (cdr l) (cdr m)) (nc (list (caddar l))))
                   ((null m)
                    (setq nl (cons `(ask ,source ,(maksand (nreverse nc))) nl))
                    (setq l nil))
                   (cond ((and (eq 'ask (caar m)) (eq source (cadar m)))
                          (setq nc (cons (caddar m) nc)))
                         (t (setq nl (cons `(ask ,source ,(maksand (nreverse nc))) nl))
                            (setq l m)
                            (return t))))))))

(defun unmodalizeor (p)
  (cons 'or (mapcar #'unmodalize (cdr p))))

(defun newsource (p)
  (setq p (newsources p))
  (remedy p))

(defun newsources (p)
  (cond ((atom p) `(ask ,(find-specialists p) ,p))
        ((eq 'and (car p)) (newsource-and p))
        ((eq 'or (car p)) (newsource-or p))
        ((eq 'not (car p))
         (setq p (newsources (cadr p)))
         (cond ((eq 'ask (car p)) `(ask ,(cadr p) (not ,(caddr p))))
               (t `(not ,p))))
        ((eq 'unprovable (car p))
         (setq p (newsources (cadr p)))
         (cond ((eq 'ask (car p)) `(ask ,(cadr p) (unprovable ,(caddr p))))
               (t `(unprovable ,p))))
        ((eq 'bagofall (car p))
         (let (plan)
           (setq plan (newsources (caddr p)))
           (cond ((eq 'ask (car plan))
                  `(ask ,(cadr plan) (bagofall ,(cadr p) ,(caddr plan) ,(cadddr p))))
                 (t `(bagofall ,(cadr p) ,plan ,(cadddr p))))))
        (t `(ask ,(find-dilettantes (car p)) ,p))))

(defun newsource-and (p)
  (do ((l (mapcar #'newsources (cdr p))) (sources) (dum) (nl))
      ((null l)
       (cond ((null (cdr nl)) (car nl))
             (t (mapc #'remedy nl) (cons 'and (nreverse nl)))))
      (cond ((not (eq 'ask (caar l))) (setq nl (cons (car l) nl) l (cdr l)))
            (t (setq sources (cadar l))
               (do ((m (cdr l) (cdr m)) (nc (list (caddar l))))
                   ((null m)
                    (setq nl (cons `(ask ,sources ,(maksand (nreverse nc))) nl))
                    (setq l nil))
                 (cond ((and (eq 'ask (caar m))
                             (setq dum (intersection* sources (cadar m))))
                        (setq sources dum nc (cons (caddar m) nc)))
                       (t (setq nl (cons `(ask ,sources ,(maksand (nreverse nc))) nl))
                          (setq l m)
                          (return t))))))))

(defun newsource-or (p)
  (cons 'or (mapcar #'remedy (mapcar #'newsources (cdr p)))))

(defun find-dilettantes (x)
  (cond ((eq 'true x) *agents*)
        ((eq 'false x) *agents*)
        ((basep x) (append *agents* (finds '?a `(or (sideline ?a ,x) (specialty ?a ,x)) *manager*)))
        (t (sortspecialists (finds '?a `(or (sideline ?a ,x) (specialty ?a ,x)) *manager*)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; consolidator
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass consolidator (agent) ())

(defmethod create (obj (type (eql 'consolidator)))
  (unless (and (boundp obj) (typep (symbol-value obj) 'consolidator))
    (set obj (make-instance 'consolidator :name obj))))

(defmethod destroy (obj (type (eql 'consolidator)))
  (when (and (symbolp obj) (boundp obj) (typep (symbol-value obj) 'transducer))
    (makunbound obj)))

(defmethod message-handler (*message* *sender* (*receiver* consolidator))
  (cond ((atom *message*) (eval *message*))
        (t (eval (plan *message* *sender* *receiver*)))))

(defmethod plan (*message* *sender* (*receiver* consolidator))
  (let ((target (find-target (name *receiver*))))
    (when target `(ask ,(name target) ,(transconsolidate *message*)))))

(defmethod save (p (th consolidator) &optional (f #'samep))
  (declare (ignore f))
  (cond ((atom p) (save p (find-target (name th))))
        ((eq (car p) '<=) (save p (get-upperlibrary th)))
        ((eq (car p) '=>) (save p (get-upperlibrary th)))
        (t (save p (find-target (name th))))))

(defmethod drop (p (th consolidator) &optional (f #'samep))
  (declare (ignore f))
  (cond ((atom p) (drop p (find-target (name th))))
        ((eq (car p) '<=) (drop p (get-upperlibrary th)))
        ((eq (car p) '=>) (drop p (get-upperlibrary th)))
        (t (drop p (find-target (name th))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun transconsolidate (c)
  (cond ((atom c) c)
        ((eq 'update (car c)) (transformupdate c))
        ((eq 'tell (car c)) (transformtell (cadr c)))
        ((eq 'untell (car c)) (transformtell `(not ,(cadr c))))
        ((eq 'eliminate (car c)) nil)
        ((eq 'ask-if (car c)) (consolidator-ask-if c))
        ((eq 'ask-one (car c)) (consolidator-ask-one c))
        ((eq 'ask-all (car c)) (consolidator-ask-all c))
        ((eq 'ask-table (car c)) (consolidator-ask-table c))
        ((eq 'ask-about (car c)) c)
        ((eq 'quote (car c)) c)
        ((macro-function (car c)) (transconsolidate (macroexpand c)))
        (t (cons (car c) (mapcar #'(lambda (x) (transconsolidate x)) (cdr c))))))

(defun consolidator-ask-if (x)
  (let (dum)
    (when (setq dum (consolidateask t (cadr x)))
      `(ask-if ,dum))))

(defun consolidator-ask-one (x)
  (let (dum)
    (when (setq dum (consolidateask (cadr x) (caddr x)))
      `(ask-one ,(cadr x) ,dum))))

(defun consolidator-ask-all (x)
  (let (dum)
    (when (setq dum (consolidateask (cadr x) (caddr x)))
      `(ask-all ,(cadr x) ,dum))))

(defun consolidator-ask-table (x)
  (do ((l (caddr x) (cdr l)) (nl))
      ((null l) `(invert-table ,(cons 'list (nreverse nl))))
      (setq nl (cons (consolidator-ask-column (car l) (cadr x)) nl))))

(defun consolidator-ask-column (slot items)
  `(extract-values ',items
     (ask-all (?x ?y)
       ,(consolidateask '(?x ?y) `(and (oneof ?x . ,items) (,slot ?x ?y))))))

(defun consolidateask (x p)
  (let (upper lower (*ancestry* 1) *target* rl)
    (setq upper (find-upperlibrary (name *receiver*)))
    (setq lower (find-lowerlibrary (name *receiver*)))
    (setq *target* (find-recipient (name *receiver*)))
    (decludes 'infotheory)
    (empty 'infotheory)
    (if (and upper (symbolp upper) (boundp upper))
        (includes 'infotheory (symbol-value upper)))
    (mapc #'(lambda (x) (insert x 'infotheory))
          (contrapositives `(<= (answer ,x) ,p)))
    (setq rl (fullresidues `(answer ,x) 'infotheory #'pivotp))
    (decludes 'infotheory)
    (empty 'infotheory)
    (if (and lower (symbolp lower) (boundp lower))
        (includes 'infotheory (symbol-value lower)))
    (mapc #'(lambda (x) (insert x 'infotheory))
          (contrapositives `(<= (answer ,x) ,(maksor rl))))
    (cond ((setq rl (fullresidues `(answer ,x) 'infotheory #'specialtyp))
           (setq p (decolonize (maksor rl)))
           (if *collapse* (setq p (collapse x p)))
           (if *collapse* (setq p (raisin x p)))
           (if *compress* (setq p (compress p)))
           p))))

(defun find-upperlibrary (x)
  (findx '?x `(upperlibrary ,x ?x) *manager*))

(defun find-lowerlibrary (x)
  (findx '?x `(lowerlibrary ,x ?x) *manager*))

(defun pivotp (r)
  (or (basep r)
      (and (groundp r) (findp `(pivot ,(name *receiver*) ,r) *manager*))))

(defun lits (x)
  (cond ((atom x) (list x))
        ((atom (car x)) (list x))
        (t x)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; coordinator
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass coordinator (agent) ())

(defmethod create (obj (type (eql 'coordinator)))
  (unless (and (boundp obj) (typep (symbol-value obj) 'coordinator))
    (set obj (make-instance 'coordinator :name obj))))

(defmethod destroy (obj (type (eql 'coordinator)))
  (when (and (symbolp obj) (boundp obj) (typep (symbol-value obj) 'coordinator))
    (makunbound obj)))

(defmethod message-handler (msg sender (receiver coordinator))
  (cond ((atom msg) msg)
        ((eq 'update (car msg)) (modify (maksand (cdr msg)) sender receiver))
        ((eq 'tell (car msg)) (modify (maksand (cdr msg)) sender receiver))
        ((eq 'untell (car msg)) (modify (maknot (cadr msg)) sender receiver))
        ((eq 'eliminate (car msg)) nil)
        ((eq 'ask-if (car msg)) (askx t (caddr msg) sender receiver))
        ((eq 'ask-one (car msg)) (askx (cadr msg) (caddr msg) sender receiver))
        ((eq 'ask-all (car msg)) (asks (cadr msg) (caddr msg) sender receiver))
        ((eq 'ask-table (car msg)) (asktable (cadr msg) (caddr msg) sender receiver))
        ((eq 'ask-about (car msg)) nil)))

(defmethod save (p (th coordinator) &optional (f #'samep))
  (declare (ignore f))
  (cond ((atom p) (save p (find-target (name th))))
        ((eq (car p) '<=) (save p (get-upperlibrary th)))
        ((eq (car p) '=>) (save p (get-upperlibrary th)))
        (t (save p (find-target (name th))))))

(defmethod drop (p (th coordinator) &optional (f #'samep))
  (declare (ignore f))
  (cond ((atom p) (drop p (find-target (name th))))
        ((eq (car p) '<=) (drop p (get-upperlibrary th)))
        ((eq (car p) '=>) (drop p (get-upperlibrary th)))
        (t (drop p (find-target (name th))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod modify (p sender (*receiver* coordinator))
  (let (upper lower (*ancestry* 1) updates )
    (setq upper (get-upperlibrary *receiver*))
    (setq lower (get-lowerlibrary *receiver*))
    (decludes 'infotheory)
    (empty 'infotheory)
    (includes 'infotheory upper)
    (setq updates (newconsequences p 'infotheory #'specialp #'pivotp))
    (decludes 'infotheory)
    (empty 'infotheory)
    (includes 'infotheory lower)
    (setq updates (newconsequences (maksand updates) 'infotheory #'specialp #'success))
    (setq updates (nreverse (newchanges (cons 'update updates) *target* nil)))
    (setq updates (filter-imports updates sender *receiver* *receiver*))
    (eval (reassemble updates))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod askx (x p sender (receiver coordinator))
  (declare (ignore sender))
  (let (dum)
    (when (setq dum (coordinateask x p receiver))
      (eval (route (makaskemone x (source dum)))))))

(defmethod asks (x p sender (receiver coordinator))
  (declare (ignore sender))
  (let (dum)
    (when (setq dum (coordinateask x p receiver))
      (eval (route (makaskemall x (source dum)))))))

(defmethod asktable (ol sl sender (receiver coordinator))
  (declare (ignore sender))
  (do ((l sl (cdr l)) (dum) (nl))
      ((null l) `(invert-table ,(cons 'list (nreverse nl))))
      (setq dum (coordinateask '(?x ?y) `(and (oneof ?x . ,ol) (,(car l) ?x ?y)) receiver))
      (setq nl (cons `(extract-values ',ol (ask-all (?x ?y) ,dum)) nl))))

(defun coordinateask (x p *receiver*)
  (let (upper lower (*ancestry* 1) rl)
    (setq upper (get-upperlibrary *receiver*))
    (setq lower (get-lowerlibrary *receiver*))
    (decludes 'infotheory)
    (empty 'infotheory)
    (includes 'infotheory upper)
    (mapc #'(lambda (x) (insert x 'infotheory))
          (contrapositives `(<= (answer ,x) ,p)))
    (setq rl (fullresidues `(answer ,x) 'infotheory #'pivotp))
    (decludes 'infotheory)
    (empty 'infotheory)
    (includes 'infotheory lower)
    (mapc #'(lambda (body) (insert `(<= (answer ,x) . ,(lits body)) 'infotheory)) rl)
    (cond ((setq rl (fullresidues `(answer ,x) 'infotheory #'specialp))
           (setq p (decolonize (maksor rl)))
           (if *collapse* (setq p (collapse x p)))
           (if *collapse* (setq p (raisin x p)))
           (if *compress* (setq p (compress p)))
           p))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

    (isa           consolidator class)
    (superclass    consolidator agent)
    (attribute     consolidator upperlibrary)
    (attribute     consolidator lowerlibrary)
    (attribute     consolidator recipient)
    (attribute     consolidator interest)
    (attribute     consolidator specialty)
    (attribute     consolidator responds)
    (attribute     consolidator performs)
    (attribute     consolidator classifier)
    (attribute     consolidator rootclass)
    (attribute     consolidator nocreate)
    (attribute     consolidator nochange)
    (attribute     consolidator rootrelation)
    (attribute     consolidator noupdate)
    (attribute     consolidator capability)
    (attribute     consolidator frontpage)
    (attribute     consolidator header)
    (attribute     consolidator footer)
    (attribute     consolidator stylesheet)
    (attribute     consolidator nocommand)
    (attribute     consolidator logfile)
    (attribute     consolidator security)

    (isa           coordinator class)
    (superclass    coordinator agent)
    (attribute     coordinator upperlibrary)
    (attribute     coordinator lowerlibrary)
    (attribute     coordinator recipient)
    (attribute     coordinator interest)
    (attribute     coordinator specialty)
    (attribute     coordinator responds)
    (attribute     coordinator performs)
    (attribute     coordinator classifier)
    (attribute     coordinator rootclass)
    (attribute     coordinator nocreate)
    (attribute     coordinator nochange)
    (attribute     coordinator rootrelation)
    (attribute     coordinator noupdate)
    (attribute     coordinator capability)
    (attribute     coordinator frontpage)
    (attribute     coordinator header)
    (attribute     coordinator footer)
    (attribute     coordinator stylesheet)
    (attribute     coordinator nocommand)
    (attribute     coordinator logfile)
    (attribute     coordinator security)

    (isa           manifold class)
    (superclass    manifold agent)
    (attribute     manifold interest)
    (attribute     manifold specialty)
    (attribute     manifold responds)
    (attribute     manifold performs)
    (attribute     manifold classifier)
    (attribute     manifold rulebase)
    (attribute     manifold derivative)
    (attribute     manifold recipient)
    (attribute     manifold rootclass)
    (attribute     manifold rootrelation)
    (attribute     manifold capability)
    (attribute     manifold nocreate)
    (attribute     manifold nochange)
    (attribute     manifold noupdate)
    (attribute     manifold nocommand)
    (attribute     manifold frontpage)
    (attribute     manifold header)
    (attribute     manifold footer)
    (attribute     manifold stylesheet)
    (attribute     manifold logfile)
    (attribute     manifold security)



    (<= (isa ?x agent) (isa ?x manifold))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; fullchange
;;; fullchanges
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod fullchange (p th &optional (obssession  #'failure) (specialty #'deltap))
  (decludes *delta*)
  (includes *delta* th)
  (empty *delta*)
  (fullasserts (mapcar #'pseudify (get-updates p)) *delta* obssession specialty)
  (dolist (p (contents *delta*))
    (cond ((atom p))
          ((eq (car p) 'pos) (save (cadr p) th))
          ((eq (car p) 'neg) (drop (cadr p) th))))
  'done)

(defmethod fullchanges (p th &optional (obssession  #'failure) (specialty #'deltap))
  (decludes *delta*)
  (includes *delta* th)
  (empty *delta*)
  (fullasserts (mapcar #'pseudify (get-updates p)) *delta* obssession specialty)
  (do ((l (contents *delta*) (cdr l)) (nl))
      ((null l) (nreverse nl))
      (cond ((atom (car l)))
            ((eq (caar l) 'pos) (setq nl (cons (cadar l) nl)))
            ((eq (caar l) 'neg) (setq nl (cons `(not ,(cadar l)) nl))))))

(defun pseudify (p)
  (cond ((atom p) `(plus ,p))
        ((eq (car p) 'not)  `(minus ,(cadr p)))
        (t `(plus ,p))))

(defun deltap (x) (find x '(pos neg)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; version with theory for indexing
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun fullramifications (facts *theory* &optional (*obsession*  #'failure) (*specialty* #'failure))
  (let (alist level tracecalls)
    (setq *termination* nil)
    (setq *unifications* 0)
    (setq alist (environment))
    (setq level 0)
    (empty *delta*)
    (dolist (p facts) (fullramificationsdepth p 0))
    (copy-list (contents *delta*))))

(defun fullramificationsdepth (p depth)
  (cond ((numgreaterp depth *depth*) (setq *termination* t) nil)
        (t (fulldrop p alist depth)
           (fullramificationsexp p depth)
           (fulldone p alist depth))))

(defun fullramificationsexp (p depth)
  (cond ((atom p) (fullramificationstest p depth))
        ((eq 'and (car p))
         (mapc #'(lambda (x) (fullramificationsdepth x depth)) (cdr p)))
        (t (fullramificationstest p depth))))

(defun fullramificationstest (p depth)
  (cond ((truep p *delta* #'equalp))
        (t (when (funcall *specialty* (operator p)) (insert p *delta*))
           (fullramificationsdb p depth *theory*))))

(defun fullramificationsdb (p depth th)
  (fullramificationsth p depth th)
  (dolist (th (includees th)) (fullramificationsdb p depth th) nl))

(defun fullramificationsth (p depth th)
  (do ((l (indexps p th) (cdr l)) (ol) (bl (environment)) (*thing*) (*answers*) (dum))
      ((null l))
      (when (and (listp (car l)) (eq (caar l) '<=))
        (do ((m (cddar l) (cdr m)) (om))
            ((null m))
            (when (setq ol (unify (car m) bl p alist))
              (if tracefacts (tracefact (car l)))
              (setq dum (maksand (revappend om (cdr m))))
              (setq *thing* (cadar l) *answers* nil)
              (let ((alist bl) (*filter* *specialty*))
                (fullall dum (list dum) bl (1+ depth) nil))
              (dolist (q *answers*) (fullramificationsdepth q (1+ depth)))
              (backup ol))
            (setq om (cons (car m) om))))))

;;;;

(defmethod classify (x (source manifold))
  (let (dum)
    (cond ((characterp x) 'character)
          ((stringp x) 'string)
          ((numberp x) 'number)
          ((setq dum (findx '?r `(classifier ,(name source) ?r) *manager*))
           (or (request `(ask-one ?class ,(list dum x '?class)) *client* source) 'thing))
          ((dolist (a (finds '?a '(specialty ?a ?r) *manager*))
             (when (not (eq (setq dum (classify x a)) 'thing)) (return dum))))
          ((get-targetclass x source))
          (t 'thing))))


(defmethod classify (x (source consolidator))
  (let (dum)
    (cond ((characterp x) 'character)
          ((stringp x) 'string)
          ((numberp x) 'number)
          ((setq dum (findx '?r `(classifier ,(name source) ?r) *manager*))
           (or (request `(ask-one ?class ,(list dum x '?class)) *client* source) 'thing))
          ((not (eq (setq dum (classify x (find-recipient (name source)))) 'thing)) dum)
          ((get-consolidator-class x source))
          (t 'thing))))

(defun get-consolidator-class (x th)
  (do ((l (contents (get-upperlibrary th)) (cdr l)) (class))
      ((null l) nil)
    (when (and (listp (car l)) (eq (caar l) '<=)
               (listp (cadar l)) (null (cddr (cadar l)))
               (setq class (findx '?c `(predicate ?c ,(caadar l)) *manager*))
               (request `(ask-if ,(list (caadar l) x)) *client* th))
      (return class))))

(defmethod classify (x (source coordinator))
  (let (dum)
    (cond ((characterp x) 'character)
          ((stringp x) 'string)
          ((numberp x) 'number)
          ((setq dum (findx '?r `(classifier ,(name source) ?r) *manager*))
           (or (request `(ask-one ?class ,(list dum x '?class)) *client* source) 'thing))
          ((not (eq (setq dum (classify x (find-recipient (name source)))) 'thing)) dum)
          ((get-consolidator-class x source))
          (t 'thing))))


(defmethod usablep (class (agent consolidator))
  (let (upper lower target)
    (setq target (find-target (name agent)))
    (cond ((usablep class target))
          (t (setq upper (get-upperlibrary agent))
             (setq lower (get-lowerlibrary agent))
             (doubleviewp class agent upper lower)))))

(defun get-upperlibrary (agent)
  (symbol-value (find-upperlibrary (name agent))))

(defun get-lowerlibrary (agent)
  (symbol-value (find-lowerlibrary (name agent))))

(defmethod usablep (class (agent coordinator))
  (let (upper lower target)
    (setq target (find-target (name agent)))
    (cond ((usablep class target))
          (t (setq upper (get-upperlibrary agent))
             (setq lower (get-lowerlibrary agent))
             (doubleviewp class agent upper lower)))))

(defmethod nonemptytablep (table (agent consolidator))
  (let (upper lower target)
    (setq upper (find-upperlibrary (name agent)))
    (setq lower (find-lowerlibrary (name agent)))
    (setq target (find-target (name agent)))
    (cond ((pivotishp target table)
           (cond ((specialishp target table)
                  (nonemptytablep table target))
                 ((rulep table lower)
                  (request `(ask-if ,(list table '@l)) nil agent))))
          ((rulep table upper)
           (request `(ask-if ,(list table '@l)) nil agent)))))

(defmethod nonemptytablep (table (agent coordinator))
  (let (upper lower target)
    (setq upper (find-upperlibrary (name agent)))
    (setq lower (find-lowerlibrary (name agent)))
    (setq target (find-target (name agent)))
    (cond ((pivotishp target table)
           (cond ((specialishp target table)
                  (nonemptytablep table target))
                 ((rulep table lower)
                  (request `(ask-if ,(list table '@l)) nil agent))))
          ((rulep table upper)
           (request `(ask-if ,(list table '@l)) nil agent)))))

;;;;


(defmethod tick ((th fullserver))
  (let (assertions retractions)
    (setq assertions (finds '?p '(pos ?p) th))
    (setq retractions (finds '?p '(neg ?p) th))
    (dolist (p retractions) (drop p th))
    (dolist (p assertions) (save p th))
    'done))

;;;;

(defmethod oldrevisions (p (th transformer))
  (let (ruleserver *target* temp query negs poss)
    (setq ruleserver (symbol-value (find-rulebase th)))
    (setq *target* (find-target (name th)))
    (setq temp (make-instance 'theory))
    (insertatoms p temp)
    (includes temp ruleserver)
    (setq query (maksor (viewresidues '(neg ?p) temp #'specialtyp #'success)))
    (setq negs (request `(ask-all (neg ?p) ,query) th *target*))
    (setq query (maksor (viewresidues '(pos ?p) temp #'specialtyp #'success)))
    (setq poss (request `(ask-all (pos ?p) ,query) th *target*))
    (decludes temp)
    (empty temp)
    (mapcar #'undifferentiate (nconc negs poss))))

;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; viewchange, viewchanges
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(deftheory gates
  (<= (mail mrg ?e)
      (event.location ?e gates-200)))
GATES

(viewspecialize 'event.location 'gates)
DONE

(viewmaterialize 'mail'gates)
DONE

(viewchanges '(event.location e23 gates-200) 'gates)
((POS (MAIL MRG E23)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(setq *baserelations* '(p q) *materializations* '(r))
(R)

(deftheory new
  (p a b)
  (p b c)
  (q c d)
  (<= (r ?x ?z) (p ?x ?y) (q ?y ?z)))
NEW

(viewchanges '(and (not (p b c)) (q b e) (q b f)) 'new)
((POS (R A F)) (POS (R A E)) (NEG (R B D)))

(deftheory bad
  (p a b)
  (<= (error "P is a function.") (p ?x ?y) (p ?x ?z) (distinct ?y ?z)))
BAD

(viewmistakes '(p a c) 'bad)
("P is a function.")

(viewmistakes '(and (not (p a b)) (p a c)) 'bad)
NIL

(resetsystem)
*

;;;;

(defun transformupdate (msg)
  (let (rulebase *target* (*intensions* nil))
    (setq rulebase (get-rulebase *receiver*))
    (setq *target* (find-target (name *receiver*)))
    (decludes 'infotheory)
    (empty 'infotheory)
    (includes 'infotheory rulebase)
    (setq msg (maksand (cdr msg)))
    (do ((l (newconsequences msg 'infotheory #'specialtyp #'interestedp) (cdr l)) (nl))
        ((null l) (when nl (cons 'update (nreverse nl))))
        (cond ((and (listp (car l)) (eq 'execute (caar l)))
               (setq nl (cons (cadar l) nl)))
              ((and (listp (car l)) (eq 'evaluate (caar l)))
               (setq nl (cons `(apply ',(caadar l) ',(cdadar l)) nl)))
              ((and (listp (car l)) (eq '=> (caar l)))
               (setq nl (cons (decolonize `(==> ,(cadar l) ,(caddar l))) nl)))
              (t (setq nl (cons (decolonize (car l)) nl)))))))

(defun transformer-ask-if (x)
  (let (dum)
    (when (setq dum (transformask t (cadr x)))
      `(ask-if ,dum))))

(defun transformer-ask-one (x)
  (let (dum)
    (when (setq dum (transformask (cadr x) (caddr x)))
      `(ask-one ,(cadr x) ,dum))))

(defun transformer-ask-all (x)
  (let (dum)
    (when (setq dum (transformask (cadr x) (caddr x)))
      `(ask-all ,(cadr x) ,dum))))

(defun transformtelltable (c receiver)
  (let (pattern rulebase *target*)
    (setq pattern (patternize (cadr c)))
    (setq rulebase (find-rulebase receiver))
    (setq *target* (find-target (name *receiver*)))
    (decludes 'infotheory)
    (empty 'infotheory)
    (if (and rulebase (symbolp rulebase) (boundp rulebase))
        (includes 'infotheory (eval rulebase)))
    (do ((l (newconsequences pattern 'infotheory #'specialtyp #'interestedp) (cdr l)) (nl))
        ((null l) `(tell (==> (oneof ,pattern . ,(cdr c)) ,(maksand (nreverse nl)))))
        (cond ((and (listp (car l)) (eq 'execute (caar l)))
               (setq nl (cons (cadar l) nl)))
              ((and (listp (car l)) (eq 'evaluate (caar l)))
               (setq nl (cons `(apply ',(caadar l) ',(cdadar l)) nl)))
              ((and (listp (car l)) (eq '=> (caar l)))
               (setq nl (cons (decolonize `(==> ,(cadar l) ,(caddar l))) nl)))
              (t (setq nl (cons (decolonize (car l)) nl)))))))

;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Tableservers
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass tableserver (tablebase agent) ())

;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; trueserver
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass trueserver (agent) ())

(defmethod envindexps (p al (th trueserver))
  (cond ((varp p) (contents th))
        ((atom p) (indexees p th))
        (t (do ((l (cdr p) (cdr l)) (dum))
               ((null l) (indexps (car p) th))
               (setq dum (unival (car l) al))
               (cond ((varp dum))
                     ((atom dum) (return (indexees dum th))))))))

(defmethod indexps (p (th trueserver))
  (flatindexps p th))

(defmethod insert (p (receiver trueserver))
  (dbinsert p receiver))

(defmethod revise (p (th trueserver))
  (dbrevise p th))

(defmethod findx (x p (th trueserver))
  (fulltruex x p th))

(defmethod finds (x p (th trueserver))
  (fulltrues x p th))

(defmethod knownp (p (th trueserver) &optional (f 'matchp))
  (declare (ignore f))
  (dataknownp p th))

;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Terminals
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar *terminal* (intern (hostname)))

(defmethod message (msg sender (receiver (eql *terminal*)))
  (declare (ignore sender))
  (fresh-line) (princ "==> ") (prin1 msg))

(defmethod request (msg sender (receiver (eql *terminal*)))
  (declare (ignore sender))
  (let ((ans))
    (fresh-line) (princ "==> ") (prin1 msg)
    (fresh-line) (princ "--> ") (setq ans (read))
    (if (and (listp ans) (eq 'reply (car ans))) (cadr ans) ans)))

;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Old stuff
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod change (p (th ruleserver))
  (cond ((atom p) (save p th))
        ((eq 'error (car p)) (cadr p))
        ((eq 'and (car p)) (dolist (x (cdr p)) (change x th)))
        (t (save p th))))

(defmethod changes (p (th ruleserver))
  (cond ((atom p) (list p))
        ((eq 'and (car p))
         (do ((l (cdr p) (cdr l)) (nl))
             ((null l) (nreverse nl))
             (setq nl (nreconc (changes (car l) th) nl))))
        (t (list p))))

;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Connection servers
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun connection-server (port)
  (start-tcp-server-process #'connection-handler port))

(defun connection-handler (stream &rest l)
  (declare (ignore l))
  (ignore-errors
   (let ((ans (package-handler (receive stream))))
     (if ans (send ans stream))
     (close stream))))

(defun package-handler (pkg)
  (let ((sender) (receiver) (content) (id) (dum))
    (cond ((atom pkg) nil)
          ((eq 'package (car pkg))
           (setq sender (getf (cdr pkg) :sender))
           (setq receiver (getf (cdr pkg) :receiver))
           (setq id (getf (cdr pkg) :reply-with))
           (setq content (getf (cdr pkg) :content))
           (cond ((null id) (message content sender receiver) nil)
                 (t (setq dum (request content sender receiver))
                    (response `(reply ,dum) receiver sender id)))))))

(defun response (msg sender receiver id)
  `(package :sender ,sender :receiver ,receiver :in-reply-to ,id :content ,msg))

;;;;

(defmethod askanswer (s sender receiver)
  (declare (ignore s sender receiver))
  nil)

(defmethod askanswers (s sender receiver)
  (declare (ignore s sender receiver))
  nil)

(defmacro ask-answer (&rest sentences)
  `(askanswer ',sentences *sender* *receiver*))

(defmacro ask-answers (&rest sentences)
  `(askanswers ',sentences *sender* *receiver*))

(defmacro ask-associates (objs)
  `(askassociates ',objs *sender* *receiver*))

(defmacro ask-rules (x)
  `(askabout ',x *sender* *receiver*))

(defmacro ask-consistent (&rest rules)
  `(askc ',(maksand rules) *sender* *receiver*))

;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; (c) Copyright 1999-2003 by Michael Genesereth.  All rights reserved.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(eval-when (compile load eval)
  (proclaim '(special *metalibrary* *manager* *library* *warehouse*
                      *theory* *ancestry* *saves* *assumables* *test*
                      *order* *collapse* *host* *port* 
                      *message* *sender* *receiver* *target* *agent*)))

(setq *saves* t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; broker
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass broker (agent) ())

(defmethod message-handler (*message* *sender* (*receiver* broker))
  (let (*library* *warehouse*)
    (setq *library* (referent (find-rulebase *receiver*)))
    (setq *warehouse* (referent (find-database *receiver*)))
    (acleval *message* *sender* *receiver*)))

(defmethod acleval (msg sender (receiver broker))
  (cond ((atom msg) (call-next-method msg sender receiver))
        ((eq 'tell (car msg))
         (includes *receiver* *library*)
         (catch 'assume (assume (cadr msg) receiver)))
        ((eq 'untell (car msg))
         (includes *receiver* *library*)
         (catch 'forget (forget (cadr msg) receiver)))
        ((eq 'eliminate (car msg))
         (includes *receiver* *library*)
         (discard (cadr msg) receiver))
        ((eq 'ask-if (car msg)) (findp (cadr msg) receiver))
        ((eq 'ask-one (car msg)) (findx (cadr msg) (caddr msg) receiver))
        ((eq 'ask-all (car msg)) (finds (cadr msg) (caddr msg) receiver))
        ((eq 'ask-table (car msg)) (findtable (cadr msg) (caddr msg)))
        ((eq 'ask-about (car msg)) (askabout (cadr msg) sender receiver))
        ((findp `(responds ?a ,(car msg)) *manager*) (askmessage msg))
        ((findp `(performs ?a ,(car msg)) *manager*) (askrequest msg))
        (t (call-next-method msg sender receiver))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; affirm, retract, discardall
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod affirm (p sender (receiver broker))
  (declare (ignore sender))
  (save p receiver))

(defmethod retract (p sender (receiver broker))
  (declare (ignore sender))
  (drop p receiver))

(defmethod discardall (p sender (receiver broker))
  (declare (ignore sender))
  (kill p receiver))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; revise, errors, revisions, notifications, reactions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; insert, uninsert
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod insert (p (th broker))
  (do ((l (finds '?a `(interest ?a ,(operator p)) *manager*) (cdr l))
       (msg `(tell ,p)) (dum))
      ((null l) 'done)
      (cond ((and (eq (car l) *sender*) (equal msg *message*)))
            ((stringp (setq dum (request msg *receiver* (car l))))
             (throw 'assume dum)))))

(defmethod uninsert (p (th broker))
  (do ((l (finds '?a `(interest ?a ,(operator p)) *manager*) (cdr l))
       (msg `(untell ,p)) (dum))
      ((null l) 'done)
      (cond ((and (eq (car l) *sender*) (equal msg *message*)))
            ((stringp (setq dum (request msg *receiver* (car l))))
             (throw 'forget dum)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; askp, askx, asks
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; asktable, askabout
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod asktable (ol sl sender (receiver broker))
  (call-next-method ol sl sender receiver))

(defmethod askabout (x sender (receiver broker))
  (do ((l (find-agents) (cdr l)) (msg `(ask-about ,x)) (nl))
      ((null l) nl)
      (cond ((and (eq (car l) sender) (equal msg *message*)))
            (t (setq nl (nconc nl (request msg receiver (car l))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; envindexps
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod envindexps (p al (th broker))
  (setq p (plugstdexp p al))
  (do ((l (finds '?a `(specialty ?a ,(car p)) *manager*) (cdr l)) (nl))
      ((null l) (nreverse nl))
      (setq nl (nreconc (request `(ask-all ,p ,p) *receiver* (car l)) nl))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Special request handling
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun askmessage (x)
  (do ((l (cdr x) (cdr l)) (nl))
      ((null l) (distribute (cons (car x) (nreverse nl))))
      (setq nl (cons (message (car l) *sender* *receiver*) nl))))

(defun askrequest (x)
  (do ((l (cdr x) (cdr l)) (nl))
      ((null l) (delegate (cons (car x) (nreverse nl))))
      (setq nl (cons (request (car l) *sender* *receiver*) nl))))

(defun distribute (msg)
  (do ((l (finds '?a `(responds ?a ,(operator msg)) *manager*) (cdr l)))
      ((null l) 'done)
      (cond ((and (eq (car l) *sender*) (equal msg *message*)))
            (t (message msg *receiver* (car l))))))

(defun delegate (msg)
  (do ((l (finds '?a `(performs ?a ,(operator msg)) *manager*) (cdr l)))
      ((null l) 'done)
      (cond ((and (eq (car l) *sender*) (equal msg *message*)))
            (t (return (request msg *receiver* (car l)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; stuff
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun find-rulebase (agent)
  (findx '?x `(rulebase ,(name agent) ?x) *manager*)) 

(defun find-database (agent)
  (findx '?x `(database ,(name agent) ?x) *manager*))

;;; needs update handling
;;; Note extreme slowness of indexing.
;;; Note that it still gets all answers to subqueries as part of this indexing!
;;; However, second agent gets narrower request, just more of them.

(defmethod acleval (msg sender (receiver agent))
  (cond ((atom msg) (call-next-method msg sender receiver))
        ((eq 'tell (car msg))
         (cond ((null (cddr msg)) (affirm (cadr msg) sender receiver))
               (t (affirmall (cadr msg) (caddr msg) sender receiver))))
        ((eq 'untell (car msg))
         (cond ((null (cddr msg)) (retract (cadr msg) sender receiver))
               (t (retractall (cadr msg) (caddr msg) sender receiver))))
        ((eq 'eliminate (car msg)) (kill (cadr msg) receiver))
        ((eq 'ask-if (car msg)) (findp (cadr msg) receiver))
        ((eq 'ask-one (car msg)) (findx (cadr msg) (caddr msg) receiver))
        ((eq 'ask-all (car msg)) (finds (cadr msg) (caddr msg) receiver))
        ((eq 'ask-table (car msg)) (findtable (cadr msg) (caddr msg)))
        ((eq 'ask-about (car msg)) (sentences (cadr msg) receiver))
        ((eq 'ask (car msg)) (request (caddr msg) receiver (cadr msg)))
        (t (call-next-method msg sender receiver))))

(defmethod acleval (x sender receiver)
  (cond ((atom x)
         (cond ((numberp x) x)
               ((characterp x) x)
               ((stringp x) x)
               ((symbolp x) (if (boundp x) (symbol-value x)))
               (t x)))
        ((eq 'if (car x))
         (cond ((acleval (cadr x) sender receiver)
                (acleval (caddr x) sender receiver))
               ((cadddr x) (acleval (cadddr x) sender receiver))))
        ((eq 'progn (car x))
         (do ((l (cdr x) (cdr l)) (ans))
             ((null l) ans)
             (setq ans (acleval (car l) sender receiver))))
        ((eq 'quote (car x)) (cadr x))
        ((eq 'setq (car x)) (set (cadr x) (acleval (caddr x) sender receiver)))
        ((special-form-p (car x)) nil)
        ((macro-function (car x)) (acleval (macroexpand x) sender receiver))
        ((fboundp (car x))
         (apply (car x) (mapcar #'(lambda (x) (acleval x sender receiver))
                                (cdr x))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; oldrevise
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod oldrevise (facts (th transformer))
  (let (ruleserver factserver)
    (setq ruleserver (symbol-value (find-rulebase th)))
    (setq factserver (find-target (name th)))
    (setq facts (reduceviews facts th))
    (request `(update . ,facts) th factserver)
    (setq facts (ramifyviews facts factserver ruleserver))
    (request `(update . ,facts) th factserver)
    'done))

(defun reduceviews (facts receiver)
  (let (rulebase (*intensions* nil))
    (setq rulebase (find-rulebase (name receiver)))
    (when (and rulebase (symbolp rulebase) (boundp rulebase))
      (setq rulebase (symbol-value rulebase)))
    (getconjuncts (residue t (maksand facts) rulebase #'basicp))))

(defun ramifyviews (facts factserver ruleserver)
  (let (insertions deletions bases materials changes)
    (multiple-value-setq (insertions deletions) (divide facts))
    (dolist (p facts) (setq bases (getbases p bases)))
    (dolist (r bases) (setq materials (getmaterials r ruleserver materials)))
    (dolist (r materials)
      (dolist (rule (getchanges r bases insertions deletions ruleserver))
        (setq changes (nreconc (request `(ask-all ,(cadr rule) ,(maksand (cddr rule))) nil factserver) changes))))
    (nreverse (uniquify changes))))

(defun getbases (p nl)
  (cond ((atom p) (adjoin p nl))
        ((find (car p) '(not and or))
         (dolist (p (cdr p)) (setq nl (getbases p nl))) nl)
        (t (adjoin (car p) nl))))

(defun getmaterials (p th nl)
  (do ((l (indexees p th) (cdr l)))
      ((null l) nl)
      (cond ((atom (car l)))
            ((eq '<= (caar l))
             (cond ((atom (cadar l)))
                   ((find (operator (cadar l)) nl))
                   ((amongp p (cddar l) #'eq)
                    (when (materialp (operator (cadar l)))
                      (setq nl (cons (operator (cadar l)) nl)))
                    (setq nl (getmaterials (operator (cadar l)) th nl))))))))

(defun getchanges (r bases positives negatives th)
  (do ((l (getrules r th) (cdr l)) (nl))
      ((null l) (nreverse nl))
      (do ((m (cddar l) (cdr m)) (om) (data) (new))
          ((null m))
          (cond ((atom (car m)))
                ((find (caar m) bases)
                 (when (setq data (getdata (caar m) positives))
                   (setq new `(oneof ,(car m) . ,data))
                   (setq new `(<= ,(cadar l) ,new . ,(revappend om (cdr m))))
                   (setq nl (cons new nl))))
                ((and (eq (caar m) 'not) (find (caadar m) bases))
                 (when (setq data (getdata (caadar m) negatives))
                   (setq new `(oneof ,(cadar m) . ,data))
                   (setq new `(<= ,(cadar l) ,new . ,(revappend om (cdr m))))
                   (setq nl (cons new nl)))))
          (setq om (cons (car m) om)))))

(defun getdata (r facts)
  (do ((l facts (cdr l)) (nl))
      ((null l) (nreverse nl))
      (cond ((atom (car l)))
            ((eq (caar l) r) (setq nl (cons (car l) nl))))))

(defun getrules (r th)
  (nconc (normalizevars (nonreductions r th #'basicp #'success))
         (normalizevars (reductions r th #'basicp #'success))))

(defun normalizevars (rules)
  (do ((l rules (cdr l)) (vars (copyargs (cadar rules))) (nl))
      ((null l) (nreverse nl))
      (cond ((atom (cadar l)) (setq nl (cons (car l) nl)))
            ((eq (caadar l) 'not)
             (do ((m (cdadr (cadar l)) (cdr m)) (n vars (cdr n)) (nm) (al) (rule))
                 ((null m) (setq rule (sublis al (car l)))
                  (setq nl (cons `(<= ,(cadr rule) . ,(nreconc nm (cddr rule))) nl)))
                 (cond ((varp (car m)) (setq al (acons (car m) (car n) al)))
                       (t (setq nm (cons `(same ,(car m) ,(car n)) nm))))))
            (t (do ((m (cdadar l) (cdr m)) (n vars (cdr n)) (nm) (al) (rule))
                   ((null m) (setq rule (sublis al (car l)))
                    (setq nl (cons `(<= ,(cadr rule) . ,(nreconc nm (cddr rule))) nl)))
                   (cond ((varp (car m)) (setq al (acons (car m) (car n) al)))
                         (t (setq nm (cons `(same ,(car m) ,(car n)) nm)))))))))

(defun copyargs (p)
  (cond ((atom p) nil)
        ((eq (car p) 'not) (copyargs (cadr p)))
        (t (do ((l (cdr p) (cdr l)) (nl))
               ((null l) (nreverse nl))
               (setq nl (cons (decolonize (newindvar)) nl))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; change
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod change (p (th transformer))
  (let (rulebase *target* (*intensions* nil))
    (setq rulebase (find-rulebase th))
    (setq *target* (find-target (name th)))
    (decludes 'infotheory)
    (empty 'infotheory)
    (if (and rulebase (symbolp rulebase) (boundp rulebase))
        (includes 'infotheory (eval rulebase)))
    (do ((l (newconsequences p 'infotheory #'specialtyp #'interestedp) (cdr l)) (nl))
        ((null l) (request `(update . ,(nreverse nl)) th *target*))
        (cond ((and (listp (car l)) (eq 'execute (caar l)))
               (setq nl (cons (cadar l) nl)))
              ((and (listp (car l)) (eq 'evaluate (caar l)))
               (setq nl (cons `(apply ',(caadar l) ',(cdadar l)) nl)))
              ((and (listp (car l)) (eq '=> (caar l)))
               (setq nl (cons (decolonize `(==> ,(cadar l) ,(caddar l))) nl)))
              (t (setq nl (cons (decolonize (car l)) nl)))))))

(defmethod change (p (th transformer))
  (request p th *target*))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; reviser
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod reviser (facts (th transformer))
  (let (ruleserver factserver changes errors)
    (setq ruleserver (symbol-value (find-rulebase th)))
    (setq factserver (find-target (name th)))
    (setq facts (reducer facts th))
    (request `(update . ,facts) th factserver)
    (setq changes (ramifier facts factserver ruleserver))
    (dolist (item changes)
      (when (and (listp item) (eq (car item) 'error))
        (setq errors (adjoin (cadr item) errors :test #'equalp))))
    (cond (errors
           (request `(update . ,(mapcar #'maknot facts)) th factserver)
           errors)
          (t (request `(update . ,(nreverse changes)) th factserver)
             'done))))

(defun reducer (facts receiver)
  (let (rulebase (*intensions* nil))
    (setq rulebase (find-rulebase (name receiver)))
    (when (and rulebase (symbolp rulebase) (boundp rulebase))
      (setq rulebase (symbol-value rulebase)))
    (getconjuncts (residue t (maksand facts) rulebase #'basicp))))

(defun ramifier (facts factserver ruleserver)
  (let (changes)
    (dolist (rule (ramifications (maksand facts) ruleserver #'basicp #'interestp))
      (cond ((atom rule) (setq changes (cons rule changes)))
            ((eq '<= (car rule))
             (setq changes (nreconc (request `(ask-all ,(cadr rule) ,(maksand (cddr rule))) nil factserver) changes)))
            (t (setq changes (cons rule changes)))))
    changes))

(defparameter *ramifications* nil)

(defmethod ramifications (p *theory* *filter* *test*)
  (let ((alist (environment)) (*ramifications*) (level 0) tracecalls)
    (setq *termination* nil)
    (ramificationdepth p alist 'true 1 nil)
    (nreverse *ramifications*)))

(defun ramificationdepth (p al res depth stack)
  (cond ((numgreaterp depth *depth*) (setq *termination* t) nil)
        ((find (operator p) (cdr stack)) nil)
        (t (if traceexpressions (tracesave p al))
           (ramificationexp p al res depth stack)
           (if traceexpressions (tracedone p al)))))

(defun ramificationexp (p al res depth stack)
  (cond ((atom p) (ramificationdb p al res depth stack *theory*))
        ((eq 'and (car p)) 
         (mapc #'(lambda (x) (ramificationdepth x al res depth stack)) (cdr p)))
        ((funcall *test* (operator p))
         (cond ((eq res 'false))
               ((eq res 'true)
                (setq *ramifications*
                      (adjoin (plugstdexp p al) *ramifications* :test #'equalp)))
               (t (setq *ramifications*
                        (cons (plugstdexp `(<= ,p ,res) al) *ramifications*))))
         (ramificationdb p al res depth stack *theory*))
        (t (ramificationdb p al res depth stack *theory*))))  

(defun ramificationdb (p al rl depth stack th)
  (cond ((ramificationth p al rl depth stack th))
        (t (do ((l (includees th) (cdr l)))
               ((null l) nil)
               (ramificationdb p al rl depth stack (car l))))))

(defun ramificationth (p al rl depth stack th)
  (do ((l (indexps p th) (cdr l)) (ol) (bl (environment))
       (antecedent) (consequent))
      ((null l))
      (cond ((atom (car l)))
            ((eq (caar l) '<=)
             (do ((m (cddar l) (cdr m)) (om))
                 ((null m))
                 (when (setq ol (unify (car m) bl p al))
                   (if tracefacts (tracefact (car l)))
                   (setq antecedent (plugstdexp (maksand (revappend om (cdr m))) bl))
                   (setq consequent (plugstdexp (cadar l) bl))
                   (ramification antecedent consequent al rl depth stack)
                   (backup ol))
                 (setq om (cons (car m) om)))))))

(defun ramification (antecedent consequent al rl depth stack)
  (do ((l (residues (vars consequent) antecedent *theory* *filter*) (cdr l)))
      ((null l) rl)
      (ramificationdepth consequent al (makand rl (car l))
                         (1+ depth) (cons (operator consequent) stack))))

(defun divide (facts)
  (do ((l facts (cdr l)) (positives) (negatives))
      ((null l) (values (nreverse positives) (nreverse negatives)))
      (cond ((atom (car l)) (setq positives (cons (car l) positives)))
            ((eq (caar l) 'not) (setq negatives (cons (cadar l) negatives)))
            (t (setq positives (cons (car l) positives))))))

(defun getconjuncts (p)
  (cond ((null p) nil)
        ((atom p) (list p))
        ((eq 'and (car p)) (cdr p))
        (t (list p))))

;;;;
#|
(defmethod message-handler (*message* *sender* (*receiver* facilitator))
  (let (*library* *warehouse* *agents*)
    (setq *library* (referent (find-rulebase *receiver*)))
    (setq *warehouse* (referent (find-database *receiver*)))
    (setq *agents* (finds '?a '(specialty ?a ?x) *manager*))   ;;; (setq *agents* (find-agents))
    (facilitate *message* *sender* *receiver*)))
|#

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; collector
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass collector (agent) ())

(defmethod message-handler (msg sender (receiver collector))
  (cond ((atom msg) msg)
        ((eq 'update (car msg)) (modify msg sender receiver))
        ((eq 'tell (car msg)) (modify msg sender receiver))
        ((eq 'untell (car msg)) (modify msg sender receiver))
        (t (request msg sender (find-target receiver)))))

(defmethod save (p (th collector) &optional (f #'samep))
  (declare (ignore f))
  (cond ((atom p) (save p (find-target (name th))))
        ((eq (car p) '<=) (save p (get-rulebase th)))
        ((eq (car p) '=>) (save p (get-rulebase th)))
        (t (save p (find-target (name th))))))

(defmethod drop (p (th collector) &optional (f #'samep))
  (declare (ignore f))
  (cond ((atom p) (drop p (find-target (name th))))
        ((eq (car p) '<=) (drop p (get-rulebase th)))
        ((eq (car p) '=>) (drop p (get-rulebase th)))
        (t (drop p (find-target (name th))))))

(defmethod modify (p sender (receiver collector))
  (let (updates targets security)
    (setq targets (find-targets (name receiver)))
    (setq security (get-security receiver))
    (setq updates (filter-imports (get-updates p) sender receiver security))
    (when updates (dolist (a targets) (request (cons 'update updates) sender a)))))

(defun get-updates (p)
  (cond ((atom p) (list p))
        ((eq (car p) 'and) (cdr p))
        ((eq (car p) 'update) (cdr p))
        (t (list p))))

(defun filter-imports (facts sender receiver security)
  (request `(ask-all ?p (and (oneof ?p . ,facts)
                             (authorized ,(name sender) ,(name receiver) ?p)))
           nil security))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; disseminator
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass disseminator (agent) ())

(defmethod message-handler (msg sender (receiver disseminator))
  (cond ((atom msg) msg)
        ((eq 'update (car msg)) (modify msg sender receiver))
        ((eq 'tell (car msg)) (modify msg sender receiver))
        ((eq 'untell (car msg)) (modify msg sender receiver))))

(defmethod save (p (th disseminator) &optional (f #'samep))
  (declare (ignore f))
  (cond ((atom p) (save p (find-target (name th))))
        ((eq (car p) '<=) (save p (get-rulebase th)))
        ((eq (car p) '=>) (save p (get-rulebase th)))
        (t (save p (find-target (name th))))))

(defmethod drop (p (th disseminator) &optional (f #'samep))
  (declare (ignore f))
  (cond ((atom p) (drop p (find-target (name th))))
        ((eq (car p) '<=) (drop p (get-rulebase th)))
        ((eq (car p) '=>) (drop p (get-rulebase th)))
        (t (drop p (find-target (name th))))))

(defmethod modify (p sender (receiver disseminator))
  (let (updates)
    (setq updates (get-updates p))
    (setq updates (filter-exports updates sender sender))
    (eval (reassemble updates))))

(defun filter-exports (facts sender security)
  (request `(ask-all (tell ?b ?p) (and (oneof ?p . ,facts)
                                       (publishable ,(name sender) ?b ?p)))
           nil security))

(defun reassemble (updates)
  (let (dum al)
    (dolist (p updates)
      (cond ((setq dum (assoc (cadr p) al))
             (rplacd dum (cons (caddr p) (cdr dum))))
            (t (setq al (acons (cadr p) (cddr p) al)))))
    (do ((l al (cdr l)) (pl))
        ((null l) (maksprogn pl))
        (setq pl (cons `(ask ,(caar l) (update . ,(nreverse (cdar l)))) pl)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Residue without consistency checking
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defparameter *filter*  #'basep)

(defun unwind (x p *theory* &optional (*filter* #'basep) (*test* #'success))
  (let ((*consistency* nil) (alist (environment)))
    (unwindexp x p)))

(defun unwindexp (x p)
  (cond ((atom p) p)
        ((eq 'and (car p)) (unwindand x p))
        ((eq 'or (car p)) (unwindor x p))
        ((and (funcall *filter* (operator p))
              (funcall *test* (setq p (plugstdexp p alist)))) p)
        (t (unwinddb x p))))

(defun unwindand (x p)
  (do ((l (cdr p) (cdr l)) (dum) (nl))
      ((null l) (maksand (nreverse nl)))
      (setq dum (unwindexp x (car l)))
      (cond ((eq dum 'true))
            ((or (null dum) (eq dum 'false)) (return 'false))
            (t (setq nl (cons dum nl))))))

(defun unwindor (x p)
  (do ((l (cdr p) (cdr l)) (dum) (nl))
      ((null l) (maksor (nreverse nl)))
      (setq dum (unwindexp x (car l)))
      (cond ((eq 'true dum) (return 'true))
            ((or (null dum) (eq dum 'false)))
            (t (setq nl (cons dum nl))))))

(defun unwinddb (x p)
  (maksor (residues x p *theory* *filter* *test*)))

(defun processablep (p)
  (or (flatp p)
      (basep (operator p))
      (findp `(and (specialty ?x ,(operator p)) (kind ?x ruleserver)) *manager*))) 

(defun flatp (p)
  (cond ((atom p))
        ((eq 'oneof (car p)))
        ((eq '== (car p)))
        ((eq 'execute (car p)))
        ((eq 'evaluate (car p)))
        ((eq 'unprovable (car p)) (flatp (cadr p)))
        ((eq 'and (car p)) (every #'flatp (cdr p)))
        ((eq 'or (car p)) (every #'flatp (cdr p)))
        (t (every #'atom p))))

;;;;


(defparameter *newtable* t)

(defmethod asktable (ol sl *sender* (*receiver* agent))
  (findtable ol sl))

(defun findtable (items slots)
  (if *newtable* (newasktable items slots) (oldasktable items slots)))

(defun oldasktable (items slots)
  (do ((l items (cdr l)) (nl))
      ((null l) (nreverse nl))
      (do ((m slots (cdr m)) (ml))
          ((null m) (setq nl (cons (nreverse ml) nl)))
          (setq ml (cons (message-handler `(ask-all ?y (,(car m) ,(car l) ?y))
                                          *sender* *receiver*)
                         ml)))))

(defun newasktable (items slots)
  (do ((l slots (cdr l)) (nl))
      ((null l) (invert-table (nreverse nl)))
      (setq nl (cons (newaskvalue (car l) items) nl))))

(defun newaskvalue (slot items)
  (extract-values items
    (request `(ask-all (?x ?y) (and (oneof ?x . ,items) (,slot ?x ?y)))
             *sender* *receiver*)))

(defmethod asktable (ol sl sender (receiver facilitator))
  (declare (ignore sender))
  (let (dum)
    (cond ((or (null ol) (null sl)) nil)
          ((and (null (cdr ol)) (null (cdr sl)))
           (setq dum (facilitateask '?y (list (car sl) (car ol) '?y) receiver))
           (list (list (eval (route (makaskemall '?y dum))))))
          (t (do ((l sl (cdr l)) (nl))
                 ((null l) (invert-table (nreverse nl)))
                 (setq dum (source (facilitateask '(?x ?y) `(and (oneof ?x . ,ol) (,(car l) ?x ?y)) receiver)))
                 (setq nl (cons (extract-values ol (eval (route (makaskemall '(?x ?y) dum)))) nl)))))))

;;;;

(defun sidelinep (r)
  (or (basep r) (findp `(sideline ?a ,r) *manager*)))

(defun nontriggerp (x)
  (not (triggerprs x 'infotheory)))

(defun triggerprs (x th)
  (cond ((triggerpth x th))
        (t (some #'(lambda (th) (triggerprs x th)) (includees th)))))

(defun triggerpth (x th)
  (do ((l (indexees x th) (cdr l)))
      ((null l) nil)
      (when (and (listp (car l)) (eq '=> (caar l))
                 (or (eq x (cadar l))
                     (and (listp (cadar l)) (eq x (caadar l)))))
        (return t))))

;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Old stuff
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; transform
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun transform (c)
  (cond ((atom c) c)
        ;((eq 'update (car c)) (transformupdate c))
        ;((eq 'tell (car c)) (transformtell (cadr c)))
        ;((eq 'untell (car c)) (transformtell `(not ,(cadr c))))
        ;((eq 'eliminate (car c)) c)
        ;((eq 'ask-if (car c)) (transformer-ask-if c))
        ;((eq 'ask-one (car c)) (transformer-ask-one c))
        ;((eq 'ask-all (car c)) (transformer-ask-all c))
        ;((eq 'ask-table (car c)) (transformer-ask-table c))
        ((eq 'ask-about (car c)) c)
        ((eq 'quote (car c)) c)
        ((macro-function (car c)) (transform (macroexpand c)))
        (t (cons (car c) (mapcar #'transform (cdr c))))))

(defun transformtell (p)
  (let ((rulebase) (*target*))
    (setq rulebase (find-rulebase *receiver*))
    (setq *target* (find-target (name *receiver*)))
    (decludes 'infotheory)
    (empty 'infotheory)
    (if (and rulebase (symbolp rulebase) (boundp rulebase))
        (includes 'infotheory (eval rulebase)))
    (do ((l (newconsequences p 'infotheory #'specialtyp #'interestedp) (cdr l)) (nl))
        ((null l) `(tell ,(maksand (nreverse nl))))
        (cond ((and (listp (car l)) (eq 'execute (caar l)))
               (setq nl (cons (cadar l) nl)))
              ((and (listp (car l)) (eq 'evaluate (caar l)))
               (setq nl (cons `(apply ',(caadar l) ',(cdadar l)) nl)))
              ((and (listp (car l)) (eq '=> (caar l)))
               (setq nl (cons (decolonize `(==> ,(cadar l) ,(caddar l))) nl)))
              (t (setq nl (cons (decolonize (car l)) nl)))))))

(defun transformer-ask-table (x)
  (do ((l (caddr x) (cdr l)) (nl))
      ((null l) `(invert-table ,(cons 'list (nreverse nl))))
      (setq nl (cons (transformer-ask-column (car l) (cadr x)) nl))))

(defun transformer-ask-column (slot items)
  `(extract-values ',items
     (ask-all (?x ?y)
       ,(transformask '(?x ?y) `(and (oneof ?x . ,items) (,slot ?x ?y))))))

(defun transformask (x p)
  (let (rl library *target* (*ancestry* 1))
    (setq library (find-rulebase *receiver*))
    (setq *target* (find-recipient (name *receiver*)))
    (decludes 'infotheory)
    (empty 'infotheory)
    (if (and library (symbolp library) (boundp library))
        (includes 'infotheory (symbol-value library)))
    (mapc #'(lambda (x) (insert x 'infotheory))
          (contrapositives `(<= (answer ,x) ,p)))
    (when (setq rl (fullresidues `(answer ,x) 'infotheory #'specialtyp))
      (setq p (decolonize (maksor rl)))
      ;(if *collapse* (setq p (collapse x p)))
      ;(if *collapse* (setq p (raisin x p)))
      ;(if *compress* (setq p (compress p)))
      p)))

;;;;

(defmethod plan (*message* *sender* (*receiver* transformer))
  (let ((target (find-target (name *receiver*))))
    (when target `(ask ,(name target) ,(transform *message*)))))

;;;;

(defun basicp (r)
  (and (groundp r) (findp `(base ?a ,r) *manager*)))

(defun materialp (r)
  (and (groundp r) (findp `(material ?a ,r) *manager*)))

;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Old stuff
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; plan
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod plan (*message* *sender* (*receiver* translator))
  (let ((target (find-target (name *receiver*))))
    (when target `(ask ,(name target) ,(translate *message*)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun translate (c)
  (cond ((atom c) c)
        ((eq 'update (car c)) (translateupdate c))
        ((eq 'tell (car c)) (translatetell (cadr c)))
        ((eq 'untell (car c)) (translatetell `(not ,(cadr c)))) ; was unprovable
        ((eq 'eliminate (car c)) c) ; this is not adequate but better than nothing
        ((eq 'ask-if (car c)) (translate-ask-if c))
        ((eq 'ask-one (car c)) (translate-ask-one c))
        ((eq 'ask-all (car c)) (translate-ask-all c))
        ((eq 'ask-answer (car c)) (translate-ask-answer c))
        ((eq 'ask-answers (car c)) (translate-ask-answers c))
        ((eq 'ask-table (car c)) (translate-ask-table c))
        ((eq 'ask-about (car c)) c)
        ((eq 'quote (car c)) c)
        ((macro-function (car c)) (translate (macroexpand c)))
        (t (cons (car c) (mapcar #'(lambda (x) (translate x)) (cdr c))))))

(defun translate-ask-if (x)
  (let (dum)
    (when (setq dum (transask t (cadr x)))
      `(ask-if ,dum))))

(defun translate-ask-one (x)
  (let (dum)
    (when (setq dum (transask (cadr x) (caddr x)))
      `(ask-one ,(cadr x) ,dum))))

(defun translate-ask-all (x)
  (let (dum)
    (when (setq dum (transask (cadr x) (caddr x)))
      `(ask-all ,(cadr x) ,dum))))

(defun translate-ask-answer (x)
  (let (dum)
    (when (setq dum (transaskanswer (cadr x) (caddr x) (cdddr x)))
      `(ask-answer ,(cadr x) ,dum))))

(defun translate-ask-answers (x)
  (let (dum)
    (when (setq dum (transaskanswer (cadr x) (caddr x) (cdddr x)))
      `(ask-answers ,(cadr x)  ,dum))))

(defun translate-ask-table (x)
  (do ((l (caddr x) (cdr l)) (nl))
      ((null l) `(invert-table ,(cons 'list (nreverse nl))))
      (setq nl (cons (translate-ask-column (car l) (cadr x)) nl))))

(defun translate-ask-column (slot items)
  `(extract-values ',items
     (ask-all (?x ?y)
       ,(transask '(?x ?y) `(and (oneof ?x . ,items) (,slot ?x ?y))))))

;;; removed handling of ==>.  Need to reinstall.

(defun translateupdate (msg)
  (let (rulebase *target* (*intensions* t))
    (setq rulebase (find-rulebase *receiver*))
    (setq *target* (find-target (name *receiver*)))
    (decludes 'infotheory)
    (empty 'infotheory)
    (if (and rulebase (symbolp rulebase) (boundp rulebase))
        (includes 'infotheory (eval rulebase)))
    (transupdate (maksand (cdr msg)))))

(defun transupdate (p)
  (do ((l (newconsequences p 'infotheory #'nonviewp #'success) (cdr l)) (nl))
      ((null l) (when nl (cons 'update (nreverse nl))))
      (cond ((and (listp (car l)) (eq 'execute (caar l)))
             (setq nl (cons (cadar l) nl)))
            ((and (listp (car l)) (eq 'evaluate (caar l)))
             (setq nl (cons `(apply ',(caadar l) ',(cdadar l)) nl)))
            ((and (listp (car l)) (eq '=> (caar l)))
             (setq nl (cons (decolonize `(==> ,(cadar l) ,(caddar l))) nl)))
            (t (setq nl (cons (decolonize (car l)) nl))))))

(defun translateupdate (msg)
  (planchange (maksand (cdr msg)) *receiver*))

(defun additem (x dum)
  (cond ((atom x) (rplacd dum (cons x (cdr dum))))
        ((eq 'and (car x)) (dolist (p (cdr x)) (additem p dum)))
        (t (rplacd dum (cons (operator x) (cdr dum))))))

(defun funnyleqp (x y)
  (cond ((eq x y))
        (t (do ((l (cdr (assoc y alist)) (cdr l)))
               ((null l) nil)
               (when (funnyleqp x (car l)) (return t))))))

(defun translatetell (p)
  (cond ((atom p) `(tell ,(transtell p)))
        ((and (eq 'and (car p)) (cddr p) (listp (cadr p))
              (every #'(lambda (x) (and (listp x) (eq (car x) (caadr p)))) (cdr p)))
         (translatetelltable (cdr p) *receiver*))
        (t `(tell ,(transtell p)))))

(defun transtell (p)
  (let (rulebase *target*)
    (setq rulebase (find-rulebase *receiver*))
    (setq *target* (find-target (name *receiver*)))
    (decludes 'infotheory)
    (empty 'infotheory)
    (if (and rulebase (symbolp rulebase) (boundp rulebase))
        (includes 'infotheory (eval rulebase)))
    (do ((l (newconsequences p 'infotheory #'nonviewp #'success) (cdr l)) (nl))
        ((null l) (maksand (nreverse nl)))
      (cond ((and (listp (car l)) (eq 'execute (caar l)))
             (setq nl (cons (cadar l) nl)))
            ((and (listp (car l)) (eq 'evaluate (caar l)))
             (setq nl (cons `(apply ',(caadar l) ',(cdadar l)) nl)))
            ((and (listp (car l)) (eq '=> (caar l)))
             (setq nl (cons (decolonize `(==> ,(cadar l) ,(caddar l))) nl)))
            (t (setq nl (cons (decolonize (car l)) nl)))))))

(defun translatetelltable (sentences receiver)
  (let (pattern rulebase *target* trigger)
    (setq pattern (patternize (car sentences)))
    (setq rulebase (find-rulebase receiver))
    (setq *target* (find-target (name *receiver*)))
    (setq trigger (findx '?x `(telltrigger ,(name receiver) ?x) *manager*))
    (decludes 'infotheory)
    (empty 'infotheory)
    (if (and rulebase (symbolp rulebase) (boundp rulebase))
        (includes 'infotheory (eval rulebase)))
    (do ((l (newconsequences pattern 'infotheory #'nonviewp #'success) (cdr l)) (nl))
        ((null l) `(update . ,(nreverse nl)))
        (cond ((equal pattern (car l))
               (unless (eq trigger 'no)
                 (setq nl (cons `(==> (oneof ,pattern . ,sentences) ,(decolonize (car l))) nl))))
              ((and (listp (car l)) (eq 'execute (caar l)))
               (setq nl (cons `(==> (oneof ,pattern . ,sentences) ,(cadar l)) nl)))
              ((and (listp (car l)) (eq 'evaluate (caar l)))
               (setq nl (cons `(==> (oneof ,pattern . ,sentences) (apply ',(caadar l) ',(cdadar l))) nl)))
              ((and (listp (car l)) (eq '=> (caar l)))
               (setq nl (cons (decolonize `(==> (and (oneof ,pattern . ,sentences) ,(cadar l)) ,(caddar l))) nl)))
              (t (setq nl (cons `(==> (oneof ,pattern . ,sentences) ,(decolonize (car l))) nl)))))))

(defun patternize (p)
  (cond ((atom p) p)
        ((eq 'not (car p)) (maknot (patternize (cadr p))))
        ((eq 'unprovable (car p)) `(unprovable ,(patternize (cadr p))))
        (t (do ((l (cdr p) (cdr l)) (vl))
               ((null l) (cons (car p) (nreverse vl)))
               (setq vl (cons (decolonize (newindvar)) vl))))))

(defun transask (x p)
  (let ((*ancestry* 1) (*library*) (*target*) (rl))
    (setq *library* (find-rulebase *receiver*))
    (setq *target* (find-target (name *receiver*)))
    (decludes 'infotheory)
    (empty 'infotheory)
    (if (and *library* (symbolp *library*) (boundp *library*))
        (includes 'infotheory (eval *library*)))
    (mapc #'(lambda (x) (save x 'infotheory))
          (contrapositives `(<= (answer ,x) ,p)))
    (cond ((setq rl (residues x `(answer ,x) 'infotheory #'nonviewp))
           (setq p (decolonize (maksor rl)))
           (if *collapse* (setq p (collapse x p)))
           (if *collapse* (setq p (raisin x p)))
           (if *compress* (setq p (compress p)))
           p))))

(defun transaskanswer (x p s)
  (let ((*ancestry* 1) (*library*) (*target*) (rl))
    (setq *library* (find-rulebase *receiver*))
    (setq *target* (find-target (name *receiver*)))
    (decludes 'infotheory)
    (empty 'infotheory)
    (if (and *library* (symbolp *library*) (boundp *library*))
        (includes 'infotheory (eval *library*)))
    (mapc #'(lambda (x) (save x 'infotheory))
          (contrapositives `(<= (answer ,x) ,p)))
    (mapc #'(lambda (x) (save x 'infotheory)) s)
    (cond ((setq rl (residues x `(answer ,x) 'infotheory #'nonviewp))
           (setq p (decolonize (maksor rl)))
           (if *collapse* (setq p (collapse x p)))
           (if *collapse* (setq p (raisin x p)))
           (if *compress* (setq p (compress p)))
           p))))

;;;;

(defmethod message-handler (*message* *sender* (*receiver* translator))
  (cond ((atom *message*))
        ((eq 'tell (car *message*)) (sendall (translate *message*) *receiver*))
        ((eq 'untell (car *message*)) (sendall (translate *message*) *receiver*))
        ((eq 'eliminate (car *message*)) (sendall (translate *message*) *receiver*))
        ((eq 'update (car *message*)) (eval *message*))
        ((eq 'updates (car *message*)) (eval *message*))
        ((eq 'ask-if (car *message*)) (eval *message*))
        ((eq 'ask-one (car *message*)) (eval *message*))
        ((eq 'ask-all (car *message*)) (eval *message*))
        ((eq 'ask-table (car *message*)) (sendone (translate *message*) *receiver*))
        ((eq 'ask-about (car *message*)) (sendone (translate *message*) *receiver*))
        ((eq 'quote (car *message*)) *message*)
        ((macro-function (car *message*)) (sendone (translate *message*) *receiver*))
        ((fboundp (car *message*)) (sendone (translate *message*) *receiver*))))

;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Old Stuff
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; plan
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod plan (msg sender receiver)
  (declare (ignore sender receiver))
  msg)

(defmethod plan (msg sender (receiver symbol))
  (cond ((and (boundp receiver) (not (eq receiver (symbol-value receiver))))
         (plan msg sender (symbol-value receiver)))
        (t (call-next-method msg sender receiver))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; facilitate
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun facilitate (msg sender receiver)
  (cond ((atom msg) msg)
        ;((eq (car msg) 'tell) (assume (maksand (cdr msg)) receiver))
        ;((eq (car msg) 'untell) (forget (cadr msg) receiver))
        ;((eq (car msg) 'update) (change (maksand (cdr msg)) receiver))
        ;((eq (car msg) 'establish) (create (cadr msg) (caddr msg) receiver))
        ;((eq (car msg) 'eliminate) (discardall (maknot (cadr msg)) sender receiver))
        ;((eq (car msg) 'ask-if) (findx t (cadr msg) receiver))
        ;((eq (car msg) 'ask-one) (findx (cadr msg) (caddr msg) receiver))
        ;((eq (car msg) 'ask-all) (finds (cadr msg) (caddr msg) receiver))
        ((eq (car msg) 'ask-table) (asktable (cadr msg) (caddr msg) sender receiver))
        ((eq (car msg) 'ask-about) (askabout (cadr msg) sender receiver))
        ((eq (car msg) 'quote) (cadr msg))
        ((macro-function (car msg)) (facilitate (macroexpand msg) sender receiver))
        ((fboundp (car msg))
         (apply (car msg) (mapcar #'(lambda (x) (facilitate x sender receiver)) (cdr msg))))))

(defmethod insert (p (th facilitator))
  (do ((l (finds '?a `(interest ?a ,(operator p)) *manager*) (cdr l))
       (msg `(tell ,p)) (dum))
      ((null l) 'done)
      (cond ((and (eq (car l) *sender*) (equal msg *message*)))
            ((stringp (setq dum (request msg *receiver* (car l))))
             (throw 'assume dum)))))

(defmethod uninsert (p (th facilitator))
  (do ((l (finds '?a `(interest ?a ,(operator p)) *manager*) (cdr l))
       (msg `(untell ,p)) (dum))
      ((null l) 'done)
      (cond ((and (eq (car l) *sender*) (equal msg *message*)))
            ((stringp (setq dum (request msg *receiver* (car l))))
             (throw 'forget dum)))))

(defmethod envindexps (p al (th facilitator))
  (setq p (plugstdexp p al))
  (do ((l (finds '?a `(specialty ?a ,(operator p)) *manager*) (cdr l)) (nl))
      ((null l) (nreverse nl))
      (setq nl (nreconc (request `(ask-all ,p ,p) *receiver* (car l)) nl))))

(defun find-agents ()
  (cons (name *library*)
        (cons (name *warehouse*) (finds '?x '(isa ?x agent) *manager*))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod assume (p (th facilitator))
  (change p th))

(defmethod forget (p (th facilitator))
  (change (maknot p) th))

(defmethod change (p (th facilitator))
  (let (updates library (*ancestry* 1))
    (setq library (get-rulebase th))
    (decludes 'infotheory)
    (empty 'infotheory)
    (includes 'infotheory library)
    (setq updates (newconsequences p 'infotheory #'specialp #'success))
    (setq updates (nreverse (newchanges (cons 'update updates) *target* nil)))
    (setq updates (make-exports updates))
    (eval (reassemble updates))))

(defmethod discardall (x sender (receiver facilitator))
  (do ((l *agents* (cdr l)) (nl))
      ((null l) nl)
      (cond ((and (eq (car l) sender) (listp *message*)
                  (eq (car *message*) 'eliminate) (equalp (cadr *message*) x)))
            ((eq (car l) receiver))
            (t (setq nl (unionize nl (request `(eliminate ,x) receiver (car l))))))))

(defun newchanges (p th nl)
  (cond ((atom p) (adjoin p nl))
        ((eq '=> (car p))
         (dolist (item (request `(ask-all ,(caddr p) ,(cadr p)) nil th))
           (setq nl (contribute item nl)))
         nl)
        ((eq 'and (car p))
         (dolist (item (cdr p)) (setq nl (newchanges item th nl))) nl)
        ((eq 'update (car p))
         (dolist (item (cdr p)) (setq nl (newchanges item th nl))) nl)
        (t (adjoin p nl :test #'equalp))))

(defun filter-exports (facts sender security)
  (request `(ask-all (tell ?b ?p) (and (oneof ?p . ,facts)
                                       (publishable ,(name sender) ?b ?p)))
           nil security))

(defun make-exports (facts)
  (do ((l facts (cdr l)) (nl))
      ((null l) (nreverse nl))
      (do ((m (finds '?a `(interest ?a ,(operator (car l))) *manager*) (cdr m)))
          ((null m))
          (setq nl (cons `(tell ,(car m) ,(car l)) nl)))))

;;;;

(defun facilitateask (x p receiver)
  (let (library residues (*ancestry* 1))
    (setq library (get-rulebase receiver))
    (decludes 'infotheory)
    (empty 'infotheory)
    (includes 'infotheory library)
    (dolist (x (contrapositives `(<= (answer ,x) ,p))) (insert x 'infotheory))
    (when (setq residues (fullresidues `(answer ,x) 'infotheory #'sidelinep #'specialp))
      (setq p (decolonize (maksor residues)))
      (if *collapse* (setq p (collapse x p)))
      ;(if *collapse* (setq p (raisin x p)))
      p)))

;;;;

(defun guessmetafacts (data warehouse interface)
  (let (predicates attributes tables dum class slot metadata)
  (dolist (sent data)
    (cond ((atom sent))
          ((find (car sent) '(<= => unprovable not and or)))
          ((basep (car sent)))
          ((and (cdr sent) (null (cddr sent)))
           (setq predicates (adjoin (car sent) predicates :test #'eq)))
          ((and (cddr sent) (null (cdddr sent)))
           (setq attributes (adjoin (car sent) attributes :test #'eq)))
          (t (setq tables (adjoin (car sent) tables :test #'eq)))))
  (dolist (predicate (nreverse predicates))
    (setq dum (disconstruct predicate))
    (setq class (newfix (car dum) (cadr dum)))
    (setq metadata (cons `(isa ,class class) metadata))
    (when (findp `(isa ,(cadr dum) class) *manager*)
      (setq metadata (cons `(superclass ,class ,(cadr dum)) metadata)))
    (setq metadata (cons `(predicate ,class ,predicate) metadata))
    (setq metadata (cons `(isa ,predicate naryrelation) metadata))
    (setq metadata (cons `(arity ,predicate 1) metadata))
    (setq metadata (cons `(specialty ,warehouse ,predicate) metadata))
    (setq metadata (cons `(interest ,warehouse ,predicate) metadata))
    (setq metadata (cons `(rootclass ,interface ,class) metadata)))
  (dolist (attribute (nreverse attributes))
    (setq dum (disconstruct attribute))
    (setq class (newfix (car dum) (cadr dum)))
    (setq slot (newfix (cadr dum) (caddr dum)))
    (setq metadata (cons `(attribute ,class ,attribute) metadata))
    (setq metadata (cons `(isa ,attribute attributerelation) metadata))
    (when (findp `(isa ,slot attributerelation) *manager*)
      (setq metadata (cons `(superrelation ,attribute ,slot) metadata)))
    (setq metadata (cons `(domain ,attribute ,class) metadata))
    (setq metadata (cons `(specialty ,warehouse ,attribute) metadata))
    (setq metadata (cons `(interest ,warehouse ,attribute) metadata))
    (setq metadata (cons `(searchstyle ,attribute dropdownlist) metadata)))
  (nreverse metadata)))

;;;;

(defmethod setvalue ((slot (eql 'indexing)) obj val)
  (let (sentences)
    (when (and (symbolp obj) (boundp obj)
               (typep (setq obj (symbol-value obj)) 'dataserver)
               (not (eq (indexing obj) val)))
      (setq sentences (contents obj))
      (empty obj)
      (setf (indexing obj) val)
      (define-theory obj "" sentences))))

(defmethod setvalue ((slot (eql 'inference)) obj val)
  (let (sentences)
    (when (and (symbolp obj) (boundp obj)
               (typep (setq obj (symbol-value obj)) 'dataserver)
               (not (eq (inference obj) val)))
      (setq sentences (contents obj))
      (empty obj)
      (setf (inference obj) val)
      (define-theory obj "" sentences))))

(defmethod setvalue ((slot (eql 'meta)) obj val)
  (when (and (symbolp obj) (boundp obj)
             (typep (setq obj (symbol-value obj)) 'dataserver))
      (setf (meta obj) (if (eq val 'dynamic) t))))

(defmethod setvalue ((slot (eql 'meta)) obj val)
  (when (eq val 'dynamic) (introspect obj)))

(defmethod setvalue ((slot (eql 'specialty)) obj val)
  (when (and (symbolp obj) (boundp obj) (typep (symbol-value obj) 'theory))
    (extension val (symbol-value obj))))

(defmethod remvalue ((slot (eql 'specialty)) obj val)
  (when (and (symbolp obj) (boundp obj) (typep (symbol-value obj) 'theory))
    (unextension val (symbol-value obj))))

(defmethod setvalue ((slot (eql 'expertise)) obj val)
  (when (and (symbolp obj) (boundp obj) (typep (symbol-value obj) 'theory))
    (intension val (symbol-value obj))))

(defmethod remvalue ((slot (eql 'expertise)) obj val)
  (when (and (symbolp obj) (boundp obj) (typep (symbol-value obj) 'theory))
    (unintension val (symbol-value obj))))

;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Compression
;;; Need to cache results
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun compress (x)
  (cond ((atom x) x)
        ((eq 'and (car x)) x)
        ((eq 'or (car x)) (compressor x))
        (t x)))

(defun compressor (x)
  (do ((l (cdr x) (cdr l)) (dum) (nl))
      ((null l)
       (maksor
        (mapcar #'(lambda (x) (makand (car x) (compress (cdr x)))) (nreverse nl))))
      (cond ((setq dum (assoc (head (car l)) nl :test #'equal))
             (rplacd dum (makor (cdr dum) (tail (car l)))))
            (t (setq nl (acons (head (car l)) (tail (car l)) nl))))))

(defun maklet (vars steps)
  (cond ((null vars)
         (cond ((null steps) nil)
               ((null (cdr steps)) (car steps))
               (t (cons 'progn steps))))
        ((null steps) nil)
        (t `(let ,vars . ,steps))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Targeting for tells
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun target1 (p)
  (do ((l (find-interestees (operator p)) (cdr l)) (nl))
      ((null l) (maksprogn (nreverse nl)))
      (setq nl (cons `(ask ,(list (car l)) (tell ,p)) nl))))

(defun target2 (p q)
  (let (source target)
    (setq source (sources p))
    (setq target (targets q))
    (cond ((and (eq 'ask (car source)) (eq 'tell (car target))
                (subsetp (cadr target) (cadr source)))
           (do ((l (cadr target) (cdr l)) (nl))
               ((null l) (maksprogn (nreverse nl)))
               (setq nl (cons `(ask ,(list (car l)) (tell ,q ,p)) nl))))
          (t `(tellemall ',target ',(remedy source))))))

(defun targets (p)
  (cond ((atom p) `(tell ,(find-interestees p) ,p))
        ((eq 'and (car p)) (target-and p))
        ((eq 'not (car p))
         (setq p (targets (cadr p)))
         (cond ((eq 'tell (car p)) `(tell ,(cadr p) (not ,(caddr p))))
               (t `(not ,p))))
        ((eq 'unprovable (car p))
         (setq p (targets (cadr p)))
         (cond ((eq 'tell (car p)) `(tell ,(cadr p) (unprovable ,(caddr p))))
               (t `(unprovable ,p))))
        (t `(tell ,(find-interestees (car p)) ,p))))

(defun target-and (p)
  (do ((l (mapcar #'targets (cdr p))) (nl))
      ((null l) (maksprogn (nreverse nl)))
      (cond ((eq 'tell (caar l))
             (do ((m (cdr l) (cdr m)) (nm (list (caddar l))))
                 ((null m)
                  (setq nl (cons `(tell ,(cadar l) ,(maksand (nreverse nm))) nl))
                  (setq l nil))
                 (cond ((and (eq 'tell (caar m))
                             (subsetp (cadar l) (cadar m))
                             (subsetp (cadar m) (cadar l)))
                        (setq nm (cons (caddar m) nm)))
                       (t (setq nl (cons `(tell ,(cadar l) ,(maksand (nreverse nm))) nl))
                          (setq l m)
                          (return t)))))
            (t (setq nl (cons (car l) nl) l (cdr l))))))

(defun find-interestees (x)
  (cond ((eq 'true x) nil)
        ((eq 'false x) nil)
        ((basep x) nil)
        (t (finds '?a `(interest ?a ,x) *manager*))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; chore
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun make-chore (obj)
  (let (basetime period operation)
    (when (and (or (not (boundp obj)) (not (processp obj)))
               (setq basetime (or (findx '?x `(basetime ,obj ?x) *manager*) 0))
               (setq period (or (findx '?x `(period ,obj ?x) *manager*)
                                (finds '?x `(runday ,obj ?x) *manager*)))
               (setq operation (findx '?x `(operation ,obj ?x) *manager*)))
      (set obj (process-run-function (prettify obj)
                 #'runchore basetime period operation)))))

(defun kill-chore (obj)
  (when (and (boundp obj) (processp (symbol-value obj)))
    (process-kill (referent obj))
    (makunbound obj)))

(defun reset-chore (obj)
  (kill-chore obj)
  (make-chore obj))

(defun runchore (basetime period operation)
  (let (now)
    (setq now (get-universal-time))
    (unless (< basetime now) (sleep (- basetime now)))
    (cond ((numberp period)
           (loop (funcall operation)
                 (setq now (get-universal-time))
                 (sleep (- period (mod (- now basetime) period)))))
          ((listp period)
           (loop (setq now (read-from-string (current-day)))
                 (when (find now period) (funcall operation))
                 (setq now (get-universal-time))
                 (sleep (- 86400 (mod (- now basetime) 86400))))))))


    (isa           chore class)
    (superclass    chore thing)
    (attribute     chore basetime)
    (attribute     chore period)
    (attribute     chore runday)
    (attribute     chore operation)

    (isa           basetime attributerelation)
    (superrelation basetime true)
    (arity         basetime 2)
    (domain        basetime chore)
    (range         basetime number)
    (unique        basetime yes)
    (total         basetime yes)
    (changestyle   basetime typein)
    (searchstyle   basetime interval)
    (comparestyle  basetime glyph)
    (inspectstyle  basetime glyph)
    (documentation basetime "Time (in UTC) afer which chore becomes runnable.")
  
    (isa           period attributerelation)
    (superrelation period true)
    (arity         period 2)
    (domain        period chore)
    (range         period number)
    (unique        period yes)
    (total         period yes)
    (changestyle   period typein)
    (searchstyle   period interval)
    (comparestyle  period glyph)
    (inspectstyle  period glyph)
    (documentation period "Number of seconds between invocations of a chore.")

    (isa           runday attributerelation)
    (superrelation runday true)
    (arity         runday 2)
    (domain        runday chore)
    (range         runday weekday)
    (unique        runday no)
    (total         runday no)
    (changestyle   runday dropdownlist)
    (searchstyle   runday dropdownlist)
    (comparestyle  runday glyph)
    (inspectstyle  runday glyph)
    (option        runday unknown)
    (documentation runday "Day on which a chore is done.  Alternative to period.")

    (isa           operation attributerelation)
    (superrelation operation true)
    (arity         operation 2)
    (domain        operation chore)
    (range         operation actioninvocation)
    (unique        operation no)
    (total         operation yes)
    (changestyle   operation dropdownlist)
    (searchstyle   operation dropdownlist)
    (comparestyle  operation glyph)
    (inspectstyle  operation glyph)
    (documentation operation "Operation performed by a chore.")

;;;;

(defparameter *types*
  '((domain isanumber number)
    (domain integer number)
    (domain real-number number)
    (domain complex-number number)
    (domain number number)
    (domain natural number)
    (domain rational-number number)
    (domain positive number)
    (domain negative number)
    (domain zero number)
    (domain odd-integer number)
    (domain even-integer number)
    (domain logbit number number) 
    (domain logtest number number)
    (domain > number number)
    (domain >= number number)
    (domain =< number number)
    (domain < number number)
    
    (domain * number number number) 
    (domain + number number number) 
    (domain - number number number) 
    (domain / number number number) 
    (domain 1+ number number) 
    (domain 1- number number) 
    (domain denominator number number) 
    (domain floor number number number) 
    (domain numerator number number) 
    (domain rem number number number) 
    
    (domain isacharacter character)
    (domain alphabetic character)
    (domain uppercase character)
    (domain lowercase character)
    (domain digit character)
    (domain alphanumeric character)
    (domain chargreater character character)
    (domain charless character character)
    
    (domain isastring string)
    (domain stringalphanumeric string string)
    (domain stringappend string string string)
    (domain stringcapitalize string string)
    (domain stringcharpos character string number)
    (domain stringdowncase string string)
    (domain stringelement string number character)
    (domain stringlength string number)
    (domain stringposition string string number)
    (domain stringsubseq string number number string)
    (domain stringsubstitute character character string)
    (domain stringupcase string string)))

;;;;

(defmethod initialize ((target (eql 'types)))
  (define-theory target "" *types*)
  'done)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Integrator Tests
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Manager
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(restartsystem)
*

(equal (contents *manager*) *sentences*)
T

(save '(isa test authorizer) *manager*)
(ISA TEST AUTHORIZER)

(not (null (typep test 'authorizer)))
T

(save '(isa test facilitator) *manager*)
(ISA TEST FACILITATOR)

(not (null (typep test 'facilitator)))
T

(save '(isa test transformer) *manager*)
(ISA TEST TRANSFORMER)

(not (null (typep test 'transformer)))
T

(save '(isa test infoserver) *manager*)
(ISA TEST INFOSERVER)

(not (null (typep test 'infoserver)))
T

(save '(isa test fileserver) *manager*)
(ISA TEST FILESERVER)

(not (null (typep test 'fileserver)))
T

(save '(isa test counter) *manager*)
(ISA TEST COUNTER)

(not (null (typep test 'counter)))
T

(plan '(ask-all ?x (isa ?x class)) nil *manager*)
(ASK-ALL ?X (ISA ?X CLASS))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Facilitator
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; something here to register the manager

(plan '(ask-all ?x (isa ?x class)) nil *facilitator*)
;(ask manager (ASK-ALL ?X (ISA ?X CLASS)))
NIL

(null (request '(ask-all ?x (isa ?x class)) nil *facilitator*))
;NIL
T

#|
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Test data
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defparameter *data*
  '((circulon.table b07981 type sauce-pan)
    (circulon.table b07981 material aluminum)
    (circulon.table b07981 interior teflon)
    (circulon.table b07981 exterior metal)
    (circulon.table b07981 color grey)
    (circulon.table b07981 capacity 3)
    (circulon.table b07981 upc "079008007981")
    (circulon.table b07981 manufacturer circulon)
    
    (circulon.table b07983 type skillet)
    (circulon.table b07983 material aluminum)
    (circulon.table b07983 interior teflon)
    (circulon.table b07983 exterior metal)
    (circulon.table b07983 color grey)
    (circulon.table b07983 diameter 10)
    (circulon.table b07983 upc "079008007983")
    (circulon.table b07983 manufacturer circulon)
    
    (circulon.table b05817 type skillet)
    (circulon.table b05817 material aluminum)
    (circulon.table b05817 interior teflon)
    (circulon.table b05817 exterior metal)
    (circulon.table b05817 color grey)
    (circulon.table b05817 diameter 12)
    (circulon.table b05817 upc "079008005817")
    (circulon.table b05817 manufacturer circulon)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Facilitator test
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(definemore *manager*
  '((interest warehouse n)
    (interest warehouse r)
    (interest warehouse s)
    (specialty warehouse p)
    (specialty warehouse q)))

(definemore *warehouse*
  '((p a d)
    (p a e)
    (p c f)
    (q a g)
    (q b h)
    (q c i)))

(definemore *library*
  '((=> (m ?x) (n ?x))
    (=> (m ?x) (p ?x ?y) (r ?x ?y))
    (=> (m ?x) (q ?x ?z) (s ?z))))

(setq *test*
      '(tell (and (==> (ask library (oneof ?x a b c)) (tell warehouse (n ?x)))
                  (==> (ask warehouse (and (oneof ?x a b c) (p ?x ?y))) (tell warehouse (r ?x ?y)))
                  (==> (ask warehouse (and (oneof ?x a b c) (q ?x ?z))) (tell warehouse (s ?z))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Transformer test
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(definemore *manager*
  '((isa elham transformer)
    (rules  elham library)
    (datarepository elham manager)
    (interest manager isa)
    (interest manager interest)
    (interest manager specialty)
    (isa steve translationagent)
    (rules  steve library)
    (datarepository steve tricia)
    (isa trish transformer)
    (rules  trish metalibrary)
    (datarepository trish circulondb)))

(definemore *library*
  '((=> (circulon.table ?x ?r ?y)
        (isa ?r attributerelation))
    
    (=> (circulon.table ?x ?r ?y)
        (interest circulondb ?r))
    
    (=> (circulon.table ?x ?r ?y)
        (specialty circulondb ?r))
    
    (=> (circulon.table ?x ?r ?y)
        (?r ?x ?y))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; translationagent test
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(definemore *manager*
  '((isa elham translationagent)
    (rules  elham metalibrary)
    (telltrigger elham no)
    (datarepository elham manager)

    (isa trish translationagent)
    (rules  trish library)
    (telltrigger trish no)
    (datarepository trish circulondb)

    (isa circulondb dataserver)
    (kind circulondb dataserver)))

(definemore *metalibrary*
  '((=> (circulon.table ?x ?r ?y)
        (isa ?r attributerelation))
    
    (=> (circulon.table ?x ?r ?y)
        (interest circulondb ?r))
    
    (=> (circulon.table ?x ?r ?y)
        (specialty circulondb ?r))))

(definemore *library*
  '((=> (circulon.table ?x ?r ?y)
        (?r ?x ?y))))

(pprint (plan `(tell-table . ,*data*) nil elham))
(pprint (plan `(tell-table . ,*data*) nil trish))
(request `(tell-table . ,*data*) nil elham)
(show 'circulondb *manager*)
(request `(tell-table . ,*data*) nil trish)
(show '? circulondb)

|#

;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; (c) Copyright 1999-2003 by Michael Genesereth.  All rights reserved.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(eval-when (compile load eval)
  (proclaim '(special *metalibrary* *manager* *library* *warehouse*
                      *theory* *ancestry* *saves* *assumables* *test*
                      *order* *collapse* *host* *port* 
                      *message* *sender* *receiver* *target* *agent*)))

(defparameter *plan* nil)
(defparameter *agents* nil)
(defparameter *collapse* 'simple)
(defparameter *compress* nil)

(defmethod plan (msg sender receiver)
  (declare (ignore sender receiver))
  msg)

(defmethod plan (msg sender (receiver symbol))
  (cond ((and (boundp receiver) (not (eq receiver (symbol-value receiver))))
         (plan msg sender (symbol-value receiver)))
        (t (call-next-method msg sender receiver))))

(defmethod save (p (th symbol) &optional (f #'samep))
  (cond ((and (boundp th) (not (symbolp (symbol-value th))))
         (save p (symbol-value th) f))
        (t (call-next-method p th f))))

(defmethod drop (p (th symbol) &optional (f #'samep))
  (cond ((and (boundp th) (not (symbolp (symbol-value th))))
         (drop p (symbol-value th) f))
        (t (call-next-method p th f))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; translator
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass translator (agent) ())

(defmethod create (obj (type (eql 'translator)))
  (unless (and (boundp obj) (typep (symbol-value obj) 'translator))
    (set obj (make-instance 'translator :name obj))))

(defmethod destroy (obj (type (eql 'translator)))
  (when (and (symbolp obj) (boundp obj) (typep (symbol-value obj) 'translator))
    (makunbound obj)))

(defmethod plan (*message* *sender* (*receiver* translator))
  (let ((target (find-target (name *receiver*))))
    (when target `(ask ,(name target) ,(translate *message*)))))

(defmethod message-handler (*message* *sender* (*receiver* translator))
  (cond ((atom *message*))
        ((eq 'update (car *message*)) (request (translate *message*) *sender* (find-target *receiver*)))
        ((eq 'tell (car *message*)) (sendall (translate *message*) *receiver*))
        ((eq 'untell (car *message*)) (sendall (translate *message*) *receiver*))
        ((eq 'eliminate (car *message*)) (sendall (translate *message*) *receiver*))
        ((eq 'ask-if (car *message*)) (sendone (translate *message*) *receiver*))
        ((eq 'ask-one (car *message*)) (sendone (translate *message*) *receiver*))
        ((eq 'ask-all (car *message*)) (sendone (translate *message*) *receiver*))
        ((eq 'ask-table (car *message*)) (sendone (translate *message*) *receiver*))
        ((eq 'ask-about (car *message*)) (sendone (translate *message*) *receiver*))
        ((eq 'quote (car *message*)) *message*)
        ((macro-function (car *message*)) (sendone (translate *message*) *receiver*))
        ((fboundp (car *message*)) (sendone (translate *message*) *receiver*))))

(defun sendone (msg receiver)
  (let ((target (find-target (name receiver))))
    (when target
      ;(drop `(datarepository ,(name receiver) ,target) *manager*) ; round robin
      ;(save `(datarepository ,(name receiver) ,target) *manager*) ; round robin
      (request msg *receiver* target))))

(defun sendall (msg receiver)
  (let (targets updates errors)
    (setq targets (find-targets (name receiver)))
    (setq updates (changes (maksand (cdr msg)) (car targets)))
    (dolist (item updates)
      (when (and (listp item) (eq (car item) 'error))
        (setq errors (cons (caddr item) errors))))
    (cond (errors)
          ((atom msg))
          (t (setq msg (list (car msg) (maksand updates)))
             (dolist (target targets) (request msg *sender* target))
             'done))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun translate (c)
  (cond ((atom c) c)
        ((eq 'update (car c)) (translateupdate c))
        ((eq 'tell (car c)) (translatetell (cadr c)))
        ((eq 'untell (car c)) (translatetell `(not ,(cadr c)))) ; was unprovable
        ((eq 'eliminate (car c)) c) ; this is not adequate but better than nothing
        ((eq 'ask-if (car c)) (translate-ask-if c))
        ((eq 'ask-one (car c)) (translate-ask-one c))
        ((eq 'ask-all (car c)) (translate-ask-all c))
        ((eq 'ask-answer (car c)) (translate-ask-answer c))
        ((eq 'ask-answers (car c)) (translate-ask-answers c))
        ((eq 'ask-table (car c)) (translate-ask-table c))
        ((eq 'ask-about (car c)) c)
        ((eq 'quote (car c)) c)
        ((macro-function (car c)) (translate (macroexpand c)))
        (t (cons (car c) (mapcar #'(lambda (x) (translate x)) (cdr c))))))

(defun translate-ask-if (x)
  (let (dum)
    (when (setq dum (transask t (cadr x)))
      `(ask-if ,dum))))

(defun translate-ask-one (x)
  (let (dum)
    (when (setq dum (transask (cadr x) (caddr x)))
      `(ask-one ,(cadr x) ,dum))))

(defun translate-ask-all (x)
  (let (dum)
    (when (setq dum (transask (cadr x) (caddr x)))
      `(ask-all ,(cadr x) ,dum))))

(defun translate-ask-answer (x)
  (let (dum)
    (when (setq dum (transaskanswer (cadr x) (caddr x) (cdddr x)))
      `(ask-answer ,(cadr x) ,dum))))

(defun translate-ask-answers (x)
  (let (dum)
    (when (setq dum (transaskanswer (cadr x) (caddr x) (cdddr x)))
      `(ask-answers ,(cadr x)  ,dum))))

(defun translate-ask-table (x)
  (do ((l (caddr x) (cdr l)) (nl))
      ((null l) `(invert-table ,(cons 'list (nreverse nl))))
      (setq nl (cons (translate-ask-column (car l) (cadr x)) nl))))

(defun translate-ask-column (slot items)
  `(extract-values ',items
     (ask-all (?x ?y)
       ,(transask '(?x ?y) `(and (oneof ?x . ,items) (,slot ?x ?y))))))

;;; removed handling of ==>.  Need to reinstall.

(defun translateupdate (msg)
  (let (rulebase *target* (*intensions* t))
    (setq rulebase (find-rulebase *receiver*))
    (setq *target* (find-target (name *receiver*)))
    (decludes 'infotheory)
    (empty 'infotheory)
    (if (and rulebase (symbolp rulebase) (boundp rulebase))
        (includes 'infotheory (eval rulebase)))
    (transupdate (maksand (cdr msg)))))

(defun transupdate (p)
  (do ((l (newconsequences p 'infotheory #'nonviewp #'success) (cdr l)) (nl))
      ((null l) (when nl (cons 'update (nreverse nl))))
      (cond ((and (listp (car l)) (eq 'execute (caar l)))
             (setq nl (cons (cadar l) nl)))
            ((and (listp (car l)) (eq 'evaluate (caar l)))
             (setq nl (cons `(apply ',(caadar l) ',(cdadar l)) nl)))
            ((and (listp (car l)) (eq '=> (caar l)))
             (setq nl (cons (decolonize `(==> ,(cadar l) ,(caddar l))) nl)))
            (t (setq nl (cons (decolonize (car l)) nl))))))

(defun translateupdate (msg)
  (planchange (maksand (cdr msg)) *receiver*))

(defun additem (x dum)
  (cond ((atom x) (rplacd dum (cons x (cdr dum))))
        ((eq 'and (car x)) (dolist (p (cdr x)) (additem p dum)))
        (t (rplacd dum (cons (operator x) (cdr dum))))))

(defun funnyleqp (x y)
  (cond ((eq x y))
        (t (do ((l (cdr (assoc y alist)) (cdr l)))
               ((null l) nil)
               (when (funnyleqp x (car l)) (return t))))))

(defun translatetell (p)
  (cond ((atom p) `(tell ,(transtell p)))
        ((and (eq 'and (car p)) (cddr p) (listp (cadr p))
              (every #'(lambda (x) (and (listp x) (eq (car x) (caadr p)))) (cdr p)))
         (translatetelltable (cdr p) *receiver*))
        (t `(tell ,(transtell p)))))

(defun transtell (p)
  (let (rulebase *target*)
    (setq rulebase (find-rulebase *receiver*))
    (setq *target* (find-target (name *receiver*)))
    (decludes 'infotheory)
    (empty 'infotheory)
    (if (and rulebase (symbolp rulebase) (boundp rulebase))
        (includes 'infotheory (eval rulebase)))
    (do ((l (newconsequences p 'infotheory #'nonviewp #'success) (cdr l)) (nl))
        ((null l) (maksand (nreverse nl)))
      (cond ((and (listp (car l)) (eq 'execute (caar l)))
             (setq nl (cons (cadar l) nl)))
            ((and (listp (car l)) (eq 'evaluate (caar l)))
             (setq nl (cons `(apply ',(caadar l) ',(cdadar l)) nl)))
            ((and (listp (car l)) (eq '=> (caar l)))
             (setq nl (cons (decolonize `(==> ,(cadar l) ,(caddar l))) nl)))
            (t (setq nl (cons (decolonize (car l)) nl)))))))

(defun translatetelltable (sentences receiver)
  (let (pattern rulebase *target* trigger)
    (setq pattern (patternize (car sentences)))
    (setq rulebase (find-rulebase receiver))
    (setq *target* (find-target (name *receiver*)))
    (setq trigger (findx '?x `(telltrigger ,(name receiver) ?x) *manager*))
    (decludes 'infotheory)
    (empty 'infotheory)
    (if (and rulebase (symbolp rulebase) (boundp rulebase))
        (includes 'infotheory (eval rulebase)))
    (do ((l (newconsequences pattern 'infotheory #'nonviewp #'success) (cdr l)) (nl))
        ((null l) `(update . ,(nreverse nl)))
        (cond ((equal pattern (car l))
               (unless (eq trigger 'no)
                 (setq nl (cons `(==> (oneof ,pattern . ,sentences) ,(decolonize (car l))) nl))))
              ((and (listp (car l)) (eq 'execute (caar l)))
               (setq nl (cons `(==> (oneof ,pattern . ,sentences) ,(cadar l)) nl)))
              ((and (listp (car l)) (eq 'evaluate (caar l)))
               (setq nl (cons `(==> (oneof ,pattern . ,sentences) (apply ',(caadar l) ',(cdadar l))) nl)))
              ((and (listp (car l)) (eq '=> (caar l)))
               (setq nl (cons (decolonize `(==> (and (oneof ,pattern . ,sentences) ,(cadar l)) ,(caddar l))) nl)))
              (t (setq nl (cons `(==> (oneof ,pattern . ,sentences) ,(decolonize (car l))) nl)))))))

(defun patternize (p)
  (cond ((atom p) p)
        ((eq 'not (car p)) (maknot (patternize (cadr p))))
        ((eq 'unprovable (car p)) `(unprovable ,(patternize (cadr p))))
        (t (do ((l (cdr p) (cdr l)) (vl))
               ((null l) (cons (car p) (nreverse vl)))
               (setq vl (cons (decolonize (newindvar)) vl))))))

(defun transask (x p)
  (let ((*ancestry* 1) (*library*) (*target*) (rl))
    (setq *library* (find-rulebase *receiver*))
    (setq *target* (find-target (name *receiver*)))
    (decludes 'infotheory)
    (empty 'infotheory)
    (if (and *library* (symbolp *library*) (boundp *library*))
        (includes 'infotheory (eval *library*)))
    (mapc #'(lambda (x) (save x 'infotheory))
          (contrapositives `(<= (answer ,x) ,p)))
    (cond ((setq rl (residues x `(answer ,x) 'infotheory #'nonviewp))
           (setq p (decolonize (maksor rl)))
           (if *collapse* (setq p (collapse x p)))
           (if *collapse* (setq p (raisin x p)))
           (if *compress* (setq p (compress p)))
           p))))

(defun transaskanswer (x p s)
  (let ((*ancestry* 1) (*library*) (*target*) (rl))
    (setq *library* (find-rulebase *receiver*))
    (setq *target* (find-target (name *receiver*)))
    (decludes 'infotheory)
    (empty 'infotheory)
    (if (and *library* (symbolp *library*) (boundp *library*))
        (includes 'infotheory (eval *library*)))
    (mapc #'(lambda (x) (save x 'infotheory))
          (contrapositives `(<= (answer ,x) ,p)))
    (mapc #'(lambda (x) (save x 'infotheory)) s)
    (cond ((setq rl (residues x `(answer ,x) 'infotheory #'nonviewp))
           (setq p (decolonize (maksor rl)))
           (if *collapse* (setq p (collapse x p)))
           (if *collapse* (setq p (raisin x p)))
           (if *compress* (setq p (compress p)))
           p))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; broker
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(setq *saves* t)

(defclass broker (agent) ())

(defmethod message-handler (*message* *sender* (*receiver* broker))
  (let (*library* *warehouse*)
    (setq *library* (referent (find-rulebase *receiver*)))
    (setq *warehouse* (referent (find-database *receiver*)))
    (acleval *message* *sender* *receiver*)))

(defmethod insert (p (th broker))
  (do ((l (finds '?a `(interest ?a ,(operator p)) *manager*) (cdr l))
       (msg `(tell ,p)) (dum))
      ((null l) 'done)
      (cond ((and (eq (car l) *sender*) (equal msg *message*)))
            ((stringp (setq dum (request msg *receiver* (car l))))
             (throw 'assume dum)))))

(defmethod uninsert (p (th broker))
  (do ((l (finds '?a `(interest ?a ,(operator p)) *manager*) (cdr l))
       (msg `(untell ,p)) (dum))
      ((null l) 'done)
      (cond ((and (eq (car l) *sender*) (equal msg *message*)))
            ((stringp (setq dum (request msg *receiver* (car l))))
             (throw 'forget dum)))))

(defmethod envindexps (p al (th broker))
  (setq p (plugstdexp p al))
  (do ((l (finds '?a `(specialty ?a ,(car p)) *manager*) (cdr l)) (nl))
      ((null l) (nreverse nl))
      (setq nl (nreconc (request `(ask-all ,p ,p) *receiver* (car l)) nl))))

;;; needs update handling
;;; Note extreme slowness of indexing.
;;; Note that it still gets all answers to subqueries as part of this indexing!
;;; However, second agent gets narrower request, just more of them.

(defmethod acleval (msg sender (receiver broker))
  (cond ((atom msg) (call-next-method msg sender receiver))
        ((eq 'tell (car msg))
         (includes *receiver* *library*)
         (catch 'assume (assume (cadr msg) receiver)))
        ((eq 'untell (car msg))
         (includes *receiver* *library*)
         (catch 'forget (forget (cadr msg) receiver)))
        ((eq 'eliminate (car msg))
         (includes *receiver* *library*)
         (discard (cadr msg) receiver))
        ((eq 'ask-if (car msg)) (findp (cadr msg) receiver))
        ((eq 'ask-one (car msg)) (findx (cadr msg) (caddr msg) receiver))
        ((eq 'ask-all (car msg)) (finds (cadr msg) (caddr msg) receiver))
        ((eq 'ask-table (car msg)) (findtable (cadr msg) (caddr msg)))
        ((eq 'ask-about (car msg)) (askabout (cadr msg) sender receiver))
        ((findp `(responds ?a ,(car msg)) *manager*) (askmessage msg))
        ((findp `(performs ?a ,(car msg)) *manager*) (askrequest msg))
        (t (call-next-method msg sender receiver))))

(defmethod acleval (msg sender (receiver agent))
  (cond ((atom msg) (call-next-method msg sender receiver))
        ((eq 'tell (car msg))
         (cond ((null (cddr msg)) (affirm (cadr msg) sender receiver))
               (t (affirmall (cadr msg) (caddr msg) sender receiver))))
        ((eq 'untell (car msg))
         (cond ((null (cddr msg)) (retract (cadr msg) sender receiver))
               (t (retractall (cadr msg) (caddr msg) sender receiver))))
        ((eq 'eliminate (car msg)) (kill (cadr msg) receiver))
        ((eq 'ask-if (car msg)) (findp (cadr msg) receiver))
        ((eq 'ask-one (car msg)) (findx (cadr msg) (caddr msg) receiver))
        ((eq 'ask-all (car msg)) (finds (cadr msg) (caddr msg) receiver))
        ((eq 'ask-table (car msg)) (findtable (cadr msg) (caddr msg)))
        ((eq 'ask-about (car msg)) (sentences (cadr msg) receiver))
        ((eq 'ask (car msg)) (request (caddr msg) receiver (cadr msg)))
        (t (call-next-method msg sender receiver))))

(defmethod acleval (x sender receiver)
  (cond ((atom x)
         (cond ((numberp x) x)
               ((characterp x) x)
               ((stringp x) x)
               ((symbolp x) (if (boundp x) (symbol-value x)))
               (t x)))
        ((eq 'if (car x))
         (cond ((acleval (cadr x) sender receiver)
                (acleval (caddr x) sender receiver))
               ((cadddr x) (acleval (cadddr x) sender receiver))))
        ((eq 'progn (car x))
         (do ((l (cdr x) (cdr l)) (ans))
             ((null l) ans)
             (setq ans (acleval (car l) sender receiver))))
        ((eq 'quote (car x)) (cadr x))
        ((eq 'setq (car x)) (set (cadr x) (acleval (caddr x) sender receiver)))
        ((special-form-p (car x)) nil)
        ((macro-function (car x)) (acleval (macroexpand x) sender receiver))
        ((fboundp (car x))
         (apply (car x) (mapcar #'(lambda (x) (acleval x sender receiver))
                                (cdr x))))))

(defmethod askabout (x sender (receiver broker))
  (do ((l (find-agents) (cdr l)) (msg `(ask-about ,x)) (nl))
      ((null l) nl)
      (cond ((and (eq (car l) sender) (equal msg *message*)))
            (t (setq nl (nconc nl (request msg receiver (car l))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; facilitator
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass facilitator (agent) ())
#|
(defmethod message-handler (*message* *sender* (*receiver* facilitator))
  (let (*library* *warehouse* *agents*)
    (setq *library* (referent (find-rulebase *receiver*)))
    (setq *warehouse* (referent (find-database *receiver*)))
    (setq *agents* (finds '?a '(specialty ?a ?x) *manager*))   ;;; (setq *agents* (find-agents))
    (facilitate *message* *sender* *receiver*)))
|#
(defmethod message-handler (*message* *sender* (*receiver* facilitator))
  (let (*library* *warehouse* *agents*)
    (setq *library* (referent (find-rulebase *receiver*)))
    (setq *warehouse* (referent (find-database *receiver*)))
    (setq *agents* (finds '?a '(specialty ?a ?x) *manager*))   ;;; (setq *agents* (find-agents))
    (eval *message*)))

(defun facilitate (msg sender receiver)
  (cond ((atom msg) msg)
        ((eq (car msg) 'tell) (assume (maksand (cdr msg)) receiver))
        ((eq (car msg) 'untell) (forget (cadr msg) receiver))
        ((eq (car msg) 'update) (change (maksand (cdr msg)) receiver))
        ;((eq (car msg) 'establish) (create (cadr msg) (caddr msg) receiver))
        ((eq (car msg) 'eliminate) (discardall (maknot (cadr msg)) sender receiver))
        ((eq (car msg) 'ask-if) (findx t (cadr msg) receiver))
        ((eq (car msg) 'ask-one) (findx (cadr msg) (caddr msg) receiver))
        ((eq (car msg) 'ask-all) (finds (cadr msg) (caddr msg) receiver))
        ((eq (car msg) 'ask-table) (asktable (cadr msg) (caddr msg) sender receiver))
        ((eq (car msg) 'ask-about) (askabout (cadr msg) sender receiver))
        ((eq (car msg) 'quote) (cadr msg))
        ((macro-function (car msg)) (facilitate (macroexpand msg) sender receiver))
        ((fboundp (car msg))
         (apply (car msg) (mapcar #'(lambda (x) (facilitate x sender receiver)) (cdr msg))))))

(defmethod save (p (th facilitator) &optional (f #'samep))
  (declare (ignore f))
  (cond ((atom p) (save p (find-target (name th))))
        ((eq (car p) '<=) (save p (get-rulebase th)))
        ((eq (car p) '=>) (save p (get-rulebase th)))
        (t (save p (find-target (name th))))))

(defmethod drop (p (th facilitator) &optional (f #'samep))
  (declare (ignore f))
  (cond ((atom p) (drop p (find-target (name th))))
        ((eq (car p) '<=) (drop p (get-rulebase th)))
        ((eq (car p) '=>) (drop p (get-rulebase th)))
        (t (drop p (find-target (name th))))))

(defmethod insert (p (th facilitator))
  (do ((l (finds '?a `(interest ?a ,(operator p)) *manager*) (cdr l))
       (msg `(tell ,p)) (dum))
      ((null l) 'done)
      (cond ((and (eq (car l) *sender*) (equal msg *message*)))
            ((stringp (setq dum (request msg *receiver* (car l))))
             (throw 'assume dum)))))

(defmethod uninsert (p (th facilitator))
  (do ((l (finds '?a `(interest ?a ,(operator p)) *manager*) (cdr l))
       (msg `(untell ,p)) (dum))
      ((null l) 'done)
      (cond ((and (eq (car l) *sender*) (equal msg *message*)))
            ((stringp (setq dum (request msg *receiver* (car l))))
             (throw 'forget dum)))))

(defmethod envindexps (p al (th facilitator))
  (setq p (plugstdexp p al))
  (do ((l (finds '?a `(specialty ?a ,(operator p)) *manager*) (cdr l)) (nl))
      ((null l) (nreverse nl))
      (setq nl (nreconc (request `(ask-all ,p ,p) *receiver* (car l)) nl))))

(defun find-agents ()
  (cons (name *library*)
        (cons (name *warehouse*) (finds '?x '(isa ?x agent) *manager*))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod assume (p (th facilitator))
  (change p th))

(defmethod forget (p (th facilitator))
  (change (maknot p) th))

(defmethod change (p (th facilitator))
  (let (updates library (*ancestry* 1))
    (setq library (get-rulebase th))
    (decludes 'infotheory)
    (empty 'infotheory)
    (includes 'infotheory library)
    (setq updates (newconsequences p 'infotheory #'specialp #'success))
    (setq updates (nreverse (newchanges (cons 'update updates) *target* nil)))
    (setq updates (make-exports updates))
    (eval (reassemble updates))))

(defmethod discardall (x sender (receiver facilitator))
  (do ((l *agents* (cdr l)) (nl))
      ((null l) nl)
      (cond ((and (eq (car l) sender) (listp *message*)
                  (eq (car *message*) 'eliminate) (equalp (cadr *message*) x)))
            ((eq (car l) receiver))
            (t (setq nl (unionize nl (request `(eliminate ,x) receiver (car l))))))))

(defun newchanges (p th nl)
  (cond ((atom p) (adjoin p nl))
        ((eq '=> (car p))
         (dolist (item (request `(ask-all ,(caddr p) ,(cadr p)) nil th))
           (setq nl (contribute item nl)))
         nl)
        ((eq 'and (car p))
         (dolist (item (cdr p)) (setq nl (newchanges item th nl))) nl)
        ((eq 'update (car p))
         (dolist (item (cdr p)) (setq nl (newchanges item th nl))) nl)
        (t (adjoin p nl :test #'equalp))))

(defun filter-exports (facts sender security)
  (request `(ask-all (tell ?b ?p) (and (oneof ?p . ,facts)
                                       (publishable ,(name sender) ?b ?p)))
           nil security))

(defun make-exports (facts)
  (do ((l facts (cdr l)) (nl))
      ((null l) (nreverse nl))
      (do ((m (finds '?a `(interest ?a ,(operator (car l))) *manager*) (cdr m)))
          ((null m))
          (setq nl (cons `(tell ,(car m) ,(car l)) nl)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod findx (x p (th facilitator))
  (let (dum)
    (when (setq dum (facilitateask x p th))
      (eval (route (makaskemone x dum))))))

(defmethod finds (x p (th facilitator))
  (let (dum)
    (when (setq dum (facilitateask x p th))
      (eval (route (makaskemall x dum))))))

(defmethod askx (x p sender (receiver facilitator))
  (declare (ignore sender))
  (findx x p receiver))

(defmethod asks (x p sender (receiver facilitator))
  (declare (ignore sender))
  (finds x p receiver))

(defmethod asktable (ol sl sender (receiver facilitator))
  (declare (ignore sender))
  (let (dum)
    (cond ((or (null ol) (null sl)) nil)
          ((and (null (cdr ol)) (null (cdr sl)))
           (setq dum (facilitateask '?y (list (car sl) (car ol) '?y) receiver))
           (list (list (eval (route (makaskemall '?y dum))))))
          (t (do ((l sl (cdr l)) (nl))
                 ((null l) (invert-table (nreverse nl)))
                 (setq dum (facilitateask '(?x ?y) `(and (oneof ?x . ,ol) (,(car l) ?x ?y)) receiver))
                 (setq nl (cons (extract-values ol (eval (route (makaskemall '(?x ?y) dum)))) nl)))))))

(defmethod askabout (x sender (receiver facilitator))
  (do ((l *agents* (cdr l)) (nl)) 
      ((null l) nl)
      (cond ((and (eq (car l) sender) (listp *message*)
                  (eq (car *message*) 'ask-about) (equalp (cadr *message*) x)))
            ((eq (car l) (name receiver)))
            (t (setq nl (unionize nl (request `(ask-about ,x) receiver (car l))))))))

(defun interestp (r)
  (or (eq r 'evaluate) (findp `(interest ?a ,r) *manager*)))

(defun specialp (r)
  (or (basep r) (findp `(specialty ?a ,r) *manager*)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Sourcing for asks
;;; Example: (or (p 1) (and (q 2) (r 3))) where art performs p, bob q and r
;;; Output: (or (ask (art) (p 1)) (ask (bob) (and (q 2) (r 3))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun source (p)
  (setq p (sources p))
  (remedy p))

(defun sources (p)
  (cond ((atom p) `(ask ,(find-specialists p) ,p))
        ((eq 'and (car p)) (source-and p))
        ((eq 'or (car p)) (source-or p))
        ((eq 'not (car p))
         (setq p (sources (cadr p)))
         (cond ((eq 'ask (car p)) `(ask ,(cadr p) (not ,(caddr p))))
               (t `(not ,p))))
        ((eq 'unprovable (car p))
         (setq p (sources (cadr p)))
         (cond ((eq 'ask (car p)) `(ask ,(cadr p) (unprovable ,(caddr p))))
               (t `(unprovable ,p))))
        ((eq 'bagofall (car p))
         (let (plan)
           (setq plan (sources (caddr p)))
           (cond ((eq 'ask (car plan))
                  `(ask ,(cadr plan) (bagofall ,(cadr p) ,(caddr plan) ,(cadddr p))))
                 (t `(bagofall ,(cadr p) ,plan ,(cadddr p))))))
        (t `(ask ,(find-specialists (car p)) ,p))))

(defun source-and (p)
  (do ((l (mapcar #'sources (cdr p))) (sources) (dum) (nl))
      ((null l)
       (cond ((null (cdr nl)) (car nl))
             (t (mapc #'remedy nl) (cons 'and (nreverse nl)))))
      (cond ((not (eq 'ask (caar l))) (setq nl (cons (car l) nl) l (cdr l)))
            (t (setq sources (cadar l))
               (do ((m (cdr l) (cdr m)) (nc (list (caddar l))))
                   ((null m)
                    (setq nl (cons `(ask ,sources ,(maksand (nreverse nc))) nl))
                    (setq l nil))
                 (cond ((and (eq 'ask (caar m))
                             (setq dum (intersection* sources (cadar m))))
                        (setq sources dum nc (cons (caddar m) nc)))
                       (t (setq nl (cons `(ask ,sources ,(maksand (nreverse nc))) nl))
                          (setq l m)
                          (return t))))))))

(defun source-or (p)
  (cons 'or (mapcar #'remedy (mapcar #'sources (cdr p)))))

(defun findallspecialists (p)
  (cond ((atom p) (find-specialists p))
        ((eq 'not (car p)) (findallspecialists (cadr p)))
        ((member (car p) '(and or <= =>))
         (do ((l (cddr p) (cdr l)) (nl (findallspecialists (cadr p))))
             ((null l) (nreverse nl))
             (setq nl (intersect nl (findallspecialists (car l))))))
        ((eq 'unprovable (car p)) (findallspecialists (cadr p)))
        (t (find-specialists (car p)))))

(defun find-specialist (x)
  (cond ((eq 'true x) 'warehouse)
        ((eq 'false x) 'warehouse)
        ((basep x) 'warehouse)
        ((findp `(specialty warehouse ,x) *manager*) 'warehouse)
        ((findp `(specialty library ,x) *manager*) 'library)
        (t (findx '?a `(specialty ?a ,x) *manager*))))

(defun find-specialists (x)
  (cond ((eq 'true x) *agents*)
        ((eq 'false x) *agents*)
        ((basep x) (append *agents* (finds '?a `(specialty ?a ,x) *manager*)))
        (t (sortspecialists (finds '?a `(specialty ?a ,x) *manager*)))))

(defun sortspecialists (specialists)
  (if (find (name *warehouse*) specialists)
      (cons (name *warehouse*) (delete (name *warehouse*) specialists))
      specialists))

(defun remedy (x)
  (cond ((atom x) x)
        ((eq 'ask (car x)) (rplaca (cdr x) (caadr x)) x)
        (t x)))

(defun divvy (msg)
  (let (dum al)
    (dolist (p (cdr msg))
      (dolist (a (finds '?a `(interest ?a ,(operator p)) *manager*))
        (cond ((setq dum (assoc a al)) (rplacd dum (cons p (cdr dum))))
              (t (setq al (acons a (list p) al))))))
    (do ((l al (cdr l)) (pl))
        ((null l) (maksprogn pl))
        (setq pl (cons `(ask ,(caar l) (update . ,(nreverse (cdar l)))) pl)))))

(defun makaskemone (x p)
  (cond ((null p) nil)
        ((atom p) `(askemone ',x ',p))
        ((eq 'ask (car p)) `(ask ,(list (cadr p)) (ask-one ,x ,(caddr p))))
        (t `(askemone ',x ',p))))

(defun makaskemall (x p)
  (cond ((null p) nil)
        ((atom p) `(askemall ',x ',p))
        ((eq 'ask (car p)) `(ask ,(list (cadr p)) (ask-all ,x ,(caddr p))))
        ((eq 'or (car p))
         (cons 'union* (mapcar #'(lambda (p) (makaskemall x p)) (cdr p))))
        (t `(askemall ',x ',p))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Collapse stuff imported from epilog
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod collapse (x p)
 "(COLLAPSE X P)
  COLLAPSE takes as argument a term and a boolean sentence.  It removes redundant
  literals from embedded conjunctions by matching variables not included in the
  specified list.  The result is a sentence with the same values for the
  specified variables as the original sentence."
  (cond ((eq *collapse* 'simple) (collapse2 x p))
        (*collapse* (collapse1 x p))
        (t p)))

(defun collapse1 (x p)
  (cond ((atom p) p)
        ((eq 'and (car p))
         (maksand (equalizer (vars x) (cdr p))))
        ((eq 'or (car p))
         (maksor (mapcar #'(lambda (p) (collapse1 x p)) (cdr p))))
        (t p)))

(defun equalizer (vl cl)
  (eqexp nil cl (makvlist vl)))

(defun equalizerexp (ol nl al)
  (cond ((null nl) (remove-duplicates (nreverse ol) :test #'equalp))
        ((do ((l (reverse ol) (cdr l)) (bl) (dum))
               ((null l) nil)
               (cond ((and (setq bl (matchpexp (car nl) (car l) al))
                           (setq dum (eqexp ol (cdr nl) bl)))
                      (return dum)))))
        ((do ((l (cdr nl) (cdr l)) (bl) (dum))
               ((null l) nil)
               (cond ((and (setq bl (matchpexp (car nl) (car l) al))
                           (setq dum (eqexp ol (cdr nl) bl)))
                      (return dum)))))
        ((setq al (clearup (car nl) al))
         (eqexp (cons (car nl) ol) (cdr nl) al))))

(defun clearup (x al)
  (cond ((indvarp x)
         (let ((dum (assoc x al)))
           (cond ((null dum) (acons x x al))
                 ((eq x (cdr dum)) al))))
        ((atom x) al)
        (t (do ((l x (cdr l)))
               ((null l) al)
               (unless (setq al (clearup (car l) al)) (return nil))))))

(defun eqexp  (ol nl al) (equalizerexp ol nl al))

(defun makvlist (x)
  (do ((l (vars x) (cdr l)) (al))
      ((null l) (nreverse (cons '(t . t) al)))
      (setq al (acons (car l) (car l) al))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Intermediate versions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun equalify (vl cl)
  (cond ((null cl) nil)
        ((null (cdr cl)) cl)
        (t (do ((l cl (cdr l)) (al (makvlist vl)) (bl) (ncl) (ol))
               ((null l) (nreverse ol))
               (do ((m cl (cdr m)) (nol) (nl))
                   ((null m) (setq ol (cons (car l) ol)))
                   (cond ((eq (car l) (car m)))
                         ((and (setq bl (matchpexp (car l) (car m) al))
                               (setq nol (sublis bl ol) nl (sublis bl l))
                               (setq ncl (revappend nol (cdr nl)))
                               (subsetp ncl cl :test #'equalp))
                          (setq cl ncl ol nol l nl)
                          (return t))))))))

(defun equalize (vl cl)
  (cond ((null cl) nil)
        ((null (cdr cl)) cl)
        (t (do ((l cl (cdr l)) (al (makvlist vl)) (bl) (dl) (nl))
               ((null l) (nreverse nl))
               (do ((m cl (cdr m)) (nnl) (ncl))
                   ((null m) (setq nl (cons (car l) nl)))
                   (cond ((eq (car l) (car m)))
                         ((and (setq bl (matchpexp (car l) (car m) al))
                               (setq nnl (sublis bl nl) ncl (sublis bl l))
                               (setq dl (revappend nnl (cdr ncl)))
                               (subsumepands dl cl al))
                          (setq cl dl nl nnl l ncl)
                          (return t))))))))

(defun subsumep (p q al)
  (cond ((atom p)
         (cond ((atom q) (eq p q))
               ((eq 'and (car q)) (find p (cdr q)))))
        ((eq 'and (car p))
         (cond ((atom q) nil)
               ((eq 'and (car q)) (subsumepands (cdr p) (cdr q) al))))
        (t (cond ((atom q) nil)
                 ((eq 'and (car q)) (some #'(lambda (x) (matchpexp p x al)) (cdr q)))
                 (t (matchpexp p q al))))))

(defun subsumepands (pl ql al)
  (cond ((null pl) al)
        (t (do ((m ql (cdr m)) (bl))
               ((null m))
               (if (and (setq bl (matchpexp (car pl) (car m) al))
                        (setq bl (subsumepands (cdr pl) ql bl)))
                   (return bl))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Old version
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun oldequalify (vl cl)
  (cond ((null cl) nil)
        ((null (cdr cl)) cl)
        (t (setq vl (varlist cl (varlist vl nil)))
           (do ((l cl (cdr l)) (nl))
               ((null l) nl)
               (setq vl (unvarlist (car l) vl))
               (cond ((member (car l) nl
                              :test #'(lambda (x y) (equalizablep x y truth vl))))
                     ((member (car l) (cdr l)
                              :test #'(lambda (x y) (equalizablep x y truth vl))))
                     (t (setq vl (varlist (car l) vl))
                        (setq nl (cons (car l) nl))))))))

(defun varlist (x al)
  (cond ((varp x)
         (let ((dum (assoc x al :test #'eq)))
           (cond (dum (rplacd dum (1+ (cdr dum))) al)
                 (t (acons x 1 al)))))
        ((atom x) al)
        (t (do ((l x (cdr l)))
               ((null l) al)
               (setq al (varlist (car l) al))))))

(defun unvarlist (x al)
  (cond ((varp x)
         (let ((dum (assoc x al :test #'eq)))
           (rplacd dum (1- (cdr dum)))
           al))
        ((atom x) al)
        (t (do ((l x (cdr l)))
               ((null l) al)
               (setq al (unvarlist (car l) al))))))

(defun equalizablep (x y al vl)
  (cond ((eq x y) al)
        ((indvarp x)
         (let ((dum))
           (cond ((setq dum (assoc x al :test #'eq))
                  (if (equal (cdr dum) y) al))
                 ((< (cdr (assoc x vl)) 1) (acons x y al)))))
        ((atom x) nil)
        ((atom y) nil)
        (t (do ((l x (cdr l)) (m y (cdr m)))
               ((null l) (if (null m) al))
               (cond ((null m) (return nil))
                     ((setq al (equalizablep (car l) (car m) al vl)))
                     (t (return nil)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Latest version
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun collapse2 (x p)
  (cond ((atom p) p)
        ((eq 'or (car p)) (collapse2or x p))
        ((eq 'and (car p)) (collapse2and x p))
        (t p)))

(defun collapse2or (x p)
  (do ((l (cdr p) (cdr l)) (nl))
      ((null l) (maksor (nreverse nl)))
      (setq nl (cons (collapse2 x (car l)) nl))))

(defun collapse2and (x p)
  (let ((cl (copy-list (cdr p))) (al truth))
    (do ((l cl (cdr l)) (bl))
        ((null l))
        (cond ((basep (operator (car l))))
              (t (do ((m (cdr l) (cdr m)))
                     ((null m))
                     (cond ((eq (car l) (car m)))
                           ((setq bl (collapser (car l) (car m) al))
                            (setq al bl)
                            (delete (car m) l)))))))
    (do ((l al (cdr l)) (vl (vars x)) (dum) (bl truth))
        ((null (cdr l)) (setq al bl))
        (cond ((find (caar l) vl)
               (setq dum (plug (cdar l) al))
               (cond ((groundp dum) (setq bl (cons (car l) bl)))
                     ((find dum vl) (setq bl (cons (car l) bl)))
                     (t (setq bl (acons dum (caar l) bl)))))
              (t (setq bl (cons (car l) bl)))))
    (setq cl (mapcar #'(lambda (x) (plug x al)) cl))
    (do ((l (vars x) (cdr l)) (dum))
        ((null l) (maksand cl))
        (if (not (eq (setq dum (plug (car l) al)) (car l)))
            (setq cl (cons `(same ,(car l) ,dum) cl))))))

(defun collapser (p q al)
  (cond ((atom p) (if (equalp p q) al))
        ((atom q) nil)
        ((eq 'not (car p)) nil)
        ((eq 'unprovable (car p)) nil)
        ((eq (car p) (car q)) (collapserexp p q al))))

(defun collapserexp (p q al)
  (let (cols keys)
    (when (and (setq cols (find-columns (car p))) (setq keys (find-keys (car p))))
      (do ((l (cdr p) (cdr l)) (m (cdr q) (cdr m)) (bl) (cl cols (cdr cl)) (kl keys))
          ((or (null l) (null m)) (if (and (null l) (null m) (null kl)) al))
          (cond ((eq (car cl) (car kl))
                 (if (equalp (plug (car l) al) (plug (car m) al))
                     (setq kl (cdr kl))
                     (return nil)))
                ((setq bl (mguexp (car l) (car m) al)) (setq al bl))
                (t (return nil)))))))


(defun raisin (x p)
  (cond ((atom p) p)
        ((eq 'or (car p)) (raisinor x p))
        (t p)))

(defun raisinor (x p)
  (cond ((null (cdr p)) nil)
        ((null (cddr p)) (cadr p))
        (t (do ((l (cdr p) (cdr l)) (al (makvarlist x)) (ol))
               ((null l) (maksor (nreverse ol)))
               (cond ((some #'(lambda (p) (relsumep p (car l) al)) ol))
                     ((some #'(lambda (p) (relsumep p (car l) al)) (cdr l)))
                     (t (setq ol (cons (car l) ol))))))))

(defun makvarlist (x)
  (do ((l (vars x) (cdr l)) (al))
      ((null l) (nreverse (cons '(t . t) al)))
      (setq al (acons (car l) (car l) al))))

(defun relsumep (p q al)
  (cond ((atom p)
         (cond ((atom q) (eq p q))
               ((eq 'and (car q)) (find p (cdr q)))))
        ((eq 'and (car p))
         (cond ((atom q) nil)
               ((eq 'and (car q)) (relsumepands (cdr p) al (cdr q)))
               (t (relsumepands (cdr p) al (list q)))))
        (t (cond ((atom q) nil)
                 ((eq 'and (car q)) (some #'(lambda (x) (matchpexp p x al)) (cdr q)))
                 (t (matchpexp p q al))))))

(defun relsumepands (pl al ql)
  (cond ((null pl))
        (t (do ((m ql (cdr m)) (bl))
               ((null m))
               (if (and (setq bl (matchpexp (car pl) (car m) al))
                        (relsumepands (cdr pl) bl ql))
                   (return t))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; collector
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass collector (agent) ())

(defmethod message-handler (msg sender (receiver collector))
  (cond ((atom msg) msg)
        ((eq 'update (car msg)) (modify msg sender receiver))
        ((eq 'tell (car msg)) (modify msg sender receiver))
        ((eq 'untell (car msg)) (modify msg sender receiver))
        (t (request msg sender (find-target receiver)))))

(defmethod save (p (th collector) &optional (f #'samep))
  (declare (ignore f))
  (cond ((atom p) (save p (find-target (name th))))
        ((eq (car p) '<=) (save p (get-rulebase th)))
        ((eq (car p) '=>) (save p (get-rulebase th)))
        (t (save p (find-target (name th))))))

(defmethod drop (p (th collector) &optional (f #'samep))
  (declare (ignore f))
  (cond ((atom p) (drop p (find-target (name th))))
        ((eq (car p) '<=) (drop p (get-rulebase th)))
        ((eq (car p) '=>) (drop p (get-rulebase th)))
        (t (drop p (find-target (name th))))))

(defmethod modify (p sender (receiver collector))
  (let (updates targets security)
    (setq targets (find-targets (name receiver)))
    (setq security (get-security receiver))
    (setq updates (filter-imports (get-updates p) sender receiver security))
    (when updates (dolist (a targets) (request (cons 'update updates) sender a)))))

(defun get-updates (p)
  (cond ((atom p) (list p))
        ((eq (car p) 'and) (cdr p))
        ((eq (car p) 'update) (cdr p))
        (t (list p))))

(defun filter-imports (facts sender receiver security)
  (request `(ask-all ?p (and (oneof ?p . ,facts)
                             (authorized ,(name sender) ,(name receiver) ?p)))
           nil security))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; disseminator
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass disseminator (agent) ())

(defmethod message-handler (msg sender (receiver disseminator))
  (cond ((atom msg) msg)
        ((eq 'update (car msg)) (modify msg sender receiver))
        ((eq 'tell (car msg)) (modify msg sender receiver))
        ((eq 'untell (car msg)) (modify msg sender receiver))))

(defmethod save (p (th disseminator) &optional (f #'samep))
  (declare (ignore f))
  (cond ((atom p) (save p (find-target (name th))))
        ((eq (car p) '<=) (save p (get-rulebase th)))
        ((eq (car p) '=>) (save p (get-rulebase th)))
        (t (save p (find-target (name th))))))

(defmethod drop (p (th disseminator) &optional (f #'samep))
  (declare (ignore f))
  (cond ((atom p) (drop p (find-target (name th))))
        ((eq (car p) '<=) (drop p (get-rulebase th)))
        ((eq (car p) '=>) (drop p (get-rulebase th)))
        (t (drop p (find-target (name th))))))

(defmethod modify (p sender (receiver disseminator))
  (let (updates)
    (setq updates (get-updates p))
    (setq updates (filter-exports updates sender sender))
    (eval (reassemble updates))))

(defun filter-exports (facts sender security)
  (request `(ask-all (tell ?b ?p) (and (oneof ?p . ,facts)
                                       (publishable ,(name sender) ?b ?p)))
           nil security))

(defun reassemble (updates)
  (let (dum al)
    (dolist (p updates)
      (cond ((setq dum (assoc (cadr p) al))
             (rplacd dum (cons (caddr p) (cdr dum))))
            (t (setq al (acons (cadr p) (cddr p) al)))))
    (do ((l al (cdr l)) (pl))
        ((null l) (maksprogn pl))
        (setq pl (cons `(ask ,(caar l) (update . ,(nreverse (cdar l)))) pl)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; authorizer
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass authorizer (agent) ())

(defmethod message-handler (msg sender (receiver authorizer))
  (cond ((atom msg) msg)
        ((eq 'update (car msg)) (modify msg sender receiver))
        ((eq 'tell (car msg)) (modify msg sender receiver))
        ((eq 'untell (car msg)) (modify msg sender receiver))
        (t (request msg sender (find-target receiver)))))

(defmethod save (p (th authorizer) &optional (f #'samep))
  (declare (ignore f))
  (cond ((atom p) (save p (find-target (name th))))
        ((eq (car p) '<=) (save p (get-rulebase th)))
        ((eq (car p) '=>) (save p (get-rulebase th)))
        (t (save p (find-target (name th))))))

(defmethod drop (p (th authorizer) &optional (f #'samep))
  (declare (ignore f))
  (cond ((atom p) (drop p (find-target (name th))))
        ((eq (car p) '<=) (drop p (get-rulebase th)))
        ((eq (car p) '=>) (drop p (get-rulebase th)))
        (t (drop p (find-target (name th))))))

(defmethod modify (p sender (receiver authorizer))
  (let (updates target security)
    (setq target (get-recipient receiver))
    (setq security (get-security receiver))
    (setq updates (filter-imports (get-updates p) sender receiver security))
    (when updates (request (cons 'update updates) sender target))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; pipe
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun createpipe (source target class owner rule)
  (let (pipe)
    (cond ((findx '?x `(and (pipe.source ?x ,source)
                            (pipe.target ?x ,target)
                            (pipe.subject ?x ,class)
                            (pipe.rule ?x ,rule)) *manager*))
          (t (setq pipe (gentemp "PIPE."))
             (save `(isa ,pipe pipe) *manager*)
             (save `(pipe.source ,pipe ,source) *manager*)
             (save `(pipe.target ,pipe ,target) *manager*)
             (save `(pipe.subject ,pipe ,class) *manager*)
             (save `(pipe.owner ,pipe ,owner) *manager*)
             (save `(pipe.rule ,pipe ,rule) *manager*)
             pipe))))

(defun deletepipe (pipe)
  (dropall `(recipient ?x ?y) `(and (pipe.source ,pipe ?x) (recipient ?x ?y)) *manager*)
  (do ((l (finds '?x `(pipe.rule ,pipe ?x) *manager*) (cdr l))
       (source (findx '?x `(pipe.source ,pipe ?x) *manager*)))
      ((null l) (kill pipe *manager*) 'done)
      (drop (car l) source)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; chore
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun make-chore (obj)
  (let (basetime period operation)
    (when (and (or (not (boundp obj)) (not (processp obj)))
               (setq basetime (or (findx '?x `(basetime ,obj ?x) *manager*) 0))
               (setq period (or (findx '?x `(period ,obj ?x) *manager*)
                                (finds '?x `(runday ,obj ?x) *manager*)))
               (setq operation (findx '?x `(operation ,obj ?x) *manager*)))
      (set obj (process-run-function (prettify obj)
                 #'runchore basetime period operation)))))

(defun kill-chore (obj)
  (when (and (boundp obj) (processp (symbol-value obj)))
    (process-kill (referent obj))
    (makunbound obj)))

(defun reset-chore (obj)
  (kill-chore obj)
  (make-chore obj))

(defun runchore (basetime period operation)
  (let (now)
    (setq now (get-universal-time))
    (unless (< basetime now) (sleep (- basetime now)))
    (cond ((numberp period)
           (loop (funcall operation)
                 (setq now (get-universal-time))
                 (sleep (- period (mod (- now basetime) period)))))
          ((listp period)
           (loop (setq now (read-from-string (current-day)))
                 (when (find now period) (funcall operation))
                 (setq now (get-universal-time))
                 (sleep (- 86400 (mod (- now basetime) 86400))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; counter
;;;  supplies sequential numbers for relations
;;;  (<indexrelation> <object> <number>)
;;;  if <object> is ground and no entry exists for <number>,
;;;  it automatically generates the next.
;;;  size stored in size and is resettable but does not erase listings.
;;;  (indexsize <relation> <number>)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass counter (dataserver) ())

(defmethod indexps (p (th counter))
  (cond ((varp p) (contents th))
        ((atom p) (indexees p th))
        ((eq 'indexsize (car p)) (countindexps p th))
        ((not (findp `(specialty ,(name th) ,(car p)) *manager*)) nil)
        ((or (null (cddr p)) (cdddr p)) nil)
        ((groundp (caddr p)) (countindexps p th))
        ((and (groundp (cadr p)) (varp (caddr p)) (not (indexp p th)))
         (let ((size (or (truex '?x `(indexsize ,(car p) ?x) th) 0)) (datum))
           (drop `(indexsize ,(car p) ,size) th)
           (setq size (1+ size) datum (list (car p) (cadr p) size))
           (insert `(indexsize ,(car p) ,size) th)
           (insert datum th)
           (list datum)))
        (t (countindexps p th))))

(defun countindexps (p th)
  (do ((l (cdr p) (cdr l)))
      ((null l) (indexps (car p) th))
      (cond ((varp (car l)))
            ((atom (car l)) (return (indexees (car l) th))))))

(defun indexp (p th)
  (find p (indexees (cadr p) th) :test #'matchp))

(defmethod envindexps (p al (th counter))
  (indexps (plug p al) th))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Special request handling
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun askmessage (x)
  (do ((l (cdr x) (cdr l)) (nl))
      ((null l) (distribute (cons (car x) (nreverse nl))))
      (setq nl (cons (message (car l) *sender* *receiver*) nl))))

(defun askrequest (x)
  (do ((l (cdr x) (cdr l)) (nl))
      ((null l) (delegate (cons (car x) (nreverse nl))))
      (setq nl (cons (request (car l) *sender* *receiver*) nl))))

(defun distribute (msg)
  (do ((l (finds '?a `(responds ?a ,(operator msg)) *manager*) (cdr l)))
      ((null l) 'done)
      (cond ((and (eq (car l) *sender*) (equal msg *message*)))
            (t (message msg *receiver* (car l))))))

(defun delegate (msg)
  (do ((l (finds '?a `(performs ?a ,(operator msg)) *manager*) (cdr l)))
      ((null l) 'done)
      (cond ((and (eq (car l) *sender*) (equal msg *message*)))
            (t (return (request msg *receiver* (car l)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; compute a relation from a source
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod compute (rel source)
  (let (arity vars)
    (when (setq arity (find-arity rel))
      (do ((i 1 (1+ i)) (vl))
          ((> i arity) (setq vars (nreverse vl)))
          (setq vl (cons (decolonize (newindvar)) vl)))
      (request `(ask-all ,vars ,(cons rel vars)) *client* source))))

(defmethod compute (rel (source symbol))
  (cond ((and (boundp source) (not (symbolp (symbol-value source))))
         (compute rel (symbol-value source)))
        (t (call-next-method rel source))))

(defun computerequest (rel)
  (let (arity vars)
    (when (setq arity (find-arity rel))
      (do ((i 1 (1+ i)) (vl))
          ((> i arity) (setq vars (nreverse vl)))
          (setq vl (cons (decolonize (newindvar)) vl)))
      `(ask-all ,vars ,(cons rel vars)) )))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Residue without consistency checking
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defparameter *filter*  #'basep)

(defun unwind (x p *theory* &optional (*filter* #'basep) (*test* #'success))
  (let ((*consistency* nil) (alist (environment)))
    (unwindexp x p)))

(defun unwindexp (x p)
  (cond ((atom p) p)
        ((eq 'and (car p)) (unwindand x p))
        ((eq 'or (car p)) (unwindor x p))
        ((and (funcall *filter* (operator p))
              (funcall *test* (setq p (plugstdexp p alist)))) p)
        (t (unwinddb x p))))

(defun unwindand (x p)
  (do ((l (cdr p) (cdr l)) (dum) (nl))
      ((null l) (maksand (nreverse nl)))
      (setq dum (unwindexp x (car l)))
      (cond ((eq dum 'true))
            ((or (null dum) (eq dum 'false)) (return 'false))
            (t (setq nl (cons dum nl))))))

(defun unwindor (x p)
  (do ((l (cdr p) (cdr l)) (dum) (nl))
      ((null l) (maksor (nreverse nl)))
      (setq dum (unwindexp x (car l)))
      (cond ((eq 'true dum) (return 'true))
            ((or (null dum) (eq dum 'false)))
            (t (setq nl (cons dum nl))))))

(defun unwinddb (x p)
  (maksor (residues x p *theory* *filter* *test*)))

(defun processablep (p)
  (or (flatp p)
      (basep (operator p))
      (findp `(and (specialty ?x ,(operator p)) (kind ?x ruleserver)) *manager*))) 

(defun flatp (p)
  (cond ((atom p))
        ((eq 'oneof (car p)))
        ((eq '== (car p)))
        ((eq 'execute (car p)))
        ((eq 'evaluate (car p)))
        ((eq 'unprovable (car p)) (flatp (cadr p)))
        ((eq 'and (car p)) (every #'flatp (cdr p)))
        ((eq 'or (car p)) (every #'flatp (cdr p)))
        (t (every #'atom p))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Targeting for tells
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun target1 (p)
  (do ((l (find-interestees (operator p)) (cdr l)) (nl))
      ((null l) (maksprogn (nreverse nl)))
      (setq nl (cons `(ask ,(list (car l)) (tell ,p)) nl))))

(defun target2 (p q)
  (let (source target)
    (setq source (sources p))
    (setq target (targets q))
    (cond ((and (eq 'ask (car source)) (eq 'tell (car target))
                (subsetp (cadr target) (cadr source)))
           (do ((l (cadr target) (cdr l)) (nl))
               ((null l) (maksprogn (nreverse nl)))
               (setq nl (cons `(ask ,(list (car l)) (tell ,q ,p)) nl))))
          (t `(tellemall ',target ',(remedy source))))))

(defun targets (p)
  (cond ((atom p) `(tell ,(find-interestees p) ,p))
        ((eq 'and (car p)) (target-and p))
        ((eq 'not (car p))
         (setq p (targets (cadr p)))
         (cond ((eq 'tell (car p)) `(tell ,(cadr p) (not ,(caddr p))))
               (t `(not ,p))))
        ((eq 'unprovable (car p))
         (setq p (targets (cadr p)))
         (cond ((eq 'tell (car p)) `(tell ,(cadr p) (unprovable ,(caddr p))))
               (t `(unprovable ,p))))
        (t `(tell ,(find-interestees (car p)) ,p))))

(defun target-and (p)
  (do ((l (mapcar #'targets (cdr p))) (nl))
      ((null l) (maksprogn (nreverse nl)))
      (cond ((eq 'tell (caar l))
             (do ((m (cdr l) (cdr m)) (nm (list (caddar l))))
                 ((null m)
                  (setq nl (cons `(tell ,(cadar l) ,(maksand (nreverse nm))) nl))
                  (setq l nil))
                 (cond ((and (eq 'tell (caar m))
                             (subsetp (cadar l) (cadar m))
                             (subsetp (cadar m) (cadar l)))
                        (setq nm (cons (caddar m) nm)))
                       (t (setq nl (cons `(tell ,(cadar l) ,(maksand (nreverse nm))) nl))
                          (setq l m)
                          (return t)))))
            (t (setq nl (cons (car l) nl) l (cdr l))))))

(defun find-interestees (x)
  (cond ((eq 'true x) nil)
        ((eq 'false x) nil)
        ((basep x) nil)
        (t (finds '?a `(interest ?a ,x) *manager*))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Compression
;;; Need to cache results
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun compress (x)
  (cond ((atom x) x)
        ((eq 'and (car x)) x)
        ((eq 'or (car x)) (compressor x))
        (t x)))

(defun compressor (x)
  (do ((l (cdr x) (cdr l)) (dum) (nl))
      ((null l)
       (maksor
        (mapcar #'(lambda (x) (makand (car x) (compress (cdr x)))) (nreverse nl))))
      (cond ((setq dum (assoc (head (car l)) nl :test #'equal))
             (rplacd dum (makor (cdr dum) (tail (car l)))))
            (t (setq nl (acons (head (car l)) (tail (car l)) nl))))))

(defun maksprogn (l)
  (cond ((null l) nil)
        ((null (cdr l)) (car l))
        (t (cons 'progn l))))

(defun maklet (vars steps)
  (cond ((null vars)
         (cond ((null steps) nil)
               ((null (cdr steps)) (car steps))
               (t (cons 'progn steps))))
        ((null steps) nil)
        (t `(let ,vars . ,steps))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Routing
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun route (c)
  (cond ((atom c) c)
        ((eq 'quote (car c)) c)
        ((eq 'ask (car c)) (rplaca (cdr c) (caadr c)) c)
        ((eq 'tellemall (car c)) c)
        (t (dolist (item (cdr c)) (route item)) c)))

(defun router (c)
  (setq c (routes c))
  (cond ((eq (name *library*) (caadr c)) (caddr c))
        (t (rplaca (cdr c) (caadr c)) c)))

(defun routes (c)
  (cond ((atom c) `(ask ,*agents* ,c))
        ((eq 'quote (car c)) `(ask ,*agents* ,c))
        ((eq 'ask (car c)) c)
        ((eq 'tellemall (car c)) `(ask ,*agents* ,c))
        (t (route-call c))))

(defun route-call (c)
  (do* ((args (mapcar #'routes (cdr c))) (l args (cdr l))
        (sources (find-performers (car c))) (s sources))
       ((null l) `(ask ,s ,(cons (car c) (mapcar #'caddr args))))
       (cond ((setq s (intersect s (cadar l))))
             (t (mapc #'(lambda (x) (rplaca (cdr x) (caadr x))) args)
                (return `(ask ,sources ,(cons (car c) args)))))))

(defun find-performers (x)
  (cond ((lispp x) (append *agents* (finds '?a `(performs ?a ,x) *manager*)))
        ((fboundp x) (list (name *library*)))
        ((eq 'if x) *agents*)
        (t (finds '?a `(performs ?a ,x) *manager*))))

(defun lispp (x)
  (find-symbol (symbol-name x) (find-package 'common-lisp)))

(defun guessmetafacts (data warehouse interface)
  (let (predicates attributes tables dum class slot metadata)
  (dolist (sent data)
    (cond ((atom sent))
          ((find (car sent) '(<= => unprovable not and or)))
          ((basep (car sent)))
          ((and (cdr sent) (null (cddr sent)))
           (setq predicates (adjoin (car sent) predicates :test #'eq)))
          ((and (cddr sent) (null (cdddr sent)))
           (setq attributes (adjoin (car sent) attributes :test #'eq)))
          (t (setq tables (adjoin (car sent) tables :test #'eq)))))
  (dolist (predicate (nreverse predicates))
    (setq dum (disconstruct predicate))
    (setq class (newfix (car dum) (cadr dum)))
    (setq metadata (cons `(isa ,class class) metadata))
    (when (findp `(isa ,(cadr dum) class) *manager*)
      (setq metadata (cons `(superclass ,class ,(cadr dum)) metadata)))
    (setq metadata (cons `(predicate ,class ,predicate) metadata))
    (setq metadata (cons `(isa ,predicate naryrelation) metadata))
    (setq metadata (cons `(arity ,predicate 1) metadata))
    (setq metadata (cons `(specialty ,warehouse ,predicate) metadata))
    (setq metadata (cons `(interest ,warehouse ,predicate) metadata))
    (setq metadata (cons `(rootclass ,interface ,class) metadata)))
  (dolist (attribute (nreverse attributes))
    (setq dum (disconstruct attribute))
    (setq class (newfix (car dum) (cadr dum)))
    (setq slot (newfix (cadr dum) (caddr dum)))
    (setq metadata (cons `(attribute ,class ,attribute) metadata))
    (setq metadata (cons `(isa ,attribute attributerelation) metadata))
    (when (findp `(isa ,slot attributerelation) *manager*)
      (setq metadata (cons `(superrelation ,attribute ,slot) metadata)))
    (setq metadata (cons `(domain ,attribute ,class) metadata))
    (setq metadata (cons `(specialty ,warehouse ,attribute) metadata))
    (setq metadata (cons `(interest ,warehouse ,attribute) metadata))
    (setq metadata (cons `(searchstyle ,attribute dropdownlist) metadata)))
  (nreverse metadata)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Execution stuff
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun askfacts (msg)
  (do ((l (find-agents) (cdr l)) (nl))
      ((null l) nl)
      (cond ((and (eq (car l) *sender*) (equal msg *message*)))
            (t (setq nl (unionize nl (request msg *receiver* (car l))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; iterated version
;;;
;;; execution depends on *group*
;;;   nil  means nested
;;;   t    means all-at-once by copying
;;;   yank means all-at-once using oneof
;;;   yank2 means all-at-once using oneof and join
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defparameter *group* 'loyal)

(defun tellemall (q p)
  (dolist (piece (askemall q p)) (tellem piece)))

(defun tellem (p)
  (cond ((atom p))
        ((eq 'tell (car p))
         (dolist (agent (cadr p))
           (request `(tell ,(caddr p)) *receiver* agent)))
        ((eq 'untell (car p))
         (dolist (agent (cadr p))
           (request `(untell ,(caddr p)) *receiver* agent)))
        ((eq 'progn (car p)) (mapc #'tellem (cdr p)))))

(defun newtellemall (&rest rules)
  (let (tells)
    (do ((l rules (cdr l)))
        ((null l))
        (cond ((atom (car l)))
              ((eq 'tell (caar l)) (setq tells (cons (car l) tells)))
              ((eq '==> (caar l))
               (dolist (answer (askemall (caddar l) (cadar l)))
                 (setq tells (contribute answer tells))))))
    (setq tells (coalesce tells))
    (do ((l tells (cdr l)) (nl) (dum))
        ((null l) 'done)
        (setq dum (request `(tell ,(maksand (cdar l))) *receiver* (caar l)))
        (cond ((stringp dum)
               (do ((m nl (cdr m)) (untell))
                   ((null m))
                   (setq untell (maksand (nreverse (mapcar #'maknot (cdar m)))))
                   (request `(tell ,untell) *receiver* (caar m)))
               (return dum))
              (t (setq nl (cons (car l) nl)))))))

(defun newcoalesce (tells)
  (do ((l tells (cdr l)) (nl))
      ((null l) nl)
      (cond ((atom (cadar l)) (setq nl (cons (list (cadar l) (caddar l)) nl)))
            (t (dolist (who (cadar l)) (setq nl (cons (list who (caddar l)) nl)))))))

(defun coalesce (tells)
  (do ((l tells (cdr l)) (nl))
      ((null l) nl)
      (cond ((atom (cadar l)) (setq nl (coal (cadar l) (caddar l) nl)))
            (t (dolist (who (cadar l)) (setq nl (coal who (caddar l) nl)))))))

(defun coal (who what nl)
  (let (dum)
    (cond ((setq dum (assoc who nl :test #'eq))
           (rplacd dum (contribute what (cdr dum))) nl)
          (t (acons who (contribute what nil) nl)))))


(defun askemone (x p)
  (cond ((eq *group* 'loyal) (loyalone x p))
        ((eq *group* 'yank) (yankemone x p))
        ((eq *group* 'yank2) (yankemone2 x p))
        ((eq *group* 'rebel) (rebelone x p))
        (t (getemone x p))))

(defun askemall (x p)
  (cond ((eq *group* 'loyal) (loyalall x p))
        ((eq *group* 'yank) (yankemall x p))
        ((eq *group* 'yank2) (yankemall2 x p))
        ((eq *group* 'rebel) (rebelall x p))
        (*group* (findemall x p))
        (t (getemall x p))))

(defun assumeall (p th)
  (cond ((and th (symbolp th) (boundp th)) (assumeall p (symbol-value th)))
        (t (includes th *library*)
           (assume p th))))

(defun forgetall (p th)
  (cond ((and th (symbolp th) (boundp th)) (forgetall p (symbol-value th)))
        (t (includes th *library*)
           (forget p th))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; loyal - new flat all-at-once execution using oneof and no alists
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun loyalone (x p)
  (loyone x p '(t) '((t))))

(defun loyone (x p vl sl)
  (cond ((null p) nil)
        ((atom p) (loyproject x vl sl))
        ((eq 'ask (car p)) (loyoneask x p vl sl))
        ((eq 'and (car p)) (loyoneand x p vl sl))
        ((eq 'or (car p)) (loyoneor x p vl sl))
        ((eq 'unprovable (car p)) (loyoneunprovable x p vl sl))
        ((eq 'bagofall (car p)) (loyonebagofall x p vl sl))))

(defun loyoneask (x p vl sl)
  (let* ((db (cadr p)) (query (caddr p)))
    (cond ((or (null vl) (equal vl '(t)))
           (request `(ask-one ,x ,query) *receiver* db))
          (t (setq query (makand `(oneof ,vl . ,sl) query))
             (request `(ask-one ,x ,query) *receiver* db)))))

(defun loyoneand (x p vl sl)
  (cond ((null (cdr p)) (loyproject x vl sl))
        ((null (cddr p)) (loyone x (cadr p) vl sl))
        (t (do ((l (cdr p) (cdr l)) (vs (loyalvars x p vl) (cdr vs)))
               ((null (cdr l)) (loyone x (car l) vl sl))
               (if (setq sl (loyal (car vs) (car l) vl sl))
                 (setq vl (car vs))
                 (return nil))))))

(defun loyoneor (x p vl sl)
  (do ((l (cdr p) (cdr l)) (dum))
      ((null l) nil)
      (when (setq dum (loyone x (car l) vl sl)) (return dum))))

(defun loyoneunprovable (x p vl sl)
  (setq sl (set-difference sl (loyal vl (cadr p) vl sl) :test #'equalp))
  (if (equal x vl) (car sl) (loyproject x vl sl)))

(defun loyonebagofall (x p vl sl)
  (let (ivl answers)
    (setq ivl (unionize vl (vars (cadr p))))
    (setq answers (loyal ivl (caddr p) vl sl))
    (do ((l sl (cdr l)) (m answers) (nl))
        ((null l) (loyproject x (append vl (list (cadddr p))) (nreverse nl)))
        (do ((il)) (nil)
            (cond ((or (null m) (not (startp (car l) (car m))))
                   (setq nl (cons (append (car l) (list (cons 'listof (nreverse il)))) nl))
                   (return t))
                  (t (setq il (cons (loyalproj (cadr p) ivl (car m)) il))
                     (setq m (cdr m))))))))

(defun loyproject (x vl sl)
  (findx x `(oneof ,vl . ,sl) *manager*))


(defun loyalall (x p)
  (loyal x p '(t) '((t))))

(defun loyal (x p vl sl)
  (cond ((null p) nil)
        ((atom p) (loyalproject x vl sl))
        ((eq 'ask (car p)) (loyalask x p vl sl))
        ((eq 'and (car p)) (loyaland x p vl sl))
        ((eq 'or (car p)) (loyalor x p vl sl))
        ((eq 'unprovable (car p)) (loyalunprovable x p vl sl))
        ((eq 'bagofall (car p)) (loyalbagofall x p vl sl))))

(defun loyalask (x p vl sl)
  (let* ((db (cadr p)) (query (caddr p)))
    (cond ((or (null vl) (equal vl '(t)))
           (request `(ask-all ,x ,query) *receiver* db))
          (t (setq query (makand `(oneof ,vl . ,sl) query))
             (request `(ask-all ,x ,query) *receiver* db)))))

(defun loyaland (x p vl sl)
  (cond ((null (cdr p)) sl)
        ((null (cddr p)) (loyal x (cadr p) vl sl))
        (t (do ((l (cdr p) (cdr l)) (vs (loyalvars x p vl) (cdr vs)))
               ((null l) (if (equal x vl) sl (loyalproject x vl sl)))
               (if (setq sl (loyal (car vs) (car l) vl sl))
                 (setq vl (car vs))
                 (return nil))))))

(defun loyalor (x p vl sl)
  (do ((l (cdr p) (cdr l)) (nl))
      ((null l) (nreverse nl))
      (setq nl (nreconc (loyal x (car l) vl sl) nl))))

(defun loyalunprovable (x p vl sl)
  (setq sl (set-difference sl (loyal vl (cadr p) vl sl) :test #'equalp))
  (if (equal x vl) sl (loyalproject x vl sl)))

(defun loyalbagofall (x p vl sl)
  (let (ivl answers)
    (setq ivl (unionize vl (vars (cadr p))))
    (setq answers (loyal ivl (caddr p) vl sl))
    (do ((l sl (cdr l)) (m answers) (nl))
        ((null l) (loyalproject x (append vl (list (cadddr p))) (nreverse nl)))
        (do ((il)) (nil)
            (cond ((or (null m) (not (startp (car l) (car m))))
                   (setq nl (cons (append (car l) (list (cons 'listof (nreverse il)))) nl))
                   (return t))
                  (t (setq il (cons (loyalproj (cadr p) ivl (car m)) il))
                     (setq m (cdr m))))))))

(defun loyalproj (x vl s)
  (findx x `(same ,vl ,s) *manager*))

(defun loyalproject (x vl sl)
  (finds x `(oneof ,vl . ,sl) *manager*))

(defun loyalvars (x p vl)
  (cdr (loyalvarscdr vl (cdr p) (vars x))))

(defun loyalvarscdr (vl pl xl)
  (cond ((null pl) (list xl))
        (t (let (nl)
             (setq nl (loyalvarscdr (goodvarsform (car pl) vl) (cdr pl) xl))
             (cons (intersect vl (unionize (vars (car pl)) (car nl))) nl)))))

(defmethod variables ((al list))
  (do ((l al (cdr l)) (nl))
      ((null (cdr l)) nl)
      (setq nl (cons (caar l) nl))))

(defun bindings (al)
  (do ((l al (cdr l)) (nl))
      ((null (cdr l)) nl)
      (setq nl (cons (cdar l) nl))))

(defun startp (l1 l2)
  (do ()
      ((null l1) t)
      (cond ((null l2) (return nil))
            ((equalp (car l1) (car l2)) (setq l1 (cdr l1) l2 (cdr l2)))
            (t (return nil)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; one-by-one execution
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun getemone (x p)
  (let ((al (askone p truth)))
    (if al (sublis al x))))

(defun askone (p al)
  (cond ((null p) nil)
        ((atom p) (list al))
        ((eq 'ask (car p)) (askoneask p al))
        ((eq 'and (car p)) (askoneand p al))
        ((eq 'or (car p)) (askoneor p al))
        (t (askoneask (list 'ask *library* p) al))))

(defun askoneand (p al)
  (askoneands (cdr p) al))

(defun askoneands (cl al)
  (cond ((null cl) al)
        ((null (cdr cl)) (askone (car cl) al))
        (t (do ((l (askem (car cl) al) (cdr l)) (bl))
               ((null l) nil)
               (if (setq bl (askoneands (cdr cl) (car l))) (return bl))))))

(defun askoneask (p al)
  (let ((dum))
    (setq p (sublis al p))
    (when (setq dum (request `(ask-one ,(caddr p) ,(caddr p)) *receiver* (cadr p)))
      (matchpexp (caddr p) dum al))))

(defun askoneor (p al)
  (do ((l (cdr p) (cdr l)) (bl))
      ((null l))
      (if (setq bl (askone (car l) al)) (return bl))))

(defun getemall (x p)
  (do ((l (askem p truth) (cdr l)) (nl))
      ((null l) (nreverse nl))
      (setq nl (adjoin (sublis (car l) x) nl :test #'equal))))

(defun askem (p al)
  (cond ((null p) nil)
        ((atom p) (list al))
        ((eq 'ask (car p)) (askallask p al))
        ((eq 'and (car p)) (askalland p al))
        ((eq 'or (car p)) (askallor p al))
        (t (askallask (list 'ask *library* p) al))))

(defun askallask (p al)
  (setq p (sublis al p))
  (do ((l (request `(ask-all ,(caddr p) ,(caddr p)) *receiver* (cadr p)) (cdr l)) (nl))
      ((null l) (nreverse nl))
      (setq nl (cons (matchpexp (caddr p) (car l) al) nl))))

(defun askalland (p al)
  (askallands (cdr p) al))

(defun askallands (cl al)
  (cond ((null cl) (list al))
        (t (do ((l (askem (car cl) al) (cdr l)) (nl))
               ((null l) (nreverse nl))
               (setq nl (nreconc (askallands (cdr cl) (car l)) nl))))))

(defun askallor (p al)
  (do ((l (cdr p) (cdr l)) (nl))
      ((null l) (nreverse nl))
      (setq nl (nreconc (askem (car l) al) nl))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; new flat all-at-once execution using oneof but all alists
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun rebelone (x p)
  (let ((nl (rebel x p (list truth))))
    (if nl (sublis (car nl) x))))

(defun rebelall (x p)
  (do ((l (rebel x p (list truth)) (cdr l)) (nl))
      ((null l) (uniquify (nreverse nl)))
      (setq nl (cons (sublis (car l) x) nl))))

(defun rebel (x p sl)
  (cond ((null p) nil)
        ((atom p) sl)
        ((eq 'ask (car p)) (rebelask x p sl))
        ((eq 'and (car p)) (rebeland x p sl))
        ((eq 'or (car p)) (rebelor x p sl))
        ((eq 'unprovable (car p)) (rebelunprovable p sl))))

(defun rebelask (x p sl)
  (let* ((db (cadr p)) (query (caddr p)) ins answers)
    (cond ((null sl) nil)
          ((setq ins (variables (car sl)))
           (do ((l sl (cdr l)) (nl))
               ((null l)
                (setq query (makand `(oneof ,ins . ,(nreverse nl)) query)))
               (setq nl (cons (bindings (car l)) nl)))
           (setq x (economize x))
           (setq answers (request `(ask-all ,x ,query) *receiver* db))
           (do ((l answers (cdr l)) (nl))
               ((null l) (nreverse nl))
               (setq nl (cons (matchpexp x (car l) truth) nl))))
          (t (setq x (economize x))
             (setq answers (request `(ask-all ,x ,query) *receiver* db))
             (do ((l answers (cdr l)) (nl))
                 ((null l) (nreverse nl))
                 (setq nl (cons (matchpexp x (car l) truth) nl)))))))

(defun rebeland (x p sl)
  (cond ((null (cdr p)) sl)
        ((null (cddr p)) (rebel x (cadr p) sl))
        (t (do ((l (cdr p) (cdr l)) (vl (rebvars x p (car sl)) (cdr vl)))
               ((null l) sl)
               (setq sl (rebel (car vl) (car l) sl))))))

(defun rebelor (x p sl)
  (do ((l (cdr p) (cdr l)) (nl))
      ((null l) (nreverse nl))
      (setq nl (nreconc (rebel x (car l) sl) nl))))

(defun rebelunprovable (p sl)
  (set-difference sl (rebel (variables (car sl)) (cadr p) sl) :test #'equal))

(defun rebvars (x p al)
  (cdr (rebvarscdr (mapcar #'car al) (cdr p) (vars x))))

(defun rebvarscdr (vl pl xl)
  (cond ((null pl) (list xl))
        (t (let (nl)
             (setq nl (rebvarscdr (goodvarsform (car pl) vl) (cdr pl) xl))
             (cons (intersect vl (unionize (vars (car pl)) (car nl))) nl)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; nested all-at-once execution by copying and returning list of answers
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun findemone (x p)
  (let ((nl (findem (newvars x p) truth)))
    (if nl (sublis (car nl) x))))

(defun findemall (x p)
  (do ((l (findem (newvars x p) (list truth)) (cdr l)) (nl))
      ((null l) (nreverse nl))
      (setq nl (adjoin (sublis (car l) x) nl :test #'equal))))

(defun findem (p sl)
  (cond ((null p) nil)
        ((atom p) sl)
        ((eq 'ask (car p)) (findemask p sl))
        ((eq 'and (car p)) (findemand p sl))
        ((eq 'or (car p)) (findemor p sl))))

(defun findemask (p sl)
  (let* ((db (cadr p)) (aspect (caddr p)) (query (cadddr p)) answers)
    (cond ((null sl) nil)
          ((null (cdr sl))
           (unless aspect (setq aspect t))
           (setq query `(ask-all ,aspect ,(sublis (car sl) query)))
           (setq answers (request query *receiver* db))
           (do ((l answers (cdr l)) (nl))
               ((null l) (nreverse nl))
               (setq nl (cons (matchpexp aspect (car l) (car sl)) nl))))
          (t (unless aspect (setq aspect t))
             (setq query `(ask-all ,aspect ,query))
             (do ((l sl (cdr l)) (nl))
                 ((null l) (setq query (cons 'list (nreverse nl))))
               (setq nl (cons (sublis (car l) query) nl)))
             (setq answers (request query *receiver* db))
             (do ((l answers (cdr l)) (n sl (cdr n)) (nl))
                 ((null l) (nreverse nl))
                 (do ((m (car l) (cdr m)))
                     ((null m))
                     (setq nl (cons (matchpexp aspect (car m) (car n)) nl))))))))

(defun findemand (p sl)
  (findemands (cdr p) sl))

(defun findemands (cl sl)
  (cond ((null cl) sl)
        (t (findemands (cdr cl) (findem (car cl) sl)))))

(defun findemor (p sl)
  (do ((l (cdr p) (cdr l)) (nl))
      ((null l) (nreverse nl))
      (setq nl (nreconc (findem (car l) sl) nl))))

(defun newvars (x p)
  (newvarsexp p nil (vars x)))

(defun newvarsexp (p vl wl)
  (cond ((atom p) p)
        ((eq 'ask (car p))
         (let ((vs (goodvars (caddr p))) (news) (useless) (locals))
           (setq news (set-difference vs vl))
           (setq useless (set-difference news wl))
           (setq locals (set-difference news useless))
           `(ask ,(cadr p) ,locals ,(caddr p))))
        ((eq 'and (car p))
         (do ((l (cdr p) (cdr l)) (nl))
             ((null l) (cons 'and (nreverse nl)))
             (setq nl (cons (newvarsexp (car l) vl (append (vars (cdr l)) wl)) nl))
             (setq vl (nconc (goodvars (car l)) vl))))
        ((eq 'or (car p))
         (do ((l (cdr p) (cdr l)) (nl))
             ((null l) (cons 'or (nreverse nl)))
             (setq nl (cons (newvarsexp (car l) vl wl) nl))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; flat all-at-once execution using oneof but all alists
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun yankemone (x p)
  (let ((nl (yankem (newvars x p) (list truth))))
    (if nl (sublis (car nl) x))))

(defun yankemall (x p)
  (do ((l (yankem (newvars x p) (list truth)) (cdr l)) (nl))
      ((null l) (uniquify (nreverse nl)))
      (setq nl (cons (sublis (car l) x) nl))))

(defun yankem (p sl)
  (cond ((null p) nil)
        ((atom p) sl)
        ((eq 'ask (car p)) (yankemask p sl))
        ((eq 'and (car p)) (yankemand p sl))
        ((eq 'or (car p)) (yankemor p sl))))

(defun yankemask (p sl)
  (let* ((db (cadr p)) (aspect (caddr p)) (query (cadddr p)) ins answers)
    (cond ((null sl) nil)
          ((null (cdr sl))
           (setq aspect (economize aspect))
           (setq query `(ask-all ,aspect ,(sublis (car sl) query)))
           (setq answers (request query *receiver* db))
           (do ((l answers (cdr l)) (nl))
               ((null l) (nreverse nl))
               (setq nl (cons (matchpexp aspect (car l) (car sl)) nl))))
          (t (do ((l (car sl) (cdr l)))
                 ((null (cdr l)))
                 (setq ins (cons (caar l) ins)))
             (setq aspect (economize (unionize ins aspect)))
             (do ((l sl (cdr l)) (nl))
                 ((null l)
                  (setq query (makand `(oneof ,ins . ,(nreverse nl)) query)))
                 (setq nl (cons (sublis (car l) ins) nl)))
             (setq answers (request `(ask-all ,aspect ,query) *receiver* db))
             (do ((l answers (cdr l)) (nl))
                 ((null l) (nreverse nl))
                 (setq nl (cons (matchpexp aspect (car l) truth) nl)))))))

(defun yankemand (p sl)
  (yankemands (cdr p) sl))

(defun yankemands (cl sl)
  (cond ((null cl) sl)
        (t (yankemands (cdr cl) (yankem (car cl) sl)))))

(defun yankemor (p sl)
  (do ((l (cdr p) (cdr l)) (nl))
      ((null l) (nreverse nl))
      (setq nl (nreconc (yankem (car l) sl) nl))))


(defun yankemone2 (x p)
  (let ((nl (yankem2 (newvars x p) (list truth))))
    (if nl (sublis (car nl) x))))

(defun yankemall2 (x p)
  (do ((l (yankem2 (newvars x p) (list truth)) (cdr l)) (nl))
      ((null l) (uniquify (nreverse nl)))
      (setq nl (cons (sublis (car l) x) nl))))

(defun yankem2 (p sl)
  (cond ((null p) nil)
        ((atom p) sl)
        ((eq 'ask (car p)) (yankemask2 p sl))
        ((eq 'and (car p)) (yankemand2 p sl))
        ((eq 'or (car p)) (yankemor2 p sl))))

(defun yankemask2 (p sl)
  (let* ((db (cadr p)) (aspect (caddr p)) (query (cadddr p)) ins answers)
    (cond ((null sl) nil)
          ((null (cdr sl))
           (setq aspect (economize aspect))
           (setq query `(ask-all ,aspect ,(sublis (car sl) query)))
           (setq answers (request query *receiver* db))
           (do ((l answers (cdr l)) (nl))
               ((null l) (nreverse nl))
               (setq nl (cons (matchpexp aspect (car l) (car sl)) nl))))
          (t (setq ins (boundvars query (car sl)))
             (setq aspect (economize (unionize ins aspect)))
             (when ins
               (do ((l sl (cdr l)) (nl))
                   ((null l)
                    (setq query (makand `(oneof ,ins . ,(uniquify (nreverse nl))) query)))
                   (setq nl (cons (sublis (car l) ins) nl))))
             (setq answers (request `(ask-all ,aspect ,query) *receiver* db))
             (do ((l answers (cdr l)) (nl))
                 ((null l) (joinal sl (nreverse nl)))
                 (setq nl (cons (matchpexp aspect (car l) truth) nl)))))))

(defun economize (aspect)
  (cond ((null aspect) '(1))
        ((atom aspect) (list aspect))
        (t aspect)))

(defun yankemand2 (p sl)
  (yankemands2 (cdr p) sl))

(defun yankemands2 (cl sl)
  (cond ((null cl) sl)
        (t (yankemands2 (cdr cl) (yankem2 (car cl) sl)))))

(defun yankemor2 (p sl)
  (do ((l (cdr p) (cdr l)) (nl))
      ((null l) (nreverse nl))
      (setq nl (nreconc (yankem2 (car l) sl) nl))))

(defun boundvars (x al)
  (do ((l (vars x) (cdr l)) (nl))
      ((null l) (nreverse nl))
      (if (assoc (car l) al) (setq nl (cons (car l) nl)))))

;;; what do we do about ground queries?
;;; following assumes that alists all have same structure

(defun joinal (as bs)
  (do ((l as (cdr l)) (dum) (nl))
      ((null l) (nreverse nl))
      (do ((m bs (cdr m)))
          ((null m))
          (when (setq dum (joinemal (car l) (car m)))
            (setq nl (cons dum nl))))))

(defun joinemal (al bl)
  (let (dum)
    (cond ((null (cdr al)) bl)
          ((setq dum (assoc (caar al) bl :test #'eq))
           (if (equal (cdar al) (cdr dum)) (joinemal (cdr al) bl)))
          ((setq bl (joinemal (cdr al) bl)) (cons (car al) bl)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Miscellaneous
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun get-security (agent)
  (symbol-value (find-security (name agent))))

;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; (c) Copyright 1999-2003 by Michael Genesereth.  All rights reserved.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(eval-when (compile load eval)
  (proclaim '(special *manager* *intensions* *target*)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; reviser
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; general
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod reviser (facts receiver)
  (declare (ignore facts receiver))
  nil)

(defmethod reviser (facts (receiver symbol))
  (cond ((and (boundp receiver) (not (symbolp (symbol-value receiver))))
         (reviser facts (symbol-value receiver)))
        (t (call-next-method facts receiver))))

(defmethod reviser (facts (th fastserver))
  (change (maksand facts) th))

(defmethod reviser (facts (th dataserver))
  (change (maksand facts) th))

(defmethod reviser (facts (th ruleserver))
  (change (maksand facts) th))

(defmethod reviser (facts (th fullserver))
  (change (maksand facts) th))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Translator
;;; needs work
;;; nonviewp checks infotheory.  ugh.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod change (p (th translator))
  (let (*target*)
    (setq *target* (find-target (name th)))
    (request (planchange p th) th *target*)))

(defun planchange (p th)
  (let (*library* *target* (*intensions* nil))
    (setq *library* (symbol-value (find-rulebase th)))
    (setq *target* (find-target (name th)))
    (decludes 'infotheory)
    (empty 'infotheory)
    (includes 'infotheory *library*)
    (do ((l (newconsequences p 'infotheory #'unviewp #'unsavep) (cdr l)) (nl))
        ((null l) `(update . ,(nreverse nl)))
        (cond ((and (listp (car l)) (eq 'execute (caar l)))
               (setq nl (cons (cadar l) nl)))
              ((and (listp (car l)) (eq 'evaluate (caar l)))
               (setq nl (cons `(apply ',(caadar l) ',(cdadar l)) nl)))
              ((and (listp (car l)) (eq '=> (caar l)))
               (setq nl (cons (decolonize `(==> ,(cadar l) ,(caddar l))) nl)))
              (t (setq nl (cons (decolonize (car l)) nl)))))))

(defmethod reviser (facts (th translator))
  (let (*library* factserver changes)
    (setq *library* (symbol-value (find-rulebase th)))
    (setq factserver (find-target (name th)))
    (setq facts (transreducer facts th))
    (request `(update . ,facts) th factserver)
    (dolist (rule (ramifications (maksand facts) *library* #'unviewp #'interestp))
      (cond ((atom rule) (setq changes (cons rule changes)))
            ((eq '<= (car rule))
             (setq changes (nreconc (request `(ask-all ,(cadr rule) ,(maksand (cddr rule))) nil factserver) changes)))
            (t (setq changes (cons rule changes)))))
    (request `(update . ,(nreverse changes)) th factserver)
    'done))

(defun transreducer (facts receiver)
  (let (rulebase (*intensions* nil))
    (setq rulebase (find-rulebase (name receiver)))
    (when (and rulebase (symbolp rulebase) (boundp rulebase))
      (setq rulebase (symbol-value rulebase)))
    (getconjuncts (residue t (maksand facts) rulebase #'unviewp))))

(defun unviewp (x)
  (not (fullviewp x *library*)))

(defun unsavep (x)
  (not (fullsavep x *library*)))

(defun fullsavep (x th)
  (cond ((fullsavepth x th))
        (t (some #'(lambda (th) (fullsavep x th)) (includees th)))))

(defun fullsavepth (x th)
  (do ((l (indexees x th) (cdr l)))
      ((null l) nil)
      (cond ((atom (car l)))
            ((eq '=> (caar l))
             (when (and (listp (cadar l)) (eq x (caadar l))) (return t))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Facilitator
;;; okay but could be better
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod reviser (facts (th facilitator))
  (let (ruleserver changes)
    (setq ruleserver (symbol-value (find-rulebase th)))
    (setq facts (reducer facts th))
    (eval (divvy `(update . ,facts)))
    (dolist (rule (ramifications (maksand facts) ruleserver #'basicp #'interestp))
      (cond ((atom rule) (setq changes (cons rule changes)))
            ((eq '<= (car rule))
             (setq changes (nreconc (finds (cadr rule) (maksand (cddr rule)) th) changes)))
            (t (setq changes (cons rule changes)))))
    (eval (divvy `(update . ,changes)))
    'done))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Translator
;;; needs work
;;; nonviewp checks infotheory.  ugh.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod revise (facts (th translator))
  (let (ruleserver factserver)
    (setq ruleserver (symbol-value (find-rulebase th)))
    (setq factserver (find-target (name th)))
    (decludes 'infotheory)
    (empty 'infotheory)
    (includes 'infotheory ruleserver)
    (setq facts (reducetransviews facts th))
    (request `(update . ,facts) th factserver)
    (setq facts (ramifytransviews facts factserver ruleserver))
    (request `(update . ,facts) th factserver)
    'done))

(defun reducetransviews (facts receiver)
  (let (rulebase (*intensions* nil))
    (setq rulebase (find-rulebase (name receiver)))
    (when (and rulebase (symbolp rulebase) (boundp rulebase))
      (setq rulebase (symbol-value rulebase)))
    (getconjuncts (residue t (maksand facts) rulebase #'nonviewp))))

(defun ramifytransviews (facts factserver ruleserver)
  (let (insertions deletions bases materials changes)
    (multiple-value-setq (insertions deletions) (divide facts))
    (dolist (p facts) (setq bases (getbases p bases)))
    (dolist (r bases) (setq materials (gettransmaterials r ruleserver materials)))
    (dolist (r materials)
      (dolist (rule (gettranschanges r bases insertions deletions ruleserver))
        (setq changes (nreconc (request `(ask-all ,(cadr rule) ,(maksand (cddr rule))) nil factserver) changes))))
    (nreverse (uniquify changes))))

(defun gettransmaterials (p th nl)
  (do ((l (indexees p th) (cdr l)))
      ((null l) nl)
      (cond ((atom (car l)))
            ((eq '<= (caar l))
             (cond ((atom (cadar l)))
                   ((find (operator (cadar l)) nl))
                   ((amongp p (cddar l) #'eq)
                    (setq nl (cons (operator (cadar l)) nl))
                    (setq nl (getmaterials (operator (cadar l)) th nl))))))))

(defun gettranschanges (r bases positives negatives th)
  (do ((l (getrules r th) (cdr l)) (nl))
      ((null l) (nreverse nl))
      (do ((m (cddar l) (cdr m)) (om) (data) (new))
          ((null m))
          (cond ((atom (car m)))
                ((find (caar m) bases)
                 (when (setq data (getdata (caar m) positives))
                   (setq new `(oneof ,(car m) . ,data))
                   (setq new `(<= ,(cadar l) ,new . ,(revappend om (cdr m))))
                   (setq nl (cons new nl))))
                ((and (eq (caar m) 'not) (find (caadar m) bases))
                 (when (setq data (getdata (caadar m) negatives))
                   (setq new `(oneof ,(cadar m) . ,data))
                   (setq new `(<= ,(cadar l) ,new . ,(revappend om (cdr m))))
                   (setq nl (cons new nl)))))
          (setq om (cons (car m) om)))))

(defun gettransrules (r th)
  (nconc (normalizevars (nonreductions r th #'nonviewp #'success))
         (normalizevars (reductions r th #'nonviewp #'success))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Facilitator
;;; okay but could be better
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod revise (facts (th facilitator))
  (let (ruleserver)
    (setq ruleserver (symbol-value (find-rulebase th)))
    (setq facts (reduceviews facts th))
    (eval (divvy `(update . ,facts)))
    (setq facts (ramifyviews facts th ruleserver))
    (eval (divvy `(update . ,facts)))
    'done))

(defun reduceviewsfac (facts receiver)
  (let (rulebase (*intensions* nil))
    (setq rulebase (find-rulebase (name receiver)))
    (when (and rulebase (symbolp rulebase) (boundp rulebase))
      (setq rulebase (symbol-value rulebase)))
    (getconjuncts (residue t (maksand facts) rulebase #'basicp))))

(defun ramifyviewsfac (facts facilitator ruleserver)
  (let (insertions deletions bases materials changes)
    (multiple-value-setq (insertions deletions) (divide facts))
    (dolist (p facts) (setq bases (getbases p bases)))
    (dolist (r bases) (setq materials (getmaterials r ruleserver materials)))
    (dolist (r materials)
      (dolist (rule (getchanges r bases insertions deletions ruleserver))
        (setq changes (nreconc (finds (cadr rule) (maksand (cddr rule)) facilitator) changes))))
    (nreverse (uniquify changes))))

;;;;

(defmethod askanswer (s sender (receiver agent))
  (declare (ignore sender))
  (findanswer s receiver))

(defmethod askanswers (s sender (receiver agent))
  (declare (ignore sender))
  (findanswers s receiver))

(defmethod askc (p sender (receiver agent))
  (declare (ignore p sender))
  t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; fullconsequence
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun fullconsequence (p th &optional (*filter* #'failure) (*test* #'success))
  (let (alist *theory* *variables* *consequences* *answers* tracecalls)
    (setq *unifications* 0)
    (setq *inferences* 0)
    (setq *termination* nil)
    (setq alist (environment))
    (setq *theory* 'epitheory)
    (setq *variables* (vars p))
    (empty 'epitheory)
    (decludes 'epitheory)
    (includes *theory* th)
    (fullconsequencedepth p alist 0)
    (contents *theory*)))

(defun fullconsequencedepth (p al depth)
  (setq *inferences* (1+ *inferences*))
  (fullsave p al depth)
  (cond ((>= *inferences* *limit*) (setq *termination* t) (fullstop p al depth))
        ((>= depth *depth*) (setq *termination* t) (fullstop p al depth))
        (t (fullconsequenceexp p al depth))))

(defun fullconsequenceexp (p al depth)
  (cond ((atom p) (fullconsequenceexpexit p al depth))
        ((eq (car p) 'and) (fullconsequenceand p al depth))
        ((eq (car p) 'or) (fullconsequenceor p al depth))
        (t (fullconsequenceexpexit p al depth))))

(defun fullconsequenceand (p al depth)
  (do ((l (cdr p) (cdr l)))
      ((null l) t)
      (unless (fullconsequencedepth (car l) al depth) (return nil))))

(defun fullconsequenceor (p al depth)
  (do ((l (cdr p) (cdr l)))
      ((null l) nil)
      (when (fullconsequencedepth (car l) al depth) (return t))))

(defun fullconsequenceexpexit (p al depth)
  (setq p (plugstdexp p al))
  (cond ((knownp p *theory* 'samep) nil)
        ;((and (fullconsequence p al) nil))
        ((and (savep p) (insert p *theory*) nil))
        (t (fullconsequencedb p al depth *theory*))))

(defun fullconsequencedb (p al depth th)
  (fullconsequenceth p al depth th)
  (do ((l (includees th) (cdr l)))
      ((null l) t)
      (when (fullconsequencedb p al depth (car l)) (return nil))))

(defun fullconsequenceth (p al depth th)
  (do ((l (envindexps p al th) (cdr l)) (ol) (bl (environment)) (ans))
      ((null l) t)
      (when (and (listp (car l)) (eq (caar l) '=>) (not (null (cddar l)))
                 (setq ol (unify (cadar l) bl p al)))
        (if tracefacts (tracefact (car l)))
        (setq ans (fullconsequencedepth (car (last (car l))) bl (1+ depth)))
        (backup ol)
        (unless ans (return nil)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; fullconsequences
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defparameter *consequence* nil)

(defun fullconsequences (p *theory* &optional (*partial* #'failure) (*total* #'basep) (*test* #'success))
  (let (alist *variables* *consequence* *answers* tracecalls)
    (setq *unifications* 0)
    (setq *inferences* 0)
    (setq *termination* nil)
    (setq alist (environment))
    (setq *variables* (vars p))
    (fullconsequencesdepth p (list p) alist 0 nil)
    (nreverse *answers*)))

(defun fullconsequencesdepth (p pl al depth cont)
  (setq *inferences* (1+ *inferences*))
  (fullcall p al depth)
  (cond ((>= *inferences* *limit*) (setq *termination* t) (fullstop (car pl) al depth))
        ((>= depth *depth*) (setq *termination* t) (fullstop (car pl) al depth))
        (t (fullconsequencesexp p pl al depth cont))))

(defun fullconsequencesexp (p pl al depth cont)
  (cond ((atom p) (fullconsequencesconstant p pl al depth cont))
        ((eq 'not (car p)) (fullconsequencesnot (cadr p) pl al depth cont))
        ((eq 'and (car p)) (fullconsequencesand p pl al depth cont))
        ((eq 'or (car p)) (fullconsequencesor p pl al depth cont))
        ((eq 'oneof (car p)) (fullconsequencesassumption p pl al depth cont))
        ((eq 'member (car p)) (fullconsequencesmember p pl al depth cont))
        ((eq 'same (car p)) (fullconsequencessame p pl al depth cont))
        ((eq 'distinct (car p)) (fullconsequencesdistinct p pl al depth cont))
	((eq 'ground (car p)) (fullconsequencesground p pl al depth cont))
	((eq 'nonground (car p)) (fullconsequencesnonground p pl al depth cont))
	((eq 'primitive (car p)) (fullconsequencesprimitive p pl al depth cont))
	((eq 'nonprimitive (car p)) (fullconsequencesnonprimitive p pl al depth cont))
	((eq '== (car p)) (fullconsequencesvalue p pl al depth cont))
	((eq 'value (car p)) (fullconsequencesvalue p pl al depth cont))
        ((eq 'execute (car p)) (fullconsequencesexecute p pl al depth cont))
        ((eq 'evaluate (car p)) (fullconsequencesevaluate p pl al depth cont))
        ((eq 'unprovable (car p)) (fullconsequencesassumption p pl al depth cont))
        ((eq 'choose (car p)) (fullconsequenceschoose p pl al depth cont))
        ((eq 'bagofall (car p)) (fullconsequencesbagofall p pl al depth cont))
        ((eq 'stringmatch (car p)) (fullconsequencesstrmatch p pl al depth cont))
	((get (car p) 'basicval) (fullconsequencesbasicval p pl al depth cont))
        ((get (car p) 'basic) (fullconsequencesbasic p pl al depth cont))
        ((funcall *total* (car p)) (fullconsequencesassumption p pl al depth cont))
        ((funcall *partial* (car p) (fullconsequencespartial p pl al depth cont)))
        (t (fullconsequencesrs p pl al depth cont))))

(defun fullconsequencesnot (p pl al depth cont)
  (cond ((atom p) (fullconsequencesnotconstant p pl al depth cont))
        ((eq 'not (car p)) (fullconsequencesexp (cadr p) pl al depth cont))
        ((eq 'and (car p)) (fullconsequencesnotand p pl al depth cont))
        ((eq 'or (car p)) (fullconsequencesnotor p pl al depth cont))
        ((eq 'oneof (car p)) nil)
        ((eq 'same (car p)) nil)
        ((eq 'distinct (car p)) nil)
	((eq 'ground (car p)) nil)
	((eq 'nonground (car p)) nil)
	((eq 'primitive (car p)) nil)
	((eq 'nonprimitive (car p)) nil)
	((eq '== (car p)) (fullconsequencesnotvalue p pl al depth cont))
	((eq 'value (car p)) (fullconsequencesnotvalue p pl al depth cont))
        ((eq 'execute (car p)) (fullconsequencesnotexecute p pl al depth cont))
        ((eq 'evaluate (car p)) (fullconsequencesnotevaluate p pl al depth cont))
        ((eq 'unprovable (car p)) (fullconsequencesexp (cadr p) pl al depth cont))
        ((eq 'choose (car p)) (fullconsequencesnotchoose p pl al depth cont))
        ((eq 'bagofall (car p)) (fullconsequencesnotbagofall p pl al depth cont))
        ((eq 'stringmatch (car p)) (fullconsequencesnotstrmatch p pl al depth cont))
	((get (car p) 'basicval) (fullconsequencesnotbasicval p pl al depth cont))
        ((get (car p) 'basic) (fullconsequencesnotbasic p pl al depth cont))
        ((funcall *total* (car p)) (fullconsequencesassumption `(not ,p) pl al depth cont))
        ((funcall *partial* (car p)) (fullconsequencespartial `(not ,p) pl al depth cont))
        (t (fullconsequencesrs `(not ,p) pl al depth cont))))

(defun fullconsequencesconstant (p pl al depth cont)
  (cond ((eq 'true p) (fullconsequenceslast pl al depth cont))
        ((eq 'false p) (fullfail (car pl) al depth))
        ((funcall *total* p) (fullconsequencesassumption p pl al depth cont))
        ((funcall *partial* p) (fullconsequencespartial p pl al depth cont))
        (t (fullconsequencesrs p pl al depth cont))))

(defun fullconsequencesnotconstant (p pl al depth cont)
  (cond ((eq 'true p) (fullfail (car pl) al depth))
        ((eq 'false p) (fullconsequenceslast pl al depth cont))
        ((funcall *total* p) (fullconsequencesassumption `(not ,p) pl al depth cont))
        (t (fullconsequencesrs `(not ,p) pl al depth cont))))

(defun fullconsequencesand (p pl al depth cont)
  (cond ((null (cdr p)) (fullconsequenceslast pl al depth cont))
        ((fullconsequencesdepth (cadr p) (cdr p) al depth (cons (list pl al depth) cont)))
        (t (fullfail (car pl) al depth))))

(defun fullconsequencesnotand (p pl al depth cont)
  (setq cont (cons (list pl al depth) cont))
  (do ((l (cdr p) (cdr l)))
      ((null l) (fullfail (car pl) al depth))
      (fullconsequencesdepth (maknot (car l)) (list (maknot (car l))) al depth cont)))

(defun fullconsequencesor (p pl al depth cont)
  (setq cont (cons (list pl al depth) cont))
  (do ((l (cdr p) (cdr l)))
      ((null l) (fullfail (car pl) al depth))
      (when (fullconsequencesdepth (car l) (list (car l)) al depth cont))))

(defun fullconsequencesnotor (p pl al depth cont)
  (cond ((null (cdr p)) (fullconsequenceslast pl al depth cont))
        ((fullconsequencesdepth (maknot (cadr p)) (mapcar #'maknot (cdr p)) al depth
                  (cons (list pl al depth) cont)))
        (t (fullfail (car pl) al depth))))

(defun fullconsequencesoneof (p pl al depth cont)
  (when (seqvarp (caddr p)) (setq p (plug p al)))
  (do ((l (cddr p) (cdr l)) (ol))
      ((null l) (fullfail (car pl) al depth))
      (when (setq ol (unify (cadr p) al (car l) al))
        (fullconsequencesexit pl al depth cont)
        (backup ol))))

(defun fullconsequencesmember (p pl al depth cont)
  (do ((l (cdaddr p) (cdr l)) (ol))
      ((null l) (fullfail (car pl) al depth))
      (when (setq ol (unify (cadr p) al (car l) al))
        (fullconsequencesexit pl al depth cont)
        (backup ol))))

(defun fullconsequencessame (p pl al depth cont)
  (let (ol)
    (cond ((setq ol (unify (cadr p) al (caddr p) al))
           (prog1 (fullconsequenceslast pl al depth cont) (backup ol)))
          (t (fullfail (car pl) al depth)))))

(defun fullconsequencesdistinct (p pl al depth cont)
  (let ((ol))
    (cond ((setq ol (unify (cadr p) al (caddr p) al))
           (backup ol) (fullfail (car pl) al depth))
          (t (fullconsequenceslast pl al depth cont)))))

(defun fullconsequencesground (p pl al depth cont)
  (cond ((and (groundp (plug (cadr p) al))) (fullconsequenceslast pl al depth cont))
        (t (fullfail (car pl) al depth))))

(defun fullconsequencesnonground (p pl al depth cont)
  (cond ((groundp (plug (cadr p) al)) (fullfail (car pl) al depth))
        (t (fullconsequenceslast pl al depth cont))))

(defun fullconsequencesprimitive (p pl al depth cont)
  (cond ((and (primitivep (plug (cadr p) al))) (fullconsequenceslast pl al depth cont))
        (t (fullfail (car pl) al depth))))

(defun fullconsequencesnonprimitive (p pl al depth cont)
  (cond ((primitivep (plug (cadr p) al)) (fullfail (car pl) al depth))
        (t (fullconsequenceslast pl al depth cont))))

(defun fullconsequencesvalue (p pl al depth cont)
  (let (x y ol)
    (setq p (plug p al) x (cadr p) y (caddr p))
    (cond ((atom x))
          ((eq 'bagofall (car x))
           (setq x (cons 'listof (fullfinds (cadr x) (caddr x) *theory*))))
          ((and (groundp x) (every #'primitivep (cdr x)))
           (setq x (funcall (get (car x) 'basicval) x))))
    (cond ((setq ol (unify x al y al))
           (prog1 (fullconsequenceslast pl al depth cont) (backup ol)))
          (t (fullfail (car pl) al depth)))))

(defun fullconsequencesnotvalue (p pl al depth cont)
  (let (x y ol)
    (setq p (plug p al) x (cadr p) y (caddr p))
    (cond ((atom x))
          ((eq 'bagofall (car x))
           (setq x (cons 'listof (fullfinds (cadr x) (caddr x) *theory*))))
          ((and (groundp x) (every #'primitivep (cdr x)))
           (setq x (funcall (get (car x) 'basicval) x))))
    (cond ((setq ol (unify x al y al)) (backup ol) (fullfail (car pl) al depth))
          (t (fullconsequenceslast pl al depth cont)))))

(defun fullconsequencesexecute (p pl al depth cont)
  (let (values ol)
    (setq p (plug p al))
    (cond ((null (cddr p))
           (cond ((ignore-errors (eval (cadr p))) (fullconsequenceslast pl al depth cont))
                 (t (fullfail (car pl) al depth))))
          ((and (car (setq values (ignore-errors (multiple-value-list (eval (cadr p))))))
                (setq ol (matchify `(listof . ,(cddr p)) al `(listof . ,(mapcar #'quotify values)) al)))
           (prog1 (fullconsequenceslast pl al depth cont) (backup ol)))
          (t (fullfail (car pl) al depth)))))

(defun fullconsequencesnotexecute (p pl al depth cont)
  (let (values ol)
    (setq p (plug p al))
    (cond ((null (cddr p))
           (cond ((ignore-errors (not (eval (cadr p)))) (fullconsequenceslast pl al depth cont))
                 (t (fullfail (car pl) al depth))))
          ((and (car (setq values (ignore-errors (multiple-value-list (eval (cadr p))))))
                (not (setq ol (matchify `(listof . ,(cddr p)) al `(listof . ,(mapcar #'quotify values)) al)))
           (fullconsequenceslast pl al depth cont)))
          (t (backup ol) (fullfail (car pl) al depth)))))          

(defun fullconsequencesevaluate (p pl al depth cont)
  (let (values ol)
    (setq p (plug p al))
    (cond ((null (cddr p))
           (cond ((ignore-errors (apply (caadr p) (cdadr p))) (fullconsequenceslast pl al depth cont))
                 (t (fullfail (car pl) al depth))))
          ((and (car (setq values (ignore-errors (multiple-value-list (apply (caadr p) (cdadr p))))))
                (setq ol (matchify `(listof . ,(cddr p)) al `(listof . ,values) al)))
           (prog1 (fullconsequenceslast pl al depth cont) (backup ol)))
          (t (fullfail (car pl) al depth)))))

(defun fullconsequencesnotevaluate (p pl al depth cont)
  (let (values ol)
    (setq p (plug p al))
    (cond ((null (cddr p))
           (cond ((ignore-errors (not (apply (caadr p) (cdadr p)))) (fullconsequenceslast pl al depth cont))
                 (t (fullfail (car pl) al depth))))
          ((and (car (setq values (ignore-errors (multiple-value-list (apply (caadr p) (cdadr p))))))
                (not (setq ol (matchify `(listof . ,(cddr p)) al `(listof . ,values) al))))
           (fullconsequenceslast pl al depth cont))
          (t (backup ol) (fullfail (car pl) al depth)))))

(defun fullconsequencesunprovable (p pl al depth cont)
  (cond ((fullone (cadr p) pl al depth cont) (fullfail (car pl) al depth))
        (t (fullconsequenceslast pl al depth cont))))

(defun fullconsequenceschoose (p pl al depth cont)
  (let (x ol)
    (setq p (plugstdexp p al))
    (setq x (findx (cadr p) (caddr p) *theory*))
    (cond ((and (not (null x)) (setq ol (unify (cadr p) alist x alist)))
           (prog1 (fullconsequenceslast pl al depth cont) (backup ol)))
          (t (fullfail (car pl) al depth)))))

(defun fullconsequencesnotchoose (p pl al depth cont)
  (setq p (plugstdexp p al))
  (cond ((findp (caddr p) *theory*) (fullfail (car pl) al depth))
        (t (fullconsequenceslast pl al depth cont))))

(defun fullconsequencesbagofall (p pl al depth cont)
  (fullconsequencesvalue `(value ,(butlast p) ,(cadddr p)) pl al depth cont))

(defun fullconsequencesnotbagofall (p pl al depth cont)
  (fullconsequencesnotvalue `(value ,(butlast p) ,(cadddr p)) pl al depth cont))

(defun fullconsequencesstrmatch (p pl al depth cont)
  (fullconsequencesexp `(execute (strmatches ,(cadr p) ,(caddr p)) ? . ,(cdddr p)) pl al depth cont))

(defun fullconsequencesnotstrmatch (p pl al depth cont)
  (fullconsequencesnot `(execute (strmatches ,(cadr p) ,(caddr p)) ? . ,(cdddr p)) pl al depth cont))

(defun fullconsequencesbasicval (p pl al depth cont)
  (let (x y ol)
    (setq x (butlast p) y (car (last p)))
    (cond ((and (setq x (groundplugstdexp x al)) (every #'primitivep (cdr x)))
           (setq x (funcall (get (car x) 'basicval) x))
           (if (and (not (null x)) (setq ol (unify y al x al)))
             (prog1 (fullconsequenceslast pl al depth cont) (backup ol))
             (fullfail (car pl) al depth)))
          (t (fullconsequencesassumption p pl al depth cont)))))

(defun fullconsequencesnotbasicval (p pl al depth cont)
  (let (x y ol)
    (setq x (butlast p) y (car (last p)))
    (cond ((and (setq x (groundplugstdexp x al)) (every #'primitivep (cdr x)))
           (setq x (funcall (get (car x) 'basicval) x))
           (cond ((null x) (fullfail (car pl) al depth))
                 ((setq ol (unify y al x al)) (backup ol) (fullfail (car pl) al depth))
                 (t (fullconsequenceslast pl al depth cont))))
          (t (fullconsequencesassumption `(not ,p) pl al depth cont)))))

(defun fullconsequencesbasic (p pl al depth cont)
  (setq p (plugstdexp p al))
  (cond ((and (groundp p) (apply (get (car p) 'basic) (cdr p)))
         (fullconsequenceslast pl al depth cont))
        (t (let ((*consequence* (cons p *consequence*))) (fullconsequenceslast pl al depth cont)))))

(defun fullconsequencesnotbasic (p pl al depth cont)
  (setq p (plug p al))
  (cond ((and (groundp p) (not (apply (get (car p) 'basic) (cdr p))))
         (fullconsequenceslast pl al depth cont))
        (t (let ((*consequence* (cons `(not ,p) *consequence*))) (fullconsequenceslast pl al depth cont)))))

(defun fullconsequencespartial (p pl al depth cont)
  (setq p (plugstdexp p al))
  (cond ((find (maknot p) *consequence* :test #'equalp) (fullfail (car pl) al depth))
        (t (let ((*consequence* (adjoin p *consequence* :test #'equalp)))
             (fullconsequencesrs p pl al depth cont)))))

(defun fullconsequencesassumption (p pl al depth cont)
  (setq p (plugstdexp p al))
  (cond ((find p *consequence* :test #'equalp) (fullconsequenceslast pl al depth cont))
        ((or (not *consistency*)
             (not (or (rebuttalp p *consequence*) (rebuttheoryp p *theory*)))
             (consistentp p *consequence*))
         (let ((*consequence* (cons p *consequence*))) (fullconsequenceslast pl al depth cont)))
        (t  (fullfail (car pl) al depth))))

(defun fullconsequencesrs (p pl al depth cont)
  (cond ((and *ancestry* (fullconsequencesancestor p al cont)) (fullfail (car pl) al depth))
        ((and (numberp *ancestry*) (fullconsequencesnumber p al cont 0))
         (fullconsequencesassumption p pl al depth cont))
        ((and *reduction* (fullconsequencesreduce p pl al depth cont)))
        ((fullconsequencesdb p pl al depth cont *theory*))
        (t (fullfail (car pl) al depth))))

(defun fullconsequencesancestor (p al cont)
  (do ((l cont (cdr l)))
      ((null l) nil)
      (if (identify (caaar l) (cadar l) p al) (return t))))

(defun fullconsequencesnumber (p al cont n)
  (let (ol)
    (cond ((numgeqp n *ancestry*))
          ((null cont) nil)
          ((atom p)
           (fullconsequencesnumber p al (cdr cont) (if (eq p (caaar cont)) (1+ n) n)))
          ((setq ol (unify p al (caaar cont) (cadar cont)))
           (prog1 (fullconsequencesnumber p al (cdr cont) (1+ n)) (backup ol)))
          (t (fullconsequencesnumber p al (cdr cont) n)))))

(defun fullconsequencesreduce (p pl al depth cont)
  (do ((l cont (cdr l)) (ol))
      ((null l))
      (when (setq ol (unify (maknot (caaar l)) (cadar l) p al))
        (fullconsequencesexit pl al depth cont)
        (backup ol))))

(defun fullconsequencesdb (p pl al depth cont th)
  (fullconsequencesth p pl al depth cont th)
  (do ((l (includees th) (cdr l)))
      ((null l) nil)
      (fullconsequencesdb p pl al depth cont (car l))))

(defun fullconsequencesth (p pl al depth cont th)
  (do ((l (envindexps p al th) (cdr l)) (bl (environment)) (ol))
      ((null l))
      (cond ((and (listp (car l)) (eq '<= (caar l)) (null (cddar l)))
             (when (setq ol (unify (cadar l) bl p al))
               (fullconsequencesexit pl al depth cont)
               (cond ((subolp ol (alist bl)) (backup ol) (return nil))
                   (t (backup ol)))))
            ((and (listp (car l)) (eq '<= (caar l)))
             (when (setq ol (unify (cadar l) bl p al))
               (fullconsequencesdepth (caddar l) (cddar l) bl
                                  (1+ depth) (cons (list pl al depth) cont))
               (backup ol)))
            ((setq ol (unify (car l) bl p al))
             (fullconsequencesexit pl al depth cont)
             (cond ((subolp ol (alist bl)) (backup ol) (return nil))
                   (t (backup ol)))))))

(defun fullconsequencesexit (pl al depth cont)
  (let (dum ans)
    (fullexit (car pl) al depth)
    (cond ((cdr pl) (fullconsequencesdepth (cadr pl) (cdr pl) al depth cont))
          (cont (fullconsequencesexit (caar cont) (cadar cont) (caddar cont) (cdr cont)))
          (t (setq ans (plugstdexp (reverse *consequence*) alist))
             (dolist (var *variables*)
               (unless (eq (setq dum (plugstdexp var alist)) var)
                 (setq ans (cons `(same ,var ,dum) ans))))
             (setq *answers* (cons (maksand ans) *answers*))))
    (fullredo (car pl) al depth)))

(defun fullconsequenceslast (pl al depth cont)
  (let (dum ans)
    (fullexit (car pl) al depth)
    (cond ((cdr pl) (fullconsequencesdepth (cadr pl) (cdr pl) al depth cont))
          (cont (fullconsequencesexit (caar cont) (cadar cont) (caddar cont) (cdr cont)))
          (t (setq ans (plugstdexp (reverse *consequence*) alist))
             (dolist (var *variables*)
               (unless (eq (setq dum (plugstdexp var alist)) var)
                 (setq ans (cons `(same ,var ,dum) ans))))
             (setq *answers* (cons (maksand ans) *answers*))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; test
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#|
(deftheory foo
  (=> (r ?x) (p ?x))
  (=> (p ?x) (q ?x) (s ?x)))

(fullconsequence '(r a) 'foo #'success #'success)

(deftheory foo
  (=> (r ?x) (or (p ?x) (q ?x)))
  (=> (q ?x) (s ?x))
  (=> (p ?x) violation))

(deftheory bar
  (<= (r ?x) (p ?x))
  (<= (r ?x) (q ?x))
  (<= (q ?x) (s ?x))
  (<= (p ?x) violation))

(fullconsequences '(r a) 'bar #'success #'failure #'success)

(deftheory baz
  (<= (not (r* ?x)) (not (r ?x)) (not (p* ?x)))
  (<= (not (r* ?x)) (not (r ?x)) (not (q* ?x)))
  (<= (not (q* ?x)) (not (q ?x)) (not (s* ?x)))
  (<= (not (s* ?x)) (not (s ?x)))
  (<= (not (p* ?x)) (not (p ?x)) violation))

(fullconsequences '(not (r* a)) 'baz #'success #'failure #'success)

(defun nonviolationp (x) (find x '(p q r s)))

(deftheory bat
  (<= (not (r* ?x)) (not (p* ?x)))
  (<= (not (r* ?x)) (not (q* ?x)))
  (<= (not (q* ?x)) (not (q ?x)) (not (s* ?x)))
  (<= (not (s* ?x)) (not (s ?x)))
  (<= (not (p* ?x)) (not (p ?x)) violation))

(fullresidues '(r* a)) 'bat #'nonviolationp #'failure #'success)

(deftheory baz
  (<= (r* ?x) (p* ?x))
  (<= (r* ?x) (q* ?x))
  (<= (q* ?x) (q ?x) (s* ?x))
  (<= (s* ?x) (s ?x))
  (<= (p* ?x) (p ?x) violation))

(defun nonviolationp (x) (find x '(p q s)))

(fullresidues '(r* a)) 'baz #'nonviolationp #'failure #'success)

(deftheory baz
  (<= (r* ?x) (p* ?x) (q* ?x))
  (<= (p* ?x) (p ?x))
  (<= (q* ?x) (q ?x) (s* ?x))
  (<= (s* ?x) (s ?x)))

(defun nonviolationp (x) (find x '(p q s)))

(fullresidues '(r* a)) 'baz #'nonviolationp #'failure #'success)

(deftheory baz
  (<= (r* ?x) (p* ?x))
  (<= (r* ?x) (q* ?x))
  (<= (p* ?x) (p ?x) (s* ?x) (t* ?x))
  (<= (q* ?x) (q ?x))
  (<= (s* ?x) (s ?x))
  (<= (t* ?x) (t ?x)))

(defun nonviolationp (x) (find x '(p q s t)))

(fullresidues '(r* a)) 'baz #'nonviolationp #'failure #'success)

(deftheory gates
  (<= (event.location+ ?e gates-200)
      (event.location ?e gates-200)
      (mail+ mrg ?e))

  (<= (mail+ ?p ?e)
      (mail ?p ?e)))

(defun oldp (x)
  (setq x (elt (symbol-name x) (1- (length (symbol-name x)))))
  (and (not (char= x #\+)) (not (char= x #\-))))

(fullresidues '(event.location+ e23 gates-200) 'gates #'oldp #'failure #'success)

(deftheory album
  (<= (photo.thumb+ ?p)
      (photo.thumb ?p))

  (<= (photo.jpeg+ ?p)
      (photo.jpeg ?p))

  (<= (photo.thumb- ?p)
      (not (photo.thumb ?p))
      (photo.jpeg+ ?p))

  (<= (photo.thumb- ?p)
      (not (photo.thumb ?p))
      (photo.jpeg ?p))

  (<= (photo.jpeg- ?p)
      (not (photo.jpeg ?p))
      (photo.thumb+ ?p))

  (<= (photo.jpeg- ?p)
      (not (photo.jpeg ?p))
      (photo.thumb ?p)))

(fullresidues '(and (photo.jpeg+ p1) (photo.thumb- p1))
              'album #'oldp #'failure #'success)

(deftheory album
  (<= (photo.thumb* ?p)
      (photo.thumb+ ?p))

  (<= (photo.jpeg* ?p)
      (photo.jpeg+ ?p))

  (<= (not (photo.thumb* ?p))
      (photo.thumb- ?p)
      (photo.jpeg* ?p))

  (<= (not (photo.thumb ?p))
      (photo.thumb- ?p)
      (photo.jpeg ?p))

  (<= (photo.jpeg~ ?p ?j)
      (photo.jpeg- ?p ?j)
      (photo.thumb ?p (thumb)))

  (<= (photo.jpeg~ ?p ?j)
      (photo.jpeg- ?p ?j)
      (photo.jpeg ?p (jpeg))
      (different (jpeg) a))

  (<= (photo.jpeg~ ?p ?j)
      (photo.jpeg- ?p ?j)
      (photo.jpeg+ ?p (jpeg))
      (different (jpeg) ?j)))

(defun newp (x)
  (setq x (elt (symbol-name x) (1- (length (symbol-name x)))))
  (not (or (char= x #\*) (char= x #\~))))

(fullresidues '(photo.jpeg~ p1 "jpeg") 'album #'newp #'failure #'success)
((AND (PHOTO.JPEG- P1 "jpeg") (PHOTO.THUMB P1 (THUMB))) 
 (AND (PHOTO.JPEG- P1 "jpeg") (PHOTO.JPEG P1 (JPEG)) (DIFFERENT (JPEG) A)) 
 (AND (PHOTO.JPEG- P1 "jpeg") (PHOTO.JPEG+ P1 (JPEG)) (DIFFERENT (JPEG) "jpeg")))
|#

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; fulltruep
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun fulltruep (p th)
  (fulltruex 't p th))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; fulltruex
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun fulltruex (*thing* p *theory*)
  (let (alist *answer*)
    (setq alist (environment))
    (when (fullsingleexp p (list p) alist 0 nil) *answer*)))

(defun fullsingleexp (p pl al depth cont)
  (fullcall p al depth)
  (cond ((atom p) (fullsingleconstant p pl al depth cont))
        ((eq (car p) 'exists) (fullsingleexp (caddr p) pl al depth cont)) ; vars
        ((eq (car p) 'forall) nil)
        ((eq (car p) 'not) (fullsinglenot (cadr p) pl al depth cont))
        ((eq (car p) 'and) (fullsingleand p pl al depth cont))
        ((eq (car p) 'or) (fullsingleor p pl al depth cont))
        ((eq (car p) 'oneof) (fullsingleoneof p pl al depth cont))
        ((eq (car p) 'same) (fullsinglesame p pl al depth cont))
        ((eq (car p) 'distinct) (fullsingledistinct p pl al depth cont))
	((eq (car p) 'ground) (fullsingleground p pl al depth cont))
	((eq (car p) 'nonground) (fullsinglenonground p pl al depth cont))
	((eq (car p) 'primitive) (fullsingleprimitive p pl al depth cont))
	((eq (car p) 'nonprimitive) (fullsinglenonprimitive p pl al depth cont))
	((eq '== (car p)) (fullsinglevalue p pl al depth cont))
	((eq 'value (car p)) (fullsinglevalue p pl al depth cont))
        ((eq 'execute (car p)) (fullsingleexecute p pl al depth cont))
        ((eq 'evaluate (car p)) (fullsingleevaluate p pl al depth cont))
        ((eq 'bagofall (car p)) (fullsinglebagofall p pl al depth cont))
        ((eq 'unprovable (car p)) (fullsingleunprovable p pl al depth cont))
        ((eq 'choose (car p)) (fullsinglechoose p pl al depth cont))
        ((eq 'stringmatch (car p)) (fullsinglestrmatch p pl al depth cont))
	((get (car p) 'basicval) (fullsinglebasicval p pl al depth cont))
        ((get (car p) 'basic) (fullsinglebasic p pl al depth cont))
        (t (fullsinglers p pl al depth cont))))

(defun fullsinglenot (p pl al depth cont)
  (cond ((atom p) (fullsinglenotconstant p pl al depth cont))
        ((eq (car p) 'exists) (fullsingleunprovable `(not ,p) pl al depth cont))
        ((eq (car p) 'forall) (fullsingleunprovable `(not ,p) pl al depth cont))
        ((eq (car p) 'not) (fullsingleexp (cadr p) pl al depth cont))
        ((eq (car p) 'and) (fullsinglenotand p pl al depth cont))
        ((eq (car p) 'or) (fullsinglenotor p pl al depth cont))
        ((eq (car p) 'oneof) (fullfail (car pl) al depth))
        ((eq (car p) 'same) (fullfail (car pl) al depth))
        ((eq (car p) 'distinct) (fullfail (car pl) al depth))
	((eq (car p) 'ground) (fullfail (car pl) al depth))
	((eq (car p) 'nonground) (fullfail (car pl) al depth))
	((eq (car p) 'primitive) (fullfail (car pl) al depth))
	((eq (car p) 'nonprimitive) (fullfail (car pl) al depth))
	((eq '== (car p)) (fullsinglenotvalue p pl al depth cont))
	((eq 'value (car p)) (fullsinglenotvalue p pl al depth cont))
        ((eq 'execute (car p)) (fullsinglenotexecute p pl al depth cont))
        ((eq 'evaluate (car p)) (fullsinglenotevaluate p pl al depth cont))
        ((eq 'unprovable (car p)) (fullsingleexp (cadr p) pl al depth cont))
        ((eq 'choose (car p)) (fullsinglenotchoose p pl al depth cont))
        ((eq 'stringmatch (car p)) (fullsinglenotstrmatch p pl al depth cont))
	((get (car p) 'basicval) (fullsinglenotbasicval p pl al depth cont))
        ((get (car p) 'basic) (fullsinglenotbasic p pl al depth cont))
        (t (fullsingleunprovable `(not ,p) pl al depth cont))))

(defun fullsingleconstant (p pl al depth cont)
  (cond ((eq 'true p) (fullsinglelast pl al depth cont))
        ((eq 'false p) (fullfail (car pl) al depth))
        (t (fullsinglers p pl al depth cont))))

(defun fullsinglenotconstant (p pl al depth cont)
  (cond ((eq 'true p) (fullfail (car pl) al depth))
        ((eq 'false p) (fullsinglelast pl al depth cont))
        (t (fullsingleunprovable `(not ,p) pl al depth cont))))

(defun fullsingleand (p pl al depth cont)
  (cond ((null (cdr p)) (fullsinglelast pl al depth cont))
        ((fullsingleexp (cadr p) (cdr p) al depth (cons (list pl al depth) cont)))
        (t (fullfail (car pl) al depth))))

(defun fullsinglenotand (p pl al depth cont)
  (setq cont (cons (list pl al depth) cont))
  (do ((l (cdr p) (cdr l)))
      ((null l) (fullfail (car pl) al depth))
      (when (fullsingleexp (maknot (car l)) (list (maknot (car l))) al depth cont)
        (return t))))

(defun fullsingleor (p pl al depth cont)
  (setq cont (cons (list pl al depth) cont))
  (do ((l (cdr p) (cdr l)))
      ((null l) (fullfail (car pl) al depth))
      (when (fullsingleexp (car l) (list (car l)) al depth cont)
        (return t))))

(defun fullsinglenotor (p pl al depth cont)
  (cond ((null (cdr p)) (fullsinglelast pl al depth cont))
        ((fullsingleexp (maknot (cadr p)) (mapcar #'maknot (cdr p)) al depth
                  (cons (list pl al depth) cont)))
        (t (fullfail (car pl) al depth))))

(defun fullsingleoneof (p pl al depth cont)
  (when (seqvarp (caddr p)) (setq p (plug p al)))
  (do ((l (cddr p) (cdr l)) (ol))
      ((null l) (fullfail (car pl) al depth))
      (cond ((not (setq ol (unify (cadr p) al (car l) al))))
            ((fullsingleexit pl al depth cont) (backup ol) (return t))
            (t (backup ol)))))

(defun fullsinglemember (p pl al depth cont)
  (when (seqvarp (caddr p)) (setq p (plug p al)))
  (do ((l (cdaddr p) (cdr l)) (ol))
      ((null l) (fullfail (car pl) al depth))
      (cond ((not (setq ol (unify (cadr p) al (car l) al))))
            ((fullsingleexit pl al depth cont) (backup ol) (return t))
            (t (backup ol)))))

(defun fullsinglesame (p pl al depth cont)
  (let (ol)
    (cond ((setq ol (unify (cadr p) al (caddr p) al))
           (prog1 (fullsinglelast pl al depth cont) (backup ol)))
          (t (fullfail (car pl) al depth)))))

(defun fullsingledistinct (p pl al depth cont)
  (let ((ol))
    (cond ((setq ol (unify (cadr p) al (caddr p) al))
           (backup ol) (fullfail (car pl) al depth))
          (t (fullsinglelast pl al depth cont)))))

(defun fullsingleground (p pl al depth cont)
  (cond ((and (groundp (plug (cadr p) al))) (fullsinglelast pl al depth cont))
        (t (fullfail (car pl) al depth))))

(defun fullsinglenonground (p pl al depth cont)
  (cond ((groundp (plug (cadr p) al)) (fullfail (car pl) al depth))
        (t (fullsinglelast pl al depth cont))))

(defun fullsingleprimitive (p pl al depth cont)
  (cond ((primitivep (plug (cadr p) al)) (fullsinglelast pl al depth cont))
        (t (fullfail (car pl) al depth))))

(defun fullsinglenonprimitive (p pl al depth cont)
  (cond ((primitivep (plug (cadr p) al)) (fullfail (car pl) al depth))
        (t (fullsinglelast pl al depth cont))))

(defun fullsinglevalue (p pl al depth cont)
  (let (x y ol)
    (setq p (plug p al) x (cadr p) y (caddr p))
    (cond ((atom x))
          ((eq 'bagofall (car x))
           (setq x (cons 'listof (fulltrues (cadr x) (caddr x) *theory*))))
          ((and (groundp x) (every #'primitivep (cdr x)))
           (setq x (funcall (get (car x) 'basicval) x))))
    (cond ((setq ol (unify x al y al))
           (prog1 (fullsinglelast pl al depth cont) (backup ol)))
          (t (fullfail (car pl) al depth)))))

(defun fullsinglenotvalue (p pl al depth cont)
  (let (x y ol)
    (setq p (plug p al) x (cadr p) y (caddr p))
    (cond ((atom x))
          ((eq 'bagofall (car x))
           (setq x (cons 'listof (fulltrues (cadr x) (caddr x) *theory*))))
          ((and (groundp x) (every #'primitivep (cdr x)))
           (setq x (funcall (get (car x) 'basicval) x))))
    (cond ((setq ol (unify x al y al)) (backup ol) (fullfail (car pl) al depth))
          (t (fullsinglelast pl al depth cont)))))

(defun fullsingleexecute (p pl al depth cont)
  (let (values ol)
    (setq p (plug p al))
    (cond ((null (cddr p))
           (cond ((ignore-errors (eval (cadr p)))
                  (fullsinglelast pl al depth cont))
                 (t (fullfail (car pl) al depth))))
          ((and (car (setq values (ignore-errors (multiple-value-list (eval (cadr p))))))
                (setq ol (matchify `(listof . ,(cddr p)) al `(listof . ,(mapcar #'quotify values)) al)))
           (prog1 (fullsinglelast pl al depth cont) (backup ol)))
          (t (fullfail (car pl) al depth)))))

(defun fullsinglenotexecute (p pl al depth cont)
  (let (values ol)
    (setq p (plug p al))
    (cond ((null (cddr p))
           (cond ((ignore-errors (not (eval (cadr p))))
                  (fullsinglelast pl al depth cont))
                 (t (fullfail (car pl) al depth))))
          ((and (car (setq values (ignore-errors (multiple-value-list (eval (cadr p))))))
                (not (setq ol (matchify `(listof . ,(cddr p)) al `(listof . ,(mapcar #'quotify values)) al))))
           (fullsinglelast pl al depth cont))
          (t (backup ol) (fullfail (car pl) al depth)))))          

(defun fullsingleevaluate (p pl al depth cont)
  (let (values ol)
    (setq p (plug p al))
    (cond ((null (cddr p))
           (cond ((evals (cadr p)) (fullsinglelast pl al depth cont))
                 (t (fullfail (car pl) al depth))))
          ((and (car (setq values (evals (cadr p))))
                (setq ol (matchify `(listof . ,(cddr p)) al `(listof . ,values) al)))
           (prog1 (fullsinglelast pl al depth cont) (backup ol)))
          (t (fullfail (car pl) al depth)))))

(defun fullsinglenotevaluate (p pl al depth cont)
  (let (values ol)
    (setq p (plug p al))
    (cond ((null (cddr p))
           (cond ((ignore-errors (not (apply (caadr p) (cdadr p))))
                  (fullsinglelast pl al depth cont))
                 (t (fullfail (car pl) al depth))))
          ((and (car (setq values (ignore-errors (multiple-value-list (apply (caadr p) (cdadr p))))))
                (not (setq ol (matchify `(listof . ,(cddr p)) al `(listof . ,values) al))))
           (fullsinglelast pl al depth cont))
          (t (backup ol) (fullfail (car pl) al depth)))))

(defun fullsinglechoose (p pl al depth cont)
  (let (x ol)
    (setq p (plugstdexp p al))
    (setq x (truex (cadr p) (caddr p) *theory*))
    (cond ((and (not (null x)) (setq ol (unify (cadr p) alist x alist)))
           (prog1 (fullsinglelast pl al depth cont) (backup ol)))
          (t (fullfail (car pl) al depth)))))

(defun fullsinglenotchoose (p pl al depth cont)
  (setq p (plugstdexp p al))
  (cond ((truep (caddr p) *theory*) (fullfail (car pl) al depth))
        (t (fullsinglelast pl al depth cont))))

(defun fullsingleunprovable (p pl al depth cont)
  (cond ((fullsingleexp (cadr p) (cdr p) al depth nil) (fullfail (car pl) al depth))
        (t (fullsinglelast pl al depth cont))))

(defun fullsinglebagofall (p pl al depth cont)
  (fullsinglevalue `(value ,(butlast p) ,(cadddr p)) pl al depth cont))

(defun fullsinglenotbagofall (p pl al depth cont)
  (fullsinglenotvalue `(value ,(butlast p) ,(cadddr p)) pl al depth cont))

(defun fullsinglestrmatch (p pl al depth cont)
  (fullsingleexp `(execute (strmatches ,(cadr p) ,(caddr p)) ? . ,(cdddr p)) pl al depth cont))

(defun fullsinglenotstrmatch (p pl al depth cont)
  (fullsinglenot `(execute (strmatches ,(cadr p) ,(caddr p)) ? . ,(cdddr p)) pl al depth cont))

(defun fullsinglebasicval (p pl al depth cont)
  (let (x y ol)
    (setq p (plug p al) x (butlast p) y (car (last p)))
    (when (and (groundp x) (every #'primitivep (cdr x)))
      (setq x (funcall (get (car x) 'basicval) x)))
    (cond ((setq ol (unify x al y al))
           (prog1 (fullsinglelast pl al depth cont) (backup ol)))
          (t (fullfail (car pl) al depth)))))

(defun fullsinglenotbasicval (p pl al depth cont)
  (let (x y ol)
    (setq p (plug p al) x (butlast p) y (car (last p)))
    (when (and (groundp x) (every #'primitivep (cdr x)))
      (setq x (funcall (get (car x) 'basicval) x)))
    (cond ((setq ol (unify x al y al)) (backup ol) (fullfail (car pl) al depth))
          (t (fullsinglelast pl al depth cont)))))

(defun fullsinglebasic (p pl al depth cont)
  (setq p (plug p al))
  (cond ((and (groundp p) (apply (get (car p) 'basic) (cdr p)))
         (fullsinglelast pl al depth cont))
        (t (fullfail (car pl) al depth))))

(defun fullsinglenotbasic (p pl al depth cont)
  (setq p (plug p al))
  (cond ((and (groundp p) (not (apply (get (car p) 'basic) (cdr p))))
         (fullsinglelast pl al depth cont))
        (t (fullfail (car pl) al depth))))

(defun fullsinglers (p pl al depth cont)
  (cond ((fullsingledb p pl al depth cont *theory*))
        (t (fullfail (car pl) al depth))))

(defun fullsingledb (p pl al depth cont th)
  (cond ((fullsingleth p pl al depth cont th))
        (t (do ((l (includees th) (cdr l)))
               ((null l) nil)
               (when (fullsingledb p pl al depth cont (car l)) (return t))))))

(defun fullsingleth (p pl al depth cont th)
  (do ((l (envindexps p al th) (cdr l)) (bl (environment)) (ol))
      ((null l))
      (when (setq ol (unify (car l) bl p al))
        (cond ((fullsingleexit pl al depth cont) (backup ol) (return t))
              ((subolp ol (alist bl)) (backup ol) (return nil))
              (t (backup ol))))))

(defun fullsingleexit (pl al depth cont)
  (let (ans)
    (fullexit (car pl) al depth)
    (cond ((cdr pl) (setq ans (fullsingleexp (cadr pl) (cdr pl) al depth cont)))
          (cont (setq ans (fullsingleexit (caar cont) (cadar cont) (caddar cont) (cdr cont))))
          (t (setq *answer* (plugstdexp *thing* alist) ans t)))
    (if ans t (fullredo (car pl) al depth))))

(defun fullsinglelast (pl al depth cont)
  (fullexit (car pl) al depth)
  (cond ((cdr pl) (fullsingleexp (cadr pl) (cdr pl) al depth cont))
        (cont (fullsingleexit (caar cont) (cadar cont) (caddar cont) (cdr cont)))
        (t (setq *answer* (plugstdexp *thing* alist)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; fulltrues
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun fulltrues (*thing* p *theory*)
  (let (alist *answers*)
    (setq alist (environment))
    (fullpluralexp p (list p) alist 0 nil)
    (nreverse (uniquify *answers*))))

(defun fullpluralexp (p pl al depth cont)
  (fullcall p al depth)
  (cond ((atom p) (fullpluralconstant p pl al depth cont))
        ((eq 'not (car p)) (fullpluralnot (cadr p) pl al depth cont))
        ((eq 'and (car p)) (fullpluraland p pl al depth cont))
        ((eq 'or (car p)) (fullpluralor p pl al depth cont))
        ((eq 'oneof (car p)) (fullpluraloneof p pl al depth cont))
        ((eq 'member (car p)) (fullpluralmember p pl al depth cont))
        ((eq 'same (car p)) (fullpluralsame p pl al depth cont))
        ((eq 'distinct (car p)) (fullpluraldistinct p pl al depth cont))
	((eq 'ground (car p)) (fullpluralground p pl al depth cont))
	((eq 'nonground (car p)) (fullpluralnonground p pl al depth cont))
	((eq 'primitive (car p)) (fullpluralprimitive p pl al depth cont))
	((eq 'nonprimitive (car p)) (fullpluralnonprimitive p pl al depth cont))
	((eq '== (car p)) (fullpluralvalue p pl al depth cont))
	((eq 'value (car p)) (fullpluralvalue p pl al depth cont))
        ((eq 'execute (car p)) (fullpluralexecute p pl al depth cont))
        ((eq 'evaluate (car p)) (fullpluralevaluate p pl al depth cont))
        ((eq 'unprovable (car p)) (fullpluralunprovable p pl al depth cont))
        ((eq 'choose (car p)) (fullpluralchoose p pl al depth cont))
        ((eq 'bagofall (car p)) (fullpluralbagofall p pl al depth cont))
        ((eq 'strmatch (car p)) (fullpluralstrmatch p pl al depth cont))
	((get (car p) 'basicval) (fullpluralbasicval p pl al depth cont))
        ((get (car p) 'basic) (fullpluralbasic p pl al depth cont))
        (t (fullpluralrs p pl al depth cont))))

(defun fullpluralnot (p pl al depth cont)
  (cond ((atom p) (fullpluralnotconstant p pl al depth cont))
        ((eq 'not (car p)) (fullpluralexp (cadr p) pl al depth cont))
        ((eq 'and (car p)) (fullpluralnotand p pl al depth cont))
        ((eq 'or (car p)) (fullpluralnotor p pl al depth cont))
        ((eq 'oneof (car p)) nil)
        ((eq 'same (car p)) nil)
        ((eq 'distinct (car p)) nil)
	((eq 'ground (car p)) nil)
	((eq 'nonground (car p)) nil)
	((eq 'primitive (car p)) nil)
	((eq 'nonprimitive (car p)) nil)
	((eq '== (car p)) (fullpluralnotvalue p pl al depth cont))
	((eq 'value (car p)) (fullpluralnotvalue p pl al depth cont))
        ((eq 'execute (car p)) (fullpluralnotexecute p pl al depth cont))
        ((eq 'evaluate (car p)) (fullpluralnotevaluate p pl al depth cont))
        ((eq 'unprovable (car p)) (fullpluralexp (cadr p) pl al depth cont))
        ((eq 'choose (car p)) (fullpluralnotchoose p pl al depth cont))
        ((eq 'bagofall (car p)) (fullpluralnotbagofall p pl al depth cont))
        ((eq 'strmatch (car p)) (fullpluralnotstrmatch p pl al depth cont))
	((get (car p) 'basicval) (fullpluralnotbasicval p pl al depth cont))
        ((get (car p) 'basic) (fullpluralnotbasic p pl al depth cont))
        (t (fullpluralunprovable `(not ,p) pl al depth cont))))

(defun fullpluralconstant (p pl al depth cont)
  (cond ((eq 'true p) (fullplurallast pl al depth cont))
        ((eq 'false p) (fullfail (car pl) al depth))
        (t (fullpluralrs p pl al depth cont))))

(defun fullpluralnotconstant (p pl al depth cont)
  (cond ((eq 'true p) (fullfail (car pl) al depth))
        ((eq 'false p) (fullplurallast pl al depth cont))
        (t (fullpluralunprovable `(not ,p) pl al depth cont))))

(defun fullpluraland (p pl al depth cont)
  (cond ((null (cdr p)) (fullplurallast pl al depth cont))
        ((fullpluralexp (cadr p) (cdr p) al depth (cons (list pl al depth) cont)))
        (t (fullfail (car pl) al depth))))

(defun fullpluralnotand (p pl al depth cont)
  (setq cont (cons (list pl al depth) cont))
  (do ((l (cdr p) (cdr l)))
      ((null l) (fullfail (car pl) al depth))
      (fullpluralexp (maknot (car l)) (list (maknot (car l))) al depth cont)))

(defun fullpluralor (p pl al depth cont)
  (setq cont (cons (list pl al depth) cont))
  (do ((l (cdr p) (cdr l)))
      ((null l) (fullfail (car pl) al depth))
      (fullpluralexp (car l) (list (car l)) al depth cont)))

(defun fullpluralnotor (p pl al depth cont)
  (cond ((null (cdr p)) (fullplurallast pl al depth cont))
        ((fullpluralexp (maknot (cadr p)) (mapcar #'maknot (cdr p)) al depth
                  (cons (list pl al depth) cont)))
        (t (fullfail (car pl) al depth))))

(defun fullpluraloneof (p pl al depth cont)
  (when (seqvarp (caddr p)) (setq p (plug p al)))
  (do ((l (cddr p) (cdr l)) (ol))
      ((null l) (fullfail (car pl) al depth))
      (when (setq ol (unify (cadr p) al (car l) al))
        (fullpluralexit pl al depth cont)
        (backup ol))))

(defun fullpluralmember (p pl al depth cont)
  (when (seqvarp (caddr p)) (setq p (plug p al)))
  (do ((l (cdaddr p) (cdr l)) (ol))
      ((null l) (fullfail (car pl) al depth))
      (when (setq ol (unify (cadr p) al (car l) al))
        (fullpluralexit pl al depth cont)
        (backup ol))))

(defun fullpluralsame (p pl al depth cont)
  (let (ol)
    (when (setq ol (unify (cadr p) al (caddr p) al))
      (fullpluralexit pl al depth cont)
      (backup ol))))

(defun fullpluraldistinct (p pl al depth cont)
  (if (unify (cadr p) al (caddr p) al) nil (fullpluralexit pl al depth cont)))

(defun fullpluralground (p pl al depth cont)
  (setq p (plug p al))
  (if (groundp p) (fullpluralexit pl al depth cont)))

(defun fullpluralnonground (p pl al depth cont)
  (setq p (plug p al))
  (if (groundp p) nil (fullpluralexit pl al depth cont)))

(defun fullpluralprimitive (p pl al depth cont)
  (setq p (plug p al))
  (if (primitivep p) (fullpluralexit pl al depth cont)))

(defun fullpluralnonprimitive (p pl al depth cont)
  (setq p (plug p al))
  (if (primitivep p) nil (fullpluralexit pl al depth cont)))

(defun fullpluralvalue (p pl al depth cont)
  (let (x y ol)
    (setq p (plug p al) x (cadr p) y (caddr p))
    (cond ((atom x))
          ((eq 'bagofall (car x))
           (setq x (cons 'listof (fulltrues (cadr x) (caddr x) *theory*))))
          ((and (groundp x) (every #'primitivep (cdr x)))
           (setq x (funcall (get (car x) 'basicval) x))))
    (cond ((setq ol (unify x al y al))
           (prog1 (fullpluralexit pl al depth cont) (backup ol)))
          (t (fullfail (car pl) al depth)))))

(defun fullpluralnotvalue (p pl al depth cont)
  (let (x y ol)
    (setq p (plug p al) x (cadr p) y (caddr p))
    (cond ((atom x))
          ((eq 'bagofall (car x))
           (setq x (cons 'listof (fulltrues (cadr x) (caddr x) *theory*))))
          ((and (groundp x) (every #'primitivep (cdr x)))
           (setq x (funcall (get (car x) 'basicval) x))))
    (cond ((setq ol (unify x al y al)) (backup ol) (fullfail (car pl) al depth))
          (t (fullpluralexit pl al depth cont)))))

(defun fullpluralexecute (p pl al depth cont)
  (let (values ol)
    (setq p (plug p al))
    (cond ((null (cddr p))
           (cond ((ignore-errors (eval (cadr p))) (fullpluralexit pl al depth cont))
                 (t (fullfail (car pl) al depth))))
          ((and (car (setq values (ignore-errors (multiple-value-list (eval (cadr p))))))
                (setq ol (matchify `(listof . ,(cddr p)) al `(listof . ,(mapcar #'quotify values)) al)))
           (prog1 (fullpluralexit pl al depth cont) (backup ol)))
          (t (fullfail (car pl) al depth)))))

(defun fullpluralnotexecute (p pl al depth cont)
  (let (values ol)
    (setq p (plug p al))
    (cond ((null (cddr p))
           (cond ((ignore-errors (not (eval (cadr p)))) (fullpluralexit pl al depth cont))
                 (t (fullfail (car pl) al depth))))
          ((and (car (setq values (ignore-errors (multiple-value-list (eval (cadr p))))))
                (not (setq ol (matchify `(listof . ,(cddr p)) al `(listof . ,(mapcar #'quotify values)) al)))
           (fullpluralexit pl al depth cont)))
          (t (backup ol) (fullfail (car pl) al depth)))))          

(defun fullpluralevaluate (p pl al depth cont)
  (let (values ol)
    (setq p (plug p al))
    (cond ((null (cddr p))
           (cond ((ignore-errors (apply (caadr p) (cdadr p))) (fullpluralexit pl al depth cont))
                 (t (fullfail (car pl) al depth))))
          ((and (car (setq values (ignore-errors (multiple-value-list (apply (caadr p) (cdadr p))))))
                (setq ol (matchify `(listof . ,(cddr p)) al `(listof . ,values) al)))
           (prog1 (fullpluralexit pl al depth cont) (backup ol)))
          (t (fullfail (car pl) al depth)))))

(defun fullpluralnotevaluate (p pl al depth cont)
  (let (values ol)
    (setq p (plug p al))
    (cond ((null (cddr p))
           (cond ((ignore-errors (not (apply (caadr p) (cdadr p)))) (fullpluralexit pl al depth cont))
                 (t (fullfail (car pl) al depth))))
          ((and (car (setq values (ignore-errors (multiple-value-list (apply (caadr p) (cdadr p))))))
                (not (setq ol (matchify `(listof . ,(cddr p)) al `(listof . ,values) al))))
           (fullpluralexit pl al depth cont))
          (t (backup ol) (fullfail (car pl) al depth)))))

(defun fullpluralunprovable (p pl al depth cont)
  (cond ((fullsingleexp (cadr p) (cdr p) al depth nil) (fullfail (car pl) al depth))
        (t (fullplurallast pl al depth cont))))

(defun fullpluralchoose (p pl al depth cont)
  (let (x ol)
    (setq p (plugstdexp p al))
    (setq x (truex (cadr p) (caddr p) *theory*))
    (cond ((and (not (null x)) (setq ol (unify (cadr p) alist x alist)))
           (prog1 (fullpluralexit pl al depth cont) (backup ol)))
          (t (fullfail (car pl) al depth)))))

(defun fullpluralnotchoose (p pl al depth cont)
  (setq p (plugstdexp p al))
  (cond ((truep (caddr p) *theory*) (fullfail (car pl) al depth))
        (t (fullpluralexit pl al depth cont))))

(defun fullpluralbagofall (p pl al depth cont)
  (fullpluralvalue `(value ,(butlast p) ,(cadddr p)) pl al depth cont))

(defun fullpluralnotbagofall (p pl al depth cont)
  (fullpluralnotvalue `(value ,(butlast p) ,(cadddr p)) pl al depth cont))

(defun fullpluralstrmatch (p pl al depth cont)
  (fullpluralexp `(execute (strmatches ,(cadr p) ,(caddr p)) ? . ,(cdddr p)) pl al depth cont))

(defun fullpluralnotstrmatch (p pl al depth cont)
  (fullpluralnot `(execute (strmatches ,(cadr p) ,(caddr p)) ? . ,(cdddr p)) pl al depth cont))

(defun fullpluralbasicval (p pl al depth cont)
  (let (x y ol)
    (setq p (plug p al) x (butlast p) y (car (last p)))
    (when (and (groundp x) (every #'primitivep (cdr x)))
      (setq x (funcall (get (car x) 'basicval) x)))
    (cond ((setq ol (unify x al y al))
           (prog1 (fullpluralexit pl al depth cont) (backup ol)))
          (t (fullfail (car pl) al depth)))))

(defun fullpluralnotbasicval (p pl al depth cont)
  (let (x y ol)
    (setq p (plug p al) x (butlast p) y (car (last p)))
    (when (and (groundp x) (every #'primitivep (cdr x)))
      (setq x (funcall (get (car x) 'basicval) x)))
    (cond ((setq ol (unify x al y al)) (backup ol) (fullfail (car pl) al depth))
          (t (fullpluralexit pl al depth cont)))))

(defun fullpluralbasic (p pl al depth cont)
  (setq p (plug p al))
  (cond ((and (groundp p) (apply (get (car p) 'basic) (cdr p))
              (fullpluralexit pl al depth cont)))
        (t (fullfail (car pl) al depth))))

(defun fullpluralnotbasic (p pl al depth cont)
  (setq p (plug p al))
  (cond ((and (groundp p) (not (apply (get (car p) 'basic) (cdr p)))
              (fullpluralexit pl al depth cont)))
        (t (fullfail (car pl) al depth))))

(defun fullpluralrs (p pl al depth cont)
  (cond ((fullpluraldb p pl al depth cont *theory*))
        (t (fullfail (car pl) al depth))))

(defun fullpluraldb (p pl al depth cont th)
  (fullpluralth p pl al depth cont th)
  (do ((l (includees th) (cdr l)))
      ((null l) nil)
      (fullpluraldb p pl al depth cont (car l))))

(defun fullpluralth (p pl al depth cont th)
  (do ((l (newindexps p al th) (cdr l)) (bl (environment)) (ol))
      ((null l) (fullfail (car pl) al depth))
      (when (setq ol (unify (car l) bl p al))
        (fullpluralexit pl al depth cont)
        (cond ((subolp ol (alist bl)) (backup ol) (return nil))
              (t (backup ol))))))

(defun fullpluralexit (pl al depth cont)
  (fullexit (car pl) al depth)
  (cond ((cdr pl) (fullpluralexp (cadr pl) (cdr pl) al depth cont))
        (cont (fullpluralexit (caar cont) (cadar cont) (caddar cont) (cdr cont)))
        (t (setq *answers* (cons (plugstdexp *thing* alist) *answers*))))
  (fullredo (car pl) al depth))

(defun fullplurallast (pl al depth cont)
  (fullexit (car pl) al depth)
  (cond ((cdr pl) (fullpluralexp (cadr pl) (cdr pl) al depth cont))
        (cont (fullpluralexit (caar cont) (cadar cont) (caddar cont) (cdr cont)))
        (t (setq *answers* (cons (plugstdexp *thing* alist) *answers*)) nil)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; fullsupports
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod fullsupports (p *theory* &optional (*filter* #'failure))
  (let (alist *residue* *answers*)
    (setq *unifications* 0)
    (setq *inferences* 0)
    (setq *termination* nil)
    (setq alist (environment))
    (fullsupportsdepth p (list p) alist 0 nil)
    (nreverse (uniquify (mapcar #'maksand *answers*)))))

(defun fullsupportsdepth (p pl al depth cont)
  (setq *inferences* (1+ *inferences*))
  (fullcall p al depth)
  (cond ((>= *inferences* *limit*) (setq *termination* t) (fullstop (car pl) al depth))
        ((>= depth *depth*) (setq *termination* t) (fullstop (car pl) al depth))
        (t (fullsupportsexp p pl al depth cont))))

(defun fullsupportsexp (p pl al depth cont)
  (cond ((atom p) (fullsupportsconstant p pl al depth cont))
        ((eq 'not (car p)) (fullsupportsnot (cadr p) pl al depth cont))
        ((eq 'and (car p)) (fullsupportsand p pl al depth cont))
        ((eq 'or (car p)) (fullsupportsor p pl al depth cont))
        ((eq 'oneof (car p)) (fullsupportsoneof p pl al depth cont))
        ((eq 'member (car p)) (fullsupportsmember p pl al depth cont))
        ((eq 'same (car p)) (fullsupportssame p pl al depth cont))
        ((eq 'distinct (car p)) (fullsupportsdistinct p pl al depth cont))
	((eq 'ground (car p)) (fullsupportsground p pl al depth cont))
	((eq 'nonground (car p)) (fullsupportsnonground p pl al depth cont))
	((eq 'primitive (car p)) (fullsupportsprimitive p pl al depth cont))
	((eq 'nonprimitive (car p)) (fullsupportsnonprimitive p pl al depth cont))
	((eq '== (car p)) (fullsupportsvalue p pl al depth cont))
	((eq 'value (car p)) (fullsupportsvalue p pl al depth cont))
        ((eq 'execute (car p)) (fullsupportsexecute p pl al depth cont))
        ((eq 'evaluate (car p)) (fullsupportsevaluate p pl al depth cont))
        ((eq 'knows (car p)) (fullsupportsknows p pl al depth cont))
        ((eq 'unprovable (car p)) (fullsupportsunprovable p pl al depth cont))
        ((eq 'choose (car p)) (fullsupportschoose p pl al depth cont))
        ((eq 'bagofall (car p)) (fullsupportsbagofall p pl al depth cont))
        ((eq 'strmatch (car p)) (fullsupportsstrmatch p pl al depth cont))
	((get (car p) 'basicval) (fullsupportsbasicval p pl al depth cont))
        ((get (car p) 'basic) (fullsupportsbasic p pl al depth cont))
        ((funcall *filter* (car p)) (fullsupportsknowndb p pl al depth cont *theory*))
        (t (fullsupportsrs p pl al depth cont))))

(defun fullsupportsnot (p pl al depth cont)
  (cond ((atom p) (fullsupportsnotconstant p pl al depth cont))
        ((eq 'not (car p)) (fullsupportsexp (cadr p) pl al depth cont))
        ((eq 'and (car p)) (fullsupportsnotand p pl al depth cont))
        ((eq 'or (car p)) (fullsupportsnotor p pl al depth cont))
        ((eq 'oneof (car p)) nil)
        ((eq 'same (car p)) nil)
        ((eq 'distinct (car p)) nil)
	((eq 'ground (car p)) nil)
	((eq 'nonground (car p)) nil)
	((eq 'primitive (car p)) nil)
	((eq 'nonprimitive (car p)) nil)
	((eq '== (car p)) (fullsupportsnotvalue p pl al depth cont))
	((eq 'value (car p)) (fullsupportsnotvalue p pl al depth cont))
        ((eq 'execute (car p)) (fullsupportsnotexecute p pl al depth cont))
        ((eq 'evaluate (car p)) (fullsupportsnotevaluate p pl al depth cont))
        ((eq 'knows (car p)) (fullsupportsnotknows p pl al depth cont))
        ((eq 'unprovable (car p)) (fullsupportsexp (cadr p) pl al depth cont))
        ((eq 'choose (car p)) (fullsupportsnotchoose p pl al depth cont))
        ((eq 'bagofall (car p)) (fullsupportsnotbagofall p pl al depth cont))
        ((eq 'strmatch (car p)) (fullsupportsnotstrmatch p pl al depth cont))
	((get (car p) 'basicval) (fullsupportsnotbasicval p pl al depth cont))
        ((get (car p) 'basic) (fullsupportsnotbasic p pl al depth cont))
        (t (fullsupportsrs `(not ,p) pl al depth cont))))

(defun fullsupportsconstant (p pl al depth cont)
  (cond ((eq 'true p) (fullsupportslast pl al depth cont))
        ((eq 'false p) (fullfail (car pl) al depth))
        (t (fullsupportsrs p pl al depth cont))))

(defun fullsupportsnotconstant (p pl al depth cont)
  (cond ((eq 'true p) (fullfail (car pl) al depth))
        ((eq 'false p) (fullsupportslast pl al depth cont))
        (t (fullsupportsrs `(not ,p) pl al depth cont))))

(defun fullsupportsand (p pl al depth cont)
  (cond ((null (cdr p)) (fullsupportslast pl al depth cont))
        ((fullsupportsdepth (cadr p) (cdr p) al depth (cons (list pl al depth) cont)))
        (t (fullfail (car pl) al depth))))

(defun fullsupportsnotand (p pl al depth cont)
  (setq cont (cons (list pl al depth) cont))
  (do ((l (cdr p) (cdr l)))
      ((null l) (fullfail (car pl) al depth))
      (fullsupportsdepth (maknot (car l)) (list (maknot (car l))) al depth cont)))

(defun fullsupportsor (p pl al depth cont)
  (setq cont (cons (list pl al depth) cont))
  (do ((l (cdr p) (cdr l)))
      ((null l) (fullfail (car pl) al depth))
      (fullsupportsdepth (car l) (list (car l)) al depth cont)))

(defun fullsupportsnotor (p pl al depth cont)
  (cond ((null (cdr p)) (fullsupportslast pl al depth cont))
        ((fullsupportsdepth (maknot (cadr p)) (mapcar #'maknot (cdr p)) al depth
                  (cons (list pl al depth) cont)))
        (t (fullfail (car pl) al depth))))

(defun fullsupportsoneof (p pl al depth cont)
  (when (seqvarp (caddr p)) (setq p (plug p al)))
  (do ((l (cddr p) (cdr l)) (ol))
      ((null l) (fullfail (car pl) al depth))
      (when (setq ol (unify (cadr p) al (car l) al))
        (fullsupportsexit pl al depth cont)
        (backup ol))))

(defun fullsupportsmember (p pl al depth cont)
  (when (seqvarp (caddr p)) (setq p (plug p al)))
  (do ((l (cdaddr p) (cdr l)) (ol))
      ((null l) (fullfail (car pl) al depth))
      (when (setq ol (unify (cadr p) al (car l) al))
        (fullsupportsexit pl al depth cont)
        (backup ol))))

(defun fullsupportssame (p pl al depth cont)
  (let (ol)
    (when (setq ol (unify (cadr p) al (caddr p) al))
      (fullsupportsexit pl al depth cont)
      (backup ol))))

(defun fullsupportsdistinct (p pl al depth cont)
  (if (unify (cadr p) al (caddr p) al) nil (fullsupportsexit pl al depth cont)))

(defun fullsupportsground (p pl al depth cont)
  (setq p (plug p al))
  (if (groundp p) (fullsupportsexit pl al depth cont)))

(defun fullsupportsnonground (p pl al depth cont)
  (setq p (plug p al))
  (if (groundp p) nil (fullsupportsexit pl al depth cont)))

(defun fullsupportsprimitive (p pl al depth cont)
  (setq p (plug p al))
  (if (primitivep p) (fullsupportsexit pl al depth cont)))

(defun fullsupportsnonprimitive (p pl al depth cont)
  (setq p (plug p al))
  (if (primitivep p) nil (fullsupportsexit pl al depth cont)))

(defun fullsupportsvalue (p pl al depth cont)
  (let (x y ol)
    (setq p (plug p al) x (cadr p) y (caddr p))
    (cond ((atom x))
          ((eq 'bagofall (car x))
           (setq x (cons 'listof (fullfinds (cadr x) (caddr x) *theory*))))
          ((and (groundp x) (every #'primitivep (cdr x)))
           (setq x (funcall (get (car x) 'basicval) x))))
    (cond ((setq ol (unify x al y al))
           (prog1 (fullsupportsexit pl al depth cont) (backup ol)))
          (t (fullfail (car pl) al depth)))))

(defun fullsupportsnotvalue (p pl al depth cont)
  (let (x y ol)
    (setq p (plug p al) x (cadr p) y (caddr p))
    (cond ((atom x))
          ((eq 'bagofall (car x))
           (setq x (cons 'listof (fullfinds (cadr x) (caddr x) *theory*))))
          ((and (groundp x) (every #'primitivep (cdr x)))
           (setq x (funcall (get (car x) 'basicval) x))))
    (cond ((setq ol (unify x al y al)) (backup ol) (fullfail (car pl) al depth))
          (t (fullsupportsexit pl al depth cont)))))

(defun fullsupportsexecute (p pl al depth cont)
  (let (values ol)
    (setq p (plug p al))
    (cond ((null (cddr p))
           (cond ((ignore-errors (eval (cadr p))) (fullsupportsexit pl al depth cont))
                 (t (fullfail (car pl) al depth))))
          ((and (car (setq values (ignore-errors (multiple-value-list (eval (cadr p))))))
                (setq ol (matchify `(listof . ,(cddr p)) al `(listof . ,(mapcar #'quotify values)) al)))
           (prog1 (fullsupportsexit pl al depth cont) (backup ol)))
          (t (fullfail (car pl) al depth)))))

(defun fullsupportsnotexecute (p pl al depth cont)
  (let (values ol)
    (setq p (plug p al))
    (cond ((null (cddr p))
           (cond ((ignore-errors (not (eval (cadr p)))) (fullsupportsexit pl al depth cont))
                 (t (fullfail (car pl) al depth))))
          ((and (car (setq values (ignore-errors (multiple-value-list (eval (cadr p))))))
                (not (setq ol (matchify `(listof . ,(cddr p)) al `(listof . ,(mapcar #'quotify values)) al)))
           (fullsupportsexit pl al depth cont)))
          (t (backup ol) (fullfail (car pl) al depth)))))          

(defun fullsupportsevaluate (p pl al depth cont)
  (let (values ol)
    (setq p (plug p al))
    (cond ((null (cddr p))
           (cond ((ignore-errors (apply (caadr p) (cdadr p))) (fullsupportsexit pl al depth cont))
                 (t (fullfail (car pl) al depth))))
          ((and (car (setq values (ignore-errors (multiple-value-list (apply (caadr p) (cdadr p))))))
                (setq ol (matchify `(listof . ,(cddr p)) al `(listof . ,values) al)))
           (prog1 (fullsupportsexit pl al depth cont) (backup ol)))
          (t (fullfail (car pl) al depth)))))

(defun fullsupportsnotevaluate (p pl al depth cont)
  (let (values ol)
    (setq p (plug p al))
    (cond ((null (cddr p))
           (cond ((ignore-errors (not (apply (caadr p) (cdadr p)))) (fullsupportsexit pl al depth cont))
                 (t (fullfail (car pl) al depth))))
          ((and (car (setq values (ignore-errors (multiple-value-list (apply (caadr p) (cdadr p))))))
                (not (setq ol (matchify `(listof . ,(cddr p)) al `(listof . ,values) al))))
           (fullsupportsexit pl al depth cont))
          (t (backup ol) (fullfail (car pl) al depth)))))

(defun fullsupportsknows (p pl al depth cont)
  (let (agent)
    (fullsupportsknowndb p pl al depth cont *theory*)
    (setq agent (plugstdexp (cadr p) al) p (caddr p))
    (do ((l (newindexps p al agent) (cdr l)) (bl (environment)) (ol))
        ((null l) (fullfail (car pl) al depth))
        (when (setq ol (unify (car l) bl p al))
          (fullsupportslast pl al depth cont)
          (cond ((subolp ol (alist bl)) (backup ol) (return nil))
                (t (backup ol)))))))

(defun fullsupportsnotknows (p pl al depth cont)
  (let (agent)
    (fullsupportsknowndb `(not ,p) pl al depth cont *theory*)
    (setq agent (plugstdexp (cadr p) al) p (caddr p))
    (do ((l (newindexps p al agent) (cdr l)) (bl (environment)) (ol))
        ((null l)  (fullsupportslast pl al depth cont))
        (when (setq ol (unify (car l) bl p al))
          (backup ol)
          (return (fullfail (car pl) al depth))))))

(defun fullsupportsunprovable (p pl al depth cont)
  (cond ((fullone (cadr p) (cdr p) al depth nil) (fullfail (car pl) al depth))
        (t (fullsupportslast pl al depth cont))))

(defun fullsupportschoose (p pl al depth cont)
  (let (x ol)
    (setq p (plugstdexp p al))
    (setq x (findx (cadr p) (caddr p) *theory*))
    (cond ((and (not (null x)) (setq ol (unify (cadr p) alist x alist)))
           (prog1 (fullsupportsexit pl al depth cont) (backup ol)))
          (t (fullfail (car pl) al depth)))))

(defun fullsupportsnotchoose (p pl al depth cont)
  (setq p (plugstdexp p al))
  (cond ((findp (caddr p) *theory*) (fullfail (car pl) al depth))
        (t (fullsupportsexit pl al depth cont))))

(defun fullsupportsbagofall (p pl al depth cont)
  (fullsupportsvalue `(value ,(butlast p) ,(cadddr p)) pl al depth cont))

(defun fullsupportsnotbagofall (p pl al depth cont)
  (fullsupportsnotvalue `(value ,(butlast p) ,(cadddr p)) pl al depth cont))

(defun fullsupportsstrmatch (p pl al depth cont)
  (fullsupportsexp `(execute (strmatches ,(cadr p) ,(caddr p)) ? . ,(cdddr p)) pl al depth cont))

(defun fullsupportsnotstrmatch (p pl al depth cont)
  (fullsupportsnot `(execute (strmatches ,(cadr p) ,(caddr p)) ? . ,(cdddr p)) pl al depth cont))

(defun fullsupportsbasicval (p pl al depth cont)
  (let (x y ol)
    (setq p (plug p al) x (butlast p) y (car (last p)))
    (when (and (groundp x) (every #'primitivep (cdr x)))
      (setq x (funcall (get (car x) 'basicval) x)))
    (cond ((setq ol (unify x al y al))
           (prog1 (fullsupportsexit pl al depth cont) (backup ol)))
          (t (fullfail (car pl) al depth)))))

(defun fullsupportsnotbasicval (p pl al depth cont)
  (let (x y ol)
    (setq p (plug p al) x (butlast p) y (car (last p)))
    (when (and (groundp x) (every #'primitivep (cdr x)))
      (setq x (funcall (get (car x) 'basicval) x)))
    (cond ((setq ol (unify x al y al)) (backup ol) (fullfail (car pl) al depth))
          (t (fullsupportsexit pl al depth cont)))))

(defun fullsupportsbasic (p pl al depth cont)
  (setq p (plug p al))
  (cond ((and (groundp p) (apply (get (car p) 'basic) (cdr p))
              (fullsupportsexit pl al depth cont)))
        (t (fullfail (car pl) al depth))))

(defun fullsupportsnotbasic (p pl al depth cont)
  (setq p (plug p al))
  (cond ((and (groundp p) (not (apply (get (car p) 'basic) (cdr p)))
              (fullsupportsexit pl al depth cont)))
        (t (fullfail (car pl) al depth))))

(defun fullsupportsrs (p pl al depth cont)
  (cond ((and *ancestry* (fullsupportsancestor p al cont)) (fullfail (car pl) al depth))
        ((and (numberp *ancestry*) (fullsupportsnumber p al cont 0))
         (setq *termination* t) (fullfail (car pl) al depth))
        ((and *reduction* (fullsupportsreduce p pl al depth cont)))
        ((fullsupportsdb p pl al depth cont *theory*))
        (t (fullfail (car pl) al depth))))

(defun fullsupportsancestor (p al cont)
  (do ((l cont (cdr l)))
      ((null l) nil)
      (if (identify (caaar l) (cadar l) p al) (return t))))

(defun fullsupportsnumber (p al cont n)
  (let (ol)
    (cond ((numgeqp n *ancestry*))
          ((null cont) nil)
          ((atom p)
           (fullsupportsnumber p al (cdr cont) (if (eq p (caaar cont)) (1+ n) n)))
          ((setq ol (unify p al (caaar cont) (cadar cont)))
           (prog1 (fullsupportsnumber p al (cdr cont) (1+ n)) (backup ol)))
          (t (fullsupportsnumber p al (cdr cont) n)))))

(defun fullsupportsreduce (p pl al depth cont)
  (do ((l cont (cdr l)) (ol))
      ((null l))
      (when (setq ol (unify (maknot (caaar l)) (cadar l) p al))
        (fullsupportsexit pl al depth cont)
        (backup ol))))

(defun fullsupportsdb (p pl al depth cont th)
  (fullsupportsth p pl al depth cont th)
  (do ((l (includees th) (cdr l)))
      ((null l))
      (fullsupportsdb p pl al depth cont (car l))))

(defun fullsupportsth (p pl al depth cont th)
  (do ((l (envindexps p al th) (cdr l)) (bl (environment)) (ol))
      ((null l))
      (cond ((and (listp (car l)) (eq '<= (caar l)) (null (cddar l)))
             (when (setq ol (unify (cadar l) bl p al))
               (fullsupportsexit pl al depth cont)
               (cond ((subolp ol (alist bl)) (backup ol) (return nil))
                   (t (backup ol)))))
            ((and (listp (car l)) (eq '<= (caar l)))
             (when (setq ol (unify (cadar l) bl p al))
               (fullsupportsdepth (caddar l) (cddar l) bl
                        (1+ depth) (cons (list pl al depth) cont))
               (backup ol)))
            ((setq ol (unify (car l) bl p al))
             (cond ((find (car l) *residue* :test #'equalp)
                    (fullsupportslast pl al depth cont))
                   (t (let ((*residue* (cons (car l) *residue*)))
                        (fullsupportslast pl al depth cont))))
             (cond ((subolp ol (alist bl)) (backup ol) (return nil))
                   (t (backup ol)))))))

(defun fullsupportsknowndb (p pl al depth cont th)
  (fullsupportsknownth p pl al depth cont th)
  (do ((l (includees th) (cdr l)))
      ((null l))
      (fullsupportsknowndb p pl al depth cont (car l))))

(defun fullsupportsknownth (p pl al depth cont th)
  (do ((l (newindexps p al th) (cdr l)) (bl (environment)) (ol))
      ((null l) (fullfail (car pl) al depth))
      (when (setq ol (unify (car l) bl p al))
        (fullsupportsexit pl al depth cont)
        (cond ((subolp ol (alist bl)) (backup ol) (return nil))
              (t (backup ol))))))

(defun fullsupportsexit (pl al depth cont)
  (fullexit (car pl) al depth)
  (cond ((cdr pl) (fullsupportsdepth (cadr pl) (cdr pl) al depth cont))
        (cont (fullsupportsexit (caar cont) (cadar cont) (caddar cont) (cdr cont)))
        (t (setq *answers* (cons (reverse *residue*) *answers*))))
  (fullredo (car pl) al depth))

(defun fullsupportslast (pl al depth cont)
  (fullexit (car pl) al depth)
  (cond ((cdr pl) (fullsupportsdepth (cadr pl) (cdr pl) al depth cont))
        (cont (fullsupportsexit (caar cont) (cadar cont) (caddar cont) (cdr cont)))
        (t (setq *answers* (cons (reverse *residue*) *answers*)) nil)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; affirm, retract, discardall
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod affirm (p sender (receiver viewserver))
  (declare (ignore sender))
  (save p receiver))

(defmethod retract (p sender (receiver viewserver))
  (declare (ignore sender))
  (drop p receiver))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; change, mistakes, changes
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod change (p (data viewserver))
  (cond ((viewmistakes p data))
        (t (dolist (p (viewchanges p data))
             (cond ((eq (car p) 'pos) (insert (cadr p) data))
                   ((eq (car p) 'neg) (drop (cadr p) data))))
           'done)))

(defun viewmistakes (p data)
  (let (temp answers)
    (setq temp (make-instance 'theory))
    (insertchanges p temp)
    (includes temp data)
    (setq answers (nextfinds '?x '(error ?x) temp))
    (decludes temp)
    (empty temp)
    answers))

(defun viewchanges (p data)
  (let (temp befores afters answers)
    (setq temp (make-instance 'theory))
    (insertchanges p temp)
    (includes temp data)
    (dolist (r (maintainees data))
      (setq befores (viewfinds (list r '@l) (list r '@l) data))
      (setq afters (nextfinds (list r '@l) (list r '@l) temp))
      (dolist (p (difference* befores afters))
        (setq answers (cons `(neg ,p) answers)))
      (dolist (p (difference* afters befores))
        (setq answers (cons `(pos ,p) answers))))
    (decludes temp)
    (empty temp)
    answers))

(defun insertchanges (p th)
  (cond ((atom p) (save `(pos ,p) th))
        ((eq (car p) 'not) (save `(neg ,(cadr p)) th))
        ((eq (car p) 'and) (mapc #'(lambda (p) (insertchanges p th)) (cdr p)))
        (t (save `(pos ,p) th))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; insert
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod insert (p (receiver viewserver))
  (factinsert p receiver))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; revise, errors, revisions, notifications, reactions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod revise (p (th viewserver))
  (let (revisions notifications reactions)
    (cond ((errors p th))
          (t (setq revisions (revisions p th))
             (setq notifications (notifications p th))
             (setq reactions (reactions p th))
             (dolist (p revisions) (factinsert p th))
             (dolist (dum notifications) (revise (maksand (cdr dum)) (car dum)))
             (dolist (dum reactions) (eval dum))
             'done))))

(defmethod errors (p (th viewserver))
  (let (temp answers)
    (setq temp (make-instance 'theory))
    (insertatoms p temp)
    (includes temp th)
    (setq answers (viewfinds '?x '(error ?x) temp))
    (decludes temp)
    (empty temp)
    answers))

(defmethod revisions (p (th viewserver))
  (let (temp pat answers)
    (setq temp (make-instance 'theory))
    (insertatoms p temp)
    (includes temp th)
    (dolist (r (recordees th))
      (setq pat `(,r @l))
      (setq answers (nconc answers (viewfinds `(not ,pat) `(neg ,pat) temp)))
      (setq answers (nconc answers (viewfinds pat `(pos ,pat) temp))))
    (dolist (r (maintainees th))
      (setq pat `(,r @l))
      (setq answers (nconc answers (viewfinds `(not ,pat) `(neg ,pat) temp)))
      (setq answers (nconc answers (viewfinds pat `(pos ,pat) temp))))
    (decludes temp)
    (empty temp)
    answers))

(defmethod notifications (p (th viewserver))
  (let (temp pat answers subscripts)
    (setq temp (make-instance 'theory))
    (insertatoms p temp)
    (includes temp th)
    (dolist (agent (subscribers '?))
      (dolist (r (subscriptions agent))
        (setq pat `(,r @l))
        (setq answers (nconc answers (viewfinds `(not ,pat) `(neg ,pat) temp)))
        (setq answers (nconc answers (viewfinds pat `(pos ,pat) temp))))
      (when answers (setq subscripts (cons (cons agent answers) subscripts)))
      (setq answers nil))
    (decludes temp)
    (empty temp)
    subscripts))

(defmethod reactions (p (th viewserver))
  (let (temp answers)
    (setq temp (make-instance 'theory))
    (insertatoms p temp)
    (includes temp th)
    (setq answers (viewfinds '?x '(trigger ?x) temp))
    (decludes temp)
    (empty temp)
    answers))

(defun insertatoms (p th)
  (cond ((atom p) (save `(plus ,p) th))
        ((eq (car p) 'and) (mapc #'(lambda (p) (insertatoms p th)) (cdr p)))
        ((eq (car p) 'plus) (save p th))
        ((eq (car p) 'minus) (save p th))
        ((eq (car p) 'pos) (save `(plus ,(cadr p)) th))
        ((eq (car p) 'neg) (save `(minus ,(cadr p)) th))
        ((eq (car p) 'not) (save `(minus ,(cadr p)) th))
        (t (save `(plus ,p) th))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; affirm, retract, discardall
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod affirm (p sender (receiver factserver))
  (declare (ignore sender))
  (save p receiver))

(defmethod retract (p sender (receiver factserver))
  (declare (ignore sender))
  (drop p receiver))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; affirm, retract
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod affirm (p sender (receiver trueserver))
  (declare (ignore sender))
  (save p receiver))

(defmethod retract (p sender (receiver trueserver))
  (declare (ignore sender))
  (drop p receiver))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; change, mistakes, changes
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod change (p (th fullserver))
  (declare (ignore sender))
  (dolist (x (get-updates p)) (fullupdate x th #'failure #'success))
  'done)

(defun get-updates (p)
  (cond ((atom p) (list p))
        ((eq (car p) 'and) (cdr p))
        ((eq (car p) 'update) (cdr p))
        (t (list p))))

;;;;



(defun nonviews (th)
  (do ((l (revisablesdb th nil) (cdr l)) (nl))
      ((null l) nl)
      (unless (fullviewp (car l) th) (setq nl (cons (car l) nl)))))

(defun revisablesdb (th nl)
  (setq nl (revisablesth th nl))
  (dolist (st (includees th)) (setq nl (revisablesdb st nl)))
  nl)

(defun revisablesth (th nl)
  (do ((l (contents th) (cdr l)) (head))
      ((null l) nl)
      (cond ((atom (car l)))
            ((and (eq (caar l) '<=) (listp (setq head (cadar l)))
                   (find (car head) '(pos neg)) (listp (setq head (cadr head))))
             (setq nl (adjoin (car head) nl))))))

;;;;

(defparameter *intensions* nil)

(defun intensionp (r th)
  (cond (*intensions*)
        ((find r (intensions th)))
        (t (some #'(lambda (x) (intensionp r x)) (includees th)))))

(defun extensionp (r th)
  (cond ((find r (extensions th)))
        (t (some #'(lambda (x) (extensionp r x)) (includees th)))))

(defun extensionalp (p al)
  (and (find (car p) (extensions *theory*)) (chkprimitivepcdr (cdr p) al)))

(defun intensionalp (p al)
  (and (find (car p) (intensions *theory*)) (chkprimitivepcdr (cdr p) al)))

;;;;

(defmethod extensions (x)
  (declare (ignore x))
  nil)

(defmethod extensions ((th symbol))
  (get th 'extensions))

(defmethod (setf extensions) (rels (th symbol))
  (setf (get th 'extensions) rels))

(defmethod intensions (x)
  (declare (ignore x))
  nil)

(defmethod intensions ((th symbol))
  (get th 'intensions))

(defmethod (setf intensions) (rels (th symbol))
  (setf (get th 'intensions) rels))

(defgeneric extension (rel th)
 (:documentation
 "(EXTENSION R TH)
  EXTENSION takes a relation and a theory as arguments.  It includes the specified
relation on the list of relations fully stored within the specified theory.  The
upshot of this is that a sentence using this relation will be deemed false unless
it is known to be true."))

(defmethod extension (rel th)
  (setf (extensions th) (adjoin rel (extensions th))))

(defgeneric unextension (rel th)
 (:documentation
 "(UNEXTENSION R TH)
  UNEXTENSION takes a relation and a theory as arguments.  It removes the specified
relation from the list of relations fully stored within the specified theory.  See
EXTENSION."))

(defmethod unextension (rel th)
  (setf (extensions th) (delete rel (extensions th))))

(defgeneric intension (rel th)
 (:documentation
 "(INTENSION R TH)
  INTENSION takes a relation and a theory as arguments.  It includes the specified
relation on the list of relations fully stored within the specified theory.  The
upshot of this is that a sentence using this relation will be deemed false unless
it is known to be true."))

(defmethod intension (rel th)
  (setf (intensions th) (adjoin rel (intensions th))))

(defgeneric unintension (rel th)
 (:documentation
 "(UNINTENSION R TH)
  UNINTENSION takes a relation and a theory as arguments.  It removes the specified
relation from the list of relations fully stored within the specified theory.  See
INTENSION."))

(defmethod unintension (rel th)
  (setf (intensions th) (delete rel (intensions th))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; New working revisions stuff supplanted by even better version
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod revisions (p (th viewserver))
  (let (temp pat answers)
    (setq temp (make-instance 'theory))
    (insertatoms p temp)
    (includes temp th)
    (dolist (r (extensions th))
      (setq pat `(,r @l))
      (setq answers (nconc answers (viewfinds `(not ,pat) `(neg ,pat) temp)))
      (setq answers (nconc answers (viewfinds pat `(pos ,pat) temp))))
    (decludes temp)
    (empty temp)
    answers))

(defmethod revisions (p (th viewserver))
  (declare (ignore th))
  (integrals p))

(defmethod revisions (p (th viewserver))
  (let (temp pat answers)
    (setq temp (make-instance 'theory))
    (insertplusminus p temp)
    (includes temp th)
    (dolist (r (extensions th))
      (setq pat `(,r @l))
      (setq answers (nconc answers (viewfinds `(not ,pat) `(minus ,pat) temp)))
      (setq answers (nconc answers (viewfinds pat `(plus ,pat) temp))))
    (decludes temp)
    (empty temp)
    answers))

(defmethod materializations (p (th viewserver))
  (let (temp pat answers)
    (setq temp (make-instance 'theory))
    (insertatoms p temp)
    (includes temp th)
    (dolist (r (maintainees th))
      (setq pat `(,r @l))
      (setq answers (nconc answers (viewfinds `(not ,pat) `(neg ,pat) temp)))
      (setq answers (nconc answers (viewfinds pat `(pos ,pat) temp))))
    (decludes temp)
    (empty temp)
    answers))

(defmethod notifications (p (th viewserver))
  (let (temp pat answers subscripts)
    (setq temp (make-instance 'theory))
    (insertposneg p temp)
    (includes temp th)
    (dolist (agent (subscribers '?))
      (dolist (r (subscriptions agent))
        (setq pat `(,r @l))
        (setq answers (nconc answers (viewfinds `(not ,pat) `(neg ,pat) temp)))
        (setq answers (nconc answers (viewfinds pat `(pos ,pat) temp))))
      (when answers (setq subscripts (cons (cons agent answers) subscripts)))
      (setq answers nil))
    (decludes temp)
    (empty temp)
    subscripts))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun differentialdependents (p agent *theory*)
   (let (*answers*)
     (differentialdependentsexp p)
     (do ((l *answers* (cdr l)) (nl))
         ((null l) nl)
         (when (findp `(extension ,(name agent) ,(car l)) *manager*)
           (setq nl (cons (car l) nl))))))

(defun differentialdependentsexp (p)
  (cond ((atom p))
        ((eq (car p) 'not)
         (diffdependents (caadr p))
         (setq *answers* (adjoin (caadr p) *answers*)))
        ((eq (car p) 'and) (dolist (p (cdr p)) (differentialdependentsexp p)))
        (t (diffdependents (car p))
           (setq *answers* (adjoin (car p) *answers*)))))

(defun diffdependents (r)
  (cond ((find r *answers*))
        (t (diffdependentsdb r *theory*))))

(defun diffdependentsdb (r th)
  (diffdependentsth r th)
  (dolist (th (includees th)) (diffdependentsdb r th)))

(defun diffdependentsth (r th)
  (do ((l (indexees r th) (cdr l)) (head))
      ((null l))
      (when (and (listp (car l)) (eq (caar l) '<=)
                 (listp (cadar l)) (setq head (cadar l))
                 (find (car head) '(plus minus)) (listp (cadr head)))
        (do ((m (cddar l) (cdr m)))
            ((null m))
            (when (and (listp (car m)) (find (caar m) '(plus minus))
                       (listp (cadar m)) (eq (caadar m) r))
              (setq *answers* (adjoin (caadr head) *answers*))
              (diffdependents (caadr head)))))))

;;;;

(defun dependents (p agent *theory*)
   (let (*answers*)
     (dependentsexp p)
     (do ((l *answers* (cdr l)) (nl))
         ((null l) nl)
         (when (findp `(extension ,(name agent) ,(car l)) *manager*)
           (setq nl (cons (car l) nl))))))

(defun dependentsexp (p)
  (cond ((atom p))
        ((eq (car p) 'not)
         (diffdependents (caadr p))
         (setq *answers* (adjoin (caadr p) *answers*)))
        ((eq (car p) 'and) (dolist (p (cdr p)) (dependentsexp p)))
        ((eq (car p) 'or) (dolist (p (cdr p)) (dependentsexp p)))
        ((get (car p) 'basic))
        ((get (car p) 'basicval))
        (t (dependentsrs p)
           (setq *answers* (adjoin (car p) *answers*)))))

(defun dependentsrs (p)
  (cond ((find p *answers*))
        (t (dependentsdb p *theory*))))

(defun dependentsdb (p th)
  (dependentsth p th)
  (dolist (th (includees th)) (dependentsdb p th)))

(defun dependentsth (p th)
  (do ((l (indexees (car p) th) (cdr l)) (head))
      ((null l))
      (when (and (listp (car l)) (eq (caar l) '<=)
                 (listp (cadar l)) (setq head (cadar l))
                 (find (car head) '(plus minus)) (listp (cadr head)))
        (do ((m (cddar l) (cdr m)))
            ((null m))
            (when (and (listp (car m)) (eq (caar m) (car p)))
              (setq *answers* (adjoin (caadr head) *answers*))
              (dependentsrs (cadr head)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; create, discard
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod discard (p sender (receiver translator))
  (declare (ignore sender))
  (discard p receiver (find-target (name receiver)))
  (discard p receiver (get-rulebase receiver)))

;;;;

(defun fullresidueunprovable (p pl al depth cont)
  (cond ((fullone (cadr p) (cdr p) al depth nil) (fullfail (car pl) al depth))
        (t (fullonelast pl al depth cont))))

(defun fullresiduesunprovable (p pl al depth cont)
  (cond ((fullone (cadr p) pl al depth cont) (fullfail (car pl) al depth))
        (t (fullresidueslast pl al depth cont))))

;;;;

(defmethod revisions (p (receiver facilitator))
  (let (*agent* *library* *agents* ruleserver temp pat pos neg answers)
    (setq *agent* (name receiver))
    (setq *library* (find-rulebase *agent*))
    (setq *agents* (find-agents))
    (setq ruleserver (symbol-value *library*))
    (setq temp (make-instance 'theory))
    (insertposneg p temp)
    (includes temp ruleserver)
    (dolist (r (publications '?))
      (setq pat `(,r @l))
      (setq neg (maksor (fullresidues `(neg ,pat) temp #'specialp)))
      (setq answers (nconc answers (askallquery `(not ,pat) neg)))
      (setq pos (maksor (fullresidues `(pos ,pat) temp #'specialp)))
      (setq answers (nconc answers (askallquery pat pos))))
    (decludes temp)
    (empty temp)
    answers))

;;;;

(defmethod revise (p sender (receiver facilitator))
  (declare (ignore sender))
  (let (revisions notifications materializations reactions factserver)
    (setq revisions (revisions p receiver) p (maksand revisions))
    (cond ((errors p receiver))
          (t (setq materializations (materializations p receiver))
             (setq notifications (notifications p receiver))
             (setq reactions (reactions p receiver))
             (setq factserver (find-target (name receiver)))
             (dolist (p revisions) (change p factserver))
             (dolist (p materializations) (change p factserver))
             (dolist (dum notifications)
               (revise (maksand (cdr dum)) receiver (car dum)))
             (dolist (dum reactions) (eval dum))
             'done))))

(defmethod revisions (p (receiver facilitator))
  (declare (ignore receiver))
  (integrals p))


(defmethod revise (p sender (receiver fullserver))
  (declare (ignore sender))
  (let (revisions notifications materializations reactions)
    (setq revisions (revisions p receiver) p (maksand revisions))
    (cond ((errors p receiver))
          (t (setq materializations (materializations p receiver))
             (setq notifications (notifications p receiver))
             (setq reactions (reactions p receiver))
             (dolist (p revisions) (change p receiver))
             (dolist (p materializations) (change p receiver))
             (dolist (dum notifications)
               (revise (maksand (cdr dum)) receiver (car dum)))
             (dolist (dum reactions) (eval dum))
             'done))))

(defmethod revisions (p (receiver fullserver))
  (declare (ignore receiver))
  (integrals p))

(defmethod notifications (p (receiver fullserver))
  (let (temp pat answers subscripts)
    (setq temp (make-instance 'theory))
    (insertplusminus p temp)
    (includes temp receiver)
    (dolist (*agent* (subscribers '?))
      (dolist (r (dependents p receiver #'subscriptionp))
        (setq pat `(,r @l))
        (setq answers (nconc answers (fullfinds `(not ,pat) `(neg ,pat) temp)))
        (setq answers (nconc answers (fullfinds pat `(pos ,pat) temp))))
      (when answers (setq subscripts (cons (cons *agent* answers) subscripts)))
      (setq answers nil))
    (decludes temp)
    (empty temp)
    subscripts))

;;;;

(defmethod revise (p sender (receiver viewserver))
  (declare (ignore sender))
  (let (revisions notifications materializations reactions)
    (setq revisions (revisions p receiver) p (maksand revisions))
    (cond ((errors p receiver))
          (t (setq materializations (materializations p receiver))
             (setq notifications (notifications p receiver))
             (setq reactions (reactions p receiver))
             (dolist (p revisions) (change p receiver))
             (dolist (p materializations) (change p receiver))
             (dolist (dum notifications)
               (revise (maksand (cdr dum)) receiver (car dum)))
             (dolist (dum reactions) (eval dum))
             'done))))

(defmethod notifications (p (receiver viewserver))
  (let (temp pat answers subscripts)
    (setq temp (make-instance 'theory))
    (insertposneg p temp)
    (includes temp receiver)
    (dolist (*agent* (subscribers '?))
      (dolist (r (dependents p receiver #'subscriptionp))
        (setq pat `(,r @l))
        (setq answers (nconc answers (viewfinds `(not ,pat) `(neg ,pat) temp)))
        (setq answers (nconc answers (viewfinds pat `(pos ,pat) temp))))
      (when answers (setq subscripts (cons (cons *agent* answers) subscripts)))
      (setq answers nil))
    (decludes temp)
    (empty temp)
    subscripts))

;;;;

(defmethod revise (p sender (receiver transformer))
  (declare (ignore sender))
  (let (revisions notifications materializations reactions factserver)
    (setq revisions (revisions p receiver) p (maksand revisions))
    (cond ((errors p receiver))
          (t (setq materializations (materializations p receiver))
             (setq notifications (notifications p receiver))
             (setq reactions (reactions p receiver))
             (setq factserver (find-target (name receiver)))
             (dolist (p revisions) (change p factserver))
             (dolist (p materializations) (change p factserver))
             (dolist (dum notifications)
               (revise (maksand (cdr dum)) receiver (car dum)))
             (dolist (dum reactions) (eval dum))
             'done))))

(defmethod notifications (p (receiver transformer))
  (let (*agent* *library* *target* ruleserver temp pat pos neg answers subscripts)
    (setq *agent* (name receiver))
    (setq *library* (find-rulebase *agent*))
    (setq *target* (find-target *agent*))
    (setq ruleserver (symbol-value *library*))
    (setq temp (make-instance 'theory))
    (insertposneg p temp)
    (includes temp ruleserver)
    (dolist (*agent* (subscribers '?))
      (dolist (r (dependents p ruleserver #'subscriptionp))
        (setq pat `(,r @l))
        (setq neg (maksor (viewresidues `(neg ,pat) temp #'specialtyp)))
        (setq answers (nconc answers (asks `(not ,pat) neg receiver *target*)))
        (setq pos (maksor (viewresidues `(pos ,pat) temp #'specialtyp)))
        (setq answers (nconc answers (asks pat pos receiver *target*))))
      (when answers (setq subscripts (cons (cons *agent* answers) subscripts)))
      (setq answers nil))
    (decludes temp)
    (empty temp)
    subscripts))

;;;;

(defmethod revise (p sender (receiver translator))
  (declare (ignore sender))
  (let (revisions notifications reactions factserver)
    (cond ((errors p receiver))
          (t (setq revisions (revisions p receiver))
             (setq notifications (notifications p receiver))
             (setq reactions (reactions p receiver))
             (setq factserver (find-target (name receiver)))
             (revise (maksand revisions) receiver factserver)
             (dolist (dum notifications)
               (revise (maksand (cdr dum)) receiver (car dum)))
             (dolist (dum reactions) (eval dum))
             'done))))

(defmethod notifications (p (receiver translator))
  (let (*agent* *library* *target* ruleserver temp pat pos neg answers subscripts)
    (setq *agent* (name receiver))
    (setq *library* (find-rulebase *agent*))
    (setq *target* (find-target *agent*))
    (setq ruleserver (symbol-value *library*))
    (setq temp (make-instance 'theory))
    (insertposneg p temp)
    (includes temp ruleserver)
    (dolist (*agent* (subscribers '?))
      (dolist (r (dependents p ruleserver #'subscriptionp))
        (setq pat `(,r @l))
        (setq neg (maksor (viewresidues `(neg ,pat) temp #'nonviewp)))
        (setq answers (nconc answers (asks `(not ,pat) neg receiver *target*)))
        (setq pos (maksor (viewresidues `(pos ,pat) temp #'nonviewp)))
        (setq answers (nconc answers (asks pat pos receiver *target*))))
      (when answers (setq subscripts (cons (cons *agent* answers) subscripts)))
      (setq answers nil))
    (decludes temp)
    (empty temp)
    subscripts))

;;;;

(defmethod revise (p sender (receiver fastserver))
  (declare (ignore sender))
  (let (revisions notifications materializations reactions)
    (setq revisions (revisions p receiver) p (maksand revisions))
    (cond ((errors p receiver))
          (t (setq materializations (materializations p receiver))
             (setq notifications (notifications p receiver))
             (setq reactions (reactions p receiver))
             (dolist (p revisions) (change p receiver))
             (dolist (p materializations) (change p receiver))
             (dolist (dum notifications)
               (revise (maksand (cdr dum)) receiver (car dum)))
             (dolist (dum reactions) (eval dum))
             'done))))

(defmethod notifications (p (receiver fastserver))
  (let (temp pat answers subscripts)
    (setq temp (make-instance 'theory))
    (insertposneg p temp)
    (includes temp receiver)
    (dolist (*agent* (subscribers '?))
      (dolist (r (dependents p receiver #'subscriptionp))
        (setq pat `(,r @l))
        (setq answers (nconc answers (viewfinds `(not ,pat) `(neg ,pat) temp)))
        (setq answers (nconc answers (viewfinds pat `(pos ,pat) temp))))
      (when answers (setq subscripts (cons (cons *agent* answers) subscripts)))
      (setq answers nil))
    (decludes temp)
    (empty temp)
    subscripts))

;;;;

<!--
<table WIDTH=100% cellpadding=0 cellspacing=0 border=0 bgcolor="#006699"><tr><td nowrap ALIGN=LEFT>
<tr valign="center">
<td width= "80" HEIGHT="25" align="center"><a href="/browser/frontdoor?" target="_top"><FONT FACE="arial" SIZE="3px" color="#FFFFFF">Home</FONT></a></td>
<td align="center" bgcolor="#00EBF7"><a href="/browser/frontdoor?" target="_top"><FONT FACE="arial" SIZE="3px" color="#FFFFFF">Donate Supplies</FONT></a></td>
<td align="center"><a href="https://www.habitat.org/giving/donate.aspx?link=1" target="_top"><FONT FACE="arial" SIZE="3px" color="#FFFFFF">Donate Money</FONT></a></td>
<td align="center"><a href="http://www.habitat.org" target="_top"><FONT FACE="arial" SIZE="3px" color="#FFFFFF">Donate Time</FONT></a></td>
<td align="center"><a href="/editor/profile?" target="_top"><FONT FACE="arial" color="#FFFFFF">Personal Profile</FONT></a></td>
<td align="center"><a href="/editor/profile?" target="_top"><FONT FACE="arial" color="#FFFFFF">Habitat Profile</FONT></a></td>
<td width= "*"><img src="/private/habitat/images/blank.gif" width="100" height="12"></td>
</tr>
</table>

<table WIDTH=100% cellpadding=4 cellspacing=0 border=0 bgcolor="#006699"><tr><td nowrap ALIGN=LEFT>
&nbsp;&nbsp;<a href="/browser/frontdoor?" target="_top"><FONT FACE="arial" SIZE="3px" color="#FFFFFF">Home</FONT></a>&nbsp;&nbsp;
&nbsp;&nbsp;<a href="/browser/frontdoor?" target="_top"><FONT FACE="arial" SIZE="3px" color="#FFFFFF">Donate Supplies</FONT></a>&nbsp;&nbsp;
&nbsp;&nbsp;<a href="https://www.habitat.org/giving/donate.aspx?link=1" target="_top"><FONT FACE="arial" SIZE="3px" color="#FFFFFF">Donate Money</FONT></a>&nbsp;&nbsp;
&nbsp;&nbsp;<a href="http://www.habitat.org" target="_top"><FONT FACE="arial" SIZE="3px" color="#FFFFFF">Donate Time</FONT></a>&nbsp;&nbsp;
&nbsp;&nbsp;<a href="/editor/profile?" target="_top"><FONT FACE="arial" color="#FFFFFF">Personal Profile</FONT></a>&nbsp;&nbsp;
&nbsp;&nbsp;<a href="/editor/profile?" target="_top"><FONT FACE="arial" color="#FFFFFF">Habitat Profile</FONT></a>&nbsp;&nbsp;
</TD></tr>
</table>
-->

;;;;

<html>
<head>
<title>Habitat for Humanity Supplies</title>
<script type="text/javascript" language="javascript">
    var http_request = false;

    function makeRequest(url) {
        http_request = false;
        if (window.XMLHttpRequest) { // Mozilla, Safari,...
            http_request = new XMLHttpRequest();
            if (http_request.overrideMimeType) {
                http_request.overrideMimeType('text/xml');}}
        else if (window.ActiveXObject) { // IE
            try {http_request = new ActiveXObject("Msxml2.XMLHTTP");} 
                catch (e) {
                try {http_request = new ActiveXObject("Microsoft.XMLHTTP");}
                    catch (e) {} }}
        if (!http_request)
           {alert('Giving up :( Cannot create an XMLHTTP instance');
            return false;}
        http_request.onreadystatechange = alertContents;
        http_request.open('POST', url, true);
        http_request.send('VALUE=(TOP PRODUCT SERVICE ORGANIZATION)');}

    function alertContents()
       {if (http_request.readyState == 4)
           {if (http_request.responseText)
               {Foo.innerHTML = http_request.responseText;}
            else {alert('There was a problem with the request.');}}}
</script>

</head>

<body BGCOLOR="WHITE" leftmargin="0" topmargin="0" marginwidth="0" marginheight="0">

<table width="100%" cellpadding="0" cellspacing="0" border="0">
<tr>
<td>
<a href="/default.aspx"><img src="/private/habitat/images/logo.gif" height="73"/></a>
</td>
<td>&nbsp;</td>
<td align="center" valign="center">
<table cellspacing="0" cellpadding="0" border="0">
<tr><td>
<a href="/language.aspx"><font color="black">Other Languages</font></a>&nbsp;|&nbsp;
<a href="../../sitemap.aspx"><font color="black">Site Map</font></a>&nbsp;|&nbsp;
<a href="/contact.aspx"><font color="black">Contact Us</font></a>
</td></tr>
<tr><td>
<form action="http://www2.habitat.org:8080/Phantom.acgi$search" method="post">
  <table cellspacing="0" cellpadding="0" border="0">
  <tr>
  <td><input name=".searchText" value="Site Search")" /></td>
  <td><input type="image" src="/private/habitat/images/go.gif" alt="Search" border="0" /></td>
  </tr>
  </table>
  <input type="hidden" name=".andOr" value="all">
  <input type="hidden" name=".Session1" value="1">
  <input type="hidden" name=".Details" value="Details">
</form>
</td></tr>
</table>
</td>
</tr>
</table>

<!--------------------------------------------------------------------------->

<table cellpadding=0 cellspacing=0 border=0 width="100%">
<tr><td bgcolor="#006699" nowrap><img src="/private/habitat/images/blank.gif" alt="" width="1" height="2"></td></tr>
</table>

<table WIDTH=100% cellpadding=0 cellspacing=0 border=0 bgcolor="#006699"><tr><td nowrap ALIGN=LEFT>
<tr valign="center">
<td width= "80" HEIGHT="25" align="center"><a href="/browser/frontdoor?" target="_top"><FONT FACE="arial" SIZE="3px" color="#FFFFFF">Home</FONT></a></td>
<td align="center"><a href="/donor/" target="_top"><FONT FACE="arial" SIZE="3px" color="#FFFFFF">Donate Supplies</FONT></a></td>
<td align="center"><a href="/buyer/" target="_top"><FONT FACE="arial" SIZE="3px" color="#FFFFFF">Buy Supplies</FONT></a></td>
<td align="center"><a href="/repository/profile?" target="_top"><FONT FACE="arial" color="#FFFFFF">Personal Profile</FONT></a></td>
<td align="center"><a href="/repository/profile?" target="_top"><FONT FACE="arial" color="#FFFFFF">Habitat Profile</FONT></a></td>
<td width= "*"><img src="/private/habitat/images/blank.gif" width="100" height="12"></td>
</tr>
</table>

<table cellpadding=0 cellspacing=0 border=0 width="100%">
<tr><td bgcolor="#006699" nowrap><img src="/private/habitat/images/blank.gif" alt="" width=1 height=1></td></tr>
</table>

<table cellpadding=0 cellspacing=0 border=0 width="100%" bgcolor="#FCFFCA">
<tr><td><img src="/private/habitat/images/giving.jpg"></td></tr>
</table>

<!--------------------------------------------------------------------------->

<BR/>

<center>
<table width="100%">
<tr>
<td width="34"><img src="/icons/clear.gif" width=34 height=1></td>
<td valign="top">
<p><img src="/private/habitat/images/family.jpg" width=226 height=150><br/><br/>
   <span>Quick Gift Guide</span><br><br>
    $10 = Box of Nails<br>
    $35 = Roof Shingles<br>
    $50 = Low Flow Toilet<br>
    $75 = Window<br>
    $100 = Kitchen Sink<br>
    $150 = Front Door<br>
    $500 = Siding<br>
    $1000 = Wallboard<br>
    $2000 = Flooring<br></span>
<img src="/icons/clear.gif" width=227 height=1>
</td>
<td id="Foo" width="*" valign="top">
<table cellpadding="4" BGCOLOR="#E7EBF7">
<tr><td colspan="2" bgcolor="#006699"><font face="arial" color="white">Step 1 - Select the type of product you wish to donate</font></td></tr>
<tr><td>
<FORM ACTION=FRONTDOORWIDGET? METHOD=POST><INPUT TYPE=HIDDEN NAME="Tree" VALUE="(TOP PRODUCT SERVICE ORGANIZATION)">
<DL>
<DD><INPUT TYPE=IMAGE NAME=PRODUCT SRC="/images/red.gif" BORDER=0/><span style="cursor: pointer; text-decoration: underline" onClick="makeRequest('frontdoorwidget?')">Product</span></DD>
<DD><INPUT TYPE=IMAGE NAME=PRODUCT SRC="/images/red.gif" BORDER=0/><A HREF="SEEK?Class=PRODUCT">Product</A></DD>
<DD><IMG SRC="/images/grey.gif"> <A HREF="SEEK?Class=SERVICE">Service</A></DD>
<DD><IMG SRC="/images/grey.gif"> <A HREF="SEEK?Class=ORGANIZATION">Organization</A></DD>
</DL>
</FORM>
</td>
</tr>
</table>
</td></tr>
</table>
</center>
<BR/>

<!--------------------------------------------------------------------------->

<table cellpadding=0 cellspacing=0 border=0 width="100%">
<tr><td bgcolor="#006699" nowrap><img src="/private/habitat/images/blank.gif" alt="" width=1 height=12></td></tr>
</table>

<div id="PageFooter" style="FONT-SIZE:10px;text-align:center;width:100%">
Thank you for visiting  Stanford's Habitat for Humanity Web site.
</div>

<center>
<div style="FONT-SIZE:10px">&copy; 2006 Stanford University Logic Group.  All rights reserved.</div>
</center>

</body>
</html>

;;;;


    function getFormValues (fobj)
      {var str = \"\";
       for(var i = 0; i < fobj.elements.length; i++)
          {switch(fobj.elements[i].type)
            {case \"text\":
               str += fobj.elements[i].name + \"=\" + escape(fobj.elements[i].value) + \"&\";
               break;
             case \"select-one\":
               str += fobj.elements[i].name + \"=\" +fobj.elements[i].options[fobj.elements[i].selectedIndex].value + \"&\";
               break;}}
       str = str.substr(0,(str.length - 1));
       return str;}

    function postEnumeratewidget (fobj)
       {Result.innerHTML = postResult('refind?','Start=&Object=?x&Class=unspsc.521518&End=&Command=Refresh');}

;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; newseek
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod process (s (action (eql 'newseek)) postlines)
  (output-prolog s 200)
  (format s "<HTML>
<HEAD>
<TITLE>~A - Search</TITLE>
</HEAD>
<table width=\"100%\" height=\"100%\">
<tr height=\"100\"><td colspan=\"3\">
<iframe src=\"header?\" NAME=\"header\" SCROLLING=\"no\" MARGINHEIGHT=\"0\" MARGINWIDTH=\"0\"></iframe>
</td></tr>
<tr height=\"*\">
<td>
<iframe src=\"tree?\" NAME=\"tree\" width=\"190\" height=\"80%\" SCROLLING=\"auto\" MARGINHEIGHT=\"0\" MARGINWIDTH=\"0\"></iframe>
</td>
<td width=\"*\">
<iframe src=\"~A\" NAME=\"query\" SCROLLING=\"auto\" MARGINHEIGHT=\"0\" MARGINWIDTH=\"0\"></iframe>
</td>
<td width=\"190\">
<iframe src=\"/pages/empty.html\" NAME=\"result\" SCROLLING=\"auto\" MARGINHEIGHT=\"0\" MARGINWIDTH=\"0\"></iframe>
</td>
<tr height=\"100\"><td colspan=\"3\">
<iframe src=\"footer?\" NAME=\"footer\" SCROLLING=\"no\" MARGINHEIGHT=\"0\" MARGINWIDTH=\"0\"></iframe>
</td></tr>
</table>
</HTML>"
          (prettify *gui*)
          (if postlines (stringappend "refind?Class=" (cdar postlines))
              (stringappend *home* "pages/search.html"))))


;;;;

    function alertRefind()
       {if (http_refind.readyState == 4)
           {if (http_refind.responseText)
               {Query.innerHTML = http_refind.responseText;}
            else {alert('There was a problem with the request.');}}}

;;;;

(defun output-slotlink (s concept)
  (let (doc)
    (unless (findp `(nocommand ,*gui* metalevel) *interface*)
      (format s "<A HREF=\"/~A/CHANGE?Object=~A\" target=\"_blank\"><IMG SRC=\"~Aimages/pencil.gif\" BORDER=\"0\"/></A>"
              (addressify (name *interface*)) (addressify concept) *home*)
      (format s "<A HREF=\"/~A/Qbeview?Object=~A\" target=\"_blank\"><IMG SRC=\"~Aimages/hammer.gif\" BORDER=\"0\"/></A>"
              (addressify *gui*) (addressify concept) *home*)) ; (name *gui*)
    (if (setq doc (find-documentation concept))
      (format s "<A HREF=\"DISPLAYTABLE?Relation=~A\" target=\"_top\" onMouseOver='window.status=\"~A\"; return true'>~A</A>"
              (addressify concept) doc (iconify concept))
      (format s "<A HREF=\"DISPLAYTABLE?Relation=~A\" target=\"_top\">~A</A>"
              (addressify concept) (iconify concept)))))

(defun output-classlink (s concept)
  (let (doc)
    (unless (findp `(nocommand ,*gui* metalevel) *interface*)
      (format s "<A HREF=\"/~A/CHANGE?Object=~A\" target=\"_blank\"><IMG SRC=\"~Aimages/pencil.gif\" BORDER=\"0\"/></A>"
              (addressify (name *interface*)) (addressify concept) *home*)
      (format s "<A HREF=\"/~A/Qbeview?Object=~A\" target=\"_blank\"><IMG SRC=\"~Aimages/hammer.gif\" BORDER=\"0\"/></A>"
              (addressify *gui*) (addressify concept) *home*))
    (if (setq doc (find-documentation concept))
      (format s "<A HREF=\"DISPLAYCLASS?Class=~A\" target=\"_top\" onMouseOver='window.status=\"~A\"; return true'>~A</A>"
              (addressify concept) doc (prettify concept))
      (format s "<A HREF=\"DISPLAYCLASS?Class=~A\" target=\"_top\">~A</A>"
              (addressify concept) (prettify concept)))))

;;;;

(defparameter *messagebreak* "



")

(defun readmultipart (s contentlength)
  (let (input)
    (setq input
          (with-output-to-string (str)
            (do ((i 1 (1+ i)))
                ((> i contentlength) )
                (write-char (read-char s) str))))
    (get-postlines input)))

(defun extractdata (body)
  (let (boundary beg end)
    (setq boundary (with-input-from-string (s body) (read-line s)))
    (setq beg (+ (search *messagebreak* body) 4))
    (setq end (- (search boundary body :start2 beg) 1))
    (subseq body beg end)))

(defun get-postlines (body)
  (let (boundary beg end postlines)
    (setq boundary (with-input-from-string (s body) (read-line s)))
    (setq beg 1 end 1)
    (loop (setq beg (search *messagebreak* body :start2 end))
          (when (not (integerp beg)) (return (nreverse postlines)))
          (setq end (search boundary body :start2 beg))
          (when (not (integerp end)) (return (nreverse postlines)))
          (setq postlines (acons "Foo" (subseq body (+ beg 4) (- end 2)) postlines)))))

;;;;

(defun output-html-prolog (s status-code)
  (format s "HTTP/1.0 ~D ~A"
	  status-code (rest (assoc status-code *http-status-codes*))) (crlf s)
  (format s "Set-Cookie: ~A=~D; path=/" (addressify *gui*) (get-universal-time)) (crlf s)
  (format s "Content-type: text/html") (crlf s)
  (crlf s))  

;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; updateselection
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod process (s (file (eql 'updateselection)) postlines)
  (let (command)
    (setq command (getf-post "Command" postlines))
    (cond ((null command)
           (process-updateclass-start s postlines))
          ((string-equal command "   Edit   ")
           (process-updateselection-kerchunk s postlines))
          (t (process-updateclass-update s postlines)))))

(defun process-updateselection-kerchunk (s postlines)
  (let (class attributes aspect kif objects count start end)
    (setq class (read-value-string (cdr (pop postlines))))
    (setq aspect (read-value-string (cdr (pop postlines))))
    (setq kif (read-from-string (cdr (pop postlines))))
    (setq start (read-value-string (getf-post "Start" postlines)))
    (setq end (read-value-string (getf-post "End" postlines)))
    (setq attributes (modifiable-slots class))
    (setq objects (request `(ask-all ,aspect ,kif) *client* *gui*))
    (multiple-value-setq (objects count start end) (trim objects start end))
    (output-prolog s 200)
    (output-header s "UpdateSelection")
    (output-old-updateclass-table s class attributes objects count start end)
    (output-updateselection-commands s class aspect kif count start end)
    (output-footer s)))

(defun output-updateselection-commands (s class aspect kif count start end)
  (multiple-value-setq (start end) (kerchunk count start end))
  (format s "<FORM ACTION=UPDATESELECTION? METHOD=POST>")
  (format-hidden s "Class" (stringize class))
  (format-hidden s "Aspect" (prettify aspect))
  (format-hidden s "Kif" (htmlify (prin1-to-string kif)))
  (format-button s "Command" "   Edit   ")
  (format s "answers ")
  (format-text s "Start" (princ-to-string start) 5)
  (format s " through ")
  (format-text s "End" (princ-to-string end) 5)
  (format s "<BR>")
  (format s "</FORM>"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; describe
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod process (s (command (eql 'describe)) postlines)
  (let (object desc)
    (setq object (read-value-string (cdr (pop postlines))))
    (setq desc (find-documentation object))
    (output-prolog s 200)
    (output-header s "Describe ~A" (prettify object))
    (format s "<P>")
    (if desc (format s "~A" desc) (format s "No description available."))
    (format s "<P>")
    (output-footer s)))

(defun output-doclink (s concept)
  (if (find-documentation concept)
      (format s "<A HREF=\"DESCRIBE?Object=~A\">~A</A>"
              (addressify concept) (iconify concept))
      (format s "~A" (iconify concept))))

;;;;

(defun widget-prolog (s)
  (format s "HTTP/1.0 200 OK") (crlf s)
  (format s "Content-type: text/xml") (crlf s)
  (crlf s)
  (format s "<?xml version=\"1.0\"?>") (crlf s))

;;;;

(defun parse-post-data (line)
  (declare (type string line))
  (loop
    with start = 0
    while (and line (not (= start (length line))))
    for eq-pos = (position #\= line :start start)
    for amp-pos = (position #\& line :start start)
    when eq-pos
    collect (cons (decode-url-chrs (subseq line start eq-pos))
                  (decode-url-chrs (subseq line (1+ eq-pos) amp-pos)))
    do (if amp-pos (setq start (1+ amp-pos)) (setq start (length line)))))

;;;;

(defun javascript ()
  "<script>
    var http_tree = false;
    var http_find = false;
    var http_request = false;
    var http_result = false;
    var textchange = false;

    function getFrontdoorwidget ()
       {http_tree = false;
        if (window.XMLHttpRequest)
           {http_tree = new XMLHttpRequest();
            if (http_tree.overrideMimeType)
               {http_tree.overrideMimeType('text/xml');}}
        else if (window.ActiveXObject)
                {try {http_tree = new ActiveXObject(\"Msxml2.XMLHTTP\");} 
                 catch (e) {try {http_tree = new ActiveXObject(\"Microsoft.XMLHTTP\");}
                            catch (e) {}}}
        if (!http_tree)
           {alert('Giving up: Cannot create an XMLHTTP instance');
            return false;}
        http_tree.onreadystatechange = alertTree;
        http_tree.open('GET', 'tree?', true);
        http_tree.send(null);}

    function postFrontdoorwidget (tree, node)
       {http_tree = false;
        if (window.XMLHttpRequest)
           {http_tree = new XMLHttpRequest();
            if (http_tree.overrideMimeType)
               {http_tree.overrideMimeType('text/xml');}}
        else if (window.ActiveXObject)
                {try {http_tree = new ActiveXObject(\"Msxml2.XMLHTTP\");} 
                 catch (e) {try {http_tree = new ActiveXObject(\"Microsoft.XMLHTTP\");}
                            catch (e) {}}}
        if (!http_tree)
           {alert('Giving up: Cannot create an XMLHTTP instance');
            return false;}
        http_tree.onreadystatechange = alertTree;
        http_tree.open('POST', 'tree?', true);
        http_tree.send('Tree=' + tree + '&Node=' + node);}

    function loneClickEventHandler (node)
      {postFind(node);}

    function postSmartFind (structure, node)
       {http_find = false;
        if (window.XMLHttpRequest) { // Mozilla, Safari,...
            http_find = new XMLHttpRequest();
            if (http_find.overrideMimeType) {
                http_find.overrideMimeType('text/xml');}}
        else if (window.ActiveXObject) { // IE
            try {http_find = new ActiveXObject(\"Msxml2.XMLHTTP\");} 
                catch (e) {
                try {http_find = new ActiveXObject(\"Microsoft.XMLHTTP\");}
                    catch (e) {} }}
        if (!http_find)
           {alert('Giving up: Cannot create an XMLHTTP instance');
            return false;}
        http_find.onreadystatechange = alertFind;
        http_find.open('POST', 'smartfind?', true);
        http_find.send('Structure=' + structure + '&Class=' + node);
        return ''}

    function postFind (node)
       {http_find = false;
        if (window.XMLHttpRequest) { // Mozilla, Safari,...
            http_find = new XMLHttpRequest();
            if (http_find.overrideMimeType) {
                http_find.overrideMimeType('text/xml');}}
        else if (window.ActiveXObject) { // IE
            try {http_find = new ActiveXObject(\"Msxml2.XMLHTTP\");} 
                catch (e) {
                try {http_find = new ActiveXObject(\"Microsoft.XMLHTTP\");}
                    catch (e) {} }}
        if (!http_find)
           {alert('Giving up: Cannot create an XMLHTTP instance');
            return false;}
        http_find.onreadystatechange = alertFind;
        http_find.open('POST', 'find?', true);
        http_find.send('Class=' + node);
        return ''}

    function postRequest (url,args)
       {http_request = false;
        if (window.XMLHttpRequest)
           {http_request = new XMLHttpRequest();
            if (http_request.overrideMimeType)
               {http_request.overrideMimeType('text/xml');}}
        else if (window.ActiveXObject) { // IE
            try {http_request = new ActiveXObject(\"Msxml2.XMLHTTP\");} 
                 catch (e) {
                try {http_request = new ActiveXObject(\"Microsoft.XMLHTTP\");}
                    catch (e) {} }}
        if (!http_request)
           {alert('Giving up: Cannot create an XMLHTTP instance');
            return false;}
        http_request.onreadystatechange = alertQuery;
        http_request.open('POST', url, false);
        http_request.send(args);}

    function postResult (url,args)
       {http_result = false;
        if (window.XMLHttpRequest)
           {http_result = new XMLHttpRequest();
            if (http_result.overrideMimeType)
               {http_result.overrideMimeType('text/xml');}}
        else if (window.ActiveXObject) { // IE
            try {http_result = new ActiveXObject(\"Msxml2.XMLHTTP\");} 
                 catch (e) {
                try {http_result = new ActiveXObject(\"Microsoft.XMLHTTP\");}
                    catch (e) {} }}
        if (!http_result)
           {alert('Giving up: Cannot create an XMLHTTP instance');
            return false;}
        http_result.onreadystatechange = alertResult;
        http_result.open('POST', url, true);
        http_result.send(args);}

    function postAugmentWidget (cell,fobj)
       {postRequest('find?',getFormValues(fobj) + '&' + cell + '.x=')}

    function postReviseWidget (cell,fobj)
       {postRequest('find?',getFormValues(fobj) + '&' + cell + '.x=');
        postResult('find?',getFormValues(form1) + '&Command=Refresh');}

    function postEnumeratewidget (fobj)
       {postResult('find?',getFormValues(fobj));}

    function postEnumerateBack (fobj)
       {postResult('list?',getFormValues(fobj) + '&Command=Back');}

    function postEnumerateNext (fobj)
       {postResult('list?',getFormValues(fobj) + '&Command=Next');}

    function getFormValues (fobj)
      {var str = \"\";
       for (var i = 0; i < fobj.elements.length; i++)
         {if (fobj.elements[i].type == \"select-one\")
             str += fobj.elements[i].name + \"=\" +fobj.elements[i].options[fobj.elements[i].selectedIndex].value + \"&\";
          else if (fobj.elements[i].type == \"radio\")
                  {if (fobj.elements[i].checked)
                      {str += fobj.elements[i].name + \"=\" + escape(fobj.elements[i].value) + \"&\"}}
          else str += fobj.elements[i].name + \"=\" + escape(fobj.elements[i].value) + \"&\";}
       str = str.substr(0,(str.length - 1));
       return str;}

    function populate (node)
      {tree.innerHTML = getFrontdoorwidget();
       if (node == '')
          {query.innerHTML = \"<BR/><BR/><center><table><tr><td><UL><LI>Click on class name at left to search for items of that class.</LI><LI>Results will appear on the right.</LI></UL></td></tr></table></center>\"}
       else query.innerHTML = postFind(node);
       result.innerHTML = \"\";}

    function alertTree()
       {if (http_tree.readyState == 4)
           {if (http_tree.responseText)
               {tree.innerHTML = http_tree.responseText;}
            else {alert('There was a problem with the request.');}}}

    function alertFind()
       {if (http_find.readyState == 4)
           {if (http_find.responseText)
               {query.innerHTML = http_find.responseText;
                postEnumeratewidget(form1)}
            else {alert('There was a problem with the request.');}}}

    function alertQuery()
       {if (http_request.readyState == 4)
           {if (http_result.responseText)
               {query.innerHTML = http_request.responseText;}
            else {alert('There was a problem with the request.');}}}

    function alertResult()
       {if (http_result.readyState == 4)
           {if (http_result.responseText)
               {result.innerHTML = http_result.responseText}
            else {alert('There was a problem with the request.')}}}

    function textEdit(e)
       {var keynum;
        if (window.event)
           {keynum = e.keyCode}
        else if(e.which)
           {keynum = e.which};
        if (keynum == 13)
           {textchange = false;
            postResult('find?',getFormValues(form1) + '&Command=Refresh');
            return false}
        else {textchange = true;
              return true}}

    function textBlur(e)
      {if (textchange == true)
          {textchange = false;
           postResult('find?',getFormValues(form1) + '&Command=Refresh')}}

    var triple_result = false;

    function postTriple (slot,object,value)
       {triple_result = false;
        if (window.XMLHttpRequest)
           {triple_result = new XMLHttpRequest();
            if (triple_result.overrideMimeType)
               {triple_result.overrideMimeType('text/xml');}}
        else if (window.ActiveXObject) { // IE
            try {triple_result = new ActiveXObject(\"Msxml2.XMLHTTP\");} 
                 catch (e) {
                try {triple_result = new ActiveXObject(\"Microsoft.XMLHTTP\");}
                    catch (e) {} }}
        if (!triple_result)
           {alert('Giving up: Cannot create an XMLHTTP instance');
            return false;}
        triple_result.onreadystatechange = alertTriple;
        triple_result.open('POST', 'triple?', true);
        triple_result.send('slot=' + slot + '&object=' + object + '&value=' + value)}

    function alertTriple()
       {if (triple_result.readyState == 4)
           {if (!triple_result.responseText)
               {alert('There was a problem with the request.')}}}

   </script>")

;;;;

(defmethod process (s (action (eql 'seek)) postlines)
  (let (init)
    (setq init (if postlines (cdar postlines) ""))
    (format-html s) (crlf s)
    (format s "<head>")
    (format s "<title>~A - Seek</title>" (prettify *gui*))
    (format s (javascript))
    (format s "</head>") (crlf s)
    (format s "<body bgcolor=\"~A\" leftmargin=\"0\" topmargin=\"0\" marginwidth=\"0\" marginheight=\"0\" onLoad=\"populate('~A')\">" *bgcolor* init) (crlf s)
    (output-header s)
    (format s "<table cellpadding=\"4\" width=\"100%\"><tr>") (crlf s)
    (format s "<td id=\"tree\" width=\"20%\" valign=\"top\"></td>") (crlf s)
    (format s "<td id=\"query\" width=\"*\" valign=\"top\" bgcolor=\"#EEEEEE\"></td>") (crlf s)
    (format s "<td id=\"result\" width=\"20%\" valign=\"top\"></td>") (crlf s)
    (format s "</tr></table>") (crlf s)
    (output-footer s)
    (finish-body s) (crlf s)
    (finish-html s) (crlf s)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; fastexhibit page
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod process (s (action (eql 'fastexhibit)) postlines)
  (let (dum)
    (cond ((setq dum (getf-post "Class" postlines))
           (process-fastexhibit s (fastitem '? (read-user-string dum))))
          ((setq dum (getf-post "Structure" postlines))
           (process-fastexhibit s (faststructure (read-user-string dum))))
          (t (http-problem s "Bad request.")))))

(defun process-fastexhibit (s structure)
  (format-html s) (crlf s)
  (format s "<head>")
  (format s "<title>Fastexhibit</title>")
  (format s (fastscript))
  (format s "</head>") (crlf s)
  (format-body s *bgcolor*) (crlf s)
  (output-header s)
  (process-fastgallery s structure)
  (output-footer s)
  (finish-body s) (crlf s)
  (finish-html s) (crlf s))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; fastshow page
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod process (s (command (eql 'fastshow)) postlines)
  (let (dum start end)
    (cond ((setq dum (getf-post "Class" postlines))
           (setq start (or (read-value-string (getf-post "Start" postlines)) 1))
           (setq end (or (read-value-string (getf-post "End" postlines)) *count*))
           (process-fastshow s (fastitem '? (read-user-string dum)) start end))
          ((setq dum (getf-post "Structure" postlines))
           (setq start (or (read-value-string (getf-post "Start" postlines)) 1))
           (setq end (or (read-value-string (getf-post "End" postlines)) *count*))
           (process-fastshow s (faststructure (read-user-string dum)) start end))
          (t (http-problem s "Bad request.")))))

(defun process-fastshow (s structure start end)
  (format-html s) (crlf s)
  (format s "<head>")
  (format s "<title>Fastshow</title>")
  (format s (fastscript))
  (format s "</head>") (crlf s)
  (format-body s *bgcolor*) (crlf s)
  (output-header s)
  (format s "<div id='result'>")
  (process-fastview s structure start end)
  (format s "</div>")
  (output-footer s)
  (finish-body s) (crlf s)
  (finish-html s) (crlf s))

(defun fastitem (object class)
  (do ((l (queryable-slots class) (cdr l)) (nl))
      ((null l) (list* object class (nreverse nl)))
      (setq nl (cons (list (car l)) nl))))

(defun faststructure (structure)
  (do ((l (queryable-slots (cadr structure)) (cdr l)) (dum) (nl))
      ((null l) (list* (car structure) (cadr structure) (nreverse nl)))
      (cond ((setq dum (assoc (car l) (cddr structure)))
             (setq nl (cons dum nl)))
            (t (setq nl (cons (list (car l)) nl))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; fastview widget
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod process (s (command (eql 'fastview)) postlines)
  (let (dum start end)
    (cond ((and (null (cdr postlines)) (setq dum (getf-post "Class" postlines)))
           (setq start (or (read-value-string (getf-post "Start" postlines)) 1))
           (setq end (or (read-value-string (getf-post "End" postlines)) *count*))
           (process-fastview s (fastitem '? (read-user-string dum)) start end))
          ((setq dum (getf-post "Structure" postlines))
           (setq start (or (read-value-string (getf-post "Start" postlines)) 1))
           (setq end (or (read-value-string (getf-post "End" postlines)) *count*))
           (process-fastview s (read-user-string dum) start end))
          (t (process-fastview s (fastparsestructure postlines) 1 *count*)))))

(defun process-fastview (s structure start end)
  (let (objects sorter attributes results count)
    (setq objects (asks (car structure) (maksand (fastconverter structure)) nil *gui*))
    (when (setq sorter (find-sorter (cadr structure)))
      (setq objects (sortem objects sorter 'ascending)))
    (setq attributes (mapcar #'car (cddr structure)))
    (multiple-value-setq (objects count start end) (trim objects start end))
    (setq results (prorequest `(ask-table ,objects ,attributes)))
    (output-fastview s structure objects attributes results count start end)))

(defun output-fastview (s structure objects slots results count start end)
  (format s "<div style='margin-left: 10px; margin-right: 10px'>") (crlf s)
  (format s "<center>")
  (cond ((= count 0)
         (format s "<p>There are no answers.</p>") (crlf s)
         (unless (findp `(or (nocommand ,*gui* create) (nocreate ,*gui* ,(cadr structure))) *interface*)
           (format s "<form action='display?' method='post' target='_blank'>" (stringize structure))
           (format-hidden s "Class" (stringize (cadr structure)))
           (format-button s "Command" "Create")
           (format s " a new ~A." (prettify (cadr structure)))
           (format s "</form>") (crlf s)))
        ((and (= start 1) (geqp end count))
         (format s "<p>There are ~D answers.</p>" count) (crlf s)
         (output-fastview-inner s structure objects slots results)
         (unless (findp `(or (nocommand ,*gui* create) (nocreate ,*gui* ,(cadr structure))) *interface*)
           (format s "<form action='display?' method='post' target='_blank'>" (stringize structure))
           (format-hidden s "Class" (stringize (cadr structure)))
           (format-button s "Command" "Create")
           (format s " a new ~A." (prettify (cadr structure)))
           (format s "</form>") (crlf s)))
        (t (format s "<table>")
           (format s "<tr><td>There are ~D answers.  The following table shows answers ~A through ~A.</td></tr>"
                   count start end)
           (format s "<tr><td>")
           (output-fastview-inner s structure objects slots results)
           (format s "</td></tr><tr><td>")
           (multiple-value-setq (start end) (kerchunk count start end))
           (output-fastview-commands s structure start end)
           (format s "</td></tr></table>")))
  (format s "</center>") (crlf s)
  (format s "</div>") (crlf s))

(defun output-fastview-inner (s structure items slots results)
  (let (class nohandle)
    (setq class (cadr structure))
    (setq nohandle (findp `(nodisplay ,class handle) *interface*))
    (format s "<table cellspacing='3' bgcolor='~A' border='~A'>" *bgcolor* *border*) (crlf s)
    (format s "<form name='form2' action='fastview?' method='post'>") (crlf s)
    (format-hidden s "Object" (stringize (car structure))) (crlf s)
    (format-hidden s "Class" (stringize (cadr structure))) (crlf s)
    (format s "<tr>")
    (unless nohandle
      (format s "<th>")
      (format s "<span style='cursor: pointer; font-weight: bold; text-decoration: underline; color: #000000' onClick=\"postResult('Fastview?',newFormValues(window.document.form2))\">")
      (format s (iconify class))
      (format s "</span>")
      (format s "</th>"))
    (dolist (pair (cddr structure))
      (format s "<th>")
      (output-fastview-cell s (car pair) (cadr pair) structure)
      (format s "</th>")
      (crlf s))
    (format s "</tr>") (crlf s)
    (format s "</form>") (crlf s)
    (do ((l items (cdr l)) (m results (cdr m)) (flag nil (not flag)))
        ((null l))
        (if flag (format s "<tr>") (format s "<tr bgcolor='#EEEEEE'>"))
        (unless nohandle
          (format s "<td align='left'>")
          (output-fastobject s (car l))
          (format s "</td>"))
        (do ((n (car m) (cdr n)) (slots slots (cdr slots)) (style) (vals)) 
            ((null n))
            (setq style (find-comparestyle (car slots)))
            (setq vals (car n))
            (setq vals (remove 'unknown vals))
            (if (every #'(lambda (val) (numberp val)) vals)
              (format s "<td align='right'>")
              (format s "<td>"))
            (loop
              for val in vals
              for first-time = t then nil
              unless first-time
              do (format s ", ")
              do (output-fastobject-in-style s val style))
            (format s "<br/></td>"))
        (format s "</tr>")
        (crlf s))
    (format s "</table>") (crlf s)))


(defun output-fastview-cell (s slot value structure)
  (let (style)
    (setq style (find-searchstyle slot))
    (cond ((eq style 'multichoicelist) (output-fastview-selector s slot value structure))
          ((eq style 'dropdownlist) (output-fastview-selector s slot value structure))
          ((eq style 'hierarchicalselector) (output-fastview-selector s slot value structure))
          ((eq style 'checkbox) (output-fastview-selector s slot value structure))
          ((eq style 'radiobutton) (output-fastview-selector s slot value structure))
          ((eq style 'interval) (output-fastview-selector s slot value structure))
          ((eq style 'textarea) (output-fastview-selector s slot value structure))
          ((eq style 'stringfield) (output-fastview-selector s slot value structure))
          ((eq style 'text) (output-fastview-glyph s slot value))
          ((eq style 'password) (output-fastview-selector s slot value structure))
          ((eq style 'urlstyle) (output-fastview-selector s slot value structure))
          ((eq style 'glyph) (output-fastview-glyph s slot value))
          (t (output-fastview-selector s slot value structure)))))

(defun output-fastview-selector (s slot value structure)
  (let (options)
    (when (setq options (fastoptions slot structure))
      (format s "<select name='~A' onChange=\"postResult('Fastview?',newFormValues(this.form))\">"
              (stringize slot))
      (dolist (option options)
        (cond ((equalp option value)
               (format s "<option value='~A' SELECTED>~A</option>" (stringize option) (newprettify slot option)) (crlf s))
              (t (format s "<option value='~A'>~A</option>" (stringize option) (newprettify slot option)) (crlf s))))
      (format s "</select>"))))

(defun output-fastview-glyph (s slot value)
  (format-hidden s (stringize slot) (stringize value))
  (format s "~A" (iconify slot)))

(defun output-fastview-commands (s structure start end)
  (format s "<form action='display?' method='post' target='_blank'>" (stringize structure))
  (format-hidden s "Class" (stringize (cadr structure)))
  (unless (findp `(or (nocommand ,*gui* create) (nocreate ,*gui* ,(cadr structure))) *interface*)
    (format-button s "Command" "Create")
    (format s " a new ~A." (prettify (cadr structure))))
  (format s "</form>") (crlf s)
  (format s "<form action='fastshow?' method='post'>")
  (format-hidden s "Structure" (htmlify (prin1-to-string structure)))
  (format-button s "Command" "Display")
  (format s "answers ")
  (format-text s "Start" (princ-to-string start) 5)
  (format s " through ")
  (format-text s "End" (princ-to-string end) 5)
  (format s "</form>") (crlf s))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; easy
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; easyseek page
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod process (s (action (eql 'easyseek)) postlines)
  (cond ((equalp (caar postlines) "Class")
         (process-easyseek s (fastitem '? (read-user-string (cdar postlines)))))
        ((equalp (caar postlines) "Structure")
         (process-easyseek s (faststructure (read-user-string (cdar postlines)))))
        (t (process-easyseek s (fastparsestructure postlines)))))

(defun process-easyseek (s structure)
  (format-html s) (crlf s)
  (format s "<head>")
  (format s "<title>Easyseek</title>")
  (format s (fastscript))
  (format s "</head>") (crlf s)
  (format-body s *bgcolor*) (crlf s)
  (output-header s)
  (format s "<form id='form1' action='easyseek?' method='post'>")
  (format s "<table width='100%' cellpadding='0' cellspacing='0' bgcolor='#EEEEEE'>")
  (format s "<tr><td height='80' align='center' valign='bottom'>")
  (format s "<p>Select desired features.  Click on images for more information.  Click <span style='cursor:pointer; font-weight:bold; text-decoration:underline;' onClick='window.document.form1.submit()'>here</span> to start over.</p>")
  (output-easyfind-structure s structure)
  (format s "</td></tr>")
  (format s "<tr><td bgcolor='#ff9933' nowrap><img src='/private/catalog/images/blank.gif' alt='' width='1' height='2'></td></tr>")
  (format s "</table>")
  (process-fastgallery s structure)
  (format s "</form>")
  (output-footer s)
  (finish-body s) (crlf s)
  (finish-html s) (crlf s))

(defun output-easyfind-structure (s structure)
  (format-hidden s "Object" (stringize (car structure)))
  (format-hidden s "Class" (stringize (cadr structure)))
  (format s "<table cellpadding='0' cellspacing='2'>")
  (format s "<tr>")
  (dolist (pair (cddr structure))
    (format s "<td>")
    (output-easyfind-selector s (car pair) (cadr pair) structure)
    (format s "</td>")
    (crlf s))
  (format s "</tr>")
  (format s "</table>")
  (format s "</form>"))

(defun output-easyfind-selector (s slot value structure)
  (let (options)
    (when (setq options (fastoptions slot structure))
      (format s "<select name='~A' onChange='window.document.form1.submit()'>"
              (stringize slot))
      (dolist (option options)
        (cond ((equalp option value)
               (format s "<option value='~A' SELECTED>~A</option>" (stringize option) (newprettify slot option)) (crlf s))
              (t (format s "<option value='~A'>~A</option>" (stringize option) (newprettify slot option)) (crlf s))))
      (format s "</select>"))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; niceseek page
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod process (s (action (eql 'niceseek)) postlines)
  (let (dum)
    (cond ((setq dum (getf-post "Class" postlines))
           (process-niceseek s (fastitem '? (read-user-string dum))))
          ((setq dum (getf-post "Structure" postlines))
           (process-niceseek s (faststructure (read-user-string dum))))
          (t (http-problem s "Bad request.")))))

(defun process-niceseek (s structure)
  (format-html s) (crlf s)
  (format s "<head>")
  (format s "<title>Niceseek</title>")
  (format s (fastscript))
  (format s "</head>") (crlf s)
  (format-body s *bgcolor*) (crlf s)
  (output-header s)
  (format s "<div id='result'>")
  (output-nicefind-structure s structure)
  (format s "</div>")
  (output-footer s)
  (finish-body s) (crlf s)
  (finish-html s) (crlf s))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; nicefind widget
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod process (s (command (eql 'nicefind)) postlines)
  (let (dum)
    (cond ((null postlines) (http-problem s "No structure specified."))
          ((setq dum (getf-post "Structure" postlines))
           (output-nicefind-structure s (read-user-string dum)))
          (t (output-nicefind-structure s (fastparsestructure postlines))))))

(defun output-nicefind-structure (s structure)
  (format s "<table width='100%' cellpadding='0' cellspacing='0' bgcolor='#EEEEEE'>")
  (format s "<tr><td height='80' align='center' valign='bottom'>")
  (format s "<p>Select desired features.  Click on images for more information.  Click <span style='cursor:pointer; font-weight:bold; text-decoration:underline;' onClick='location.reload()'>here</span> to start over.</p>")
  (format s "<form id='form1' action='easyseek?' method='post'>")
  (format-hidden s "Object" (stringize (car structure)))
  (format-hidden s "Class" (stringize (cadr structure)))
  (format s "<table cellpadding='0' cellspacing='2'>")
  (format s "<tr>")
  (dolist (pair (cddr structure))
    (format s "<td>")
    (output-nicefind-selector s (car pair) (cadr pair) structure)
    (format s "</td>")
    (crlf s))
  (format s "</tr>")
  (format s "</table>")
  (format s "</form>")
  (format s "</td></tr>")
  (format s "<tr><td bgcolor='#ff9933' nowrap><img src='/private/catalog/images/blank.gif' alt='' width='1' height='2'></td></tr>")
  (format s "</table>")
  (process-fastgallery s structure)
  'done)

(defun output-nicefind-selector (s slot value structure)
  (let (options)
    (when (setq options (fastoptions slot structure))
      (format s "<select name='~A' onChange='postResult(\"nicefind?\",newFormValues(this.form))'>"
              (stringize slot))
      (dolist (option options)
        (cond ((equalp option value)
               (format s "<option value='~A' SELECTED>~A</option>" (stringize option) (newprettify slot option)) (crlf s))
              (t (format s "<option value='~A'>~A</option>" (stringize option) (newprettify slot option)) (crlf s))))
      (format s "</select>"))))

;;;;


(defun factfindoptions (slot structure source)
  (let (predicate)
    (setq predicate (find-predicate (cadr structure)))
    (do ((l (indexees slot source) (cdr l)) (nl))
        ((null l) (nreverse nl))
        (when (and (eq (caar l) slot) (factdoublep predicate (cadar l) source))
          (setq nl (adjoin (caddar l) nl :test #'equalp))))))

;;;;

(defun fastcreatescript ()
  "<script type='text/javascript' language='javascript'>

  function selectvalue (slot,domain)
   {var widget = document.getElementsByName(slot)[0];
    var value = showModalDialog('selectvalue?Slot=' + slot + '&Domain=' + domain,'','resizable:yes');
    if (value) widget.value = value}

  function selector (category, slot, name)
   {var cell = document.getElementsByName(name)[0];
    var value = showModalDialog('options?Class=' + category + '&Attribute=' + slot,'','resizable:yes');
    if (value) cell.value = value}

    function postResult (url,args)
       {http_result = false;
        if (window.XMLHttpRequest)
           {http_result = new XMLHttpRequest();
            if (http_result.overrideMimeType)
               {http_result.overrideMimeType('text/xml');}}
        else if (window.ActiveXObject) { // IE
            try {http_result = new ActiveXObject(\"Msxml2.XMLHTTP\");}
                 catch (e) {
                try {http_result = new ActiveXObject(\"Microsoft.XMLHTTP\");}
                    catch (e) {} }}
        if (!http_result)
           {alert('Giving up: Cannot create an XMLHTTP instance');
            return false;}
        http_result.onreadystatechange = alertResult;
        http_result.open('POST', url, true);
        http_result.send(args);}

    function alertResult()
       {if (http_result.readyState == 4)
           {if (http_result.responseText)
               {result.innerHTML = http_result.responseText}
            else {alert('There was a problem with the request in alertResult.')}}}

  function textEdit(e)
   {var keynum;
    if (window.event)
       {keynum = e.keyCode}
    else if(e.which)
       {keynum = e.which};
    if (keynum == 13)
       {textchange = false;
        postResult('fastcreate?',newFormValues(form1));
        return false}
    else {textchange = true;
          return true}}

  function textBlur(e)
  {if (textchange == true)
      {textchange = false;
       postResult('fastcreate?',newFormValues(form1))}}

  function newFormValues (fobj)
    {var str = '';
     for (var i = 0; i < fobj.elements.length; i++)
       {if (fobj.elements[i].type == 'select-one')
           {var val = fobj.elements[i].options[fobj.elements[i].selectedIndex].value;
            str += fobj.elements[i].name + '=' + val + '&';}
        else if (fobj.elements[i].type == 'radio' || fobj.elements[i].type == 'checkbox')
                {if (fobj.elements[i].checked)
                    {str += fobj.elements[i].name + '=' + escape(fobj.elements[i].value) + '&'}}
        else if (fobj.elements[i].type != 'submit')
                str += fobj.elements[i].name + '=' + escape(fobj.elements[i].value) + '&';}
     str = str.substr(0,(str.length - 1));
     return str;}

   </script>")

;;;;

(defun output-fastcreate-selector (s slot value)
  (let (options)
    (when (setq options (find-alternatives slot))
      (format s "<select name='~A' onChange='postResult(\"fastcreate?\",newFormValues(this.form))'>"
              (stringize slot))
      (dolist (option options)
        (cond ((equalp option value)
               (format s "<option value='~A' SELECTED>~A</option>" (stringize option) (newprettify slot option)) (crlf s))
              (t (format s "<option value='~A'>~A</option>" (stringize option) (newprettify slot option)) (crlf s))))
      (format s "</select>"))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Scripts
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun fastscript ()
  "<script>
    var http_request = false;
    var http_result = false;
    var textchange = false;

    function postRequest (url,args)
       {http_request = false;
        if (window.XMLHttpRequest)
           {http_request = new XMLHttpRequest();
            if (http_request.overrideMimeType)
               {http_request.overrideMimeType('text/xml');}}
        else if (window.ActiveXObject) { // IE
            try {http_request = new ActiveXObject(\"Msxml2.XMLHTTP\");}
                 catch (e) {
                try {http_request = new ActiveXObject(\"Microsoft.XMLHTTP\");}
                    catch (e) {} }}
        if (!http_request)
           {alert('Giving up: Cannot create an XMLHTTP instance');
            return false;}
        http_request.onreadystatechange = alertQuery;
        http_request.open('POST', url, true);
        http_request.send(args);}

    function alertQuery()
       {if (http_request.readyState == 4)
           {if (http_request.responseText)
               {query.innerHTML = http_request.responseText;}
            else {alert('There was a problem with the request in alertQuery.');}}}

    function postResult (url,args)
       {http_result = false;
        if (window.XMLHttpRequest)
           {http_result = new XMLHttpRequest();
            if (http_result.overrideMimeType)
               {http_result.overrideMimeType('text/xml');}}
        else if (window.ActiveXObject) { // IE
            try {http_result = new ActiveXObject(\"Msxml2.XMLHTTP\");}
                 catch (e) {
                try {http_result = new ActiveXObject(\"Microsoft.XMLHTTP\");}
                    catch (e) {} }}
        if (!http_result)
           {alert('Giving up: Cannot create an XMLHTTP instance');
            return false;}
        http_result.onreadystatechange = alertResult;
        http_result.open('POST', url, true);
        http_result.send(args);}

    function alertResult()
       {if (http_result.readyState == 4)
           {if (http_result.responseText)
               {result.innerHTML = http_result.responseText}
            else {alert('There was a problem with the request in alertResult.')}}}

    function textEdit(e)
       {var keynum;
        if (window.event)
           {keynum = e.keyCode}
        else if(e.which)
           {keynum = e.which};
        if (keynum == 13)
           {textchange = false;
            postResult('fastshow?',newFormValues(form1));
            return false}
        else {textchange = true;
              return true}}

    function textBlur(e)
      {if (textchange == true)
          {textchange = false;
           postResult('fastshow?',newFormValues(form1))}}

    function newFormValues (fobj)
      {var str = \"\";
       for (var i = 0; i < fobj.elements.length; i++)
         {if (fobj.elements[i].type == \"select-one\")
             {var val = fobj.elements[i].options[fobj.elements[i].selectedIndex].value;
              str += fobj.elements[i].name + \"=\" + val + \"&\";}
          else if (fobj.elements[i].type == \"radio\" || fobj.elements[i].type == \"checkbox\")
                  {if (fobj.elements[i].checked)
                      {str += fobj.elements[i].name + \"=\" + escape(fobj.elements[i].value) + \"&\"}}
          else str += fobj.elements[i].name + \"=\" + escape(fobj.elements[i].value) + \"&\";}
       str = str.substr(0,(str.length - 1));
       return str;}

  function postMessage (url,args)
    {http_request = false;
     http_request = new XMLHttpRequest();
     if (http_request.overrideMimeType)
        {http_request.overrideMimeType('text/xml')};
     if (!http_request)
        {alert('Giving up: Cannot create an XMLHTTP instance');
         return false};
     http_request.onreadystatechange = alertMessage;
     http_request.open('POST', url, true);
     http_request.send(args)}

  function alertMessage ()
    {if (http_request.readyState == 4)
        {if (http_request.responseText)
            {return true}
         else {alert('There was a problem with the request.')}}}

   </script>")

;;;;

(defun output-fastcreate-cell (s style slot domain value)
  (cond ((eq style 'selector) (output-selector s slot (find-alternatives slot) value))
        ((eq style 'combobox) (output-combobox s slot domain value 40))
        ((eq style 'pseudoselector) (output-pseudoselector s slot domain))
        ((eq style 'radiobutton) (format-radiobuttons s slot (find-alternatives slot) value))
        ((eq style 'stringfield) (format-text s slot value 40))
        ((eq style 'text) (format-text s slot value 40))
        ((eq style 'textarea) (format-textarea s slot value 4 40))
        ((eq style 'datestyle) (output-datestyle s slot value))
        ((eq style 'dollarstyle) (output-dollarstyle s slot value 6))
        ((eq style 'prettystyle) (output-simple s value))
        ((eq style 'glyph) (output-handle s value))
        (t (format-text s (stringize slot) (stringize value) 40))))

(defun output-fastchange-cell (s style slot domain value)
  (cond ((eq style 'selector) (output-selector s slot (find-alternatives slot) value))
        ((eq style 'combobox) (output-combobox s slot domain value 40))
        ((eq style 'pseudoselector) (output-pseudoselector s slot domain))
        ((eq style 'radiobutton) (format-radiobuttons s slot (find-alternatives slot) value))
        ((eq style 'stringfield) (format-text s slot value 40))
        ((eq style 'text) (format-text s slot value 40))
        ((eq style 'textarea) (format-textarea s slot value 4 40))
        ((eq style 'datestyle) (output-datestyle s slot value))
        ((eq style 'dollarstyle) (output-dollarstyle s slot value 6))
        ((eq style 'prettystyle) (output-simple s value))
        ((eq style 'glyph) (output-handle s value))
        (t (format-text s (stringize slot) (stringize value) 40))))

;;;;


(defmethod process-fastinspectpage (s object class)
  (format-html s) (crlf s)
  (output-head s (format nil "Inspect ~A" (prettify object))) (crlf s)
  (format-body s *bgcolor*) (crlf s)
  (output-header s)
  (format s "<div style='margin-left:10px; margin-right:10px'>") (crlf s)
  (output-fastinspect s object class)
  (output-fastinspect-commands s object class)
  (format s "</div>")
  (output-footer s)
  (finish-body s) (crlf s)
  (finish-html s) (crlf s))

(defun output-fastcreate (s structure)
  (let ((*buttons* 0))
    (format s "<form action='fastcreatepage?' method='post' name='form1'>")
    (output-fastcreate-structure s structure)
    (format-button s "Command" "Create")
    (format s " this ~A." (prettify (cadr structure))) (crlf s)
    (format s "</form>")))
(defun output-fastchange (s structure)
  (let ((*buttons* 0))
    (format s "<form action='fastchangepage?' method='post' name='form1'>")
    (output-fastchange-structure s structure)
    (format-button s "Command" "Record")
    (format s " changes.")
    (format s "</form>") (crlf s)
    'done))

;;;;


(defun computeinstances (structure source)
  (asks (car structure) (maksand (fastconverter structure)) nil source))

;;;;


(defmethod findinstances (structure (source factserver))
  (cond ((atom structure) (factinstance structure source))
        ((every #'(lambda (pair) (null (cdr pair))) (cddr structure))
         (factinstance (cadr structure) source))
        (t (car (factfindinstances structure source)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod findinstances (structure (source factserver))
  (factfindinstances structure source))

(defun factfindinstances (structure source)
  (do ((l (cddr structure) (cdr l)) (items) (flag))
      ((null l)
       (cond ((eq (cadr structure) 'thing) items)
             (flag
              (factfilteritems items (find-predicate (cadr structure)) source))
             (t (factinstances (cadr structure) source))))
      (cond ((null (cdar l)))
            ((triplep 'searchstyle (caar l) 'text *manager*)
             (if flag (setq items (textfilterobjects items (caar l) (cadar l) source))
                 (setq items (textobjects (caar l) (cadar l) source) flag t)))
            (flag (setq items (factfindfilterobjects items (caar l) (cadar l) source)))
            (t (setq items (factfindobjects (caar l) (cadar l) source) flag t)))))

(defun factfindobjects (slot value source)
  (cond ((atom value) (factobjects slot value source))
        (t (do ((l (factfindinstances value source) (cdr l)) (nl))
               ((null l) nl)
               (setq nl (union* nl (factobjects slot (car l) source)))))))

(defun factfindfilterobjects (items slot value source)
  (cond ((atom value) (factfilterobjects items slot value source))
        (t (do ((l items (cdr l)) (nl))
               ((null l) (nreverse nl))
               (dolist (value (factfindinstances value source))
                 (when (triplep slot (car l) value source)
                   (setq nl (cons (car l) nl))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defmethod findinstance (structure (source dualserver))
  (cond ((atom structure) (dualinstance structure source))
        ((every #'(lambda (pair) (null (cdr pair))) (cddr structure))
         (dualinstance (cadr structure) source))
        (t (car (dualfindinstances structure source)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod findinstances (structure (source dualserver))
  (cond ((atom structure) (dualinstances structure source))
        ((every #'(lambda (pair) (null (cdr pair))) (cddr structure))
         (dualinstances (cadr structure) source))
        (t (dualfindinstances structure source))))

(defun dualfindinstances (structure source)
  (do ((l (cddr structure) (cdr l)) (items) (flag))
      ((null l)
       (if flag
         (dualfilteritems items (find-predicate (cadr structure)) source)
         (dualinstances (cadr structure) source)))
      (cond ((null (cdar l)))
            ((triplep 'searchstyle (caar l) 'text *manager*)
             (if flag (setq items (dualtextfilterobjects items (caar l) (cadar l) source))
                 (setq items (dualtextobjects (caar l) (cadar l) source) flag t)))
            (flag (setq items (dualfindfilterobjects items (caar l) (cadar l) source)))
            (t (setq items (dualfindobjects (caar l) (cadar l) source) flag t)))))

(defun dualfindobjects (slot value source)
  (cond ((atom value) (factobjects slot value source))
        (t (do ((l (dualfindinstances value source) (cdr l)) (nl))
               ((null l) nl)
               (setq nl (union* nl (dualobjects slot (car l) source)))))))

(defun dualfindfilterobjects (items slot value source)
  (cond ((atom value) (dualfilterobjects items slot value source))
        (t (do ((l items (cdr l)) (nl))
               ((null l) (nreverse nl))
               (dolist (value (dualfindinstances value source))
                 (when (triplep slot (car l) value source)
                   (setq nl (cons (car l) nl))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


  function makeURL (url)
   {var host = location.host;
    var port = location.port;
    if (!port) port = '80';
    return 'http://' + host + ':' + port + url}

;;;;


  function postMessage (url,args)
    {http_request = false;
     http_request = new XMLHttpRequest();
     if (http_request.overrideMimeType)
        {http_request.overrideMimeType('text/xml')};
     if (!http_request)
        {alert('Giving up: Cannot create an XMLHTTP instance');
         return false};
     http_request.onreadystatechange = alertMessage;
     http_request.open('POST', url, true);
     http_request.send(args)}

  function alertMessage ()
    {if (http_request.readyState == 4)
        {if (http_request.responseText)
            {return true}
         else {alert('There was a problem with the request.')}}}

;;;;


(defmethod output-fastcreate-cell (s object class slot value (style (eql 'pseudoselector)))
  (declare (ignore object value))
  (output-pseudoselector s slot class))

(defmethod output-fastchange-cell (s object class slot value (style (eql 'pseudoselector)))
  (declare (ignore object value))
  (output-pseudoselector s slot class))

(defun output-pseudoselector (s slot domain)
  (cond ((askp `(and ,(list (find-predicate domain) '?x) ,(list slot '?x '?*)) nil *gui*)
         (output-text s slot "" 30)
         (format s "<span style='cursor:pointer;text-decoration:underline;color:#FF0000'
                                 onClick=\"selectvalue('~A','~A')\">View Options</span>"
                 slot (stringize domain)))
        (t (output-text s slot "" 40))))

;;;;


(defun textobjects (slot value source)
  (do ((l (indexees slot source) (cdr l)) (nl))
      ((null l) (nreverse (uniquify nl)))
      (when (and (eq (caar l) slot) (substringp value (caddar l)))
        (setq nl (cons (cadar l) nl)))))

;;;;


(defmethod findoptions (attribute structure (source factserver))
  (factfindoptions attribute structure source))

(defun factfindoptions (slot structure source)
  (do ((l (findinstances (screen slot structure) source) (cdr l)) (nl))
      ((null l) (nreverse nl))
      (do ((m (indexees (car l) source) (cdr m)))
          ((null m))
          (when (and (eq (caar m) slot) (equalp (cadar m) (car l)))
            (setq nl (adjoin (caddar m) nl :test #'equalp))))))

;;;;


function mouseclick ()
 {var ev = window.event;
  if (ev.shiftKey == 1 && ev.target.id)
     {window.open('fastchange?object=' + ev.target.id,null,'height=480,width=480'); ev.preventDefault(); return false}}

function click (obj)
 {var ev = window.event;
  var xpos = (screen.width-640)/2;
  var height = screen.height-200;
  if (ev.shiftKey == 1)
     {window.open('fastchangepage?object=' + obj,null,'height='+height+',width=640,left='+xpos+',top=100,scrollbars=1')}
  else {window.open('inspect?object=' + obj,null,'height=480,width=480')}}

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; keysearch
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod process (s (file (eql 'keysearch)) postlines)
  (let (query answers)
    (setq query (getf-post "String" postlines))
    (cond ((string= query "")
           (html-message s (format nil "Please specify a substring to retrieve.")))
          ((and (setq answers (readwords query))
                (setq answers (askassociates answers *gui*)))
           (cond ((null (cdr answers)) (output-inspect s (caar answers) (cadar answers)))
                 (t (output-selection s answers))))
          (t (html-message s (format-warning nil "No matching items of known type."))))))

(defmethod askassociates (x receiver)
  (sendone `(askassociates ,x) receiver))

(defmethod askassociates (x (receiver symbol))
  (cond ((and (boundp receiver) (not (symbolp (symbol-value receiver))))
         (askassociates x (symbol-value receiver)))
        (t (call-next-method x receiver))))

(defmethod askassociates (x (receiver dataserver))
  (cond ((atom x) (associates x receiver))
        (t (do ((l (cdr x) (cdr l)) (nl (associates (car x) receiver)))
               ((null l) (nreverse nl))
               (setq nl (intersect nl (associates (car l) receiver)))))))

(defun sendone (msg receiver)
  (let ((target (find-target (name receiver))))
    (when target
      ;(drop `(datarepository ,(name receiver) ,target) *manager*) ; round robin
      ;(save `(datarepository ,(name receiver) ,target) *manager*) ; round robin
      (request msg *receiver* target))))

(defun sendall (msg receiver)
  (let (targets updates errors)
    (setq targets (find-targets (name receiver)))
    (setq updates (changes (maksand (cdr msg)) (car targets)))
    (dolist (item updates)
      (when (and (listp item) (eq (car item) 'error))
        (setq errors (cons (caddr item) errors))))
    (cond (errors)
          ((atom msg))
          (t (setq msg (list (car msg) (maksand updates)))
             (dolist (target targets) (request msg *sender* target))
             'done))))

#|
(defmethod associates (x th)
  (do ((l (indexees x th) (cdr l)) (nl))
      ((null l) (nreverse nl))
      (cond ((atom (car l)))
            ((eq (caar l) x))
            ((eq (cadar l) x)
             (when (cddar l) (setq nl (cons (caddar l) nl))))
            ((eq (caddar l) x) (setq nl (cons (cadar l) nl))))))
|#

(defun aprolist (s)
  (do ((l (readstrings s) (cdr l)) (nl))
      ((null l) (nreverse nl))
      (do ((m (apropos-list (string-upcase (car l))) (cdr m)) (nm))
          ((null m) (when nm (setq nl (cons (nreverse nm) nl))))
          (cond ((varp (car m)))
                ((request `(ask-if ,(makisa (car m) '?y *gui*)) *client* *gui*)
                 (setq nm (cons (car m) nm)))))))

(defun readwords (s)
  (do ((beg 0) (pos (findspace s 0) (findspace s beg)) (dum) (nl))
      ((null pos) (nreverse nl))
      (setq dum (remove-if-not #'normalp (subseq s beg pos)))
      (unless (string= dum "") (setq nl (cons (read-from-string dum) nl)))
      (setq beg (1+ pos))))

(defun readstrings (s)
  (do ((beg 0) (pos (findspace s 0) (findspace s beg)) (dum) (nl))
      ((null pos) (nreverse nl))
      (setq dum (remove-if-not #'normalp (subseq s beg pos)))
      (unless (string= dum "") (setq nl (cons dum nl)))
      (setq beg (1+ pos))))

(defun findspace (s b)
  (cond ((>= b (length s)) nil)
        ((position #\space s :start b))
        (t (length s))))

(defun normalp (c)
  (or (char= c #\space) (char= c #\.) (alphanumericp c)))

;;;;

(defun process-search-pipe (s postlines)
  (let (structure target pipe)
    (multiple-value-setq (structure postlines) (parsestructure postlines))
    (setq target (read-user-string (getf-post "Target" postlines)))
    (setq pipe (pipe structure *gui* target *client* *gui*))
    (format-html s) (crlf s)
    (output-head s "Pipe") (crlf s)
    (format-body s *bgcolor*) (crlf s)
    (output-header s)
    (format s "<P>~A created.</P>" (prettify pipe))
    (output-footer s)
    (finish-body s) (crlf s)
    (finish-html s) (crlf s)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; keysearch
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod process (s (file (eql 'keysearch)) postlines)
  (let (query answers)
    (setq query (getf-post "String" postlines))
    (cond ((string= query "")
           (html-message s (format nil "Please specify a substring to retrieve.")))
          (t (do ((l (readstrings query) (cdr l)) (dum) (nl))
                 ((null l) (setq answers (nreverse nl)))
                 (setq dum (apropos-list (string-upcase (car l))))
                 (setq nl (cons (remove-if #'varp dum) nl)))
             (setq answers (relatives answers (find-classes *gui*) *warehouse*))
             (cond ((null answers)
                    (html-message s (format-warning nil "No matching items.")))
                   ((null (cdr answers))
                    (output-inspect s (caar answers) (cadar answers)))
                   (t (output-selections s answers)))))))

(defun relatives (possibilities classes th)
  (do ((i 0 (1+ i)) (dum))
      ((> i 4) nil)
      (setq possibilities (affiliates possibilities th))
      (when (and (setq dum (intersections possibilities))
                 (setq dum (classifyem dum classes)))
        (return dum))))

(defun classifyem (items classes)
  (declare (ignore classes))
  (request `(ask-all (?x ?y) (and (oneof ?x . ,items)
                                  ;(oneof ?y . ,classes)
                                  ,(makisa '?x '?y *gui*)))
           *client* *gui*))

(defun relatives (possibilities classes th)
  (declare (ignore classes))
  (do ((i 0 (1+ i)) (dum))
      ((> i 4) nil)
      (cond ((setq dum (intersections possibilities)) (return (filterem dum)))
            (t  (setq possibilities (affiliates possibilities th))))))

(defun filterem (possibilities)
  (do ((l possibilities (cdr l)) (nl))
      ((null l) (nreverse nl))
      (when (nodep (car l) *warehouse*) (setq nl (cons (car l) nl)))))

(defun nodep (x th)
  (do ((l (indexees x th) (cdr l)))
      ((null l) nil)
      (when (and (listp (car l)) (find x (cdar l) :test #'equalp)) (return t))))

(defun affiliates (possibilities th)
  (do ((l possibilities (cdr l)) (nl))
      ((null l) (nreverse nl))
      (setq nl (cons (associates (car l) th) nl))))

(defun associates (options th)
  (do ((l options (cdr l)) (nl))
      ((null l) (hashway (nconc options (nreverse nl))))
      (setq nl (nreconc (neighbors (car l) th) nl))))

(defun neighbors (x th)
  (do ((l (indexees x th) (cdr l)) (nl))
      ((null l) (nreverse nl))
      (cond ((atom (car l)))
            ((eq (caar l) x))
            ;((eq (cadar l) x)
            ; (when (and (cddar l) (symbolp (caddar l))) (setq nl (cons (caddar l) nl))))
            ((eq (caddar l) x)
             (when (symbolp (cadar l)) (setq nl (cons (cadar l) nl)))))))

(defun intersections (lists)
  (cond ((null lists) nil)
        ((null (cdr lists)) (car lists))
        (t (apply #'smartintersection* lists))))

(defun smartintersection* (&rest lists)
  (let (dum)
    (dolist (x (car lists)) (remprop x 'mark))
    (dolist (l (cdr lists))
      (dolist (x l)
        (setq dum (get x 'mark))
        (setf (get x 'mark) (if (integerp dum) (1+ dum) 1))))
    (setq dum (length (cdr lists)))
    (do ((l (car lists) (cdr l)) (nl))
        ((null l) (nreverse nl))
        (when (eql (get (car l) 'mark) dum) (setq nl (cons (car l) nl))))))

(defun output-selections (s answers)
  (format-html s) (crlf s)
  (output-head s "Choose Object") (crlf s)
  (format-body s *bgcolor*) (crlf s)
  (output-header s)
  (setq answers (sort answers #'lessp))
  (format s "<P>Select an item from the following list.")
  (format s "<P><TABLE>")
  (dolist (answer answers)
    (format s "<TR><TD><A HREF=\"Inspect?Object=~A\">~A</A></TD></TR>"
            (addressify answer) (prettify answer)))
  (format s "</TABLE>")
  (output-footer s)
  (finish-body s) (crlf s)
  (finish-html s) (crlf s))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; pipe
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod pipe (structure source target sender (receiver interface))
  (pipe structure source target sender (get-recipient receiver)))

(defun createpipe (source target class owner rule)
  (let (pipe)
    (cond ((findx '?x `(and (pipe.source ?x ,source)
                            (pipe.target ?x ,target)
                            (pipe.subject ?x ,class)
                            (pipe.rule ?x ,rule)) *manager*))
          (t (setq pipe (gentemp "PIPE."))
             (save `(isa ,pipe pipe) *manager*)
             (save `(pipe.source ,pipe ,source) *manager*)
             (save `(pipe.target ,pipe ,target) *manager*)
             (save `(pipe.subject ,pipe ,class) *manager*)
             (save `(pipe.owner ,pipe ,owner) *manager*)
             (save `(pipe.rule ,pipe ,rule) *manager*)
             pipe))))

(defun deletepipe (pipe)
  (dropall `(recipient ?x ?y) `(and (pipe.source ,pipe ?x) (recipient ?x ?y)) *manager*)
  (do ((l (finds '?x `(pipe.rule ,pipe ?x) *manager*) (cdr l))
       (source (findx '?x `(pipe.source ,pipe ?x) *manager*)))
      ((null l) (kill pipe *manager*) 'done)
      (drop (car l) source)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; call
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod process (s (command (eql 'call)) postlines)
  (cond ((null postlines) (http-problem s "No command specified."))
        (t (process-call s postlines))))

(defun process-call (s postlines)
  (let (action args result error)
    (setq action (read-value-string (cdar postlines)))
    (setq args (get-arguments (cdr postlines)))
    (multiple-value-setq (result error) (apply action args))
    (format-html s) (crlf s)
    (output-head s "Result") (crlf s)
    (format-body s *bgcolor*) (crlf s)
    (output-header s)
    (format s "<PRE>")
    (cond (error (format s "Error."))
          (t (print-acl s result)))
    (format s "</PRE>")
    (output-footer s)
    (finish-body s) (crlf s)
    (finish-html s) (crlf s)))

(defun get-arguments (postlines)
  (do ((l postlines (cdr l)) (nl))
      ((null l) (nreverse nl))
      (setq nl (cons (read-value-string (cdar l)) nl))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; dispatcher
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod process (s (file (eql 'dispatcher)) postlines)
  (cond ((null (cdr postlines)) (process-dispatcher-start s))
        (t (process-dispatcher-revision s postlines))))

(defun process-dispatcher-start (s)
  (declare (ignore postlines))
  (let (tree)
    (setq tree (cons 'top (find-actions *gui*)))
    (format-html s) (crlf s)
    (output-head s "Select Action") (crlf s)
    (format-body s *bgcolor*) (crlf s)
    (output-header s)
    (format s "<FORM ACTION=DISPATCHER? METHOD=POST>")
    (format-hidden s "Tree" tree)
    (format s "<DL>")
    (dolist (item (cdr tree))
      (format s "<DD>")
      (output-action s item))
    (format s "</DL>")
    (format s "</FORM>")
    (output-apropos s)
    (output-footer s)
    (finish-body s) (crlf s)
    (finish-html s) (crlf s)))

(defun process-dispatcher-revision (s postlines)
  (let (tree command)
    (setq tree (read-user-string (cdr (pop postlines))))
    (setq command (caar postlines))
    (setq command (read-user-string (subseq command 0 (- (length command) 2))))
    (setq tree (toggleaction command tree))
    (format-html s) (crlf s)
    (output-head s "Select Action") (crlf s)
    (format-body s *bgcolor*) (crlf s)
    (output-header s)
    (format s "<FORM ACTION=DISPATCHER? METHOD=POST>")
    (format-hidden s "Tree" tree)
    (format s "<DL>")
    (dolist (item (cdr tree))
      (format s "<DD>")
      (output-action s item))
    (format s "</DL>")
    (format s "</FORM>")
    (output-apropos s)
    (output-footer s)
    (finish-body s) (crlf s)
    (finish-html s) (crlf s)))

(defun find-actions (interface)
  (finds '?c `(rootaction ,interface ?c) *interface*))

(defun find-visible-subaction (class)
  (findx '?c `(superaction ?c ,class) *interface*))

(defun find-visible-subactions (class)
  (finds '?c `(superaction ?c ,class) *interface*))

(defun toggleaction (action tree)
  (cond ((eq action tree) (cons action (find-visible-subactions action)))
        ((atom tree) tree)
        ((eq action (car tree)) action)
        (t (do ((l tree (cdr l)) (nl))
               ((null l) (nreverse nl))
               (setq nl (cons (toggleaction action (car l)) nl))))))

(defun output-action (s tree)
  (cond ((atom tree)
         (if (find-visible-subaction tree) (output-closed-action s tree)
             (output-lone-action s tree)))
        (t (format s "<DL>")
           (format s "<DT>")
           (output-open-action s (car tree))
           (dolist (item (cdr tree))
             (format s "<DD>")
             (output-action s item))
           (format s "</DL>"))))

(defun output-open-action (s action)
  (format s "<INPUT TYPE=IMAGE NAME=~A SRC=\"~Aimages/green.gif\" BORDER=0>" action *home*)
  (format s " <A HREF=\"~A?\">~A</A>"
          (addressify action) (iconify action)))

(defun output-lone-action (s action)
  (format s "<IMG SRC=\"~Aimages/grey.gif\">" *home*)
  (format s " <A HREF=\"~A?\">~A</A>"
          (addressify action) (iconify action)))

(defun output-closed-action (s action)
  (format s "<INPUT TYPE=IMAGE NAME=~A SRC=\"~Aimages/red.gif\" BORDER=0>" action *home*)
  (format s " <A HREF=\"~A?\">~A</A>"
          (addressify action) (iconify action)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Analyze
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod process (s (file (eql 'analyze)) postlines)
  (format-html s) (crlf s)
  (output-head s "Analyze Query") (crlf s)
  (format-body s *bgcolor*) (crlf s)
  (output-header s)
  (let ((aspect (read-from-post-data "ASPECT" postlines)) query)
    (setq query (read-user-string (getf-post "QUERY" postlines)))
    (format s "<P>The query")
    (output-kif s (list 'ask-all aspect query))
    (format s "resulted in zero matches.<P><HR>")
    (unless (find (first query) '(and or))
      (format s "<P>Further analysis is possible only for compound (AND/OR) queries.")
      (output-footer s)
      (finish-body s) (crlf s)
      (finish-html s) (crlf s)
      (return-from process))
    (format s "<DL>")
    (loop
      for conjunct in (rest query)
      for num =
      (request `(length (ask-all ,aspect ,conjunct)) *client* *agent*)
      do (format s "<DT>")
      (output-kif s conjunct)
      (if (numberp num)
        (format s "<DD> ...matches ~D object~:P." num)
        (format s "<DD> ...matches no objects.")))
    (format s "</DL>"))
  (output-footer s)
  (finish-body s) (crlf s)
  (finish-html s) (crlf s))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Assert
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod process (s (command (eql 'assert)) postlines)
  (let (command)
    (cond ((null postlines) (process-assert-start s))
          ((and (setq command (getf-post "Command" postlines)) nil))
          ((equalp command "Assert") (process-assert-assert s postlines))
          ((equalp command "Retract") (process-assert-retract s postlines))
          ((equalp command "Trace Assertion") (process-assert-execution s postlines))
          (t (http-problem s "Obsolete form.")))))

(defun process-assert-start (s)
  (format-html s) (crlf s)
  (output-head s "Edit sentences") (crlf s)
  (format-body s *bgcolor*) (crlf s)
  (output-header s)
  (format s "<FORM ACTION=\"ASSERT?\" METHOD=\"POST\">")
  (format s "<TEXTAREA NAME=\"Sentences\" ROWS=\"10\" COLS=\"70\"></TEXTAREA>~%")
  
  (format s "<TABLE WIDTH=100%>~%")
  (format s "<TR><TD WIDTH=50% VALIGN=\"TOP\">~%")
  (format-button s "Command" "Assert")
  (format s "<BR/>")
  (format-button s "Command" "Retract")
  (format s "</TD><TD>")
  (format-button s "Command" "Trace Assertion")
  (format s "</TD></TR>")
  (format s "</TABLE>")
  (format s "</FORM>")
  (output-footer s)
  (finish-body s) (crlf s)
  (finish-html s) (crlf s))

(defun process-assert-assert (s postlines)
  (let (sentences error)
    (multiple-value-setq (sentences error)
      (read-sentences (getf-post "Sentences" postlines)))
    (cond (error (http-problem s "Syntax error."))
          (t (format-html s) (crlf s)
             (output-head s "Result") (crlf s)
             (format-body s *bgcolor*) (crlf s)
             (output-header s)
             (dolist (sentence sentences) (prorequest `(tell ,sentence)))
             (format s "<P>Done.<P>")
             (output-footer s)
             (finish-body s) (crlf s)
             (finish-html s) (crlf s)))))

(defun process-assert-execution (*stream* postlines)
  (let (sentences error)
    (multiple-value-setq (sentences error)
      (read-sentences (getf-post "Sentences" postlines)))
    (cond (error (http-problem *stream* "Syntax error."))
          (t (format-html *stream*) (crlf *stream*)
             (output-head *stream* "Assert Execution Trace") (crlf *stream*)
             (format-body *stream* *bgcolor*) (crlf *stream*)
             (output-header *stream*)
             (force-output *stream*)
             (format *stream* "<XMP>~%")
             (let ((*trace-device* *stream*) (traceexpressions))
               (trace-expression '?x)
               (dolist (sentence sentences) (prorequest `(tell ,sentence)))
               (untrace-expression '?x))
             (format *stream* "</XMP>~%")
             (format *stream* "<HR/>")
             (format *stream* "<P>Done.<P>")
             (output-footer *stream*)
             (finish-body *stream*) (crlf *stream*)
             (finish-html *stream*) (crlf *stream*)))))

(defun process-assert-retract (s postlines)
  (let (sentences error)
    (multiple-value-setq (sentences error)
      (read-sentences (getf-post "Sentences" postlines)))
    (cond (error (http-problem s "Syntax error."))
          (t (format-html s) (crlf s)
             (output-head s "Result") (crlf s)
             (format-body s *bgcolor*) (crlf s)
             (output-header s)
             (dolist (sentence sentences) (prorequest `(untell ,sentence)))
             (format s "<P>Done.<P>")
             (output-footer s)
             (finish-body s) (crlf s)
             (finish-html s) (crlf s)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; table
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod process (s (file (eql 'table)) postlines)
  (let (command)
    (cond ((null postlines) (output-empty-table s))
          ((null (cdr postlines)) (output-table s (cdar postlines)))
          ((and (setq command (getf-post "Command" postlines)) nil))
          ((string= command " Count ") (count-table s postlines))
          ((string= command "Display") (answer-table s postlines))
          ((string= command "  Save  ") (rulify-table s postlines))
          ((string= command "  Drop  ") (derulify-table s postlines))
          ((string= command "Add") (add-table s postlines))
          ((string= command "Edit") (edit-table s postlines))
          ((string= command "Convert to SQL Query") (sql-table s postlines))
          ((string= command "Convert to ACL Query") (acl-table s postlines))
          (t (http-problem s "Obsolete form.")))))

(defun output-empty-table (s)
  (let ((*var-count* 0))
    (format-html s) (crlf s)
    (output-head s "QBE Query") (crlf s)"
    (format-body s *bgcolor*) (crlf s)
    (output-header s)
    (format s "<FORM ACTION=TABLE? METHOD=POST>")
    (format s "Find ")
    (format-text s "Variables" "" 32)
    (format s " such that<BR><P>")
    (format-hidden s "Begingroup" "AND")
    (format-hidden s "Endgroup" "")
    (format s "<P>")
    (output-table-commands s)
    (format s "</FORM>")
    (output-footer s)
    (finish-body s) (crlf s)
    (finish-html s) (crlf s)))

(defun output-table (s query)
  (let ((relation (read-value-string query)) (*var-count* 0))
    (format-html s) (crlf s)
    (output-head s "QBE Query") (crlf s)
    (format-body s *bgcolor*) (crlf s)
    (output-header s)
    (format s "<FORM ACTION=TABLE? METHOD=POST>")
    (format s "Find ")
    (format-text s "Variables" "" 32)
    (format s " such that<BR><P>")
    (format-hidden s "Begingroup" "AND")
    (output-subtable s relation)
    (format-hidden s "Endgroup" "")
    (format s "<P>")
    (output-table-commands s)
    (format s "</FORM>")
    (output-footer s)
    (finish-body s) (crlf s)
    (finish-html s) (crlf s)))

(defun derulify-table (s *postlines*)
  (let (vars kif relation (*var-count* 0))
    (format-html s) (crlf s)
    (output-head s "Save Rule") (crlf s)
    (format-body s *bgcolor*) (crlf s)
    (output-header s)
    (setq vars (cdr (pop *postlines*)))
    (setq kif (question-group))
    (setq vars (construct-aspect vars kif))
    (setq kif (rectify kif))
    (setq relation (read-value-string (getf-post "Relation" *postlines*)))
    (cond ((not relation) (format s "<P>Bad relation name.<P>"))
          (t (dolist (rule (brfs `(<= ,(cons relation vars) ,kif)))
               (drop rule *library*))
           (format s "<P>Rule added to library.<P>")))
    (output-footer s)
    (finish-body s) (crlf s)
    (finish-html s) (crlf s)))

(defun edit-table (s *postlines*)
  (let (kif (*var-count* 0))
    (format-html s) (crlf s)
    (output-head s "QBE Query") (crlf s)
    (format-body s *bgcolor*) (crlf s)
    (output-header s)
    (format s "<FORM ACTION=TABLE? METHOD=POST>")
    (format s "Find ")
    (format-text s "Variables" (cdr (pop *postlines*)) 32)
    (format s " such that<BR><P>")
    (setq kif (question-group))
    (generate-table s kif)
    (format s "<P>")
    (reissue-table-commands s *postlines*)
    (format s "</FORM>")
    (output-footer s)
    (finish-body s) (crlf s)
    (finish-html s) (crlf s)))

(defun generate-table (s x)
  (cond ((atom x))
        ((eq 'unprovable (car x)) (generate-atom s (cadr x) nil))
        ((eq 'not (car x)) (generate-atom s (cadr x) nil))
        ((and (eq 'and (car x)) (cddr x)) (generate-and s x))
        ((and (eq 'or (car x)) (cddr x)) (generate-or s x))
        (t (generate-atom s x t))))

(defun output-subtable (s relation)
  (let ((arity (arity-of-relation relation)))
    (cond ((> arity 4) (output-column-table s relation arity))
          (t (output-row-table s relation arity)))))

(defun output-column-table (s relation arity)
  (format-hidden s "Start" "")
  (format-hidden s "Style" "Table")
  (format-hidden s "Relation" (stringize relation))
  (format-checkbox s "Not" "" nil)
  (format s "<B>~A</B><BR>" (prettify relation))
  (format s "<TABLE BORDER>")
  (do ((i 1 (1+ i)) (columns (columns relation) (cdr columns)))
      ((> i arity))
      (format s "<TR><TH ALIGN=LEFT>")
      (format s (prettify (or (car columns) "")))
      (format s "</TH>")
      (format s "<TD>")
      (format-text s "Val" (stringize (specvar (or (car columns) i))) 30)
      (format s "</TD></TR>"))
  (format s "</TABLE>")
  (format-hidden s "End" ""))

(defun output-row-table (s relation arity)
  (let ((columns (columns relation)))
    (format-hidden s "Start" "")
    (format-hidden s "Style" "Table")
    (format-hidden s "Relation" (stringize relation))
    (format s "<B>~A</B><BR>" (prettify relation))
    (format s "<TABLE BORDER>")
    (format s "<TR>")
    (format s "<TD></TD>")
    (format s "</TD>")
    (do ((i 1 (1+ i)) (columns columns (cdr columns)))
        ((> i arity))
        (format s "<TH>")
        (format s (prettify (or (car columns) "")))
        (format s "</TH>"))
    (format s "</TR>")
    (format s "<TR>")
    (format s "<TD>")
    (format-checkbox s "Not" "" nil)
    (format s "</TD>")
    (do ((i 1 (1+ i)) (columns columns (cdr columns)))
        ((> i arity))
        (format s "<TD>")
        (format-text s "Val" (stringize (specvar (or (car columns) i))) 20)
        (format s "</TD>"))
    (format s "</TR>")
    (format s "</TABLE>")
    (format-hidden s "End" "")))

(defun output-table-commands (s)
  (let ((relations (sort (find-relations) #'lessp)))
    (format s "<HR>~%")
    (format-hidden s "== Commands ==" "")
    (format s "<TABLE WIDTH=100%>~%")
    (format s "<TR><TD WIDTH=50% VALIGN=\"TOP\">~%")
    (unless (findp `(nocommand ,*gui* count) *interface*)
      (format-button s "Command" " Count ")
      (format s " all answers for query"))
    (format s "</TD><TD>")
    (unless (findp `(nocommand ,*gui* add) *interface*)
      (format-button s "Command" "Add")
      (format s "<SELECT NAME=\"Addition\">~%")
      (format s "<OPTION SELECTED>~A~%" (stringize (car relations)))
      (dolist (relation (cdr relations))
        (format s "<OPTION>~A~%" (stringize relation)))
      (format s "</SELECT>"))
    (format s "</TD></TR><TR><TD>")
    (unless (findp `(nocommand ,*gui* display) *interface*)
      (format-button s "Command" "Display")
      (format s " answers ")
      (format-text s "Start" "1" 5)
      (format s " through ")
      (format-text s "Solutions" "20" 5))
    (format s "</TD><TD>")
    (unless (findp `(nocommand ,*gui* sql) *interface*)
      (format-button s "Command" "Convert to SQL Query"))
    (format s "</TD></TR><TR><TD>")
    (unless (findp `(nocommand ,*gui* rulify) *interface*)
      (format-button s "Command" "  Save  ")
      (format s "rule for relation")
      (format-text s "Relation" "" 20))
    (format s "</TD><TD>")
    (unless (findp `(nocommand ,*gui* acl) *interface*)
      (format-button s "Command" "Convert to ACL Query"))
    (format s "</TD></TR></TABLE>~%")))

(defun reissue-table-commands (s postlines)
  (let ((start (or (getf-post "Start" postlines) "1"))
        (solutions (or (getf-post "Solutions" postlines) "20"))
        (relation (or (getf-post "Relation" postlines) ""))
        (relations (sort (find-relations) #'lessp)))
    (format s "<HR>~%")
    (format-hidden s "== Commands ==" "")
    (format s "<TABLE WIDTH=100%>~%")
    (format s "<TR><TD WIDTH=50% VALIGN=\"TOP\">~%")
    (unless (findp `(nocommand ,*gui* count) *interface*)
      (format-button s "Command" " Count ")
      (format s " all answers for query"))
    (format s "</TD><TD>")
    (unless (findp `(nocommand ,*gui* add) *interface*)
      (format-button s "Command" "Add")
      (format s "<SELECT NAME=\"Addition\">~%")
      (format s "<OPTION SELECTED>~A~%" (stringize (car relations)))
      (dolist (relation (cdr relations))
        (format s "<OPTION>~A~%" (stringize relation)))
      (format s "</SELECT>"))
    (format s "</TD></TR><TR><TD>")
    (unless (findp `(nocommand ,*gui* display) *interface*)
      (format-button s "Command" "Display")
      (format s " answers ")
      (format-text s "Start" start 5)
      (format s " through ")
      (format-text s "Solutions" solutions 5))
    (format s "</TD><TD>")
    (unless (findp `(nocommand ,*gui* sql) *interface*)
      (format-button s "Command" "Convert to SQL Query"))
    (format s "</TD></TR><TR><TD>")
    (unless (findp `(nocommand ,*gui* rulify) *interface*)
      (format-button s "Command" "  Save  ")
      (format s "rule for relation")
      (format-text s "Relation" relation 20))
    (format s "</TD><TD>")
    (unless (findp `(nocommand ,*gui* acl) *interface*)
      (format-button s "Command" "Convert to ACL Query"))
    (format s "</TD></TR></TABLE>~%")))

(defun count-table (s *postlines*)
  (let (aspect kif count (*var-count* 0))
    (format-html s) (crlf s)
    (output-head s "Request") (crlf s)
    (format-body s *bgcolor*) (crlf s)
    (output-header s)
    (setq aspect (cdr (pop *postlines*)))
    (setq kif (question-group))
    (setq aspect (construct-aspect aspect kif))
    (setq kif (rectify kif))
    (setq count (request `(length (ask-all ,aspect ,kif)) *client* *agent*))
    (cond ((or (not (numberp count)) (= count 0))
           (display-failure s aspect kif))
          ((= count 1)
           (format s "<P>There is 1 viewable answer.<P>")
           (output-morerows-commands s aspect kif count 0 0))
          (t (format s "<P>There are ~D viewable answers." count)
             (output-morerows-commands s aspect kif count 0 0)))
    (output-footer s)
    (finish-body s) (crlf s)
    (finish-html s) (crlf s)))

(defun answer-table (s *postlines*)
  (let (vars aspect kif results count start end (*var-count* 0))
    (format-html s) (crlf s)
    (output-head s "Result") (crlf s)
    (format-body s *bgcolor*) (crlf s)
    (output-header s)
    (setq vars (cdr (pop *postlines*)))
    (setq kif (question-group))
    (setq start (read-value-string (getf-post "Start" *postlines*)))
    (setq end (read-value-string (getf-post "Solutions" *postlines*)))
    (setq aspect (construct-aspect vars kif))
    (setq results (request `(ask-all ,aspect ,kif) *client* *agent*))
    (setq count (length results))
    (cond ((not (integerp start)) (setq start 1))
          ((< start 1) (setq start 1)))
    (cond ((not (integerp end)) (setq end count))
          ((< end 1) (setq end (min count 20)))
          ((> end count) (setq end count)))
    (when (< end start) (setq end start))
    (cond ((and (= start 1) (= end count)))
          ((> start count) (setq results nil))
          (t (setq results (subseq results (1- start) end))))
    (cond ((= count 0) (display-none s aspect kif))
          (t (display-result s aspect results count start end)
             (output-morerows-commands s aspect kif count start end)))
    (output-footer s)
    (finish-body s) (crlf s)
    (finish-html s) (crlf s)))

(defun handle-table (s *postlines*)
  (let (vars aspect kif results count start end (*var-count* 0))
    (setq vars (cdr (pop *postlines*)))
    (setq kif (question-group))
    (setq start (read-value-string (getf-post "Start" *postlines*)))
    (setq end (read-value-string (getf-post "Solutions" *postlines*)))
    (setq aspect (construct-aspect vars kif))
    (setq results (request `(ask-all ,aspect ,kif) *client* *agent*))
    (setq count (length results))
    (cond ((not (integerp start)) (setq start 1))
          ((< start 1) (setq start 1)))
    (cond ((not (integerp end)) (setq end count))
          ((< end 1) (setq end (min count 20)))
          ((> end count) (setq end count)))
    (when (< end start) (setq end start))
    (cond ((and (= start 1) (= end count)))
          ((> start count) (setq results nil))
          (t (setq results (subseq results (1- start) end))))
    (cond ((= count 0))
          (t (showtable s aspect results t)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; morerows
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod process (s (file (eql 'morerows)) *postlines*)
  (let (aspect kif start end results count)
    (format-html s) (crlf s)
    (output-head s "Morerows Result") (crlf s)
    (format-body s *bgcolor*) (crlf s)
    (output-header s)
    (setq aspect (read-from-string (cdr (pop *postlines*))))
    (setq kif (read-from-string (cdr (pop *postlines*))))
    (setq start (read-value-string (getf-post "Start" *postlines*)))
    (setq end (read-value-string (getf-post "Solutions" *postlines*)))
    (setq results (request `(ask-all ,aspect ,kif) *client* *agent*))
    (setq count (length results))
    (cond ((not (integerp start)) (setq start 1))
          ((< start 1) (setq start 1)))
    (cond ((not (integerp end)) (setq end count))
          ((< end 1) (setq end (min count 20)))
          ((> end count) (setq end count)))
    (when (< end start) (setq end start))
    (cond ((and (= start 1) (= end count)))
          ((> start count) (setq results nil))
          (t (setq results (subseq results (1- start) end))))
    (cond ((= count 0) (display-failure s aspect kif))
          (t (display-result s aspect results count start end)
             (output-morerows-commands s aspect kif count start end)))
    (output-footer s)
    (finish-body s) (crlf s)
    (finish-html s) (crlf s)))

(defun output-morerows-commands (s aspect kif count start end)
  (cond ((and (= start 1) (= end count)))
        ((> count end) (setq start (1+ end) end (min count (+ end 20))))
        (t (setq start 1 end (min count 20))))
  (format s "<FORM ACTION=MOREROWS? METHOD=POST>")
  (format-hidden s "Aspect" (htmlify (prin1-to-string aspect)))
  (format-hidden s "Kif" (htmlify (prin1-to-string kif)))
  (format-button s "Command" "Display")
  (format s "answers ")
  (format-text s "Start" (princ-to-string start) 5)
  (format s " through ")
  (format-text s "Solutions" (princ-to-string end) 5)
  (format s "</FORM>"))

(defun rulify-table (s *postlines*)
  (let (vars kif relation (*var-count* 0))
    (format-html s) (crlf s)
    (output-head s "Save Rule") (crlf s)
    (format-body s *bgcolor*) (crlf s)
    (output-header s)
    (setq vars (cdr (pop *postlines*)))
    (setq kif (question-group))
    (setq vars (construct-aspect vars kif))
    (setq kif (rectify kif))
    (setq relation (read-value-string (getf-post "Relation" *postlines*)))
    (cond ((not relation) (format s "<P>Bad relation name.<P>"))
          (t (dolist (rule (brfs `(<= ,(cons relation vars) ,kif)))
               (save rule *library*))
           (format s "<P>Rule added to library.<P>")))
    (output-footer s)
    (finish-body s) (crlf s)
    (finish-html s) (crlf s)))

(defun add-table (s *postlines*)
  (let (op kif relation (*var-count* 0))
    (format-html s) (crlf s)
    (output-head s "QBE Query") (crlf s)
    (format-body s *bgcolor*) (crlf s)
    (output-header s)
    (format s "<FORM ACTION=TABLE? METHOD=POST>")
    (format s "Find ")
    (format-text s "Variables" (cdr (pop *postlines*)) 32)
    (format s " such that<BR><P>")
    (setq op (read-value-string (getf-post "Begingroup" *postlines*)))
    (setq kif (question-group))
    (setq relation (getf-post "Addition" *postlines*))
    (generate-table s (append-relation op kif (read-value-string relation)))
    (format s "<P>")
    (reissue-table-commands s *postlines*)
    (format s "</FORM>")
    (output-footer s)
    (finish-body s) (crlf s)
    (finish-html s) (crlf s)))

(defun append-relation (op kif relation)
  (let ((new (cons relation (make-list (arity-of-relation relation)
                                       :initial-element '?*))))
    (cond ((atom kif) (list op kif new))
          ((eq op (car kif)) (nconc kif (list new)))
          ((eq op (car kif)) (nconc kif (list new)))
          (t (list op kif new)))))

(defun sql-table (s *postlines*)
  (let (vars kif (*var-count* 0))
    (format-html s) (crlf s)
    (output-head s "SQL Query") (crlf s)
    (format-body s *bgcolor*) (crlf s)
    (output-header s)
    (format-html s) (crlf s)
    (output-head s "SQL Query") (crlf s)
    (format-body s *bgcolor*) (crlf s)
    (output-header s)
    (setq kif (question-group))
    (format s "<FORM ACTION=SQL? METHOD=POST>~%")
    (format s "<TEXTAREA NAME=\"Message\" ROWS=10 COLS=70>")
    (format s "~A" (sql `(ask-all ,(construct-aspect vars kif) ,kif)))
    (format s "</TEXTAREA><P>~%")
    (format s "<HR>~%")
    (format s "<P><INPUT TYPE=SUBMIT VALUE=Submit> this SQL request")
    (format s "</FORM>")
    (format s "<P>")
    (output-footer s)
    (finish-body s) (crlf s)
    (finish-html s) (crlf s)))

(defun acl-table (s *postlines*)
  (let (vars kif (*var-count* 0))
    (setq vars (cdr (pop *postlines*)))
    (setq kif (question-group))
    (generate-acl s `(ask-all ,(construct-aspect vars kif) ,(rectify kif)))))

(defun question-group ()
  (do ((operator (read-user-string (cdr (pop *postlines*)))) (nl))
      ((or (null *postlines*) (string= (caar *postlines*) "Endgroup"))
       (pop *postlines*)
       (if (null (cdr nl)) (car nl) (cons operator (nreverse nl))))
      (cond ((string= (caar *postlines*) "Begingroup")
             (setq nl (cons (question-group) nl)))
            ((string= (caar *postlines*) "Start")
             (setq nl (cons (question-table) nl)))
            (t (pop *postlines*)))))

(defun question-table ()
  (let (relation flag)
    (pop *postlines*)
    (pop *postlines*)
    (setq relation (read-value-string (cdr (pop *postlines*))))
    (when (string-equal (caar *postlines*) "Not")
      (pop *postlines*)
      (setq flag t))
    (do ((nl))
        ((or (null *postlines*) (string= (caar *postlines*) "End"))
         (pop *postlines*)
         (if (not flag) (cons relation (nreverse nl))
             `(unprovable ,(cons relation (nreverse nl)))))
        (setq nl (cons (question-value (cdr (pop *postlines*))) nl)))))

(defun question-value (x)
  (if (string= x "") '?* (read-value-string x)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; insert
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod process (s (file (eql 'insert)) postlines)
  (let ((name (cdr (pop postlines))))
    (cond ((null postlines) (output-insert s name))
          (t (with-lock-grabbed (*lock*) (insert-tuple s postlines))))))

(defun output-insert (s name)
  (let ((relation (read-value-string name)) (*var-count* 0))
    (format-html s) (crlf s)
    (output-head s "Insert Tuple") (crlf s)
    (format-body s *bgcolor*) (crlf s)
    (output-header s)
    (format s "<FORM ACTION=INSERT? METHOD=POST>")
    (format-hidden s "Relation" (stringize relation))
    (format s "<TABLE>")
    (format-hidden s "Begingroup" "AND")
    (output-updatetable-table s relation)
    (format-hidden s "Endgroup" "")
    (format s "<P>")
    (format s "<HR>")
    (format-button s "Command" "Insert")
    (format s " tuple<BR>")
    (format s "</FORM>")
    (output-footer s)
    (finish-body s) (crlf s)
    (finish-html s) (crlf s)))

(defun insert-tuple (s *postlines*)
  (let (kif (*var-count* 0))
    (format-html s) (crlf s)
    (output-head s "Insert Tuple") (crlf s)
    (format-body s *bgcolor*) (crlf s)
    (output-header s)
    (setq kif (question-group))
    (cond (kif (prorequest `(tell ,kif))
               (format s "<P>Tuple inserted.<P>"))
          (t (format s "<P>Bad tuple.<P>")))
    (output-footer s)
    (finish-body s) (crlf s)
    (finish-html s) (crlf s)))

(defun output-updatetable-table (s relation)
  (let ((arity (arity-of-relation relation)))
    (format-hidden s "Start" "")
    (format-hidden s "Style" "Table")
    (format-hidden s "Relation" (stringize relation))
    (format s "<B>~A</B><BR>" (prettify relation))
    (cond ((> arity 4) (output-update-column s relation arity))
          (t (output-update-row s relation arity)))
    (format-hidden s "End" "")))

(defun output-update-column (s relation arity)
  (format s "<TABLE BORDER>")
  (do ((i 1 (1+ i)) (columns (columns relation) (cdr columns)))
      ((> i arity))
      (format s "<TR><TH ALIGN=LEFT>")
      (format s (prettify (or (car columns) "")))
      (format s "</TH>")
      (format s "<TD>")
      (format-text s "Val" "" 30)
      (format s "</TD></TR>"))
  (format s "</TABLE>"))

(defun output-update-row (s relation arity)
  (let ((columns (columns relation)))
    (format s "<TABLE BORDER>")
    (format s "<TR>")
    (do ((i 1 (1+ i)) (columns columns (cdr columns)))
        ((> i arity))
        (format s "<TH>")
        (format s (prettify (or (car columns) "")))
        (format s "</TH>"))
    (format s "</TR>")
    (format s "<TR>")
    (do ((i 1 (1+ i)) (columns columns (cdr columns)))
        ((> i arity))
        (format s "<TD>")
        (format-text s "Val" "" 20)
        (format s "</TD>"))
    (format s "</TR>")
    (format s "</TABLE>")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; delete
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod process (s (file (eql 'delete)) postlines)
  (let ((name (cdr (pop postlines))))
    (cond ((null postlines) (output-delete s name))
          (t (with-lock-grabbed (*lock*) (delete-tuple s postlines))))))

(defun output-delete (s name)
  (let ((relation (read-value-string name)) (*var-count* 0))
    (format-html s) (crlf s)
    (output-head s "Delete Tuple(s)") (crlf s)
    (format-body s *bgcolor*) (crlf s)
    (output-header s)
    (format s "<FORM ACTION=\"DELETE?\" METHOD=\"POST\">")
    (format-hidden s "Relation" (stringize relation))
    (format s "<TABLE>~%")
    (format-hidden s "Begingroup" "AND")
    (output-updatetable-table s relation)
    (format-hidden s "Endgroup" "")
    (format s "<P>")
    (format s "<HR>")
    (format-button s "Command" "Delete")
    (format s " tuple<BR>")
    (format s "</FORM>")
    (output-footer s)
    (finish-body s) (crlf s)
    (finish-html s) (crlf s)))

(defun delete-tuple (s *postlines*)
  (let (kif (*var-count* 0))
    (format-html s) (crlf s)
    (format-html s) (crlf s)
    (output-head s "Delete Tuple(s)") (crlf s)
    (format-body s *bgcolor*) (crlf s)
    (output-header s)
    (format-body s *bgcolor*) (crlf s)
    (output-header s)
    (setq kif (question-group))
    (cond (kif (prorequest `(untell ,kif))
               (format s "<P>Tuple(s) deleted.<P>"))
          (t (format s "<P>Bad tuple.<P>")))
    (output-footer s)
    (finish-body s) (crlf s)
    (finish-html s) (crlf s)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; display
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod process (s (file (eql 'displaytable)) postlines)
  (cond ((null postlines) (html-message s "No relation specified."))
        (t (output-displaytable s postlines))))

(defun output-displaytable (s postlines)
  (let (relation arity columns aspect kif results count end)
    (setq relation (read-value-string (cdr (pop postlines))))
    (setq arity  (arity-of-relation relation))
    (setq columns (get-columns relation arity))
    (do ((i 1 (1+ i)) (nl))
        ((> i arity) (setq aspect (nreverse nl)))
        (setq nl (cons (decolonize (newindvar)) nl)))
    (setq kif (cons relation aspect))
    (setq results (prorequest `(ask-all ,aspect ,kif)))
    (setq count (length results))
    (setq end (min count 20))
    (setq results (subseq results 0 end))
    (format-html s) (crlf s)
    (output-head s "Display Relation") (crlf s)
    (format-body s *bgcolor*) (crlf s)
    (output-header s)
    (cond ((= count 0) (display-none s aspect kif))
          (t (display-result s columns results count 1 end)
             (output-displaytable-commands s relation aspect kif count 1 end)))
    (output-footer s)
    (finish-body s) (crlf s)
    (finish-html s) (crlf s)))

(defun output-displaytable-commands (s relation aspect kif count start end)
  (multiple-value-setq (start end) (kerchunk count start end))
  (format s "<FORM ACTION=MOREROWS? METHOD=POST>")
  (format-hidden s "Aspect" (htmlify (prin1-to-string aspect)))
  (format-hidden s "Kif" (htmlify (prin1-to-string kif)))
  (format-button s "Command" "Display")
  (format s "answers ")
  (format-text s "Start" (princ-to-string start) 5)
  (format s " through ")
  (format-text s "Solutions" (princ-to-string end) 5)
  (format s "<BR>")
  (format s "</FORM>")
  (format s "<FORM ACTION=UPDATE? METHOD=POST>")
  (format-hidden s "Relation" (stringize relation))
  (format-hidden s "Start" (stringize start))
  (format-hidden s "End" (stringize end))
  (format-button s "Command" " Edit ")
  (format s "</FORM>"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Update table
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod process (s (file (eql 'upload)) postlines)
  (cond ((null (cdr postlines)) (process-upload-start s postlines))
        (t (process-upload-upload s postlines))))

(defun process-upload-start (s postlines)
  (format-html s) (crlf s)
  (output-head s "Upload") (crlf s)
  (format-body s *bgcolor*) (crlf s)
  (output-header s)
  (format s "<P>To upload a file from your local machine to the server,
first select the file by pushing the browse button, then push the Upload
button.</P><BR>")
  (format s "<FORM ACTION=Upload? METHOD=POST ENCTYPE=\"multipart/form-data\">")
  (format s "<INPUT TYPE=FILE ACCEPT=\"*\" SIZE=\"40\" NAME=\"Filecontent\" VALUE=\"\">")
  (format-hidden s "Filename" (cdar postlines))
  (format-button s "Command" "Upload")
  (format s "</FORM>")
  (output-footer s)
  (finish-body s) (crlf s)
  (finish-html s) (crlf s))

(defun process-upload-upload (s postlines)
  (let (filename)
    (setq filename (cdadr postlines))
    (with-open-file (out filename :direction :output :if-exists :supersede)
      (princ (cdar postlines) out))
    (when (startstringp "~mrg/" filename)
      (setq filename (stringappend "file:///gullible/" (subseq filename 5))))
    (format-html s) (crlf s)
    (output-head s "Upload") (crlf s)
    (format-body s *bgcolor*) (crlf s)
    (output-header s)
    (format s "Content saved to the following URL.<BR>")
    (format s filename)
    (output-footer s)
    (finish-body s) (crlf s)
    (finish-html s) (crlf s)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Update table
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod process (s (file (eql 'updatetable)) postlines)
  (let (command)
    (setq command (getf-post "Command" postlines))
    (cond ((null command) (output-update s postlines))
          ((string-equal command " Edit ") (output-update s postlines))
          ((string-equal command "Display") (output-update-display s postlines))
          (t (update-table s postlines)))))

(defun output-update (s postlines)
  (let (relation arity columns aspect kif results count end)
    (setq relation (read-value-string (cdr (pop postlines))))
    (setq arity  (arity-of-relation relation))
    (setq columns (get-columns relation arity))
    (do ((i 1 (1+ i)) (nl))
        ((> i arity) (setq aspect (nreverse nl)))
        (setq nl (cons (decolonize (newindvar)) nl)))
    (setq kif (cons relation aspect))
    (setq results (prorequest `(ask-all ,aspect ,kif)))
    (setq count (length results))
    (setq end (min count 20))
    (setq results (subseq results 0 end))
    (format-html s) (crlf s)
    (output-head s "Update Relation") (crlf s)
    (format-body s *bgcolor*) (crlf s)
    (output-header s)
    (if results (show-update-table s relation columns results count 1 end)
        (format s "<P>No tuples found.  Arity missing or relation empty.<P>"))
    (output-footer s)
    (finish-body s) (crlf s)
    (finish-html s) (crlf s)))

(defun output-update-display (s postlines)
  (let (relation arity columns aspect kif results count start end)
    (setq relation (read-value-string (cdr (pop postlines))))
    (setq start (read-value-string (getf-post "Start" postlines)))
    (setq end (read-value-string (getf-post "End" postlines)))
    (setq arity  (arity-of-relation relation))
    (setq columns (get-columns relation arity))
    (do ((i 1 (1+ i)) (nl))
        ((> i arity) (setq aspect (nreverse nl)))
        (setq nl (cons (decolonize (newindvar)) nl)))
    (setq kif (cons relation aspect))
    (setq results (prorequest `(ask-all ,aspect ,kif)))
    (multiple-value-setq (results count start end) (trim results start end))
    (format-html s) (crlf s)
    (output-head s "Update Relation") (crlf s)
    (format-body s *bgcolor*) (crlf s)
    (output-header s)
    (if results (show-update-table s relation columns results count start end)
        (format s "<P>No tuples found.  Arity missing or relation empty.<P>"))
    (output-footer s)
    (finish-body s) (crlf s)
    (finish-html s) (crlf s)))

(defun update-table (s postlines)
  (let (relation rows results tells untells)
    (setq relation (read-user-string (cdr (pop postlines))))
    (do ((row))
        ((not (string= (caar postlines) "Row")) (setq rows (nreverse rows)))
        (multiple-value-setq (row postlines) (get-row postlines))
        (setq rows (cons row rows)))
    (setq results (read-user-string (getf-post "Results" postlines)))
    (do ((l rows (cdr l)) (m results (cdr m)) (nr))
        ((null l)
         (setq untells (nreverse untells))
         (setq tells (nreverse tells))
         (setq rows (nreverse nr)))
        (cond ((equal (car l) (car m)) (setq nr (cons (car l) nr)))
              ((null (car l)) (setq untells (cons (car m) untells)))
              (t (when (car m) (setq untells (cons (car m) untells)))
                 (setq tells (cons (car l) tells))
                 (setq nr (cons (car l) nr)))))
    (dolist (row untells) (prorequest `(untell ,(cons relation row))))
    (format-html s) (crlf s)
    (output-head s "Update Relation") (crlf s)
    (format-body s *bgcolor*) (crlf s)
    (output-header s)
    (do ((l tells (cdr l)) (dum))
        ((null l))
        (when (stringp (setq dum (prorequest `(tell ,(cons relation (car l))))))
          (do ((m tells (cdr l)))
              ((eq l m))
              (prorequest `(untell ,(cons relation (car l)))))
          (do ((l untells (cdr l)))
              ((null l))
              (prorequest `(tell ,(cons relation (car l)))))
          (http-problem s dum)
          (return nil)))
    (format s "<P>Table Updated.<P>")
    (output-footer s)
    (finish-body s) (crlf s)
    (finish-html s) (crlf s)))

(defun show-update-table (s relation columns results count start end)
  (cond ((= count 1) (format s "<P>There is 1 viewable answer.<P>"))
        (t (format s "<P>There are ~D viewable answers.  The following table shows answers ~A through ~A.<P>~%"
                     count start end)))
  (force-output s)
  (format s "<FORM ACTION=UPDATE? METHOD=POST>")
  (format-hidden s "Relation" (stringize relation))
  (format s "<CENTER><TABLE BORDER>~%")
  (format s "<TR>")
  (dolist (column columns)
    (format s "<TH>")
    (output-slotlink s column)
    (format s "</TH>"))
  (format s "</TR>~%")
  (do ((l results (cdr l)) (i 1 (1+ i)))
      ((null l))
      (format s "<TR>")
      (format-hidden s "Row" "")
      (loop
        for item in (car l)
        do (if (numberp item)
             (format s "<TD ALIGN=\"RIGHT\">")
             (format s "<TD>"))
        (format-text s "Entry" (stringize item) 20)
        (format s "</TD>~%"))
      (format s "</TR>~%"))
  (format s "<TR>")
  (format-hidden s "Row" "")
  (do ((l columns (cdr l)))
      ((null l))
      (format s "<TD>")
      (format-text s "Entry" "" 20)
      (format s "</TD>"))
  (format s "</TR>~%")
  (format s "</TABLE></CENTER><P>~%")
  (force-output s)
  (format s "<HR>~%")
  (format-hidden s "Columns" (htmlify (format nil "~S" columns)))
  (format-hidden s "Results" (htmlify (format nil "~S" results)))
  (cond ((and (= start 1) (= end count)))
        ((> count end) (setq start (1+ end) end (min count (+ end 20))))
        (t (setq start 1 end (min count 20))))
  (format-button s "Command" "Update")
  (format s "table<BR>")
  (format s "</FORM>")
  (format s "<FORM ACTION=UPDATE? METHOD=POST>")
  (format-hidden s "Relation" (stringize relation))
  (format-button s "Command" "Display")
  (format s "answers ")
  (format-text s "Start" (stringize start) 5)
  (format s " through ")
  (format-text s "End" (stringize end) 5)
  (format s "</FORM>"))

(defun get-row (postlines)
  (pop postlines)
  (get-entries "Entry" postlines))

(defun get-entries (slotname pl)
  (do ((l pl (cdr l)) (flag t) (values))
      ((or (null l) (not (string= (caar l) slotname)))
       (values (and flag (nreverse values)) l))
      (cond ((string= (cdar l) "") (setq flag nil))
            (t (setq values (cons (read-value-string (cdar l)) values))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun generate-table (s x)
  (cond ((atom x))
        ((eq 'unprovable (car x)) (generate-atom s (cadr x) nil))
        ((and (eq 'and (car x)) (cddr x)) (generate-and s x))
        ((and (eq 'or (car x)) (cddr x)) (generate-or s x))
        (t (generate-atom s x t))))

(defun generate-atom (s x flag)
  (cond ((cddddr (cdr x)) (generate-column-table s x flag))
        (t (generate-row-table s x flag))))

(defun generate-column-table (s x flag)
  (format-hidden s "Start" "")
  (format-hidden s "Style" "Table")
  (format-hidden s "Relation" (stringize (car x)))
  (format-checkbox s "Not" "" (not flag))
  (format s "<B>~A</B><BR>" (prettify (car x)))
  (format s "<TABLE BORDER>")
  (do ((l (cdr x) (cdr l)) (columns (columns (car x)) (cdr columns)))
      ((null l))
      (format s "<TR><TH ALIGN=LEFT>")
      (format s (prettify (or (car columns) "")))
      (format s "</TH>")
      (format s "<TD>")
      (format-text s "Val" (stringize (car l)) 30)
      (format s "</TD></TR>"))
  (format s "</TABLE>")
  (format-hidden s "End" ""))

(defun generate-row-table (s x flag)
  (format-hidden s "Start" "")
  (format-hidden s "Style" "Table")
  (format-hidden s "Relation" (stringize (car x)))
  (format s "<B>~A</B><BR>" (prettify (car x)))
  (format s "<TABLE BORDER>")
  (format s "<TR>")
  (format s "<TD></TD>")
  (do ((l (cdr x) (cdr l)) (columns (columns (car x)) (cdr columns)))
      ((null l))
      (format s "<TH>")
      (format s (prettify (or (car columns) "")))
      (format s "</TH>"))
  (format s "</TR>")
  (format s "<TR>")
  (format s "<TD>")
  (format-checkbox s "Not" "" (not flag))
  (format s "</TD>")
  (do ((l (cdr x) (cdr l)))
      ((null l))
      (format s "<TD>")
      (format-text s "Val" (stringize (car l)) 15)
      (format s "</TD>"))
  (format s "</TR>")
  (format s "</TABLE>")
  (format-hidden s "End" ""))

(defun generate-and (s x)
  (format s "<P>")
  (output-selector s 'begingroup '(and or) 'and)
  (format s "<B>Group</B><BR>")
  (format s "<BLOCKQUOTE>") (crlf s)
  (dolist (conjunct (cdr x))
    (format s "<P>")
    (generate-table s conjunct))
  (format-hidden s "Endgroup" "")
  (format s "</BLOCKQUOTE>") (crlf s))

(defun generate-or (s x)
  (format s "<P>")
  (output-selector s 'begingroup '(and or) 'or)
  (format s "<B>Group</B><BR>")
  (format s "<BLOCKQUOTE>") (crlf s)
  (dolist (disjunct (cdr x))
    (format s "<P>")
    (generate-table s disjunct))
  (format-hidden s "Endgroup" "")
  (format s "</BLOCKQUOTE>") (crlf s)
  (format s "<P>"))

(defun makpredicate (predicate object class)
  (declare (ignore predicate))
  (makpred object class *gui*))
;  (if predicate (list predicate object) `(isa ,object ,class)))

(defun first-aspect (postlines)
  (read-value-string (getf-post "Aspect" postlines)))

(defun construct-aspect (vars kif)
  (cond ((not (string= vars "")) (read-vars vars))
        ((nreverse (all-vars-in kif nil)))
        (t (list 1))))

(defun read-vars (str)
  (declare (type string str))
  (setq str (substitute #\space #\, str))
  (setq str (substitute #\- #\: str))
  (loop
      for (var count) =
	(multiple-value-list
	    (ignore-errors (read-from-string str nil nil)) )
      then
	(multiple-value-list
	    (ignore-errors (read-from-string str nil nil :start count)) )
      while var
      collect var ))

(defun all-vars-in (x nl)
  (cond ((eq '?* x) nl)
        ((varp x) (adjoin x nl))
        ((atom x) nl)
        (t (do ((l x (cdr l)))
               ((null l) nl)
               (setq nl (all-vars-in (car l) nl))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Output
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun output-view (s obj class)
  (cond ((not (symbolp obj)) (format s "~A" (prettify obj)))
        ;((not (viewablep *client* obj)) (format s "~A" (iconify obj)))
        (t (format s "<A HREF=\"VIEW?Object=~A&Class=~A\" target=\"_top\">~A</A>"
                   (addressify obj) (addressify class) (iconify obj)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; display.lisp
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun display-result (s aspect results count start end)
  (cond ((= count 1) (format s "<P>There is 1 viewable answer.<P>"))
        (t (format s "<P>There are ~D viewable answers.  The following table shows answers ~A through ~A.<P>~%"
                     count start end)))
  (force-output s)
  (show-table s aspect results))

(defun display-none (s aspect kif)
  (format s "<P>There are no viewable answers.</P>")
  (make-analyze-button s aspect kif))

(defun show-table (s columns results)
  (let ((nobasket (or (null results) (findp `(nocommand ,*gui* save) *interface*))))
    (format s "<FORM ACTION=TUPLES? METHOD=POST>")
    (format s "<CENTER>")
    (showtable s columns results nobasket)
    (format s "</CENTER><P>")
    (force-output s)
    (unless nobasket
      (format s "<P>")
      (format-button s "Command" "Save")
      (format s "selected rows as relation")
      (format-text s "Relation" "" 40)
      (format-hidden s "Arity" (princ-to-string (length columns)))
      (format-hidden s "Results" (htmlify (format nil "~S" results))))
    (format s "</FORM>")))

(defun showtable (s columns results nobasket)
  (format s "<TABLE BGCOLOR=WHITE BORDER>")
  (format s "<TR>")
  (unless nobasket (format s "<TH></TH>"))
  (dolist (column columns)
    (format s "<TH>")
    (format s "~A" (iconify column))
    (format s "</TH>"))
  (format s "</TR>")
  (do ((l results (cdr l)) (i 1 (1+ i)))
      ((null l))
    (format s "<TR>")
    (unless nobasket
      (format s "<TH>")
      (format-checkbox s (prettify i) "" t)
      (format s "</TH>"))
    (loop
      for item in (car l)
      do (if (numberp item)
           (format s "<TD ALIGN=RIGHT>")
           (format s "<TD>"))
      (output-value s item)
      (format s "</TD>"))
    (format s "</TR>"))
  (format s "</TABLE>"))

(defun make-analyze-button (s aspect kif)
  (declare (type stream s))
  (declare (type (or symbol list) aspect))
  (declare (type list kif))
  (format s "<BR>")
  (format s "<FORM ACTION=ANALYZE? METHOD=POST>")
  (format-hidden s "ASPECT" (stringize aspect))
  (format-hidden s "QUERY" (htmlify (format nil "~S" kif)))
  (format s "<INPUT TYPE=SUBMIT VALUE=\"Analyze\"> zero answers.")
  (format s "</FORM>"))

(defun display-failure (s aspect kif)
  (format s "<P>There are no viewable answers.")
  (make-analyze-button s aspect kif)
  (format s "<P><DL><DT>TIPS to avoid zero answers:")
  (format s "<DD>Do not supply or select values for all slots.")
  (format s "<DD>Add more alternatives to menus.")
  (format s "<DD>Add fewer alternatives to checkboxes.")
  (format s "<DD>Do not check the \"Exact\" box for text items.")
  (format s "<DD>Make sure your spelling is correct.")
  (format s "</DL><P>"))

;;;;


(defun output-empty-button (s)
  (format s "<IMAGE SRC=\"~Aimages/blank.gif\" BORDER=\"0\"/>" *home*))

(defun output-empty-button (s)
  (format s "<IMAGE SRC=\"~Aimages/transparent.gif\" WIDTH=\"11\" HEIGHT=\"11\" BORDER=\"0\"/>" *home*))

(defun output-empty-button (s)
  (format s "<IMAGE SRC='~Aimages/empty.gif' BORDER='0'/>" *home*))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; A little something to show the class hierarchy
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun showframes (interface)
  (if (stringp interface) (setq interface (read-value-string interface)))
  (format *stream* "</XMP>")
  (output-frames *stream* interface))

(defun output-frames (s interface)
  (do ((l (find-classes interface) (cdr l)))
      ((null l))
      (format s "<DD>")
      (output-classes s (car l))))

(defun showclasses (class)
  (if (stringp class) (setq class (read-value-string class)))
  (format *stream* "</XMP>")
  (output-classes *stream* 'thing))

(defun output-classes (s class)
  (format s "<DL><DT>")
  (output-value s class)
  (crlf s)
  (do ((l (find-subclasses class) (cdr l)))
      ((null l))
      (format s "<DD>")
      (output-classes s (car l)))
  (format s "</DL>")
  (crlf s))

;;;;

;(load "gullible:infomaster:source:oldforma")
;(load "gullible:infomaster:source:conforma")
;(load "gullible:infomaster:source:neoforma")

;;;;


(defun fastcomparestructure (structure)
  (do ((l (displayable-slots (cadr structure)) (cdr l)) (dum) (nl))
      ((null l) (list* (car structure) (cadr structure) (nreverse nl)))
      (cond ((setq dum (assoc (car l) (cddr structure)))
             (setq nl (cons dum nl)))
            (t (setq nl (cons (list (car l)) nl))))))

;;;;


(defmethod output-fastfind-cell (s slot value (style (eql 'fastselector)))
  (format s "<div id='~A'>" (stringize slot))
  (output-fastselector s slot value)
  (format s "</div>"))

;;;;


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
         {var slot = document.getElementById(http_slot);
          slot.innerHTML = http_result.responseText}
      else {alert('There was a problem with the request in alertResult.')}}}

;;;;


(defun output-colorpicker (s slot value)
 (when (null value) (setq value 'unknown))
 (let ((colors (cons 'unknown (asks '?x '(color.instance ?x) nil *repository*))) (i 0))
   (format s "<input type='hidden' id='input~A' name='~A' value='~A' />" *buttons* slot (if (equal value "") "" (stringize value)))
   (format s "<div onClick='showColorPicker(\"~A\",event)' id='box~A' onkeydown='return HandleKeyDown(event)'>" *buttons* *buttons*)
   (if (or (eq value 'unknown) (equal value ""))
     (format s "<span style='color:#0000cc;cursor:pointer'>[<img align='top' src='~Acolor_swatch.gif' />choose]</span>" *imageprefix*)
     (format s "<table cellspacing='0' cellpadding='0'><tr><td><div class='color' style='background-color:~A'>&nbsp;</div></td><td><span>~A</span></td><td><span style='color:#0000cc; margin-left: 10px; cursor:pointer'>[<img align='top' src='~Acolor_swatch.gif' />change]</span></td></tr></table>" (htmlcolor value) value *imageprefix*))
   (format s "</div>")
   (format s "<div style='margin-top: 3px; margin-left: 3px; border: 1px solid gray; background-color: white; position: absolute;  z-index: 3; font-family:Tahoma, Helvetica, Arial, sans-serif;font-size: 11px; display: none' id='cp~A' onClick='CancelBubble(event)'>" *buttons*)
   (format s "<table id='cptable~A' cellspacing='10' cellpadding='0' style='font-family:Tahoma, Helvetica, Arial, sans-serif;font-size: 11px; -moz-user-select: none; -khtml-user-select: none; user-select: none;' onselectstart='return false'>" *buttons*)
   (format s "<tr style='text-align:center'>")
   (dolist (color colors)
     (if (eq color 'unknown)
       (format s "<td align='center'><div class='cpNone' onclick=\"cpClicked('','None',event)\">&nbsp;</div><span>None</span></td>")
       (format s "<td align='center'><div class='cp' onclick=\"cpClicked('~A','~A',event)\" style='background-color:~A'>&nbsp;</div><span>~A</span></td>"
            (stringize color) (htmlcolor color) (htmlcolor color) color))
     (setq i (+ i 1))
     (when (eq (mod i 7) 0)
       (format s "</tr><tr>")))
   (format s "</tr>")
   (format s "</table>")
   (format s "</div>"))
   (format s "<iframe id='iframe~A' style='display:none;position:absolute;z-index:2;filter:mask();' frameborder='0'></iframe>" *buttons*) (crlf s))

(defun htmlcolor (color)
  (cond ((equalp color "Bone") "#FFFFCC")
        ((equalp color "Burgundy") "#830330")
        ((equalp color "Bronze") "#9C6963")
        (t color)))

;;;;


(defun output-extraselector (s name class value size)
  (when (eq value 'unknown) (setq value ""))
  (format s "<input type='text' id='box~A' name='~A' value='~A' size='~A' autocomplete='off' onFocus='showMenu(~A); selectElement(document.getElementById(\"box~A\").value);' onClick='CancelBubble(event);' onkeydown='return HandleKeyDown(event);' />"
          *buttons* name value size *buttons* *buttons* *buttons*) (crlf s)
  (format s "<div id='cbmenu~A' tabindex='-1' style='border:1px solid #000; background-color:#fff; position: absolute; overflow: auto; z-index: 2; cursor:pointer; font-size: 100%; display:none'>" *buttons*) (crlf s)
  (let (options (i 0))
    (setq options (mapcar #'prettyname (options name class *gui*)))
    (dolist (option options)
      (format s "<div id='cb~A_~A' onMouseOver='comboSelect(\"cb~A_~A\")' onClick='document.getElementById(\"box~A\").value=\"~A\"; hidePopup(); CancelBubble(event);' style='padding:2px;'>~A</div>"
              *buttons* i *buttons* i *buttons* option option)
      (crlf s)
      (setq i (+ i 1))))
  (format s "</div>") (crlf s)
  (format s "<iframe id='iframe~A' style='display:none;position:absolute;z-index:2;filter:mask();' frameborder='0'></iframe>" *buttons*) (crlf s))

;;;;

(defmethod process (s (command (eql 'fastselect)) postlines)
  (let (context slot value options)
    (setq slot (read-user-string (cdr (pop postlines))))
    (setq context (read-user-string (cdr (pop postlines))))
    (setq value (read-user-string (cdr (pop postlines))))
    (cond ((or (null value) (equalp value 'unknown))
           (output-fastselector-options s slot nil (findalternatives slot)))
          #|((find value context)
           (do ((l context (cdr l)))
               ((null l))
               (when (eq (car l) value) (rplacd l nil) (return t)))
           (setq options (find-components slot value context))
           (output-fastselector-options s slot context options))|#
          (t (setq context (nreverse (cons value (nreverse (findcontext slot value)))))
             (setq options (find-components slot value context))
             (output-fastselector-options s slot context options)))))

;;;;

(defmethod output-fastchange-cell (s object class slot value (style (eql 'radiobutton)))
  (declare (ignore object class))
  (let (options)
    (when (setq options (findalternatives slot))
      (format s "<table>")
      (format s "<tr><td>")
      (if (find value options :test #'equalp)
          (format s "<input type='radio' name='R~A' slot='~A' value=''/>" *buttons* (stringize slot))
          (format s "<input type='radio' name='R~A' slot='~A' value='' checked='true'/>" *buttons* (stringize slot)))
      (format s "</td></tr>")
      (dolist (option options)
        (format s "<tr><td>")
        (if (equalp option value)
            (format s "<input type='radio' name='R~A' slot='~A' value='~A' checked='true'/>"
                    *buttons* (stringize slot) (stringize option))
            (format s "<input type='radio' name='R~A' slot='~A' value='~A'/>"
                    *buttons* (stringize slot) (stringize option)))
        (format s "~A" (prettyname option))
        (format s "</td></tr>"))
      (format s "</table>"))))

;;;;


(defun output-fastfind-structure (s x)
  (format s "Find every <b>~A</b> with the following properties." (prettyname (cadr x)))
  (format-hidden s "Oject" '?*)
  (format-hidden s "Class" (stringize (cadr x)))
  (format s "<br/>")
  (format s "<table cellspacing='3'>")
  (do ((l (cddr x) (cdr l)) (multivalued) (multiple) (style) (label) (flag))
      ((null l))
      (format s "<tr><th align='left' valign='top'>")
      (unless (eq (caar l) flag)
        (setq multivalued (find-multivalued (caar l)))
        (setq style (find-searchstyle (caar l)))
        (setq label (find-searchlabel (caar l)))
        (output-slotlink s (caar l)))
      (setq multiple (or (eq (caar l) flag) (eq (caar l) (caadr l))))
      (setq flag (caar l))
      (format s "</th><td width='32' valign='top'>")
      (incf *buttons*)
      (if multivalued (output-fastfind-add s *buttons*) (output-empty-button s))
      (if multiple (output-fastfind-delete s *buttons*) (output-empty-button s))
      (format s "</td><td>")
      (output-fastfind-cell s (caar l) (cadar l) style)
      (format s "</td>")
      (when label (format s "<td>~A</td>" label))
      (format s "</tr>")
      (crlf s))
  (format s "</table>") (crlf s)
  'done)

(defmethod output-fastfind-cell (s slot value (style (eql 'menu)))
  (output-fastfind-cell s slot value 'selector))

;;;;

#|
(defun output-fancyselector (s slot value)
  (cond ((or (null value) (equalp value 'unknown))
         (output-fancyselector-start s slot 'unknown))
        (t (output-fancyselector-change s slot value))))

(defun output-fancyselector-start (s slot old)
  (format s "<input type='text' name='~A' hidden='~A' value='~A' disabled='true'/>"
          (stringize slot) (stringize old) (prettyname old))
  (format s "<span style='color:#ff0000; cursor:pointer' onClick='fancyselect(\"~A\",\"~A\",\"()\",\"~A\")'>Select</span>"
          (stringize slot) (stringize old) (stringize (result 'option slot *manager*)))
  (format s "<div id='~A'></div>" (stringize slot)))

(defun output-fancyselector-change (s slot value)
  (format s "<input type='text' name='~A' hidden='~A' value='~A' disabled='true'/>"
          (stringize slot) (stringize value) (prettyname value))
  (format s "<span style='color:#ff0000; cursor:pointer' onClick='fancyselect(\"~A\",\"~A\",\"()\",\"~A\")'>Change</span>"
          (stringize slot) (stringize value) (stringize (findalternatives slot)))
  (format s "<div id='~A'></div>" (stringize slot)))

(defun output-fancyselector-select (s slot old context options)
  (format s "<input type='text' name='~A' hidden='~A' value='~A' disabled='true'/>"
          (stringize slot) (stringize old) (prettyname old))
  (format s "<div id='~A'>" slot)
  (output-fancyselector-options s slot old context options)
  (format s "</div>"))
|#

;;;;


(defun stylesheet ()
  (format nil "<style type='text/css'>
.calleft {border-left:1px #A2BBDD solid;background:#eee}
.calright {border-right:1px #A2BBDD solid;background:#eee}
.caltop {border-top:1px #A2BBDD solid}

.gp, .gpSelected {
  cursor:pointer;
  height:18px;
  width:24px;
  padding-top: 4px;
  background-repeat: no-repeat;
  border: 1px solid black;
}

.gpSelected {
  background-color: black;
  color: white;
}

.cp, .cpSelected, .color {
  cursor:pointer;
  height:24px;
  width:24px;
  background-repeat: no-repeat;
}

.color {margin-right: 3px}

.cpNone {
  cursor:pointer;
  height:24px;
  width:24px;
  background-image:url(~Acrossnotselected.gif);
  background-repeat: no-repeat;
}

.cpNoneSelected {
  cursor:pointer;
  height:24px;
  width:24px;
  background-image:url(~Acrossselected.gif);
  background-repeat: no-repeat;
}


.color {background-image:url(~Acolorbox.gif); height:20px; width:20px;}
.cpSelected {background-image:url(~Acolorsquareselected.gif);}
.cp {background-image:url(~Acolorsquare.gif);}

.sbox {
  list-style-type: none;
  margin: 10px 5px 0 5px;
  padding: 5px;
  font-size: smaller;
  border: 1px dotted black;
}

.sb, .sbSelected {cursor: pointer; margin-bottom: 2px}

.sbSelected {
 font-weight: bold;
}

.sb:hover, .sbSelected:hover {text-decoration:underline !important}

.ssHeader {font-size: small; font-weight:bold}

</style>" *imageprefix* *imageprefix* *imageprefix* *imageprefix* *imageprefix*))

;;;;


function doclick (obj)
 {var height = window.outerHeight-100;
  var width = window.outerWidth-100;
  var left = window.screenX+50;
  var top = window.screenY+50;
  if (window.event.shiftKey == 1)
     {if (window.event.altKey == 1)
         {window.open('fastchangepage?object=' + obj,null,'height='+height+',width='+width+',left='+left+',top='+top)}
      else {window.location=('fastchangepage?object=' + obj)}}
  else if (window.event.altKey == 1)
          {window.open('fastinspectpage?object=' + obj,null,'height='+height+',width='+width+',left='+left+',top='+top)}
       else {window.location=('fastinspectpage?object=' + obj)}}


;;;;


function showColorPicker(n,evt)
 {cancelBubble(evt);
  var cp = gColorPicker;
  hidePopup();
  gColorPicker = n;
  var cp = document.getElementById('cp' + n);
  var f = document.getElementById('iframe' + n);
  cp.style.display='block';
  var box = EltBox('box' + n);
  f.style.left = cp.style.left = box.left;
  f.style.top = cp.style.top = box.bottom;
  f.style.width = cp.offsetWidth;
  f.style.height = cp.offsetHeight;
  f.style.display = 'block';
  var table = toElem('cptable' + n);
  for (r = 0; r < table.rows.length; r++) {
     var row = table.rows[r];
     for (c = 0; c < row.cells.length; c++) {
        var cell = row.cells[c];
        var text = cell.childNodes[1].innerHTML;
        var value = toElem('input' + gColorPicker).value;
        if(value.length == 0 && text=='None')
          {cell.childNodes[0].className='cpNoneSelected'}
        else if(text == 'None') {cell.childNodes[0].className='cpNone'}
        else if(value.replace('\"','').replace('\"','') == text) {cell.childNodes[0].className='cpSelected'}
        else {cell.childNodes[0].className='cp'}}}}

function hideColorPicker ()
 {if (typeof gColorPicker == 'string')
     {document.getElementById('cp' + gColorPicker).style.display='none';
      document.getElementById('iframe' + gColorPicker).style.display='none';
      gColorPicker = null}}

function cpClicked (val,color,evt)
 {toElem('input' + gColorPicker).value=val;
  if (val == '')
     {toElem('box' + gColorPicker).innerHTML= '<span style=\"color:#0000cc;cursor:pointer\">[<img align=\"top\" src=\"~Acolor_swatch.gif\" />choose]</span>';}
  else {toElem('box' + gColorPicker).innerHTML= '<table cellspacing=\"0\" cellpadding=\"0\"><tr><td><div class=\"color\" style=\"background-color:' + color + '\">&nbsp;</div></td><td><span>' + val.replace('\"','').replace('\"','') + '</span></td><td><span style=\"color:#0000cc; margin-left: 10px; cursor:pointer\">[<img align=\"top\" src=\"~Acolor_swatch.gif\" />change]</span></td></tr></table>';}
  hidePopup();
  cancelBubble(evt)}

;;;;


(defun process-fastfindpage-another (s structure index)
  (do ((l (cddr structure) (cdr l)) (i 1 (1+ i)))
      ((null l) (output-fastfindpage s structure))
      (when (equal index i)
        (rplacd l (cons (list (caar l)) (cdr l)))
        (output-fastfindpage s structure)
        (return 'done))))

(defun process-fastfindpage-removal (s structure index)
  (do ((l (cdr structure) (cdr l)) (i 1 (1+ i)))
      ((null (cdr l)) (output-fastfindpage s structure))
      (when (equal index i)
        (rplacd l (cddr l))
        (output-fastfindpage s structure)
        (return 'done))))

;;;;

       else if (fobj.elements[i].hidden)
               {str += ' ' + listify(fobj.elements[i].name, escape(fobj.elements[i].hidden))}

;;;;




(defmethod output-fastgallery-cell (s class slot value structure (style (eql 'menu)))
  (output-fastgallery-cell s class slot value structure 'selector))

(defmethod output-fastgallery-cell (s class slot value structure (style (eql 'selector)))
  (declare (ignore class))
  (let (options)
    (setq options (cons 'unknown (fastoptions slot structure)))
    (output-newselector s slot options value)))

(defmethod output-fastgallery-cell (s class slot value structure (style (eql 'fastselector)))
  (declare (ignore class))
  (output-weirdfastselector s slot value structure))

(defmethod output-fastgallery-cell (s class slot value structure (style (eql 'fancyselector)))
  (output-fastgallery-cell s class slot value structure 'fastselector))

(defmethod output-fastgallery-cell (s class slot value structure (style (eql 'combobox)))
  (declare (ignore class))
  (let (options)
    (when (setq options (fastoptions slot structure))
      (setq options (cons 'unknown options))
      (output-newcombobox s slot options (prettyname value) 30))))

(defmethod output-fastgallery-cell (s class slot value structure (style (eql 'checkbox)))
  (output-fastgallery-cell s class slot value structure 'radiobutton))

(defmethod output-fastgallery-cell (s class slot value structure (style (eql 'radiobutton)))
  (declare (ignore class))
  (let (options)
    (cond ((setq options (fastoptions slot structure))
           (setq options (cons nil options))
           (output-newradiobutton s slot options value))
          (t (format-hidden s (stringize slot) "")))))

(defmethod output-fastgallery-cell (s class slot value structure (style (eql 'typein)))
  (declare (ignore class structure))
  (format-newtypein s (stringize slot) (stringize value) 20))

(defmethod output-fastgallery-cell (s class slot value structure (style (eql 'stringfield)))
  (declare (ignore class structure))
  (format-newstringfield s (stringize slot) (prettyname value) 20))

(defmethod output-fastgallery-cell (s class slot value structure (style (eql 'text)))
  (output-fastgallery-cell s class slot value structure 'stringfield))

(defmethod output-fastgallery-cell (s class slot value structure (style (eql 'textarea)))
  (output-fastgallery-cell s class slot value structure 'stringfield))

(defmethod output-fastgallery-cell (s class slot value structure (style (eql 'password)))
  (declare (ignore class structure))
  (format-newpassword s (stringize slot) (prettyname value) 20))

(defmethod output-fastgallery-cell (s class slot value structure (style (eql 'dateinput)))
  (declare (ignore class structure))
  (output-newdateinput s slot value))

(defmethod output-fastgallery-cell (s class slot value structure (style (eql 'datestyle)))
  (declare (ignore class structure))
  (format-hidden s (stringize slot) (stringize value))
  (when value (output-simple s value)))

(defmethod output-fastgallery-cell (s class slot value structure (style (eql 'dollarinput)))
  (declare (ignore class structure))
  (output-newdollarinput s slot value 6))

(defmethod output-fastgallery-cell (s class slot value structure (style (eql 'dollarstyle)))
  (declare (ignore class structure))
  (format-hidden s (stringize slot) (stringize value))
  (when (realp value) (format s "$~$" value)))

(defmethod output-fastgallery-cell (s class slot value structure (style (eql 'emailstyle)))
  (declare (ignore class structure))
  (format-hidden s (stringize slot) (stringize value))
  (when value (format s "<a href='mailto:~A'><font color='red'>~A</font></a>" value value)))

(defmethod output-fastgallery-cell (s class slot value structure (style (eql 'htmlstyle)))
  (declare (ignore class structure))
  (format-hidden s (stringize slot) (stringize value))
  (when value (format s "~A" value)))

(defmethod output-fastgallery-cell (s class slot value structure (style (eql 'imagestyle)))
  (declare (ignore class structure))
  (format-hidden s (stringize slot) (stringize value))
  (when value (format s "<img src='~A'/>" value)))

(defmethod output-fastgallery-cell (s class slot value structure (style (eql 'urlstyle)))
  (declare (ignore class structure))
  (format-hidden s (stringize slot) (stringize value))
  (when value (format s "<a href='~A'>~A</a>" value (htmlify value))))

(defmethod output-fastgallery-cell (s class slot value structure (style (eql 'glyph)))
  (declare (ignore class structure style))
  (format-hidden s (stringize slot) (stringize value))
  (if value (output-handle s value)))

(defmethod output-fastgallery-cell (s class slot value structure (style (eql 'prettystyle)))
  (declare (ignore class structure style))
  (format-hidden s (stringize slot) (stringize value))
  (if value (output-simple s value)))

(defmethod output-fastgallery-cell (s class slot value structure style)
  (declare (ignore class structure style))
  (format-hidden s (stringize slot) (stringize value)))

;;;;




function fastoldselect (slot,value)
 {http_slot = slot;
  postFastoldselect('fastoldselect?','Slot=' + slot + '&Value=' + value)}

function postFastoldselect (url,args)
 {http_result = false;
  if (window.XMLHttpRequest)
     {http_result = new XMLHttpRequest();
      if (http_result.overrideMimeType)
         {http_result.overrideMimeType('text/xml');}}
  else if (window.ActiveXObject)
          {try {http_result = new ActiveXObject('Msxml2.XMLHTTP')}
           catch (e) {try {http_result = new ActiveXObject('Microsoft.XMLHTTP')}
                      catch (e) {} }}
  http_result.onreadystatechange = alertFastoldelect;
  http_result.open('POST', url, true);
  http_result.send(args);}

function alertFastoldelect()
 {if (http_result.readyState == 4)
     {if (http_result.responseText)
         {var cell = document.getElementById(http_slot);
          cell.innerHTML = http_result.responseText}
      else {alert('There was a problem with the request in alertResult.')}}}

function fastnewselect (slot,value)
 {http_slot = slot;
  postFastnewelect('fastnewselect?','Slot=' + slot + '&Value=' + value)}

function postFastnewelect (url,args)
 {http_result = false;
  if (window.XMLHttpRequest)
     {http_result = new XMLHttpRequest();
      if (http_result.overrideMimeType)
         {http_result.overrideMimeType('text/xml');}}
  else if (window.ActiveXObject)
          {try {http_result = new ActiveXObject('Msxml2.XMLHTTP')}
           catch (e) {try {http_result = new ActiveXObject('Microsoft.XMLHTTP')}
                      catch (e) {} }}
  http_result.onreadystatechange = alertFastnewselect;
  http_result.open('POST', url, true);
  http_result.send(args);}

function alertFastnewselect()
 {if (http_result.readyState == 4)
     {if (http_result.responseText)
         {var cell = document.getElementById(http_slot);
          cell.innerHTML = http_result.responseText;
          postReplace(form1)}
      else {alert('There was a problem with the request in alertResult.')}}}



function fancyoldselect (slot,old,context,value)
 {http_slot = slot;
  http_old = value;
  postFancyselect('fancyoldselect?','Slot=' + slot + '&Old=' + old + '&Context=' + context + '&Value=' + value)}

function fancyolddeselect (slot,old,context,value)
 {http_slot = slot;
  http_old = value;
  postFancyselect('fancyolddeselect?','Slot=' + slot + '&Old=' + old + '&Context=' + context + '&Value=' + value)}

function fancycancel (slot,old)
 {document.getElementById(slot).innerHTML=''}

function postFancyoldselect (url,args)
 {http_result = false;
  if (window.XMLHttpRequest)
     {http_result = new XMLHttpRequest();
      if (http_result.overrideMimeType)
         {http_result.overrideMimeType('text/xml');}}
  else if (window.ActiveXObject)
          {try {http_result = new ActiveXObject('Msxml2.XMLHTTP')}
           catch (e) {try {http_result = new ActiveXObject('Microsoft.XMLHTTP')}
                      catch (e) {} }}
  http_result.onreadystatechange = alertFancyoldselect;
  http_result.open('POST', url, true);
  http_result.send(args);}

function alertFancyoldselect()
 {if (http_result.readyState == 4)
     {if (http_result.responseText)
         {var elem = document.getElementById(http_slot);
          if (http_result.responseText == ' ')
             {document.getElementsByName(http_slot)[0].value=http_old};
          elem.innerHTML = http_result.responseText}
      else {alert('There was a problem with the request in alertResult.')}}}

function fancyoldselect (slot,old,context,value)
 {http_slot = slot;
  http_old = value;
  postFancy('fancyoldselect?','Slot=' + slot + '&Old=' + old + '&Context=' + context + '&Value=' + value)}

function fancyolddeselect (slot,old,context,value)
 {http_slot = slot;
  http_old = value;
  postFancy('fancyolddeselect?','Slot=' + slot + '&Old=' + old + '&Context=' + context + '&Value=' + value)}

function fancycancel (slot,old)
 {document.getElementById(slot).innerHTML=''}

function postFancynewselect (url,args)
 {http_result = false;
  if (window.XMLHttpRequest)
     {http_result = new XMLHttpRequest();
      if (http_result.overrideMimeType)
         {http_result.overrideMimeType('text/xml');}}
  else if (window.ActiveXObject)
          {try {http_result = new ActiveXObject('Msxml2.XMLHTTP')}
           catch (e) {try {http_result = new ActiveXObject('Microsoft.XMLHTTP')}
                      catch (e) {} }}
  http_result.onreadystatechange = alertFancynewselect;
  http_result.open('POST', url, true);
  http_result.send(args);}

function alertFancynewselect()
 {if (http_result.readyState == 4)
     {if (http_result.responseText)
         {var elem = document.getElementById(http_slot);
          if (http_result.responseText == ' ')
             {var inp = document.getElementsByName(http_slot)[0];
              inp.value=http_old;
              postReplace(inp.form)};
          elem.innerHTML = http_result.responseText}
      else {alert('There was a problem with the request in alertResult.')}}}

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

;;;;


(defun output-fastcreate-inner (s structure)
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
  (format s "</table>") (crlf s))

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
;;; fastviewscript
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


(defmethod materializations (p (receiver viewserver))
  (let (*agent* temp pat answers)
    (setq *agent* (name receiver))
    (setq temp (make-instance 'theory))
    (insertposneg p temp)
    (includes temp receiver)
    (dolist (r (dependents p receiver #'materialp))
      (setq pat `(,r @l))
      (setq answers (nconc answers (viewfinds `(not ,pat) `(neg ,pat) temp)))
      (setq answers (nconc answers (viewfinds pat `(pos ,pat) temp))))
    (decludes temp)
    (empty temp)
    answers))

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
  (format-head s)
  (format s "<title>fastshowpage</title>") (crlf s)
  (format-javascript s) (crlf s)
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
         (format s "<tr><td align='center'>There are no answers.</td></tr>")
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
         (format s "<tr><td align='center'>There are ~D answers.</td></tr>" count)
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
    (format s "<table bgcolor='~A' border='~A'>" *bgcolor* *border*) (crlf s)
    (format s "<tr>")
    (format s "<form name='form1' action='~A?' method='post'>" *fastshowpage*) (crlf s)
    (format-hidden s "Object" (stringize (car structure))) (crlf s)
    (format-hidden s "Class" (stringize (cadr structure))) (crlf s)
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
        (output-fastshow-cells s slot values (find-searchstyle slot))
        (format s "</th>")
      (crlf s))
    (format s "</form>") (crlf s)
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
            (if (and vals (every #'(lambda (val) (numberp val)) vals))
                (format s "<td align='right'>")
                (format s "<td>"))
            (output-cells s (car slots) vals style)
            ;(when vals (output-handle-in-style s (car vals) style))
            ;(dolist (val (cdr vals))
            ;  (format s ", ")
            ;  (output-handle-in-style s val style))
            (format s "</td>"))
        (format s "</tr>")
        (crlf s))
    (format s "</table>") (crlf s)))


(defun newprettify (slot x)
  (cond ((eq x 'unknown) (stringappend "Any " (iconify slot)))
        (t (prettyname x))))

;;;;

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

(defmethod output-fastcreate-cells (s slot values (style (eql 'tabulator)))
  (let (class slots results)
    (setq class (find-range slot))
    (setq slots (createable-slots class))
    (setq results (prorequest `(ask-table ,values ,slots)))
    (format s "<div>")
    (output-fastupdate-inner s slot class values slots results)
    (format s "</div>")))

(defmethod output-fastcreate-cells (s slot values (style (eql 'subframe)))
  (format s "<table cellpadding='0' cellspacing='0'>")
  (incf *buttons*)

  (format s "<tr id='~A' qualifier='skip' style='display:None'>" slot)
  (format s "<td width='32' valign='top'>")
  (output-additem s slot)
  (output-remitem s)
  (format s "</td><td>")
  (output-fastcreate-cell s slot nil style)
  (format s "</td>")
  (format s "</tr>")

  (dolist (value values)
    (incf *buttons*)
    (format s "<tr><td width='32' valign='top'>")
    (output-additem s slot)
    (output-remitem s)
    (format s "</td><td>")
    (output-fastcreate-cell s slot value style) (crlf s)
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

(defmethod output-fastcreate-cells (s slot values style)
  (output-fastcreate-unicells s slot values style))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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
  (format-oldstringfield s (stringize slot) (prettyname value) 40))

(defmethod output-fastcreate-cell (s slot value (style (eql 'textarea)))
  (format-oldstringfield s (stringize slot) (prettyname value) 40))

(defmethod output-fastcreate-cell (s slot value (style (eql 'password)))
  (format-password s slot (prettyname value) 40))

(defmethod output-fastcreate-cell (s slot value (style (eql 'dateinput)))
  (output-olddateinput s slot value))

(defmethod output-fastcreate-cell (s slot value (style (eql 'dollarinput)))
  (output-olddollarinput s slot value 6))

(defmethod output-fastcreate-cell (s slot value (style (eql 'glyph)))
  (when value
    (output-hidden s slot value)
    (output-handle s value)))

(defmethod output-fastcreate-cell (s slot value (style (eql 'prettystyle)))
  (when value
    (output-hidden s slot value)
    (output-simple s value)))

(defmethod output-fastcreate-cell (s slot value (style (eql 'datestyle)))
  (when value
    (output-hidden s slot value)
    (output-simple s value)))

(defmethod output-fastcreate-cell (s slot value (style (eql 'dollarstyle)))
  (when value
    (output-hidden s slot value)
    (when (realp value) (format s "$~$" value))))

(defmethod output-fastcreate-cell (s slot value (style (eql 'htmlstyle)))
  (when value
    (output-hidden s slot value)
    (format s "~A" (htmlify value))))

(defmethod output-fastcreate-cell (s slot value (style (eql 'emailstyle)))
  (when value
    (output-hidden s slot value)
    (format s "<a href='mailto:~A'><font color='red'>~A</font></a>"
	    (stringize value) (htmlify value))))

(defmethod output-fastcreate-cell (s slot value (style (eql 'urlstyle)))
  (when value
    (output-hidden s slot value)
    (format s "<a href='~A'>~A</a>" value (htmlify value))))

(defmethod output-fastcreate-cell (s slot value (style (eql 'imagestyle)))
  (when value
    (output-hidden s slot value)
    (format s "<img src='~A'/>" value)))

(defmethod output-fastcreate-cell (s slot value (style (eql 'subframe)))
  (let (class)
    (setq class (find-range slot))
    (format s "<div slot='~A' id='~A' category='~A' style='border:groove; width:640'>"
            slot value class)
    (format s "<span>~A</span>" (prettyname value))
    (format s "<br/>")
    (output-fastcreate-inner s (changeitem value class))
    (format s "</div>")))

(defmethod output-fastcreate-cell (s slot value style)
  (declare (ignore style))
  (output-hidden s slot value))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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
  (format s "<table cellpadding='0' cellspacing='0'>")
  (incf *buttons*)

  (format s "<tr id='~A' qualifier='skip' style='display:None'>" slot)
  (format s "<td width='32' valign='top'>")
  (output-addslot s slot)
  (output-remslot s slot)
  (format s "</td><td>")
  (output-fastcreate-cell s slot nil style)
  (format s "</td>")
  (format s "</tr>")

  (dolist (value values)
    (incf *buttons*)
    (format s "<tr><td width='32' valign='top'>")
    (output-addslot s slot)
    (output-remslot s slot)
    (format s "</td><td>")
    (output-fastcreate-cell s slot value style) (crlf s)
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



(defmethod output-fastview-cells (s slot values structure (style (eql 'menu)))
  (let (options)
    (setq options (fastoptions slot structure))
    (incf *buttons*)
    (when options (output-newmenu s slot options values))
    (crlf s)))

(defmethod output-fastview-cells (s slot values structure (style (eql 'selector)))
  (output-fastview-multicells s slot values structure style))

(defmethod output-fastview-cells (s slot values structure (style (eql 'fastselector)))
  (incf *buttons*)
  (output-fastview-cell s slot (car values) structure style))

(defmethod output-fastview-cells (s slot values structure (style (eql 'fancyselector)))
  (incf *buttons*)
  (output-fastview-cell s slot (car values) structure style))

(defmethod output-fastview-cells (s slot values structure (style (eql 'combobox)))
  (incf *buttons*)
  (output-fastview-cell s slot (car values) structure style))

(defmethod output-fastview-cells (s slot values structure (style (eql 'checkbox)))
  (let (options)
    (setq options (fastoptions slot structure))
    (incf *buttons*)
    (when options (output-newcheckbox s slot options values))
    (crlf s)))

(defmethod output-fastview-cells (s slot values structure (style (eql 'radiobutton)))
  (incf *buttons*)
  (output-fastview-cell s slot (car values) structure style))

(defmethod output-fastview-cells (s slot values structure (style (eql 'typein)))
  (output-fastview-multicells s slot values structure style))

(defmethod output-fastview-cells (s slot values structure (style (eql 'stringfield)))
  (output-fastview-multicells s slot values structure style))

(defmethod output-fastview-cells (s slot values structure (style (eql 'text)))
  (output-fastview-multicells s slot values structure style))

(defmethod output-fastview-cells (s slot values structure (style (eql 'textarea)))
  (output-fastview-multicells s slot values structure style))

(defmethod output-fastview-cells (s slot values structure (style (eql 'password)))
  (output-fastview-multicells s slot values structure style))

(defmethod output-fastview-cells (s slot values structure (style (eql 'dateinput)))
  (output-fastview-multicells s slot values structure style))

(defmethod output-fastview-cells (s slot values structure (style (eql 'datestyle)))
  (incf *buttons*)
  (output-fastview-cell s slot (car values) structure style))

(defmethod output-fastview-cells (s slot values structure (style (eql 'dollarinput)))
  (output-fastview-multicells s slot values structure style))

(defmethod output-fastview-cells (s slot values structure (style (eql 'dollarstyle)))
  (incf *buttons*)
  (output-fastview-cell s slot (car values) structure style))

(defmethod output-fastview-cells (s slot values structure (style (eql 'emailstyle)))
  (incf *buttons*)
  (output-fastview-cell s slot (car values) structure style))

(defmethod output-fastview-cells (s slot values structure (style (eql 'htmlstyle)))
  (incf *buttons*)
  (output-fastview-cell s slot (car values) structure style))

(defmethod output-fastview-cells (s slot values structure (style (eql 'imagestyle)))
  (incf *buttons*)
  (output-fastview-cell s slot (car values) structure style))

(defmethod output-fastview-cells (s slot values structure (style (eql 'urlstyle)))
  (incf *buttons*)
  (output-fastview-cell s slot (car values) structure style))

(defmethod output-fastview-cells (s slot values structure (style (eql 'glyph)))
  (incf *buttons*)
  (output-fastview-cell s slot (car values) structure style))

(defmethod output-fastview-cells (s slot values structure (style (eql 'prettystyle)))
  (incf *buttons*)
  (output-fastview-cell s slot (car values) structure style))

(defmethod output-fastview-cells (s slot values structure style)
  (incf *buttons*)
  (output-fastview-cell s slot (car values) structure style))

(defun output-fastview-multicells (s slot values structure style)
  (let (multivalued multiple)
    (setq multivalued (find-multivalued slot))
    (setq multiple (cdr values))
    (format s "<table cellpadding='0' cellspacing='0'>")
    (incf *buttons*)
    (format s "<tr><td valign='center'>")
    (when multivalued (output-add s *buttons*))
    (when multiple (output-delete s *buttons*))
    (format s "</td><td valign='center'>")
    (output-fastview-cell s slot (car values) structure style) (crlf s)
    (format s "</td></tr>")
    (dolist (value (cdr values))
      (incf *buttons*)
      (format s "<tr><td valign='center'>")
      (when multivalued (output-add s *buttons*))
      (when multiple (output-delete s *buttons*))
      (format s "</td><td>")
      (output-fastview-cell s slot value structure style) (crlf s)
      (format s "</td></tr>"))
    (format s "</table>")
    (crlf s)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod output-fastview-cell (s slot value structure (style (eql 'selector)))
  (let (options)
    (setq options (cons 'unknown (fastoptions slot structure)))
    (output-newselector s slot options value)))

(defmethod output-fastview-cell (s slot value structure (style (eql 'fastselector)))
  (let (options)
    (setq options (cons 'unknown (fastoptions slot structure)))
    (output-newselector s slot options value)))

(defmethod output-fastview-cell (s slot value structure (style (eql 'fancyselector)))
  (let (options)
    (setq options (cons 'unknown (fastoptions slot structure)))
    (output-newselector s slot options value)))

(defmethod output-fastview-cell (s slot value structure (style (eql 'combobox)))
  (let (options)
    (when (setq options (fastoptions slot structure))
      (setq options (cons 'unknown options))
      (output-newcombobox s slot options (prettyname value) 20))))

(defmethod output-fastview-cell (s slot value structure (style (eql 'radiobutton)))
  (let (options)
    (cond ((setq options (fastoptions slot structure))
           (setq options (cons nil options))
           (output-newradiobutton s slot options value))
          (t (format-hidden s (stringize slot) "")))))

(defmethod output-fastview-cell (s slot value structure (style (eql 'typein)))
  (declare (ignore structure))
  (format-newtypein s (stringize slot) (stringize value) 20))

(defmethod output-fastview-cell (s slot value structure (style (eql 'stringfield)))
  (declare (ignore structure))
  (format-newstringfield s (stringize slot) (prettyname value) 20))

(defmethod output-fastview-cell (s slot value structure (style (eql 'text)))
  (declare (ignore structure))
  (format-newstringfield s (stringize slot) (prettyname value) 20))

(defmethod output-fastview-cell (s slot value structure (style (eql 'textarea)))
  (declare (ignore structure))
  (format-newstringfield s (stringize slot) (prettyname value) 20))

(defmethod output-fastview-cell (s slot value structure (style (eql 'password)))
  (declare (ignore structure))
  (format-newpassword s (stringize slot) (prettyname value) 20))

(defmethod output-fastview-cell (s slot value structure (style (eql 'dateinput)))
  (declare (ignore structure))
  (output-newdateinput s slot value))

(defmethod output-fastview-cell (s slot value structure (style (eql 'datestyle)))
  (declare (ignore structure))
  (format-hidden s (stringize slot) (stringize value))
  (if value (output-simple s value) (format s "~A" (iconify slot))))

(defmethod output-fastview-cell (s slot value structure (style (eql 'dollarinput)))
  (declare (ignore structure))
  (output-newdollarinput s slot value 6))

(defmethod output-fastview-cell (s slot value structure (style (eql 'dollarstyle)))
  (declare (ignore structure))
  (format-hidden s (stringize slot) (stringize value))
  (if (realp value) (format s "$~$" value) (format s "~A" (iconify slot))))

(defmethod output-fastview-cell (s slot value structure (style (eql 'emailstyle)))
  (declare (ignore structure))
  (format-hidden s (stringize slot) (stringize value))
  (if value (format s "<a href='mailto:~A'><font color='red'>~A</font></a>" value value)
      (format s "~A" (iconify slot))))

(defmethod output-fastview-cell (s slot value structure (style (eql 'htmlstyle)))
  (declare (ignore structure))
  (format-hidden s (stringize slot) (stringize value))
  (if value (format s "~A" value) (format s "~A" (iconify slot))))

(defmethod output-fastview-cell (s slot value structure (style (eql 'imagestyle)))
  (declare (ignore structure))
  (format-hidden s (stringize slot) (stringize value))
  (if value (format s "<img src='~A'/>" value) (format s "~A" (iconify slot))))

(defmethod output-fastview-cell (s slot value structure (style (eql 'urlstyle)))
  (declare (ignore structure))
  (format-hidden s (stringize slot) (stringize value))
  (if value (format s "<a href='~A'>~A</a>" value (htmlify value)) (format s "~A" (iconify slot))))

(defmethod output-fastview-cell (s slot value structure (style (eql 'glyph)))
  (declare (ignore structure))
  (format-hidden s (stringize slot) (stringize value))
  (format s "~A" (iconify slot)))

(defmethod output-fastview-cell (s slot value structure (style (eql 'prettystyle)))
  (declare (ignore structure))
  (format-hidden s (stringize slot) (stringize value))
  (format s "~A" (iconify slot)))

(defmethod output-fastview-cell (s slot value structure style)
  (declare (ignore structure style))
  (format-hidden s (stringize slot) (stringize value))
  (format s "~A" (iconify slot)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod output-slowfind-cells (s slot values (style (eql 'checkbox)))
  (let (options)
    (when (setq options (findalternatives slot))
      (output-oldcheckbox s slot options values))))

(defmethod output-slowfind-cells (s slot values (style (eql 'combobox)))
  (output-slowfind-unicells s slot values style))

(defmethod output-slowfind-cells (s slot values (style (eql 'dateinput)))
  (output-slowfind-unicells s slot values style))

(defmethod output-slowfind-cells (s slot values (style (eql 'datestyle)))
  (output-slowfind-unicells s slot values style))

(defmethod output-slowfind-cells (s slot values (style (eql 'dollarinput)))
  (output-slowfind-unicells s slot values style))

(defmethod output-slowfind-cells (s slot values (style (eql 'dollarstyle)))
  (output-slowfind-unicells s slot values style))

(defmethod output-slowfind-cells (s slot values (style (eql 'emailstyle)))
  (output-slowfind-unicells s slot values style))

(defmethod output-slowfind-cells (s slot values (style (eql 'fastselector)))
  (output-slowfind-unicells s slot values style))

(defmethod output-slowfind-cells (s slot values (style (eql 'fancyselector)))
  (output-slowfind-unicells s slot values style))

(defmethod output-slowfind-cells (s slot values (style (eql 'glyph)))
  (output-slowfind-unicells s slot values style))

(defmethod output-slowfind-cells (s slot values (style (eql 'htmlstyle)))
  (output-slowfind-unicells s slot values style))

(defmethod output-slowfind-cells (s slot values (style (eql 'imagestyle)))
  (output-slowfind-unicells s slot values style))

(defmethod output-slowfind-cells (s slot values (style (eql 'menu)))
  (let (options)
    (when (setq options (findalternatives slot))
      (output-oldmenu s slot options values))))

(defmethod output-slowfind-cells (s slot values (style (eql 'password)))
  (output-slowfind-unicells s slot values style))

(defmethod output-slowfind-cells (s slot values (style (eql 'prettystyle)))
  (output-slowfind-unicells s slot values style))

(defmethod output-slowfind-cells (s slot values (style (eql 'radiobutton)))
  (output-slowfind-unicells s slot values style))

(defmethod output-slowfind-cells (s slot values (style (eql 'selector)))
  (output-slowfind-unicells s slot values style))

(defmethod output-slowfind-cells (s slot values (style (eql 'stringfield)))
  (output-slowfind-unicells s slot values style))

(defmethod output-slowfind-cells (s slot values (style (eql 'subframe)))
  (output-slowfind-unicells s slot values style))

(defmethod output-slowfind-cells (s slot values (style (eql 'tabulator)))
  (output-slowfind-unicells s slot values style))

(defmethod output-slowfind-cells (s slot values (style (eql 'text)))
  (output-slowfind-unicells s slot values style))

(defmethod output-slowfind-cells (s slot values (style (eql 'textarea)))
  (output-slowfind-unicells s slot values style))

(defmethod output-slowfind-cells (s slot values (style (eql 'typein)))
  (output-slowfind-unicells s slot values style))

(defmethod output-slowfind-cells (s slot values (style (eql 'urlstyle)))
  (output-slowfind-unicells s slot values style))

(defmethod output-slowfind-cells (s slot values style)
  (output-slowfind-unicells s slot values style))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod output-slowfind-cell (s slot value (style (eql 'checkbox)))
  (let (options)
    (when (setq options (findalternatives slot))
      (output-oldcheckbox s slot options (list value)))))

(defmethod output-slowfind-cell (s slot value (style (eql 'combobox)))
  (let (options)
    (when (setq options (findalternatives slot))
      (setq options (cons 'unknown options))
      (output-oldcombobox s slot options (prettyname value) 30))))

(defmethod output-slowfind-cell (s slot value (style (eql 'dateinput)))
  (output-olddateinput s slot value))

(defmethod output-slowfind-cell (s slot value (style (eql 'datestyle)))
  (format-hidden s (stringize slot) (stringize value))
  (output-simple s value))

(defmethod output-slowfind-cell (s slot value (style (eql 'dollarinput)))
  (output-olddollarinput s slot value 6))

(defmethod output-slowfind-cell (s slot value (style (eql 'dollarstyle)))
  (format-hidden s (stringize slot) (stringize value))
  (when (realp value) (format s "$~$" value)))

(defmethod output-slowfind-cell (s slot value (style (eql 'emailstyle)))
  (format-hidden s (stringize slot) (stringize value))
  (when value (format s "<a href='mailto:~A'><font color='red'>~A</font></a>" value value)))

(defmethod output-slowfind-cell (s slot value (style (eql 'fastselector)))
  (output-oldfastselector s slot value))

(defmethod output-slowfind-cell (s slot value (style (eql 'fancyselector)))
  (output-oldfancyselector s slot value))

(defmethod output-slowfind-cell (s slot value (style (eql 'glyph)))
  (format-hidden s (stringize slot) (stringize value))
  (output-handle s value))

(defmethod output-slowfind-cell (s slot value (style (eql 'htmlstyle)))
  (format-hidden s (stringize slot) (stringize value))
  (when value (format s "~A" value)))

(defmethod output-slowfind-cell (s slot value (style (eql 'imagestyle)))
  (format-hidden s (stringize slot) (stringize value))
  (when value (format s "<img src='~A'/>" value)))

(defmethod output-slowfind-cell (s slot value (style (eql 'menu)))
  (let (options)
    (when (setq options (findalternatives slot))
      (output-oldmenu s slot options (list value)))))

(defmethod output-slowfind-cell (s slot value (style (eql 'password)))
  (format-oldpassword s (stringize slot) (prettyname value) 40))

(defmethod output-slowfind-cell (s slot value (style (eql 'prettystyle)))
  (format-hidden s (stringize slot) (stringize value))
  (output-simple s value))

(defmethod output-slowfind-cell (s slot value (style (eql 'radiobutton)))
  (let (options)
    (when (setq options (findalternatives slot))
      (setq options (cons 'unknown options))
      (output-oldradiobutton s slot options value))))

(defmethod output-slowfind-cell (s slot value (style (eql 'selector)))
  (let (options)
    (when (setq options (findalternatives slot))
      (setq options (cons 'unknown options))
      (output-oldselector s slot options value))))

(defmethod output-slowfind-cell (s slot value (style (eql 'stringfield)))
  (format-oldstringfield s (stringize slot) (prettyname value) 40))

(defmethod output-slowfind-cell (s slot value (style (eql 'subframe)))
  (format-hidden s (stringize slot) (stringize value)))

(defmethod output-slowfind-cell (s slot value (style (eql 'tabulator)))
  (format-hidden s (stringize slot) (stringize value)))

(defmethod output-slowfind-cell (s slot value (style (eql 'text)))
  (format-oldstringfield s (stringize slot) (prettyname value) 40))

(defmethod output-slowfind-cell (s slot value (style (eql 'textarea)))
  (format-oldstringfield s (stringize slot) (prettyname value) 40))

(defmethod output-slowfind-cell (s slot value (style (eql 'typein)))
  (format-oldtypein s (stringize slot) (stringize value) 40))

(defmethod output-slowfind-cell (s slot value (style (eql 'urlstyle)))
  (format-hidden s (stringize slot) (stringize value))
  (when value (format s "<a href='~A'>~A</a>" value (htmlify value))))

(defmethod output-slowfind-cell (s slot value style)
  (declare (ignore style))
  (format-hidden s (stringize slot) (stringize value)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod output-slowfind-unicells (s slot values style)
  (format s "<table cellpadding='0' cellspacing='0'>")
  (format s "<tr><td>")
  (output-slowfind-cell s slot (car values) style) (crlf s)
  (format s "</td></tr>")
  (dolist (value (cdr values))
    (format s "<tr><td>")
    (output-slowfind-cell s slot value style) (crlf s)
    (format s "</td></tr>"))
  (format s "</table>")
  (crlf s))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod output-slowchange-cells (s slot values (style (eql 'checkbox)))
  (let (options)
    (when (setq options (findalternatives slot))
      (output-oldcheckbox s slot options values))))

(defmethod output-slowchange-cells (s slot values (style (eql 'combobox)))
  (output-slowchange-unicells s slot values style))

(defmethod output-slowchange-cells (s slot values (style (eql 'dateinput)))
  (output-slowchange-unicells s slot values style))

(defmethod output-slowchange-cells (s slot values (style (eql 'datestyle)))
  (output-slowchange-unicells s slot values style))

(defmethod output-slowchange-cells (s slot values (style (eql 'dollarinput)))
  (output-slowchange-unicells s slot values style))

(defmethod output-slowchange-cells (s slot values (style (eql 'dollarstyle)))
  (output-slowchange-unicells s slot values style))

(defmethod output-slowchange-cells (s slot values (style (eql 'emailstyle)))
  (output-slowchange-unicells s slot values style))

(defmethod output-slowchange-cells (s slot values (style (eql 'fastselector)))
  (output-slowchange-unicells s slot values style))

(defmethod output-slowchange-cells (s slot values (style (eql 'fancyselector)))
  (output-slowchange-unicells s slot values style))

(defmethod output-slowchange-cells (s slot values (style (eql 'glyph)))
  (output-slowchange-unicells s slot values style))

(defmethod output-slowchange-cells (s slot values (style (eql 'htmlstyle)))
  (output-slowchange-unicells s slot values style))

(defmethod output-slowchange-cells (s slot values (style (eql 'imagestyle)))
  (output-slowchange-unicells s slot values style))

(defmethod output-slowchange-cells (s slot values (style (eql 'menu)))
  (let (options)
    (when (setq options (findalternatives slot))
      (output-oldmenu s slot options values))))

(defmethod output-slowchange-cells (s slot values (style (eql 'password)))
  (output-slowchange-unicells s slot values style))

(defmethod output-slowchange-cells (s slot values (style (eql 'prettystyle)))
  (output-slowchange-unicells s slot values style))

(defmethod output-slowchange-cells (s slot values (style (eql 'radiobutton)))
  (output-slowchange-unicells s slot values style))

(defmethod output-slowchange-cells (s slot values (style (eql 'selector)))
  (output-slowchange-unicells s slot values style))

(defmethod output-slowchange-cells (s slot values (style (eql 'stringfield)))
  (output-slowchange-unicells s slot values style))

(defmethod output-slowchange-cells (s slot values (style (eql 'subframe)))
  (output-slowchange-unicells s slot values style))

(defmethod output-slowchange-cells (s slot values (style (eql 'tabulator)))
  (output-slowchange-unicells s slot values style))

(defmethod output-slowchange-cells (s slot values (style (eql 'text)))
  (output-slowchange-unicells s slot values style))

(defmethod output-slowchange-cells (s slot values (style (eql 'textarea)))
  (output-slowchange-unicells s slot values style))

(defmethod output-slowchange-cells (s slot values (style (eql 'typein)))
  (output-slowchange-unicells s slot values style))

(defmethod output-slowchange-cells (s slot values (style (eql 'urlstyle)))
  (output-slowchange-unicells s slot values style))

(defmethod output-slowchange-cells (s slot values style)
  (output-slowchange-unicells s slot values style))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod output-slowchange-cell (s slot value (style (eql 'checkbox)))
  (let (options)
    (when (setq options (findalternatives slot))
      (output-oldcheckbox s slot options (list value)))))

(defmethod output-slowchange-cell (s slot value (style (eql 'combobox)))
  (let (options)
    (when (setq options (findalternatives slot))
      (setq options (cons 'unknown options))
      (output-oldcombobox s slot options (prettyname value) 40))))

(defmethod output-slowchange-cell (s slot value (style (eql 'dateinput)))
  (output-olddateinput s slot value))

(defmethod output-slowchange-cell (s slot value (style (eql 'datestyle)))
  (when value
    (output-hidden s (stringize slot) (stringize value))
    (output-simple s value)))

(defmethod output-slowchange-cell (s slot value (style (eql 'dollarinput)))
  (output-olddollarinput s slot value 6))

(defmethod output-slowchange-cell (s slot value (style (eql 'dollarstyle)))
  (when value
    (output-hidden s slot value)
    (format s "$~$" value)))

(defmethod output-slowchange-cell (s slot value (style (eql 'emailstyle)))
  (when value
    (output-hidden s slot value)
    (format s "<a href='mailto:~A'><font color='red'>~A</font></a>" value value)))

(defmethod output-slowchange-cell (s slot value (style (eql 'fastselector)))
  (output-oldfastselector s slot value))

(defmethod output-slowchange-cell (s slot value (style (eql 'fancyselector)))
  (output-oldfancyselector s slot value))

(defmethod output-slowchange-cell (s slot value (style (eql 'glyph)))
  (when value
    (output-hidden s slot value)
    (output-handle s value)))

(defmethod output-slowchange-cell (s slot value (style (eql 'htmlstyle)))
  (when value
    (output-hidden s slot value)
    (format s "~A" value)))

(defmethod output-slowchange-cell (s slot value (style (eql 'imagestyle)))
  (when value
    (output-hidden s slot value)
    (format s "<img src='~A'/>" value)))

(defmethod output-slowchange-cell (s slot value (style (eql 'menu)))
  (let (options)
    (when (setq options (findalternatives slot))
      (output-oldmenu s slot options (list value)))))

(defmethod output-slowchange-cell (s slot value (style (eql 'password)))
  (format-password s slot (prettyname value) 40))

(defmethod output-slowchange-cell (s slot value (style (eql 'prettystyle)))
  (when value
    (output-hidden s slot value)
    (output-simple s value)))

(defmethod output-slowchange-cell (s slot value (style (eql 'radiobutton)))
  (let (options)
    (when (setq options (findalternatives slot))
      (setq options (cons nil options))
      (output-oldradiobutton s slot options value))))

(defmethod output-slowchange-cell (s slot value (style (eql 'selector)))
  (let (options)
    (when (setq options (findalternatives slot))
      (setq options (cons 'unknown options))
      (output-oldselector s slot options value))))

(defmethod output-slowchange-cell (s slot value (style (eql 'stringfield)))
  (format-oldstringfield s (stringize slot) (prettyname value) 40))

(defmethod output-slowchange-cell (s slot value (style (eql 'subframe)))
  (output-hidden s slot value))

(defmethod output-slowchange-cell (s slot value (style (eql 'tabulator)))
  (output-hidden s slot value))

(defmethod output-slowchange-cell (s slot value (style (eql 'text)))
  (format-oldstringfield s (stringize slot) (prettyname value) 40))

(defmethod output-slowchange-cell (s slot value (style (eql 'textarea)))
  (format-textarea s slot (prettyname value) 4 80))

(defmethod output-slowchange-cell (s slot value (style (eql 'typein)))
  (format-oldtypein s (stringize slot) (stringize value) 40))

(defmethod output-slowchange-cell (s slot value (style (eql 'urlstyle)))
  (when value
    (output-hidden s slot value)
    (format s "<a href='~A'>~A</a>" value (htmlify value))))

(defmethod output-slowchange-cell (s slot value style)
  (declare (ignore style))
  (output-hidden s slot value))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod output-slowchange-unicells (s slot values style)
  (format s "<table cellpadding='0' cellspacing='0'>")
  (format s "<tr><td>")
  (output-slowchange-cell s slot (car values) style) (crlf s)
  (format s "</td></tr>")
  (dolist (value (cdr values))
    (format s "<tr><td>")
    (output-slowchange-cell s slot value style) (crlf s)
    (format s "</td></tr>"))
  (format s "</table>")
  (crlf s))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod output-fastseek-cell (s slot value structure (style (eql 'menu)))
  (output-fastseek-cell s slot value structure 'selector))

(defmethod output-fastseek-cell (s slot value structure (style (eql 'selector)))
  (let (options)
    (cond ((setq options (fastoptions slot structure))
           (setq options (cons 'unknown options))
           (output-newselector s slot options value))
          (t (format-hidden s (stringize slot) "")))))

(defmethod output-fastseek-cell (s slot value structure (style (eql 'fastselector)))
  (let (options)
    (cond ((setq options (fastoptions slot structure))
           (setq options (cons 'unknown options))
           (output-newselector s slot options value))
          (t (format-hidden s (stringize slot) "")))))

(defmethod output-fastseek-cell (s slot value structure (style (eql 'fancyselector)))
  (let (options)
    (cond ((setq options (fastoptions slot structure))
           (setq options (cons 'unknown options))
           (output-newselector s slot options value))
          (t (format-hidden s (stringize slot) "")))))

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


;;;;


(defun output-fastfind (s structure)
  (let ((*buttons* 0))
    (format s "<form name='form1' action='~A?'>" *fastfindpage*)
    (format s "Find every <b>~A</b> with the following properties."
            (prettyname (cadr structure)))
    (format-hidden s "Object" (stringize (car structure))) (crlf s)
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


;;;;


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
      (output-fastview-cells s slot values structure (find-searchstyle slot))
      (format s "</th>")
      (crlf s))
  (format s "</tr>")
  (format s "</table>")
  (format s "</form>"))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; fastmaskpage
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defparameter *fastmaskpage* "fastmaskpage")

(defmethod process (s (command (eql 'fastmaskpage)) postlines)
  (let (dum start end)
    (cond ((setq dum (getf-post "Class" postlines))
           (setq dum (fastcomparestructure (read-user-string dum)))
           (setq start (or (read-value-string (getf-post "Start" postlines)) 1))
           (setq end (or (read-value-string (getf-post "End" postlines)) *count*))
           (process-fastmaskpage s dum start end))
          ((setq dum (getf-post "Structure" postlines))
           (setq start (or (read-value-string (getf-post "Start" postlines)) 1))
           (setq end (or (read-value-string (getf-post "End" postlines)) *count*))
           (process-fastmaskpage s (read-user-string dum) start end))
          (t (http-problem s "Bad request.")))))

(defun process-fastmaskpage (s structure start end)
  (format-html s) (crlf s)
  (format-head s)
  (format s "<title>fastmaskpage</title>") (crlf s)
  (format-javascript s) (crlf s)
  (finish-head s) (crlf s)
  (format-body s *bgcolor*) (crlf s)
  (output-header s)
  (format-border s)
  (process-fastmask s structure start end)
  (finish-border s)
  (output-footer s)
  (finish-body s) (crlf s)
  (finish-html s) (crlf s))


(defun process-fastmask (s structure start end)
  (let (objects sorter attributes results count)
    (setq objects (findinstances (viewconvert structure) *gui*))
    (when (setq sorter (find-sorter (cadr structure)))
      (setq objects (sortem objects sorter 'ascending)))
    (multiple-value-setq (objects count start end) (trim objects start end))
    (setq attributes (displayable-slots (cadr structure)))
    (setq results (prorequest `(ask-table ,objects ,attributes)))
    (output-fastmask s structure objects attributes results count start end)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun output-fastmask (s structure objects slots results count start end)
  (format s "<center>")
  (format s "<br/>")
  (cond ((= count 0)
         (format s "<table>")
         (format s "<tr><td align='center'>There are no answers.</td></tr>")
         (unless (emptystructurep structure)
           (format s "<tr><td>")
           (output-fastmask-inner s structure objects slots results)
           (format s "</td></tr>"))
         (format s "<tr><td>")
         (output-fastmask-create s (cadr structure) structure)
         (format s "</td></tr></table>"))
        ((= count 1)
         (format s "<table>")
         (format s "<tr><td align='center'>There is 1 answer.</td></tr>")
         (format s "<tr><td>")
         (output-fastmask-inner s structure objects slots results)
         (format s "</td></tr>")
         (format s "<tr><td>")
         (output-fastmask-create s (cadr structure) structure)
         (format s "</td></tr></table>"))
        ((and (= start 1) (geqp end count))
         (format s "<table>")
         (format s "<tr><td align='center'>There are ~D answers.</td></tr>" count)
         (format s "<tr><td>")
         (output-fastmask-inner s structure objects slots results)
         (format s "</td></tr>")
         (format s "<tr><td>")
         (output-fastmask-create s (cadr structure) structure)
         (format s "</td></tr></table>"))
        (t (format s "<table>")
           (format s "<tr><td align='center'>There are ~D answers.  The following table shows answers ~A through ~A.</td></tr>"
                   count start end)
           (format s "<tr><td>")
           (output-fastmask-inner s structure objects slots results)
           (format s "</td></tr><tr><td>")
           (multiple-value-setq (start end) (kerchunk count start end))
           (output-fastmask-create s (cadr structure) structure)
           (output-fastmask-display s (cadr structure) structure start end)
           (format s "</td></tr></table>")))
  (format s "</center>") (crlf s))

(defun output-fastmask-inner (s structure items slots results)
  (let (class nohandle (*buttons* 0))
    (setq class (cadr structure))
    (setq nohandle (findp `(nodisplay ,class handle) *interface*))
    (format s "<table bgcolor='~A' border='~A'>" *bgcolor* *border*) (crlf s)
    (format s "<tr>")
    (format s "<form name='form1' action='~A?' method='post'>" *fastmaskpage*) (crlf s)
    (format-hidden s "Object" (stringize (car structure))) (crlf s)
    (format-hidden s "Class" (stringize (cadr structure))) (crlf s)
    (unless nohandle
      (format s "<th>")
      (format s "<span style='cursor:pointer; text-decoration:underline; color:#000000'
                       onClick='postReplace(window.document.form1)'>")
      (format s (iconify class))
      (format s "</span>")
      (format s "</th>"))
    (do ((l slots (cdr l)) (values))
        ((null l))
        (setq values (getslotvals (car l) (cddr structure)))
        (format s "<th>")
        (output-fastshow-cells s (car l) values (find-searchstyle (car l)))
        (format s "</th>")
      (crlf s))
    (format s "</form>") (crlf s)
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
            (if (and vals (every #'(lambda (val) (numberp val)) vals))
                (format s "<td align='right'>")
                (format s "<td>"))
            (output-cells s (car slots) vals style)
            (format s "</td>"))
        (format s "</tr>")
        (crlf s))
    (format s "</table>") (crlf s)))

(defun output-fastmask-create (s class structure)
  (declare (ignore class))
  (unless (findp `(or (nocommand ,*gui* create) (nocreate ,*gui* ,(cadr structure))) *interface*)
    (format s "<form action='fastcreatepage?' method='post'>" (stringize structure))
    (format-hidden s "Class" (stringize (cadr structure)))
    (format-button s "Command" "Create")
    (format s " a new ~A." (prettify (cadr structure)))
    (format s "</form>") (crlf s)))

(defun output-fastmask-display (s class structure start end)
  (declare (ignore class))
  (format s "<form action='fastmaskpage?' method='post'>")
  (format-hidden s "Structure" (htmlify (prin1-to-string structure)))
  (format-button s "Command" "Display")
  (format s "answers ")
  (format-text s "Start" (princ-to-string start) 5)
  (format s " through ")
  (format-text s "End" (princ-to-string end) 5)
  (format s "</form>") (crlf s))


;;;;

(defun output-fastfind-multicells (s slot values style)
  (let (multivalued multiple)
    (setq multivalued (find-multivalued slot))
    (setq multiple (cdr values))
    (format s "<table cellpadding='0' cellspacing='0'>")
    (incf *buttons*)
    (format s "<tr>")
    (format s "<td width='32' valign='top'>")
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

;;;;


function remslot (obj)
 {var node = obj.parentNode.parentNode;
  //obj = getobject(node);
  node.parentNode.removeChild(node);
  //propagate(obj,obj);
  return true}



;;;;


(defun output-fastshow-multicells (s slot values style)
  (let (multivalued multiple)
    (setq multivalued (find-multivalued slot))
    (setq multiple (cdr values))
    (format s "<table cellpadding='0' cellspacing='0'>")
    (incf *buttons*)
    (format s "<tr><td valign='center'>")
    (when multivalued (output-add s *buttons*))
    (when multiple (output-delete s *buttons*))
    (format s "</td><td valign='center'>")
    (output-fastshow-cell s slot (car values) style) (crlf s)
    (format s "</td></tr>")
    (dolist (value (cdr values))
      (incf *buttons*)
      (format s "<tr><td valign='center'>")
      (when multivalued (output-add s *buttons*))
      (when multiple (output-delete s *buttons*))
      (format s "</td><td>")
      (output-fastshow-cell s slot value style) (crlf s)
      (format s "</td></tr>"))
    (format s "</table>")
    (crlf s)))


;;;;


(defun output-fastview-multicells (s slot values structure style)
  (let (multivalued multiple)
    (setq multivalued (find-multivalued slot))
    (setq multiple (cdr values))
    (format s "<table cellpadding='0' cellspacing='0'>")
    (incf *buttons*)
    (format s "<tr><td valign='center'>")
    (when multivalued (output-add s *buttons*))
    (when multiple (output-delete s *buttons*))
    (format s "</td><td valign='center'>")
    (output-fastview-cell s slot (car values) structure style) (crlf s)
    (format s "</td></tr>")
    (dolist (value (cdr values))
      (incf *buttons*)
      (format s "<tr><td valign='center'>")
      (when multivalued (output-add s *buttons*))
      (when multiple (output-delete s *buttons*))
      (format s "</td><td>")
      (output-fastview-cell s slot value structure style) (crlf s)
      (format s "</td></tr>"))
    (format s "</table>")
    (crlf s)))

(defun output-add (s x)
  (format s "<image src='~Ainfomaster/images/add.gif' border='0' onClick='postAnother(form1,\"~A\")'/>"
          *home* (stringize x)))

(defun output-delete (s x)
  (format s "<image src='~Ainfomaster/images/delete.gif' border='0' onClick='postRemoval(form1,\"~A\")'/>"
          *home* (stringize x)))


function postAnother (fobj,index)
 {postRequest(fobj.action,'Structure=' + anotherStructure(fobj,index) + '&Command=Refresh')}

function postRemoval (fobj,index)
 {postRequest(fobj.action,'Structure=' + removalStructure(fobj,index) + '&Command=Refresh')}



function anotherStructure (fobj,ind)
 {var str = '';
  str += '(' + fobj.elements[0].value;
  str += ' ' + fobj.elements[1].value;
  for (var i = 2; i < fobj.elements.length; i++)
      {if (fobj.elements[i].qualifier == 'skip') {str = str}
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
       else if (fobj.elements[i].qualifier == 'string')
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
      {if (fobj.elements[i].qualifier == 'skip') {str = str}
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
       else if (fobj.elements[i].qualifier == 'string')
               {if (fobj.elements[i].getAttribute('index') != ind && fobj.elements[i].value == '')
                   {str +=  ' ' + listify(fobj.elements[i].name, '')}
                else {str += ' ' + listify(fobj.elements[i].name, stringize(fobj.elements[i].value))}}
       else if (fobj.elements[i].getAttribute('index') != ind)
               {str += ' ' + listify(fobj.elements[i].name, escape(fobj.elements[i].value))}}
  str = str + ')';
  return str}


;;;;

(defun output-fastgallery-query (s structure)
  (let ((*buttons* 0))
    (format s "<form name='form1' action='~A?'>" *performative*)
    (format s "Find every <b>~A</b> with the following properties."
            (prettyname (cadr structure)))
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
        (format s "</trim>")
        (crlf s))
    (format s "</table>") (crlf s)
    (format s "</form>")
    'done))


(defun output-fastshow-unicells (s slot values style)
  (format s "<table cellpadding='0' cellspacing='0'>")
  (incf *buttons*)
  (format s "<tr><td width='32' valign='top'>")
  (output-empty-button s)
  (output-empty-button s)
  (format s "</td><td>")
  (output-fastshow-cell s slot (car values) style) (crlf s)
  (format s "</td></tr>")
  (dolist (value (cdr values))
    (incf *buttons*)
    (format s "<tr><td width='32' valign='top'>")
    (output-empty-button s)
    (output-empty-button s)
    (format s "</td><td>")
    (output-fastshow-cell s slot value style) (crlf s)
    (format s "</td></tr>"))
  (format s "</table>")
  (crlf s))

(defun output-fastshow-multicells (s slot values style)
  (let (multivalued multiple)
    (setq multivalued (find-multivalued slot))
    (setq multiple (cdr values))
    (format s "<table cellpadding='0' cellspacing='0'>")
    (incf *buttons*)

    (format s "<tr id='~A' qualifier='skip' style='display:None'>" (stringize slot))
    (format s "<td width='32' valign='top'>")
    (output-newslot s slot)
    (output-remslot s)
    (format s "</td><td>")
    (output-fastshow-cell s slot nil style)
    (format s "</td>")
    (format s "</tr>")

    (format s "<tr><td width='32' valign='top'>")
    (if multivalued (output-newslot s slot) (output-empty-button s))
    (output-empty-button s)
    (format s "</td><td>")
    (output-fastshow-cell s slot (car values) style) (crlf s)
    (format s "</td></tr>")

    (dolist (value (cdr values))
      (incf *buttons*)
      (format s "<tr><td width='32' valign='top'>")
      (if multivalued (output-newslot s slot) (output-empty-button s))
      (if (or multivalued multiple) (output-remslot s) (output-empty-button s))
      (format s "</td><td>")
      (output-fastshow-cell s slot value style) (crlf s)
      (format s "</td></tr>"))

    (format s "</table>")
    (crlf s))


(defun output-fasttableau-query (s structure)
  (let ((*buttons* 0))
    (format s "<form name='form1' action='~A?'>" *performative*)
    (format s "Find every <b>~A</b> with the following properties."
            (prettyname (cadr structure)))
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
        (format s "</trim>")
        (crlf s))
    (format s "</table>") (crlf s)
    (format s "</form>")
    'done))

(defun output-fastview-unicells (s slot values structure style)
  (format s "<table cellpadding='0' cellspacing='0'>")
  (incf *buttons*)
  (format s "<tr><td width='32' valign='top'>")
  (output-empty-button s)
  (output-empty-button s)
  (format s "</td><td>")
  (output-fastview-cell s slot (car values) structure style) (crlf s)
  (format s "</td></tr>")
  (dolist (value (cdr values))
    (incf *buttons*)
    (format s "<tr><td width='32' valign='top'>")
    (output-empty-button s)
    (output-empty-button s)
    (format s "</td><td>")
    (output-fastview-cell s slot value structure style) (crlf s)
    (format s "</td></tr>"))
  (format s "</table>")
  (crlf s))

(defun output-fastview-multicells (s slot values structure style)
  (let (multivalued multiple)
    (setq multivalued (find-multivalued slot))
    (setq multiple (cdr values))
    (format s "<table cellpadding='0' cellspacing='0'>")
    (incf *buttons*)

    (format s "<tr id='~A' qualifier='skip' style='display:None'>" (stringize slot))
    (format s "<td width='32' valign='top'>")
    (output-newslot s slot)
    (output-remslot s)
    (format s "</td><td>")
    (output-fastview-cell s slot nil structure style)
    (format s "</td>")
    (format s "</tr>")

    (format s "<tr><td width='32' valign='top'>")
    (if multivalued (output-newslot s slot) (output-empty-button s))
    (output-empty-button s)
    (format s "</td><td>")
    (output-fastview-cell s slot (car values) structure style) (crlf s)
    (format s "</td></tr>")

    (dolist (value (cdr values))
      (incf *buttons*)
      (format s "<tr><td width='32' valign='top'>")
      (if multivalued (output-newslot s slot) (output-empty-button s))
      (if (or multivalued multiple) (output-remslot s) (output-empty-button s))
      (format s "</td><td>")
      (output-fastview-cell s slot value structure style) (crlf s)
      (format s "</td></tr>"))

    (format s "</table>")
    (crlf s)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Old Stuff
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; image
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
#|
(defmethod process (s (dir (eql 'image)) postlines)
  (let (photo image original)
    (setq photo (read-user-string (cdar postlines)))
    (setq image (result 'photo.image photo *repository*))
    (setq original (result 'photo.original photo *repository*))
    (unless image (setq image original))
    (format-html s) (crlf s)
    (format-body s *bgcolor*) (crlf s)
    (format s "<table width='100%' border='2'><tr><td>")
    (format s "<a href='~A~A'>" *web* (if original original image))
    (format s "<img src='~A~A' width='100%'>" *web* image)
    (format s "</a>")
    (format s "</table>")
    (finish-body s) (crlf s)
    (finish-html s) (crlf s)
    'done))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; newgallery
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod process (s (dir (eql 'newgallery)) postlines)
  (let (album (*count* 1000))
    (setq album (read-user-string (cdar postlines)))
    (format-html s) (crlf s)
    (format s "<head>") (crlf s)
    (output-title s "Gallery") (crlf s)
    (format s "~A" (albumscript))
    (format s "</head>") (crlf s)
    (format-body s *bgcolor*) (crlf s)
    (output-header s)
    (format s "<h3>Album ~A:</h3>" (prettify album)) (crlf s)
    (outputnewgallery s (asks '(item ?x ?y) `(album.item ,album ?x ?y) nil *gui*))
    (output-footer s)
    (finish-body s) (crlf s)
    (finish-html s) (crlf s)
    'done))

(defun outputnewgallery (s items)
  (format s "<center><table width='90%' cellpadding='4' border='0'>")
  (do ((l items) (count 1) (note))
      ((or (null l) (> count *count*)))
      (format s "<tr valign='top'>")
      (do ((i 1 (1+ i)))
          ((> i 4))
          (format s "<td width='25%' align='center'>")
          (when l
            (setq note (or (result 'photo.note (cadar l) *gui*) ""))
            (when (stringp (caddar l)) (setq note (strappend (caddar l) note)))
            (format s (result 'photo.link (cadar l) *gui*))
            (format s "<br/>")
            (format s "<a href='~a' target='_blank'><img src='~Ainfoserver/examples/album/images/magnifier.gif'/></a> " (cadar l) *home*)
            (format s "<font face='verdana' size='2'><span id='~A'>~A</span></font>" count note)
            (when (eq *gui* 'insider)
              (format s "<IMG SRC='~Ainfoserver/examples/album/images/pencil.gif' onClick='editor(\"~A\",\"~A\")'/>" *home* (cadar l) count))
            (format s "<br/>")
            (format s "<br/>")
            (setq l (cdr l) count (1+ count)))
          (format s "</td>"))
      (format s "</tr>") (crlf s))
  (format s "</table></center>"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; output-gallery
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod output-gallery (s items (class (eql 'craft)) count start end)
  (declare (ignore class))
  (format s "<center>")
  (format s "<br/>")
  (cond ((= count 1) (format s "<b>There is 1 answer.</b>"))
        ((<= count *count*)
         (format s "<b>There are ~A items matching your search.</b>" count))
        (t (format s "<b>There are ~A items matching your search.</b>" count)
           (format s "<br/>")
           (format s "The following table shows items ~A - ~A." start end)))
  (format s "</center>")
  (format s "<center><table width='90%' cellpadding='4' border='0'>")
  (do ((l items) (count start))
      ((or (null l) (> count end)))
    (format s "<tr valign='top'>")
    (do ((i 1 (1+ i)) (note))
        ((> i 4))
      (format s "<td width='25%' align='center'>")
      (when l
        (setq note (or (askx '?x `(craft.note ,(car l) ?x) nil *gui*) ""))
        (format s (askx '?x `(craft.link ,(car l) ?x) nil *gui*))
        (format s "<br/>")
        (format s "<a href='~a' target='_blank'><img src='~Ainfoserver/examples/album/images/magnifier.gif'/></a> " (car l) *home*)
        (format s "<font face='verdana' size='2'><span id='~A'>~A</span></font>" count note)
        (when (eq *gui* 'insider)
          (format s "<img src='~Ainfoserver/examples/album/images/pencil.gif' onClick='editor('~A','~A')'/>" *home* (car l) count))
        (format s "<br/>")
        (format s "<br/>")
        (setq l (cdr l) count (1+ count)))
      (format s "</td>"))
    (format s "</tr>") (crlf s))
  (format s "</table></center>") (crlf s)
  'done)

(defmethod output-gallery (s items (class (eql 'photo)) count start end)
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
      (do ((i 1 (1+ i)) (note))
          ((> i 4))
          (format s "<td width='25%' align='center'>")
          (when l
            (setq note (or (askx '?x `(photo.note ,(car l) ?x) nil *gui*) ""))
            (format s (askx '?x `(photo.link ,(car l) ?x) nil *gui*))
            (format s "<br/>")
            (format s "<a href='~a' target='_blank'><img src='~Ainfoserver/examples/album/images/magnifier.gif'/></a> " *home* (car l))
            (format s "<font face='verdana' size='2'><span id='~A'>~A</span></font>" count note)
            (when (eq *gui* 'insider)
              (format s "<img src='~Ainfoserver/examples/album/images/pencil.gif' onClick='editor('~A','~A')'/>" *home* (car l) count))
            (format s "<br/>")
            (format s "<br/>")
            (setq l (cdr l) count (1+ count)))
        (format s "</td>"))
    (format s "</tr>") (crlf s))
  (format s "</table>")
  (format s "</center>") (crlf s)
  'done)

(defmethod output-item (s item (class (eql 'photo)))
  (let (note)
    (setq note (or (result 'photo.note item *gui*) ""))
    (format s (result 'photo.link item *gui*))
    (format s "<br/>")
    (format s "<a href='~a' target='_blank'><img src='~Ainfoserver/examples/album/images/magnifier.gif'/></a> " item *home*)
    (format s "<font face='verdana' size='2'><span id='~A'>~A</span></font>" item note)
    (when (eq *gui* 'insider)
      (format s "<img src='~Ainfoserver/examples/album/images/pencil.gif' onClick='editor('~A','~A')'/>" *home* item item))
    (format s "<br/>")
    (format s "<br/>")))

(defmethod output-item (s item (class (eql 'craft)))
  (let (structure)
    (setq structure (makedisplaystructure item class))
    (format s "<center>")
    (format s (result 'craft.link item *gui*))
    (format s "<br/>")
    (format s "<font face='verdana' size='2'>")
    (format s "<B>")
    (output-handle s item)
    (format s "</B>")
    (format s "</center>")
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

(defmethod output-item (s item (class (eql 'photo)))
  (let (structure)
    (setq structure (makedisplaystructure item class))
    (format s "<center>")
    (format s (result 'photo.link item *gui*))
    (format s "<br/>")
    (format s "<font face='verdana' size='2'>")
    (format s "<B>")
    (output-handle s item)
    (format s "</B>")
    (format s "</center>")
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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; showdirectorypage
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod process (s (dir (eql 'showdirectorypage)) postlines)
  (cond ((null postlines) (process-showdirectorypage-start s))
        (t (process-showdirectorypage-display s (cdar postlines)))))

(defun process-showdirectorypage-start (s)
    (format-html s) (crlf s)
    (format s "<head>") (crlf s)
    (output-title s "Gallery") (crlf s)
    (format s "</head>") (crlf s)
    (format-body s *bgcolor*) (crlf s)
    (output-header s)
    (format s "<br/>")
    (format s "<form action='showdirectorypage?' method='post'>")
    (format-text s "Directory" "gullible:multimedia:" 40) (crlf s)
    (format s "<br/>")
    (format s "<input type='submit' value='Submit'/>") (crlf s)
    (format s "</form>")
    (format s "<br/>")
    (output-footer s)
    (finish-body s) (crlf s)
    (finish-html s) (crlf s))

(defun process-showdirectorypage-display (s directory)
  (let (files (*count* 1000))
    (setq files (dirvisuals directory))
    (format-html s) (crlf s)
    (format s "<head>") (crlf s)
    (output-title s "Gallery") (crlf s)
    (format s "</head>") (crlf s)
    (format-body s *bgcolor*) (crlf s)
    (output-header s)
    (output-directory s files)
    (output-footer s)
    (finish-body s) (crlf s)
    (finish-html s) (crlf s)
    'done))

(defun output-directory (s items)
  (format s "<center><table width='90%' cellpadding='4' border='0'>")
  (do ((l items) (count 1))
      ((or (null l) (> count *count*)))
      (format s "<tr valign='top'>")
      (do ((i 1 (1+ i)))
          ((> i 4))
          (format s "<td width='25%' align='center'>")
          (when l
            (format s "<a href='~A~A.jpeg'><img src='~A~A.jpeg' height='120'/></a>" *web* (car l) *web* (car l))
            (format s "<br/>")
            (format s "<br/>") (crlf s)
            (setq l (cdr l) count (1+ count)))
          (format s "</td>"))
      (format s "</tr>") (crlf s))
  (format s "</table></center>"))

(defun dirvisuals (dir)
  (do ((l (directory dir) (cdr l)) (dum) (nl))
      ((null l) (nreverse nl))
      (when (find (pathname-type (car l)) '("jpeg" "jpg") :test #'equalp)
        (setq dum (filate (jpegname (abbreviate (namestring (car l))))))
        (setq nl (cons dum nl)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; editor
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod process (s (dir (eql 'editor)) postlines)
  (let (photo note)
    (setq photo (read-user-string (cdr (pop postlines))))
    (setq note (or (prorequest `(ask-one ?x (photo.note ,photo ?x))) ""))
    (format-html s) (crlf s)
    (output-head s "Edit Note") (crlf s)
    (format-body s "#dddddd") (crlf s)
    (format s "<center>")
    (format s "<br/>")
    (format s "<h3>Note for ~A</h3>" (prettify photo))
    (format s "<form>")
    (format-hidden s 'photo photo)
    (format-hidden s 'original note)
    (format-textarea s 'note note 20 80)
    (format s "<table width='30%'><tr>")
    (format s "<td width='33%' align='center'>")
    (format s "<input type='button' value='Cancel' onClick='window.close()'/>")
    (format s "</td><td width='34%' align='center'>")
    (format s "<input type='button' value='Revert' onClick='this.form.elements[2].value=this.form.elements[1].value'/>")
    (format s "</td><td width='33%' align='center'>")
    (format s "<input type='button' value='Record' onClick='{window.returnValue = this.form.elements[2].value; window.close()}'/>")
    (format s "</td>")
    (format s "</tr></table>")
    (format s "</form>")
    (format s "</center>")
    (finish-body s) (crlf s)
    (finish-html s) (crlf s)
    'done))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; prettify
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun prettify (x)
  (cond ((numberp x) (princ-to-string x))
        ((characterp x) (htmlify (princ-to-string x)))
        ((stringp x) (prettifystring x))
        ((find x '(unknown unknown1 unknown2 unknown3)) "")
        ((symbolp x) (htmlify (make-pretty-string x)))
        (t "")))

(defun prettifystring (x)
  (cond ((startstringp "/multimedia" x)
         (setq x (remove (code-char 182) x))
         (cond ((endstringp ".txt" x)
                (stringappend "<a href='" *web* x "' target='fullsize'><font color='red'>View Document</font></a>"))
               ((endstringp ".html" x)
                (stringappend "<a href='" *web* x "' target='fullsize'><font color='red'>" x "</font></a>"))
               ((endstringp ".mpg" x)
                (stringappend "<a href='file://" x "' target='fullsize'><font color=red>" x "</font></a>"))
               ((endstringp ".mov" x)
                (stringappend "<a href='file://" x "' target='fullsize'><font color=red>" x "</font></a>"))
               ((and (probe-file (macify (stringappend "/gullible/" x ".jpeg")))
                     (probe-file (macify (stringappend "/gullible/" x ".jpg"))))
                (stringappend "<a href='" *web* x ".jpeg' target='fullsize'><img src='" *web* x ".jpg'/></a>"))
               ((probe-file (macify (stringappend "/gullible/" x ".jpeg")))
                (stringappend "<a href='" *web* x ".jpeg' target='fullsize'><img src='" *web* x ".jpeg' height=120/></a>"))
               ((probe-file (macify (stringappend "/gullible/" x ".jpg")))
                (stringappend "<a href='" *web* x ".jpg' target='fullsize'><img src='" *web* x ".jpg' height=120/></a>"))
               (t x)))
        ((html-string? x) x)
        (t (htmlify x))))

(defun visual (dir)
  (setq dir (substitute #\: #\. (symbol-name dir)))
  (setq dir (stringappend "gullible:" dir ":*"))
  (do ((l (directory dir) (cdr l)))
      ((null l) "/multimedia/clipart/noimage")
      (when (find (pathname-type (car l)) '("jpeg" "jpg") :test #'equalp)
        (return (filate (jpegname (abbreviate (namestring (car l)))))))))

(defun visuals (dir)
  (setq dir (substitute #\: #\. (symbol-name dir)))
  (setq dir (stringappend "gullible:" dir ":*"))
  (do ((l (directory dir) (cdr l)) (dum) (nl))
      ((null l) (nreverse nl))
      (when (find (pathname-type (car l)) '("jpeg" "jpg") :test #'equalp)
        (setq dum (filate (jpegname (abbreviate (namestring (car l))))))
        (setq nl (cons dum nl)))))

(defun abbreviate (x)
  (subseq (namestring x) 9))

(defun filate (s)
  (strappend "/" (substitute #\/ #\: s)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; createallalbums
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun createallalbums ()
  (let ((*repository* (make-instance 'theory)))
    (createdirectories "gullible:multimedia:album:" 1)
    (createdirectories "gullible:multimedia:coolpix:" 0)
    (createdirectories "gullible:multimedia:kodak:" 1)
    (createdirectories "gullible:multimedia:maureen:" 0)
    (createdirectories "gullible:multimedia:umax:" 0)
    (createdirectories "gullible:multimedia:worlds:" 0)
    (dumpclass 'album *repository* "gullible:album.kif" 'ookif)
    'done))

(defun createdirectories (fn n)
  (let (subs)
    (setq subs (directory (stringappend fn "*") :files nil :directories t))
    (cond ((null subs) (createalbum fn (superdirectory fn)))
          ((= n 0) (createalbums fn (album fn)))
          (t (dolist (x subs) (createdirectories (namestring x) (1- n)))))))

(defun createalbums (fn directory)
  (let (subs)
    (setq subs (directory (stringappend fn "*") :files nil :directories t))
    (cond ((null subs) (createalbum fn directory))
          (t (dolist (x subs)
               (createalbums (namestring x) directory))))))

(defun createalbum (fn directory)
  (let (album)
    (setq album (album fn))
    (unless (doublep 'album.instance album *repository*)
      (insert `(album.instance ,album) *repository*)
      (insert `(album.directory ,album ,directory) *repository*)
      (insert `(album.sample ,album ,(visual album)) *repository*))
    album))

(defun album (s)
  (let (pos end)
    (setq pos (position #\: s))
    (setq end (position #\: s :from-end t))
    (intern (stringupcase (substitute #\. #\: (subseq s (1+ pos) end))))))

(defun superdirectory (s)
  (let (pos end)
    (setq pos (position #\: s))
    (setq end (position #\: s :from-end t))
    (setq end (position #\: s :end end :from-end t))
    (intern (stringupcase (substitute #\. #\: (subseq s (1+ pos) end))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; printalbum
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun printalbum (album directory)
  (let (item thumb image fn pos)
    (format t "    (album.instance ~A)" album) (terpri t)
    (format t "    (album.author   ~A michael_genesereth)" album) (terpri t)
    (dolist (pathname (directory (stringappend directory "*") :files t :directories nil))
      (when (equalp (pathname-type pathname) "jpeg")
        (setq item (photoname (namestring pathname)))
        (format t "    (album.item     ~(~A~) ~(~A~))" album item) (terpri t)))
    (dolist (pathname (directory (stringappend directory "*") :files t :directories nil))
      (cond ((not (equalp (pathname-type pathname) "jpeg")))
            ((doublep 'photo.instance (setq item (photoname (namestring pathname))) *repository*)
             (terpri t)
             (format t "    (photo.instance ~(~A~))" item) (terpri t)
             (dolist (slot (nreverse (set-difference (slots-of-class 'photo) '(photo.album photo.link photo.commentary))))
               (dolist (val (asks '?y (list slot item '?y) nil *repository*))
                 (princ "    (" t)
                 (format t "~(~A~)" slot)
                 (princ " " t) (formatn t " " (- 14 (length (symbol-name slot))))
                 (format t "~(~A~)" item)
                 (princ " " t)
                 (format t "~(~S~)" val)
                 (princ ")" t)
                 (terpri t))))
            (t (setq fn (namestring pathname))
               (setq image (filate (abbreviate fn)))
               (setq pos (position #\. fn :from-end t))
               (setq fn (strappend (subseq fn 0 pos) ".jpg"))
               (setq thumb (filate (abbreviate fn)))
               (terpri t)
               (format t "    (photo.instance ~(~A~))" item) (terpri t)
               (format t "    (photo.subject  ~(~A~) something)" item) (terpri t)
               (format t "    (photo.object   ~(~A~) something)" item) (terpri t)
               (format t "    (photo.activity ~(~A~) something)" item) (terpri t)
               (format t "    (photo.location ~(~A~) something)" item) (terpri t)
               (format t "    (photo.state    ~(~A~) something)" item) (terpri t)
               (format t "    (photo.year     ~(~A~) something)" item) (terpri t)
               (format t "    (photo.author   ~(~A~) michael_genesereth)" item) (terpri t)
               (format t "    (photo.thumb    ~(~A~) ~S)" item thumb) (terpri t)
               (format t "    (photo.image    ~(~A~) ~S)" item image) (terpri t)
               (format t "    (photo.note     ~(~A~) \"\")" item) (terpri t)
               (format t "    (photo.release  ~(~A~) 14)" item) (terpri t))))
    album))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; javascript
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun albumscript ()
  "<script type='text/javascript' language='javascript'>

function upload ()
 {document.getElementById('uploader').style.display='none';
  document.getElementById('waiting').style.display='';
  document.form1.Filename.value=document.form1.Content.value;
  document.form1.submit()}

  function editor (photo,count)
   {var cell = document.getElementById(count);
    var note = showModalDialog('/insider/editor?photo=' + photo);
    if (note)
       {cell.innerHTML = note;
        postRequest('triple?','slot=photo.note&object='+photo+'&value=\"'+note+'\"')}}

  var http_request = false;

  function postRequest (url,args)
   {http_request = false;
    http_request = new XMLHttpRequest();
    if (http_request.overrideMimeType)
       {http_request.overrideMimeType('text/xml')};
    if (!http_request)
       {alert('Giving up: Cannot create an XMLHTTP instance');
        return false};
    http_request.onreadystatechange = alertResponse;
    http_request.open('POST', url, true);
    http_request.send(args)}

  function alertResponse ()
   {if (http_request.readyState == 4)
       {if (http_request.responseText)
           {return true}
        else {alert('There was a problem with the request.')}}}

   </script>")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; renaming
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
#|
(dolist (x (cdr *dir*))
  (rename-file x (make-pathname :directory "gullible:multimedia:olympus:2006:" :name (pathname-name x) :type "jpeg")))
|#
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; checkarchive
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod checkmedia ()
  (+ (checkcrafts)
     (checkmovies)
     (checkphotos)
     (checkstories)
     (checkworlds)))

(defun checkcrafts ()
  (do ((l (instances 'craft *repository*) (cdr l)) (dum) (n 0))
      ((null l) n)
      (when (and (setq dum (result 'craft.image (car l) *repository*))
                 (not (probe-file (filize dum))))
        (setq n (1+ n))
        (fresh-line)
        (format t "Missing image - (craft.image ~S ~S)" (car l) dum))
      (when (and (setq dum (result 'craft.thumb (car l) *repository*))
                 (not (probe-file (filize dum))))
        (setq n (1+ n))
        (fresh-line)
        (format t "Missing thumb - (craft.thumb ~S ~S)" (car l) dum))))

(defun checkmovies ()
  (do ((l (instances 'movie *repository*) (cdr l)) (dum) (n 0))
      ((null l) n)
      (when (and (setq dum (result 'movie.mpeg (car l) *repository*))
                 (not (probe-file (filize dum))))
        (setq n (1+ n))
        (fresh-line)
        (format t "Missing movie - (movie.mpeg ~S ~S)" (car l) dum))))

(defun checkphotos ()
  (do ((l (instances 'photo *repository*) (cdr l)) (dum) (n 0))
      ((null l) n)
      (when (and (setq dum (result 'photo.thumb (car l) *repository*))
                 (not (probe-file (filize dum))))
        (setq n (1+ n))
        (fresh-line)
        (format t "Missing thumb - (photo.thumb ~S ~S)" (car l) dum))
      (when (and (setq dum (result 'photo.image (car l) *repository*))
                 (not (probe-file (filize dum))))
        (setq n (1+ n))
        (fresh-line)
        (format t "Missing image - (photo.image ~S ~S)" (car l) dum))
      (when (and (setq dum (result 'photo.original (car l) *repository*))
                 (not (probe-file (filize dum))))
        (setq n (1+ n))
        (fresh-line)
        (format t "Missing original - (photo.original ~S ~S)" (car l) dum))))

(defun checkstories ()
  (do ((l (instances 'story *repository*) (cdr l)) (dum) (n 0))
      ((null l) n)
      (when (and (setq dum (result 'story.link (car l) *repository*))
                 (not (probe-file (filize dum))))
        (setq n (1+ n))
        (fresh-line)
        (format t "Missing story - (story.link ~S ~S)" (car l) dum))))

(defun checkworlds ()
  (do ((l (instances 'world *repository*) (cdr l)) (dum) (n 0))
      ((null l) n)
      (when (and (setq dum (result 'world.mpeg (car l) *repository*))
                 (not (probe-file (filize dum))))
        (setq n (1+ n))
        (fresh-line)
        (format t "Missing world - (world.mpeg ~S ~S)" (car l) dum))))

(defun filize (s)
  (stringappend "gullible:" (substitute #\: #\/ s)))

(defun webize (s)
  (strappend "file:///" (substitute #\/ #\: s)))

(defun jpegize (x)
  (setq x (stringdowncase (symbol-name x)))
  (stringappend "gullible:multimedia:" (substitute #\: #\. x) ".jpeg"))

(defun jpgize (x)
  (setq x (stringdowncase (symbol-name x)))
  (stringappend "gullible:multimedia:" (substitute #\: #\. x) ".jpg"))

(defun movize (x)
  (setq x (stringdowncase (symbol-name x)))
  (stringappend "gullible:multimedia:" (substitute #\: #\. x) ".mov"))

(defun checkarchive (archive private directory)
  (setq private (directory private))
  (setq directory (directory directory))
  (dolist (x (directory archive))
    (setq x (namestring x))
    (cond ((not (substringp ".jpg" x)))
          ((find x private :test #'sameimagep))
          ((find x directory :test #'sameimagep))
          (t (fresh-line)
             (format t (unixize x))))))

(defun sameimagep (x y)
  (setq x (namestring x))
  (setq y (namestring y))
  (equalp (subseq x (- (length x) 7) (- (length x) 4))
          (subseq y (- (length y) 7) (- (length y) 4))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; checkdirectory
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun checkdirectory (dir)
  (dolist (x (directory dir))
    (setq x (unixize (namestring x)))
    (when (and (or (substringp ".jpeg" x) (substringp ".jpg" x))
               (not (indexees x *repository*)))
      (fresh-line)
      (format t x))))

(defun unixize (s)
  (substitute #\/ #\: (subseq s 8)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; createallphotos
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun createallphotos (fn)
  (cond ((substringp ".jpeg" fn) (createphoto fn) (createjpeg fn))
        ((substringp ".jpg" fn) (createthumb fn))
        ((not (directoryp fn)) nil)
        (t (dolist (x (directory (strappend fn "*") :directories t))
             (createallphotos (namestring x)))
           'done)))

(defmethod createphoto (fn)
  (save `(photo.instance ,(photoname fn)) *repository*))

(defmethod createjpeg (fn)
  (save `(photo.image ,(photoname fn) ,fn) *repository*))

(defmethod createthumb (fn)
  (save `(photo.thumb ,(photoname fn) ,fn) *repository*))

(defun photoname (fn)
  (let (pos end)
    (setq pos (position #\: fn))
    (setq pos (position #\: fn :start (1+ pos)))
    (setq end (position #\. fn :from-end t))
    (intern (stringupcase (substitute #\. #\: (subseq fn (1+ pos) end))))))

(defun photolink (fn)
  (let (pos end)
    (setq pos (position #\: fn))
    (setq end (position #\. fn :from-end t))
    (substitute #\/ #\: (subseq fn pos end))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; createdirectory
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun createdirectory (dir)
  (cond ((not (directoryp dir)) (createobject dir))
        (t (dolist (x (directory (strappend dir "*") :directories t))
             (createdirectory (namestring x)))
           'done)))

(defun createobject (fn)
  (let (type)
    (setq type (pathname-type fn))
    (cond ((equalp type "jpeg")
           (save `(photo.instance ,(photoname fn)) *repository*)
           (save `(photo.image ,(photoname fn) ,fn) *repository*))
          ((equalp type "rm")
           (save `(movie.instance ,(filatenew (jpegname (shortenstring fn)))) *repository*))
          ((equalp type "wmv")
           (save `(movie.instance ,(filatenew (jpegname (shortenstring fn)))) *repository*))
          ((equalp type "mpg")
           (save `(movie.instance ,(filatenew (jpegname (shortenstring fn)))) *repository*))
          ((equalp type "mov")
           (save `(movie.instance ,(filatenew (jpegname (shortenstring fn)))) *repository*))
          ((equalp type "txt")
           (save `(story.instance ,(filatenew (jpegname (shortenstring fn)))) *repository*))
          ((equalp type "html")
           (save `(story.instance ,(filatenew (jpegname (shortenstring fn)))) *repository*)))))

(defun shortenstring (x)
  (subseq (namestring x) 9))

(defun jpegname (s)
  (subseq s 0 (position #\. s :from-end t)))

(defun filatenew (s)
  (strappend "file:///" (substitute #\/ #\: s)))
|#


;;;;

(defun newprettify (slot x)
  (cond ((eq x 'unknown) (stringappend "Any " (iconify slot)))
        (t (prettyname x))))


;;;;



(defun output-newslot (s x)
  (format s "<image src='~Ainfomaster/images/add.gif' border='0' onClick='newslot(\"~A\",this)'/>"
          *home* (stringize x)))

(defun output-remslot (s)
  (format s "<image src='~Ainfomaster/images/delete.gif' border='0' onClick='remslot(this)'/>"
          *home*))

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

    (format s "<tr id='~A' qualifier='skip' style='display:None'>" (stringize slot))
    (format s "<td width='32' valign='top'>")
    (output-newslot s slot)
    (output-delslot s)
    (format s "</td><td>")
    (output-fastfind-cell s slot nil style)
    (format s "</td>")
    (format s "</tr>")

    (format s "<tr><td width='32' valign='top'>")
    (if multivalued (output-newslot s slot) (output-empty-button s))
    (output-empty-button s)
    (format s "</td><td>")
    (output-fastfind-cell s slot (car values) style) (crlf s)
    (format s "</td></tr>")

    (dolist (value (cdr values))
      (incf *buttons*)
      (format s "<tr><td width='32' valign='top'>")
      (if multivalued (output-newslot s slot) (output-empty-button s))
      (if (or multivalued multiple) (output-delslot s) (output-empty-button s))
      (format s "</td><td>")
      (output-fastfind-cell s slot value style) (crlf s)
      (format s "</td></tr>"))

    (format s "</table>")
    (crlf s)))

function newslot (slot,obj)
 {var node = obj.parentNode.parentNode;
  var copy = document.getElementById(slot).cloneNode(true);
  node.parentNode.insertBefore(copy,node.nextSibling);
  copy.setAttribute('qualifier','active');
  copy.style.display = '';
  return true}

function remslot (obj)
 {var node = obj.parentNode.parentNode;
  node.parentNode.removeChild(node);
  postReplace(form1);
  return true}

;;;;

(defun output-fastshow-unicells (s slot values style)
  (format s "<table cellpadding='0' cellspacing='0'>")
  (incf *buttons*)
  (format s "<tr><td>")
  (output-fastshow-cell s slot (car values) style) (crlf s)
  (format s "</td></tr>")
  (dolist (value (cdr values))
    (format s "<tr><td>")
    (incf *buttons*)
    (output-fastshow-cell s slot value style) (crlf s)
    (format s "</td></tr>"))
  (format s "</table>") (crlf s)
  'done)

(defun output-fastshow-multicells (s slot values style)
  (format s "<table cellpadding='0' cellspacing='0'>")
  (dolist (value values)
    (format s "<tr><td>")
    (incf *buttons*)
    (output-fastshow-cell s slot value style) (crlf s)
    (format s "</td></tr>"))
  (when (or (null values) (find-multivalued slot))
    (format s "<tr><td>")
    (incf *buttons*)
    (output-fastshow-cell s slot nil style) (crlf s)
    (format s "</td></tr>"))
  (format s "</table>") (crlf s)
  'done)
