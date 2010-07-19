;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; (c) Copyright 1999-2006 by Michael Genesereth.  All rights reserved.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(eval-when (compile load eval)
  (proclaim '(special *manager* *string* *max-line-size* *trace*)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Loading and Dumping files
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod loadfile (fn target lang meta)
  (declare (ignore fn target lang meta))
  "Loadfile works with baskets only.")

(defmethod loadfile (fn (target symbol) lang meta)
  (cond ((and (boundp target) (not (symbolp (symbol-value target))))
         (loadfile fn (symbol-value target) lang meta))
        (t (call-next-method fn target lang meta))))

(defmethod loadfile (fn (target theory) lang meta)
  (dolist (sent (read-file-contents fn lang))
    (cond ((atom sent) (insert sent target))
          ((find (car sent) '(not unprovable)) (drop (cadr sent) target))
          (t (insert sent target))))
  (when (eq meta 'yes) (introspect target))
  'done)

(defmethod loaddata (fn target)
  (loadfile fn target 'kif 'no))

(defmethod read-file-contents (fn lang)
  (declare (ignore fn lang))
  "Unknown format.")


(defmethod unloadfile (fn target lang meta)
  (declare (ignore fn target lang meta))
  "Unloadfile works with baskets only.")

(defmethod unloadfile (fn (target symbol) lang meta)
  (cond ((and (boundp target) (not (symbolp (symbol-value target))))
         (unloadfile fn (symbol-value target) lang meta))
        (t (call-next-method fn target lang meta))))

(defmethod unloadfile (fn (target theory) lang meta)
  (dolist (sent (read-file-contents fn lang)) (drop sent target))
  (when (eq meta 'yes) (introspect target))
  'done)


(defmethod dumpagent (source fn lang)
  (declare (ignore source fn lang))
  "DumpAgent works with baskets only.")

(defmethod dumpagent ((source symbol) fn lang)
  (cond ((and (boundp source) (not (symbolp (symbol-value source))))
         (dumpagent (symbol-value source) fn lang))
        (t (call-next-method source fn lang))))

(defmethod dumpfile (source fn lang)
  (dumpagent source fn lang))


(defmethod dumpclass (class source fn lang)
  (declare (ignore class source fn lang))
  "DumpClass works with baskets only.")

(defmethod dumpclass (class (source symbol) fn lang)
  (cond ((and (boundp source) (not (symbolp (symbol-value source))))
         (dumpclass class (symbol-value source) fn lang))
        (t (call-next-method class source fn lang))))


(defmethod dumprelation (rel source fn lang)
  (declare (ignore rel source fn lang))
  "DumpRelation works with baskets only.")

(defmethod dumprelation (rel (source symbol) fn lang)
  (cond ((and (boundp source) (not (symbolp (symbol-value source))))
         (dumpRelation rel (symbol-value source) fn lang))
        (t (call-next-method rel source fn lang))))


(defmethod dumprules (source fn lang)
  (declare (ignore source fn lang))
  "DumpRules works with baskets only.")

(defmethod dumprules ((source symbol) fn lang)
  (cond ((and (boundp source) (not (symbolp (symbol-value source))))
         (dumprules (symbol-value source) fn lang))
        (t (call-next-method source fn lang))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; KIF files
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod read-file-contents (fn (lang (eql 'kif)))
  (with-open-file (f fn :direction :input)
    (do ((a (read f nil) (read f nil)) (nl))
	((null a) (nreverse nl))
        (setq nl (cons a nl)))))

(defmethod dumptheory ((th theory) fn (lang (eql 'kif)))
  (dump-theory th fn)
  'done)

(defmethod dumpagent ((source theory) fn (lang (eql 'kif)))
  (let (contents)
    (setq contents (contents source))
    (with-open-file (f fn :direction :output :if-exists :supersede)
      (when (eq source *manager*)
        (do ((l *sentences* (cdr l)))
            ((null l))
            (cond ((equalp (car l) (car contents))
                   (setq contents (cdr contents)))
                  (t (print `(unprovable ,(car l)) f)))))
      (do ((m contents (cdr m)))
          ((null m) 'done)
        (print (car m) f)))))

(defmethod dumpclass (class (source theory) fn (lang (eql 'kif)))
  (with-open-file (f fn :direction :output :if-exists :supersede)
    (outputclassookif f class source)
    (formatn f ";;;;;;;;;;;;;;;;" 5) (terpri f)
    (formatn f ";;;;;;;;;;;;;;;;" 5) (terpri f)
    (formatn f ";;;;;;;;;;;;;;;;" 5) (terpri f)
    'done))

(defmethod dumprelation (rel (source theory) fn (lang (eql 'kif)))
  (with-open-file (f fn :direction :output :if-exists :supersede)
    (outputrelsents f rel source)))

(defmethod dumprules ((source theory) fn (lang (eql 'kif)))
  (with-open-file (f fn :direction :output :if-exists :supersede)
    (do ((m (contents source) (cdr m)))
        ((null m) 'done)
        (when (and (listp (car m)) (find (caar m) '(<= =>)))
          (outputkif f (car m))))))

(defun outputkif (s kif)
  (cond ((atom kif) (format s "~S" kif))
        ((null (cddr kif)) (format s "~A" kif))
        ((eq '<= (car kif))
         (format s "(<= ~S" (cadr kif))
         (dolist (literal (cddr kif))
           (format s "~%    ~S" literal))
         (format s ")"))
        ((eq '=> (car kif))
         (format s "(=> ~S" (cadr kif))
         (dolist (literal (cddr kif))
           (format s "~%    ~S" literal))
         (format s ")"))
        ((eq '==> (car kif))
         (format s "(==> ~S" (cadr kif))
         (dolist (literal (cddr kif))
           (format s "~%     ~S" literal))
         (format s ")"))
        (t (format s "~S" kif))))

(defun outputrelsents (s rel th)
  (formatn s ";;;;;;;;;;;;;;;;" 5) (terpri s)
  (format s ";;; ~A" rel) (terpri s)
  (formatn s ";;;;;;;;;;;;;;;;" 5) (terpri s)
  (do ((m (indexees rel th) (cdr m)))
      ((null m) (terpri s) (terpri s) 'done)
      (when (and (listp (car m)) (eq rel (caar m)))
        (print (car m) s))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; OOKIF files
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod read-file-contents (fn (lang (eql 'ookif)))
  (with-open-file (f fn :direction :input)
    (do ((a (read f nil) (read f nil)) (nl))
	((null a) (nreverse nl))
        (setq nl (cons a nl)))))

(defmethod dumptheory ((th theory) fn (lang (eql 'ookif)))
  (with-open-file (f fn :direction :output :if-exists :supersede)
    (dolist (class (get-classes th))
      (outputclassookif f class th)
      (terpri f))
    (formatn f ";;;;;;;;;;;;;;;;" 5) (terpri f)
    (formatn f ";;;;;;;;;;;;;;;;" 5) (terpri f)
    (formatn f ";;;;;;;;;;;;;;;;" 5) (terpri f)
    'done))

(defmethod dumpagent ((source theory) fn (lang (eql 'ookif)))
  (let (predicate (*print-case* :downcase))
    (with-open-file (f fn :direction :output :if-exists :supersede)
      (formatn f ";;;;;;;;;;;;;;;;" 5) (terpri f)
      (format f ";;; ~A" (name source)) (terpri f)
      (formatn f ";;;;;;;;;;;;;;;;" 5) (terpri f)
      (dolist (obj (get-objects source))
        (terpri f)
        (when (setq predicate (find-predicate (classify obj source)))
          (format f "    (~A ~A)" predicate obj))
        (terpri f)
        (dolist (slot (get-slots obj source))
          (dolist (fact (get-facts slot obj source))
            (princ "    " f) (prin1 fact f) (terpri f))))
      (terpri f)
      (formatn f ";;;;;;;;;;;;;;;;" 5) (terpri f)
      (formatn f ";;;;;;;;;;;;;;;;" 5) (terpri f)
      (formatn f ";;;;;;;;;;;;;;;;" 5) (terpri f)
      'done)))

(defmethod dumpclass (class (source theory) fn (lang (eql 'ookif)))
  (with-open-file (f fn :direction :output :if-exists :supersede)
    (outputclassookif f class source)
    (formatn f ";;;;;;;;;;;;;;;;" 5) (terpri f)
    (formatn f ";;;;;;;;;;;;;;;;" 5) (terpri f)
    (formatn f ";;;;;;;;;;;;;;;;" 5) (terpri f)
    'done))

(defun outputclassookif (f class source)
  (let (predicate slots maxlen (*print-case* :downcase))
    (formatn f ";;;;;;;;;;;;;;;;" 5) (terpri f)
    (format f ";;; ~A" class) (terpri f)
    (formatn f ";;;;;;;;;;;;;;;;" 5) (terpri f)
    (setq predicate (find-predicate class))
    (setq slots (remove-if #'(lambda (x) (fullviewp x source)) (attributes class)))
    (setq maxlen (maxlength (cons predicate slots)))
    (dolist (obj (sort (instances class source) #'lessp))
      (terpri f)
      (princ "    (" f)
      (prin1 predicate f)
      (princ " " f) (formatn f " " (- maxlen (length (symbol-name predicate))))
      (prin1 obj f)
      (princ ")" f)
      (terpri f)
      (dolist (slot slots)
        (dolist (val (results slot obj source))
          (princ "    (" f)
          (prin1 slot f)
          (princ " " f) (formatn f " " (- maxlen (length (symbol-name slot))))
          (prin1 obj f)
          (princ " " f)
          (prin1 val f)
          (princ ")" f)
          (terpri f))))
    (terpri f)
    'done))

(defun outputclassookif (f class source)
  (let (predicate slots maxlen (*print-case* :downcase))
    (formatn f ";;;;;;;;;;;;;;;;" 5) (terpri f)
    (format f ";;; ~A" class) (terpri f)
    (formatn f ";;;;;;;;;;;;;;;;" 5) (terpri f)
    (setq predicate (find-predicate class))
    (setq slots (attributes class))
    (setq maxlen (maxlength (cons predicate slots)))
    (dolist (obj (sort (instances class source) #'lessp))
      (terpri f)
      (princ "    (" f)
      (prin1 predicate f)
      (princ " " f) (formatn f " " (- maxlen (length (symbol-name predicate))))
      (prin1 obj f)
      (princ ")" f)
      (terpri f)
      (dolist (slot slots)
        (dolist (val (trues '?y (list slot obj '?y) source))
          (princ "    (" f)
          (prin1 slot f)
          (princ " " f) (formatn f " " (- maxlen (length (symbol-name slot))))
          (prin1 obj f)
          (princ " " f)
          (prin1 val f)
          (princ ")" f)
          (terpri f))))
    (terpri f)
    'done))
   
(defun get-objects (th)
  (do ((l (contents th) (cdr l)) (nl))
      ((null l)  (nreverse nl))
      (when (atom (cadar l)) (setq nl (adjoin (cadar l) nl)))))

(defun get-classes (th)
  (do ((l (contents th) (cdr l)) (class) (nl))
      ((null l) (nreverse (remove-duplicates nl)))
      (when (and (cdar l) (null (cddar l))
                 (setq class (findx '?c `(predicate ?c ,(caar l)) *manager*)))
        (setq nl (cons class nl)))))

(defun get-slots (x th)
  (do ((l (indexees x th) (cdr l)) (nl))
      ((null l) (nreverse nl))
      (when (and (eq (cadar l) x) (cddar l)) (setq nl (adjoin (caar l) nl)))))

(defun get-facts (slot x th)
  (do ((l (indexees x th) (cdr l)) (nl))
      ((null l) (nreverse nl))
      (when (and (eq slot (caar l)) (eq x (cadar l)))
        (setq nl (cons (car l) nl)))))

(defun get-instances (class th)
  (sort (instances class th) #'lessp))

(defun get-values (slot x th)
  (do ((l (indexees x th) (cdr l)) (nl))
      ((null l) (nreverse nl))
      (when (and (eq slot (caar l)) (eq x (cadar l)))
        (setq nl (cons (caddar l) nl)))))

(defun maxlength (items)
  (do ((l items (cdr l)) (len 0))
      ((null l) len)
      (when (> (length (symbol-name (car l))) len)
        (setq len (length (symbol-name (car l)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; RDF Files
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod read-file-contents (fn (lang (eql 'rdf)))
  (rdfsentences (parsexml (scanxml (read-from-file fn)))))

(defun rdfsentences (tree)
  (let (*sentences*)
    (cond ((atom tree))
          ((atom (car tree)))
          ((eq 'rdf.rdf (caar tree)) (dolist (form (cdr tree)) (rdfexp form)))
          (t (rdfexp tree)))
    (nreverse *sentences*)))

(defun rdfexp (tree)
  (let (class object)
    (cond ((atom tree) tree)
          ((atom (car tree)) nil)
          ((setq object (rdfname (car tree)))
           (setq class (caar tree))
           (push (list (prefix class 'instance) object) *sentences*)
           (do ((l (cdr tree) (cdr l)) (slot) (val))
               ((null l) object)
               (setq slot (caaar l))
               (cond ((cdar l)
                      (setq val (rdfexp (cadar l)))
                      (push (list slot object val) *sentences*))
                     ((setq val (rdfvalue (caar l)))
                      (push (list slot object val) *sentences*))))))))

(defun rdfname (tree)
  (dolist (attr (cdr tree))
    (when (and (listp attr) (eq (car attr) '=) (eq (cadr attr) 'rdf.id))
      (return (read-value-string (caddr attr))))))

(defun rdfvalue (tree)
  (dolist (attr (cdr tree))
    (when (and (listp attr) (eq (car attr) '=) (eq (cadr attr) 'rdf.resource))
      (return (caddr attr)))))

(defmethod dumpagent ((source theory) fn (lang (eql 'rdf)))
  (let (class)
    (with-open-file (f fn :direction :output :if-exists :supersede)
      (format f "<?xml version=\"1.0\"?>") (terpri f) (terpri f)
      (format f "<rdf:RDF>") (terpri f)
      (dolist (obj (get-objects source))
        (setq class (classify obj source))
        (format f "  <~A OID=\"~A\">" class obj) (terpri f)
        (dolist (slot (get-slots obj source))
          (dolist (val (get-values slot obj source))
            (format f "    <~A>" slot)
            (if (string-slot-p slot) (format f (htmlify val))
                (format f "<THING OID=\"~A\"/>" val))
            (format f "</~A>" slot) (terpri f)))
        (format f "  </~A>" class) (terpri f))
      (format f "</rdf:RDF>") (terpri f)
      'done)))

(defmethod dumpclass (class (source theory) fn (lang (eql 'rdf)))
  (with-open-file (f fn :direction :output :if-exists :supersede)
    (format f "<?xml version=\"1.0\"?>") (terpri f) (terpri f)
    (format f "<rdf:RDF>") (terpri f)
    (outputclasssxml f class source)
    (format f "</rdf:RDF>") (terpri f)
    'done))

(defun outputclassrdf (s class source)
  (dolist (obj (get-instances class source))
    (format s "  <~A OID=\"~A\">" class obj)
    (terpri s)
    (dolist (slot (attributes class))
      (dolist (val (get-values slot obj source))
        (format s "    <~A>" slot)
        (if (string-slot-p slot) (format s (htmlify val))
            (format s "<THING OID=\"~A\"/>" val))
        (format s "</~A>" slot)
        (terpri s)))
    (format s "  </~A>" class)
    (terpri s))
  (terpri s))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; SXML Files
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod read-file-contents (fn (lang (eql 'sxml)))
  (sxmlsentences (parsexml (scanxml (read-from-file fn)))))

(defmethod dumpagent ((source theory) fn (lang (eql 'sxml)))
  (let (class)
    (with-open-file (f fn :direction :output :if-exists :supersede)
      (format f "<?xml version=\"1.0\"?>") (terpri f) (terpri f)
      (format f "<SXML>") (terpri f)
      (dolist (obj (get-objects source))
        (setq class (classify obj source))
        (format f "  <~A OID=\"~A\">" class obj) (terpri f)
        (dolist (slot (get-slots obj source))
          (dolist (val (get-values slot obj source))
            (format f "    <~A>" slot)
            (if (string-slot-p slot) (format f (htmlify val))
                (format f "<THING OID=\"~A\"/>" val))
            (format f "</~A>" slot) (terpri f)))
        (format f "  </~A>" class) (terpri f))
      (format f "</SXML>") (terpri f)
      'done)))

(defmethod dumpclass (class (source theory) fn (lang (eql 'sxml)))
  (with-open-file (f fn :direction :output :if-exists :supersede)
    (format f "<?xml version=\"1.0\"?>") (terpri f) (terpri f)
    (format f "<SXML>") (terpri f)
    (outputclasssxml f class source)
    (format f "</SXML>") (terpri f)
    'done))

(defun outputclasssxml (s class source)
  (dolist (obj (get-instances class source))
    (format s "  <~A OID=\"~A\">" class obj)
    (terpri s)
    (dolist (slot (attributes class))
      (dolist (val (get-values slot obj source))
        (format s "    <~A>" slot)
        (if (string-slot-p slot) (format s (htmlify val))
            (format s "<THING OID=\"~A\"/>" val))
        (format s "</~A>" slot)
        (terpri s)))
    (format s "  </~A>" class)
    (terpri s))
  (terpri s))

(defun sxmlsentences (tree)
  (let (*sentences*)
    (cond ((atom tree))
          ((atom (car tree)))
          ((eq 'sxml (caar tree)) (dolist (form (cdr tree)) (sxmlexp form)))
          (t (sxmlexp tree)))
    (nreverse *sentences*)))

(defun sxmlexp (tree)
  (cond ((atom tree) tree)
        ((listp (car tree)) (sxmlelement tree))
        (t (sxmlname tree))))

(defun sxmlelement (tree)
  (let (class object)
    (setq class (caar tree))
    (setq object (sxmlname (car tree)))
    (setq *sentences* (cons (list (prefix class 'instance) object) *sentences*))
    (do ((l (cdr tree) (cdr l)) (slot) (val))
        ((null l))
        (setq slot (caaar l))
        (when (cdar l)
          (setq val (sxmlexp (cadar l)))
          (setq *sentences* (cons `(,slot ,object ,val) *sentences*))))))

(defun sxmlname (tree)
  (dolist (attr (cdr tree))
    (when (and (listp attr) (eq '= (car attr)) (eq 'oid (cadr attr)))
      (return (read-value-string (caddr attr))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; TDC files
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod read-file-contents (fn (lang (eql 'tdc)))
  (with-open-file (s fn :direction :input)
    (do ((line (read-line s nil nil) (read-line s nil nil)) (nl))
          ((null line) (nreverse nl))
          (unless (equal line "") (setq nl (nreconc (read-table line s) nl))))))

(defun read-table (line s)
  (let (fields predicates slots)
    (setq fields (readtdtline line))
    (setq predicates (predicates (read-user-string (car fields))))
    (setq slots (mapcar #'read-user-string (cdr fields)))
    (do ((line (read-line s nil nil) (read-line s nil nil)) (object) (nl))
        ((or (null line) (equal line "")) (nreverse nl))
        (setq fields (readtdtline line))
        (setq object (read-user-string (car fields)))
        (dolist (p predicates) (setq nl (cons (list p object) nl)))
        (do ((l slots (cdr l)) (m (cdr fields) (cdr m)))
            ((null l))
            (unless (string= (car m) "")
              (do ((old 0) (new (position #\, (car m)) (position #\, (car m) :start old)))
                  ((null new) (setq nl (cons (list (car l) object
                                                   (read-user-string (subseq (car m) old new)))
                                             nl)))
                  (setq nl (cons (list (car l) object
                                       (read-user-string (subseq (car m) old new))) nl))
                  (setq old (1+ new))))))))

(defmethod dumpagent ((th theory) fn (language (eql 'tdc)))
  (with-open-file (f fn :direction :output :if-exists :supersede)
    (dolist (class (get-classes th))
      (outputclasstdc f class th)
      (terpri f))
    'done))

(defmethod dumpclass (class (source theory) fn (lang (eql 'tdc)))
  (with-open-file (f fn :direction :output :if-exists :supersede)
    (outputclasstdc f class source)))

(defun outputclasstdc (s class source)
  (let (slots)
    (setq slots (attributes class))
    (prin1 class s)
    (dolist (slot slots)
      (write-char #\tab s)
      (prin1 slot s))
    (terpri s)
    (dolist (x (get-instances class source))
      (prin1 x s)
      (dolist (slot slots)
        (let (values)
          (write-char #\tab s)
          (setq values (get-values slot x source))
          (when values (prin1 (car values) s))
          (dolist (val (cdr values)) (princ ", " s) (prin1 val s))))
        (terpri s))
    'done))

(defun predicates (class)
  (do ((l (superclasses class) (cdr l)) (p) (nl))
      ((null l) (nreverse nl))
      (when (setq p (find-predicate (car l))) (setq nl (cons p nl)))))

(defun superclasses (class)
  (do ((l (finds '?s `(superclass ,class ?s) *manager*) (cdr l)) (nl))
      ((null l) (cons class (nreverse nl)))
      (setq nl (nreconc (superclasses (car l)) nl))))

(defun readtdtline (s)
  (do ((old 0) (new (position #\tab s) (position #\tab s :start old)) (nl))
      ((null new) (nreverse (cons (subseq s old new) nl)))
      (setq nl (cons (subseq s old new) nl) old (1+ new))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; XML files
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod read-file-contents (fn (lang (eql 'xml)))
  (xmlsentences (parsexml (scanxml (substitute #\$ #\: (read-from-file fn))))))

(defun xmlsentences (tree)
  (let (*sentences*)
    (xmlobject tree)
    (nreverse *sentences*)))

(defun xmlobject (tree)
  (let (object)
    (cond ((atom tree) tree)
          ((atom (car tree))
           (setq object (xmlname tree))
           (xmlhead object tree))
          (t (setq object (xmlname (car tree)))
             (xmlhead object (car tree))
             (xmlbody object tree)))))

(defun xmlname (head)
  (do ((l (cdr head) (cdr l)))
      ((null l) (gentemp "X"))
      (when (and (listp (car l)) (eq '= (caar l))
                 (or (eq 'oid (cadar l)) (eq 'id (cadar l))))
        (return (read-value-string (caddar l))))))

(defun xmlhead (object head)
  (setq *sentences* (cons (list (prefix 'xml (car head)) object) *sentences*))
  (do ((l (cdr head) (cdr l)))
      ((null l) object)
      (cond ((atom (car l)))
            ((eq 'oid (cadar l)))
            ((eq 'id (cadar l)))
            (t (setq *sentences* (cons (list (prefix 'xml (cadar l)) object (caddar l)) *sentences*))))))

(defun xmlhead (object head)
  (setq *sentences* (cons (list (car head) object) *sentences*))
  (do ((l (cdr head) (cdr l)))
      ((null l) object)
      (cond ((atom (car l)))
            ((eq 'oid (cadar l)))
            ((eq 'id (cadar l)))
            (t (setq *sentences* (cons (list (prefix 'xml (cadar l)) object (caddar l)) *sentences*))))))

(defun xmlbody (object tree)
  (do ((l (cdr tree) (cdr l)))
      ((null l) object)
      (setq *sentences* (cons `(xml.component ,object ,(xmlobject (car l))) *sentences*))))

(defun prefix (name sent)
  (intern (strappend (symbol-name name) "." (symbol-name sent))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun scanxml (*string*)
   (do ((n 0) (char) (lexeme) (nl))
       ((>= n (length *string*)) (nreverse nl))
       (setq char (elt *string* n))
       (cond ((whitep char) (setq n (1+ n)))
             ((char-equal char #\-)
              (cond ((not (> (length *string*) (1+ n)))
                     (setq nl (cons '- nl) n (1+ n)))
                    ((char-equal (elt *string* (1+ n)) #\>)
                     (setq nl (cons 'rcomment nl) n (+ n 2)))
                    (t (setq nl (cons '- nl) n (1+ n)))))
             ((sxmlidcharp char)
              (multiple-value-setq (lexeme n) (scanxmlsymbol n))
              (setq nl (cons lexeme nl)))
             ((char-equal char #\")
              (multiple-value-setq (lexeme n) (scanxmlstring (1+ n)))
              (setq nl (cons lexeme nl)))
             ((char-equal char #\')
              (multiple-value-setq (lexeme n) (scanxmlsinglestring (1+ n)))
              (setq nl (cons lexeme nl)))
             ((char-equal char #\=) (setq nl (cons '= nl) n (1+ n)))
             ((char-equal char #\<)
              (cond ((not (> (length *string*) (1+ n)))
                     (setq nl (cons 'lparen nl) n (1+ n)))
                    ((char-equal (elt *string* (1+ n)) #\/)
                     (setq nl (cons 'lclose nl) n (+ n 2)))
                    ((char-equal (elt *string* (1+ n)) #\?)
                     (setq nl (cons 'lquest nl) n (+ n 2)))
                    ((char-equal (elt *string* (1+ n)) #\!)
                     (setq nl (cons 'lcomment nl) n (+ n 2)))
                    (t (setq nl (cons 'lparen nl) n (1+ n)))))
             ((char-equal char #\/)
              (cond ((not (> (length *string*) (1+ n)))
                     (setq nl (cons '\/ nl) n (1+ n)))
                    ((char-equal (elt *string* (1+ n)) #\>)
                     (setq nl (cons 'rclose nl) n (+ n 2))
                      (multiple-value-setq (lexeme n) (scanpcdata n))
                      (when lexeme (setq nl (cons lexeme nl))))
                    (t (setq nl (cons '/ nl) n (1+ n)))))
             ((char-equal char #\?)
              (cond ((not (> (length *string*) (1+ n)))
                     (setq nl (cons '? nl) n (1+ n)))
                    ((char-equal (elt *string* (1+ n)) #\>)
                     (setq nl (cons 'rquest nl) n (+ n 2)))
                    (t (setq nl (cons '? nl) n (1+ n)))))
             ((char-equal char #\>) (setq nl (cons 'rparen nl) n (1+ n))
              (multiple-value-setq (lexeme n) (scanpcdata n))
              (when lexeme (setq nl (cons lexeme nl))))
             (t (setq nl (cons (intern (make-string 1 :initial-element char)) nl)
                      n (1+ n))))))
#|
(defun scanxmlsymbol (n)
  (do ((end n))
      ((or (= end (length *string*))
           (not (sxmlidcharp (elt *string* end))))
       (values (intern (string-upcase (translate-html-chars (subseq *string* n end)))) end))
      (setq end (1+ end))))
|#
(defun scanxmlsymbol (n)
  (values (read-from-string
           (with-output-to-string (s)
             (do ((char))
                 ((or (= n (length *string*))
                      (not (sxmlidcharp (setq char (elt *string* n))))))
                 (when (char= char #\:) (setq char #\.))
                 (write-char char s)
                 (setq n (1+ n)))))
          n))

(defun sxmlidcharp (char)
  (or (alphanumericp char) (find char '(#\. #\: #\_ #\-))))
#|
(defun scanpcdata (str n)
  (declare (ignore str))
   (values
    (let ((str
            (with-output-to-string (s)
              (do ((char))
                  ((or (= n (length *string*))
                       (char-equal (setq char (elt *string* n)) #\<)))
                  (write-char char s)
                  (setq n (1+ n))))))
      (if  (zerop (length str)) nil
           (loop
             for ch across str
             unless (whitep ch) do (return (translate-html-chars str))
             finally (return nil))))
    n))
|#
(defun scanpcdata (n)
  (do ((end n) (str))
      ((or (= end (length *string*)) (char= (elt *string* end) #\<))
       (setq str (subseq *string* n end))
       (if (string= str "") (values nil end)
           (values (translate-html-chars str) end)))
      (setq end (1+ end))))

(defun scanxmlstring (n)
  (do ((end n))
      ((or (= end (length *string*))
           (char= (elt *string* end) #\"))
       (values (translate-html-chars (subseq *string* n end)) (1+ end)))
      (setq end (1+ end))))

(defun scanxmlsinglestring (n)
  (do ((end n))
      ((or (= end (length *string*))
           (char= (elt *string* end) #\'))
       (values (translate-html-chars (subseq *string* n end)) (1+ end)))
      (setq end (1+ end))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun parsexml (*string*)
  (setq *string* (stripjunk *string*))
  (xmlexp *string*))

(defun stripjunk (s)
  (do ((l s) (nl))
      ((null l) (nreverse nl))
      (cond ((eq (car l) 'lquest) (setq l (stripquestion l)))
            ((eq (car l) 'lcomment) (setq l (stripcomment l)))
            (t (setq nl (cons (car l) nl) l (cdr l))))))

(defun stripquestion (s)
  (do ((l s (cdr l)))
      ((or (null l) (eq (car l) 'rquest)) (cdr l))))

(defun stripcomment (s)
  (do ((l s (cdr l)))
      ((or (null l) (eq (car l) 'rcomment)) (cdr l))))

(defun xmlexp (s)
  (declare (ignore s))
  (let (start)
    (setq start (xmlsingle *string*))
    (cond ((atom start) start)
          ((eq 'single (car start)) (cdr start))
          ((eq 'open (car start)) (cdr (xmlbroad *string* (cdr start))))
          ((eq 'close (car start)) nil))))

(defun xmlsingle (s)
  (declare (ignore s))
  (cond ((eq 'lparen (car *string*))
         (pop *string*)
         (do ((nl))
             ((null *string*) (cons 'single (nreverse nl)))
             (cond ((eq 'rparen (car *string*))
                    (pop *string*)
                    (return (cons 'open (nreverse nl))))
                   ((eq 'rclose (car *string*))
                    (pop *string*)
                    (return (cons 'single (list (nreverse nl)))))
                   ((eq '= (cadr *string*))
                    (setq nl (cons `(= ,(car *string*) ,(caddr *string*)) nl))
                    (pop *string*) (pop *string*) (pop *string*))
                   (t (setq nl (cons (pop *string*) nl))))))
        ((eq 'lclose (car *string*))
         (pop *string*)
         (do ((nl))
             ((null *string*) (cons 'close (nreverse nl)))
             (cond ((or (eq 'rparen (car *string*)) (eq 'rclose (car *string*)))
                    (pop *string*)
                    (return (cons 'close (nreverse nl))))
                   (t (setq nl (cons (pop *string*) nl))))))))

(defun xmlbroad (s left)
    (declare (ignore s))
    (do ((dum) (nl))
        ((null *string*) (cons 'single (cons left (nreverse nl))))
        (setq dum (xmlsingle *string*))
        (cond ((null dum) (setq nl (cons (pop *string*) nl)))
              ((atom dum)
               (setq nl (cons 'single (cons dum (nreverse nl)))))
              ((eq 'single (car dum))
               (setq nl (cons (cdr dum) nl)))
              ((eq 'open (car dum))
               (setq nl (cons (cdr (xmlbroad *string* (cdr dum))) nl)))
              ((and (eq 'close (car dum)) (equalp (car left) (cadr dum)))
               (return (cons 'single (cons left (nreverse nl))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; DML Stuff
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun dumpdml (th fn)
  (with-open-file (f fn :direction :output :if-exists :supersede)
    (format f "<DML>") (terpri f)
    (dolist (obj (get-objects th))
      (terpri f)
      (dolist (sent (facts obj th))
        (unless (atom sent) (outputdml f sent) (terpri f))))
    (format f "</DML>") (terpri f)
    'done))

(defun outputdml (s p)
  (format s "<~A" (car p))
  (do ((l (cdr p) (cdr l)) (columns (find-columns (car p)) (cdr columns)))
      ((null l))
      (if (car columns)
          (format s " ~A=~A" (stringize (car columns)) (stringize (car l)))
          (format s " ~A" (stringize (car l)))))
  (format s "/>")
  p)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; CDF files
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod loadclass (class fn (target theory) (lang (eql 'cdf)) meta)
  (declare (ignore class))
  (dolist (fact (read-file-class fn lang)) (insert fact target))
  (when (eq meta 'dynamic) (introspect target))
  'done)

(defmethod read-file-class (fn (lang (eql 'cdf)))
  (let (predicate slots)
    (with-open-file (s fn :direction :input)
      (setq slots (readsimpleline (read-line s nil nil)))
      (setq predicate (getpredicate (car slots)))
      (setq slots (cdr slots))
      (do ((line (read-line s nil nil) (read-line s nil nil)) (nl))
          ((null line) (nreverse nl))
          (unless (equal line "")
            (setq nl (nreconc (readcomplexline line predicate slots) nl)))))))

(defun readsimpleline (s)
  (do ((old 0) (new (position #\tab s) (position #\tab s :start old)) (nl))
      ((null new) (nreverse (cons (intern (subseq s old new)) nl)))
      (setq nl (cons (intern (subseq s old new)) nl) old (1+ new))))

(defun readcomplexline (s predicate slots)
  (let (old new id)
    (setq old 0 new (position #\tab s))
    (setq id (read-from-string (subseq s old new)) old (1+ new))
    (do ((l slots (cdr l)) (nl (list (list predicate id))))
        ((null l) (nreverse nl))
        (setq new (position #\tab s :start old))
        (dolist (item (readsegment s old new))
          (setq nl (cons (list (car l) id item) nl)))
        (setq old (1+ new))
        (unless (> (length s) old) (return (nreverse nl))))))

(defun readsegment (s beg end)
  (do ((new (position #\, s :start beg :end end) (position #\, s :start beg :end end))
       (dum) (nl))
      ((null new) (setq dum (read-user-string (subseq s beg end)))
                  (when dum (setq nl (cons dum nl)))
                  (nreverse nl))
      (setq dum (read-user-string (subseq s beg new)) beg (1+ new))
      (when dum (setq nl (cons dum nl)))))

(defun getpredicate (class)
  (cond ((find-predicate class))
        (t (intern (stringappend (symbol-name class) ".INSTANCE")))))


(defparameter *errors* nil)

(defmethod validatefile (fn (lang (eql 'cdf)) *agent*)
  (validatesentences (read-file-contents fn lang) lang))

(defmethod validatesentences (sentences lang)
  (do ((l sentences (cdr l)) (*errors*))
      ((null l) (if *errors* (prettifyerrors (nreverse *errors*) lang) 'done))
      (checksentence (car l))))
      
(defun checksentence (sent)
  (cond ((atom sent))
        ((find (car sent) '(not and or unprovable => <=))
         (mapc #'checksentence (cdr sent)))
        (t (checksentencearity sent))))

(defun checksentencearity (sent)
  (let ((arity (find-arity (car sent))))
    (cond ((not (numberp arity)))
          ((equal arity (1- (length sent))))
          (t (setq *errors* (cons (list "Bad arity" (car sent)) *errors*))
             nil))))

(defmethod prettifyerrors (errors (lang (eql 'cdf)))
  (with-output-to-string (s)
    (do ((l errors (cdr l)))
        ((null l))
        (format s "~A - ~A" (caar l) (cadar l))
        (when (cdr l) (crlf s)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; TDT files
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod loaddatatdt (fn target &optional table headers)
  (dolist (sent (readtdt fn table headers))
    (insert sent target))
  'done)

(defun readtdt (fn table headers)
  (unless table (setq table (slashify fn)))
  (with-open-file (s fn :direction :input)
    (when headers (read-line s nil nil))
    (do ((line (read-line s nil nil) (read-line s nil nil)) (nl))
        ((null line) (nreverse nl))
        (unless (equal line "")
          (setq nl (cons (cons table (readcdtline line #\tab)) nl))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod read-file-contents (fn (lang (eql 'tdt)))
  (with-open-file (s fn :direction :input)
    (do ((line (read-line s nil nil) (read-line s nil nil))
         (rel (slashify fn)) (nl))
        ((null line) (nreverse nl))
        (unless (equal line "")
          (setq nl (cons (cons rel (readcdtline line #\tab)) nl))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; CDT files
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod dumprelation (rel (source theory) fn (lang (eql 'cdt)))
  (dumprelationcdt rel source fn #\,))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; SDT files
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod dumprelation (rel (source theory) fn (lang (eql 'sdt)))
  (dumprelationcdt rel source fn #\space))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; TDT files
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod dumprelation (rel (source theory) fn (lang (eql 'tdt)))
  (dumprelationcdt rel source fn #\tab))

(defmethod readrelation (rel fn (lang (eql 'tdt)))
  (readrelationcdt fn rel #\tab))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; VDT files
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod dumprelation (rel (source theory) fn (lang (eql 'vdt)))
  (dumprelationcdt rel source fn #\|))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; CSV files
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod dumprelation (rel (source theory) fn (lang (eql 'csv)))
  (declare (ignore rel source fn lang))
  "Not implemented.")



(defun readrelationcdt (fn rel char)
  (with-open-file (s fn :direction :input)
    (do ((line (read-line s nil nil) (read-line s nil nil)) (nl))
        ((null line) (nreverse nl))
        (unless (equal line "")
          (setq nl (cons (cons rel (readcdtline line char)) nl))))))

(defun readcdtline (s char)
  (do ((old 0) (new (position char s) (position char s :start old)) (nl))
      ((null new) (nreverse (cons (subseq s old new) nl)))
      (setq nl (cons (subseq s old new) nl) old (1+ new))))

(defun loadrelcsv (rel fn th)
  (with-open-file (s fn :direction :input)
    (do ((line (read-line s nil nil) (read-line s nil nil)))
        ((null line) 'done)
        (unless (equal line "")
          (insert (cons rel (readcsvline line)) th)))))

(defun readcsvline (s)
  (do ((beg 1) (end (csvend s 1) (csvend s beg)) (len (length s)) (nl))
      ((null end) (nreverse nl))
      (setq nl (cons (subseq s beg end) nl))
      (unless (> len (setq beg (+ end 3))) (return (nreverse nl)))))

(defun csvend (s start)
  (do ((pos (position #\" s :start start) (position #\" s :start pos))
       (end (length s)))
      ((null pos) nil)
      (cond ((= (1+ pos) end) (return pos))
            ((char= (elt s (1+ pos)) #\") (setq pos (+ pos 2)))
            (t (return pos)))))


(defun dumprelationcdt (rel th fn char)
  (with-open-file (f fn :direction :output :if-exists :supersede)
    (do ((l (indexees rel th) (cdr l)))
        ((null l) 'done)
        (when (and (listp (car l)) (eq rel (caar l)))
          (writecdtline (cdar l) char f)
          (terpri f)))))

(defun writecdtline (line char s)
  (cond ((null line))
        (t (princ (car line) s)
           (do ((l (cdr line) (cdr l)))
               ((null l) line)
               (write-char char s)
               (prin1 (car l) s)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; HTML stuff
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod read-file-contents (fn (lang (eql 'html)))
  `((file.xll ,(slashify fn) ,(parsehtml (scanxml (substitute #\- #\: (read-from-file fn)))))))

(defun read-from-file (fn) 
  (with-open-file (f fn :direction :input)
    (with-output-to-string (s)
      (do ((a (read-char f nil nil) (read-char f nil nil)))
          ((null a) t)
          (write-char a s)))))

(defun parsehtml (*string*)
  (setq *string* (stripjunk *string*))
  (htmlexp *string*))

(defun htmlexp (s)
  (declare (ignore s))
  (cond ((stringp (car *string*)) (pop *string*))
        ((eq (car *string*) 'lparen) (htmlbroad *string*))
        ((eq (car *string*) 'lclose) nil)
        ((eq (car *string*) 'rparen) nil)
        ((eq (car *string*) 'rclose) nil)
        (t (pop *string*))))

(defun htmlbroad (s)
  (declare (ignore s))
  (let (head)
    (setq head (htmlheader *string*))
    (do ((nl))
        ((null *string*) (cons head (nreverse nl)))
        (cond ((and (eq (car *string*) 'lparen)
                    (equalp (cadr *string*) (car head)))
               (return (cons head (nreverse nl))))
              ((and (eq (car *string*) 'lclose)
                    (equalp (cadr *string*) (car head))
                    (eq (caddr *string*) 'rparen))
               (pop *string*) (pop *string*) (pop *string*)
               (return (cons head (nreverse nl))))
              ((and (eq (car *string*) 'lclose)
                    (not (equalp (cadr *string*) (car head)))
                    (eq (caddr *string*) 'rparen))
               (return (cons head (nreverse nl))))
              (t (setq nl (cons (htmlexp *string*) nl)))))))

(defun htmlheader (s)
  (declare (ignore s))
  (when (eq (car *string*) 'lparen)
    (pop *string*)
    (do ((nl))
        ((null *string*) (nreverse nl))
        (cond ((eq 'rparen (car *string*))
               (pop *string*)
               (return (nreverse nl)))
              ((eq 'rclose (car *string*))
               (pop *string*)
               (return (nreverse nl)))
              ((eq '= (cadr *string*))
               (setq nl (cons `(= ,(car *string*) ,(caddr *string*)) nl))
               (pop *string*) (pop *string*) (pop *string*))
              (t (setq nl (cons (pop *string*) nl)))))))

(defun translate-html-chars (str)
  "Translate &quot; -> \", &amp; -> &, &gt; -> >, &lt; -> <, &#XX; -> <??>"
  (declare (type string str))
  (loop
      with translations =
	'(("amp" . "&") ("gt" . ">") ("lt" . "<") ("quot" . "\""))
      with already-seen = 0
      with html-ex-chr
      with trans-char
      for start = (position #\& str :start already-seen)
      for end = (when start (position #\; str :start start))
      while (and start end)
      do (setq html-ex-chr (subseq str (1+ start) end))
	 (setq trans-char
	   (find html-ex-chr translations :key #'first :test #'string=) )
	 (cond
	  (trans-char
	   (setq trans-char (rest trans-char))
	   (setq str
	     (concatenate 'string
	       (subseq str 0 start) trans-char (subseq str (1+ end)) )))
	  ((string= html-ex-chr ""))
	  ((and (eql (elt html-ex-chr 0) #\#)
		(not (find-if-not #'digit-char-p (subseq html-ex-chr 1))) )
	   (setq str
	     (concatenate 'string
	       (subseq str 0 start)
	       (string (code-char (read-from-string (subseq html-ex-chr 1))))
	       (subseq str (1+ end)) ))))
	 (setq already-seen (1+ start))
      finally (return str) ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; XCL stuff
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun slashify (s)
  (intern (stringupcase (stringappend "/" (substitute #\/ #\: s)))))

(defun webify (s)
  (stringappend "doc?Path=/" (substitute #\/ #\: s)))

(defun readxcl (s)
  (unxclify (parsexml (scanxml (substitute #\- #\: s)))))

(defun unxclify (x)
  (cond ((atom x) x)
        ((eq (car x) 'variable) (intern (caddr (cadr x))))
        ((eq (car x) 'object) (read-from-string (caddr (cadr x))))
        ((eq (car x) 'function) (intern (caddr (cadr x))))
        ((eq (car x) 'relation) (intern (caddr (cadr x))))
        ((eq (car x) 'action) (intern (caddr (cadr x))))
        ((atom (car x)) (cons (car x) (mapcar #'unxclify (cdr x))))
        ((eq (caar x) 'quote) (list 'quote (unxclify (cadr x))))
        ((eq (caar x) 'reply) (list 'reply (unxclify (cadr x))))
        ((eq (caar x) 'string) (cadadr x))
        ((eq (caar x) 'list) (mapcar #'unxclify (cdr x)))
        ((eq (caar x) 'actioninvocation) (mapcar #'unxclify (cdr x)))
        ((eq (caar x) 'funterm) (mapcar #'unxclify (cdr x)))
        ((eq (caar x) 'relsent) (mapcar #'unxclify (cdr x)))
        ((eq (caar x) 'negation) (list 'not (unxclify (cadr x))))
        ((eq (caar x) 'conjunction) (cons 'and (mapcar #'unxclify (cdr x))))
        ((eq (caar x) 'disjunction) (cons 'or (mapcar #'unxclify (cdr x))))
        ((eq (caar x) 'implication) (cons '=> (mapcar #'unxclify (cdr x))))
        ((eq (caar x) 'reduction) (cons '<= (mapcar #'unxclify (cdr x))))
        (t (cons (caar x) (mapcar #'unxclify (cdr x))))))

(defun xclify (x)
  (with-output-to-string (s)
    (xclifyactioninvocation x 0 s)))

(defun xclifyactioninvocation (x n s)
  (cond ((atom x) (xclifyexp x n s))
        ((eq 'quote (car x))
         (printspaces (* 2 n) s) (format s "<QUOTE>~%")
         (xclifyexp (cadr x) (1+ n) s)
         (printspaces (* 2 n) s) (format s "</QUOTE>~%"))
        ((eq 'tell (car x))
         (printspaces (* 2 n) s) (format s "<ACTIONINVOCATION>~%")
         (xclifyaction 'tell (1+ n) s)
         (xclifysentence (cadr x) (1+ n) s)
         (printspaces (* 2 n) s) (format s "</ACTIONINVOCATION>~%"))
        ((eq 'untell (car x))
         (printspaces (* 2 n) s) (format s "<ACTIONINVOCATION>~%")
         (xclifyaction 'untell (1+ n) s)
         (xclifysentence (cadr x) (1+ n) s)
         (printspaces (* 2 n) s) (format s "</ACTIONINVOCATION>~%"))
        ((eq 'reply (car x))
         (printspaces (* 2 n) s) (format s "<REPLY>~%")
         (xclifyexp (cadr x) (1+ n) s)
         (printspaces (* 2 n) s) (format s "</REPLY>~%"))
        (t (printspaces (* 2 n) s) (format s "<ACTIONINVOCATION>~%")
           (xclifyaction (car x) (1+ n) s)
           (dolist (x (cdr x)) (xclifyactioninvocation x (1+ n) s))
           (printspaces (* 2 n) s) (format s "</ACTIONINVOCATION>~%"))))

(defun xclifyaction (x n s)
  (printspaces (* 2 n) s)
  (format s "<ACTION ID=\"~A\"/>~%" (htmlify (symbol-name x))))

(defun xclifyexp (x n s)
  (cond ((stringp x)
         (printspaces (* 2 n) s)
         (format s "<STRING>")
         (format s (htmlify (princ-to-string x)))
         (format s "</STRING>~%"))
        ((atom x)
         (printspaces (* 2 n) s)
         (format s "<OBJECT ID=\"~A\"/>~%" (htmlify (prin1-to-string x))))
        (t (printspaces (* 2 n) s) (format s "<LIST>~%")
           (dolist (x x) (xclifyexp x (1+ n) s))
           (printspaces (* 2 n) s) (format s "</LIST>~%"))))

(defun xclifysentence (x n s)
  (cond ((atom x)
         (printspaces (* 2 n) s)
         (format s "<OBJECT ID=\"~A\"/>~%" (htmlify (symbol-name x))))
        ((eq 'not (car x))
         (printspaces (* 2 n) s) (format s "<NEGATION>~%")
         (xclifysentence (cadr x) (1+ n) s)
         (printspaces (* 2 n) s) (format s "</NEGATION>~%"))
        ((eq 'and (car x))
         (printspaces (* 2 n) s) (format s "<CONJUNCTION>~%")
         (dolist (x (cdr x)) (xclifysentence x (1+ n) s))
         (printspaces (* 2 n) s) (format s "</CONJUNCTION>~%"))
        ((eq 'or (car x))
         (printspaces (* 2 n) s) (format s "<DISJUNCTION>~%")
         (dolist (x (cdr x)) (xclifysentence x (1+ n) s))
         (printspaces (* 2 n) s) (format s "</DISJUNCTION>~%"))
        ((eq '=> (car x))
         (printspaces (* 2 n) s) (format s "<IMPLICATION>~%")
         (dolist (x (cdr x)) (xclifysentence x (1+ n) s))
         (printspaces (* 2 n) s) (format s "</IMPLICATION>~%"))
        ((eq '<= (car x))
         (printspaces (* 2 n) s) (format s "<REDUCTION>~%")
         (dolist (x (cdr x)) (xclifysentence x (1+ n) s))
         (printspaces (* 2 n) s) (format s "</REDUCTION>~%"))
        (t (printspaces (* 2 n) s) (format s "<RELSENT>~%")
           (printspaces (* 2 (1+ n)) s)
           (format s "<RELATION ID=\"~A\"/>~%" (htmlify (symbol-name (car x))))
           (dolist (x (cdr x)) (xclifyterm x (1+ n) s))
           (printspaces (* 2 n) s) (format s "</RELSENT>~%"))))

(defun xclifyterm (x n s)
  (cond ((atom x)
         (printspaces (* 2 n) s)
         (format s "<OBJECT ID=\"~A\"/>~%" (htmlify (symbol-name x))))
        (t (printspaces (* 2 n) s) (format s "<FUNTERM>~%")
           (printspaces (* 2 (1+ n)) s)
           (format s "<FUNCTION ID=\"~A\"/>~%" (htmlify (symbol-name (car x))))
           (dolist (x (cdr x)) (xclifyterm x (1+ n) s))
           (printspaces (* 2 n) s) (format s "</FUNTERM>~%"))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; introspect
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod introspect (target)
  (declare (ignore target))
  "Introspect works only with dataservers.")

(defmethod introspect ((target symbol))
  (cond ((and (boundp target) (not (symbolp (symbol-value target))))
         (introspect (symbol-value target)))
        (t (call-next-method target))))

(defmethod introspect ((target theory))
  (dolist (sent (guessmetadata target))
    (request `(tell ,sent) *client* *manager*))
  'done)

(defmethod unintrospect (target)
  (declare (ignore target))
  "Unintrospect works only with dataservers.")

(defmethod unintrospect ((target symbol))
  (cond ((and (boundp target) (not (symbolp (symbol-value target))))
         (unintrospect (symbol-value target)))
        (t (call-next-method target))))

(defmethod unintrospect ((target theory))
  (dolist (sent (guessmetadata target))
    (request `(tell (not ,sent)) *client* *manager*))
  'done)

(defun guessmetadata (source)
  (let (predicates attributes tables dum class slot metadata)
  (dolist (sent (contents source))
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
    (setq metadata (cons `(specialty ,(name source) ,predicate) metadata))
    (setq metadata (cons `(rootclass ,(name source) ,class) metadata)))
  (dolist (attribute (nreverse attributes))
    (setq dum (disconstruct attribute))
    (setq class (newfix (car dum) (cadr dum)))
    (setq slot (newfix (cadr dum) (caddr dum)))
    (setq metadata (cons `(attribute ,class ,attribute) metadata))
    (setq metadata (cons `(isa ,attribute attributerelation) metadata))
    (when (findp `(isa ,slot attributerelation) *manager*)
      (setq metadata (cons `(superrelation ,attribute ,slot) metadata)))
    (setq metadata (cons `(domain ,attribute ,class) metadata))
    (setq metadata (cons `(specialty ,(name source) ,attribute) metadata))
    (setq metadata (cons `(interest ,(name source) ,attribute) metadata))
    (setq metadata (cons `(searchstyle ,attribute dropdownlist) metadata)))
  (nreverse metadata)))

(defun disconstruct (x)
  (let (name beg end)
    (setq name (symbol-name x))
    (setq beg (position #\. name))
    (setq end (position #\. name :from-end t))
    (cond ((or (not beg) (not end)) (list nil nil x))
          ((= beg end) 
           (list nil (intern (subseq name 0 beg))
                     (intern (subseq name (1+ beg)))))
          (t (list (intern (subseq name 0 beg))
                   (intern (subseq name (1+ beg) end))
                   (intern (subseq name (1+ end))))))))

(defun deconstruct (x)
  (setq x (symbol-name x))
  (do ((old 0) (new (position #\. x) (position #\. x :start old)) (nl))
      ((null new) (nreverse (cons (intern (subseq x old)) nl)))
      (setq nl (cons (intern (subseq x old new)) nl) old (1+ new))))

(defun newfix (name sent)
  (cond ((null name) sent)
        ((null sent) name)
        (t (intern (strappend (symbol-name name) "." (symbol-name sent))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; reading stuff
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun read-value-string (str)
  (cond ((stringablep str) (read-user-string str))
        (t (read-user-string (substitute #\- #\space str)))))

(defun stringablep (str)
  (do ((i 0 (1+ i)) (n (length str)))
      ((>= i n) nil)
      (cond ((char-equal (elt str i) #\space))
            ((char-equal (elt str i) #\") (return t))
            (t (return nil)))))

(defun read-user-string (str)
  (declare (type string str))
  (ignore-errors (read-from-string str nil nil)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; printing stuff
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun printspaces (n s)
  (do ((i 1 (1+ i)))
      ((> i n))
      (princ " " s)))

(defun crlf (s)
  (format s "~A~A" #\return #\linefeed))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; manager stuff
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun string-slot-p (slot)
  (find (findx '?x `(changestyle ,slot ?x) *interface*)
        '(stringfield text textarea password)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
