;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; (c) Copyright 1999-2000 by Michael R. Genesereth.  All rights reserved.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ACL/XML Files
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(eval-when (compile load eval) (proclaim '(special *string*)))

(defmethod loadfile (fn (target theory) (lang (eql 'xacl)) meta)
  (xmlinsert (parsexml (scanxml (read-from-file fn))) target meta)
  'done)

(defmethod unloadfile (fn (target theory) (lang (eql 'xacl)) meta)
  (xmluninsert (parsexml (scanxml (read-from-file fn))) target meta))

(defmethod dumpfile ((source theory) fn (lang (eql 'xacl)))
  (dumpxml source fn))

(defmethod loadrelation (rel fn (target theory) (lang (eql 'xacl)) meta)
  (loadrelxml rel fn target meta))

(defmethod unloadrelation (rel fn (target theory) (lang (eql 'xacl)) meta)
  (unloadrelxml rel fn target meta))

(defmethod dumprelation (rel (source theory) fn (lang (eql 'xacl)))
  (dumprelxml rel source fn))


(defparameter *ids* nil)

(defparameter *tuples* nil)

(defun xmlinsert (tree source meta)
  (let (*ids*)
    (dolist (object (cdr tree)) (xmlinsertobject object source meta))
    'done))

(defun xmlinsertobject (tree source meta)
  (let (class id slot val)
    (setq class (caar tree))
    (setq id (xmlname (car tree)))
    (fancycreate id class source meta)
    (dolist (cell (cdr tree))
      (setq slot (caar cell))
      (setq val (xmlsubobject (cadr cell) source meta))
      (fancysave class slot id val source meta))
    id))

(defun xmlsubobject (tree source meta)
  (cond ((atom tree) tree)
        ((eq 'pcdata (car tree)) (cadr tree))
        ((listp (car tree)) (xmlinsertobject tree source meta))
        (t (xmlname tree))))

(defun xmlname (tree)
  (dolist (attr (cdr tree))
    (when (and (listp attr) (eq '= (car attr)) (eq 'id (cadr attr)))
      (return (read-value-string (caddr attr))))))

(defun fancycreate (obj class source meta)
  (let ((sub (prefix (name source) class)))
    (when (eq meta 'dynamic)
      (telladministrator `(isa ,sub class))
      (when (findp `(isa ,class class) *manager*)
        (telladministrator `(superclass ,sub ,class)))
      (telladministrator `(frame ,(name source) ,sub))
      (telladministrator `(isa ,sub relation))
      (telladministrator `(arity ,sub 1))
      (telladministrator `(specialty ,(name source) ,sub))
      (telladministrator `(table ,(name source) ,sub)))
    (save `(isa ,obj,sub) source)
    (save (makpred obj sub source) source)))

(defun fancysave (class relation object value source meta)
  (let ((rel (prefix (name source) relation)) (sub (prefix (name source) class)))
    (when (eq meta 'dynamic)
      (telladministrator `(attribute ,sub ,rel))
      (telladministrator `(isa ,rel relation))
      (when (findp `(isa ,relation relation) *manager*)
        (telladministrator `(superrelation ,rel ,relation)))
      (telladministrator `(arity ,rel 2))
      (telladministrator `(domain ,rel ,sub))
      (when (setq class (find-range relation))
        (telladministrator `(range ,rel ,class)))
      (telladministrator `(table ,(name source) ,rel)))
    (save `(,rel ,object ,value) source)))

(defun prefix (name sent)
  (intern (strappend (symbol-name name) "." (symbol-name sent))))


(defun xmluninsert (tree source meta)
  (let (*ids*)
    (dolist (object (cdr tree)) (xmluninsertobject object source meta))
    'done))

(defun xmluninsertobject (tree source meta)
  (let (class id slot val)
    (setq class (caar tree))
    (setq id (xmlname (car tree)))
    (fancydestroy id class source meta)
    (dolist (cell (cdr tree))
      (setq slot (caar cell))
      (setq val (xmluninsertold (cadr cell) source meta))
      (fancydrop class slot id val source meta))
    id))

(defun xmluninsertold (tree source meta)
  (cond ((atom tree) tree)
        ((eq 'pcdata (car tree)) (cadr tree))
        ((listp (car tree)) (xmlinsertobject tree source meta))
        (t (xmlname tree))))

(defun fancydestroy (obj class source meta)
  (let ((sub (prefix (name source) class)))
    (when (eq meta 'dynamic)
      (untelladministrator `(isa ,sub class))
      (untelladministrator `(superclass ,sub ,class))
      (untelladministrator `(frame ,(name source) ,sub))
      (untelladministrator `(isa ,sub relation))
      (untelladministrator `(arity ,sub 1))
      (untelladministrator `(specialty ,(name source) ,sub))
      (untelladministrator `(table ,(name source) ,sub)))
    (drop `(isa ,obj,sub) source)
    (drop (makpred obj sub source) source)))

(defun fancydrop (class relation object value source meta)
  (let ((rel (prefix (name source) relation))
        (sub (prefix (name source) class)))
    (when (eq meta 'dynamic)
      (untelladministrator `(attribute ,sub ,rel))
      (untelladministrator `(isa ,rel relation))
      (untelladministrator `(superrelation ,rel ,relation))
      (untelladministrator `(arity ,rel 2))
      (untelladministrator `(domain ,rel ,sub))
      (untelladministrator `(specialty ,(name source) ,rel))
      (untelladministrator `(table ,(name source) ,rel)))
    (drop `(,rel ,object ,value) source)))


(defun dumpxml (th fn)
  (with-open-file (f fn :direction :output :if-exists :supersede)
    (format f "<XML>") (terpri f)
    (dolist (sent (contents th))
      (unless (atom sent) (outputxml f sent) (terpri f)))
    (format f "</XML>") (terpri f)
    'done))

(defun outputxml (s p)
  (format s "<~A" (car p))
  (do ((l (cdr p) (cdr l)) (columns (find-columns (car p)) (cdr columns)))
      ((null l))
      (if (car columns)
          (format s " ~A=\"~A\"" (stringify (car columns)) (stringify (car l)))
          (format s " \"~A\"" (stringify (car l)))))
  (format s "/>")
  p)


(defun loadrelxacl (rel fn target meta)
  (let (xml)
    (setq xml (read-xml-from-file fn))
    (cond ((atom xml))
          ((eq rel (car xml)) (insert xml target))
          ((eq 'and (car xml))
           (dolist (sent (cdr xml))
             (when (and (listp sent) (eq rel (car sent)))
               (when (eq meta 'dynamic) (fancyinsert sent target))
               (insert sent target)))))))

(defun unloadrelxacl (rel fn target meta)
  (let (xml)
    (setq xml (read-xml-from-file fn))
    (cond ((atom xml))
          ((eq rel (car xml)) (insert xml target))
          ((eq 'and (car xml))
           (dolist (sent (cdr xml))
             (when (and (listp sent) (eq rel (car sent)))
               (drop sent target)
               (when (eq meta 'dynamic) (fancyuninsert sent target))))))))

(defun dumprelxacl (rel th fn)
  (with-open-file (f fn :direction :output :if-exists :supersede)
    (format f "<XML>") (terpri f)
    (dolist (sent (indexees rel th))
      (when (and (listp sent) (eq rel (car sent)))
        (outputxml f sent) (terpri f)))
    (format f "</XML>") (terpri f)
    'done))

(defun read-xacl-from-file (fn)
  `(and ,@(second (xml->kif (parsexml (scanxml (read-from-file fn)))))))


(defun readxml-from-string (str)
  (convertxml (parsexml (scanxml str))))

(defun scanxml (*string*)
   (do ((n 0) (char) (lexeme) (nl))
       ((>= n (length *string*)) (nreverse nl))
       (setq char (elt *string* n))
       (cond ((whitep char) (setq n (1+ n)))
             ((xmlidcharp char)
              (multiple-value-setq (lexeme n) (scanxmlsymbol n))
              (setq nl (cons lexeme nl)))
             ((char-equal char #\")
              (multiple-value-setq (lexeme n) (scanxmlstring (1+ n)))
              (setq nl (cons lexeme nl)))
             ((char-equal char #\=) (setq nl (cons '= nl) n (1+ n)))
             ((char-equal char #\<)
              (cond ((and (> (length *string*) (1+ n))
                          (char-equal (elt *string* (1+ n)) #\/))
                     (setq nl (cons 'lclose nl) n (+ n 2)))
                    (t (setq nl (cons 'lparen nl) n (1+ n)))))
             ((char-equal char #\/)
              (cond ((and (> (length *string*) (1+ n))
                          (char-equal (elt *string* (1+ n)) #\>))
                     (setq nl (cons 'rclose nl) n (+ n 2))
                      (multiple-value-setq (lexeme n) (scanpcdata (subseq *string* n) n))
                      (when lexeme (setq nl (cons lexeme nl))))
                    (t (setq nl (cons '/ nl) n (1+ n)))))
             ((char-equal char #\>) (setq nl (cons 'rparen nl) n (1+ n))
              (multiple-value-setq (lexeme n) (scanpcdata (subseq *string* n) n))
              (when lexeme (setq nl (cons lexeme nl))))
             (t (setq nl (cons (intern (make-string 1 :initial-element char)) nl)
                      n (1+ n))))))

(defun scanxmlsymbol (n)
  (values (read-from-string
           (with-output-to-string (s)
             (do ((char))
                 ((or (= n (length *string*))
                      (not (xmlidcharp (setq char (elt *string* n))))))
                 (write-char char s)
                 (setq n (1+ n)))))
          n))
(defun xmlidcharp (char)
  (or (alphanumericp char) (find char '(#\. #\: #\_ #\-))))

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
        unless (whitep ch) do (return str)
        finally (return nil))))
      n))

(defun scanxmlstring (n)
   (values (translate-html-chars
            (with-output-to-string (s)
              (do ((char))
                  ((or (= n (length *string*))
                       (char= #\" (setq char (elt *string* n)))))
                  (write-char char s)
                  (setq n (1+ n)))))
           (1+ n)))

(defun parsexml (*string*)
  (xmlexp *string*))

(defun xmlexp (s)
  (declare (ignore s))
   (let (start)
     (setq start (xmlsingle *string*))
     (cond ((atom start) start)
           ((eq 'single (car start)) (cdr start))
           ((eq 'open (car start)) (cdr (xmlbroad *string* (cdr start))))
           ((eq 'close (car start)) nil))))

(defun xmlbroad (s left )
    (declare (ignore s))
    (do ((dum) (nl))
        ((null *string*) (cons 'single (cons left (nreverse nl))))
        (setq dum (xmlsingle *string*))
        (cond ((null dum) (setq nl (cons `(pcdata ,(pop *string*)) nl)))
              ((atom dum)
               (setq nl (cons 'single (cons dum (nreverse nl)))))
              ((eq 'single (car dum))
               (setq nl (cons (cdr dum) nl)))
              ((eq 'open (car dum))
               (setq nl (cons (cdr (xmlbroad *string* (cdr dum))) nl)))
              ((and (eq 'close (car dum)) (equalp (car left) (cadr dum)))
               (return (cons 'single (cons left (nreverse nl))))))))

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
                    (return (cons 'single (nreverse nl))))
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

(defun convertxml (x)
  (cond ((atom x) x)
        ((atom (car x)) (convertxmlsent x))
        ((eq 'xml (caar x)) (maksand (mapcar #'convertxml (cdr x))))
        (t 'true)))

(defun convertxmlsent (x)
  (let (columns)
    (cond ((setq columns (find-columns (car x)))
           (do ((l columns (cdr l)) (nl))
               ((null l) (cons (car x) (nreverse nl)))
               (do ((m (cdr x) (cdr m)))
                   ((null m) (setq nl (cons nil nl)))
                   (when (and (listp (car m)) (equalp (car l) (cadar m)))
                     (setq nl (cons (caddar m) nl))
                     (return t)))))
          (t (do ((l (cdr x) (cdr l)) (nl))
                 ((null l) (cons (car x) (nreverse nl)))
                 (cond ((atom (car l)) (setq nl (cons (car l) nl)))
                       ((and (eq '= (caar l)) (cddar l))
                        (setq nl (cons (caddar l) nl)))
                       (t (setq nl (cons nil nl)))))))))


(defun xml->kif (tree)
   (let (*ids* *tuples*)
    (xml->kif-1 nil nil tree)
    (list *ids* *tuples*)))

(defun xml->kif-1 (grandparent parent tree)
    (cond
          ;; <tag>tag<tag>
          ((and (listp (car tree))(eq (cadadr tree) (caar tree)))
           (process-tag grandparent parent (car tree)))
          ;; <tag></tag>
          ((and (listp (car tree))(= (length tree) 1))
           (process-tag grandparent parent (car tree)))
          ;; <tag>....</tag>
          ((listp (car tree))
           (let ((id
                  (process-tag grandparent parent (car tree))))
            (loop
              for subtree in (cdr tree)
              do (xml->kif-1 parent id subtree))))  ;; promot parent.
          ;; text
          ((eq (car tree) 'pcdata)
           (push `(,parent ,grandparent ,(cadr tree)) *tuples*))
          ;; <tag/>
          ((atom (car tree))
           (process-tag grandparent parent tree))))

(defun process-tag (grandparent parent tag)
    (let* ((id (or (read-value-string (caddar (member '(= id) (rest tag) :test 'tree-equal
                                   :key #'(lambda (list)  (subseq list 0 2))))) ;; wd
                   (first tag)))
 
           (value  (caddar (member '(= value) (rest tag) :test
                                   'tree-equal :key #'(lambda (list)
                                                        (subseq list 0 2))))))
      (loop for attr in (rest tag)
            unless (or (eq (cadr attr) 'id)(eq (cadr attr) 'value))
            if id do (push `(,(second attr) ,id ,(third attr)) *tuples*))
      (when (and id (not (eq id (first tag))) (not grandparent))
        (push `(ISA ,id ,(first tag)) *tuples*)
        (push id *ids*))
      (when (and parent value)
        (push `(,(first tag) ,parent ,value) *tuples*))
      (when (and grandparent parent id (not (member parent *ids*)))
        (push `(,parent ,grandparent ,id) *tuples*))
      id))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
