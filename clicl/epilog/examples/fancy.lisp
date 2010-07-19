;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Insertion and Uninsertion routines;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;(defgeneric insert (p th) (:documentation "(INSERT P TH)  INSERT takes a sentence p and a theory TH as arguments.  It indexes P and adds  P to the contents of TH, updating *THEORIES* accordingly."))(defmethod insert (p th)  (content p th)  (index p p th)  p)(defgeneric uninsert (p th) (:documentation "(UNINSERT P TH)  UNINSERT takes a sentence P and a theory TH as arguments.  It unindexes P and  removes P from the contents of TH, updating *THEORIES* accordingly."))(defmethod uninsert (p th)  (unindex p p th)  (uncontent p th)  p);;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Basic Indexing Subroutines;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;(defgeneric index (p d th) (:documentation "(INDEX P D TH)  INDEX takes an expression p, a sentence d, and a theory TH as arguments.  It  adds D at the end of the indexees of all atoms in P."))(defmethod index ((p list) d th)  (index (car p) d th)  (index (cdr p) d th))(defmethod index ((p symbol) d th)  (cond ((null p))        ((varp p))        (t (setf (get p th) (fancyendq d (get p th))))))(defmethod index (p d th)  (setf (getf (gethash p plistarray) th)        (fancyendq d (getf (gethash p plistarray) th))))(defgeneric unindex (p d th) (:documentation "(UNINDEX P TH)  UNINDEX takes an expression P, a sentence D, and a theory TH as arguments.  It  removes D from the indexees of all atoms in P."))(defmethod unindex ((p list) d th)  (unindex (car p) d th)  (unindex (cdr p) d th))(defmethod unindex ((p symbol) d th)  (cond ((null p))        ((varp p))        (t (setf (get p th) (fancydelq d (get p th))))))(defmethod unindex (p d th)  (setf (getf (gethash p plistarray) th)        (fancydelq d (getf (gethash p plistarray) th))))(defgeneric indexps (p th) (:documentation "(INDEXPS P TH)  INDEXPS takes an expression and a theory as arguments and returns a list of  the sentences in the specified theory containing the specified atom."))(defmethod indexps (p th)  (indexees p th))(defgeneric indexees (p th) (:documentation "(INDEXEES P TH)  INDEXEES takes an atom and a theory as arguments and returns a list of the  sentences in the specified theory containing the specified atom."))(defmethod indexees ((x list) th)  (cond ((varp (setq x (operator x))) (contents th))        ((null x) (contents th))        (t (indexees x th))))(defmethod indexees ((x symbol) th)  (car (get x th)))(defmethod indexees (x th)  (car (getf (gethash x plistarray) th)));;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Theory subroutines;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;(defgeneric content (p th) (:documentation "(CONTENT P TH)  CONTENT takes an expression p and a theory TH as arguments.  It adds P to the  contents of TH."))(defmethod content (p (th symbol))  (let ((dum (property th 'sentences)))    (cond ((null dum)           (setq *theories* (cons th *theories*))           (setf (contents th) (list p)))          (t (rplacd (cdr dum) (list p))             (rplacd dum (cddr dum))))  p))(defgeneric uncontent (p th) (:documentation "(UNCONTENT P TH)  UNCONTENT takes an expression P and a theory TH as arguments.  It removes P  from the contents of TH."))(defmethod uncontent (p (th symbol))  (if (null (setf (get th 'sentences) (fancydelq p (get th 'sentences))))       (setq *theories* (delete th *theories* :count 1)))  p)(defgeneric contents (th) (:documentation "(CONTENTS TH)  CONTENTS takes a theory as argument and returns a list of the sentences stored  in that theory."))(defmethod contents ((th symbol))  (car (get th 'sentences)))(defun fancyendq (x dl)  (cond ((null dl) (cons (setq x (list x)) x))        ((eq x (cadr dl)) dl)        (t (rplacd (cdr dl) (list x))           (rplacd dl (cddr dl)))))(defun fancydelq (x dl)  (cond ((null dl) nil)        ((eq x (caar dl))         (if (null (cdar dl)) nil (rplaca dl (cdar dl))))        (t (do ((l (car dl) (cdr l)))               ((null (cdr l)) dl)               (when (eq x (cadr l))                 (rplacd l (cddr l))                 (if (null (cdr l)) (rplacd dl l))                 (return dl))))));;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;