;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; (c) Copyright 1999-2008 by Michael Genesereth.  All rights reserved.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(eval-when (compile load eval)
  (proclaim '(special *client* *manager* *var-count*)))

(defun askonemanager (x p)
  (findx x p *manager*))

(defun askallmanager (x p)
  (finds x p *manager*))

(defun tellallmanager (x p)
  (dolist (sentence (finds x p *manager*))
    (request `(tell ,sentence) *client* *manager*)))

(defun untellallmanager (x p)
  (dolist (sentence (finds x p *manager*))
    (request `(tell (not ,sentence)) *client* *manager*)))

(defun telladministrator (p)
  (request `(tell ,p) *client* *manager*))

(defun untelladministrator (p)
  (request `(tell (not ,p)) *client* *manager*))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Copying and Renaming Concepts
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; rename
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod rename (old new source)
  (declare (ignore old new source))
  "Bad arguments to Rename.")

(defmethod rename (old new (source list))
  (dolist (x source) (rename old new x)))

(defmethod rename (old new (source symbol))
  (cond ((and (boundp source) (not (symbolp (symbol-value source))))
         (rename old new (symbol-value source)))
        (t (call-next-method old new source))))

(defmethod rename (old new (source theory))
  (let (olds news)
    (setq olds (facts old source))
    (setq news (subst new old olds))
    (kill old source)
    (dolist (new news) (save new source))
    'done))

(defmethod rename (old new (source (eql nil)))
  (declare (ignore old new))
  "No source selected.")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Duplicate
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod duplicate (old new source)
  (declare (ignore old new source))
  "Bad arguments to Duplicate.")

(defmethod duplicate (old new (source symbol))
  (cond ((and (boundp source) (not (symbolp (symbol-value source))))
         (duplicate old new (symbol-value source)))
        (t (call-next-method old new source))))

(defmethod duplicate (old new (source (eql nil)))
  (declare (ignore old new))
  "No source selected.")

(defmethod duplicate (old (newname string) (source theory))
  (let (case new olds news)
    (cond ((null newname) (format nil "No new name supplied for ~A." old))
          ((= (length newname) 0) (format nil "No new name supplied for ~A." old))
          (t (setq case (findx '?y `(casename ,old ?y) *manager*))
             (setq new (read-from-string (string-upcase newname)))
             (when case (save `(casename ,new ,newname) *manager*))
             (setq olds (remove-if #'(lambda (x) (find (car x) '(prettyname casename)))
			           (facts old source)))
             (setq news (subst new old olds))
             (dolist (new news) (insert new source))
             'done))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Move
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod move (object source target)
  (declare (ignore object source target))
  "Move works only with local agents.")

(defmethod move (object (source symbol) target)
  (cond ((and (boundp source) (not (symbolp (symbol-value source))))
         (move object (symbol-value source) target))
        (t (call-next-method object source target))))

(defmethod move (object source (target symbol))
  (cond ((and (boundp target) (not (symbolp (symbol-value target))))
         (move object source (symbol-value target)))
        (t (call-next-method object source target))))

(defmethod move (object (source theory) (target theory))
  (dolist (sent (facts object source)) (save sent target))
  (kill object source)
  'done)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Moving Tables
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Copytabledata
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod copytabledata (table source target)
  (declare (ignore table source target))
  "Incorrect arguments.")

(defmethod copytabledata (table (source symbol) target)
  (copytabledata table (symbol-value source) target))

(defmethod copytabledata (table source (target symbol))
  (copytabledata table source (symbol-value target)))

(defmethod copytabledata (table (source agent) (target agent))
  (let (facts ans)
    (setq facts (request `(ask-about ,table) *client* source))
    (setq ans (request `(tell (and . ,facts)) *client* target))
    (cond ((stringp ans) ans)
          (t (when (findp `(specialty ,(name source) ,table) *manager*)
               (telladministrator `(specialty ,(name target) ,table)))
             (when (findp `(interest ,(name source) ,table) *manager*)
               (telladministrator `(interest ,(name target) ,table)))
             (telladministrator `(rootrelation ,(name target) ,table))))
    'done))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Droptabledata
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod droptabledata (table source)
  (declare (ignore table source))
  "Incorrect arguments.")

(defmethod droptabledata (table (source symbol))
  (droptabledata table (symbol-value source)))

(defmethod droptabledata (table (source agent))
  (request `(eliminate ,table) *client* source)
  (untelladministrator `(specialty ,(name source) ,table))
  (untelladministrator `(interest ,(name source) ,table))
  (untelladministrator `(rootrelation ,(name source) ,table))
  'done)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Movetabledata
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod movetabledata (table source target)
  (copytabledata table source target)
  (droptabledata table source))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Moving Classes
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; copyclassdata
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod copyclassdata (class source target)
  (let (predicate)
    (cond ((not (setq predicate (find-predicate class)))
           "Error - Class has no predicate.")
          (t (copytabledata predicate source target)
             (telladministrator `(rootclass ,(name target) ,class))
             (dolist (rel (attributes class)) (copytabledata rel source target)) 
             'done))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; dropclassdata
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod dropclassdata (class target)
  (let (predicate)
    (cond ((not (setq predicate (find-predicate class)))
           "Error - Class has no predicate.")
          (t (droptabledata predicate target)
             (untelladministrator `(rootclass ,(name target) ,class))
             (dolist (table (attributes class)) (droptabledata table target))
             'done))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; moveclassdata
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod moveclassdata (class source target)
  (copyclassdata class source target)
  (dropclassdata class source))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Moving Contents of Agents
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; copyagentdata
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod copyagentdata (source target)
  (declare (ignore source target))
  "CopyAgentData called on non-agent.")

(defmethod copyagentdata ((source symbol) target)
  (copyagentdata (symbol-value source) target))

(defmethod copyagentdata ((source agent) (target symbol))
  (copyagentdata source (symbol-value target)))

(defmethod copyagentdata ((source agent) (target agent))
  (request `(tell (and . ,(contents source))) *client* target)
  'done)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; dropagentdata
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod dropagentdata ((source agent))
  (initialize source)
  'done)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; moveagentdata
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod moveagentdata ((source agent) (target agent))
  (copyagentdata source target)
  (dropagentdata source))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Transforms
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; applytransforms
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod applytransforms (source target)
  (declare (ignore source target))
  "ApplyTransforms called on non-agent.")

(defmethod applytransforms ((source symbol) target)
  (applytransforms (symbol-value source) target))

(defmethod applytransforms ((source agent) (target symbol))
  (applytransforms source (symbol-value target)))

(defmethod applytransforms ((source agent) (target agent))
  (request `(update . ,(contents source)) *client* target)
  'done)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Writing Rules
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; differentiate
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun differentiate (rule)
  (let (rules)
    (dolist (rule (brfs rule))
      (setq rules (diffrule rule rules)))
    (nreverse rules)))

(defun diffrule (rule rules)
  (cond ((atom rule))
        ((and (eq '<= (car rule)) (cddr rule) (null (cdddr rule)))
         (setq rules (cons `(<= (pos ,(cadr rule))
                                (pos ,(caddr rule))) rules))
         (setq rules (cons `(<= (neg ,(cadr rule))
                                (neg ,(caddr rule))) rules)))
        ((and (eq '<= (car rule)) (cdddr rule) (null (cddddr rule)))
         (setq rules (cons `(<= (pos ,(cadr rule))
                                (pos ,(caddr rule))
                                ,(cadddr rule)
                                (unprovable (neg ,(cadddr rule)))) rules))
         (setq rules (cons `(<= (pos ,(cadr rule))
                                (pos ,(cadddr rule))
                                ,(caddr rule)
                                (unprovable (neg ,(caddr rule)))) rules))
         (setq rules (cons `(<= (pos ,(cadr rule))
                                (pos ,(caddr rule))
                                (pos ,(cadddr rule))) rules))
         (setq rules (cons `(<= (neg ,(cadr rule))
                                (neg ,(caddr rule))
                                ,(cadddr rule)) rules))
         (setq rules (cons `(<= (neg ,(cadr rule))
                                (neg ,(cadddr rule))
                                ,(caddr rule)) rules)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; separate
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod separate (rel target)
  (separaterules rel target))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; separaterules
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod separaterules (rel target)
  (declare (ignore rel target))
  "SeparateRules works only with baskets.")

(defmethod separaterules (rel (target symbol))
  (cond ((and (boundp target) (not (symbolp (symbol-value target))))
         (separaterules rel (symbol-value target)))
        (t (call-next-method rel target))))

(defmethod separaterules (rel (target theory))
  (dolist (rule (separaterules rel nil)) (save rule target))
  'done)

(defmethod separaterules (rel (target (eql nil)))
  (separaterelation rel))

(defun separaterelation (rel)
  (let (keys columns keyvars body)
    (when (and (setq keys (find-keys rel)) (setq columns (find-columns rel)))
      (setq keyvars (mapcar #'specvar keys))
      (setq body (cons rel (mapcar #'specvar columns)))
      (do ((l columns (cdr l)) (m (cdr body) (cdr m)) (head) (nl))
          ((null l) (nreverse nl))
          (unless (find (car l) keys)
            (setq head (cons (car l) (append keyvars (list (car m)))))
            (setq nl (cons `(<= ,head ,body) nl)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; separatedata
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod separatedata (rel source target)
  (declare (ignore rel source target))
  "SeparateData works only with dataservers.")

(defmethod separatedata (rel (source symbol) target)
  (cond ((and (boundp source) (not (symbolp (symbol-value source))))
         (separatedata rel (symbol-value source) target))
        (t (call-next-method rel source target))))

(defmethod separatedata (rel source (target symbol))
  (cond ((and (boundp target) (not (symbolp (symbol-value target))))
         (separatedata rel source (symbol-value target)))
        (t (call-next-method rel source target))))

(defmethod separatedata (rel source (target dataserver))
  (let (keys columns)
    (when (and (setq keys (find-keys rel)) (setq columns (find-columns rel)))
      (do ((l (compute rel source) (cdr l)) (key nil nil))
          ((null l) 'done)
          (do ((m columns (cdr m)) (n (car l) (cdr n)) (kl keys))
              ((null m) (setq key (nreverse key)))
              (when (eq (car m) (car kl))
                (setq key (cons (car n) key) kl (cdr kl))))
          (do ((m columns (cdr m)) (n (car l) (cdr n)) (kl keys))
              ((null m) 'done)
              (cond ((eq (car m) (car kl)) (setq kl (cdr kl)))
                    (t (insert (cons (car m) (append key (list (car n)))) target)))))
      (dolist (column columns)
        (telladministrator `(specialty ,(name target) ,column))
        (telladministrator `(interest ,(name target) ,column))
        (telladministrator `(rootrelation ,(name target) ,column)))
      'done)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; consolidate
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod consolidate (rel target)
  (consolidaterules rel target))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; consolidaterules
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod consolidaterules (rel target)
  (declare (ignore rel target))
  "ConsolidateRules works only with baskets")

(defmethod consolidaterules (rel (target symbol))
  (cond ((and (boundp target) (not (symbolp (symbol-value target))))
         (consolidaterules rel (symbol-value target)))
        (t (call-next-method rel target))))

(defmethod consolidaterules (rel (target theory))
  (save (consolidaterules rel nil) target)
  'done)

(defmethod consolidaterules (rel (target (eql nil)))
  (let (keys columns keyvars body)
    (when (and (setq keys (find-keys rel)) (setq columns (find-columns rel)))
      (setq keyvars (mapcar #'specvar keys))
      (setq body (cons rel (mapcar #'specvar columns)))
      (do ((l columns (cdr l)) (m (cdr body) (cdr m)) (head) (rl))
          ((null l) `(<= ,body . ,(nreverse rl)))
          (unless (find (car l) keys)
            (setq head (cons (car l) (append keyvars (list (car m)))))
            (setq rl (cons head rl)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; consolidatedata
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod consolidatedata (rel source target)
  (declare (ignore rel source target))
  "ConsolidateRules works only with dataservers.")

(defmethod consolidatedata (rel (source symbol) target)
  (cond ((and (boundp source) (not (symbolp (symbol-value source))))
         (consolidatedata rel (symbol-value source) target))
        (t (call-next-method rel source target))))

(defmethod consolidatedata (rel source (target symbol))
  (cond ((and (boundp target) (not (symbolp (symbol-value target))))
         (consolidatedata rel source (symbol-value target)))
        (t (call-next-method rel source target))))

(defmethod consolidatedata (rel source (target dataserver))
  (let (keys columns keyvars vars query)
    (when (and (setq keys (find-keys rel)) (setq columns (find-columns rel)))
      (setq keyvars (mapcar #'specvar keys))
      (setq vars (mapcar #'specvar columns))
      (do ((l columns (cdr l)) (m vars (cdr m)) (condition) (rl))
          ((null l) (setq query (maksand (nreverse rl))))
          (unless (find (car l) keys)
            (setq condition (cons (car l) (append keyvars (list (car m)))))
            (setq rl (cons condition rl))))
      (dolist (answer (request `(ask-all ,vars ,query) *client* source))
        (insert (cons rel answer) target))
      (telladministrator `(specialty ,(name target) ,rel))
      (telladministrator `(interest ,(name target) ,rel))
      (telladministrator `(rootrelation ,(name target) ,rel))
      'done)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; reify
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod reify (rel)
  (let (arity keys class predicate)
    (cond ((not (integerp (setq arity (find-arity rel))))
           "Error - Relation has no arity.")
          ((null (setq keys (find-keys rel)))
           "Error - Relation has no key.")
          ((cdr keys)
           "Error - Relation has more than one key.")
          (t (setq class (intern (strappend (symbol-name rel) ".ENTITY")))
             (telladministrator `(isa ,class class))
             (unless (setq predicate (find-predicate class))
               (setq predicate (intern (strappend (symbol-name class) ".ISA")))
               (telladministrator `(predicate ,class ,predicate)))
             (do ((i 1 (1+ i)) (slot))
                 ((> i arity))
                 (setq slot (intern (strappend (symbol-name class) "." (princ-to-string i))))
                 (telladministrator `(attribute ,class ,slot)))
             (telladministrator `(isa ,predicate naryrelation))
             (telladministrator `(arity ,predicate 1))
             (dolist (attribute (attributes class))
               (telladministrator `(isa ,attribute attributerelation))
               (telladministrator `(arity ,attribute 2))
               (telladministrator `(domain ,attribute ,class))
               (telladministrator `(unique ,attribute yes))
               (telladministrator `(total ,attribute yes)))
             class))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; reifyrules
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod reifyrules (rel class target)
  (declare (ignore rel class target))
  "ReifyRules works only with baskets")

(defmethod reifyrules (rel class (target symbol))
  (cond ((and (boundp target) (not (symbolp (symbol-value target))))
         (reifyrules rel class (symbol-value target)))
        (t (call-next-method rel class target))))

(defmethod reifyrules (rel class (target theory))
  (let (answer)
    (cond ((stringp (setq answer (reify rel))) answer)
          (t (dolist (rule (reifyrelation rel class (or (find-classifier (name target)) 'isa)))
               (save rule target))
             'done))))

(defmethod reifyrules (rel class (target (eql nil)))
  (let (answer)
    (cond ((stringp (setq answer (reify rel))) answer)
          (t (reifyrelation rel class 'isa)))))

(defun reifyrelation (rel class classifier)
  (let (columns key keyvar predicate attributes entity base rules)
    (setq columns (find-columns rel))
    (setq key (car (find-keys rel)))
    (setq keyvar (specvar key))
    (setq predicate (find-predicate class))
    (setq attributes (attributes class))
    (setq rules (cons `(<= (,classifier ?x ,class) (,predicate ?x)) rules))
    (setq rules (cons `(<= (,predicate ?x) (,(car attributes) ?x ?y)) rules))
    (dolist (attribute attributes)
      (setq rules (cons `(<= (isone ?x ,class) (,attribute ?x ?y)) rules))
      (setq rules (cons `(<= (not (,attribute ?x ?y)) (isnot ?x ,class)) rules)))
    (setq entity (specvar class))
    (setq base (cons rel (mapcar #'specvar columns)))
    (do ((l attributes (cdr l)) (m (cdr base) (cdr m)) (i 1 (1+ i)) (head) (out))
        ((null l))
      (cond ((member (find-range (car l)) '(nil thing string))
             (setq head (list (car l) entity (car m)))
             (setq rules (cons `(<= ,head ,base (makestring ,entity ,keyvar))
                               rules)))
            (t (setq out (specvar i))
               (setq head (list (car l) entity out))
               (setq rules (cons `(<= ,head ,base
                                      (makestring ,entity ,keyvar)
                                      (makestring ,out ,(car m))) rules)))))
    (nreverse rules)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; reifydata
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod reifydata (rel class source target )
  (declare (ignore rel class source target))
  "ReifyData works only with dataservers.")

(defmethod reifydata (rel class (source symbol) target)
  (cond ((and (boundp source) (not (symbolp (symbol-value source))))
         (reifydata rel class (symbol-value source) target))
        (t (call-next-method rel class source target))))

(defmethod reifydata (rel class source (target symbol))
  (cond ((and (boundp target) (not (symbolp (symbol-value target))))
         (reifydata rel class source (symbol-value target)))
        (t (call-next-method rel class source target))))

(defmethod reifydata (rel class (source dataserver) (target dataserver))
  (let (columns key predicate attributes ranges classifier)
    (setq columns (find-columns rel))
    (setq key (position (car (find-keys rel)) columns))
    (setq predicate (find-predicate class))
    (setq attributes (attributes class))
    (setq ranges (mapcar #'find-range attributes))
    (setq classifier (or (find-classifier (name target)) 'isa))
    (do ((l (compute rel source) (cdr l)) (item) (value))
        ((null l))
        (setq item (symbolize (elt (car l) key)))
        (insert (list classifier item class) target)
        (insert (list predicate item) target)
        (do ((m attributes (cdr m)) (r ranges (cdr r)) (n (car l) (cdr n)))
            ((null m))
            (if (member (car r) '(nil string thing)) 
                (setq value (if (car n) (car n) ""))
                (setq value (symbolize (car n))))
            (insert (list (car m) item value) target)))
    (telladministrator `(rootclass ,(name target) ,class))
    (telladministrator `(specialty ,(name target) ,predicate))
    (telladministrator `(interest ,(name target) ,predicate))
    (telladministrator `(rootrelation ,(name target) ,predicate))
    (dolist (attribute attributes)
      (telladministrator `(specialty ,(name target) ,attribute))
      (telladministrator `(interest ,(name target) ,attribute))
      (telladministrator `(rootrelation ,(name target) ,attribute)))
    'done))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; tabulate
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod tabulate (class)
  (let (attributes rel arity)
    (setq attributes (attributes class))
    (setq rel (intern (strappend (symbol-name class) ".TABLE")))
    (setq arity (1+ (length attributes)))
    (telladministrator `(isa ,rel naryrelation))
    (telladministrator `(arity ,rel ,arity))
    (do ((i 1 (1+ i)) (column))
        ((> i arity))
        (setq column (intern (strappend (symbol-name rel) "." (princ-to-string i))))
        (telladministrator `(column ,rel ,column)))
    (dolist (column (columns rel))
      (telladministrator `(isa ,column attributerelation))
      (telladministrator `(arity ,column 2)))
    rel))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; tabulaterules
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod tabulaterules (class rel target)
  (declare (ignore class rel target))
  "TabulateRules works only with baskets")

(defmethod tabulaterules (class rel (target symbol))
  (cond ((and (boundp target) (not (symbolp (symbol-value target))))
         (tabulaterules class rel (symbol-value target)))
        (t (call-next-method class rel target))))

(defmethod tabulaterules (class rel (target theory))
  (dolist (rule (tabulaterules class rel nil)) (save rule target))
  'done)

(defmethod tabulaterules (class rel (target (eql nil)))
  (let (predicate attributes vars body)
    (setq predicate (find-predicate class))
    (setq attributes (attributes class))
    (do ((l attributes (cdr l)) (i 1 (1+ i)) (vl) (nl))
        ((null l)
         (setq vars (cons '?x (nreverse vl)))
         (setq body (cons (list predicate '?x) (nreverse nl))))
        (setq vl (cons (specvar i) vl))
        (setq nl (cons (list (car l) '?x (car vl)) nl)))
    (list `(<= (,rel . ,vars) . ,body))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; tabulatedata
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod tabulatedata (class rel source target )
  (declare (ignore class rel source target))
  "TabulateData works only with dataservers.")

(defmethod tabulatedata (class rel (source symbol) target)
  (cond ((and (boundp source) (not (symbolp (symbol-value source))))
         (tabulatedata class rel (symbol-value source) target))
        (t (call-next-method class rel source target))))

(defmethod tabulatedata (class rel source (target symbol))
  (cond ((and (boundp target) (not (symbolp (symbol-value target))))
         (tabulatedata class rel source (symbol-value target)))
        (t (call-next-method class rel source target))))

(defmethod tabulatedata (class rel (source dataserver) (target dataserver))
  (let (predicate attributes vars query)
    (setq predicate (find-predicate class))
    (setq attributes (attributes class))
    (do ((l attributes (cdr l)) (i 1 (1+ i)) (vl) (nl))
        ((null l)
         (setq vars (cons '?x (nreverse vl)))
         (setq query (maksand (cons (list predicate '?x) (nreverse nl)))))
        (setq vl (cons (specvar i) vl))
        (setq nl (cons (list (car l) '?x (car vl)) nl)))
    (dolist (row (finds vars query source))
      (insert (cons rel (mapcar #'princ-to-string row)) target))
    (telladministrator `(specialty ,(name target) ,rel))
    (telladministrator `(interest ,(name target) ,rel))
    (telladministrator `(rootrelation ,(name target) ,rel))
    'done))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; interrelate
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod interrelate (rel target)
  (interrelaterules rel target))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; interrelaterules
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod interrelaterules (rel target)
  (declare (ignore rel target))
  "InterrelateRules works only with baskets")

(defmethod interrelaterules (rel (target symbol))
  (cond ((and (boundp target) (not (symbolp (symbol-value target))))
         (interrelaterules rel (symbol-value target)))
        (t (call-next-method rel target))))

(defmethod interrelaterules (rel (target theory))
  (let (arity vars)
    (cond ((not (integerp (setq arity (find-arity rel))))
           "Error - Relation has no arity.")
          (t (setq vars (specvars arity))
             (dolist (super (finds '?s `(superrelation ,rel ?s) *manager*))
               (unless (eq super 'true)
                 (save `(<= (,super . ,vars) (,rel . ,vars)) target)
                 (save `(=> (,rel . ,vars) (,super . ,vars)) target)))
             'done))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; incorporate
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod incorporate (class target)
  (incorporaterules class target))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; incorporaterules
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod incorporaterules (class target)
  (declare (ignore class target))
  "IncorporateRules works only with baskets")

(defmethod incorporaterules (class (target symbol))
  (cond ((and (boundp target) (not (symbolp (symbol-value target))))
         (incorporaterules class (symbol-value target)))
        (t (call-next-method class target))))

(defmethod incorporaterules (class (target theory))
  (let (predicate superpredicate classifier)
    (setq predicate (makpred '?x class (name target)))
    (setq classifier (makisa '?x class (name target)))
    (dolist (super (finds '?s `(superclass ,class ?s) *manager*))
      (setq superpredicate (makpred '?x super (name target)))
      (unless (eq super 'thing)
        (save `(<= ,superpredicate ,predicate) target)
        (save `(=> ,predicate ,superpredicate) target)))
    (save `(<= ,classifier ,predicate) target)
    (save `(=> ,predicate ,classifier) target)
    (save `(<= (isone ?x ,class) ,predicate) target)
    (save `(<= (not ,predicate) (isnot ?x ,class)) target)
    (dolist (slot (finds '?s `(attribute ,class ?s) *manager*))
      (save `(<= (isone ?x ,class) (,slot ?x ?y)) target)
      (save `(<= (not (,slot ?x ?y)) (isnot ?x ,class)) target)
      (interrelaterules slot target))
    'done))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; integrate
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod integrate (source target)
  (integraterules source target))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; integraterules
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod integraterules (source target)
  (declare (ignore source target))
  "IntegrateRules works only with baskets")

(defmethod integraterules (source (target symbol))
  (cond ((and (boundp target) (not (symbolp (symbol-value target))))
         (integraterules source (symbol-value target)))
        (t (call-next-method source target))))

(defmethod integraterules (source (target theory))
  (dolist (class (find-residents (name source))) (incorporaterules class target))
  'done)

;(defun find-residents (source)
;  (finds '?c `(and (specialty ,source ?r) (predicate ?c ?r)) *manager*))

(defun find-residents (source)
  (finds '?c `(rootclass ,source ?c) *manager*))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; backwardrules
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod backwardrules (rel source target)
  (declare (ignore rel source target))
  "BackwardRules works only with baskets.")

(defmethod backwardrules (rel (source symbol) target)
  (cond ((and (boundp source) (not (symbolp (symbol-value source))))
         (backwardrules rel (symbol-value source) target))
        (t (call-next-method rel source target))))

(defmethod backwardrules (rel source (target symbol))
  (cond ((and (boundp target) (not (symbolp (symbol-value target))))
         (backwardrules rel source (symbol-value target)))
        (t (call-next-method rel source target))))

(defmethod backwardrules (rel (source theory) (target theory))
  (dolist (rule (backwardrules rel source nil)) (save rule target))
  'done)

(defmethod backwardrules (rel (source theory) (target (eql nil)))
  (let (rules)
    (dolist (rule (indexees rel source))
      (setq rules (nreconc (makposrules rel (cnf rule)) rules)))
    (dolist (rule (indexees rel source))
      (setq rules (nreconc (maknegrules rel (cnf rule)) rules)))
    (remove-duplicates (standardize (nreverse rules)) :test #'samep)))

(defun makposrules (rel clause)
  (do ((l (cdr clause) (cdr l)) (rule) (nl))
      ((null l) (nreverse nl))
      (when (and (listp (car l)) (eq rel (caar l)))
        (setq rule `(<= ,(car l) . ,(mapcar #'maknot (remove (car l) (cdr clause)))))
        (setq nl (cons rule nl)))))

(defun maknegrules (rel clause)
  (do ((l (cdr clause) (cdr l)) (rule) (nl))
      ((null l) (nreverse nl))
      (when (and (listp (car l)) (eq 'not (caar l))
                 (listp (cadar l)) (eq rel (caadar l)))
        (setq rule `(<= ,(car l) . ,(mapcar #'maknot (remove (car l) (cdr clause)))))
        (setq nl (cons rule nl)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; forwardrules
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod forwardrules (rel source target)
  (declare (ignore rel source target))
  "ForwardRules works only with local baskets.")

(defmethod forwardrules (rel (source symbol) target)
  (cond ((and (boundp source) (not (symbolp (symbol-value source))))
         (forwardrules rel (symbol-value source) target))
        (t (call-next-method rel source target))))

(defmethod forwardrules (rel source (target symbol))
  (cond ((and (boundp target) (not (symbolp (symbol-value target))))
         (forwardrules rel source (symbol-value target)))
        (t (call-next-method rel source target))))

(defmethod forwardrules (rel (source theory) (target theory))
  (dolist (rule (forwardrules rel source nil)) (save rule target))
  'done)

(defmethod forwardrules (rel (source theory) (target (eql nil)))
  (let (rules)
    (dolist (rule (indexees rel source))
      (setq rules (nreconc (makposimplications rel (cnf rule)) rules)))
    (dolist (rule (indexees rel source))
      (setq rules (nreconc (maknegimplications rel (cnf rule)) rules)))
    (remove-duplicates (nreverse rules) :test #'samep)))

(defun makposimplications (rel clause)
  (do ((l (cdr clause) (cdr l)) (literals) (rule) (nl))
      ((null l) (nreverse nl))
      (when (and (listp (car l)) (eq rel (caar l)))
        (setq literals (mapcar #'maknot (remove (car l) (cdr clause))))
        (dolist (literal literals)
          (setq rule `(=> ,literal .
                          ,(nconc (remove literal literals) (list (car l)))))
          (setq nl (cons rule nl))))))

(defun maknegimplications (rel clause)
  (do ((l (cdr clause) (cdr l)) (literals) (rule) (nl))
      ((null l) (nreverse nl))
      (when (and (listp (car l)) (eq 'not (caar l))
                 (listp (cadar l)) (eq rel (caadar l)))
        (setq literals (mapcar #'maknot (remove (car l) (cdr clause))))
        (dolist (literal literals)
          (setq rule `(=> ,literal .
                          ,(nconc (remove literal literals) (list (car l)))))
          (setq nl (cons rule nl))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; triggerrules
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod triggerrules (rel source target)
  (declare (ignore rel source target))
  "TriggerRules works only with baskets")

(defmethod triggerrules (rel (source symbol) target)
  (cond ((and (boundp source) (not (symbolp (symbol-value source))))
         (triggerrules rel (symbol-value source) target))
        (t (call-next-method rel source target))))

(defmethod triggerrules (rel source (target symbol))
  (cond ((and (boundp target) (not (symbolp (symbol-value target))))
         (triggerrules rel source (symbol-value target)))
        (t (call-next-method rel source target))))

(defmethod triggerrules (rel (source theory) (target theory))
  (dolist (rule (triggerrules rel source nil)) (save rule target))
  'done)

(defmethod triggerrules (rel (source theory) (target (eql nil)))
  (let (rules)
    (dolist (rule (indexees rel source))
      (setq rules (nreconc (makpostriggers rel (cnf rule)) rules)))
    (dolist (rule (indexees rel source))
      (setq rules (nreconc (maknegtriggers rel (cnf rule)) rules)))
    (remove-duplicates (nreverse rules) :test #'samep)))

(defun makpostriggers (rel clause)
  (do ((l (cdr clause) (cdr l)) (rule) (literals) (nl))
      ((null l) (nreverse nl))
      (when (and (listp (car l)) (eq 'not (caar l))
                 (listp (cadar l)) (eq rel (caadar l)))
        (setq literals (remove (car l) (cdr clause)))
        (dolist (literal literals)
          (setq rule `(=> ,(maknot (car l)) .
                          ,(nconc (mapcar #'maknot (remove literal literals))
                                  (list literal))))
          (setq nl (cons rule nl))))))

(defun maknegtriggers (rel clause)
  (do ((l (cdr clause) (cdr l)) (rule) (literals) (nl))
      ((null l) (nreverse nl))
      (when (and (listp (car l)) (eq rel (caar l)))
        (setq literals (remove (car l) (cdr clause)))
        (dolist (literal literals)
          (setq rule `(=> ,(maknot (car l)) .
                          ,(nconc (mapcar #'maknot (remove literal literals))
                                  (list literal))))
          (setq nl (cons rule nl))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; evert
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod evertrules (rel source target)
  (declare (ignore rel source target))
  "EvertRules works only with baskets.")

(defmethod evertrules (rel (source symbol) target)
  (cond ((and (boundp source) (not (symbolp (symbol-value source))))
         (evertrules rel (symbol-value source) target))
        (t (call-next-method rel source target))))

(defmethod evertrules (rel source (target symbol))
  (cond ((and (boundp target) (not (symbolp (symbol-value target))))
         (evertrules rel source (symbol-value target)))
        (t (call-next-method rel source target))))

(defmethod evertrules (rel (source theory) (target theory))
  (dolist (rule (eversion rel source)) (save rule target))
  'done)

(defmethod evertrules (rel (source theory) (target (eql nil)))
  (eversion rel source))

(defmethod evertrules ((rel (eql nil)) (source dataserver) (target dataserver))
  (mapc #'(lambda (p) (save p target)) (eversions source))
  'done)

(defmethod evertrules ((rel (eql nil)) (source dataserver) (target (eql nil)))
  (eversions source))

(defun eversions (source)
  (do ((l (tables source) (cdr l)) (nl))
      ((null l) (nreverse nl))
      (setq nl (nreconc (eversion (car l) source) nl))))

(defun eversion (rel source)
  (let (rules)
    (setq rules (collect rel (indexees rel source)))
    (cond ((null rules) nil)
          (t (reversion (standardize rules))))))

(defun collect (rel rules)
  (do ((l rules (cdr l)) (nl))
      ((null l) (nreverse nl))
      (cond ((atom (car l)))
            ((and (eq '<= (caar l)) (listp (cadar l)) (eq rel (caadar l)))
             (setq nl (cons (car l) nl))))))

(defun tables (th)
  (do ((l (contents th) (cdr l)) (nl))
      ((null l) (nreverse nl))
      (setq nl (adjoin (operator (car l)) nl))))

;;; bug when variable repeated

(defun standardize (rules)
  (let (head (*var-count* 0))
    (do ((l (arguments (car rules)) (cdr l)) (nl))
        ((null l)
         (setq head (cons (operator (car rules)) (nreverse nl)))
         (when (and (listp (cadar rules)) (eq 'not (caadar rules)))
           (setq head (maknot head))))
        (setq nl (cons (uservar) nl)))
    (do ((l rules (cdr l)) (body) (nl))
        ((null l) (nreverse nl))
        (setq body (cddar l))
        (do ((m (arguments (car l)) (cdr m)) (n (arguments head) (cdr n)))
            ((null m) (setq nl (cons `(<= ,head . ,body) nl)))
            (cond ((equal (car m) (car n)))
                  ((varp (car m)) (setq body  (subst (car n) (car m) body)))
                  (t (setq body (cons `(same ,(car n) ,(car m))
                                      (subst (car n) (car m) body)))))))))

(defun oldreversion (rules)
  (let (normals abnormals news rule)
    (do ((l rules (cdr l)) (i 1 (1+ i)) (vl (vars (cadar rules))) (rel) (nl))
        ((null l) (setq rule `(<= ,(cadar rules) ,(maksor (nreverse nl)))))
        (cond ((existentialp (cddar l) vl)
               (setq rel (newrelation (caadar l) i))
               (setq rule `(<= ,(cons rel (cdadar l)) . , (cddar l)))
               (setq nl (cons (cons rel (cdadar l)) nl))
               (setq news (cons rule news)))
              (t (setq nl (cons (maksand (cddar l)) nl)))))
    (setq normals (positives (revert rule)))
    (do ((l news (cdr l)))
        ((null l))
        (setq abnormals (nconc (positives (revert (existentialize (car l)))) abnormals)))
    (do ((l abnormals (cdr l)) (rule))
        ((null l))
        (setq rule (findrule (caddar l) normals))
        (rplacd (cdar l) (cddr rule)))
    (do ((l news (cdr l)))
        ((null l))
        (setq normals (delete (findrule (cadar l) normals) normals :test #'eq)))
    (nconc normals abnormals (nreverse news))))

(defun reversion (rules)
  (let (normals abnormals news rule)
    (do ((l rules (cdr l)) (i 1 (1+ i)) (vl (vars (cadar rules))) (rel) (nl))
        ((null l) (setq rule `(<= ,(cadar rules) ,(maksor (nreverse nl)))))
        (cond ((existentialp (cddar l) vl)
               (setq rel (newrelation (caadar l) i))
               (setq rule `(<= ,(cons rel (cdadar l)) . , (cddar l)))
               (setq nl (cons (cons rel (cdadar l)) nl))
               (setq news (cons rule news)))
              (t (setq nl (cons (maksand (cddar l)) nl)))))
    (setq normals (positives (revert rule)))
    (do ((l news (cdr l)))
        ((null l))
        (setq abnormals (nconc (positives (revert (existentialize (car l)))) abnormals)))
    (nconc normals (nreverse news) abnormals)))

(defun findrule (head rules)
  (do ((l rules (cdr l)))
      ((null l) nil)
      (when (equal head (cadar l)) (return (car l)))))

(defun existentialp (x vl)
  (cond ((varp x) (not (find x vl)))
        ((atom x) nil)
        (t (some #'(lambda (x) (existentialp x vl)) x))))

(defun newrelation (rel i)
  (intern (strappend (symbol-name rel) (princ-to-string i))))

(defun existentialize (rule)
  (let (heads tails news)
    (setq heads (vars (cadr rule)) tails (vars (cddr rule)))
    (cond ((setq news (set-difference tails heads))
           `(<= ,(cadr rule) (exists ,news ,(maksand (cddr rule)))))
          (t rule))))

(defun revert (rule)
  (list '<= (maknot (cadr rule)) (maknot (caddr rule))))

(defun positives (rule)
  (do ((l (clauses rule) (cdr l)) (nl))
      ((null l) (nreverse nl))
      (setq nl (nreconc (clauserules (car l)) nl))))

(defun clauserules (clause)
  (do ((l (cdr clause) (cdr l)) (nl))
      ((null l) (nreverse nl))
      (cond ((and (listp (car l)) (eq 'not (caar l))))
            (t (setq nl (cons (list* '<=  (car l)
                                     (mapcar #'maknot (remove (car l) (cdr clause))))
                              nl))))))

(defun find-specialties (x)
  (finds '?x `(specialty ,(name x) ?x) *manager*))

(defun find-interests (x)
  (finds '?x `(interest ,(name x) ?x) *manager*))

(defun printrules (rules)
  (mapc #'pprint rules)
  'done)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Materialization
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; materializeschema
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod materializeschema (class source target)
  (dolist (class (find-subclasses class))
    (materializeschema class source target))
  (materializeclass class source target))

(defmethod rematerializeschema (class source target)
  (dematerializeschema class target)
  (materializeschema class source target))

(defmethod dematerializeschema (class target)
  (dematerializeclass class target)
  (dolist (class (find-subclasses class))
    (dematerializeschema class target))
  'done)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; materializetree
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod materializetree (class source target)
  (let (predicate)
    (dolist (class (find-subclasses class))
      (materializetree class source target))
    (when (setq predicate (find-predicate class))
      (materializetable predicate source target))
    'done))

(defmethod rematerializetree (class source target)
  (dematerializetree class target)
  (materializetree class source target))

(defmethod dematerializetree (class target)
  (let (predicate)
    (when (setq predicate (find-predicate class))
      (dematerializetable predicate target))
    (dolist (class (find-subclasses class))
      (dematerializetree class target))
    'done))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; materializeclass
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod materializeclass (class source target)
  (let (predicate)
    (cond ((not (setq predicate (find-predicate class)))
           "Error - Class has no predicate.")
          (t (materializetable predicate source target)
             (dolist (slot (attributes class))
               (materializetable slot source target))
             'done))))

(defmethod rematerializeclass (class source target)
  (dematerializeclass class target)
  (materializeclass class source target))

(defmethod dematerializeclass (class target)
  (let (predicate)
    (cond ((not (setq predicate (find-predicate class)))
           "Error - Class has no predicate.")
          (t (dematerializetable predicate target)
             (dolist (slot (attributes class)) (dematerializetable slot target))
             'done))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; materializetable
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod materializetable (rel source target)
  (forwarder rel source target))

(defmethod materializetable (rel (source symbol) target)
  (cond ((and (boundp source) (not (symbolp (symbol-value source))))
         (materializetable rel (symbol-value source) target))
        (t (call-next-method rel source target))))

(defmethod materializetable (rel source (target symbol))
  (cond ((and (boundp target) (not (symbolp (symbol-value target))))
         (materializetable rel source (symbol-value target)))
        (t (call-next-method rel source target))))

(defmethod materializetable (rel source (target fastserver))
  (materializer rel source target))

(defmethod materializetable (rel source (target dataserver))
  (materializer rel source target))

(defmethod materializetable (rel source (target ruleserver))
  (materializer rel source target))

(defmethod materializetable (rel source (target fullserver))
  (materializer rel source target))

(defun forwarder (rel source target)
  (let (answers)
    (setq answers (mapcar #'(lambda (x) (cons rel x)) (compute rel source)))
    (request `(tell ,(maksand answers)) *client* target)
    (insert `(material ,(name target) ,rel) *manager*)
    'done))

(defun materializer (rel source target)
  (dolist (x (compute rel source)) (insert (cons rel x) target))
  (insert `(material ,(name target) ,rel) *manager*)
  'done)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; rematerializetable
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod rematerializetable (table source target)
  (dematerializetable table target)
  (materializetable table source target))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; dematerializetable
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod dematerializetable (rel target)
  (when (materialishp (name target) rel) (unforwarder rel target)))

(defmethod dematerializetable (rel (target symbol))
  (cond ((and (boundp target) (not (symbolp (symbol-value target))))
         (dematerializetable rel (symbol-value target)))
        (t (call-next-method rel target))))

(defmethod dematerializetable (rel (target fastserver))
  (when (materialishp (name target) rel) (dematerializer rel target)))

(defmethod dematerializetable (rel (target dataserver))
  (when (materialishp (name target) rel) (dematerializer rel  target)))

(defmethod dematerializetable (rel (target ruleserver))
  (when (materialishp (name target) rel) (dematerializer rel target)))

(defmethod dematerializetable (rel (target fullserver))
  (when (materialishp (name target) rel) (dematerializer rel target)))

(defun unforwarder (rel target)
  (untelladministrator `(material ,(name target) ,rel))
  (request `(eliminate ,rel) *client* target)
  'done)

(defun dematerializer (rel target)
  (unmaterial (name target) rel)
  (dolist (sentence (indexees rel target))
    (when (and (listp sentence) (eq rel (car sentence)))
      (fullunindex sentence sentence target)))
  (remrel rel target)
  'done)

(defun remrel (rel th)
  (if (null (setf (content th) (reldelq rel (content th))))
      (setq *theories* (delete th *theories* :count 1)))
  rel)

(defun reldelq (rel dl)
  (cond ((null dl) nil)
        (t (do ((l (car dl) (cdr l)))
               ((null l) (setq dl nil))
               (cond ((relp rel (car l)) (rplaca dl (cdar dl)))
                     (t (return dl))))
           (do ((l (car dl)))
               ((null (cdr l)) dl)
               (cond ((relp rel (cadr l))
                      (rplacd l (cddr l))
                      (if (null (cdr l)) (rplacd dl l)))
                     (t (setq l (cdr l))))))))

(defun relp (rel x)
  (and (listp x) (eq rel (car x))))

(defun materialishp (agent table)
  (do ((l (indexees table *manager*) (cdr l)))
      ((null l) nil)
      (when (and (listp (car l)) (eq (caar l) 'material)
                 (eq (cadar l) agent) (eq (caddar l) table))
        (return t))))

(defun unmaterial (agent table)
  (do ((l (indexees table *manager*) (cdr l)))
      ((null l) nil)
      (when (and (listp (car l)) (eq (caar l) 'material)
                 (eq (cadar l) agent) (eq (caddar l) table))
        (uninsert (car l) *manager*)
        (return t))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; concordance
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod concordance (attribute source map (target symbol))
  (concordance attribute source map (symbol-value target)))

(defmethod concordance (attribute source map (target theory))
  (telladministrator `(isa ,map naryrelation))
  (telladministrator `(arity ,map 2))
  (telladministrator `(specialty warehouse ,map))
  (telladministrator `(interest warehouse ,map))
  (dolist (item (request `(ask-all ?y ,(list attribute '?x '?y)) nil source))
    (insert (list map item item) target))
  (format nil "Concordance <A HREF=/~A/displaytable?relation=~A>~A</A> created."
          (name target) map map))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; resetagent
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod resetagent (target meta)
  (declare (ignore target meta))
  "ResetAgent works only with dataservers.")

(defmethod resetagent ((target symbol) meta)
  (cond ((and (boundp target) (not (symbolp (symbol-value target))))
         (resetagent (symbol-value target) meta))
        (t (call-next-method target meta))))

(defmethod resetagent ((target theory) meta)
  (when (eq meta 'yes)
    (untellallmanager `(rootclass ,(name target) ?x) `(rootclass ,(name target) ?x))
    (untellallmanager `(specialty ,(name target) ?x) `(specialty ,(name target) ?x))
    (untellallmanager `(interest ,(name target) ?x) `(interest ,(name target) ?x))
    (untellallmanager `(rootrelation ,(name target) ?x) `(rootrelation ,(name target) ?x)))
  (initialize target)
  'done)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; removeduplicatedata
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod removeduplicatedata (target)
  (declare (ignore target))
  "RemoveDuplicateData works only with dataservers.")

(defmethod removeduplicatedata ((target symbol))
  (cond ((and (boundp target) (not (symbolp (symbol-value target))))
         (removeduplicatedata (symbol-value target)))
        (t (call-next-method target))))

(defmethod removeduplicatedata ((target (eql nil)))
  "No source selected.")

(defmethod removeduplicatedata ((target theory))
  (let (olds news)
    (setq olds (contents target))
    (setq news (uniquificate olds))
    (do ((l olds (cdr l)) (m news))
        ((null l))
        (cond ((eq (car l) (car m)) (setq m (cdr m)))
              (t (uninsert (car l) target))))
    'done))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; countrows
;;; countcells
;;; countbytes
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod countrows ((agent symbol))
  (cond ((and (boundp agent) (not (symbolp (symbol-value agent))))
         (countrows (symbol-value agent)))
        (t (call-next-method agent))))

(defmethod countrows (th)
  (length (contents th)))


(defmethod countcells ((agent symbol))
  (cond ((and (boundp agent) (not (symbolp (symbol-value agent))))
         (countcells (symbol-value agent)))
        (t (call-next-method agent))))

(defmethod countcells ((th theory))
  (do ((l (contents th) (cdr l)) (n 0))
      ((null l) n)
      (setq n (+ (length (car l)) n))))


(defmethod countbytes ((agent symbol))
  (cond ((and (boundp agent) (not (symbolp (symbol-value agent))))
         (countbytes (symbol-value agent)))
        (t (call-next-method agent))))

(defmethod countbytes ((th theory))
  (do ((l (contents th) (cdr l)) (n 0))
      ((null l) (* n 8))
      (setq n (+ (* (length (car l)) 2) n 1))))

(defun conses (x)
  (cond ((atom x) 0)
        (t (do ((l x (cdr l)) (n 0))
               ((null l) n)
               (setq n (+ (conses (car l)) 1 n))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Checkdata checks the sentences in a database for restrictions on arity and
;;; uniqueness.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod checkdata (source)
  (declare (ignore source))
  "Checkdata works only with dataservers.")

(defmethod checkdata ((agent symbol))
  (cond ((and (boundp agent) (not (symbolp (symbol-value agent))))
         (checkdata (symbol-value agent)))
        (t (call-next-method agent))))

(defmethod checkdata ((source theory))
  (let ((errors 0))
    (dolist (sent (contents source))
      (setq errors (+ (checksent sent) errors)))
    (dolist (rel (tables source))
      (setq errors (+ (checkuniqueness rel source) errors)))
    errors))

(defun badarities (source)
  (do ((l (contents source) (cdr l)) (arity) (nl))
      ((null l) (nreverse nl))
      (cond ((atom (car l)))
            ((find (caar l) '(not and or unprovable => <=)))
            ((not (setq arity (find-arity (caar l)))))
            ((equal arity (1- (length (car l)))))
            (t (setq nl (cons `(error "Bad arity" ,(car l)) nl))))))

(defun checksent (sent)
  (cond ((atom sent) 0)
        ((find (car sent) '(not and or unprovable => <=)) 0)
        (t (checkarity sent))))

(defun checkarity (sent)
  (let ((arity (find-arity (car sent))))
    (cond ((and arity (not (equal arity (1- (length sent) ))))
           (fresh-line)
           (princ "Bad arity - ") (princ sent)
           1)
          (t 0))))

(defun nonuniques (rel source)
  (let (columns keys)
    (cond ((findp `(unique ,rel yes) *manager*)
           (finds `(,rel ?x ?y) `(and (,rel ?x ?y) (,rel ?x ?z) (distinct ?y ?z))
                  source))
          ((and (setq columns (find-columns rel)) (setq keys (find-keys rel)))
           (do ((l columns (cdr l)) (m keys) (xl) (yl) (nl) (*var-count* 0))
               ((null l) (setq xl (nreverse xl) yl (nreverse yl) nl (nreverse nl))
                (finds (cons rel xl) `(and ,(cons rel xl) ,(cons rel yl) ,(maksor nl))
                       source))
               (cond ((eq (car l) (car m))
                      (setq xl (cons (genvar) xl) yl (cons (car xl) yl))
                      (setq m (cdr m)))
                     (t (setq xl (cons (genvar) xl) yl (cons (genvar) yl))
                        (setq nl (cons `(distinct ,(car xl) ,(car yl)) nl)))))))))

(defun checkuniqueness (rel source)
  (let (violations)
    (setq violations (nonuniques rel source))
    (dolist (violation violations)
      (fresh-line)
      (princ "Uniqueness violation - ") (princ violation))
    (length violations)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Checkrules checks a dataserver for a variety of bad or inefficient
;;; conditions and prints out warnings for each condition found.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod checkrules (source)
  (declare (ignore source))
  "Checkrules works only with ruleservers.")

(defmethod checkrules ((agent symbol))
  (cond ((and (boundp agent) (not (symbolp (symbol-value agent))))
         (checkrules (symbol-value agent)))
        (t (call-next-method agent))))

(defmethod checkrules ((th theory))
  (do ((l (contents th) (cdr l)) (flag nil nil) (n 0))
      ((null l) n)
      (when (unsafep (car l))
        (setq flag t n (1+ n))
        (fresh-line)
        (princ "The following rule is unsafe (unused variable in head).")
        (terpri))
      (when (find (car l) (cdr l) :test #'samep)
        (setq flag t n (1+ n))
        (fresh-line)
        (princ "The following rule is duplicated.")
        (terpri))
      (when (emptyp (car l))
        (setq flag t n (1+ n))
        (fresh-line)
        (princ "The following rule is empty (type problem or bad arity).")
        (terpri))
      (when (cartesianp (car l))
        (setq flag t n (1+ n))
        (fresh-line)
        (princ "The following rule is Cartesian.")
        (terpri))
      (when (explosivep (car l) th)
        (setq flag t n (1+ n))
        (fresh-line)
        (princ "The following rule is explosive (more than one disjunctive condition).")
        (terpri))
      (when flag
        (terpri)
        (print t (car l))
        (terpri)
        (princ "--------------------------------")
        (terpri))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; A rule is unsafe iff the head contains variables that are not set in  body.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun unsafes (th)
  (remove-if-not #'unsafep (contents th)))

(defun unsafep (rule)
  (cond ((atom rule) nil)
        ((and (eq '<= (car rule))
              (or (atom (cadr rule)) (not (eq 'not (caadr rule)))))
         (not (subsetp (vars (cadr rule)) (goodvars (maksand (cddr rule))))))
        (t nil)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; duplicates
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun duplicates (th)
  (do ((l (contents th) (cdr l)) (nl))
      ((null l) (nreverse nl))
      (when (find (car l) (cdr l) :test #'samep)
        (setq nl (cons (car l) nl)))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; A rule is Cartesian iff it contains more than one antecedent whose args are
;;; all unbound variables.
;;; Cartesian property suggests a possible variable misspelling.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun cartesians (th)
  (remove-if-not #'cartesianp (contents th)))

(defun cartesianp (rule)
  (cond ((atom rule) nil)
        ((eq '<= (car rule))
         (cartp (literals (maksand (cddr rule)))))
        (t nil)))

(defun cartp (literals)
  (do ((l literals (cdr l)) (flag) (vl))
      ((null l) nil)
      (cond ((atom (car l)))
            ((eq 'unprovable (caar l)))
            ((basep (caar l)) (setq vl (nconc (goodvars (car l)) vl)))
            ((fixedp (car l) vl)
             (setq vl (nconc (vars (car l)) vl)))
            (flag (return t))
            (t (setq flag t)
               (setq vl (nconc (vars (car l)) vl))))))

(defun literals (p)
  (cond ((atom p) (list p))
        ((eq 'and (car p)) (cdr p))
        (t (list p))))

(defun fixedp (p vl)
  (do ((l (cdr p) (cdr l)))
      ((null l) nil)
      (if (or (not (varp (car l))) (member (car l) vl)) (return t))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; A rule is empty iff it cannot possibly produce an answer for the head.
;;; At present this determination is based on type checking only.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun empties (th)
  (do ((l (contents th) (cdr l)) (nl))
      ((null l) (nreverse nl))
      (if (emptyp (car l)) (setq nl (cons (car l) nl)))))

(defun emptyp (rule)
  (let (alist)
    (cond ((atom rule) nil)
          ((eq '<= (car rule)) (not (checkp (maksand (cddr rule)) truth))))))

(defun checkp (p al)
  (cond ((atom p))
        ((eq 'not (car p)) al)
        ((eq 'and (car p))
         (do ((l (cdr p) (cdr l)))
             ((null l) al)
             (unless (setq al (checkp (car l) al)) (return nil))))
        ((eq 'or (car p))
         (do ((l (cdr p) (cdr l)))
             ((null l) nil)
             (when (setq al (checkp (car l) al)) (return al))))
        ((eq 'unprovable (car p)) al)
        ((eq 'execute (car p)) al)
        ((eq 'evaluate (car p)) al)
        ((eq 'strmatch (car p)) (checkpstrmatch p al))
        ((get (car p) 'basicval) (checkpbasic p al))
        ((get (car p) 'basic) (checkpbasic p al))
        (t (checkpother p al))))

(defun checkpstrmatch (p al)
  (do ((l (cdr p) (cdr l)))
      ((null l) al)
      (setq al (checkptype (car l) al 'string))
      (when (null al) (return nil))))

(defun checkpbasic (p al)
  (do ((l (cdr p) (cdr l))
       (m (findx '(@l) `(domain ,(car p) @l) 'types) (cdr m)))
      ((null m) al)
      (setq al (checkptype (car l) al (car m)))
      (when (null al) (return nil))))

(defun checkpother (p al)
  (let (arity domain range)
    (setq domain (findx '?x `(domain ,(car p) ?x) *manager*))
    (setq range (findx '?x `(range ,(car p) ?x) *manager*))
    (setq arity (find-arity (car p)))
    (cond ((null (cdr p)) al)
          ((not (setq al (checkptype (cadr p) al domain))) nil)
          ((null (cddr p)) al)
          ((not (setq al (checkptype (caddr p) al range))) nil)
          (arity (if (eql (length (cdr p)) arity) al))
          (t al))))

(defun checkptype (x al type)
  (cond ((varp x) (checkpvar x al type))
        ((null type) al)
        ((member type '(number character string)) (if (typep x type) al))
        ((checkpobj x type) al)))

(defun checkpvar (x al type)
  (let (dum)
    (cond ((setq dum (cdr (assoc x al)))
           (if (or (null type) (eq dum type)) al))
          ((null type) al)
          (t (acons x type al)))))

(defun checkpobj (x type)
  (and (setq x (findx '?x `(isa ,x ?x) *manager*)) (subclassp x type)))

(defun subclassp (x y)
  (cond ((eq x y))
        ((setq x (findx '?x `(superclass ,x ?x) *manager*)) (subclassp x y))))

(defun subrelationp (x y)
  (cond ((eq x y))
        ((setq x (findx '?x `(superrelation ,x ?x) *manager*))
         (subrelationp x y))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; A relation is explosive iff
;;;   (1) it has multiple rules
;;;   (2) it is used in a join
;;; When this happend the joined work gets needlessly duplicated.
;;; Therefore, it is generally a good idea to materialize such relations.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun explosives (th)
  (do ((l (contents th) (cdr l)) (nl))
      ((null l) (nreverse nl))
      (if (explosivep (car l) th) (setq nl (cons (car l) nl)))))

(defun explosivep (rule th)
  (cond ((atom rule) nil)
        ((not (eq '<= (car rule))) nil)
        (t (do ((l (cddr rule) (cdr l)) (flag))
               ((null l) nil)
               (cond ((basep (operator (car l))))
                     ((not (disjunctivedefp (operator (car l)) th)))
                     (flag (return t))
                     (t (setq flag t)))))))

(defun disjunctivedefp (rel th)
  (do ((l (indexees rel th) (cdr l)) (flag))
      ((null l) nil)
      (cond ((atom (car l)))
            ((or (eq rel (caar l))
                 (and (listp (cadar l)) (eq rel (caadar l))))
             (if flag (return t) (setq flag t))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; checkrelation checks arity
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod checkrelation (relation agent)
  (let ((errors 0))
    (dolist (violation (find-arity-violations relation agent))
      (setq errors (1+ errors))
      (fresh-line) (princ (cadr violation)) (princ " -- ") (princ (caddr violation)))
    errors))

(defmethod checkrelation (table (agent symbol))
  (cond ((and (boundp agent) (not (symbolp (symbol-value agent))))
         (checkrelation table (symbol-value agent)))
        (t (call-next-method table agent))))

(defun get-subclasses (class)
  (nreverse (getsubclasses class (list class))))

(defun getsubclasses (class cl)
  (do ((l (find-subclasses class) (cdr l)))
      ((null l) cl)
      (setq cl (getsubclasses (car l) (adjoin (car l) cl)))))

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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
