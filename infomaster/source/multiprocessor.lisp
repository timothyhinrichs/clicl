;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; (c) Copyright 1999-2003 by Michael R. Genesereth.  All rights reserved.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(eval-when (compile load eval)
  (proclaim '(special *metarules*)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Basics
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun globalize (x site)
  (cond ((findp `(nameisglobal ,x no) *manager*)
         (intern (strappend (symbol-name x) "@" site)))
        (t x)))

(defun makremote (agent host port)
  (telladministrator `(isa ,agent aclserver))
  (telladministrator `(host ,agent ,host))
  (telladministrator `(port ,agent ,port))
  'done)

(defun killremote (agent host port)
  (untelladministrator `(isa ,agent aclserver))
  (untelladministrator `(host ,agent ,host))
  (untelladministrator `(port ,agent ,port))
  'done)

(defun makbasicsentences (agent site ip port)
  (let (global sentences)
    (setq global (globalize agent site))
    (setq sentences (cons `(isa ,global aclserver) sentences))
    (setq sentences (cons `(host ,global ,ip) sentences))
    (setq sentences (cons `(port ,global ,port) sentences))
    (dolist (table (find-classifiers agent))
      (setq sentences (cons `(agentisarelation ,global ,table) sentences)))
    (nreverse sentences)))

(defun makallsentences (agents site ip port)
  (let (global sentences)
    (dolist (agent agents)
      (setq global (globalize agent site))
      (setq sentences (cons `(isa ,global aclserver) sentences))
      (setq sentences (cons `(host ,global ,ip) sentences))
      (setq sentences (cons `(port ,global ,port) sentences))
      (dolist (table (find-classifiers agent))
        (setq sentences (cons `(agentisarelation ,global ,table) sentences)))
      (dolist (table (find-specialties agent))
        (setq sentences (cons `(specialty ,global ,table) sentences)))
      (dolist (table (find-interests agent))
        (setq sentences (cons `(interest ,global ,table) sentences)))
      (dolist (class (find-frames agent))
        (setq sentences (cons `(rootclass ,global ,class) sentences)))
      (dolist (table (find-tables agent))
        (setq sentences (cons `(rootrelation ,global ,table) sentences))))
    (nreverse sentences)))

(defun maknotsentences (agents site)
  (let (global sentences)
    (dolist (agent agents)
      (setq global (globalize agent site))
      (do ((l (indexees agent *manager*) (cdr l)))
          ((null l))
          (cond ((atom (car l)))
                ((and (eq (caar l) 'isa) (eq (cadar l) agent))
                 (setq sentences (cons `(not (isa ,global ,(caddar l))) sentences))))))
    (nreverse sentences)))

(defun metadata ()
  (do ((l *sentences* (cdr l)) (contents (contents *manager*)) (nl))
      ((null l) (nreconc nl contents))
      (cond ((equalp (car l) (car contents))
             (setq contents (cdr contents)))
            (t (setq nl (cons `(not ,(car l)) nl))))))

(defun metarules ()
  (do ((l *metarules* (cdr l)) (contents (contents *metalibrary*)) (nl))
      ((null l) (nreconc nl contents))
      (cond ((equalp (car l) (car contents))
             (setq contents (cdr contents)))
            (t (setq nl (cons `(unprovable ,(car l)) nl))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; replicate
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod replicate (site ip port)
  (let (infoservers sentences rules mgr mlb global)
    (setq site (stringupcase site))
    (setq infoservers (finds '?x '(isa ?x infoserver) *manager*))
    (setq infoservers (delete 'manager infoservers))
    (setq infoservers (delete 'metalibrary infoservers))
    (setq sentences (metadata))
    (setq rules (metarules))
    (setq mgr (globalize 'manager site))
    (setq mlb (globalize 'metalibrary site))
    (makremote mgr ip port)
    (request `(restartsystem) *client* mgr)
    (when sentences (request `(definemore ,mgr ',(metadata)) *client* mgr))
    (when rules (request `(definemore ,mlb ',(metarules)) *client* mgr))
    (dolist (agent infoservers)
      (when (setq sentences (contents agent))
        (setq global (globalize agent site))
        (request `(definemore ,global ',sentences) *client* mgr)))
    (killremote mgr ip port)
    'done))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; registerselfwithanother
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod registerselfwithanother (site ip port)
  (let (global sentences adm)
    (setq site (stringupcase site))
    (dolist (agent (find-all-agents))
      (setq global (globalize agent *host*))
      (setq sentences (cons `(isa ,global aclserver) sentences))
      (setq sentences (cons `(host ,global ,*ip*) sentences))
      (setq sentences (cons `(port ,global ,*port*) sentences))
      (dolist (table (find-classifier agent))
        (setq sentences (cons `(agentisarelation ,global ,table) sentences)))
      (dolist (table (find-specialties agent))
        (setq sentences (cons `(specialty ,global ,table) sentences)))
      (dolist (table (find-interests agent))
        (setq sentences (cons `(interest ,global ,table) sentences)))
      (dolist (class (find-frames agent))
        (setq sentences (cons `(rootclass ,global ,class) sentences)))
      (dolist (table (find-tables agent))
        (setq sentences (cons `(rootrelation ,global ,table) sentences))))
    (setq sentences (nreverse sentences))
    (setq adm (globalize 'administrator site))
    (makremote adm ip port)
    (request `(tell ,(maksand sentences)) *client* adm)
    (killremote adm ip port)
    'done))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; registeranotherwithself
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod registeranotherwithself (site ip port)
  (let (sentences mgr)
    (setq site (stringupcase site))
    (setq mgr (globalize 'manager site))
    (makremote mgr ip port)
    (dolist (agent (request `(ask-all ?x (isa ?x agent)) *client* mgr))
      (setq sentences (cons `(isa ,agent aclserver) sentences))
      (setq sentences (cons `(host ,agent ,*host*) sentences))
      (setq sentences (cons `(port ,agent ,*port*) sentences))
      (dolist (table (request `(ask-all ?x (agentisarelation ,agent ?x)) *client* mgr))
        (setq sentences (cons `(agentisarelation ,agent ,table) sentences)))
      (dolist (table (request `(ask-all ?x (specialty ,agent ?x)) *client* mgr))
        (setq sentences (cons `(specialty ,agent ,table) sentences)))
      (dolist (table (request `(ask-all ?x (interest ,agent ?x)) *client* mgr))
        (setq sentences (cons `(interest ,agent ,table) sentences)))
      (dolist (class (request `(ask-all ?x (rootclass ,agent ?x)) *client* mgr))
        (setq sentences (cons `(rootclass ,agent ,class) sentences)))
      (dolist (table (request `(ask-all ?x (rootrelation ,agent ?x)) *client* mgr))
        (setq sentences (cons `(rootrelation ,agent ,table) sentences))))
    (setq sentences (nreverse sentences))
    (killremote mgr ip port)
    (telladministrator (maksand sentences))
    'done))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; register
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod register (site ip port)
  (let (agents locals remotes adm)
    (replicate site ip port)
    (setq site (stringupcase site))
    (setq agents (find-all-agents))
    (setq locals (makallsentences agents site ip port))
    (setq remotes (makallsentences agents *host* *ip* *port*))
    (setq adm (globalize 'administrator site))
    (makremote adm ip port)
    (request `(tell ,(maksand remotes)) *client* adm)
    (request `(tell (recipient ,adm ,(intern (externalize 'manager)))) *client* adm)
    (killremote adm ip port)
    (telladministrator (maksand locals))
    (telladministrator `(recipient administrator ,adm))
    'done))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; reproduce
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod reproduce (site ip port)
  (let (agents dataservers ruleservers otherservers
        sentences unsentences locals remotes rules mgr mlb adm)
    (setq site (stringupcase site))
    (setq agents (find-all-agents))
    (setq dataservers (delete 'manager (find-dataservers)))
    (setq ruleservers (delete 'metalibrary (find-ruleservers)))
    (setq otherservers (difference* agents dataservers))
    (setq sentences (mysublis (globals otherservers site) (metadata)))
    (setq sentences (mysublis (globals dataservers *host*) sentences))
    (setq unsentences (maknotsentences dataservers *host*))
    (setq locals (makallsentences otherservers site ip port))
    (setq locals (nconc locals (makbasicsentences 'warehouse site ip port)))
    (setq remotes (makallsentences agents *host* *ip* *port*))
    (setq rules (metarules))
    (setq mgr (globalize 'manager site))
    (setq mlb (globalize 'metalibrary site))
    (setq adm (globalize 'administrator site))
    (makremote adm ip port)
    (request `(restartsystem) *client* adm)
    (when sentences (request `(tell ,(maksand sentences)) *client* adm))
    (when unsentences (request `(tell ,(maksand unsentences)) *client* adm))
    (when rules (request `(definemore ,mlb ',rules) *client* adm))
    (dolist (agent ruleservers)
      (setq sentences (contents agent))
      (request `(definemore ,(globalize agent site) ',sentences) *client* adm))
    (request `(tell ,(maksand remotes)) *client* adm)
    (request `(tell (recipient ,adm ,(intern (externalize 'manager)))) *client* adm)
    (killremote adm ip port)
    (telladministrator (maksand locals))
    (telladministrator `(recipient administrator ,mgr))
    'done))

(defun find-dataservers ()
  (finds '?x '(or (kind ?x tableserver)
                  (kind ?x fastserver)
                  (kind ?x dataserver)) *manager*))

(defun find-ruleservers ()
  (finds '?x '(or (kind ?x ruleserver) (kind ?x fullserver)) *manager*))

(defun plugall (x al)
  (cond ((atom x) (plugatom x al))
	((eq 'quote (car x)) x)
	(t (do ((l x (cdr l)) (nl))
               ((null l) (nreverse nl))
               (setq nl (cons (plugall (car l) al) nl))))))

(defun plugatom (x al)
  (let (dum)
    (cond ((setq dum (assoc x al :test #'equalp)) (cdr dum))
          (t x))))

(defun mysublis (al x)
  (cond ((atom x) (sublis al x))
        (t (do ((l x (cdr l)) (nl))
               ((null l) (nreverse nl))
               (setq nl (cons (mysublis al (car l)) nl))))))

(defun globals (agents site)
  (do ((l agents (cdr l)) (al))
      ((null l) (nreverse al))
      (setq al (acons (car l) (globalize (car l) site) al))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
