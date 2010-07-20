;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; (c) Copyright 2004-2007 by Michael Genesereth.  All rights reserved.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Gamemaster code
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(eval-when (compile load eval) (proclaim '(special *trace*)))
#|
(defparameter *log*)
(defparameter *loglock* (make-lock))

(defmethod insert (p (th (eql *repository*)))
  (when *log* (logmessage-with-lock `(pos ,p) *log*))
  (call-next-method p th))

(defmethod uninsert (p (th (eql *repository*)))
  (when *log* (logmessage-with-lock `(neg ,p) *log*))
  (call-next-method p th))
  
(defun logmessage-with-lock (msg fn)
  (with-lock-grabbed (*loglock*) 
    (with-open-file
      (log fn :direction :io :if-exists :append :if-does-not-exist :create)
      (prin1 msg log)
      (terpri log))))

(defun loadlog (fn th)
  (with-open-file (f fn :direction :input)
    (do ((a (read f nil) (read f nil)))
	((null a) 'done)
        (cond ((atom a) (insert a th))
              ((eq (car a) 'pos) (insert (cadr a) th))
              ((eq (car a) 'neg) (drop (cadr a) th))
              (t (insert a th))))))

(defmethod index (x d (th (eql *repository*)))
  (flatindex x d th))
|#


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; match
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass match (theory)
  ((name       :accessor name       :initarg :name       :initform nil)
   (roles      :accessor roles      :initarg :roles      :initform nil)
   (startclock :accessor startclock :initarg :startclock :initform 10)
   (playclock  :accessor playclock  :initarg :playclock  :initform 10)
   (players    :accessor players    :initarg :players    :initform nil)
   (history    :accessor history    :initarg :history    :initform nil)
   (herstory   :accessor herstory   :initarg :herstory   :initform nil)
   (timer      :accessor timer      :initarg :timer      :initform 0)
   (data       :accessor data       :initarg :data       :initform nil)
   (theory     :accessor theory     :initarg :theory     :initform nil)
   (alist      :accessor alist      :initarg :alist      :initform nil)
   (slot       :accessor slot       :initarg :slot       :initform nil)))

(defmethod envindexps (p al (th match))
  (cond ((atom p) (envindexps p al (theory th)))
        ((eq (car p) 'true) (envindexps p al (data th)))
        ((eq (car p) 'does) (envindexps p al (data th)))
        (t (envindexps p al (theory th)))))

(defmethod makematch (name description startclock playclock &rest players)
  (let (matchobj)
    (setq matchobj (make-instance 'match))
    (setf (name matchobj) name)
    (setf (roles matchobj) (items 'role description))
    (setf (startclock matchobj) startclock)
    (setf (playclock matchobj) playclock)
    (setf (players matchobj) players)
    (setf (theory matchobj) description)
    ;(when (triplep 'match.scrambled name 'yes *repository*)
    ;  (setf (alist matchobj) (scramblelist description)))
    (set name matchobj)))

#|
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; playmatch
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod playmatch (match)
  (let (matchobj)
    (when (setq matchobj (openmatch match))
      (startmatch matchobj)
      (do () ((stepmatch matchobj)))
      (stopmatch matchobj)
      (closematch match))))

(defmethod startmatch (matchobj)
  (when *trace* (tracestart (name matchobj)))
  (setf (data matchobj) (initials (theory matchobj)))
  (setf (herstory matchobj) (list (strip (data matchobj))))
  (setf (history matchobj) nil)
  (do ((l (roles matchobj) (cdr l)) (m (players matchobj) (cdr m)))
      ((null l) 'done)
      (start (name matchobj) (car l) (theory matchobj)
             (startclock matchobj) (playclock matchobj) (car m))))

(defmethod stepmatch (matchobj)
  (let (actions)
    (do ((l (players matchobj) (cdr l)) (nl))
        ((null l) (setq actions (nreverse nl)))
        (setq nl (cons (play (car (history matchobj)) (car l)) nl)))
    (when *trace* (tracemoves (roles matchobj) actions))
    (setf (data matchobj)
          (consactions (roles matchobj) actions (data matchobj)))
    (setf (data matchobj) (simulate matchobj))
    (when *trace* (tracestate (data matchobj)))
    (setf (history matchobj) (cons actions (history matchobj)))
    (setf (herstory matchobj) (cons (strip (data matchobj)) (herstory matchobj)))
    (termp matchobj)))

(defmethod stopmatch (matchobj)
  (when *trace* (tracestop (name matchobj)))
  (do ((m (players matchobj) (cdr m)))
      ((null m) 'done)
      (stop (car (history matchobj)) (car m))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; runmatch
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod runmatch (match)
  (let (matchobj)
    (when (setq matchobj (openmatch match))
      (beginmatch matchobj)
      (do () ((tickmatch matchobj)))
      (endmatch matchobj)
      (closematch match))))

(defmethod beginmatch (matchobj)
  (when *trace* (tracestart (name matchobj)))
  (setf (data matchobj) (initials (theory matchobj)))
  (setf (herstory matchobj) (list (strip (data matchobj))))
  (setf (history matchobj) nil)
  (do ((l (roles matchobj) (cdr l)) (m (players matchobj) (cdr m)))
      ((null l) 'done)
      (begin (name matchobj) (car l) (theory matchobj)
             (startclock matchobj) (playclock matchobj) (car m)))
  (setf (timer matchobj) (+ (get-universal-time) (startclock matchobj) 7))
  (do ((end (+ (get-universal-time) (startclock matchobj))))
      ((> (get-universal-time) end) 'done)
      (sleep 1)))

(defmethod tickmatch (matchobj)
  (let (actions)
    (dolist (p (players matchobj)) (commence (car (history matchobj)) p))
    (setf (timer matchobj) (+ (get-universal-time) (playclock matchobj) 7))
    (process-wait "Play" #'(lambda (p) (every #'action p)) (players matchobj))
    (do ((l (players matchobj) (cdr l)) (nl))
        ((null l) (setq actions (nreverse nl)))
        (setq nl (cons (action (car l)) nl))
        (setf (action (car l)) nil))
    (when *trace* (tracemoves (roles matchobj) actions))
    (setf (data matchobj)
          (consactions (roles matchobj) actions (data matchobj)))
    (setf (data matchobj) (simulate matchobj))
    (when *trace* (tracestate (data matchobj)))
    (setf (history matchobj) (cons actions (history matchobj)))
    (setf (herstory matchobj) (cons (strip (data matchobj)) (herstory matchobj)))
    (termp matchobj)))

(defmethod endmatch (matchobj)
  (when *trace* (tracestop (name matchobj)))
  (do ((m (players matchobj) (cdr m)))
      ((null m) 'done)
      (stop (car (history matchobj)) (car m))))

(defmethod commence (actions player)
  (setf (action player) nil)
  (process-run-function "Play" #'play actions player)
  'active)

(defun tracestart (match)
  (fresh-line t)
  (format t "~A: Starting ~A" (get-universal-time) match)
  (terpri t))

(defun tracemoves (roles actions)
  (do ((l roles (cdr l)) (m actions (cdr m)) (time (get-universal-time)))
      ((null l) (terpri) 'done)
      (terpri)
      (format t "~A: ~A Action: ~A" time (car l) (car m))))

(defun tracestop (match)
  (terpri t)
  (format t "~A: Stopping ~A" (get-universal-time) match)
  (terpri t))

(defun tracestate (data)
  (terpri t)
  (dolist (p data) (print p)))

;(defun trace-message (message)
;  (fresh-line t)
;  (format t "~A: ~A" (get-universal-time) message)
;  (terpri t))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; openmatch
;;; closematch
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun openmatch (match)
  (let (game players startclock playclock)
    (setq game (result 'match.game match *repository*))
    (setq players (results 'match.player match *repository*))
    (setq startclock (result 'match.startclock match *repository*))
    (setq playclock (result 'match.playclock match *repository*))
    (setq players (mapcar #'makeplayer players))
    (when (and game (boundp game) (integerp playclock))
      (drop `(match.status ,match ready) *repository*)
      (save `(match.status ,match active) *repository*)
      (apply #'makematch match (symbol-value game) startclock playclock players))))

(defmethod closematch (match)
  (let (object data history herstory reward)
    (setq object (symbol-value match))
    (setq data (data object))
    (do ((l (history object) (cdr l)) (nl))
        ((null l) (setq history (cons 'prog nl)))
        (setq nl (cons (cons 'ply (car l)) nl)))
    (setq herstory (nreverse (herstory object)))
    (do ((l (roles object) (cdr l)) (nl))
        ((null l) (setq reward (cons 'listof (nreverse nl))))
        (setq nl (cons (reward (car l) object) nl)))
    (save `(match.data ,match ,data) *repository*)
    (save `(match.history ,match ,history) *repository*)
    (save `(match.herstory ,match ,herstory) *repository*)
    (save `(match.rewards ,match ,reward) *repository*)
    (drop `(match.status ,match active) *repository*)
    (save `(match.status ,match terminal) *repository*)
    ;(makunbound match)
    reward))

(defun makeplayer (name)
  (cond ((doublep 'local.instance name *repository*) (createlocal name))
        ((doublep 'remote.instance name *repository*) (createforeigner name))
        (t (createhuman name))))

(defun killplayer (name)
  (cond ((doublep 'local.instance name *repository*) (createlocal name))
        ((doublep 'remote.instance name *repository*) (createforeigner name))
        (t (createhuman name))))

(defun createlocal (name)
  (make-instance name))

(defun createforeigner (name)
  (let (host port)
    (setq host (result 'remote.host name *repository*))
    (setq port (result 'remote.port name *repository*))
    (when (and host port) (makeforeigner name host port))))

(defun createhuman (name)
  (makehuman name))
|#
(defun strip (data)
  (do ((l data (cdr l)) (nl))
      ((null l) (nreverse nl))
      (cond ((atom (car l)) (setq nl (cons (car l) nl)))
            ((eq (caar l) 'true) (setq nl (cons (cadar l) nl)))
            ((eq (caar l) 'does))
            (t (setq nl (cons (cadar l) nl))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; database stuff
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun games (player)
  (length (findmatches player)))

(defun total (player)
  (do ((l (findmatches player) (cdr l)) (total 0))
      ((null l) total)
      (setq total (+ total (findreward player (car l))))))

(defun percentage (player)
  (let (games)
    (setq games (games player))
    (cond ((equal games 0) 100)
          (t (values (round (total player) games))))))

(defun findmatches (player)
  (viewfinds '?x `(and (match.player ?x ,player) (match.status ?x terminal))
             *repository*))

(defun findreward (player match)
  (do ((l (findplayers match) (cdr l)) (m (findrewards match) (cdr m)))
      ((null l) 0)
      (when (eq (car l) player) (return (or (car m) 0)))))

(defun findgame (match)
  (result 'match.game match *repository*))

(defun finddefinition (game)
  (cond ((and (symbolp game) (boundp game)) (symbol-value game))
        ((result 'game.definition game *repository*))))

(defun findroles (match)
  (viewfinds '?x `(and (match.game ,match ?g) (game.role ?g ?x)) *repository*))

(defun findplayers (match)
  (results 'match.player match *repository*))

(defun finddata (match)
  (result 'match.data match *repository*))

(defun findstate (match step)
  (let (herstory)
    (setq herstory (result 'match.herstory match *repository*))
    (cond ((and (> step 0) (<= step (length herstory)))
           (elt herstory (1- step)))
          (t (finddata match)))))

(defun findhistory (match)
  (mapcar #'cdr (cdr (result 'match.history match *repository*))))

(defun findherstory (match)
  (result 'match.herstory match *repository*))

(defun computeherstory (match)
  (let (roles state theory herstory)
    (setq theory (finddefinition (findgame match)))
    (setq roles (items 'role theory))
    (setq state (initials theory))
    (setq herstory (list (strip state)))
    (dolist (move (findhistory match))
      (setq state (nconc state theory))
      (setq state (sort (simulate (consactions roles move state)) #'minlessp))
      (setq herstory (cons (strip state) herstory)))
    (nreverse herstory)))

(defun findrewards (match)
  (cdr (result 'match.rewards match *repository*)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Useful subroutines
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defgeneric start (match role description startclock playclock player))

(defgeneric play (actions player))

(defgeneric stop (actions player))

(defgeneric begin (match role description startclock playclock player))

(defgeneric commence (actions player))

(defgeneric end (actions player))

(defun legalp (role action theory)
  (and action (groundp action)  (gdlfindp `(legal ,role ,action) theory)))

(defun legalx (role theory)
  (gdlfindx '?x `(legal ,role ?x) theory))

(defun legals (role theory)
  (gdlfinds '?x `(legal ,role ?x) theory))

(defun others (role roles theory xl nl)
  (cond ((null roles) (cons xl nl))
        ((eq (car roles) role) (others role (cdr roles) theory xl nl))
        (t (dolist (x (legals (car roles) theory))
             (setq nl (others role (cdr roles) theory (cons x xl) nl)))
           nl)))

(defun joints (role action roles theory xl nl)
  (cond ((null roles) (cons (reverse xl) nl))
        ((eq (car roles) role)
         (joints role action (cdr roles) theory (cons action xl) nl))
        (t (dolist (x (legals (car roles) theory))
             (setq nl (joints role action (cdr roles) theory (cons x xl) nl)))
           nl)))

(defun consactions (roles actions data)
  (do ((n roles (cdr n)) (o actions (cdr o)))
      ((null n) data)
      (setq data (cons `(does ,(car n) ,(car o)) data))))

(defun initials (theory)
  (gdlfinds `(true ?p) `(init ?p) theory))

(defun simulate (theory)
  (gdlfinds `(true ?p) `(next ?p) theory))

(defun reward (role theory)
  (or (gdlfindx '?x `(goal ,role ?x) theory) 50))

(defun rewards (roles theory)
  (do ((l roles (cdr l)) (nl))
      ((null l) (nreverse nl))
      (setq nl (cons (reward (car l) theory) nl))))

(defun termp (theory)
  (gdlfindp `terminal theory))

(defun minlessp (x y)
  (cond ((numberp x)
         (cond ((numberp y) (< x y))
               (t t)))
        ((symbolp x)
         (cond ((numberp y) nil)
               ((symbolp y) (string< (symbol-name x) (symbol-name y)))
               (t t)))
        (t (cond ((numberp y) nil)
                 ((symbolp y) nil)
                 (t (do ((l x (cdr l)) (m y (cdr m)))
                        (nil)
                        (cond ((null l) (return (not (null m))))
                              ((null m) (return nil))
                              ((minlessp (car l) (car m)) (return t))
                              ((equal (car l) (car m)))
                              (t (return nil)))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Loading and Dumping
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun loadgame (name fn)
  (set name (read-file-contents fn 'kif))
  'done)

(defun dumpgamemaster ()
  (dumpclass 'language *repository* "gullible:mrg:gallery:language.kif" 'ookif)
  (dumpclass 'status *repository* "gullible:mrg:gallery:status.kif" 'ookif)
  (dumpclass 'game *repository* "gullible:mrg:gallery:game.kif" 'ookif)
  (dumpclass 'match *repository* "gullible:mrg:gallery:match.kif" 'ookif)
  (dumpclass 'local *repository* "gullible:mrg:gallery:local.kif" 'ookif)
  (dumpclass 'remote *repository* "gullible:mrg:gallery:remote.kif" 'ookif)
  (dumpclass 'person *repository* "gullible:mrg:gallery:person.kif" 'ookif)
  'done)

;;; (dumpagent *repository* "gullible:gamemaster:source:repository.kif" 'ookif)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Testing
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; setupplayer, settupmatch, testmatch, timematch
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defparameter *timer* t)

(defun setupplayer (player role description startclock playclock)
  (let (playerobj)
    (setq playerobj (makeplayer player))
    (start 'test role description startclock playclock playerobj)
    playerobj))

(defun setupmatch (game startclock playclock &rest players)
  (let (description)
    (setq description (symbol-value game))
    (setq players (mapcar #'makeplayer players))
    (apply #'makematch 'test description startclock playclock players)
    (symbol-value 'test)))

(defun testmatch (game startclock playclock &rest players)
  (let (description matchobj history (*timer* nil))
    (setq description (symbol-value game))
    (setq players (mapcar #'makeplayer players))
    (apply #'makematch 'test description startclock playclock players)
    (setq matchobj (symbol-value 'test))
    (startmatch matchobj)
    (do () ((stepmatch matchobj)))
    (stopmatch matchobj)
    (do ((l (history matchobj) (cdr l)) (nl))
        ((null l) (setq history (cons 'prog nl)))
        (setq nl (cons (maksply (car l)) nl)))
    (makunbound 'test)
    history))

(defun timematch (game startclock playclock &rest players)
  (let (description matchobj history (*timer* t))
    (setq description (symbol-value game))
    (setq players (mapcar #'makeplayer players))
    (apply #'makematch 'test description startclock playclock players)
    (setq matchobj (symbol-value 'test))
    (beginmatch matchobj)
    (do () ((tickmatch matchobj)))
    (endmatch matchobj)
    (do ((l (history matchobj) (cdr l)) (nl))
        ((null l) (setq history (cons 'prog nl)))
        (setq nl (cons (maksply (car l)) nl)))
    (makunbound 'test)
    history))

(defun maksply (actions)
  (cond ((null actions) nil)
        ((null (cdr actions)) (car actions))
        (t (cons 'ply actions))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
