;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; empirical.lisp
;;;     routines for empirically evaluating automated reasoning systems
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;; Graph Coloring ;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun print-graphcoloring-results (graphdesc results &optional (stream t))
  (format stream "(GRAPHCOLORING ~A ~A)~%" graphdesc results))

(defun test-on-graphcoloring (func samplesize minnodes maxnodes incnodes 
			      &optional (timeout 300) (outputfunc #'print-graphcoloring-results))
  "(TEST-ON-GRAPHCOLORING FUNC SAMPLESIZE MINNODES MAXNODES INCNODES)
   FUNC is a function that takes as input a set of ground atoms representing a graph
   coloring problem and a descriptor of the problem instance.  Runs FUNC 
   on a number of graph coloring infuncallstances, where nodes run from MINNODES to
   MAXNODES incremented by INCNODES, the adjacency matrix is generated randomly
   by gen-random-graph, and the hues tested are k, 2/3k, and 1/3k in size
   (see KR2008 for definition of k).  The number of random graphs generated for
   each node/hue -size combination is controlled by SAMPLESIZE. TIMEOUT is the number
   of seconds at which to stop any instance.  Any value besides a
   positive integer makes TIMEOUT unbounded."
  (let (g k hue10k hue06k hue03k edgenum)
    (do ((i minnodes (+ i incnodes)))
	((> i maxnodes))
      (dotimes (j samplesize)
	(setq g (gen-random-graph i))
	(setq k (max-incident-edges g))
	(setq edgenum (- (length g) i))
	(setq hue10k (maptimes #'(lambda () (list 'hue (tosymbol (gentemp "hue")))) (1+ k)))
	(setq hue06k (maptimes #'(lambda () (list 'hue (tosymbol (gentemp "hue")))) (ceiling (* 2 k) 3)))
	(setq hue03k (maptimes #'(lambda () (list 'hue (tosymbol (gentemp "hue")))) (ceiling k 3)))	
	(run-time timeout func (nconc hue10k g))
	(unless (eq *run-time-time* 'timeout) 
	  (funcall outputfunc (list 'instance i edgenum (1+ k) '1.0k) *run-time-result*))
	(run-time timeout func (nconc hue06k g))
	(unless (eq *run-time-time* 'timeout) 
	  (funcall outputfunc (list 'instance i edgenum (ceiling (* 2 k) 3) '0.6k) *run-time-result*))
	(run-time timeout func (nconc hue03k g))
	(unless (eq *run-time-time* 'timeout) 
	  (funcall outputfunc (list 'instance i edgenum (ceiling k 3) '0.3k) *run-time-result*))))))

(defun gen-random-graph (nodecount)
   "(GEN-RANDOM-GRAPH NODECOUNT) takes a positive integer as input
   and returns an undirected graphs defined extensionally as a set of REGIONs,
   and a set of ADJ relationships.  The number of regions in the graph is NODECOUNT."
   (let (region adj)
     (setq region (gen-n-things 'object nodecount))
     (setq adj (gen-random-acyclic-relation 'adj region))
     (nconc region adj)))

(defun max-incident-edges (g)
  (let ((h (make-hash-table)))
    (dolist (v g)
      (when (eq (car v) 'adj)
	(if (gethash (second v) h)
	    (setf (gethash (second v) h) (1+ (gethash (second v) h)))
	    (setf (gethash (second v) h) 1))
	(if (gethash (third v) h)
	    (setf (gethash (third v) h) (1+ (gethash (third v) h)))
	    (setf (gethash (third v) h) 1))))
    (apply #'max (mapcar #'second (hash2list h)))))
 

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;; Blocks World ;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun print-blocksworld-results (desc results &optional (stream t))
  (format stream "(BLOCKSWORLD ~A ~A)~%" desc results))

(defun test-on-blocksworld (func samplesize minblocks maxblocks incblocks 
			      &optional (timeout 300) (outputfunc #'print-blocksworld-results))
  "(TEST-ON-BLOCKSWORLD FUNC SAMPLESIZE MINNODES MAXNODES INCNODES)
   FUNC is a function that takes as input a set of ground atoms representing a graph
   coloring problem and a descriptor of the problem instance.  Runs FUNC 
   on a number of blocks world funcall instances, where the number of blocks runs from MINNODES to
   MAXNODES incremented by INCNODES, the blocks world instance is generated randomly
   by gen-random-blocksworld, and the time steps tested are 3k, 1.5k, and 0.5k, where k
   is the number of blocks.  The number of random blocksworld problems generated for
   each block/time combination is controlled by SAMPLESIZE. TIMEOUT is the number
   of seconds at which to stop any instance.  Any value besides a
   positive integer makes TIMEOUT unbounded."
  (let (init final)
    (do ((i minblocks (+ i incblocks)))
	((> i maxblocks))
      (dotimes (j samplesize)
	(multiple-value-setq (init final) (gen-random-blocksworld i))
	(test-on-blocksworld-time func init final (* 3 i) 'time30k timeout outputfunc)
	(test-on-blocksworld-time func init final (ceiling (* 1.5 i)) 'time15k timeout outputfunc)
	(test-on-blocksworld-time func init final (ceiling (* 0.5 i)) 'time05k timeout outputfunc)))))

(defun test-on-blocksworld-time (func init final timestepnum name 
				 &optional (timeout 300) (outputfunc #'print-blocksworld-results))
  "(TEST-ON-BLOCKSWORLD-TIME FUNC INIT FINAL TIMESTEPNUM NAME TIMEOUT) runs FUNC on the blocksworld
   instance defined by init, final, with TIMESTEPNUM time steps.  NAME is passed along as part of
   the instance description."
  (let (times succ th)
    (setq init (morph-blocksworld init 'init))
    (setq final (morph-blocksworld final 'goal))
    (setq times (gen-n-things 'time timestepnum))
    (setq succ (gen-ordering 'succ times))
    (setq th (append (union init final :test #'equal) times succ))
;    (funcall func th)))
    (run-time timeout func th)
    (funcall outputfunc (list 'instance init final times name) 
	     (if (eq *run-time-time* 'timeout) 'timeout *run-time-result*))))

(defun morph-blocksworld (facts modifier)
  "(MORPH-BLOCKSWORLD FACTS MODIFIER) takes a set of blocksworld facts and adjusts each to either
   the initial state or final state (according to modifier)."
  (mapcar #'(lambda (x) (cond ((atom x) x)
			      ((eq (car x) 'clear) (cons (tosymbol (list 'clear_ modifier)) (cdr x)))
			      ((eq (car x) 'on) (cons (tosymbol (list 'on_ modifier)) (cdr x)))
			      ((eq (car x) 'table) (cons (tosymbol (list 'table_ modifier)) (cdr x)))
			      (t x))) facts))

(defun gen-random-blocksworld (blockcount)
  "(GEN-RANDOM-BLOCKSWORLD BLOCKCOUNT) generates two sets of on(x,y),
   clear(X), and table(x) atoms defining the start and final states
   of a blocks world problem with BLOCKCOUNT blocks.  Note initial state
   is complete but final state is not, e.g. if (on a b) is not included
   in the initial state then (not (on a b)) is true; however, 
   in the final state (on a b) is allowed to be true."
  (let (init final blocks)
    (setq blocks nil)
    (dotimes (i blockcount)
      (push (tosymbol (list 'block i)) blocks))
    (setq blocks (nreverse blocks))
    (setq blocks (mapcar #'(lambda (x) (list 'block x)) blocks))

    ; final state is a random permutation.
    (setq final (nconc (gen-blocksworld-tower (permutation blocks)) blocks))
    ; initial state is another random permutation
    (setq init (nconc (gen-blocksworld-tower (permutation blocks)) blocks))
    (values init final)))

(defun gen-blocksworld-tower (tower)
  "(GEN-BLOCKSWORLD-TOWER TOWER) given a tower of blocks, generate
   logical axioms describing that tower.  First is the top, and last is the bottom."
    (do ((tow tower (cdr tow))
	 (axioms (list `(clear ,(second (first tower))))))
	((null tow) (nreverse axioms))
      (if (null (cdr tow))
	  (push `(table ,(second (first tow))) axioms)
	  (push `(on ,(second (first tow)) ,(second (first (cdr tow)))) axioms))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;; Task Scheduling ;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun print-taskscheduling-results (desc results &optional (stream t))
  (format stream "(TASKSCHEDULING ~A ~A)~%" desc results))

(defun test-on-taskscheduling (func samplesize mintasks maxtasks inctasks 
			      &optional (timeout 300) (outputfunc #'print-taskscheduling-results))
  "(TEST-ON-TASKSCHEDULING FUNC SAMPLESIZE MINNODES MAXNODES INCNODES)
   FUNC is a function that takes as input a set of ground atoms representing a graph
   coloring problem and a descriptor of the problem instance.  Runs FUNC 
   on a number of blocks world funcall instances, where the number of blocks runs from MINNODES to
   MAXNODES incremented by INCNODES, the blocks world instance is generated randomly
   by gen-random-blocksworld, and the time steps tested are 3k, 1.5k, and 0.5k, where k
   is the number of blocks.  The number of random blocksworld problems generated for
   each block/time combination is controlled by SAMPLESIZE. TIMEOUT is the number
   of seconds at which to stop any instance.  Any value besides a
   positive integer makes TIMEOUT unbounded."
  (let (tasks actors actornum)
    (do ((i mintasks (+ i inctasks)))
	((> i maxtasks))
      (setq actornum (max 2 (ceiling i 10)))   ; 10% of the number of tasks
      (dotimes (j samplesize)
	(setq tasks (gen-random-taskscheduling i))
	(setq actors (gen-n-things 'actor actornum))
	; number of tasks (guaranteed satisfiable)
	; minimum number of days is the length of the longest path
	; average tasks/actors 
	(test-on-taskscheduling-time func actors tasks 
				     (max-path (remove-if-not #'cddr tasks)) 'min timeout outputfunc)
	(test-on-taskscheduling-time func actors tasks  (ceiling i actornum) 'avg timeout outputfunc)
	(test-on-taskscheduling-time func actors tasks i 'tasks timeout outputfunc)))))

(defun test-on-taskscheduling-time (func actors tasks daynum name 
				 &optional (timeout 300) (outputfunc #'print-blocksworld-results))
  "(TEST-ON-TASKSCHEDULING-TIME FUNC DATA TIMESTEPNUM NAME TIMEOUT) runs FUNC on the taskscheduling
   instance defined by DATA with DAYNUM time steps.  NAME is passed along as part of
   the instance description."
  (let (days lt th deps tsks)
    ; prepare data
    (setq days (gen-n-things 'day daynum))
    (setq lt (gen-transitive-closure 'lt (gen-ordering 'whatever days)))
    (setq th (append days lt actors tasks))
    ; prepare description
    (setq deps (length (remove-if-not #'cddr tasks)))
    (setq tsks (- (length tasks) deps))
    ; invoke
    ;(funcall func th)))
    (run-time timeout func th)
    (funcall outputfunc (list 'instance tsks deps (length actors) daynum name) 
	     (if (eq *run-time-time* 'timeout) 'timeout *run-time-result*))))

(defun gen-random-taskscheduling (taskcount)
  "(GEN-RANDOM-TASKSCHEDULING TASKCOUNT) generates a set of dependencies
   for TASKCOUNT tasks."
  (setq taskcount (gen-n-things 'task taskcount))
  (nconc taskcount (gen-random-acyclic-relation 'depends taskcount)))

(defun max-path (binreln)
  "(MAX-PATH BINRELN) computes the length of the longest path through
   the binary relation BINRELN."
  (setq binreln (mapcar #'cdr binreln))
  (do ((new binreln)
       (old nil) (i 0))
      ((setequal old new :test #'equal) (values i new))
    (setq old new)
    (setq new (union binreln (project (join binreln new 2 1) 1 3) :test #'equal))
    (setq i (1+ i))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;; General Purpose KIF Generation ;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun gen-n-things (name n)
  "(GEN-N-THINGS NAME N) returns the list ((NAME NAME0) ... (NAME NAMEN-1))."
  (let (things)
    (dotimes (i n (nreverse things))
      (push (list name (tosymbol (list name i))) things))))
      
(defun gen-random-function (domain range &optional (name 'f))
  "(GEN-RANDOM-FUNCTION DOMAIN RANGE NAME) returns a randomly generated, total
   function NAME: DOMAIN -> RANGE.  Returned as a set of atoms of the form (d r)."
  (let ((result nil) rangearray n)
    (setq rangearray (list2array range))
    (setq n (array-dimension rangearray 0))
    (dolist (d domain (nreverse result))
      (push (list name d (aref rangearray (random n))) result))))

(defun gen-random-acyclic-relation (name nodes)
  "(GEN-RANDOM-ACYCLIC-RELATION NAME NODES) takes a relation NAME and a
   list of (type obj) and returns a random list of
   (NAME obj1 obj2) such that there is no loop in the graph."
  (do ((r1 nodes (cdr r1)) (adj nil))
      ((null r1) (nreverse adj))
    (dolist (r2 (cdr r1))
      (when (> (random 2) 0) (push (list name (second (car r1)) (second r2)) adj)))))

(defun gen-ordering (name relation)
  "(GEN-ORDERING NAME RELATION) computes an ordering on the unary RELATION
   named NAME."
  (do ((ts relation (cdr ts))
       (succ nil))
      ((null (cdr ts)) (nreverse succ))
    (push (list name (second (first ts)) (second (second ts))) succ)))

(defun gen-transitive-closure (name relation)
  "(GEN-TRANSITIVE-CLOSURE NAME RELATION) takes a binary RELATION and computes
   the transitive closure using NAME as the relation.  By repeated squaring."
  (do ((new (mapcar #'(lambda (x) (cons name (cdr x))) relation))
       (old nil) (i 0))
      ((setequal old new :test #'equal) (values new i))
    (setq old new)
    (setq new (gen-square new))
    (setq i (1+ i))))

(defun gen-square (relation)
  (let (result)
    (dolist (outer relation (union (uniquify result) relation :test #'equal))
      (dolist (inner relation)
	(when (eq (third outer) (second inner)) 
	  (push (list (first outer) (second outer) (third inner)) result))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;; System Data Manipulation ;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun esodatalog-grounding-testdata2csv (infile outfile &optional (timeout 300))
  "(ESODATALOG-GROUNDING-TESTDATA2CSV INFILE OUTFILE) translates data for 
   esodatalog-grounding routines to CSV for Excel."
  (with-open-file (out outfile :direction :output :if-does-not-exist :create :if-exists :supersed)
    (dolist (r (read-file infile))
      (format out "~A" (first r))
      (dolist (d (cdr (second r)))
	(format out ",~A" d))
      (cond ((eq (third r) 'timeout) (format out ",timeout (~A)~%" timeout))
	    (t
	     (do ((d (cdr (third r)) (cddr d)))
		 ((null (cdr d)))
	       (format out ",~$" (car d))
	       (format out ",~A" (cadr d)))
	     (format out "~%"))))))

; assume format for files is (problem value)
; two files for each system: outputs and times
; assume all of the files and output fit into memory simultaneously

; input: list of (systemname outputfile timefile)
; output if a theory of (problem value time value time .... value time)
;    where the order of value/times is the order of systems in the input.
(defun merge-system-data (in &key (test #'eq))
  "(MERGE-SYSTEM-DATA IN) takes output and time datum for several systems and
   combines it to create a single comparison table."
  (let ((ds nil) probs)
    ; put all the data into memory (indexed by filenames)
    (dolist (s in)
      (push
       (list (first s)
	     (define-theory (make-instance 'prologtheory) "" (read-file (second s)))
	     (define-theory (make-instance 'prologtheory) "" (read-file (third s))))
       ds))
    (setq ds (nreverse ds))

    ; create a list of all problems
    (setq probs (mapunion #'(lambda (x) (mapcar #'first (contents (third x)))) 
			  ds :test test))
    
    ; create final data structure, using TIMEOUT for missing values
    (let (out data tmp)
      (setq out (define-theory 'prologtheory "" nil))
      (dolist (p probs)
	(setq data nil)
	(dolist (d ds)
	  ; value
	  (setq tmp (factfindx '?x `(,p ?x) (second d)))
	  (unless tmp (setq tmp 'timeout))
	  (push tmp data)
	  ; time
	  (setq tmp (factfindx '?x `(,p ?x) (third d)))
	  (unless tmp (setq tmp 'timeout))
	  (push tmp data))
	(save (cons p (nreverse data)) out))
      out)))
		
(defun dump-to-text-delimited (th file &optional (delimiter " "))
  (with-open-file (f file :direction :output :if-does-not-exist :create 
		     :if-exists :supersede)
    (dolist (v (contents th))
      (princ (car v) f)
      (dolist (w (cdr v))
	(princ delimiter f)
	(princ w f))
      (format f "~%"))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;; TPTP ;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; Note: this code has only been ported to the new version of epilog
; in so far as to make the code for filtering through problems work.  
; To run Epilog over tptp, need to upgrade the necessary functions, starting with main.

(defparameter *root-dir* "gullible:users:thinrich:research:projects:tptp:ver3.3.0")
(defparameter *log-format* 'kif "choose either kif or xml")

(defun prepend-root (path) (concatenate 'string *root-dir* ":" (string path)))

(defparameter *datafile* (prepend-root "out.txt"))
(defparameter *completefile* (prepend-root "out.txt.complete"))
(defparameter *restartfile* (prepend-root "tptpfilesrestart.txt"))
(defparameter *problemfile* (prepend-root "tptpfiles.txt"))
(defparameter *testfile* (prepend-root "..:tptpfiles-short.txt"))

; these two added just to quiet compiler warnings
(defparameter *horn* nil)
(defparameter *maxdepth* 10000000)


(defun tptp (&optional (time 60) (problemfile *problemfile*))
  "(TPTP time) run Epilog on all the tptp problems"
  (tptp-main problemfile time))

(defun tptpr (&optional (time 60))
  "(TPTPR time) restart Epilog on all the tptp problems"
  (restart *problemfile* time))

(defun tptpr-manual (&optional (time 60) (problemfile *restartfile*))
  (run problemfile time *datafile* *completefile*))

(defun tlh (&optional (time 30))
  (tptp-main *testfile* time))

(defun tlhr (&optional (time 30))
  (restart *testfile* time))


;;; Hunt through all the problems, looking for those that satisfy a particular criteria

(defstruct probleminfo file status)
(defun get-problems2 (problemfile)
  "(GET-PROBLEMS2 PROBLEMFILE) PROBLEMFILE contains a sequence of lists of the following form.
    (<fullpathtofile> <status> &rest <lotsofstats>).  Returns a list of probleminfo structs.
    For now, just keep the filename and the status around."
  (mapcar #'(lambda (x) (make-probleminfo :file (first x) :status (second x))) (read-file problemfile)))


(defun find-dcas (problemfile)
   "(FIND-DCAS PROBLEMFILE) finds all those fileinfos that point to a tptp file that include a 
    dca, according to optimistic-has-dcap."
   (let ((result nil))
     (dolist (p (get-problems2 problemfile))
       (format t ".")
       (define-theory 'tlhth "" (read-file (probleminfo-file p)))
       (when (optimistic-has-dcap 'tlhth)
         (setq result (cons p result))
         (print p) (format t "~%")
         (get-problems2 problemfile)))
     (nreverse result)))

(defun optimistic-has-dcap (th)
  "(OPTIMISTIC-HAS-DCAP TH) returns T if there is some clause in TH that contains only positive equality literals."
  (some #'sentence-contains-dca (contents th)))

(defun sentence-contains-dca (s)
  (setq s (strip-quantifiers s))
  (if (and (listp s) (eq (car s) 'or) (every #'literalp (cdr s)))
    (clauseset-is-dcap (cdr s))
    nil))
(defun clauseset-is-dcap (c)
  (every #'(lambda (lit) 
             (and (positive-literalp lit) (eq (relation lit) '=)))
         c))

;;; Run Epilog on a large number of problems, one per file

(defun tptp-main (problemfile time-limit &optional (logfile *datafile*) (completefile *completefile*))
  "(MAIN PROBLEMFILE TIME-LIMIT LOGFILE COMPLETEFILE) tests all the problems in PROBLEMFILE, giving each problem
   TIME-LIMIT seconds, logging results in LOGFILE, and storing which problems were completed using COMPLETEFILE, 
   first deleting COMPLETEFILE and LOGFILE.  
   Calls to increase the time limit should be made to run, with the exact same arguments.
   PROBLEMFILE here is a sequence of lines of the form <fullpathname> <status> <equality>."
  (delete-file completefile)
  (delete-file logfile)
  (run problemfile time-limit logfile completefile))

(defun restart (problemfile time-limit &optional (logfile *datafile*) (completefile *completefile*) (restartfile *restartfile*))
  "(RESTART PROBLEMFILE TIME-LIMIT LOGFILE COMPLETEFILE RESTARTFILE) computes the problems left
   in PROBLEMFILE that are not in COMPLETEFILE, outputting that list to RESTARTFILE.
   Runs through those problems and logs the result by appending to LOGFILE and COMPLETEFILE."
  (compute-restart problemfile completefile restartfile)  ; problemfile - completefile = restartfile
  (run restartfile time-limit logfile completefile))

(defun compute-restart (problemfile completefile restartfile)
  "(COMPUTE-RESTART PROBLEMFILE COMPLETEFILE RESTARTFILE) computes the problems not 
   yet complete in PROBLEMFILE by comparing to COMPLETEFILE.  Outputs those files in
   RESTARTFILE."
  (let ((todo (filediff (get-problems problemfile) (read-file completefile))))
    (with-open-file (out restartfile :direction :output :if-exists :supersede :if-does-not-exist :create)
      (dolist (p todo)
        (format out "\"~A\" ~A ~A~%" (get-info-file p) (get-info-status p) (get-info-equality p))))))

(defun run (problemfile time-limit &optional (logfile *datafile*) (completefile *completefile*))
  "(RUN PROBLEMFILE TIME-LIMIT LOGFILE COMPLETEFILE) reads in the list of problems in PROBLEMFILE, runs
   each of those not in COMPLETEFILE for TIME-LIMIT seconds.  Logs to LOGFILE and COMPLETEFILE."
  (let* ((fileinfo1 (get-problems problemfile))
         (fileinfo (filediff fileinfo1 (read-file completefile))))  ; only work on those without solutions
    (format *trace-device* "Trying with time-limit ~D~%" time-limit)
    (run-list fileinfo time-limit logfile completefile)))

(defun run-list (filelist time-limit logfile completefile)
  "(RUN-LIST FILELIST TIME-LIMIT LOGFILE COMPLETEFILE) runs Epilog on the list of files in FILELIST
   with time limit TIME-LIMIT, logging to LOGFILE and COMPLETEFILE.  Assumes each file in FILELIST
   is a fileinfo object."
  (let ((len (length filelist))
        (i 0))
    (with-open-file (outfile logfile :direction :output
                             :if-exists :append
                             :if-does-not-exist :create)
      (with-open-file (complete completefile :direction :io
                               :if-exists :append
                               :if-does-not-exist :create)
        (dolist (file filelist t)
          (setf i (+ i 1))
          (format *trace-device* "~&#~A of ~A (~A): " i len (tptp-id (get-info-file file)))
          (test-file file outfile complete time-limit)
          (format *trace-device* "done~%"))))))

(defun filediff (fileinfolist filelist) 
  "(FILEDIFF fileinfolist  filelist): remove all fileinfo entries where the file is in filelist"
  (let ((result nil))
    (dolist (i fileinfolist (nreverse result))
      (if (not (member (get-info-file i) filelist :test #'equal))
        (setf result (cons i result))))))


; Running Epilog on single file

(defun test-file (fileinfo outfile completefile time-limit)
  "(TEST-FILE FILEINFO OUTFILE COMPLETEFILE TIME-LIMIT) runs Epilog on the file info object
    FILEINFO, for TIME-LIMIT seconds, logging to OUTFILE and COMPLETEFILE.  Assumes the file
    specified contains a bunch of KIF sentences.  Ignores problems with equality."
  (if (not (eq (get-info-equality fileinfo) 0))
    (log-epilog 'none -1 fileinfo "Equality" outfile completefile)
    (let* ((start (get-universal-time))
           (id (process-run-function "test" 'test-epilog fileinfo outfile completefile)))
      (process-wait "finish or time-out" #'finished start time-limit id)
      (process-kill id))))

(defun finished (start secs id)
  "(FINISHED START SECS ID) determines whether process id has finished.
      (1) The process terminated.
      (2) The process has run more than secs seconds from start, 
           unless secs is 0, which indicates no time limit."
  (cond ((not (ccl::process-active-p id)) t)
        ((and (> secs 0) (> (- (get-universal-time) start) secs)) t)
        (t nil)))

; WHEN UPGRADING, SHOULD TURN OFF ATTACHMENTS
; SHOULD COMPUTE ALL CONTRAPOSITIVES OF THE THEORY
(defun test-epilog (fileinfo outfile completefile)
  "(TEST-EPILOG FILEINFO OUTFILE COMPLETEFILE) computes the starting time, reads in the file in FILEINFO, 
    asks epilog if it can prove 'goal' from those sentences.  It then logs the results."

  (let* ((start (get-universal-time))
         (kif (read-file (get-info-file fileinfo) ))
         (th (make-instance 'theory))
         (result) (end)
         (*ancestry* t) (*start* 1) (*increment* 1))
	 ;(*minimum-stack-overflow-size* (* 10 1024 1024)))
    
    (define-theory th "" (brfs (maksand kif)))

    ; tptp translation always gives us a goal: NEED TO TURN ON
    ;(setq result (fullinfop 'goal th :contras t :check-horn t))
    (setq result (fullprovep 'goal th))
    (setq end (get-universal-time))
    
    (log-epilog result (- end start) fileinfo "" outfile completefile)
    (format *trace-device* "logged...")))


;;; Logging

; log the results
(defun log-epilog (result time fileinfo &optional (comment "") (outfile t) (completefile t))
  "(LOG-EPILOG result time fileinfo comment outfile completefile) 
   LOG-EPILOG logs various features to outfile and logs that it did so in completefile."
  (case *log-format*
    (xml (log-epilog-xml result time fileinfo comment outfile completefile))
    (kif (log-epilog-kif result time fileinfo comment outfile completefile))))


(defun log-epilog-xml (result time fileinfo &optional (comment "") (outfile t) (completefile t))
  "(LOG-EPILOG-XML does the logging in XML format."
  (let* ((correct (epilog-correctness result (get-info-status fileinfo))))

    (format outfile "<file name=\"~A\">~%" (get-info-file fileinfo))
    (format outfile "    <result>~A</result>~%" result)
    (format outfile "    <status>~A</status>~%" (get-info-status fileinfo))
    (format outfile "    <seconds>~A</seconds>~%" time)
    (format outfile "    <correctness>~A</correctness>~%" correct)
    (format outfile "    <comment>~A</comment>~%" comment)
    (format outfile "    <inferences>~A</inferences>~%" *inferences*)
    (format outfile "    <unifications>~A</unifications>~%" *unifications*)
    (format outfile "    <ancestry>~A</ancestry>~%" *ancestry*)
    (format outfile "    <id start=\"~A\" increment=\"~A\" depth=\"~A\" limit=\"~A\" />" 
            *start* *increment* *depth* *limit*)
    (format outfile "    <horn>~A</horn>~%" *horn*)
    (format outfile "    <maxdepth>~A</maxdepth>~%" *maxdepth*)
    (format outfile "</file>~%")
    (format completefile "\"~A\"~%" (get-info-file fileinfo))))
    

(defun log-epilog-kif (result time fileinfo &optional (comment "") (outfile t) (completefile t))
  "(LOG-EPILOG-XML does the logging in XML format."
  (let* ((correct (epilog-correctness result (get-info-status fileinfo)))
         (id (tptp-id (get-info-file fileinfo))))

    (format outfile "(tptpproblem.instance ~A)~%" id)
    (format outfile "(tptpproblem.name ~A \"~A\")~%"  id (get-info-file fileinfo))
    (format outfile "(tptpproblem.result ~A ~A)~%" id result)
    (format outfile "(tptpproblem.status ~A ~A)~%" id (get-info-status fileinfo))
    (format outfile "(tptpproblem.time ~A ~A)~%" id time)
    (format outfile "(tptpproblem.correctness ~A ~A)~%" id correct)
    (format outfile "(tptpproblem.comment ~A ~A)~%" id comment)
    (format outfile "(tptpproblem.inferences ~A ~A)~%" id *inferences*)
    (format outfile "(tptpproblem.unifications ~A ~A)~%" id *unifications*)
    (format outfile "(tptpproblem.ancestry ~A ~A)~%" id *ancestry*)
    (format outfile "(tptpproblem.id.start ~A ~A)~%" id *start*)
    (format outfile "(tptpproblem.id.increment ~A ~A)~%" id *increment*)
    (format outfile "(tptpproblem.id.depth ~A ~A)~%" id *depth*)
    (format outfile "(tptpproblem.id.limit ~A ~A)~%" id *limit*)
    (format outfile "(tptpproblem.horn ~A ~A)~%" id *horn*)
    (format outfile "(tptpproblem.maxdepth ~A ~A)~%" id *maxdepth*)
    (format outfile "~%")

    (format completefile "\"~A\"~%" (get-info-file fileinfo))))


(defun tptp-id (path)
  "(TPTP-ID PATH) returns the problem id of the TPTP problem named in PATH,
   which can be a fully qualified path on disk, using : to separate directories."
  (do ( (rpath) (nameindex) (name) (extindex) (id)) ()
    (setq rpath (reverse (string path)))
    (setq nameindex (search ":" rpath))
    (if (not nameindex)
      (setq name rpath)
      (setq name (subseq rpath 0 nameindex)))
    (setq extindex (search "." name))  ; backwards name again
    (when (not extindex) (return (reverse name)))
    (setq id (reverse (subseq name (1+ extindex))))
    (return id)))

(defun tptp-dir-from-id (id) (subseq (string id) 0 3))

(defun epilog-correctness (epilog status)
  "(EPILOG-CORRECTNESS epilog status) determines whether the calculated result
   epilog matches the status of the problem."
  (cond ((eq 'none epilog) 'na)
        ((eq 'tautology status) (eq 't epilog))
        ((eq 'unsatisfiable status) (eq 't epilog))
        ((eq 'satisfiable status) (eq 'nil epilog))
        ((eq 'countersatisfiable status) (eq 'nil epilog))
        ((eq 'theorem status) (eq 't epilog))
        ((eq 'unknown status) 'unknown)
        ((eq 'open status) 'open)
        (t nil)))

;;; Problemfile routines

; Parse the problem file--for now, it is simply a list of files with status and equality fields
(defun get-problems (file)
  (get-problems-list (read-file file)))

; Parse the problem list
(defun get-problems-list (l)
  (let ((result nil))
    (do ((i l (cdddr i)))
        ((null i) (nreverse result))
      (setf result (cons (list (car i) (cadr i) (caddr i)) result)))))

(defun filter-problems (fileinfolist)
  "(FILTER-PROBLEMS fileinfolist) removes problems with equality.
      Assumes only filename and status are needed."
  (let ((result nil))
    (do ((i fileinfolist (cdr i)))
        ((null i) (reverse result))
      (if (eq (get-info-equality (car i)) 0)
        (setf result (cons (list (get-info-file (car i)) (get-info-status (car i))) 
                           result))))))


(defun get-files (listss)
  "(GET-FILES listss) returns a list of the filenames from a list of fileinfo"
  (let ((result nil))
    (do ((i listss (cdr i)))
        ((null i) (reverse result))
      (setf result (cons (get-info-file (car i)) result)))))

(defun get-info-file (fileinfo)
  "(GET-INFO-FILE fileinfo) returns the filename portion of the file-info"
  (car fileinfo))
(defun get-info-status (fileinfo)
  "(GET-INFO-STATUS fileinfo) returns the status portion of the file-info"
  (cadr fileinfo))
(defun get-info-equality (fileinfo)
  "(GET-INFO-EQUALITY fileinfo) returns the equality portion of the file-info"
  (caddr fileinfo))

                
;;; Output Data to Various Formats

(defparameter *epilog-properties* '(name result status time correctness inferences unifications ancestry id.start id.increment id.depth id.limit horn maxdepth))
(defparameter *epilog-properties-all* (cons 'id *epilog-properties*))

(defun tptp-to (th stream headerfunc datumfunc footerfunc)
  "(TPTP-to TH STREAM HEADERFUNC DATUMFUNC FOOTERFUNC) collects all the tptp result data from the
   theory TH, and writes it to STREAM according to the functions HEADERFUNC, DATUMFUNC, and FOOTERFUNC."
  (let ((data (amass-tptp-results th)))
    (funcall headerfunc stream)
    (dolist (d data)
      (funcall datumfunc stream d))
    (funcall footerfunc stream)))

(defun amass-tptp-results (th)
  "(AMASS-TPTP-RESULTS TH) finds all the TPTP results in theory TH and constructs an association 
   list of tptp properties."
  (let ((ids (fullfinds '?x '(tptpproblem.instance ?x) th)))
    (do ((is ids (cdr is))
         (results nil)
         (result))
        ((null is) (nreverse results))
      (setq result (mapcar #'(lambda (x)
                               (cons x 
                                     (fullfindx '?y 
                                                (list (read-from-string (concatenate 'string "tptpproblem." (string x)))
                                                      (car is)
                                                      '?y)
                                                th)))
                           *epilog-properties*))
      (setq result (acons 'id (car is) result))
      (setq results (cons result results)))))

(defun tptp-property (property result)
  "(TPTP-PROPERTY PROPERTY RESULT) finds property PROPERTY in RESULT."
  (cdr (assoc property result)))


;;; General-use 

(defun tptp-to-empty (stream) (declare (ignore stream)))


;;; Comma-separated
(defun tptp-to-csv (th outfile)
  "(TPTP-TO-CSV TH OUTFILE) writes the tptp logging data in TH into file OUTFILE in a comma separated format."
  (tptp-to th outfile #'tptp-to-empty #'tptp-to-csv-datum #'tptp-to-empty))

(defun tptp-to-csv-header (stream)
  "(TPTP-TO-CSV-HEADER STREAM) outputs to STREAM the header for the CSV output of tptp data."
  (mapcar #'(lambda (x) (format stream "~A, " x)) (butlast *epilog-properties-all*))
  (format stream "~A~%" (car (last *epilog-properties-all*))))

(defun tptp-to-csv-datum (stream datum)
  "(TPTP-TO-CSV-DATUM STREAM DATUM) outputs a single tptp benchmark result to stream as comma separated."
  (mapcar #'(lambda (x) (format stream "~A, " (tptp-property x datum)))
          (butlast *epilog-properties-all*))
  (format stream "~A~%" (tptp-property (car (last *epilog-properties-all*)) datum)))

;;; HTML
(defun tptp-to-html (th outfile)
  "(TPTP-TO-HTML TH OUTFILE) writes the tptp logging data in TH into file OUTFILE in HTML."
  (tptp-to th outfile #'tptp-to-html-header #'tptp-to-html-datum #'tptp-to-html-footer))

(defun tptp-to-html-header (stream)
  "(TPTP-TO-HTML-HEADER SRTEAM) outputs to STREAM the header for the HTML output of tptp data."
  (format stream "<table border=\"1\">~%")
  (format stream "<tr>")
  (mapcar #'(lambda (x) (format stream "<td>~:(~A~)</td>" x)) *epilog-properties-all*)
  (format stream "</tr>~%"))

(defun tptp-to-html-footer (stream) (format stream "</table>"))

(defun tptp-to-html-datum (stream datum)
  "(TPTP-TO-HTML-DATUM STEAM DATUM) outputs to STREAM the html output for the tptp datum DATUM."
  (format stream "<tr>")
  (mapcar #'(lambda (x) (format stream "<td>~A</td>" (tptp-property x datum))) *epilog-properties-all*)
  (format stream "</tr>~%"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
