;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; lisp-enhance.lisp
;;    routines adding/augmenting Lisp data structures and functions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar *globaltmp* nil "global temporary variable used in various routines")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;; Symbols ;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun tosymbol (p)
  "(TOSYMBOL LIST) takes a list and returns a symbol made out of that list.  
   The list elements must be coercable to strings via string."
  (cond ((stringp p) (tosymbol (read-sentences p)))
	((atom p) (read-user-string (tostring p)))
        (t (read-user-string (tostring (mapcar #'tosymbol p))))))

(defun tostring (p)
  "(TOSTRING LIST) takes an object and returns the string made out of that
   object by coercing via format and concatenating, if necessary."
  (cond ((stringp p) p)
        ((atom p) (format nil "~A" p))
        (t
         (reduce #'(lambda (x y)
                     (concatenate 'string x (tostring y)))
                 p
                 :initial-value ""))))

(defun tostructuredsymbol (p)
  "(TOSTRUCTUREDSYMBOL P) takes an atom or a list and converts it into a 
   symbol that preserves the structure of p."
  (cond ((symbolp p) p)
        ((atom p) (read-from-string (tostring p)))
        (t
         (read-from-string (tostructuredstring p)))))

(defun tostructuredstring (p)
  "(TOSTRUCTUREDSTRING P) takes an atom or a list and converts it into a string,
   where the structure of P, if it exists, is maintained.  
   Turns (a (b c) d) into *a_*b_c*_d*."
  (cond ((stringp p) p)
        ((atom p) (format nil "~A" p))
        (t
         (let ((contents 
		(reduce #'(lambda (x y) 
			    (concatenate 'string x "_" (tostructuredstring y)))
			(cdr p)
			:initial-value 
			(concatenate 'string 
				     "*" 
				     (tostructuredstring (car p)) ))))
           (concatenate 'string contents "*")))))

(defun tospacedstring (p)
  (cond ((stringp p) p)
        ((atom p) (format nil "~A" p))
        (t
         (reduce #'(lambda (x y) 
		     (concatenate 'string x " " (tospacedstring y))) (cdr p)
                 :initial-value (tospacedstring (car p))))))

(defun tolist (s)
  (cond ((atom s) (list s))
	((list s) s)
	(t nil)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;; Lists ;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun indexof (item list &key (test #'eq))
  (do ((l list (cdr l))
       (i 0 (1+ i)))
      ((or (null l) (funcall test item (car l)))
       (if (null l) nil i))))

(defun cross-product (set power)
  "(CROSS-PRODUCT SET POWER) builds all tuples of size POWER out of SET."
  (cond ((= power 1) (mapcar #'list set))
        (t (mapcan #'(lambda (sofar) (mapcar #'(lambda (elem) (cons elem sofar)) set))
                   (cross-product set (1- power))))))

(defun subsets (s)
  "(SUBSETS S) returns the set of all subsets of S."
  (declare (notinline subsets))
  (cond ((null s) (list nil))
        (t
         (let ((sm1 (subsets (cdr s))))
           (nconc (mapcar #'(lambda (x) (cons (car s) x)) sm1) sm1)))))

(defun permutation (l)
  "(PERMUTATION S) returns a random permutation of the list L."
  (let ((l (copy-list l)) (newl nil) n val)
    (setq n (length l))
    (dotimes (i n (nreverse newl))
      (multiple-value-setq (val l) (slice-n (random (- n i)) l))
      (push val newl))))

(defun slice-n (index list)
  "(SLICE-N INDEX LIST) returns the INDEXth value in list LIST and removes that value from LIST destructively."
  (cond ((= index 0) (values (car list) (cdr list)))
	(t
	 (do ((myl (cdr list) (cdr myl))
	      (prev list (cdr prev))
	      (j 1 (1+ j)) (val))
	     ((or (null myl) (> j index)))
	   (when (= j index) 
	     (setq val (car myl))
	     (setf (cdr prev) (cdr myl))
	     (return (values val list)))))))


(defun nonempty-subsets (s)
  "(NONEMPTY-SUBSETS S) returns the set of all nonempty subsets of S."
  (delete-if #'not (subsets s)))

(defun all-same (f list &key (test #'eq) (skip nil))
  "(ALL-SAME F LIST TEST SKIP) checks whether the result of applying f to each item in LIST
   gives the same result according to TEST, except for the values in the list SKIP."
  (cond ((null list) t)
        (t
         (let ((sofar (funcall f (car list))))
           (do ((ls (cdr list) (cdr ls))
                (tmp))
               ((null ls) sofar)
             (setq tmp (funcall f (car ls)))
             (when (and (not (funcall test tmp sofar)) (not (member tmp skip)))
               (return nil)))))))

(defun allbutone (func list)
  "(ALLBUTONE FUNC LIST) returns T iff at most one of the items in LIST 
   does not satisfy FUNC."
  (do ((ls list (cdr ls))
       (alltrue t) (res))
      ((null ls) t)
    (setq res (funcall func (car ls)))
    (when (and (not alltrue) (not res)) (return nil))
    (when (not res) (setq alltrue nil))))

(defun split (test list)
  "(SPLIT-ON TEST LIST) returns two lists using values.  The first is the
    list of items where test returns t.  The second is the rest of the list."
  (do ((ls list (cdr ls))
       (pos nil)
       (neg nil))
      ((null ls) (values (nreverse pos) (nreverse neg)))
    (if (funcall test (car ls))
      (setq pos (cons (car ls) pos))
      (setq neg (cons (car ls) neg)))))

(defun group-by (list func &key (test #'eq))
  (let (h v res)
    ; group list elements by function
    (setq h (make-hash-table :test test))
    (dolist (l list)
      (setq v (funcall func l))
      (setf (gethash v h) (cons l (gethash v h))))
    ; put into an association list
    (setq res nil)
    (with-hash-table-iterator (it h)
      (loop
	   (multiple-value-bind (entry-p key value) (it)
	     (if entry-p 
		 (push (cons key value) res)
		 (return res)))))))

(defun filter (test list)
  (mapcan #'(lambda (x) (if (funcall test x) (list x) nil)) list))

(defun substitute-rec (newitem olditem x &key (test #'eq))
   "(SUBSTITUTE-REC NEWITEM OLDITEM X) returns a copy of X
   with NEWITEM REPLACING OLDITEM."
   (subst newitem olditem x :test test))

(defun signifier (p) (if (atom p) p (car p)))

(defun seteq (x y &optional (test #'eq))
  (and (subsetp x y :test test) (subsetp y x :test test)))

(defun member* (x list) (if (eq list '*) t (if (atom list) (member x (list list)) (member x list))))
 
(defun maptree (function list &key (test #'atom))
  (cond ((funcall test list) (funcall function list))
        ((atom list) list)
        (t (mapcar #'(lambda (x) (maptree function x :test test)) list))))

(defun mapunion (function list &key (test #'eq))
  "(MAPUNION FUNCTION LIST EQUAL) applies function to each element of list,
   and unions all the results usisimilarize-ng EQUAL as the test."
  (do ((l list (cdr l))
       (result nil))
      ((null l) result)
    (setq result (union (funcall function (car l)) result :test test))))

(defun mapadjoin (function list &key (test #'eq))
  "(MAPADJOIN FUNCTION LIST TEST) applies FUNCTION to each element of list,
   and adjoins the result of each using TEST as the test."
  (do ((l list (cdr l))
       (result nil))
      ((null l) result)
    (setq result (adjoin (funcall function (car l)) result :test test))))

(defun maptimes (function times)
  (let ((result nil))
    (dotimes (i times) (setq result (cons (funcall function) result)))
    result))

(defun mapcarnot (function &rest lists)
  "(MAPCARNOT FUNCTION LIST) mapcars FUNCTION onto LIST but then removes
   all the NILs in the result."
  (remove-if #'not (apply #'mapcar function lists)))

(defun copy-prefix (list until)
  "(COPY-PREFIX LIST UNTIL) returns a copy of LIST up til the cdr equals UNTIL."
  (do ((res nil)
       (l list (cdr l)))
      ((null l) (nreverse res))
    (cond ((eq l until) (return (nreverse res)))
	  (t
	   (push (car l) res)))))

(defun firstn (n l) 
  (do ((ls l (cdr ls))
       (tmp nil)
       (i 0 (1+ i)))
      ((or (= i n) (null ls)) (nreverse tmp))
    (setq tmp (cons (car ls) tmp))))

(defun list2array (l) (make-array (length l) :initial-contents l))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;; Hashtable ;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun hash2list (hash)
  (if (not (hash-table-p hash)) 
      hash
      (let ((res nil))
	(with-hash-table-iterator (my-iterator hash)
	  (loop
	     (multiple-value-bind (entry-p key value)
		 (my-iterator)
	       (if entry-p
		   (push (list key value) res)
		   (return)))))
	res)))

(defun hash2bl (hash)
  (if (not (hash-table-p hash)) 
      hash
      (let ((res nil))
	(with-hash-table-iterator (my-iterator hash)
	  (loop
	     (multiple-value-bind (entry-p key value)
		 (my-iterator)
	       (if entry-p
		   (push (cons key value) res)
		   (return)))))
	res)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;; Arrays ;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun array2list (a)
  "(ARRAY2LIST A) turns a one-dimension array A into a list."
  (let ((res nil))
    (dotimes (i (array-dimension a 0))
      (setq res (cons (aref a i) res)))
    (nreverse res)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;; Strings ;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun drop-quotes (string) (subseq string 1 (1- (length string))))

(defun split-on (string items)
  (setq items (mapcar #'(lambda (x) (cons x (length x))) items))
  (nreverse
   (let ((list nil) (start 0) (end t))
     (do () 
         ((not end) (push (subseq string start nil) list))
       (setq end nil)
       (setq end (mapcar #'(lambda (x) (cons (search (car x) string :start2 start) (cdr x))) items))
       ; find minimum position
       (setq end (first (sort end 
                              #'(lambda (x y) (cond ((not x) (if y nil t))
                                                    ((not y) t)
                                                    (t (< x y))))
                              :key #'car)))
       (when (car end)
         (push (subseq string start (car end)) list)
         (setq start (+ (cdr end) (car end))))
       (setq end (car end))))))

(defun split-string (string &optional (ws '(#\Space #\Tab)) max)
  "Split `string' along whitespace as defined by the sequence `ws'.
Whitespace which causes a split is elided from the result.  The whole
string will be split, unless `max' is provided, in which case the
string will be split into `max' tokens at most, the last one
containing the whole rest of the given `string', if any."
  (flet ((is-ws (char) (find char ws)))
    (nreverse
     (let ((list nil) (start 0) (words 0) end)
       (loop
        (when (and max (>= words (1- max)))
          (return (cons (subseq string start) list)))
        (setf end (position-if #'is-ws string :start start))
        (push (subseq string start end) list)
        (incf words)
        (unless end (return list))
        (setf start (1+ end)))))))

;;; this and that may have different behaviour on strings with
;;; repeated whitespace -- e.g. "foo  bar"

(defun split-quoted (str &optional max (ws '(#\Space #\Tab)))
  "Split `string' along whitespace as defined by the sequence `ws',
but ignoring whitespace in quoted strings.  Whitespace which causes a
split is elided from the result.  The whole string will be split,
unless `max' is a non-negative integer, in which case the string will
be split into `max' tokens at most, the last one containing the whole
rest of the given `string', if any."
  (do ((i 0 (1+ i))
       (words '())
       (split-allowed-p t)
       (word '()))
      ((>= i (length str))
       (reverse (cons (coerce (reverse word) 'string) words)))
    (if (eql (elt str i) #\")
        (setf split-allowed-p (not split-allowed-p)))
    (if (eql (elt str i) #\\)
        (setf i (1+ i)))                ;advance past escape chars
    (if (and split-allowed-p
             (or (not max) (< (length words) (1- max)))
             (member (elt str i) ws))
        (progn
          (setf words (cons (coerce (reverse word) 'string) words))
          (setf word '()))
      (setf word (cons (elt str i) word)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;; Input/Output ;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;; Disk ;;;;;;;;;;;;;;;;

; Read a lisp file's contents and return list
(defun read-file (filename)
  (with-open-file (ifile (string filename) 
			 :direction :input :if-does-not-exist :error)
    (do ((result nil (cons next result))
         (next (read ifile nil 'eof)  (read ifile nil 'eof)))
        ((equal next 'eof) (nreverse result)))))

(defun dump-file (string filename)
  (with-open-file (ifile (string filename)
			 :direction :output 
			 :if-does-not-exist :create
			 :if-exists :supersede)
    (princ string ifile)))

(defun read-any-file (filename)
  "(READ-ANY-FILE FILENAME) reads the contents of filename
   as a string--not necessarily a Lisp file."
  (if (not filename) nil
  (let ((result (make-array 0 :element-type 'character 
			    :adjustable t 
			    :fill-pointer 0)))
    (with-open-file (ifile filename :direction :input)
      (do ((next (read-char ifile nil 'eof) (read-char ifile nil 'eof)))
          ((eq next 'eof) result)
        (format result "~A" next))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;; Time ;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar *timesofar* 0 
  "global variable for accummulating the time taken so far")

(defmacro add-time (form)
  "(ADD-TIME FORM) increments *timesofar* by the result of cpu-time 
   called on FORM."
  `(setq *timesofar* (+ *timesofar* (internal-time #'(lambda () (progn ,form))))))

(defun internal-time (f)
  "(INTERNAL-TIME F) runs function F and returns two values: the CPU time required 
   for that function in internal-units-per-second and the result of F."
  (let* ((initial-real-time (get-internal-real-time))
         (initial-run-time (get-internal-run-time))
         (initial-gc-time (gctime))
	 (result))
    (setq result (multiple-value-list (funcall f)))
    (when (null (cdr result)) (setq result (first result)))
    (let* ((elapsed-real-time (- (get-internal-real-time) initial-real-time))
           (elapsed-run-time (- (get-internal-run-time) initial-run-time))
           (elapsed-gc-time (- (gctime) initial-gc-time))
           (elapsed-mf-time (- elapsed-real-time elapsed-run-time)))
      (values (- elapsed-real-time elapsed-mf-time elapsed-gc-time) result))))

(defun cpu-time (f) 
  "(CPU-TIME F) returns the time to execute F at millisecond-ish granularity
  and F's result."
  (let (m result)
    (multiple-value-setq (m result) (internal-time f))
    (values (* 1.0 (/ m internal-time-units-per-second)) result)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;; Processes ;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar *kill* nil "kill the process at hand")

(defvar *run-time-result*)
(defvar *run-time-time*)
(defun run-time (time func &rest args)
  (setq *run-time-result* 'timeout)
  (setq *run-time-time* nil)
  (cond ((and (integerp time) (>= time 0))
  ; initialize answer in case function times out  
	 (let* ((start (get-universal-time))
		(id (process-run-function 
		     "run-time" 
		     #'(lambda () (multiple-value-setq (*run-time-time* *run-time-result*)
				    (cpu-time #'(lambda () (apply func args))))))))
	   (process-wait "finish or time-out" #'process-finished start time id)
	   (process-kill id)))
	(t
	 (multiple-value-setq (*run-time-time* *run-time-result*)
	   (cpu-time #'(lambda () (apply func args))))))
  (values *run-time-result* *run-time-time*))

; Determine whether a process has finished or the allowed time has expired
(defun process-finished (start secs id)
  "(FINISHED START SECS ID) determines whether process id has finished.
      (1) The process terminated.
      (2) The process has run more than secs seconds from start, 
           unless secs is 0, which indicates no time limit."
  (cond (*kill* t)
        ((not (ccl::process-active-p id)) t)
        ((and (> secs 0) (> (- (get-universal-time) start) secs)) t)
        (t nil)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;; Lisp Code ;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun assemble (infiles &optional 
		 (tgtdir "")
		 (tarfile "/Users/thinrich/Desktop/code.tar"))
  (let (l tmpdir)
    ; make tmp space
    (setq tmpdir "/tmp/tlh/code/")
    (run-program "rm" (list "-Rf" tmpdir))
    (run-program "mkdir" (list "-p" tmpdir))
    ; assemble file list
    (dolist (infile infiles)
      (dolist (form (read-file infile))
	(when (and (listp form) (eq (car form) 'load))
	  (push (mak-lisp-file (second form)) l))))
    (setq l (nreverse l))
    ; copy files to tmp space (can't execute arbitrarily long UNIX tar command)
    (dolist (f l)
      (if (probe-file f)
	  (run-program "cp" (list f tmpdir))
	  (format t "File not found: ~A~%" f)))
    ; create new loader file
    (with-open-file (f (tostring (list tmpdir "loader.lisp")) 
		       :direction :output :if-does-not-exist :create :if-exists :supersede)
      (dolist (v l)
	(setq v (car (last (split-string v '(#\/)))))
	(format f "(load \"~A~A\")~%" tgtdir v)))
    ; tar files but cd to directory first
    (run-program "/Users/thinrich/Research/scripts/tardir" (list tarfile tmpdir))
    tarfile))

(defun mak-lisp-file (f)
  (let (path file)
    (setq path (split-string f '(#\/)))
    (setq file (car (last path)))
    (setq path (butlast path))
    (setq file (split-string file '(#\.)))
    (when (not (cdr file)) (setq file (nconc file (list "lisp"))))
    (setq file (cons (car file)
		     (mapcan #'(lambda (x) (list "." x)) (cdr file))))
    (setq path (nconc path (list (tostring file))))
    (tostring (cons (car path) (mapcan #'(lambda (x) (list "/" x)) (cdr path))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;; Binary decision tree ;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Information theoretic construction, via entropy
; interpreted version, where data consists of positive instances
;   negative instances are all of the others.
; The number of total instances at node a1...an in the tree when
;   there are m attributes is 2^(m-n).

(defstruct binary-decisionnode 
  (value nil)   ; if a child node, the value for this branch
  (split nil)   ; if has children, the attribute on which this node splits
  (pos nil)
  (neg nil))

(defun entropy (data depth numattributes)
  "(ENTROPY DATA DEPTH) give us the entropy for the data set DATA
   at depth DEPTH in the tree when there are a total of NUMATTRIBUTES."
  (entropy-math (length data) depth numattributes))

(defun entropy-math (poscount depth numattributes)
  (let (total posp negp)
    (setq total (expt 2 (- numattributes depth)))
    (setq posp (/ poscount total))
    (setq negp (/ (- total poscount) total))
    (* -1 (+ (if (= posp 0) 0 (* posp (log posp 2))) 
	     (if (= negp 0) 0 (* negp (log negp 2)))))))

(defun weightedentropy (poscount negcount depth numattributes)
  "(WEIGHTEDENTROPY SPLIT TOTAL DEPTH NUMATTRIBUTES)"
  (let (tot)
    (setq tot (+ poscount negcount))
    (+ (* (/ poscount tot) (entropy-math poscount depth numattributes))
       (* (/ negcount tot) (entropy-math negcount depth numattributes)))))

(defun decision-tree-split (data attributes key depth numattributes)
  (let (splits wentropy pos neg)
    ; compute all possible splits and their weightedentropies
    (setq splits nil)
    (dolist (a attributes)
      (multiple-value-setq (pos neg) 
	(split #'(lambda (x) (funcall key x a)) data))
      (setq wentropy (weightedentropy (length pos) (length neg) depth numattributes))
      (push (list wentropy a pos neg) splits))
(dolist (v splits) (pprint v))
    ; the attribute to split on is the one with the lowest weighted entropy
    (setq splits (cdr (find (apply #'min (mapcar #'first splits)) splits :key #'first)))
    (values (first splits) (second splits) (third splits))))


(defun build-binary-decision-tree (data attributes key &optional (depth 0))
  "(BUILD-BINARY-DECISION-TREE DATA ATTRIBUTES KEY) takes data, 
   a list of attributes, and a function that
   given a data element and an attribute returns T or NIL.
   Returns a decision-tree for interpretation, where the attribute
   order is chosen based on information game."
  (declare (notinline build-binary-decision-tree))
  (cond ((null attributes) data)
	(t
	 (let (att pos neg root numattributes)
	   (setq numattributes (length attributes))
	   
	   ; find the first attribute to split on
	   (multiple-value-setq (att pos neg) 
	     (decision-tree-split data attributes key depth numattributes))
	   ; build decision node with that attribute and
	   ;  recurse on each attribute value
	   (setq root (make-binary-decisionnode :split att))
	   (setq attributes (remove att attributes))
	   (setf (binary-decisionnode-pos root) 
		 (build-binary-decision-tree pos attributes key (1+ depth)))
	   (setf (binary-decisionnode-neg root) 
		 (build-binary-decision-tree neg attributes key (1+ depth)))
	   root))))


; uses eq to compare attribute values
(defun multi-split (f data)
  (let ((h (make-hash-table)) (vals nil) (res nil) (val))
    (dolist (d data)
      (setq val (funcall f d))
      (setq vals (adjoin val vals))
      (setf (gethash val h) (cons d (gethash val h))))
    (dolist (v vals res)
     (push (cons v (gethash v h)) res))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;; Graph ;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar *graph-debug* nil)

; Same as above, but with smaller memory footprint.  Does not preserve the order nodes/edges were inserted.
;   Includes labels on edges. 

(defstruct agraph (adjacency-list nil))
(defstruct agraph-node name (slot nil))  ; e.g. use slot for DFS color
(defstruct agraph-edge node (label nil) (slot nil))  ; may want to slot edges too

(defun agraph-insert-noded (name graph)
  "(AGRAPH-INSERT-NODED NAME GRAPH) inserts a new node named NAME into the 
   agraph GRAPH. Returns object inserted.  Destructive."
  (let ((new (list (make-agraph-node :name name))))
    (setf (agraph-adjacency-list graph) (cons new (agraph-adjacency-list graph)))
    new))

(defun agraph-find-node (name graph &key (test #'eq))
  "(AGRAPH-FIND-NODE NAME GRAPH) finds the node named NAME in GRAPH and returns
    its adjacency list."
  (find name 
        (agraph-adjacency-list graph)
        :key #'(lambda (x) (agraph-node-name (car x)))
        :test test))

(defun agraph-update-edge (from to old new graph &key (nodetest #'eq) (labeltest #'eq))
  "(AGRAPH-UPDATE-EDGE FROM TO OLD NEW GRAPH TEST) Destructively changes the label on the edge from
   FROM to TO labeled OLD to NEW using NODETEST to test equivalence of nodes and LABELTEST to
   test the equivalence of labels."
  (let ((e (find old (agraph-find-edges from to graph :test nodetest) :test labeltest :key #'agraph-edge-label)))
    (when e 
      (setf (agraph-edge-label e) new))
    e))

(defun agraph-find-edges (from to graph &key (test #'eq))
  "(AGRAPH-FIND-EDGES FROM TO GRAPH TEST) returns a list of edges from FROM to TO in
   the graph GRAPH.  There may be more than one edge since edges have labels."
  (remove-if-not #'(lambda (x) (funcall test to (agraph-node-name (agraph-source-node (agraph-edge-node x))))) 
                 (agraph-dest-nodes (agraph-find-node from graph :test test))))

(defun agraph-insert-edged (from to label graph)
  "(AGRAPH-INSERT-EDGED FROM TO LABEL GRAPH) adds a new edge into the graph labeled LABEL.
    Destructive. Returns FROM adjacency list.  Adds nodes if not found"
  (let ((nodelist (agraph-adjoin-noded from graph)) 
        (target (agraph-adjoin-noded to graph)))

    (setf (cdr nodelist) (cons (make-agraph-edge :node target :label label) 
                               (cdr nodelist)))
    nodelist))

(defun agraph-adjoin-noded (name graph &key (test #'eq))
  "(AGRAPH-ADJOIN-NODED NAME GRAPH) inserts a new node named NAME into the 
   adjacency list. Treats assoc of nodes as a set. Returns adjlist Destructive"
  (let ((new (list (make-agraph-node :name name))))
    (if (null (agraph-adjacency-list graph))
      (car (setf (agraph-adjacency-list graph) (list new)))
      (adjoind2 new (agraph-adjacency-list graph)
                :key #'(lambda (x) (agraph-node-name (car x)))
                :test test))))

(defun agraph-adjoin-edged (from to label graph &key (test #'eq))
  "(AGRAPH-ADJOIN-EDGED FROM TO ADJLIST) adds a new edge into the adjacency list.
    Treats edges as a set.  Returns FROM adjacency list.  Destructive. "
  (let* ((nodelist (agraph-adjoin-noded from graph :test test))
         (target (agraph-adjoin-noded to graph :test test))
         (labeledtarget (make-agraph-edge :node target :label label)))
    (if (null (cdr nodelist))
      (car (setf (cdr nodelist) (list labeledtarget)))
      (adjoind2 labeledtarget 
                (cdr nodelist) 
                :test #'(lambda (x y) (and (funcall test (agraph-node-name (car (agraph-edge-node x))) (agraph-node-name (car (agraph-edge-node y))))
                                          (funcall test (agraph-edge-label x) (agraph-edge-label y))))))
    nodelist))

(defun agraph-source-node (node-adjlist) (car node-adjlist))
(defun agraph-dest-nodes (node-adjlist) (cdr node-adjlist))

(defun agraph-print (graph)
  "(AGRAPH-PRINT GRAPH) prints GRAPH to the screen. Returns T."
  (format t "~&")
  (dolist (nl (agraph-adjacency-list graph))
    (format t "~A:" (agraph-node-name (agraph-source-node nl)))
    (dolist (n (agraph-dest-nodes nl))
      (format t " (~A)~A" (agraph-edge-label n) (agraph-node-name (agraph-source-node (agraph-edge-node n)))))
    (format t "~%"))
  t)

(defun adjoind2 (item list &key (test #'eq) (key #'identity))
  "(ADJOIND ITEM LIST) acts just like adjoin, but is destructive and returns
   the element in the result of adjoining LIST that is equal according to TEST and KEY
   to ITEM."
  (assert item nil "ADJOIND can only be called on a non-empty list.")
  (do ((ls list (cdr ls))
       (last nil)
       (itemkey (funcall key item)))
      ((null ls) (car (setf (cdr last) (list item))))
    (when (funcall test itemkey (funcall key (car ls)))
      (return (car ls)))
    (setq last ls)))

(defun reset-node-slots (alist)
  "(RESET-NODE-SLOTS ALIST) destructively sets all the slots of the agraph-node objects in
   ALIST to NIL."
  (dolist (n alist)
    (setf (agraph-node-slot n) nil)))

(defun agraph-connected-components (g &key (test #'eq))
  "(CONNECTED-COMPONENTS G) returns a list of the components of graph G.  Each component
   is represented as a list of the names of nodes in G.  Assumes undirected graph, i.e.
   that there is an edge u->v iff there is an edge u<-v."
  (let (comps comp n sofar)
    (setq comps nil sofar nil)
    (dolist (adj (agraph-adjacency-list g))
      (setq n (agraph-node-name (car adj)))
      (unless (member n sofar :test test)
        (setq comp (agraph-find-connected n g :test test))
        (setq comps (cons comp comps))
        (setq sofar (append comp sofar))))
    comps))

(defun agraph-find-connected (name graph &key (test #'eq))
  "(AGRAPH-CONNECTED NAME GRAPH) finds all the names of nodes reachable from the source node
   named NAME in the graph GRAPH.  Ignores multiple edges."
  (let ((node (agraph-find-node name graph :test test)) (result)
        (alist nil))
    (cond ((not node) nil)
          (t
           (setq result (agraph-find-connected-mark node graph))  ; get answer
           ; reset slots
           (reset-node-slots alist)
           result))))

(defun agraph-find-connected-mark (adjlist graph)
  "(AGRAPH-CONNECTED-MARK NODE GRAPH) finds all the nodes reachable from the source node
   of ADJLIST in GRAPH.  Marks visited nodes as it goes
   and stores pointers to those nodes in alist.  Returns a list of node names."

  (cond ((agraph-node-slot (agraph-source-node adjlist)) nil)
        (t
         (setf (agraph-node-slot (agraph-source-node adjlist)) t)   ; mark the node
         (setf alist (cons (agraph-source-node adjlist) alist))  ; remember so we can reset quickly
         (cons (agraph-node-name (agraph-source-node adjlist))
               (mapcan #'(lambda (x) (agraph-find-connected-mark x graph)) 
		       (mapcar #'agraph-edge-node (agraph-dest-nodes adjlist)))))))

(defun agraph-cyclicp (g)
  "(AGRAPH-CYCLICP G) returns T iff agraph G has a cycle."
  (let ((alist nil))
    (dolist (adj (agraph-adjacency-list g) nil)
      (when (agraph-cyclicp-mark adj)
	(reset-node-slots alist)
	(return t)))))

(defun agraph-cyclicp-mark (adjlist)
  "(AGRAPH-CYCLIC-MARK ADJLIST GRAPH) finds all the nodes reachable from the source node
   of ADJLIST in GRAPH.  Marks visited nodes as it goes
   and stores pointers to those nodes in alist.  Returns a list of node names."
  (cond ((agraph-node-slot (agraph-source-node adjlist))
	 (eq (agraph-node-slot (agraph-source-node adjlist)) 'open))  ; cycle iff hit open node
        (t
         (setf (agraph-node-slot (agraph-source-node adjlist)) 'open)   ; mark the node open
         (push (agraph-source-node adjlist) alist)  ; remember so we can reset quickly
	 (dolist (x (agraph-dest-nodes adjlist))
	   (when (agraph-cyclicp-mark (agraph-edge-node x)) (return-from agraph-cyclicp-mark t)))
	 (setf (agraph-node-slot (agraph-source-node adjlist)) 'closed)  ; mark the node closed
	 nil)))

(defun agraph-reachablep (from to g &key (test #'eq))
  "(REACHABLEP FROM TO G) returns T iff in the graph G starting at FROM, there
   is a path that ends at TO."
  (agraph-find-path from to g :test test))

(defun agraph-find-path (from to g &key (test #'eq))
  "(AGRAPH-FIND-PATH FROM TO G) returns a list whose first element is FROM and whose
   last element is TO representing a path in G from FROM to TO, if one exists.  Otherwise
   returns NIL."
  (let ((fromnode (agraph-find-node from g :test test))
        (tonode (agraph-find-node to g :test test))
        (res))
    (agraph-find-path-mark fromnode tonode g)
    (setq res (extract-path fromnode tonode g))
    (reset-node-slots alist)
    (nreverse res)))

(defun extract-path (fromnode tonode g)
  "(EXTRACT-PATH FROMNODE TONODE G) returns a list of node names from the pointers
   stored in the slots of graph G starting at TONODE and stopping at FROMNODE.  These
   are backpointers, hence the variable names."
  (cond ((not tonode) nil)
        ((eq tonode fromnode) (list (agraph-node-name (agraph-source-node tonode))))
        ((not (agraph-node-slot (agraph-source-node tonode))) nil)
        (t
         (cons (agraph-node-name (agraph-source-node tonode))
               (extract-path fromnode (agraph-node-slot (agraph-source-node tonode)) g)))))

(defun agraph-find-path-mark (fromnode tonode graph)
  "(AGRAPH-FIND-PATH-MARK NODE GRAPH) finds a path from FROMNODE to TONODE in graph GRAPH.
   Marks nodes as visited and adds parent pointers as it goes. Returns T once found or NIL otherwise."
  
  (cond ((eq fromnode tonode) T)
        ((or (not fromnode) (not tonode)) nil)
        (t
         ; find all the neighbors of FROMNODE and try one
         (some #'(lambda (x) 
                   (when (not (agraph-node-slot (agraph-source-node (agraph-edge-node x))))
                     (setf (agraph-node-slot (agraph-source-node (agraph-edge-node x))) fromnode)
                     (setf alist (cons (agraph-source-node (agraph-edge-node x)) alist))
                     (agraph-find-path-mark (agraph-edge-node x) tonode graph))) 
               (agraph-dest-nodes fromnode)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
