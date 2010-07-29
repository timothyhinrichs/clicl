;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; external.lisp
;;;     routines for interfacing with external systems
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defparameter *minisat* "/Applications/research/minisat/simp/minisat")
(defvar *datalognot* 'not)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Terminal ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun pretty-print-s (facts)
  "(PRETTY-PRINT-S FACTS) pretty-prints facts to a string."
  (let ((result (make-array 0 
			    :element-type 'character 
			    :adjustable t 
			    :fill-pointer 0)))
    (pretty-print facts result)
    result))

(defun pretty-print (facts &optional (s t))
  "(PRETTY-PRINT FACTS) prints results of pretty-print to the terminal."
  (cond ((listp facts)
         (if (atom (first facts))     ; single fact or a list of facts
           (pretty-print-one facts s)
           (pretty-print-list facts s)) t)
        ((hash-table-p facts)
         (maphash #'(lambda (key value) 
		      (format s "~S: ~S~%" key value)) facts) t)
        (t (format s "~S~%" facts) t)))

(defun pretty-print-list (facts &optional (s t))
  "(PRETTY-PRINT-LIST FACTS) pretty-prints each fact, making required 
   replacements, and adds extra lines between each one.
   Returns a string."
  (mapc #'(lambda (x) (pretty-print-one x s)) facts))

(defvar *connectives* '(<= => <=> or and not))
(defvar *html-connectives* 
  '((<= . "&lt;=") (=> . "=&gt;") (<=> . "&lt;=&gt;") 
    (or . "or") (and . "and") (not . "not")))

(defun pretty-print-one (fact s)
  (let ((indent "  ") (offset "    "))
    (cond ((not (listp fact)) 
           (format s "~A" indent) 
           (pretty-print-term fact s) 
           (format s "~%"))
          (t
           (cond ((eq (car fact) '<=)
                  (format s "~A(<= " indent)
                  (pretty-print-term (cadr fact) s)
                  (when (cddr fact) (format s "~%"))
                  (do ((c (cddr fact) (cdr c)))
                      ((null c))
                    (format s "~A~A" indent offset)
                    (pretty-print-term (car c) s)
                    (when (cdr c) (format s "~%")))
                  (format s ")~%"))
                 (t
                  (format s "~A" indent)
                  (pretty-print-term fact s)
                  (format s "~%")))))))

(defun pretty-print-term (a s)
  (cond ((stringp a) (prin1 a s))
        ((atom a) (format s "~(~A~)" a))
        (t
         (format s "(")
         (pretty-print-term (car a) s)
         (dolist (term (cdr a))
           (format s " ")
           (pretty-print-term term s))
         (format s ")"))))

(defun pcontents (th)
  "(PCONTENTS TH) prints out the contents of th using pretty-print.  
   Returns the theory."
  (pretty-print (contents th))
  th)

(defun pfacts (x th)
  "(PFACTS X TH) prints out those sentences in theory TH mentioning 
   something that unifies with x."
  (pretty-print (facts x th)))

(defun indent (depth stream &optional 
	       (textoutput #'(lambda (x) (declare (ignore x)) "|")))
  (do ((i 1 (1+ i)))
      ((> i depth))
      (format stream " ~A " (funcall textoutput i))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; TPTP ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;; Input ;;;;;;;;;;;;;;;; 

(defun read-file-tptp (tptpprob)
  (setq tptpprob (string tptpprob))
  (read-file (concatenate 'string 
			  "gullible:Users:thinrich:Apps:tptp:TPTP-v3.0.1:kif:" 
                          (subseq tptpprob 0 3) ":"
                          tptpprob 
                          ".kif")))

(defun kif2epilogtp (tptpprob 
		     &key (indir "gullible:Users:thinrich:Apps:tptp:TPTP-v3.0.1:epilog:")
		     (outdir "gullible:users:thinrich:apps:tptp:TPTP-v3.0.1:epilog_full:")
		     (goalify nil)
		     (contrapositives nil)
		     (sort nil))
  "(KIF2EPILOGTP TPTPPROB DIR) runs kif2epilogl on the TPTP problem tptpprob, 
  outputs the result into outdir, and names the theory tptpprob"
  (setq tptpprob (string tptpprob))

  ; the first 3 characters of the name give the directory
  (let* ((dir (subseq tptpprob 0 3))
         (extindex (search "." (reverse tptpprob)))
         (th (reverse (subseq (reverse tptpprob) (1+ extindex))))
         (doc (if contrapositives 
		  "includes all contrapositives" 
		  "may not include all contrapositives")))
    (kif2epilogl (concatenate 'string indir dir ":" tptpprob)
                 (concatenate 'string outdir dir ":" tptpprob)
                 th
                 :goalify goalify
                 :contrapositives contrapositives
                 :doc doc
                 :sort sort)))

;;;;;;;;;;;;;;;; Output ;;;;;;;;;;;;;;;; 
(defun fhl2tptp (th &key (stream t) (dca t) (una t))
  (setq th (fhlc th 'fol 'unsat :dca dca :una una))
  (dolist (q (contents th))
    (kif2tptp (quantify q) 'axiom stream)))

(defun fhlq2tptpq (p th &key (stream t) (dca t) (una t))
  (multiple-value-setq (th p) (fhlc th 'fol 'valid :query p :dca dca :una una))
  (dolist (q (contents th))
    (kif2tptp q 'axiom stream))
  (kif2tptp p 'conjecture stream))

(defun kif2tptp (p type s &optional (fof t))
  (if fof
    (setq fof "fof")
    (setq fof "cnf"))
  (format s "~A(~A, ~(~A~), ~%" fof (gentemp "form") type)
  (kif2tptp-formula p s)
  (format s ").~%"))

(defun kif2tptp-formula (p s)
  (cond ((atom p) (format s "~A" p))
        ((eq (car p) 'not) 
         (format s "( ~A " (tptp-logic 'not))
         (kif2tptp-formula (second p) s)
         (format s ")~%"))

        ((member (car p) '(and or))
         (format s "(")
         (list2infix (cdr p) (tptp-logic (car p)) #'kif2tptp-formula s)
         (format s ")"))

        ((eq (car p) '<=)
         (cond ((cdddr p) (kif2tptp-formula `(<= ,(cadr p) ,(maksand (cddr p))) s))
               ((not (cddr p)) (kif2tptp-formula (second p) s))
               (t
                (format s "( ")
                (kif2tptp-formula (second p) s)
                (format s " ~A " (tptp-logic '<=))
                (kif2tptp-formula (third p) s)
                (format s ")"))))

        ((eq (car p) '=>)
         (cond ((cdddr p) (kif2tptp-formula `(=> ,(maksand (cdr (butlast p))) ,(car (last p))) s))
               (t
                (format s "(")
                (kif2tptp-formula (second p) s)
                (format s " ~A " (tptp-logic '=>))
                (kif2tptp-formula (third p) s)
                (format s ")"))))

        ((eq (car p) '<=>)
         (format s "(")
         (kif2tptp-formula (second p) s)
         (format s " ~A " (tptp-logic '<=>))
         (kif2tptp-formula (third p) s)
         (format s ")"))

        ((member (car p) '(forall exists))
         (format s "( ~A [" (tptp-logic (car p)))
         (list2infix (second p) "," #'kif2tptp-var s)
         (format s "] : ")
         (kif2tptp-formula (third p) s)
         (format s ")~%"))

        (t
         (kif2tptp-term p s))))

(defun kif2tptp-term (p s)
  (cond ((not p) (format s "nil"))
        ((varp p) (kif2tptp-var p s)) 
        ((atom p) (kif2tptp-constant p s))
        (t
         (cond ((eq (car p) '=) 
                (format s "(")
                (list2infix (cdr p) '= #'kif2tptp-term s)
                (format s ")"))
               (t
                (kif2tptp-constant (car p) s)
                (when (cdr p)
                  (format s "(")
                  (kif2tptp-term (second p) s)
                  (dolist (e (cddr p))
                    (format s ", ")
                    (kif2tptp-term e s))
                  (format s ")")))))))

(defun kif2tptp-constant (p s)
  (when (numberp p)
    (setq p (stringappend "tlh" (tostring p))))
  (setq p (format nil "~A" p))
  (when (stringposition "-" p)
    ;(format t "~&Warning: collapsing symbol ~A to " p)
    (setq p (coerce (remove (coerce "-" 'character) (coerce p 'list) :test #'char=) 'string))
    ;(format t "~A.~%" p)
    )

  (format s "~(~A~)" p))

(defun tptp-logic (op)
  (case op
    (and '&)
    (or #\|)
    (not '~)
    (forall '!)
    (exists '?)
    (=> '=>)
    (<= '<=)
    (<=> '<=>)
    (otherwise 'uhoh)))

(defun list2infix (l op func s)
  (cond ((atom l) (funcall func l s))
        ((null (cdr l)) (funcall func (car l) s))
        (t
         (funcall func (car l) s)
         (dolist (i (cdr l))
           (format s " ~A " op)
           (funcall func i s)))))

(defun kif2tptp-var (v s) 
  (setq v (devariable v))
  (if (numberp v)
    (format s "~:@(TLH~A~)" v)
    (format s "~:@(~A~)" v)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; XCSP ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;; XCSP to * ;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; only worried about map coloring instances
; input has been written in a form Lisp will easily read.
; see Research/scripts/style/xcsp and ../xslt.pl
; for now, just enough to handle the map coloring instances

#|
(defun xcsp2dlvs-map (fn &optional 
                            (indir "gullible:users:thinrich:apps:mapcsp:out:") 
                            (compdir "gullible:users:thinrich:apps:mapcsp:dlvcomp:")
                            (incdir "gullible:users:thinrich:apps:mapcsp:dlvinc:"))
  (when (probe-file (stringappend indir (tostring fn)))
    (define-theory 'test "" (read-file (stringappend indir (tostring fn))))
    (with-open-file (s (stringappend compdir (tostring fn)) :direction :output :if-does-not-exist :create :if-exists :supersede)
      (xcsp2completedlv-map 'test s))
    ; (with-open-file (s (stringappend incdir (tostring fn)) :direction :output :if-does-not-exist :create :if-exists :supersede)
    ;   (xcsp2dlv-map 'test s))
))
(dolist (f (set-difference (mapcar #'file-namestring (directory "gullible:users:thinrich:apps:mapcsp:out:*"))
                           (mapcar #'file-namestring (directory "gullible:users:thinrich:apps:mapcsp:dlvcompshort:*"))
                           :test #'equal))
  (print f)
  (xcsp2dlvs-map f))

|#

; differs from xcsp2dlv-map because here we can use an existential quantifier
(defun xcsp2tptp-map (th &optional (s t))
  (let (node adj hue basic exist)
    (setq hue (xcsp2complete-map-hue th))
    (setq adj (xcsp2complete-map-adj th))
    (setq node (xcsp2complete-map-node th))
    (setq basic (list '(<= (hue ?y) (color ?x ?y))
                      '(<= (node ?x) (color ?x ?y))
                      '(<= (not (color ?z ?y)) (color ?x ?y) (adj ?x ?z))))
    ; every node is colored one of the hues.
    (setq exist `(<= (exists ?y (color ?x ?y)) (node ?x)))
    (fhl2tptp (nconc (cons exist basic) hue adj node) :stream s)))

(defun xcsp2dlv-map (th &optional (s t))  
  (let ((*datalognot* '-) node adj hue basic exist)
    (setq hue (xcsp2complete-map-hue th))
    (setq adj (xcsp2complete-map-adj th))
    (setq node (xcsp2complete-map-node th))
    (setq basic (list '(<= (node ?x) (color ?x ?y))
                      '(<= (not (color ?z ?y)) (color ?x ?y) (adj ?x ?z))))
    ; every node is colored one of the hues.
    (setq exist (list '<= (maksor (mapcar #'(lambda (x) `(color ?x ,(second x))) hue)) '(node ?x)))
    (print-datalog (nconc (cons exist basic) hue adj node) s)))

(defun xcsp2completedlv-map (th &optional (s t))
  (setq th (xcsp2complete-map th))
  (setq th (mapcar #'(lambda (rule) (mapcar #'(lambda (lit) (cond ((atom lit) lit)
                                                                  ((and (eq (car lit) 'not) (listp (second lit)) (eq (first (second lit)) '=))
                                                                   (maknot `(e ,(second (second lit)) ,(third (second lit)))))
                                                                  (t lit))) rule))
                   th))
  (push `(<= (e ?x ?x) (hue ?x)) th)
  (let ((*datalognot* 'not))
    (print-datalog th s)
    (print-datalog-atom 'go? s)
    (format s "~%")))

(defun xcsp2prolog-map (th &optional (s t))
  (let ((*datalognot* 'notparens))
    (print-datalog (xcsp2complete-map th) s)))

(defun xcsp2complete-map (th)
  (let (hue adj b adjhash vars v1 v2 newadds)
    (setq hue (xcsp2complete-map-hue th))
    (setq adj (xcsp2complete-map-adj th))
    ;(setq node (xcsp2complete-map-node th))
    (setq adjhash (make-hash-table))
    (setq vars (make-hash-table))
    ; hash on both first and second args
    (dolist (a adj) 
      (setf (gethash (second a) adjhash) (cons a (gethash (second a) adjhash)))
      (setf (gethash (third a) adjhash) (cons a (gethash (third a) adjhash))))

    ; compute body of ans
    (setq b nil)
    (dolist (q adj)
      (setq v1 (second q))
      (setq v2 (third q))
      ; add this adj as a distinction constraint only if it has not been added
      (when (member q (gethash v1 adjhash))
        (setq newadds nil)

        ; add hue for the first variable if necessary
        (unless (gethash v1 vars)
          (push `(hue ,(makevariable v1)) b)
          (setf (gethash v1 vars) t))
        ; add all equality pairs that we can
        (dolist (p (gethash v1 adjhash))
          (when (and (gethash (second p) vars) (gethash (third p) vars))
            (push `(not (= ,(makevariable (second p)) ,(makevariable (third p)))) b)
            (push p newadds)))

        ; add hue for the second variable if necessary
        (unless (gethash v2 vars)
          (push `(hue ,(makevariable v2)) b)
          (setf (gethash v2 vars) t))
        ; add all equality pairs that we can
        (dolist (p (gethash v2 adjhash))
          ; be more efficient to remove p from adjhash as we go--avoids this not member check in newadds
          (when (and (gethash (second p) vars) (gethash (third p) vars) (not (member p newadds :test #'equal)))
            (push `(not (= ,(makevariable (second p)) ,(makevariable (third p)))) b)
            (push p newadds)))

        ; remove the used adj facts from hash table
        (dolist (v (remove-duplicates (apply #'append (mapcar #'cdr newadds))))
          (setf (gethash v adjhash) (set-difference (gethash v adjhash) newadds)))))

    (nconc hue (mapcar #'(lambda (x) (maksafe x 'hue nil)) (reduce-vars-per-rule (list* '<= 'go (nreverse b)))))))

    ;(setq head (cons 'ans (mapcar #'(lambda (x) (makevariable (second x))) node)))
    ;(list* (list '<= 'go head)
    ;       (list* '<= head (nreverse b))
    ;       hue)))

(defun reduce-vars-per-rule (r)
  "(REDUCE-VARS-PER-RULE R) takes a rule h <= b1 ^ ... ^ bn and returns 
   a set of rules where the number of variables per rule is hopefully reduced.
   Probably only works for map coloring: go <= hue(x) ^ hue(y) ^ x#y ^ hue(z) ^ y#z
   becomes go <= hue(x) ^ a1(x), a1(x) <= hue(y) ^ x#y ^ a2(y), a2(y) <= hue(z) ^ y#z."
  (cond ((atom r) (list r))
        ((and (eq (car r) '<=) (not (cddr r))) (list r))
        ((eq (car r) '<=) 
         (let (segments seg tmp vars nexthead)
           (setq tmp nil)
           ; segment the rule into the resulting rule bodies
           ; eat until first positive literal; then on the second, start a new segment.
           (dolist (l (cddr r))
             (cond ((and tmp (positive-literalp l)) 
                    (push (nreverse seg) segments)
                    (setq seg (list l)))
                   ((positive-literalp l)
                    (push l seg)
                    (setq tmp t))
                   (t (push l seg))))
           (push (nreverse seg) segments)

           ; compute the variables for each segment (intelligently).  Notice we're using
           ;  the fact that the rule segments are in reverse order already
           (setq tmp nil)
           (dolist (s segments)
             (push (cons s tmp) vars)
             (setq tmp (union (vars s) tmp)))
           ;(print vars)

           ; now, for each segment create a rule for that segment and insert the head of that rule 
           ;    at the end of the previous segment.  Notice the segments are in the proper order now.
           (setq nexthead (list* 'a0 (intersection (vars (car (first vars))) (cdr (first vars)))))
           (setq r (append (list* '<= (second r) (car (first vars))) 
                           (list nexthead)))
           (do ((v (cdr vars) (cdr v))
                (i 1 (1+ i)) nextnexthead (result (list r)))
               ((null v) (nreverse result))
             (setq nextnexthead (list* (tosymbol (format nil "a~A" i)) 
                                       (intersection (vars (union (car (first v)) (vars nexthead))) (cdr (first v)))))
             (if (null (cdr v))
               (push (list* '<= nexthead (car (first v))) result)
               (push (append (list* '<= nexthead (car (first v))) (list nextnexthead)) result))
             (setq nexthead nextnexthead))))))

(defun xcsp2complete-map-adj (th)
  (let (pairs e pos x y)
    (setq pairs (viewfinds '?x '(constraint ?name ?scope ?x) th))
    (setq e nil)
    (dolist (p pairs (nreverse e))
      (setq pos (position #\Space p))
      (setq x (tosymbol (subseq p 0 pos)))
      (setq y (tosymbol (subseq p (1+ pos))))
      (push `(adj ,x ,y) e))))
   
(defun xcsp2complete-map-hue (th)
  (let (h pos low high newth)
    ; define hues
    (setq newth nil)
    (setq h (viewfinds '?x '(domain ?y ?x) th))
    (assert (null (cdr h)) nil "Should only have one domain in a map coloring problem.")
    (setq h (car h))
    (setq pos (position #\. h))
    (setq low (tosymbol (subseq h 0 pos)))
    (setq pos (position #\. h :start (1+ pos)))
    (setq high (tosymbol (subseq h (1+ pos))))
    (dotimes (i (1+ (- high low)) newth)
      (push `(hue ,i) newth))))

(defun xcsp2complete-map-node (th)
  (nreverse (mapcar #'(lambda (x) `(node ,(tosymbol x))) (viewfinds '?x '(var ?x @y) th))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;; KIF to XCSP ;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; NEEDS TO BE FACTORED: FIRST CONVERT THE FHLQ INTO A CSP AND THEN
;   WRITE THAT CSP OUT INTO THE XCSP FORMAT.

(defun fhlq2xcsp (p th &optional (s t))
  "(FHLQ2XCSP P TH S) converts the finite herbrand logic query
   TH |= Q into a CSP, output in XCSP format.  Assumes TH consists
   of a set of extensionally defined tables (just the positive or the
   negative literals), and p is a negative, disjunctive, constant-free
   query over just those tables.  There is a solution to the resulting
   CSP iff the entailment DOES NOT hold."
  (let (objs prop tmp)
    ; associate with each object constant an integer
    (setq objs (objs (maksand (contents th))))
    (setq prop 'tlhdcsymb2index)
    (setq tmp 0)
    (dolist (v objs)
      (setf (get (parameter-symbol v) prop) tmp)
      (setq tmp (1+ tmp)))
    
    ; convert problem to XCSP, passing prop along
    (setq p (strip-quantifiers (nnf (maknot (quantify p)))))
    (format s "<instance>~%")
    (fhlq2csp-domains th prop s)
    (fhlq2csp-variables p s)
    (fhlq2csp-relations th prop s)
    (fhlq2csp-constraints p s)
    (format s "</instance>~%")))

(defun fhlq2csp-domains (th prop s)
  ; for now, just the one domain
  (let (dca)
    (setq dca (compute-dca th))
    (format s "<domains nbDomains=\"1\">~%")
    (format s "<domain name=\"univ\" nbValues=\"~A\">~%" (length dca))
    (format s "~A " (get (car dca) prop))
    (dolist (v (cdr dca))
      (format s " ~A " (get v prop)))
    (format s "~%")
    (format s "</domain></domains>~%")))

(defun fhlq2csp-variables (p s)
  (let (vars)
    (setq vars (freevars p))
    (format s "<variables nbVariables=\"~A\">~%" (length vars))
    (dolist (v vars)
      (format s "<variable name=\"~A\" domain=\"univ\" />~%" v))
    (format s "</variables>~%")))

(defun fhlq2csp-relations (th prop s)
  (let (pos neg usingnegs ext p rs)
    (setq rs (remove-if-not #'isrelation (get-vocabulary (maksand (contents th)))))
    (format s "<relations nbRelations=\"~A\">~%" (length rs))
    (dolist (r rs)
      (setq p (cons (parameter-symbol r) (maptimes #'newindvar (parameter-arity r))))
      (setq pos (fullfinds p p th))
      (setq neg (fullfinds (maknot p) (maknot p) th))
      (setq usingnegs (not (null neg)))
      (if usingnegs
        (setq ext (mapcar #'maknot neg))  ; flip back to positives
        (setq ext pos))
      (format s "<relation name=\"~A\" arity=\"~A\" nbTuples=\"~A\" semantics=\"~A\">~%"
              (parameter-symbol r) (parameter-arity r) (length ext)
              (if usingnegs "conflicts" "supports"))
      (tuple2xcsp (first ext) prop s)
      (dolist (atom (cdr ext))
        (format s " | ")
        (tuple2xcsp atom prop s))
      (format s "~%</relation>~%"))
    (format s "</relations>~%")))

(defun tuple2xcsp (atom prop s)
  (format s "~A" (get (second atom) prop))
  (dolist (v (cddr atom))
    (format s " ~A" (get v prop))))

(defun fhlq2csp-constraints (p s)
  (setq p (drop-ands p))
  (format s "<constraints nbConstraints=\"~A\">~%" (length p))
  (dolist (c p)
    (format s "<constraint name=\"~A\" arity=\"~A\" scope=\"~A\" reference=\"~A\" />~%"
            (gentemp "c") (1- (length c)) (tospacedstring (cdr c)) (car c)))
  (format s "</constraints>~%"))




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; SAT ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;(defvar *tree* nil)
(defvar *sents* nil)
;(defvar *count* 0)

(defvar *verbosity* 'low)
(defun set-verbosity (a) (setq *verbosity* a))
(defun low-verbosityp () (member *verbosity* '(low medium high)))
(defun med-verbosityp () (member *verbosity* '(medium high)))
(defun high-verbosityp () (eq *verbosity* 'high))

(defun sat-solve (ground)
  "(SAT-SOLVE GROUND) takes a theory of GROUND clauses as input.  
   Translates clauses to DIMACS and invokes a SAT solver.  Then translates
   the model provided by the SAT solver back to extensions in ground logic
   and returns the result (or :unsat if SAT solver produces no model)."
  (let ((filename "/tmp/epilog/sat-solve.dimacs") num2atom)
    (with-open-file (f filename :direction :output 
		       :if-does-not-exist :create :if-exists :supersede)
      (setq num2atom (kifcs2dimacs (contents ground) f)))
    (run-sat-solver filename num2atom)))

(defun run-sat-solver (filename num2atom &optional (solver 'minisat))
  (case solver
    (minisat (run-minisat-solver filename num2atom))))

(defun run-minisat-solver (infile num2atom)
  (let (realtime r (outfile "/tmp/epilog/minisat.out"))
    (setq realtime (get-internal-real-time))
    (run-program *minisat* (list infile outfile))
    (setq realtime (/ (- (get-internal-real-time) realtime) internal-time-units-per-second))
    (setq r (read-any-file outfile))
    (setq r (split-string r '(#\Newline)))
    (cond ((search "unsat" (first r)) (values :unsat realtime))
	  (t ; parse resulting model and invert to ground atoms
	   (setq r (mapcar #'tosymbol (split-string (second r) '(#\Space))))
	   (setq r (run-sat-solver-invertmodel (butlast r) num2atom))
	   (values r realtime)))))

(defun run-sat-solver-invertmodel (numlist num2atom)
  (mapcarnot #'(lambda (x) (if (< x 0) nil (gethash x num2atom))) numlist))

;;;;;;;;;;;;;; Printing to DIMACS ;;;;;;;;;;;;;;

; version where we use formula renaming and then ground and use any clausifier with timing actual conversion process
(defun fhlq2sat-mem-timed (p th &key (stream t) (una t) (reflex t))
  "(FHLQ2SAT-MEM P TH S) converts the finite herbrand logic query
    TH |= P into a satisfiability problem, output in DIMACS cnf.
    This version minimizes memory consumption by running each sentence all the way to dimacs and
    out on to disk before touching the next sentence."
  (fhl2sat-mem-timed (cons (maknot (quantify p)) (contents th)) :stream stream :una una :reflex reflex))

(defun fhl2sat-mem-timed (th &key (stream t) (una t) (reflex t))
  "(FHL2SAT-MEM P TH S) converts the finite herbrand theory TH into DIMACS cnf.
    This version minimizes memory consumption by running each sentence all the way to dimacs and
    out on to disk before touching the next sentence."
  (let (dca newps newq others newth clauses (*count* 0) clausecount (*timesofar* 0))
    (format t "~&Preparing ~A sentences ...~%" (length (contents th)))
    ; rename formulas before generating equality and preparing discrimination tree
    (setq newth nil)
    (add-time (dolist (q (contents th))
                (cond ((base-defn q) (setq newth (cons q newth)))
                      (t (multiple-value-setq (newq others) (formula-renaming-small q))
			 (setq newth (append (cons newq others) newth))))))  
    (add-time (setq dca (compute-dca newth)))
  
    ; add equality when necessary
    (add-time 
       ; don't need the dca, symmetry, or transitivity since when grounded, it is a tautology
       ; in addition, don't need substitution because of dca/una
     (setq newth (cons `(= ?x ?x) (nconc (equality-axioms newth 
							  :reflex reflex
							  :sym nil
							  :trans nil
							  :subst nil 
							  :unique-names una 
							  :dca nil) 
					 newth))))
    
    (when (high-verbosityp) (dolist (v newth) (pprint v)))
    (when (med-verbosityp) (print (remove-if #'isobject (get-vocabulary (maksand newth)))))
    
    (format t "~&Preparing discrimination tree ...~%")
    (add-time (setq *tree* (make-dtree)))
    (add-time (dt-prep (get-vocabulary (maksand newth)) *tree*))
    
    (format t "~&Grounding and Converting each of ~A sentences to dimacs ...~%" (length newth))   
    ; walk over this set of sentences, and for each one ground it, convert to cnf, and write it to the stream.
    (setq clausecount 0)
    (dolist (p newth)
      (add-time (setq newps (mapcan #'drop-ands (ground-herbrand p dca nil))))
      (dolist (q newps)
        (add-time (setq clauses (clausesets q)))
        (dolist (c clauses)
          (kifcs2dimacs-aux c *tree* stream)
          (setq clausecount (1+ clausecount)))))
    (format t "Constructed ~A clauses.~%" clausecount)
    (format stream "p cnf ~A ~A~%" *count* clausecount)
    *timesofar*))

; version where we use formula renaming and then ground and use any clausifier
; Other variants can be found later.
(defun fhlq2sat-mem (p th &key (stream t) (una t) (reflex nil))
  "(FHLQ2SAT-MEM P TH S) converts the finite herbrand logic query
    TH |= P into a satisfiability problem, output in DIMACS cnf.
    This version minimizes memory consumption by running each sentence all the way to dimacs and
    out on to disk before touching the next sentence."
  (fhl2sat-mem (cons (maknot (quantify p)) (contents th)) :stream stream :una una :reflex reflex))

(defun fhl2sat-mem (th &key (stream t) (una t) (reflex t))
  "(FHL2SAT-MEM P TH S) converts the finite herbrand theory TH
    into DIMACS cnf.
    This version minimizes memory consumption by running each sentence all the way to dimacs and
    out on to disk before touching the next sentence."
  (let (objs newps newq others newth clauses (*count* 0) clausecount)
    (format t "~&Preparing ~A sentences ...~%" (length (contents th)))

    ; rename formulas before generating equality and preparing discrimination tree
    (setq newth nil)
    (dolist (q (contents th))
      ; rip out numbers
      (setq q (replace-numbers q))
      ; then apply formula renaming
      (cond ((base-defn q) (setq newth (cons q newth)))
            (t (multiple-value-setq (newq others) (formula-renaming-small q))
	       (setq newth (append (cons newq others) newth)))))

    ; don't need the dca, symmetry, or transitivity since when grounded, they are all tautologies
    ; in addition, don't need substitution because of dca/una
    ; defaults reflect these statements
    (setq newth (cons `(= ?x ?x) (nconc (equality-axioms newth 
							 :reflex reflex
							 :sym nil
							 :trans nil
							 :subst nil 
							 :unique-names una 
							 :dca nil) 
					newth)))
    (setq objs (compute-dca newth))   
    (when (high-verbosityp) (dolist (v newth) (pprint v)))
    (when (med-verbosityp) (print (remove-if #'isobject (get-vocabulary (maksand newth)))))

    (format t "~&Preparing discrimination tree ...~%")
    (setq *tree* (make-dtree))
    (dt-prep (get-vocabulary (maksand newth)) *tree*)

    (format t "~&Grounding and Converting each of ~A sentences to dimacs ...~%" (length newth))
    ; walk over this set of sentences, and for each one ground it, convert to cnf, and write it to the stream.
    (setq clausecount 0)
    (dolist (p newth)
      (setq newps (mapcan #'drop-ands (ground-herbrand p objs nil)))
      (dolist (q newps)
        (setq clauses (clausesets q))
        (dolist (c clauses)
          (kifcs2dimacs-aux c *tree* stream)
          (setq clausecount (1+ clausecount)))))
    (format t "Constructed ~A clauses.~%" clausecount)
    (format stream "p cnf ~A ~A~%" *count* clausecount)))

(defun fhl2sat (th &key (stream t) (una t) (reflex t))
  "(FHLQ2SAT P TH S) converts the finite herbrand logic query
    TH |= P into a satisfiability problem, output in DIMACS cnf.
    Less efficient memory-wise than fhl2sat-mem but code is more modular."
  (setq th (fhlc th 'groundclauses 'unsat :una una :reflex reflex))
  (kifcs2dimacs th stream))

(defun fhlq2sat (p th &key (stream t) (una t) (reflex t))
  "(FHLQ2SAT P TH S) converts the finite herbrand logic query
    TH |= P into a satisfiability problem, output in DIMACS cnf."
  (setq th (fhlc th 'groundclauses 'unsat :query p :una una :reflex reflex))
  (kifcs2dimacs th stream))

(defun kifcs2dimacs (p &optional (s t))
  "(KIFCS2DIMACS P S) takes a set of ground clauses P and outputs P in conjunctive
   normal form in the DIMACS format: each ground atom is assigned a positive
   integer, and a negative literal is assigned the negation of that number.
   Returns a hash table mapping integers to ground atoms (for inverting results
   of SAT solver)."
  ;(format t "Mapping ground atoms to integers ...~%")
  (let ((*count* 0) int2atom)
    (setq *tree* (make-dtree))
    (setq int2atom (make-hash-table))
    (dt-prep (get-vocabulary (maksand p)) *tree*)
    
    (dolist (c p)
      (setq c (replace-numbers c))
      (kifcs2dimacs-aux c *tree* s int2atom))
    (format s "p cnf ~A ~A~%" *count* (length p))
    int2atom))

(defun kifcs2dimacs-aux (c tree s &optional (int2atom (make-hash-table)))
  "(KIFCS2DIMACS C S) takes a ground clause C, a discimination tree TREE
   mapping ground atoms to integers, a hash table INT2ATOM mapping integers
   to ground atoms, and a stream S.   It outputs C in DIMACS
   format to S while keeping TREE and INT2ATOM synchronized.  DIMACS
   represents each atom as a positive integer and each negated atom
   as -1 times the number for the atom."
  (let (val atom)
    (dolist (l (if (eq (relation c) 'or) (cdr c) (list c)))
      (setq atom (drop-not l))
      (setq val (dt-lookup atom tree))
      (cond ((numberp val) 
             (if (negative-literalp l)
               (format s "-~A " val)
               (format s "~A " val)))
            (t
             (setq *count* (1+ *count*))
             (dt-insert atom *count* tree)
	     (setf (gethash *count* int2atom) atom)
             (if (negative-literalp l)
               (format s "-~A " *count*)
               (format s "~A " *count*)))))
    (format s "0~%")))

#| Kept for posterity

; version where we ground in place then use structure-preserving clausal form conversion
(defun fhlq2sat-mem3 (p th &optional (s t) (add-equality t))
  (fhl2sat-mem2 (cons (maknot (quantify p)) (contents th)) s add-equality ))

(defun fhl2sat-mem3 (th &optional (s t) (add-equality t))
  (let (dca newps newth clauses maxkids symb2index (*count* 0) clausecount)
    (format t "~&Preparing ~A sentences ...~%" (length (contents th)))

    ; rename formulas before generating equality and preparing discrimination tree
    (setq newth nil)
    (setq newth (contents th))
    (setq dca (compute-dca newth))

    ; add equality when necessary
    (when add-equality
      (setq newth (cons `(= ?x ?x) (append (equality-axioms newth :rst nil :subst t :unique-names t :dca nil) newth))))
    
    (format t "~&Preparing discrimination tree ...~%")
    (setq *tree* (make-instance 'discrimination-tree))
    (multiple-value-setq (symb2index maxkids) (discrimination-tree-prep2 (get-vocabulary (maksand newth)) *tree*))

    ; walk over this set of sentences, and for each one ground it, convert to cnf, and write it to the stream.
    (format t "~&Converting each of ~A sentences to dimacs ...~%" (length newth))
    (setq clausecount 0)
    (dolist (p newth)
      (setq newps (mapcan #'drop-ands (ground-herbrand p dca nil)))
      (dolist (q newps)
        (setq clauses (mapcar #'(lambda (x) (if (and (listp x) (eq (car x) 'or)) (cdr x) x)) (cnf-small q)))
        (dolist (c clauses)
          (kifcs2dimacs-aux c symb2index maxkids s)
          (setq clausecount (1+ clausecount)))))
    (format t "Constructed ~A clauses.~%" clausecount)
    (format s "p cnf ~A ~A~%" *count* clausecount)))

; version where we ground in place and then use MRG's clausifier
(defun fhlq2sat-mem2 (p th &optional (s t) (add-equality t))
  (fhl2sat-mem2 (cons (maknot (quantify p)) (contents th)) s add-equality ))

(defun fhl2sat-mem2 (th &optional (s t) (add-equality t))
  (let (dca newps newth clauses maxkids symb2index (*count* 0) clausecount)
    (format t "~&Preparing ~A sentences ...~%" (length (contents th)))
    ; rename formulas before generating equality and preparing discrimination tree
    (setq newth nil)
    (setq newth (contents th))
    (setq dca (compute-dca newth))

    ; add equality when necessary
    (when add-equality
      (setq newth (cons `(= ?x ?x) (append (equality-axioms newth :rst nil :subst t :unique-names t :dca nil) newth))))
    (format t "~&Preparing discrimination tree ...~%")
    (setq *tree* (make-instance 'discrimination-tree))
    (multiple-value-setq (symb2index maxkids) (discrimination-tree-prep2 (get-vocabulary (maksand newth)) *tree*))

    ; walk over this set of sentences, and for each one ground it, convert to cnf, and write it to the stream.
    (format t "~&Converting each of ~A sentences to dimacs ...~%" (length newth))
    (setq clausecount 0)
    (dolist (p newth)
      (setq newps (mapcan #'drop-ands (ground-herbrand p dca nil)))
      (dolist (q newps)
        (setq clauses (clausesets q))
        (dolist (c clauses)
          (kifcs2dimacs-aux c symb2index maxkids s)
          (setq clausecount (1+ clausecount)))))
    (format t "Constructed ~A clauses.~%" clausecount)
    (format s "p cnf ~A ~A~%" *count* clausecount)))

|#


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Prover9 and MACE ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun fhlq2prover9q (p th &key (stream t) (dca t) (una t))
  (multiple-value-setq (th p) (fhlc th 'fol 'valid :query p :dca dca :una una))
  (kifq2prover9q p th stream))

(defun fhlq2maceq (p th &key (stream t) (dca t) (una t))
  (setq th (fhlc th 'fol 'unsat :query (quantify p) :dca dca :una una))
  (kif2mace th stream))

(defun fhl2mace (th &key (stream t) (dca t) (una t))
  (setq th (fhlc th 'fol 'unsat :dca dca :una una))
  (kif2mace th stream))

(defun kifq2prover9q-grounded (query sos &optional (s t) (smallcnf t))
  (let* ((dca (compute-dca (cons query (contents sos))))
         (theory nil) newq)

    ; ground each of the sentences in sos: grounding returns a set of sentences
    (dolist (p (contents sos))
      (setq theory (nconc theory (mapcan #'drop-ands (ground-herbrand p dca smallcnf)))))

    ; ground the query
    (setq newq (ground-herbrand query dca smallcnf))
    (kifq2prover9q (car newq) (append (mapcan #'drop-ands (cdr newq)) theory) s)))

(defun kifq2prover9q-small (query sos &optional (s t))
  "(KIFQ2PROVER9Q-SMALL QUERY SOS S) converts QUERY and SOS to a small form
   of CNF and then outputs the clauses in the prover9 format."
  (cond ((literalp query) (kifq2prover9q query (mapcan #'cnf-small (contents sos)) s))
        (t
         (let (newsos newgoal) 
           (setq newgoal (gentemp "goal"))
           (setq newsos (mapcan #'cnf-small (cons `(<= ,newgoal ,(quantify query)) (contents sos))))
           (kifq2prover9q newgoal newsos s)))))


(defun kifq2prover9q (query sos &optional (s t))
  (format s "~%set(prolog_style_variables).~%")
  (format s "set(auto).~%")
  (format s "formulas(sos).~%~%")
  (kiffs2otter (contents sos) s)
  (format s "end_of_list.")
  (format s "~%~%")
  (format s "formulas(goals).~%")
  (kiffs2otter (drop-ands query) s)
  (format s "~&end_of_list."))

(defun kifq2mace (query sos &optional (s t))
  (format s "~%set(prolog_style_variables).~%")
  (format s "formulas(sos).~%~%")
  (kiffs2otter (contents sos) s)
  (format s "~%~%")
  (kiff2otter (maknot (quantify query)) s)
  (format s "~%")
  (format s "end_of_list."))

(defun kif2mace (sos &optional (s t))
  (format s "~%set(prolog_style_variables).~%")
  (format s "formulas(sos).~%~%")
  (kiffs2otter (contents sos) s)
  (format s "~%~%")
  (format s "end_of_list."))

(defun kiffs2prover9 (formulas &optional (s t))
  (kiffs2otter formulas s))

(defun kiff2prover9 (formula &optional (s t))
  (kiff2otter formula s))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; LaTeX ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defun kiffs2latex (formulas &optional (dialect 'fol) (s t))
  (mapc #'(lambda (x) (kiff2latex x dialect s)) formulas))

(defun kifcs2latex (clauses &optional (dialect 'prolog) (s t))
  (format s "\\noindent\\texttt{")
  (dolist (c clauses)
    (kifc2latex c dialect s)
    (format s "\\\\~%"))
  (format s "}~%"))

(defun kiff2latex (p &optional (dialect 'fol) (s t))
  (setq p (kif2infix p))
  (infixkiff2latex (kif2infix p) dialect s))

(defun kifc2latex (p &optional (dialect 'fol) (s t))
  (setq p (kif2infix p))
  (infixkifc2latex (kif2infix p) dialect s))

(defun kif2infix (p)
  (cond ((atom p) p)
        ((member (car p) '(forall exists)) (list (first p) (second p) (kif2infix (third p))))
        ((eq (car p) 'not) (list 'not (kif2infix p)))
        ((member (car p) '(and or <=>))
         (cond ((null (cdr p)) (if (eq (car p) 'or) 'false 'true))
               (t
                (do ((newp (list (second p))) (as (cddr p) (cdr as)))
                    ((null as) (nreverse newp))
                  (setq newp (list* (kif2infix (car as)) (car p) newp))))))
        ((eq (car p) '<=) (list (kif2infix (second p)) '<= (kif2infix (maksand (cddr p)))))
        ((eq (car p) '=>) (list (kif2infix (maksand (cdr (butlast p)))) '=> (kif2infix (car (last p)))))
        (t p)))

; infix kif formula to latex
(defmethod infixkiff2latex (p (dialect (eql 'fol)) s)
  (let (tmp)
    (cond ((atom p) (format s "~A" (atom2latex p dialect)))
          ((member (first p) '(forall exists))
           (setq tmp (atom2latex (first p) dialect))
           (if (atom (second p))
             (format s "~A ~A" tmp (var2latex (second p) dialect))
             (dolist (v (second p))
               (format s "~A ~A" tmp (var2latex v dialect))))
           (format s ".(")
           (infixkiff2latex (third p) dialect s)
           (format s ")"))
          ((eq (first p) 'not)
           (format s "~A " (atom2latex 'not dialect))
           (infixkiff2latex (second p) dialect s))
          ((member (second p) '(<= => <=> and or))
           (format s "(")
           (infixkiff2latex (first p) dialect s)
           (dolist (h (cdr p))
             (format s " ")
             (cond ((atom h) (format s "~A" (atom2latex h dialect)))
                   (t (infixkiff2latex h dialect s))))
           (format s ")"))
          (t 
           (infixkif2latex-term p dialect s)))))

(defmethod infixkif2latex-term (u dialect s)
  (cond ((varp u) (format s "~A" (var2latex u dialect)))
        ((atom u) (format s "~A" (atom2latex u dialect)))  ; covers both operators and nonoperators
        (t
         (format s "~A(" (atom2latex (first u) dialect))
         (when (cdr u)
           (infixkif2latex-term (second u) dialect s))
         (dolist (h (cddr u))
           (format s ", ")
           (infixkif2latex-term h dialect s))
         (format s ") "))))

; infix kif clauses to latex
(defmethod infixkifc2latex (p (dialect (eql 'prolog)) s)
  (declare (notinline infixkiff2latex))
  (cond ((atom p) (format s "~A" (atom2latex p dialect)))
        ((eq (first p) 'not)
         (format s "~A " (atom2latex 'not dialect))
         (infixkifc2latex (second p) dialect s))
        ((eq (second p) '<=)  ; notice here we include no parentheses
         (infixkifc2latex (first p) dialect s)
         (dolist (h (cdr p))
           (format s " ")
           (cond ((atom h) (format s "~A" (atom2latex h dialect)))
                 (t (infixkifc2latex h dialect s)))))
        ((eq (second p) 'and)
         (infixkifc2latex (first p) dialect s)
         (dolist (h (cdr p))
           (format s " ")
           (cond ((atom h) (format s "~A" (atom2latex h dialect)))
                 (t (infixkifc2latex h dialect s)))))
        (t 
         (infixkif2latex-term p dialect s))))


(defmethod var2latex (v dialect)
  (declare (ignore dialect))
  v)

(defmethod var2latex (v (dialect (eql 'fol)))
  (format nil "~(~A~)" (devariable v)))

(defmethod var2latex (v (dialect (eql 'prolog)))
  (format nil "~:(~A~)" (devariable v)))

(defmethod atom2latex (op (dialect (eql 'fol)))
  (case op
    (and "\\wedge")
    (or "\\vee")
    (not "\neg")
    (<= "\\Leftarrow")
    (=> "\\Rightarrow")
    (<=> "\\Leftrightarrow")
    (forall "\\forall")
    (exists "\\exists")
    (otherwise (format nil "~(~A~)" op))))

(defmethod atom2latex (op (dialect (eql 'prolog)))
  (case op
    (and ",")
    (or "|")
    (not "not")
    (<= "\\mbox{ $:\\!-\\ $}")
    (=> "\\mbox{ $-\\!:\\ $}")
    (<=> "\\mbox{ $:\\!-\\ $}")
    (forall "\\forall")
    (exists "\\exists")
    (otherwise (format nil "~(~A~)" op))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Otter ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;; Formulas ;;;;;;;;;;;;;;;;
(defun kiffs2otter (formulas &optional (s t))
  (dolist (f formulas)
    (kiff2otter f s)
    (format s "~%")))

(defun kiff2otter (formula &optional (s t))
  "(KIFF2OTTER FORMULA S) prints out the kif formula FORMULA to stream s
   in otter syntax."
  (formula2otter (quantify formula) s)
  (format s "."))

(defun formula2otter (p s)
  "(FORMULA2OTTER P) takes a formula P and converts it to the formula-form
   of the otter syntax, i.e. with quantification."
  (cond ((atom p) (print-otter-lit (lit2otter p) s))

        ((find (car p) '(forall exists))
         (cond ((atom (cadr p))
                (format s "(")
                (print-otter-logic (otter-logic (car p)) s)
                (space s)
                (print-otter-atom (cadr p) s)
                (space s))
               (t
                (dolist (v (cadr p))
                  (format s "(")
                  (print-otter-logic (otter-logic (car p)) s)
                  (space s)
                  (print-otter-atom v s)
                  (space s))))
         (formula2otter (caddr p) s)
         (if (atom (cadr p))
           (format s ")")
           (dotimes (v (length (cadr p)))
             (format s ")"))))

        ((find (car p) '(or and))
         (when (cdr p)
           (when (cddr p) (format s "("))
           (formula2otter (cadr p) s)
           (dolist (h (cddr p))
             (print-otter-logic (otter-logic (car p)) s)
             (space s)
             (formula2otter h s))
           (when (cddr p) (format s ")"))))

        ((eq (car p) '=>)
         (when (cddr p)
           (format s "(")
           (formula2otter (maksand (cdr (butlast p))) s)
           (print-otter-logic (otter-logic '=>) s)
           (space s))
         (formula2otter (car (last p)) s)
         (when (cddr p) (format s ")")))
        ((eq (car p) '<=)
         (when (cddr p)
           (format s "(")
           (formula2otter (maksand (cddr p)) s)
           (print-otter-logic (otter-logic '=>) s)
           (space s))
         (formula2otter (cadr p) s)
         (when (cddr p) (format s ")")))

        ((eq (car p) '<=>)
         (format s "(")
         (formula2otter (cadr p) s)
         (space s)
         (print-otter-logic (otter-logic '<=>) s)
         (space s)
         (formula2otter (caddr p) s)
         (format s ")"))

        ((eq (car p) 'not)
         (print-otter-logic (otter-logic 'not) s)
         (formula2otter (cadr p) s))
        (t (print-otter-lit (lit2otter p) s)
           (space s))))

(defun otter-logic (symb)
  "(OTTER-LOGIC SYMB) takes a logical symbol and returns the otter equivalent."
  (case symb
    (forall 'all)
    (exists 'exists)
    (or #\|)
    (and '&)
    (=> '->)
    (<=> '<->)
    (not '-)
    (otherwise 'uhoh)))





;;;;;;;;;;;;;;; Clausal ;;;;;;;;;;;;;;;;;
(defun kif2otter (p &optional (stream t))
  "(KIF2OTTER P) takes a kif sentence and prints out the 
   otter equivalent."

  (format stream "~%set(prolog_style_variables).~%")
  (format stream "set(auto).~%")
  (format stream "list(usable).~%~%")
  (print-otter (mapcar #'clause2otter (clausesets p)) stream)
  (format stream "end_of_list.")
  t
)
#|  Duplicated above
(defun kif2mace (p &optional (stream t))
  "(KIF2MACE P) takes a kif sentence P and prints out the mace equivalent."
  (format stream "~%assign(iterate_up_to, 10).~%")
  (format stream "set(prolog_style_variables).~%")
  (format stream "set(verbose).~%")
  (format stream "clauses(theory).~%")
  (print-otter (mapcar #'clause2otter (clausesets p)) stream)
  (format stream "end_of_list.~%")
  t)
|#

;;;;;;;;;;;; Converting ;;;;;;;;;;;;
;; Convert as much as possible, but leave variables
;;  in tact so printing can be done easily.
;; Leave in prefix notation.

(defun clause2otter (clause)
  "(CLAUSE2OTTER CLAUSE) takes a clause CLAUSE and returns
   the otter version with | interspersed."
  (if (null clause) nil

      (let ((result (list (lit2otter (car clause)))))
        (do ((c (cdr clause) (cdr c)))
            ((null c) (reverse (cons "." result)))
          (setq result (cons (lit2otter (car c)) (cons "|" result))))))
)
(defun lit2otter (lit)
  "(LIT2OTTER ATOM) takes a literal LIT and returns the
   otter version."
  (cond ((atom lit) lit)
        ((and (listp lit) (eq (car lit) 'not)) (maknot (lit2otter (cadr lit))))
        ((and (listp lit) (null (cdr lit))) (car lit))
        ((and (listp lit) (eq (car lit) 'same)) (cons '= (mapcar #'term2otter (cdr lit))))
        ((and (listp lit) (eq (car lit) 'distinct)) (maknot (cons '= (mapcar #'term2otter (cdr lit)))))
        ;((and (listp lit) (eq (car lit) '=)) (cons 'e (mapcar #'term2otter (cdr lit))))
        (t (cons (car lit) (mapcar #'term2otter (cdr lit)))) )
)
(defun term2otter (term)
  "(TERM2OTTER TERM) takes a term and returns the otter equivalent."
  (cond ;((varp term) (devariable term))
        ((numberp term) (if (< term 0) (tosymbol (list "tlhminus" (abs term))) (tosymbol (list "tlh" term)))) 
        ((atom term) term)
        ((emptyfunction term) (car term))      ; if an empty function, just return function name
        ((listp term) (cons (car term) (mapcar #'term2otter (cdr term))))
        (t term))
)

;;;;;;;;;;;; Printing ;;;;;;;;;;;;;


(defun print-otter (clauses &optional (stream t))
  "(PRINT-OTTER CLAUSES) prints each clause out, one line at a time."
  (dolist (c clauses)
    (dolist (l c)
      (print-otter-lit l stream)
      (format stream " "))
    (format stream "~%"))
  t
)
(defun print-otter-lit (lit &optional (stream t))
  "(PRINT-OTTER-LIT LIT) takes a kif literal LIT and prints the 
   otter equivalent."
  (cond ((and (listp lit) (eq (car lit) 'not)) (print-otter-not lit stream))
        (t (print-otter-atom lit stream)))
)
(defun print-otter-not (lit &optional (stream t))
  "(PRINT-OTTER-NOT LIT) takes a kif negative literal LIT and prints the
   otter equivalent."
  (format stream "-")
  (print-otter-lit (cadr lit) stream)
)
(defun print-otter-atom (atom &optional (stream t))
  "(PRINT-OTTER-ATOM ATOM) takes a kif atom/term and prints the otter equivalent."
  (cond
        ((varp atom)
         (let ((v (devariable atom)))
           (if (numberp v)
             (format stream "~:@(TLH~A~)" v)
             (format stream "~:@(~A~)" v))))
        ((atom atom) (format stream "~(~A~)" atom))
#|        ((eq (car atom) 'distinct)
         (format stream "-")
         (print-otter-atom (cadr atom) stream)
         (format stream "=")
         (print-otter-atom (caddr atom) stream))
|#
        ((or (eq (car atom) '=) (eq (car atom) 'same))
         (format stream "(")
         (print-otter-atom (cadr atom) stream)
         (format stream "=")
         (print-otter-atom (caddr atom) stream)
         (format stream ")"))

        (t
         (cond ((null (cdr atom)) (format stream "~A()" (car atom)))
               (t
                (format stream "~(~A~)(" (car atom))
                (print-otter-atom (cadr atom) stream)
                (dolist (te (cddr atom))
                  (format stream ",")
                  (print-otter-atom te stream))
                (format stream ")")))))
)


(defun print-otter-logic (symb s)
  "(PRINT-OTTER-LOGIC SYMB) prints out a logical symbol to stream S."
  (format s "~(~A~)" symb))

(defun space (s) (format s " "))

;(defun newindvar () (gensym "?V"))

;(defun newseqvar () (gensym "@V"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Datalog ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun kifs2datalog (th &optional (stream t))
  (dolist (p (contents th)) (kif2datalog p stream)))

(defun kif2datalog (p &optional (stream t))
  (print-datalog (list (rule2datalog p)) stream))

;;;;;;;;;;;; Converting to datalog ;;;;;;;;;;;;
;; Convert as much as possible, but leave variables
;;  in tact so printing can be done easily.
;; Leave in prefix notation.

(defun rule2datalog (r)
  "(RULE2DATALOG R) takes a rule R and returns
   the infix datalog version."
  (cond ((null r) nil)
        ((atom r) (lit2datalog r))
	((eq (car r) '<=) (mapcar #'lit2datalog r))
        (t (lit2datalog r))))

(defun lit2datalog (lit)
  "(LIT2DATALOG LIT) takes a literal LIT and returns the
   datalog version."
  (cond ((atom lit) lit)
        ((eq (car lit) 'not) (maknot (lit2datalog (cadr lit))))
        ((eq (car lit) 'same) (cons 'equalitytlh (mapcar #'term2datalog (cdr lit))))
        ((eq (car lit) 'distinct) (maknot (cons 'equalitytlh (mapcar #'term2datalog (cdr lit)))))
        ((eq (car lit) '=) (cons 'equalitytlh (mapcar #'term2datalog (cdr lit))))
        (t (cons (car lit) (mapcar #'term2datalog (cdr lit))))))

(defun term2datalog (term)
  "(TERM2DATALOG TERM) takes a term and returns the datalog equivalent."
  (cond ;((varp term) (devariable term))
        ((atom term) term)
        ;((listp term) (cons (car term) (mapcar #'term2datalog (cdr term))))
        (t term)))

(defun emptyfunction (term)
  "(EMPTYFUNCTION TERM) returns T iff the term is a functional term with no arguments."
  (and (listp term) (null (cdr term))))



;;;;;;;;;;;; Printing ;;;;;;;;;;;;;

(defun print-datalog (rules &optional (stream t))
  "(PRINT-DATALOG RULES) prints each clause out, one line at a time."
  (dolist (r rules)
    (print-datalog-rule r stream) 
    (format stream "~%")))

(defun print-datalog-rule (r &optional (stream t))
  "(PRINT-DATALOG-RULE R STREAM) prints the DATALOG version of RULE."
  (cond ((atom r) (print-datalog-lit r stream) (format stream "."))
        ((and (eq (car r) '<=) (null (cddr r)))
         (print-datalog-lit (cadr r) stream) (format stream "."))
        ((eq (car r) '<=)
         (print-datalog-disj (cadr r) stream)
         (format stream " :- ")
         (print-datalog-lit (caddr r) stream)
         (dolist (l (cdddr r))
           (format stream ", ")
           (print-datalog-lit l stream))
         (format stream "."))
        (t (print-datalog-lit r stream) (format stream "."))))

(defun print-datalog-disj (disj &optional (stream t))
  (cond ((atom disj) (print-datalog-lit disj stream))
        ((eq (car disj) 'or)
         (print-datalog-lit (second disj) stream)
         (dolist (l (cddr disj))
           (format stream " v ")
           (print-datalog-lit l stream)))
        (t (print-datalog-lit disj stream))))

(defun print-datalog-lit (lit &optional (stream t))
  "(PRINT-DATALOG-LIT LIT) takes a kif literal LIT and prints the 
   datalog equivalent."
  (cond ((and (listp lit) (or (eq (car lit) 'not)
			      (eq (car lit) 'naf))) 
	 (print-datalog-not lit stream))
        (t (print-datalog-atom lit stream))))

(defun print-datalog-not (lit &optional (stream t))
  "(PRINT-DATALOG-NOT LIT) takes a kif negative literal LIT and prints the
   datalog equivalent."
  (cond ((eq (car lit) 'naf)
	 (format stream "not ")
	 (print-datalog-lit (cadr lit) stream))
	((eq *datalognot* 'notparens)
         (format stream "~A(" *datalognot*)
         (print-datalog-lit (cadr lit) stream)
         (format stream ")"))
        ((eq *datalognot* 'not)
         (format stream "~A " *datalognot*)
         (print-datalog-lit (cadr lit) stream))
        (t
         (format stream "~A" *datalognot*)
         (print-datalog-lit (cadr lit) stream))))

(defun print-datalog-atom (atom &optional (stream t))
  "(PRINT-DATALOG-ATOM ATOM) takes a kif atom/term and prints the datalog equivalent."
  (cond
        ((eq atom '<=) (format stream ":-"))
        ((varp atom) (format stream "~:@(~A~)" 
			     (datalog-variable (devariable atom))))
        ((atom atom) (format stream "~(~A~)" atom))
        ((or (eq (car atom) '=) (eq (car atom) 'same))
         (print-datalog-atom (cadr atom) stream)
         (format stream "=")
         (print-datalog-atom (caddr atom) stream))

        (t
         (cond ((null (cdr atom)) (format stream "~A()" (car atom)))
               (t
                (format stream "~(~A~)(" (car atom))
                (print-datalog-atom (cadr atom) stream)
                (dolist (te (cddr atom))
                  (format stream ",")
                  (print-datalog-atom te stream))
                (format stream ")"))))))

(defun datalog-variable (v)
  (if (numberp v) (tosymbol (list 'v v)) v))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Javascript ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun kifrule2javascript (p &optional (stream t))
  (cond ((atom p) (print-datalog-lit p stream))
        ((and (eq (car p) '<=) (null (cddr p)))
         (print-datalog-lit (second p) stream))
        ((eq (car p) '<=)
	 (kif2javascript (second p) stream) 
         (format stream " ~A " (javascriptoperator '<=))
         (kif2javascript (third p) stream)
         (dolist (l (cdddr p))
           (format stream " ~A " (javascriptoperator 'and))
           (print-datalog-lit l stream)))
	(t (print-datalog-lit p stream))))

(defun kif2javascript (p &optional (stream t)) 
  "(KIF2JAVASCRIPT P STREAM) prints the JAVASCRIPT version of P."
  (cond ((atom p) (print-datalog-lit p stream))
        ((and (or (eq (car p) '<=) (eq (car p) '=>)) (null (cddr p)))
         (print-datalog-lit (second p) stream))

        ((eq (car p) '<=)
	 (format stream "(")
	 (kif2javascript (second p) stream) 
         (format stream " ~A " (javascriptoperator '<=))
         (kif2javascript (third p) stream)
         (dolist (l (cdddr p))
           (format stream " ~A " (javascriptoperator 'and))
           (kif2javascript l stream))
	 (format stream ")"))

	((eq (car p) '=>)
	 (format stream "(")
	 (do ((ls (cdr p) (cdr ls)))
	     ((null (cddr ls)) (progn (kif2javascript (car ls) stream)
				      (format stream " ~A " (javascriptoperator '=>))
				      (kif2javascript (cadr ls) stream)))
	   (kif2javascript (car ls) stream)
	   (format stream " ~A " (javascriptoperator 'and)))
	 (format stream ")"))

	((eq (car p) '<=>)
	 (format stream "(")
	 (kif2javascript (second p) stream)
	 (format stream " ~A " (javascriptoperator '<=>))
	 (kif2javascript (third p) stream))
	
	((member (car p) '(and or))
	 (format stream "(")
	 (kif2javascript (second p) stream)
	 (dolist (q (cddr p))
	   (format stream " ~A " (javascriptoperator (car p)))
	   (kif2javascript q stream))
	 (format stream ")"))

	((eq (car p) 'not) 
	 (format stream "~A" (javascriptoperator 'not))
	 (kif2javascript (second p) stream))

	((member (car p) '(forall exists))
	 (format stream "(")
	 (if (atom (second p))
	     (format stream "~A ~A: " (javascriptoperator (car p)) (datalog-variable (devariable (second p))))
	     (dolist (v (second p))
	       (format stream "~A ~A:" (javascriptoperator (car p)) (datalog-variable (devariable v)))))
	 (kif2javascript (third p) stream)
	 (format stream ")"))

        (t (print-datalog-lit p stream))))

(defun javascriptoperator (o)
  (case o
    (forall "A")
    (exists "E")
    (or "|")
    (and "&")
    (<= ":-")
    (=> "=>")
    (<=> "<=>")
    (not "~")))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; HTML ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar *keywords* nil 
  "The constants given their own spans for color coding via CSS.")

(defun folstyle ()
  "
	div.sentencewrap {
		position: relative;
	}
 	div.sentence {
		position: relative;
		clear: left;
	}
	div.atom {
		position: relative;
		margin-left: 4px;
	}
	div.head {
		position: relative;
		float: left;
		margin-left: 4px;
	}
	div.body {
		position: relative;
		float: left;
		margin-left: 4px;
	}
    ")

(defun kif2html (fol &optional (s t))
  "(KIF2HTML (FOL S) translates the kif rule FOL into HTML to S."
  (let ((*real-ops* '(forall exists <= => <=> and or not)))
    (fol2html fol s 0)))

(defun fol2html (fol s depth)
  "(FOL2HTML FOL S DEPTH) outputs FOL prettily in HTML to S, starting in the 
   context of DEPTH sentences deep, i.e. DEPTH is the number of parentheses 
   that need to be added at the end of the call."
  (cond ((atom fol)
         (format s "<div class=\"sentence\"><div class=\"head\">")
         (atomic2html fol s)
         (maptimes #'(lambda () (format s ")")) depth)
         (format s "</div></div>")
         (crlf s))

        ((and (find (car fol) '(<= => and or <=>)) (find (car fol) *real-ops*))

         ; wrapper
         (format s "<div class=\"sentence\">")

         ; make the operator the head of the sentence
         (format s "<div class=\"head\">(")
         (operator2html (car fol) s)
         (format s "</div>")

         ; include each of the operands in the body
         (format s "<div class=\"body\">")
         (fols2html (cdr fol) s (1+ depth))
         (format s "</div>")

         ; end the sentence
         (format s "</div>") (crlf s))


        ((and (find (car fol) '(forall exists)) (find (car fol) *real-ops*))
         ; wrapper
         (format s "<div class=\"sentence\">")

         ; make the operator the head of the sentence
         (format s "<div class=\"head\">(")
         (operator2html (car fol) s)
         (format s " ")
         (vars2html (cadr fol) s)
         (format s "</div>")

         ; make the rest of the rule into the body.
         (format s "<div class=\"body\">")
         (fol2html (caddr fol) s (1+ depth))
         (format s "</div>")

         ; end the sentence
         (format s "</div>") (crlf s))

        (t (format s "<div class=\"sentence\"><div class=\"head\">")
           (atomic2html fol s)
           (maptimes #'(lambda () (format s ")")) depth)
           (format s "</div></div>"))))

(defun fols2html (ps s depth)
  "(FOLS2HTML PS S DEPTH) call fol2html for all but the last element in PS 
   with depth 0, and then call fol2html on the last element with depth DEPTH. 
   This puts the appropriate number of parentheses after the last element."
  (cond ((not (cdr ps)) (fol2html (car ps) s depth))
        (t
         (do ((p ps (cdr p)))
             ((not (cdr p)) (fol2html (car p) s depth))
           (fol2html (car p) s 0)))))

(defun vars2html (vs s)
  "(VARS2HTML VS S) outputs a list of variables to HTML."
  (cond ((atom vs) (atomic2html vs s))
        (t
         (format s "(")
         (atomic2html (car vs) s)
         (mapc #'(lambda (x) (format s " ") (atomic2html x s)) (cdr vs))
         (format s ")"))))

(defun operator2html (op s)
  "(OPERATOR2HTML OP S) outputs an operator to HTML."
  (let ((hop (cdr (assoc op *html-connectives*))))
    (when (not hop) (setq hop op))
    (format s "<span class=\"operator\">~(~A~)</span>" hop)))

(defun atomic2html (a s)
  "(ATOMIC2HTML A S) outputs atoms and terms to html."
  (cond ((atom a) 
         (let ((span (lookup-span a)))
           (if span
             (format s "<span class=\"~A\">~(~A~)</span>" span a)
             (format s "~(~A~)" a))
           ))
        (t
         (format s "(")
         (atomic2html (car a) s)
         (mapc #'(lambda (x) (format s " ") (atomic2html x s)) (cdr a))
         (format s ")")
         )))

(defun lookup-span (a)
  "(LOOKUP-SPAN A) returns the span name for the vocabulary element A.  Handles
   variables as well."
  (cond ((varp a) 'var)
        ((listp a) nil)
        ((find a *keywords*) a)
        (t nil)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Configit ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; Configit is a company with an object-oriented language for specifying large
;   product configuration problems.

#|
(yacc:parse-with-lexer (dso-lex:lex-slow '*configitlexer* "Gender : [
  \"Male\",
  \"Female\"
];" '(whitespace)) *configitparser*)
|#

(dso-lex:deflexer *configitlexer* (:priority-only t)
  ("\\\[" lbracket)
  ("\\\]" rbracket)
  (":" colon)
  ("\"[a-zA-Z0-9_ ]*\"" string drop-quotes)
  ("," comma)
  (";" semicolon)
  ("[a-zA-Z0-9_]+" symbol tosymbol)
  ("\\s+" whitespace))

(yacc:define-parser *configitparser*
  (:start-symbol type)
  (:terminals (lbracket rbracket colon string comma semicolon symbol))

  (type (symbol colon lbracket commalist rbracket semicolon 
		#'(lambda (symbol colon lbracket list rbracket semicolon)
		    (declare (ignore colon lbracket rbracket semicolon))
		    `(type ,(tosymbol symbol) ,list))))

  (commalist (term #'(lambda (x) (list x)))
	     (term comma commalist #'(lambda (term comma commalist) 
				       (declare (ignore comma)) 
				       (cons term commalist))))

  (term (symbol #'tosymbol) string ))




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
