;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; testing.lisp
;;;    History of tests run on automated reasoning systems.
;;;    Keep tests to ensure reproducibility of paper results. 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;; Comparison of SAT, CSP, Datalog ;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; Originally used for extensional reasoning testing during thesis

;;;;; Grounding + SAT

(defun test-sat-ww ()
  (dolist (x '(14)) ;4 5 6 7 8 9 10 11 12 13 14))
    (with-open-file (s (format nil "gullible:users:thinrich:Apps:minisat:wump-enum-~A.1" x) :direction :output :if-does-not-exist :create :if-exists :supersede)
      (setq start (get-universal-time)) 
      (format t "~&~%~%Starting ~Ax~A at ~A~%" x x start)
      (fhlq2sat '(forall (?x ?y) (=> (possgold ?x) (posswumpus ?y) (westof ?x ?y))) (gen-ww-enum x nil) s)
      (format t "~&Transformation for size ~Ax~A took ~A seconds.~%" x x (- (get-universal-time) start)))))


;;;;;; Datalog (cheating a bit)

(defun test-datalog-ww ()
  (dolist (x '(4 5 6 7 8 9 10 11 12 13 14))
    (setq start (get-universal-time)) 
    (format t "~&~%~%Starting ~Ax~A at ~A~%" x x start)
    (time (setq b (datalog-ww-enum-convert x)))
    (unless (time (datalog-ww-enum-run b)) (format t "Error on ~A~%" x))
    (format t "~&Transformation+solving for size ~Ax~A took ~A seconds.~%" x x (- (get-universal-time) start))))


(defun datalog-ww-enum-convert (n)
  "(DATALOG-WW-ENUM N) generates a WW-enum version of size N, converts the data to
   Datalog form, includes that data with the datalog rules above, and runs viewfindp
   on query."
  (define-theory (make-instance 'prologtheory) "" (fhls2datalog-ww (gen-ww-enum n))))

(defun datalog-ww-enum-run (th)
  (let (res)
    (includes wumpusworld-datalog-stenchshine th)
    (setq res (viewfindp 'query wumpusworld-datalog-stenchshine))
    (unincludes wumpusworld-datalog-stenchshine th)
    (empty th)
    res))

(defun fhls2datalog-ww (ps)
  (mapcan #'fhl2datalog-ww ps))

(defun fhl2datalog-ww (p)
  "(FHL2DATALOG P) converts P into Datalog."
  (cond ((base-defn p) (ground-table p nil nil))
        (t nil)))

#|
cell192  cell188  cell184  cell180  
cell191  cell187  cell183  cell179  
cell190  cell186  cell182  cell178  
cell189  cell185  cell181  cell177  
|#

;;;;; CSP (materialize possgold and posswumpus)
; representing possgold/posswumpus as pos literals
; since query must be negative, we can't write gold ^ wumpus => westof (-gold | -wumpus | westof) 
;    instead, we just negate westof twice, i.e. westof is represented by its complement in the table
;    and we use (not westof) in place of westof in the query.

(defun test-csp-ww ()
  (dolist (x '(12 13 14)) ;'(4 5 6 7 8 9 10 11 12 13 14))
    (with-open-file (s (format nil "gullible:users:thinrich:apps:abscon2006:wump-enum-~A.xml" x) :direction :output :if-does-not-exist :create :if-exists :supersede)
      (setq start (get-universal-time)) 
      (format t "~&~%~%Starting ~Ax~A at ~A~%" x x start)
      (setq a (define-theory (make-instance 'metheory) "" (fhls2boolean (gen-ww-enum x nil)))) 
      (setq start2 (get-universal-time))
      (format t "~&Starting possgold ~A seconds later ..." (- start2 start))
      (print (setq possgold (fullfinds '(possgold ?x) '(possgold ?x) a))) 
      (setq start3 (get-universal-time))
      (format t "~&Starting posswumpus another ~A seconds later ..." (- start3 start2))
      (print (setq posswumpus (fullfinds '(posswumpus ?x) '(posswumpus ?x) a)))
      (setq start4 (get-universal-time))
      (format t "~&Starting westof another ~A seconds later ..." (- start4 start3))
      (print (length (setq nwestof (fullfinds '(westof ?x ?y) '(westof ?x ?y) a))))
      (setq nwestof (mapcar #'maknot nwestof))
      (setq start5 (get-universal-time))
      (format t "~&Starting XCSP writing after another ~A seconds ..." (- start5 start4))
      (fhlq2csp '(forall (?x ?y) (=> (possgold ?x) (posswumpus ?y) (not (westof ?x ?y)))) (nconc possgold posswumpus nwestof) s) 
      (format t "~&Transformation for size ~Ax~A took ~A seconds.~%" x x (- start5 start)))
    (empty a)))


(defun fhls2boolean-ww (ps)
  (let ((dca (compute-dca ps)))
    (mapcan #'(lambda (x) (fhl2boolean-ww x dca)) ps)))

(defun fhl2boolean-ww (p dca)
  (cond ((base-defn p) (ground-table p dca))
        ((eq (car p) '<=>) `((<= ,(second p) ,(nnf (car (ground (third p) dca nil))))))
        (t (list p))))


;;;;; Model building (with mace4)
; two versions: (1) represent tables as pos and neg literals 
;  (2) represent with <=> definitions

(defun test-modelbuilding-ww ()
  (dolist (x '(13)) ;'(4 5 6 7 8 9 10 11 12 13 14))
    (with-open-file (s (format nil "gullible:users:thinrich:apps:ladr-september-2006:wump-enum-~A" x) :direction :output :if-does-not-exist :create :if-exists :supersede)
      (setq start (get-universal-time)) 
      (format t "~&~%~%Starting ~Ax~A at ~A~%" x x start)
      (setq b (gen-ww-enum x))
      (format t "Materializing base tables ...~%")
      (setq b (fhls2extensional b))
      (format t "Writing to file ...~%")
      (kifq2mace '(forall (?x ?y) (=> (possgold ?x) (posswumpus ?y) (westof ?x ?y))) b s)
      (format t "~&Transformation for size ~Ax~A took ~A seconds.~%" x x (- (get-universal-time) start)))
    (sleep 2)))

(defun fhls2extensional-ww (ps)
  (let ((dca (compute-dca ps)))
    (mapcan #'(lambda (x) (fhl2extensional-ww x dca)) ps)))

(defun fhl2extensional-ww (p dca)
  "(FHL2DATALOG P) converts P into Datalog."
  (cond ((base-defn p) (ground-table p dca))
        (t (list p))))

;;;;; Theorem Proving (with Prover9)
; two versions: (1) represent tables as pos and neg literals 
;   (2) represent with <=> definitions

(defun test-prover9-ww ()
  (dolist (x '(4 5 6 7 8 9 10 11 12 13 14))
    (with-open-file (s (format nil "gullible:users:thinrich:apps:ladr-september-2006:wump-enum-~A" x) :direction :output :if-does-not-exist :create :if-exists :supersede)
      (setq start (get-universal-time)) 
      (format t "~&~%~%Starting ~Ax~A at ~A~%" x x start)
      (setq b (gen-ww-enum x))
      (format t "Materializing base tables ...~%")
      (setq b (fhls2extensional b))
      (format t "Writing to file ...~%")
      (kifq2prover9q '(forall (?x ?y) (=> (possgold ?x) (posswumpus ?y) (westof ?x ?y))) b s)
      (format t "~&Transformation for size ~Ax~A took ~A seconds.~%" x x (- (get-universal-time) start)))
    (sleep 2)))


;;;;; Theorem Proving (with Epilog)
; two versions: (1) represent tables as pos and neg literals 
;    (2) represent with <=> definitions

(defun test-epilog-ww ()
  (dolist (x '(4 5 6 7 8 9 10 11 12 13 14))
    (let ((th (make-instance 'theory)))
      (setq start (get-universal-time)) 
      (format t "~&~%~%Starting ~Ax~A at ~A~%" x x start)
      (setq b (fhls2extensional (gen-ww-enum x)))
      (format t "Converting to clausal ...~%")
      (setq b (mapcan #'brfs b))
      (format t "Indexing ...~%")
      (define-theory th "" b)
      (format t "Running fullprovex after ~A seconds of conversion ...~%" (- (get-universal-time) start))
      (fullprovex '(forall (?x ?y) (=> (possgold ?x) (posswumpus ?y) (westof ?x ?y))) b)
      (format t "~&Conversion plus solution for size ~Ax~A took ~A seconds.~%" x x (- (get-universal-time) start)))
    (sleep 2))) 

; More wumpus world
; originally at top of DIMACS section of interlingua

(defun test-sat2-ww ()
  (let ((queries '((westof |cell15| |cell0|) 
		   (westof |cell9515| |cell9483|)
		   (westof |cell10661| |cell10602|) 
		   (westof |cell497| |cell403|) 
		   (westof |cell28147| |cell28010|)
		   (westof |cell38931| |cell38743|)
		   (westof |cell58515| |cell58268|)
		   (westof |cell91734| |cell91420|)
		   (westof |cell145071| |cell144682|))))

  (do ((is '(4 6 8 10 12 14 16 18 20) (cdr is)) 
       (qs queries (cdr qs)) (start))
      ((or (null is) (null qs)))
    (with-open-file (f (format nil "gullible:users:thinrich:research:projects:extensional:logicpuzzles:wumpusworldtests:wump~A.dimacs" (car is)) :direction :output :if-does-not-exist :create :if-exists :supersede)
      (setq start (get-universal-time))
      (format t "~&Converting ~A...~%" (car is))
      (fhlq2sat (car qs) (read-from-string (stringappend "wump" (tostring (car is)))) f)
      (format t "~&Time for ~A: ~A~%" (car is) (- (get-universal-time) start))))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
