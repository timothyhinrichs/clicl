; meta and standard resolution

(in-package :tlh)

(export 'resfind)
(export 'metafind)


(defparameter *csp-relations* '(unifyp varr relation function object =))

(defvar *theory* 'global
  "current theory")

(defvar *tracemessages* nil
   "Whether to print out debugging info: T means to print out info.")
(defvar *traceexpressions* nil
  "List of expressions to trace.")
(defvar *trace-device* t
  "Device to output tracing to.")


(defvar *inferences* 0
  "Number of successful applications of an inference rule.")
(defvar *subsumptions* 0
  "Number of calls to subsumption routine.")

(defvar *current-clause-number* 0
  "current number of clauses" )
(defun new-clause-number ()
  "(NEW-CLAUSE-NUMBER) returns the current clause number and then bumps the counter."
  (setq *current-clause-number* (1+ *current-clause-number*))
  *current-clause-number*
)
(defun reset-clause-number () (setq *current-clause-number* 0))

(defstruct clause 
  (disjunction nil)
  (number nil)
  (rule nil)
  (parents nil)
)
(defstruct clausesetinfo
  (clauses nil)  ; set of clauses
  (unsat nil)   ; if ice finds two literals that are complimentary
)

(defun params ()
  (let ((f "~A:~30T~A~40T~A~%"))
    (format t f "*tautology-elim*" *tautology-elim* "Whether to eliminate tautologies.")
    (format t f "*subsumption*" *subsumption* "Whether to resolve within positive tr literals.")
    (format t f "*factoring*" *factoring* "Whether to use factoring at the baselevel.")
    (format t f "*indexing*" *indexing* "Whether to use indexing for subsumption.")
    (format t f "*attachments*" *attachments* "Whether to use procedural attachments.")
    (format t f "*mrgmgu*" *mrgmgu* "Whether to use Mike's unify for mgu.")
    (format t "~%")
    (format t f "*tr*" *tr* "Whether to resolve within positive tr literals.")
    (format t f "*demote*" *demote* "Whether to demote tr literals to baselevel literals.")
    (format t f "*ground-tr*" *ground-tr* "Whether to metaresolve only when tr literals are fully quoted.")
    (format t f "*complex-empty-clause*" *complex-empty-clause* "Attaching to determine whether clause of cspslast is unsat.")
    (format t f "*tr-disjunctions*" *tr-disjunctions* "Whether to introduce disjunctions when metaunifying.")
    (format t f "*trfirst*" *trfirst* "Whether to order tr first.")
    (format t f "*cspslast*" *cspslast* "Whether to order *csp-relations* last.")
    (format t f "*csp-relations*" *csp-relations* "")
    (format t "~&Note: metafactoring has not been implemented.~%")
    )
  T
)
(defun info ()
  (format t "Set of support, Tautology elimination, Subsumption, Ordering for metalevel constructs~%")
  (format t "   Recursive descent unification, factoring~%")
  (format t "Tim Hinrichs (thinrich@stanford.edu)~%")
  (format t "Version 0.1~%")
  T
)

(defun data ()
  (format t "Unifications: ~A,  Metaunifications: ~A,  Subsumptions: ~A,  Clauses: ~A~%" 
          *unifications* *metaunifications* *subsumptions* *current-clause-number*)
)

#| --------------------- Theory definitions ----------------------- |#

(defun define-theory (th doc sents)
   "(DEFINE-THEORY TH DOC SENTS) stores all of SENTS into the theory
   named TH and returns TH."
   (empty th)
   (mapcar #'(lambda (x) (save x th)) sents)
   (setf (get th 'desc) doc)
   th
)
(defmacro deftheory (th &rest stuff)
  "(DEFTHEORY TH STUFF) calls define-theory."
  (if (stringp (car stuff))
    `(define-theory ',th ,(car stuff) ',(cdr stuff))
    `(define-theory ',th "" ',stuff))
)

(defun empty (th)
   "(EMPTY TH) removes all the sentences from theory TH."
  (setf (get th 'contents) nil)
)
(defun save (sent th)
   "(SAVE SENT TH) saves SENT to the theory named TH."
  (setf (get th 'contents) (cons sent (get th 'contents))) 
)
(defun contents (th)
   "(CONTENTS TH) returns all the sentences in TH."
  (get th 'contents)
)


#| ------------------------- MetaResolution -------------------------- |#


(defun metafind (sos *theory* &optional (maxdisjuncts 1))
  "(METAFIND SOS TH) adds the SOS to TH after enquoting it
   in tr."
  (let ((sos2 (mapcar #'(lambda (x) (list (list 'tr (maksor (mapcar #'(lambda (y) (enquote y)) x))))) sos)))
    (metaresolution sos2 (append (disjunction-skeleton maxdisjuncts) (contents *theory*)))
    )
)
(defun findunsat (sos *theory*)
  (metaresolution sos (contents *theory*))
)

(defun disjunction-skeleton (k)
  "(DISJUNCTION-SKELETON K) produces all disjunctions up to length k
   of -d(x) | tr(x)."
  (assert (>= k 0) nil "Disjunction-skeleton takes positive integers only.")
  (if (= k 0)
    nil
    (do ((i k (1- i))
         (skeletons nil)
         (disjunction nil))
        ((< i 2) (cons `((not (d ?x)) (tr ?x)) skeletons))
      (setq disjunction (maksor (common-lisp-user::maptimes #'newtmpvar i)))
      (setq skeletons (cons (list (maknot `(d ,disjunction)) (maktr disjunction))
                            skeletons)) ))
)

(defun metaresolution (sos background)
  "(METARESOLUTION SOS BACKGROUND) runs metaresolution using SOS as the
   set of support and BACKGROUND as the background clauses.  Returns T
   if the empty clause is found; else returns NIL."

  (reset-clause-number)
  (setq *unifications* 0 *inferences* 0 *metaunifications* 0 *subsumptions* 0)
  (let ((b nil) (s nil) binfo sinfo)
    
    ; number background
    (do ((cs background (cdr cs)))
        ((null cs))
      (setq b (cons (make-clause :disjunction (car cs)
                                 :number (new-clause-number)
                                 :rule 'given)
                    b)))
    ; number SOS
    (do ((cs sos (cdr cs)))
        ((null cs))
      (setq s (cons (make-clause :disjunction (car cs)
                                 :number (new-clause-number)
                                 :rule 'given)
                    s)))
    
    ; put back in order
    (setq b (nreverse b))
    (setq s (nreverse s))

    (clear-index *theory*)

    ; prune and index
    (setq binfo (deletion-strategy b))
    (setq sinfo (deletion-strategy s))

    (setq b (clausesetinfo-clauses binfo))
    (setq s (clausesetinfo-clauses sinfo))
    (setq b (sort b #'< :key #'clause-number))
    (setq s (sort s #'< :key #'clause-number))

    ; trace starting
    (restrace-newlevel 0)
    (mapcar #'restrace b)
    (restrace-newlevel 0)
    (mapcar #'restrace s)
    
    ; run metaresolution
    (when (and (not (clausesetinfo-unsat binfo)) (not (clausesetinfo-unsat sinfo)))
      (metaresolution-aux s b)) )
)

(defun metaresolution-aux-old (sos background)
   "(METARESOLUTION-AUX SOS BACKGROUND) returns T if clauses SOS U BACKGROUND are inconsistent.
   Else returns Nil.  It is the workhorse of the resolution routine."
  
  (let ((newresolvents nil) ; list of newly produced resolvents for a single level.
        (info nil)
        (lastsos (last sos))         ; pointer to the end of the sos
        (lastnew nil)                      ; pointer to the end of the newresolvents
        (allstop nil)
        (unsat nil) (result nil))

    ; append sos to the end of background
    (if background
      (setf (cdr (last background)) sos)  ; quick append the sos to the end of background
      (setq background sos))

    (do ((level 1 (1+ level))
         (new sos))
        (allstop result) 
      
      ; output start of new level
      (restrace-newlevel level)

      (do ((outer background (cdr outer)))  ; two finger method
          ((or (null outer) allstop))
        (do ((inner new (cdr inner))
             (resolvents nil))
            ((or (null inner) allstop))

          ; compute all resolvents
          (setq resolvents (metaresolve-with-factors (car outer) (car inner)))
          
          ; filter out using deletion strategies
          (setq info (deletion-strategy resolvents))
          (setq resolvents (clausesetinfo-clauses info))
          
          ; output these new clauses
          (mapcar #'restrace resolvents)
          
          ; set termination if necessary
          (setq unsat (clausesetinfo-unsat info))
          (when unsat (setq allstop t) (setq result t) (return t))
          
          
          ; update list of newly produced resolvents and
          ;    the location of the last element in that list.
          (cond ((null newresolvents)
                 (setq newresolvents resolvents)
                 (setq lastnew (last newresolvents)) )
                (t 
                 (setf (cdr lastnew) resolvents)
                 (setq lastnew (last lastnew)) )) ))
  
      ; if found empty clause, we're done.  If search space empty,
      ;   we're done.  Else iterate again with a different clause set.
      (cond (unsat (setq result t) (setq allstop t))
            ((null newresolvents) (setq result nil) (setq allstop t)) 
            (t (setf (cdr lastsos) newresolvents)
               (setq lastsos (last lastsos))
               (setq new newresolvents)
               (setq newresolvents nil)
               (setq lastnew nil)
               ))))
)
(defun metaresolution-aux (sos background)
  "(METARESOLUTION-AUX SOS BACKGROUND) returns T if clauses SOS U BACKGROUND are inconsistent.
   Else returns nil. "
  (let ((oldresolvents (make-queue :head sos :tail (last sos)))
        (newresolvents (make-queue))
        (outer (make-queue :head background :tail (last background)))
        (result nil)
        (allstop nil)
        )

    ; until we find unsat or no new resolvents
    (do ((level 1 (1+ level))
         info)
        (allstop result) 

      (restrace-newlevel level)
 
      ; newresolvents will be the next level of computation
      (setq newresolvents (make-queue))
      
      ; resolvents for outer with the last batch of new resolvents
      (setq info (metaresolution-aux-allpairs (queue-head outer) (queue-head oldresolvents)))
      (when (clausesetinfo-unsat info) (return t))
      (queue-append newresolvents (clausesetinfo-clauses info))
      
      ; resolvents for last batch with itself
      (setq info (metaresolution-aux-triangle (queue-head oldresolvents)))
      (when (clausesetinfo-unsat info) (return t))
      (queue-append newresolvents (clausesetinfo-clauses info))
      
      ; if no new resolvents, exit
      (when (null (queue-head newresolvents)) (return nil))
      
      ; update outer and oldresolvents for next iteration
      (queue-append outer (queue-head oldresolvents))
      (setq oldresolvents (make-queue :head (queue-head newresolvents) :tail (queue-tail newresolvents))) ))

)
(defun metaresolution-aux-allpairs (set1 set2)
  "(METARESOLUTION-AUX-ALLPAIRS SET1 SET2) returns all resolvents from the two sets, in the form of clausesetinfo."
  (let ((newresolvents (make-queue)) ; list of newly produced resolvents for a single level.
        info resolvents
        (allstop nil)
        (unsat nil))

    ; two finger method
    (do ((outer set1 (cdr outer)))
        ((or (null outer) allstop))
      (do ((inner set2 (cdr inner)))
          ((or (null inner) allstop))

        ; compute all resolvents
        (setq resolvents (metaresolve-with-factors (car outer) (car inner)))
          
        ; filter out using deletion strategies
        (setq info (deletion-strategy resolvents))
        (setq resolvents (clausesetinfo-clauses info))
          
        ; output these new clauses
        (mapcar #'restrace resolvents)          
          
        ; update list of newly produced resolvents and
        ;    the location of the last element in that list.
        (queue-append newresolvents (clausesetinfo-clauses info))

        ; set termination if necessary
        (setq unsat (clausesetinfo-unsat info))
        (when unsat (setq allstop t)) ))


    ; return the resolvents found, with the appropriate unsat
    (make-clausesetinfo :clauses (queue-head newresolvents) :unsat unsat) )

)
(defun metaresolution-aux-triangle (set)
  "(METARESOLUTION-AUX-TRIANGLE SET) returns all resolvents from all pairs of set, in the form of clausesetinfo,
   computing all pairs using the triangle of n^2."
  (let ((newresolvents (make-queue)) ; list of newly produced resolvents for a single level.
        info resolvents
        (allstop nil)
        (unsat nil))

    ; two finger method
    (do ((outer set (cdr outer)))
        ((or (null outer) allstop))
      (do ((inner outer (cdr inner)))   ; only difference from the above is that inner is init'ed to outer
          ((or (null inner) allstop))

        ; compute all resolvents
        (setq resolvents (metaresolve-with-factors (car outer) (car inner)))
          
        ; filter out using deletion strategies
        (setq info (deletion-strategy resolvents))
        (setq resolvents (clausesetinfo-clauses info))
          
        ; output these new clauses
        (mapcar #'restrace resolvents)          
          
        ; update list of newly produced resolvents and
        ;    the location of the last element in that list.
        (queue-append newresolvents (clausesetinfo-clauses info))

        ; set termination if necessary
        (setq unsat (clausesetinfo-unsat info))
        (when unsat (setq allstop t)) ))


    ; return the resolvents found, with the appropriate unsat
    (make-clausesetinfo :clauses (queue-head newresolvents) :unsat unsat) )
)

(defun empty-clause-full (clause)
  "(EMPTY-CLAUSE-FULL CLAUSE) uses Gamma_M to determine whether CLAUSE is unsat."
  (when (subsetp (mapcar #'relation (clause-disjunction clause)) *csp-relations*)
    (common-lisp-user::fullfindp (maksand (mapcar #'(lambda (x) (maknot (2package x 'common-lisp-user))) 
                                                  (clause-disjunction clause)))
                                 'common-lisp-user::unify))
)
(defun 2package (l package)
  "(2PACKAGE L) changes all the symbols of L into an element of PACKAGE."
  (cond ((atom l) (symbol2package l package))
        (t (mapcar #'(lambda (x) (2package x package)) l)))
)
(defun symbol2package (symbol package)
  "(SYMBOL2PACKAGE SYMBOL) turn SYMBOL into PACKAGE::SYMBOL."
  (read-from-string (concatenate 'string (string package) "::" (string symbol)))
)

(defun notrs (disjunction)
  "(NOTRS DISJUNCTION) returns T iff the list of clauses DISJUNCTION has
   no tr literals."
  (every #'(lambda (x) (not (eq (relation x) 'tr))) disjunction)
)


(defun metaresolve-with-factors (c1 c2)
   "(METARESOLVE-WITH-FACTORS C1 C2) returns the list of all 
   metaresolvents of all factors of c1 and c2."
   (let ((factors1 (all-metafactors c1))
         (factors2 (all-metafactors c2))
         (allresolvents nil) 
         (resolvents nil))

     ; compute all the resolvents of the original clauses
     (setq allresolvents (metaresolve-full-tr c1 c2)) 

     ; compute all the resolvents of factors, adding in the factors themselves to the list of resolvents as necessary
     (dolist (f1 factors1)
       (dolist (f2 factors2)
         (setq resolvents (metaresolve-full-tr f1 f2))
         (when resolvents
           (setq allresolvents (append allresolvents
                                       (cons f1 (cons f2 resolvents)))) ) ))
     
     ; sort by clause number (necessary because of factors computation)
     (sort allresolvents #'< :key #'clause-number ))
)

(defun candidate-literals-func (disjunction)
  "(CANDIDATE-LITERALS-FUNC DISJUNCTION) returns a function which for each literal in 
   DISJUNCTION determines whether it is a candidate for resolution."
  (cond ((notrs disjunction)
         (if *cspslast*
           #'(lambda (x) (not (member x *csp-relations*)))
           #'success))
        (t
          (cond (*trfirst* #'(lambda (x) (eq (relation x) 'tr)))
                (*cspslast* #'(lambda (x) (not (member x *csp-relations*))))
                (t #'success))))
)

(defun all-metafactors (clause)
   "(ALL-FACTORS CLAUSE) returns a list with all the factors of
    clause except itself."

   (if *factoring*
     (let* ((pass (candidate-literals-func (clause-disjunction clause)))
            (groups (group-by-signed-relation (clause-disjunction clause) :applytest pass))
            (result nil)
            (subsets nil)
            (mgu nil))
       
       ; compute interesting subsets that might unify
       (dolist (g groups)
         ; only compute subsets for groups of at least two elements
         (when (cdr g) (setq subsets (union subsets (subsets g) :test #'equal))))
       
       ; compute whether there is a unifier for each of those interesting subsets
       (dolist (s subsets)
         (setq mgu (mgu-n s))
         (when mgu 
           (setq result 
                 (cons (make-clause :disjunction (delete-duplicates (mapcar #'(lambda (x) (plug x mgu)) (clause-disjunction clause)) :test #'equal)
                                    :rule 'factor
                                    :parents (list (clause-number clause))
                                    :number (new-clause-number))
                       result))))
       result )
     nil)
)


(defun metaresolve-full (clause1 clause2)
   "(RESOLVE-FULL CLAUSE1 CLAUSE2) returns the list of all first-order resolvents 
    of clauses c1 and c2."

   (let* ((c1 (clause-disjunction clause1))
          (c2 (clause-disjunction clause2))
          (pass1 (candidate-literals-func c1))
          (pass2 (candidate-literals-func c2)))

     (setq c1 (stdize-apart c1 c2))  
     (do ((lit1 c1 (cdr lit1))
          (resolvents nil))
         ((null lit1) resolvents)
       (do ((lit2 c2 (cdr lit2))
            (resolvent) (contrib1) (contrib2) (mgu) )
           ((null lit2))
         
         
         ; when c1 and c2 include complementary, unifiable literals, 
         ;   produce a resolvent.
         (when (and (funcall pass1 (car lit1)) (funcall pass2 (car lit2)))
           (setq mgu (mgu (car lit1) (maknot (car lit2))))
           (when mgu
             (setq contrib1 (plug (remove (car lit1) c1 :test #'equal) mgu))
	     (setq contrib2 (plug (remove (car lit2) c2 :test #'equal) mgu))
             (setq resolvent (make-clause :disjunction (union contrib1 contrib2 :test #'equal)
                                          :number (new-clause-number)
                                          :rule 'resolution
                                          :parents (list (clause-number clause1) (clause-number clause2)))) 
             (setq resolvents (cons resolvent resolvents)) ) ))))
)
(defvar *counter* 0)
(defun metaresolve-full-tr (clause1 clause2)
   "(RESOLVE-FULL-TR CLAUSE1 CLAUSE2) returns the list of all first-order resolvents 
    of clauses c1 and c2, including those from tr literals."

   (let* ((c1 (clause-disjunction clause1))
          (c2 (clause-disjunction clause2))
          (pass1 (candidate-literals-func c1))
          (pass2 (candidate-literals-func c2)))

     (setq c1 (stdize-apart c1 c2))  
     (do ((lit1 c1 (cdr lit1))
          (resolvents nil))
         ((null lit1) resolvents)

       ; normal n^2 computation
       (do ((lit2 c2 (cdr lit2))
            (resolvent) (contrib1) (contrib2) (mgu) mgus )
           ((null lit2))
         
         
         ; when c1 and c2 include complementary, unifiable literals, 
         ;   produce a resolvent.
         (when (and (funcall pass1 (car lit1)) (funcall pass2 (car lit2)))
           (setq mgu (mgu (car lit1) (maknot (car lit2))))
           (when mgu
             (setq contrib1 (plug (remove (car lit1) c1 :test #'equal) mgu))
	     (setq contrib2 (plug (remove (car lit2) c2 :test #'equal) mgu))
             (setq resolvent (make-clause :disjunction (union contrib1 contrib2 :test #'equal)
                                          :number (new-clause-number)
                                          :rule 'resolution
                                          :parents (list (clause-number clause1) (clause-number clause2)))) 
             (setq resolvents (cons resolvent resolvents)) ) )

         (when *tr*
           ; when c1 and c2 are both tr literals, produce resolvents as necessary
           (when (and (listp (car lit1)) (listp (car lit2)) 
                      (eq (caar lit1) 'tr) (eq (caar lit2) 'tr))
             (setq mgus (mgu-tr2-res (cadar lit1) (cadar lit2)))
             
             ; produce a new resolvent for each metaunifier in mgus
             (dolist (m mgus)
               (setq contrib1 (plug (remove (car lit1) c1 :test #'equal) (metaunifier-mgu m)))
               (setq contrib2 (plug (remove (car lit2) c2 :test #'equal) (metaunifier-mgu m)))
               (setq resolvent (make-clause :disjunction (append (union contrib1 contrib2 :test #'equal) (strip-or (metaunifier-disjunction m)))
                                            :number (new-clause-number)
                                            :rule 'metaresolution
                                            :parents (list (clause-number clause1) (clause-number clause2))))
               (setq resolvents (cons resolvent resolvents)))) )))
     )
)
(defun resolve (c1 c2)
   "(RESOLVE C1 C2) returns the list of all first-order resolvents 
    of clauses c1 and c2."

   (setq c1 (stdize-apart c1 c2))  
   (do ((lit1 c1 (cdr lit1))
        (resolvents nil))
       ((null lit1) resolvents)
     (do ((lit2 c2 (cdr lit2))
          (resolvent) (contrib1) (contrib2) (mgu) )
         ((null lit2))
       
       
       ; when c1 and c2 include complementary, unifiable literals, 
       ;   produce a resolvent.
       (setq mgu (mgu (car lit1) (maknot (car lit2)))) 
       (when mgu
         (setq contrib1 (plug (remove (car lit1) c1 :test #'equal) mgu))
         (setq contrib2 (plug (remove (car lit2) c2 :test #'equal) mgu))
         (setq resolvent (union contrib1 contrib2 :test #'equal))
         (setq resolvents (cons resolvent resolvents)) ) ))
)


(defun demote-clause (clause)
  "(DEMOTE-CLAUSE CLAUSE) returns all the demoted clauses resulting from CLAUSE."

  (do ((lit1 (clause-disjunction clause) (cdr lit1))
       (resolvents nil))
      ((null lit1) (if (null resolvents) (list clause) resolvents))

    (when (and (listp (car lit1)) (eq (caar lit1) 'tr) (groundp (car lit1)))
      (setq resolvents (cons (make-clause :disjunction (union (remove (car lit1) (clause-disjunction clause) :test #'equal) 
                                                              (makset (dequote (cadar lit1))) :test #'equal)
                                          :number (new-clause-number)
                                          :rule 'demotion
                                          :parents (list (clause-number clause)) )
                             resolvents))))
)


(defun func-typep (term type)
  "(FUNC-TYPEP) returns T iff TERM is a functional term with function constant TYPE."
  (cond ((atom term) nil)
        (t (eq (car term) type)))
)



(defun empty-clause (c)
   "(EMPTY-CLAUSE C) returns T if c is an empty clause.  We assume
    c has had its OR stripped."
   (null c)
) 


#| --------------------- Restriction Strategies ------------------ |#


(defun deletion-strategy (target)
  "(DELETION-STRATEGY TARGET) returns the result of running our deletion strategies
   on TARGET.  Returns clausesetinfo."

  ; if there are no resolvents, go to next iteration.
  ; Otherwise, check if the empty clause is included.
  ;    If not add the resolvents to the end of the list.
  (cond  ((null target) (make-clausesetinfo :clauses nil))        
         (t

          (let ((result nil))
            ; check whether any of target is the empty clause, signified by allstop
            (do ((s target (cdr s))
                 (allstop nil))
                ((or (null s) allstop) result)
              (when (and *complex-empty-clause* (empty-clause-full (car s)))
                (setq result
                      (make-clausesetinfo :clauses
                                          (append target
                                                  (list (make-clause :number (new-clause-number)
                                                                     :parents (list (clause-number (car s)))
                                                                     :rule 'unification-theory
                                                                     :disjunction nil)))
                                          :unsat t))
                (setq allstop t) (return))
              (when (empty-clause (clause-disjunction (car s)))
                (setq result (make-clausesetinfo :clauses target :unsat t))
                (setq allstop t) (return)))

            ; only if we failed to find the empty clause do we proceed
            (cond (result result)
                  (t                   

                   ; demotion 
                   (when *demote*
                     (setq target (mapcar #'demote-clause target)))

                   ; procedural attachments
                   (when *attachments*
                     (setq target (mapcar #'procedural-attachments target)))

                   ; eliminate tautologies
                   (when *tautology-elim* 
                     (setq target (remove-if #'(lambda (x) (tautologyp (clause-disjunction x))) target)))
                   
                   ; eliminate subsumed clauses, checking if the result has complementary literals
                   (if *subsumption* 
                     (let ((target2 (subsump-index target)))
                       (if (eq (type-of target2) 'clausesetinfo) 
                         (make-clausesetinfo :clauses (append target (clausesetinfo-clauses target2)) :unsat t)
                         (make-clausesetinfo :clauses target2)))
                     (make-clausesetinfo :clauses target)) )) )))
)

(defun procedural-attachments (clause)
  "(PROCEDURAL-ATTACHMENTS CLAUSE) returns a clause after having applied all procedural attachments."

  (setf (clause-disjunction clause) (mapcar-nil #'pa-distinct (clause-disjunction clause)))
  clause
)
(defun mapcar-nil (func l)
  "(MAPCAR-NIL FUNC L) mapcars func to l but then removes all the nils in the result."
  (remove-if #'null (mapcar func l))
) 
(defun pa-distinct (lit)
  "(PA-DISTINCT LIT) returns LIT or NIL, depending on whether lit is -distinct and ground."
  (if (and (listp lit) (eq (car lit) 'not) (listp (cadr lit)) (eq (caadr lit) 'distinct) (groundp lit))
    nil
    lit)
)

(defun tautologyp (clause)
  "(TAUTOLOGYP CLAUSE) returns T iff clause is a tautology."
  (let ((new (sort (copy-list clause) #'string< :key #'relation)))
    (do ((cs new (cdr cs)))
        ((null (cdr cs)) nil)
      (when (and (pos-and-neg (car cs) (cadr cs))
                 (equal (maknot (car cs)) (cadr cs)))
        (return t)) ))
)

(defun subsump-index (newresolvents)
  "(SUBSUMP-INDEX NEWRESOLVENTS) indexes all those newresolvents that are not subsumed.  Returns
   a count of the number indexed or a clausesetinfo if a simple contradiction is detected."

  ; walk over each new resolvent
  (do ((nr newresolvents (cdr nr))
       (chkcomp nil)
       (stop nil)
       (result (make-queue))
       n)
      ((null nr) (if (eq (type-of result) 'queue) (queue-head result) result))
    
    ; checking for complementary literals should only be done if the
    ;   length of this clause is 1.
    (setq n (clause-disjunction (car nr)))
    (setq chkcomp (and (listp n) (null (cdr n))))
    (setq stop nil)
    
    ; walk over all clauses that might subsume
    (do ((ps (lookup-subsumers n *theory*) (cdr ps))
         p)
        ((or (null ps) stop) 'dontcare)

      (setq p (clause-disjunction (car ps)))    ; parent clause
      
      ; check for simple contradiction
      (when (and chkcomp (listp p) (null (cdr p)) 
                 (pos-and-neg (car n) (car p)) (annihilatep (car n) (car p)))
        (setq stop t)
        (setq result (make-clausesetinfo
                      :unsat t
                      :clauses (list (make-clause :disjunction 'nil :number (new-clause-number)
                                  :rule 'resolution
                                  :parents (list (clause-number (car nr)) (clause-number (car ps)))))))
        (index-clause (car (clausesetinfo-clauses result)) *theory*)
        (return))

      ; check for subsumption
      (when (subsump p n)
        (setq stop t)
        (return)))
      
    ; index if not subsumed and put onto queue
    (when (not stop) 
      (index-clause (car nr) *theory*)
      (queue-enqueue result (car nr)))
  )
)


(defun subsump (c d)
   "(SUBSUMP C D) returns T iff C subsumes D.  Algorithm: Chang and Lee."

   (setq *subsumptions* (1+ *subsumptions*))
   (let* ((w (mapcar #'(lambda (x) (list (maknot x))) (ground-new d)))
          (u (list c)))
     (do (nextu)
         ((member nil u) t)
       (do ((us u (cdr us)))
           ((null us))
         (do ((ws w (cdr ws)))
             ((null ws))
           (setq nextu (union nextu (resolve (car ws) (car us)) :test #'equal)) ))
       (when (null nextu) (return nil))
       (setq u nextu)
       (setq nextu nil) ))
)
(defun ground-new (p)
  "(GROUND-NEW P) grounds P with constants not before seen."

  (let ((vs (findvars p))
        (bl (new-bindinglist)))
    (dolist (v vs)
      (bind v (gentemp "a") bl))
    (plug p bl))
)

#|
  (let* ((new1 (stdize-apart clause1 clause2))
         (m (mgu new1 clause2)))
     (cond (m 
            (subsetop (plug new1 m) (plug clause2 m)))
           (t nil)))
|#

(defun annihilatep (lit1 lit2)
   "(ANNIHILATE LIT1 LIT2) returns T iff lit1 and lit2 resolve to
    the empty clause."
   (let ((new1 (stdize-apart lit1 lit2)))
       (mgu (maknot new1) lit2))
)
(defun subsetop (l1 l2)
   "(SUBSETOP L1 L2) returns T iff the ordered set L1 is a subset of L2."
   (subsetp l1 l2)
)





#| ------------------------- Quote Manipulation -------------------------- |#

(defun kwote (p) (list 'quote p))

(defun enquote (lit)
  "(ENQUOTE LIT) turns a simple literal LIT into the quoted version of LIT."
  (cond ((atom lit) (kwote lit))
        ((eq (car lit) 'not) (list 'cons_not (enquote (cadr lit))))
        (t (cons 'listof (mapcar #'enquote lit))))
)

(defun quotedp (x)
  "(QUOTEDP X) returns T iff x is quoted."
  (and (listp x) (eq (car x) 'quote))
)
(defun qatom (x)
  "(QATOM X) takes an expression x and returns T iff x is a quoted atom."
  (and (quotedp x) (atom (cadr x)) (null (cddr x)))
)
(defun unquote (x)
  "(UNQUOTE X) takes an expression x and returns the expression with the first quote
   removed if there is one."
  (if (quotedp x) (cadr x) x)
)
(defun dequote (x)
  "(DEQUOTE X) returns the unquoted version of x, assuming it is fully quoted."
  (cond ((atom x) x)
        ((quotedp x) (unquote x))
        ((eq (car x) 'listof) (mapcar #'dequote (cdr x)))
        ((eq (car x) 'cons_or) (cons 'or (mapcar #'dequote (cdr x))))
        ((eq (car x) 'cons_and) (cons 'and (mapcar #'dequote (cdr x))))
        ((eq (car x) 'cons_not) (cons 'not (mapcar #'dequote (cdr x))))
        ((eq (car x) 'cons_<=) (cons '<= (mapcar #'dequote (cdr x))))
        ((eq (car x) 'cons_=>) (cons '=> (mapcar #'dequote (cdr x))))
        ((eq (car x) 'cons_<=>) (cons '<=> (mapcar #'dequote (cdr x))))
        ((eq (car x) 'cons_forall) (cons 'forall (mapcar #'dequote (cdr x))))
        ((eq (car x) 'cons_exists) (cons 'exists (mapcar #'dequote (cdr x))))
        ((eq (car x) 'or) (cons 'or (mapcar #'dequote (cdr x))))
        (t (assert nil nil "Dequote got something weird.")))
)
(defun qvarp (x)
  "(QVARP X) returns T iff X is a quoted variable."
  (and (qatom x) (varp (cadr x)))
)
(defun qconst (x)
  "(QCONST X) returns T iff X is a quoted constant."
  (and (qatom x) (not (qvarp x)))
)
(defun allquoted (x)
  "(ALLQUOTED X) returns T iff X is an expression representing a totally quoted sentence,
   e.g. '(p a)' is all quoted but '(p a <?x>)' is not all quoted."
  (cond ((atom x) nil)
        ((qatom x) t)
        ((eq (car x) 'listof)
         (every #'allquoted (cdr x)))
        (t nil))
)


(defun subsets (list)
   "(SUBSETS LIST) returns all subsets of list."
  (cond ((null list) nil) 
        ((null (cdr list)) (list list nil))
        (t 
           (let ((result nil) (tmp nil))
              (setq tmp (subsets (cdr list)))
              ; result is all the subsets without the head plus
              ;   all the subsets with the head.
              (setq result (append result
                                   tmp
                                   (mapcar #'(lambda (x) (cons (car list)
                                                               x)) 
                                           tmp))) 
              result))) 
)

(defun group-by-signed-relation (clause &key (ignoretest #'failure) (applytest #'success))
  "(GROUP-BY-SIGNED-RELATION CLAUSE IGNORELIST) returns the literals of clause, grouped by their
   signed relation, ignoring all those signed relations in IGNORELIST."
  (let ((table (make-hash-table :test #'equal :size (length clause)))
        (result nil) sr)

    ; add to bin if we're not ignoring it
    (dolist (c clause)
      (setq sr (signed-relation c))
      (when (and (not (funcall ignoretest sr)) (funcall applytest sr))
        (setf (gethash sr table) 
              (cons c (gethash sr table)) )))

    (with-hash-table-iterator (i table)
      (labels ((try (got-one &optional key value)
                 (declare (ignore key))
                 (when got-one
                   (setq result (cons value result))
                   (multiple-value-call #'try (i)))))
        (multiple-value-call #'try (i))))
    result)
)


   
#| ------------------------------------ Tracing ------------------------------- |#

(defun trace-expression (&rest r)
  (if *traceexpressions*
    (setf (cdr (last *traceexpressions*)) r)
    (setq *traceexpressions* r))
  T
)
(defun untrace-expression (&rest r)
  (if (null r) 
    (setq *traceexpressions* nil)
    (setq *traceexpressions* (remove-if #'(lambda (x) (member x r :test #'equal)) *traceexpressions*)))
)
(defun tracerun ()
   "(TRACERUN) turns tracing on for resolution."
   (trace resolution resolution-aux resolve)
)

(defun restrace-newlevel (level)
  "(RESTRACE-NEWLEVEL LEVEL) outputs a header for the start of a new level."
  (when *traceexpressions* 
    (format *trace-device* "~&Level ~A ~%" level))
)
(defun restrace-level (clauses level)
  "(RESTRACE-LEVEL CLAUSES LEVEL) outputs the set of next clauses."
  (when *traceexpressions* 
    (format *trace-device* "Level ~A ~%" level))
  (mapcar #'restrace clauses)
)
(defun restrace (clause)
  "(RESTRACE CLAUSE) prints out CLAUSE indented for DEPTH."
  (cond ((null *traceexpressions*))
        (t (tracemessage 1 clause)))
)
(defun tracemessage (n clause)
  (fresh-line *trace-device*)
  (tracespaces n) 
  ;(princ clause *trace-device*)
  (format *trace-device* "~A.~8T~A [~@(~A~): ~A]~%" 
          (clause-number clause) (clause-disjunction clause) (clause-rule clause) (clause-parents clause))
)

(defun tracespaces (n)
  (do ((i 1 (1+ i)))
      ((> i n))
      (princ " | " *trace-device*)))



(define-theory 'unsat "" '((or (p ?x) (q ?x)) (or (not (p ?x)) (q ?x)) (or (p ?x) (not (q ?x))) (or (not (p ?x)) (not (q ?x)))) )

(define-theory 'sat "" '((or (p ?x) (q ?x)) (or (not (p ?x)) (q ?x)) (or (p ?x) (not (q ?x)))))

(define-theory 'fact "" '((or (p ?x) (p ?y)) (or (not (p ?u)) (not (p ?v)))))
   
