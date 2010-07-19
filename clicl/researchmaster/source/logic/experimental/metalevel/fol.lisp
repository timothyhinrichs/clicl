; 1.75 hours to implement correct solution without any refinements
;   (even ICE)
(in-package :tlh)

(export 'resfind)

(defvar *tracemessages* nil
   "Whether to print out debugging info: T means to print out info.")
(defvar *traceexpressions* nil
  "List of expressions to trace.")
(defvar *trace-device* t
  "Device to output tracing to.")

(defvar *inferences* 
  "Number of successful applications of an inference rule.")

(defun trace-expression ()
  (setq *traceexpressions* t)
)
(defun tracerun ()
   "(TRACERUN) turns tracing on for resolution."
   (trace resolution resolution-aux resolve)
)
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
    `(define-theory ,th ,(car stuff) ,(cdr stuff))
    `(define-theory ,th "" ,stuff))
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


#| ------------------------- Resolution -------------------------- |#

(defun resfind (sos th)
   "(RESFIND SOS TH) runs resolution on the theory TH using SOS."
   (resolution sos (contents th))
)

(defun resolution(sos background)
   "(RESOLUTION SOS BACKGROUND) returns T if BACKGROUND U SOS are inconsistent.  
    Else returns nil."
   
   (reset-clause-number)
   (let ((b nil) (s nil))

     ; number background
     (do ((cs (strip-ors background) (cdr cs)))
         ((null cs))
       (setq b (cons (make-clause :disjunction (car cs)
                                  :number (new-clause-number)
                                  :rule 'given)
                     b)))
     ; number SOS
     (do ((cs (strip-ors sos) (cdr cs)))
         ((null cs))
       (setq s (cons (make-clause :disjunction (car cs)
                                  :number (new-clause-number)
                                  :rule 'given)
                     s)))

     (setq b (nreverse b))
     (setq s (nreverse s))
     
     (restrace-newlevel 0)
     (mapcar #'restrace b)
     (restrace-newlevel 0)
     (mapcar #'restrace s)
     (resolution-aux s b))
)

(defun resolution-aux(sos background)
   "(RESOLUTION-AUX SOS BACKGROUND) returns T if clauses SOS U BACKGROUND are inconsistent.
   Else returns Nil.  It is the workhorse of
   the resolution routine."
  
  (let ((newresolvents nil) ; list of newly produced resolvents.
        (info nil)
        ;(lastback (cdr (last background))) ; pointer to the end of the background
        (lastsos (cdr (last sos)))         ; pointer to the end of the sos
        (lastnew nil)                      ; pointer to the end of the newresolvents
        (allstop nil)
        (unsat nil) (result nil))

    (setf (cdr (last background)) sos)  ; quick append the sos to the end of background
    (do ((level 1 (1+ level)))
        (allstop result)
      
      ; output start of new level
      (restrace-newlevel level)
      
      (do ((outer background (cdr outer)))  ; two finger method
          ((or (null outer) allstop))
        (do ((inner sos (cdr inner))
             (resolvents))
            ((or (null inner) allstop))
          
          ; compute all resolvents
          (setq resolvents (resolve-with-factors (car outer) (car inner)))
          
          ; filter out using deletion strategies
          (setq info (deletion-strategy resolvents newresolvents background))
          (setq resolvents (clausesetinfo-clauses info))
          
          ; output these new clauses
          (mapcar #'restrace resolvents)
          
          ; set termination if necessary
          (setq unsat (clausesetinfo-unsat info))
          (when unsat (setq allstop t) (return))
          
          
          ; update list of newly produced resolvents and
          ;    the location of the last element in that list.
          (cond ((null newresolvents)
                 (setq newresolvents resolvents)
                 (setq lastnew (cdr (last resolvents))) )
                (t 
                 (setf lastnew resolvents)
                 (setq lastnew (last lastnew)) )) ))
      
      
      ; if found empty clause, we're done.  If search space empty,
      ;   we're done.  Else iterate again with a different clause set.
      (cond (unsat (setq result t) (setq allstop t))
            ((null newresolvents) (setq result nil) (setq allstop t)) 
            (t (setf lastsos newresolvents)
               (setq lastsos (cdr (last lastsos)))
               (setq newresolvents nil)
               (setq lastnew nil)
               ))))
)

(defstruct clausesetinfo
  (clauses nil)  ; set of clauses
  (unsat nil)   ; if ice finds two literals that are complimentary
)

(defun empty-clause (c)
   "(EMPTY-CLAUSE C) returns T if c is an empty clause.  We assume
    c has had its OR stripped."
   (null c)
) 

(defun resolve-with-factors (c1 c2)
   "(RESOLVE-WITH-FACTORS C1 C2) returns the list of all 
   resolvents of all factors of c1 and c2."
   (let ((factors1 (all-factors c1))
         (factors2 (all-factors c2))
         (allresolvents nil))
      (dolist (f1 factors1)
        (dolist (f2 factors2)
           (setq allresolvents (union allresolvents
                                      (resolve-full f1 f2)
                                      :test #'equalp)) )) 
      allresolvents)
)
(defun all-factors (clause)
   "(ALL-FACTORS CLAUSE) returns a list with all the factors of
    clause including itself."
   (let ((result (list clause))
         (subsets nil) (mgu nil))
     (setq subsets (append (subsets (positive-literals (clause-disjunction clause)))
                           (subsets (negative-literals (clause-disjunction clause))) ))
     (dolist (s subsets)
        (setq mgu (mgu-n s))
        (when mgu 
          (setq result 
                (cons (make-clause :disjunction (delete-duplicates (mapcar #'(lambda (x) (plug x mgu)) clause) :test #'equal)
                                   :rule 'factor
                                   :parents (list (clause-number clause))
                                   :number (new-clause-number))
                      result))))
     result )
)

(defun resolve-full (clause1 clause2)
   "(RESOLVE-FULL CLAUSE1 CLAUSE2) returns the list of all first-order resolvents 
    of clauses c1 and c2."
   (let ((c1 (clause-disjunction clause1))
         (c2 (clause-disjunction clause2)))

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
           (setq resolvent (make-clause :disjunction (union contrib1 contrib2 :test #'equal)
                                        :number (new-clause-number)
                                        :rule 'resolution
                                        :parents (list (clause-number clause1) (clause-number clause2)))) 
           (setq resolvents (cons resolvent resolvents)) ) )))
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

#| --------------------- Restriction Strategies ------------------ |#


(defun deletion-strategy (target new old)
  "(DELETION-STRATEGY CLAUSES) returns the result of running our deletion strategies
   on TARGET in light of NEW and OLD."

  ; if there are no resolvents, go to next iteration.
  ; Otherwise, check if the empty clause is included.
  ;    If not add the resolvents to the end of the list.
  (cond  ((null target) (make-clausesetinfo :clauses nil))
         ((some #'(lambda (x) (empty-clause (clause-disjunction x))) target) 
          (make-clausesetinfo :clauses target :unsat t))
         (t

          ; eliminate tautologies
          (setq target (remove-if #'(lambda (x) (tautologyp (clause-disjunction x))) target))
          
          ; eliminate subsumed clauses, checking if the result has complementary literals
          (let ((target2 (subsump-lists target new old)))
            (if (eq target2 t) 
              (make-clausesetinfo :clauses target :unsat t)
              (make-clausesetinfo :clauses target2))) ))
)

(defun tautologyp (clause)
  "(TAUTOLOGYP CLAUSE) returns T iff clause is a tautology."
  (let ((new (sort clause #'string< :key #'relation)))
    (do ((cs new (cdr cs)))
        ((null (cdr cs)) nil)
      (when (and (pos-and-neg (car cs) (cadr cs))
                 (equal (maknot (car cs)) (cadr cs)))
        (return t)) ))
)

(defun subsump-lists (newresolvents resolvents clauses)
 "(SUBSUMP NEWRESOLVENTS RESOLVENTS CLAUSES) returns T if there are 
   complementary, unifiable literals in the union of these 3 sets.  
   Otherwise returns 
   the newresolvents, with the identical clauses removed and all remaining
   literals sorted."

   ; for each newresolvent, check if it should be eliminated.
   (do ((nrs newresolvents (cdr nrs))
        (result nil) (s))
       ((null nrs) result)
     (setq s (subsump-indiv-lists (car nrs) resolvents clauses))
     (cond ((eq s 't) (return t))
           (s (setq result (cons s result)))
           (t nil)))
)
  
(defun subsump-indiv-lists (res res1 res2)
   "(SUBSUMP-INDIV RES RES1 RES2) assumes RES, RES1, and RES2 are all ordered.
   Returns T if RES is complimentary and unifiable with one of the clauses in RES1 or
   RES2.  Returns NIL if RES is subsumed by something in res1 or res2.  Otherwise
   just returns RES."
   (let ((r1 (subsump-indiv-list res res1)))
      (cond ((not r1) nil)
            ((eq r1 t) t)
            (t (subsump-indiv-list res res2)) ))   
)
(defun subsump-indiv-list (new oldlist)
   "(SUBSUMP-INDIV-LIST NEW OLDLIST) returns T if NEW clashes with one of OLDLIST.
    Returns NIL if NEW is subsumed by one of OLDLIST.  Otherwise returns new."
   (do ((os oldlist (cdr os))
        (osdisj)
        (new2 (clause-disjunction new)))
       ((null os) new)

     ; grab the actual disjunction
     (setq osdisj (clause-disjunction (car os)))

     ; complementary unary clauses
     (when (and (= (length new2) 1)         ; unit literal
                (= (length osdisj) 1)    ; unit literal
                (pos-and-neg (car new2) (car osdisj))
                (annihilatep (car new2) (car osdisj)))
           (return t))
     (when (subsump osdisj new2) (return nil)) )
)


(defun subsump (c d)
   "(SUBSUMP C D) returns T iff C subsumes D.  Algorithm description: Chang and Lee."

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
        (bl (make-bindinglist)))
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





#| ------------------------- Logic Manipulation -------------------------- |#

(defun pos-and-neg (lit1 lit2)
  "(POS-AND-NEG LIT1 LIT2) returns T iff lit1 is negative and lit2 is positive or
   vice versa."
  (cond ((atom lit1)
         (cond ((atom lit2) nil)
               ((eq (car lit2) 'not) t)
               (t nil)))
        ((atom lit2)
         (cond ((eq (car lit1) 'not) t)
               (t nil)))
        ((or
          (and (negativep lit1) (not (negativep lit2)))
          (and (negativep lit2) (not (negativep lit1))))
         t)
        (t nil))
)
(defun negativep (lit)
  "(NEGATIVEP LIT) returns T iff lit is negative."
  (cond ((atom lit) nil)
        ((eq (car lit) 'not) t)
        (t nil))
)
(defun relation (lit)
  "(RELATION LIT) returns the relation constant in LIT."
  (cond ((atom lit) lit)
        ((eq (car lit) 'not)
         (relation (cadr lit)))
        (t (car lit)))
)
(defun strip-ors (clauses)
   "(STRIP-ORS CLAUSES) removes all the or's from the CLAUSES."
  (mapcar #'(lambda (x) (if (listp x) (cdr x) x)) clauses)
)

(defun positive-literals (clause)
   "(POSITIVE-LITERALS CLAUSE) returns all the positive literals out
    of CLAUSE."
    (remove-if #'(lambda (x) (and (listp x) (eq (car x) 'not))) clause) 
)
(defun negative-literals (clause)
   "(NEGATIVE-LITERALS CLAUSE) returns all the negative literals out
    of CLAUSE."
    (remove-if-not #'(lambda (x) (and (listp x) (eq (car x) 'not))) clause) 
)
(defun subsets (list)
   "(SUBSETS LIST) returns all subsets of list."
  (cond ((null list) nil) 
        ((= (length list) 1) (list list nil))
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

(defun maknot(lit)
   "(MAKNOT LIT) returns the negated form of LIT."
   (cond ((atom lit) (list 'not lit))
         ((and (listp lit) (eq (car lit) 'not)) (cadr lit))
         ((listp lit) (list 'not lit))
         (t nil))
)


   
#| ------------------------------------ Tracing ------------------------------- |#

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
   
