;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Generators
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(eval-when (compile load eval)
  (proclaim '(special *theory* *names* *functionals*
              alist level tracecalls tracefacts traceexpressions)))

(defvar *reduction* t
 "*REDUCTION* determines whether or not reductions are performed.  The default
  is T.")

(defvar *ancestry* nil
 "*ANCESTRY* determines whether or not various inference routines save and
  check ancestries in processing subgoals.  The default is NIL.")

(defparameter *saves* nil
 "*SAVES* is a variable that determines which literals are saved in forward
  chaining.  If the value of *SAVES* is a list, then a derived literal is saved
  if and only if its primary logical constant, function constant, or relation
  constant is an item on this list.  If the value is anything other than a list,
  all derived literals are saved.")

(defvar *start* 10
 "*START* is the initial depth cutoff for iterative deepening.")

(defvar *increment* 10
 "*INCREMENT* is the amount by which the depth cutoff is incremented on each
  round of iterative deepening.")

(defvar *depth* 1000000
 "*DEPTH* has as value a positive integer indicating the depth of search for
  iterative deepening.  Its default is 1000000.")

(defvar *consistency-depth* 1000000
 "*CONSISTENCY-DEPTH* has as value a positive integer indicating the depth of search 
  when checking consistency.  Its default is 1000000.")

(defvar *termination* nil
 "*TERMINATION* records whether the most recent depth-limited search attempt
  ended because of a depth cutoff.")

(defvar *inferences* 0
 "*INFERENCES* records the number of inference steps in the current inference
  process.")


(defmethod assume (p *theory*)
 "(ASSUME P *THEORY*)
  ASSUME takes a sentence and a theory as arguments.  The sentence is assumed
  to be a literal.  ASSUME uses model elimination to derive conclusions from the
  specified sentence and the sentences in the specified theory and its included
  theories.  The search is bottom-up, depth-first, and statically-ordered and
  uses *DEPTH* as a depth limit.  If it derives a literal with relation constant
  on the list *SAVES*, it saves the literal it derives into the specified theory.
  ASSUME returns T as value."
  (let ((alist (environment)) (level 0) tracecalls)
    (setq *termination* nil)
    (assumedepth p alist 1)
    t))

(defun assumedepth (p al depth)
  (cond ((numgreaterp depth *depth*) (setq *termination* t) nil)
        (t (if traceexpressions (tracesave p al))
           (assumeexp p al depth)
           (if traceexpressions (tracedone p al)))))

(defun assumeexp (p al depth)
  (cond ((atom p) (assumeexpexit p al depth))
        ((eq 'and (car p)) 
         (mapc #'(lambda (x) (assumedepth x al depth)) (cdr p)))
        ((eq 'or (car p))
         (assumeexps '(failure) p (mapcar #'maknot (butlast (cdr p))) al depth))
        (t (assumeexpexit p al depth))))

(defun assumeexpexit (p al depth)
  (setq p (plugstdexp p al))
  (cond ((knownp p *theory* 'samep) nil)
        ((and (listp p) (eq 'execute (car p))) (ignore-errors (eval (cadr p))))
        ((and (listp p) (eq 'evaluate (car p))) (ignore-errors (apply (caadr p) (cdadr p))))
        ((and (savep p) (insert p *theory*) nil))
        (t (assumedb p al depth *theory*))))

(defun savep (p)
  (or (not (listp *saves*)) (member (operator p) *saves* :test #'eq)))

(defun assumedb (p al depth th)
  (cond ((assumeth p al depth th))
        (t (do ((l (includees th) (cdr l)))
               ((null l) nil)
               (assumedb p al depth (car l))))))

(defun assumeth (p al depth th)
  (do ((l (envindexps p al th) (cdr l)) (ol) (bl (environment)) (ans))
      ((null l))
      (when (and (listp (car l)) (eq (caar l) '=>) (not (null (cddar l)))
                 (setq ol (unify (cadar l) bl p al)))
        (if tracefacts (tracefact (car l)))
        (setq ans (assumeexps '(failure) (car l) (butlast (cddar l)) bl depth))
        (backup ol)
        (if ans (return ans)))))

(defmethod envindexps (p al th)
 "(ENVINDEXPS P ENV TH)
  ENVINDEXPS takes an expression, an environment, and a theory as arguments and
  returns a list of sentences in the specified theory containing a component
  that could potentially unify with the specified expression in the specified
  environment.  ENVINDEXPS is used by all inference subroutines.  Users are free
  to redefine ENVINDEXPS to implement specialized subroutines.  The default
  implementation (INDEXPS P TH) ignores the environment."
  (declare (ignore al))
  (indexps p th))

(defun assumeexps (next rule pl al depth)
  (cond ((null pl) (assumedepth (car (last rule)) al (1+ depth))
         (profail nil next))
        ((eq 'cut (car pl)) (assumeexps next rule (cdr pl) al depth) t)
        ((finddepth (car pl) al depth nil
                    next `(assumeexps ,rule ,(cdr pl) ,al ,depth)))))


(defmethod forget (p th)
 "(FORGET P TH)
  FORGET takes a sentence and a theory as arguments.  The sentence is assumed
  to be a literal.  FORGET uses model elimination to derive conclusions from the
  specified sentence and the sentences in the specified theory and its included
  theories.  The search is bottom-up, depth-first, and statically-ordered and
  uses *DEPTH* as a depth limit.  If it derives a literal with relation constant
  on the list *SAVES*, it drops the literal it derives from the specified theory.
  FORGET returns T as value."
  (let ((alist (environment)) (*theory* 'forget) (level 0) tracecalls)
    (setq *termination* nil)
    (decludes 'forget)
    (empty 'forget)
    (includes *theory* th)
    (forgetdepth p alist 1)
    t))

(defun forgetdepth (p al depth)
  (cond ((numgreaterp depth *depth*) (setq *termination* t) nil)
        (t (if traceexpressions (tracedrop p al))
           (forgetexp p al depth)
           (if traceexpressions (tracedone p al)))))

(defun forgetexp (p al depth)
  (cond ((atom p) (forgetexpexit p al depth))
        ((eq 'and (car p)) 
         (mapc #'(lambda (x) (forgetdepth x al depth)) (cdr p)))
        (t (forgetexpexit p al depth))))

(defun forgetexpexit (p al depth)
  (setq p (plugstdexp p al))
  (cond ((truep p *theory* 'samep) nil)
        ((and (savep p) (uninsert p (car (includees *theory*)))
              (insert p *theory*) nil))    ;;; bug here
        (t (forgetdb p al depth *theory*))))

(defun forgetdb (p al depth th)
  (cond ((forgetth p al depth th))
        (t (do ((l (includees th) (cdr l)))
               ((null l) nil)
               (forgetdb p al depth (car l))))))

(defun forgetth (p al depth th)
  (do ((l (envindexps p al th) (cdr l)) (ol) (bl (environment)) (ans))
      ((null l))
      (when (and (listp (car l)) (eq (caar l) '=>) (not (null (cddar l)))
                 (setq ol (unify (cadar l) bl p al)))
        (if tracefacts (tracefact (car l)))
        (setq ans (forgets '(failure) (car l) (butlast (cddar l)) bl depth))
        (backup ol)
        (if ans (return ans)))))

(defun forgets (next rule pl al depth)
  (cond ((null pl) (forgetdepth (car (last rule)) al (1+ depth))
         (profail nil next))
        ((eq 'cut (car pl)) (forgets next rule (cdr pl) al depth) t)
        ((finddepth (car pl) al depth nil
                    next `(forgets ,rule ,(cdr pl) ,al ,depth)))))


(defmethod findp (p th)
 "(FINDP P TH)
  FINDP takes a sentence and a theory as arguments.  It tries to prove the
  sentence from the specified theory and its included theories using model
  elimination.  The search is done in iterative deepening fashion, controlled by
  the variables *START*, *INCREMENT*, and *DEPTH*.  If FINDP is able to prove
  the sentence, it returns T; otherwise, it returns NIL."
  (findx t p th))

(defmethod findx (x p *theory*)
 "(FINDX X P *THEORY*)
  FINDX takes as argument a term, a sentence, and a theory.  It tries to prove
  the specified sentence from the specified theory and its included theories
  using the model elimination.  The search is done in iterative deepening
  fashion, controlled by the variables *START*, *INCREMENT*, and *DEPTH*.  If
  FINDX is able to prove the sentence, it returns a copy of the specified term
  with variables replaced by values obtained during the proof process.  If it
  fails to prove the sentence, the value is NIL."
  (do ((old *depth*) (*depth* (min *start* *depth*) (+ *depth* *increment*))
       (alist (environment)) (level 0) (tracecalls))
      ((> *depth* old) nil)
      (setq *termination* nil *inferences* 0 *unifications* 0)
      (cond ((finddepth p alist 1 nil '(failure) '(identity))
             (return (plugstdexp x alist)))
            ((not *termination*) (return nil)))))

(defmethod finds (x p *theory*)
 "(FINDS X P *THEORY*)
  FINDS takes as argument a term, a sentence, and a theory.  It tries to
  prove the specified sentence from the specified theory and its included
  theories using model elimination.  The search is depth-first and
  statically-ordered and uses *DEPTH* as a depth limit.  If FINDS succeeds
  in proving the sentence, it returns a list of copies of the specified term,
  one for each way the sentence can be proved.  In each copy, the variables are
  replaced by values obtained during the proof process.  If the sentence cannot
  be proved, FINDS returns NIL."
  (let ((alist (environment)) (level 0) tracecalls)
    (setq *termination* nil *inferences* 0 *unifications* 0)
    (do ((next (finddepth p alist 1 nil '(failure) '(identity)) (profail nil next))
         (nl))
        ((null next) (nreverse (uniquify nl)))
        (setq nl (cons (plugstdexp x alist) nl)))))

(defmethod findanswer (s th)
  (decludes 'epitheory)
  (empty 'epitheory)
  (includes 'epitheory th)
  (mapc #'(lambda (x) (save x 'epitheory)) s)
  (findx '(@l) `(answer @l) 'epitheory))

(defmethod findanswers (s th)
  (decludes 'epitheory)
  (empty 'epitheory)
  (includes 'epitheory th)
  (mapc #'(lambda (x) (save x 'epitheory)) s)
  (finds '(@l) `(answer @l) 'epitheory))

(defmethod mapone (rel args th)
 "(MAPONE FUN ARGS TH)"
  (do ((l args (cdr l)) (nl))
      ((null l) (nreverse nl))
      (setq nl (cons (findx '?y (list rel (car l) '?y) th) nl))))

(defmethod mapall (rel args th)
 "(MAPALL FUN ARGS TH)"
  (do ((l args (cdr l)) (nl))
      ((null l) (nreverse nl))
      (setq nl (cons (finds '?y (list rel (car l) '?y) th) nl))))

(defmethod findg (x p th)
 "(FINDG X P *THEORY*)
  FINDG takes as argument a term, a sentence, and a theory.  It returns a
  continuation that on each call tries to prove the specified sentence from the
  specified theory and its included theories.  The search is done in iterative
  deepening fashion, controlled by the variables *START*, *INCREMENT*, and
  *DEPTH*.  If the continuation is able to prove the sentence, it returns a copy
  of the specified term with variables replaced by values obtained during the
  proof process.  After each successful attempt, the continuation can be called
  again to get the next answer.  Once all answers have been enumerated, the
  continuation returns NIL."
  (let* ((al (environment))
         (cont `(procall (finddepth ,p ,al 1 nil (failure) (identity))))
         (tcs) (inferences 0) (unifications 0))
    #'(lambda ()
        (let ((*theory* th) (alist al) (level 0) (tracecalls tcs))
          (setq *termination* nil *inferences* inferences *unifications* unifications)
          (when (setq cont (profail nil cont))
            (setq tcs tracecalls inferences *inferences* unifications *unifications*)
            (plugstdexp x alist))))))


(defun finddepth (p al depth stack next done)
  (setq *inferences* (1+ *inferences*))
  (cond ((numgreaterp depth *depth*) (setq *termination* t) (profail nil next))
        ((and traceexpressions
              (protracecall p al done)
              (setq next `(protracefail ,next ,done))
              (setq done `(protraceexit ,done)) nil))
        (t (findexp p al depth stack next done))))

(defun findexp (p al depth stack next done)
  (cond ((atom p) (findconst p al depth stack next done))
        ((eq 'not (car p)) (findnotexp (cadr p) al depth stack next done))
        ((eq 'and (car p)) (findand p al depth stack next done))
        ((eq 'or (car p)) (findor p al depth stack next done))
	((eq 'same (car p)) (findsame p al next done))
	((eq 'distinct (car p)) (finddistinct p al next done))
	((eq 'oneof (car p)) (findoneof p al next done))
	((eq 'choose (car p)) (findchoose p al next done))
	((eq 'bagofall (car p)) (findbagofall p al next done))
	((eq 'unprovable (car p)) (findunprovable (cadr p) al next done))
	((eq 'ground (car p)) (findground p al next done))
	((eq 'nonground (car p)) (findnonground p al next done))
	((eq 'execute (car p)) (findexecute p al next done))
	((eq 'evaluate (car p)) (findevaluate p al next done))
	((eq 'stringmatch (car p)) (findstringmatch p al next done))
	((get (car p) 'basicval) (findbasicvalue p al depth stack next done))
	((get (car p) 'basic) (findbasic p al depth stack next done))
	(t (findcallfail p al depth stack next done))))

(defun findnotexp (p al depth stack next done)
  (cond ((atom p) (findnotconst p al depth stack next done))
        ((eq 'not (car p)) (finddepth (cadr p) al depth stack next done))
        ((eq 'and (car p)) (findornot p al depth stack next done))
        ((eq 'or (car p)) (findandnot p al depth stack next done))
	((eq 'same (car p)) (profail nil next))
	((eq 'distinct (car p)) (profail nil next))
	((eq 'oneof (car p)) (profail nil next))
	((eq 'choose (car p)) (profail nil next))
	((eq 'bagofall (car p)) (findnotbagofall p al next done))
	((eq 'unprovable (car p)) (finddepth (cadr p) al depth stack next done))
	((eq 'ground (car p)) (profail nil next))
	((eq 'nonground (car p)) (profail nil next))
	((eq 'execute (car p)) (findnotexecute p al next done))
	((eq 'evaluate (car p)) (findnotevaluate p al next done))
	((eq 'stringmatch (car p)) (profail nil next))
        ((get (car p) 'basicval) (findnotbasicvalue p al depth stack next done))
        ((get (car p) 'basic) (findnotbasic p al depth stack next done))
        (t (findcallfail `(not ,p) al depth stack next done))))

(defun findconst (p al depth stack next done)
  (cond ((eq 'true p) (proexit next done))
        ((eq 'cut p) (proexit next done))
        ((eq 'false p) (profail nil next))
        (t (findcallfail p al depth stack next done))))

(defun findnotconst (p al depth stack next done)
  (cond ((eq 'true p) (profail nil next))
        ((eq 'cut p) (profail nil next))
        ((eq 'false p) (proexit next done))
        (t (findcallfail `(not ,p) al depth stack next done))))


(defun findand (p al depth stack next done)
  (cond ((null (cdr p)) (proexit next done))
        (t (findands next (cdr p) al depth stack done))))

(defun findands (ans pl al depth stack done)
  (cond ((null pl) (proexit ans done))
        ((eq 'cut (car pl))
         (findands `(procut ,depth ,ans) (cdr pl) al depth stack done))
        (t (finddepth (car pl) al depth stack
                      ans `(findands ,(cdr pl) ,al ,depth ,stack ,done)))))

(defun procut (ans depth next)
  (cond (ans (profail ans next))
        (t (profail (1- depth) next))))

(defun findandnot (p al depth stack next done)
  (cond ((null (cdr p)) (proexit next done))
        (t (finddepth `(not ,(cadr p)) al depth stack
                      next `(findandnots ,(cddr p) ,al ,depth ,stack ,done)))))

(defun findandnots (ans pl al depth stack done)
  (cond ((null pl) (proexit ans done))
        ((eq 'cut (car pl))
         (findandnots `(procut ,ans ,done) (cdr pl) al depth stack done))
        (t (finddepth `(not ,(car pl)) al depth stack
                      ans `(findandnots ,(cdr pl) ,al ,depth ,stack ,done)))))

(defun findor (p al depth stack next done)
  (findors nil (cdr p) al depth stack next done))

(defun findors (ans pl al depth stack next done)
  (cond ((not (null ans)) (profail ans next))
        ((null pl) (profail nil next))
        (t (finddepth (car pl) al depth stack
                      `(findors ,(cdr pl) ,al ,depth ,stack ,next ,done) done))))

(defun findornot (p al depth stack next done)
  (findornots nil (cdr p) al depth stack next done))

(defun findornots (ans pl al depth stack next done)
  (cond ((not (null ans)) (profail ans next))
        ((null pl) (profail nil next))
        (t (finddepth `(not ,(car pl)) al depth stack
                      `(findornots ,(cdr pl) ,al ,depth ,stack ,next ,done) done))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Same is equals but succeeds only on unifiable terms.
;;; Distinct is true of all pairs of objects
;;; but epilog succeeds only in those cases where the arguments are different
;;; This is why there is no (not (distinct <x> <y>))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun findoneof (p al next done)
  (findoneofnext nil (cadr p) (cddr p) al nil next done))

(defun findoneofnext (ans x xl al ol next done)
  (backup ol)
  (cond ((not (null ans)) (profail ans next))
        ((null xl) (profail nil next))
        ((setq ol (unify x al (car xl) al))
         (proexit `(findoneofnext ,x ,(cdr xl) ,al ,ol ,next ,done) done))
        (t (findoneofnext nil x (cdr xl) al nil next done))))
          

(defun findsame (p al next done)
  (let ((ol))
    (cond ((setq ol (unify (caddr p) al (cadr p) al))
           (proexit `(findvaluenext ,ol ,next) done))
          (t (profail nil next)))))

(defun finddistinct (p al next done)
  (let ((ol))
    (cond ((setq ol (unify (caddr p) al (cadr p) al))
           (backup ol) (profail nil next))
          (t (proexit next done)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Note that both primitive and nonprimitive are true of all objects
;;; but Epilog succeeds only on terms that are primitive or nonprimitive.
;;; This is why (nonprimitive <x>) is not (not (primitive <x>))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun findground (p al next done)
  (if (chkgroundp (cadr p) al) (proexit next done) (profail nil next)))

(defun findnonground (p al next done)
  (if (chkgroundp (cadr p) al) (profail nil next) (proexit next done)))

(defun findprimitive (p al next done)
  (if (chkprimitivep (cadr p) al) (proexit next done) (profail nil next)))

(defun findnonprimitive (p al next done)
  (if (chkprimitivep (cadr p) al) (profail nil next) (proexit next done)))

(defun chkgroundp (x al)
  (cond ((indvarp x)
         (cond ((null (cddr (setq x (getbdg x al)))) nil)
               ((chkgroundp (cadr x) (cddr x)))))
        ((atom x))
	((eq 'quote (car x)))
        (t (chkgroundcdr (cdr x) al))))

(defun chkgroundcdr (x al)
  (do ((l x (cdr l)) (ans))
      ((null l) t)
      (cond ((seqvarp (car l))
             (cond ((null (cddr (setq ans (getbdg (car l) al)))) (return nil))
                   ((chkgroundcdr (cadr ans) (cddr ans)))
                   (t (return nil))))
            ((chkgroundp (car l) al))
            (t (return nil)))))

(defun chkprimitivep (x al)
  (cond ((indvarp x)
         (cond ((null (cddr (setq x (getbdg x al)))))
               ((chkprimitivep (cadr x) (cddr x)))))
        ((atom x) (cond ((numberp x))
                        ((stringp x))
                        ((characterp x))
                        ((eq 'pi x) nil)
                        ((not (listp *names*)))
                        ((member x *names* :test #'eq) t)))
	((eq 'quote (car x)))
        ((eq 'listof (car x)) (chkprimitivepcdr (cdr x) al))
        ((get (car x) 'basicval) nil)
        ((not (listp *functionals*)) (chkprimitivepcdr (cdr x) al))
        ((member (car x) *functionals* :test #'eq)
         (chkprimitivepcdr (cdr x) al))))

(defun chkprimitivepcdr (x al)
  (do ((l x (cdr l)) (ans))
      ((null l) t)
      (cond ((seqvarp (car l))
             (cond ((null (cddr (setq ans (getbdg (car l) al)))))
                   ((chkprimitivepcdr (cadr ans) (cddr ans)))
                   (t (return nil))))
            ((chkprimitivep (car l) al))
            (t (return nil)))))

(defun findunprovable (p al next done)
  (cond ((findp (plugstdexp p al) *theory*) (profail nil next))
        (t (proexit next done))))

(defun findunknown (p al next done)
  (cond ((knownp (plugstdexp p al) *theory*) (profail nil next))
        (t (proexit next done))))

(defun findchoose (p al next done)
  (let (x ol)
    (setq p (plugstdexp p al))
    (setq x (findx (cadr p) (caddr p) *theory*))
    (if (and (not (null x)) (setq ol (unify (cadr p) alist x alist)))
        (proexit `(findvaluenext ,ol ,next) done)
        (profail nil next))))

(defun findbagofall (p al next done)
  (findval `(value ,(butlast p) ,(cadddr p)) al next done))

(defun findnotbagofall (p al next done)
  (findnotvalue `(value ,(butlast p) ,(cadddr p)) al next done))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Not clear whether it should go on to database in case of nongroundprimitive.
;;; basic goes on; value does not.  This is weird.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun findval (p al next done)
  (let ((x (findbasicval (cadr p) al)) (y (caddr p)) (ol))
    (if (and (not (null x)) (setq ol (unify y al x al)))
        (proexit `(findvaluenext ,ol ,next) done)
        (profail nil next))))

(defun findvaluenext (ans ol next)
  (backup ol)
  (profail ans next))

(defun findnotvalue (p al next done)
  (let ((x (findbasicval (cadr p) al)) (y (caddr p)) (ol))
    (cond ((null x) (profail nil next))
          ((setq ol (unify y al x al)) (backup ol) (profail nil next))
          (t (proexit next done)))))

(defun findstringmatch (p al next done)
  (let (pat str dum ol)
    (cond ((and (setq pat (groundplugstdexp (cadr p) al)) (primitivep pat)
                (setq str (groundplugstdexp (caddr p) al)) (primitivep str)
                (car (setq dum (multiple-value-list (stringmatches pat str))))
                (setq ol (unify (cdddr p) al (cdr dum) al)))
               (proexit `(findvaluenext ,ol ,next) done))
          (t (profail nil next)))))

;;; needless inefficiency here.  Just needs to check atom and car.

(defun findbasicval (x al)
  (let (dum)
    (cond ((null (setq dum (plugstdexp x al))) nil)
          ((atom dum) (if (and (groundp dum) (primitivep dum)) dum))
          ((eq 'execute (car dum))
           (multiple-value-setq (x dum)
             (ignore-errors (values (eval (cadr dum)))))
           (unless dum (quotify x)))
          ((eq 'evaluate (car dum))
           (multiple-value-setq (x dum)
             (ignore-errors (values (apply (caadr dum) (cdadr dum)))))
           (unless dum (quotify x)))
          ((eq 'choose (car dum))
           (findx (cadr dum) (caddr dum) *theory*))
          ((eq 'bagofall (car dum))
           (cons 'listof (finds (cadr dum) (caddr dum) *theory*)))
          ((and (get (car dum) 'basicval)
                (every 'primitivep (cdr dum))
                (groundp dum))
           (funcall (get (car dum) 'basicval) dum))
          ((and (groundp dum) (primitivep dum)) dum))))

(defun findexecute (p al next done)
  (let (arg values ol)
    (setq arg (plugstdexp (cadr p) al))
    (cond ((null (cddr p)) (findval `(value ,p 't) al next done))
          ((and (car (setq values (ignore-errors (multiple-value-list (eval arg)))))
                (setq ol (unify (cddr p) al (mapcar #'quotify values) alist)))
           (proexit `(findvaluenext ,ol ,next) done))
          (t (profail nil next)))))

(defun findnotexecute (p al next done)
  (let (arg values ol)
    (setq arg (plugstdexp (cadr p) al))
    (cond ((null (cddr p)) (findnotvalue `(value ,p 't) al next done))
          ((and (car (setq values (ignore-errors (multiple-value-list (eval arg)))))
                (setq ol (unify (cddr p) al (mapcar #'quotify values) alist)))
           (backup ol)
           (profail nil next))
          (t (proexit next done)))))

(defun findevaluate (p al next done)
  (let (arg values ol)
    (setq arg (plugstdexp (cadr p) al))
    (cond ((null (cddr p)) (findval `(value ,p t) al next done))
          ((and (car (setq values (evals arg)))
                (setq ol (unify (cddr p) al values alist)))
           (proexit `(findvaluenext ,ol ,next) done))
          (t (profail nil next)))))

(defun findnotevaluate (p al next done)
  (let (arg values ol)
    (setq arg (plugstdexp (cadr p) al))
    (cond ((null (cddr p)) (findnotvalue `(value ,p t) al next done))
          ((and (car (setq values (ignore-errors (multiple-value-list (apply (car arg) (cdr arg))))))
                (setq ol (unify (cddr p) al values alist)))
           (backup ol)
           (profail nil next))
          (t (proexit next done)))))

(defun findbasic (p al depth stack next done)
  (let (ans)
    (cond ((and (setq ans (groundplugstdexp p al))
                (every #'primitivep (cdr ans)))
           (if (apply (get (car ans) 'basic) (cdr ans))
               (proexit next done)
               (profail nil next)))
          (t (findcallfail p al depth stack next done)))))

(defun findbasicvalue (p al depth stack next done)
  (let ((x (butlast p)) (y (car (last p))) (ol))
    (cond ((and (setq x (groundplugstdexp x al)) (every #'primitivep (cdr x)))
           (setq x (funcall (get (car x) 'basicval) x))
           (if (and (not (null x)) (setq ol (unify y al x al)))
               (proexit `(findvaluenext ,ol ,next) done)
               (profail nil next)))
          (t (findcallfail p al depth stack next done)))))

(defun findnotbasic (p al depth stack next done)
  (let (ans)
    (cond ((and (setq ans (groundplugstdexp p al))
                (every #'primitivep (cdr ans)))
           (if (not (apply (get (car ans) 'basic) (cdr ans)))
               (proexit next done)
               (profail nil next)))
          (t (findcallfail `(not ,p) al depth stack next done)))))

(defun findnotbasicvalue (p al depth stack next done)
  (let ((x (butlast p)) (y (car (last p))) (ol))
    (cond ((and (setq x (groundplugstdexp x al)) (every #'primitivep (cdr x)))
           (setq x (funcall (get (car x) 'basicval) x))
           (cond ((null x) (profail nil next))
                 ((setq ol (unify y al x al)) (backup ol) (profail nil next))
                 (t (proexit next done))))
          (t (findcallfail `(not ,p) al depth stack next done)))))


(defun findcallfail (p al depth stack next done)
  (findrs p al depth stack `(findcallfailnext ,depth ,next) done))

(defun findcallfailnext (ans depth next)
  (cond ((null ans) (profail nil next))
        ((eql ans depth) (profail nil next))
        (t (profail ans next))))

(defun findrs (p al depth stack next done)
  (cond ((and *ancestry* (findancestor p al stack)) (profail nil next))
        (*reduction* ;;; (and *reduction* (or (atom p) (not (eq 'not (car p)))))
         (findrsloop nil p al depth stack stack nil next done))
        (t (finddb p al depth stack *theory* next done))))

(defun findancestor (p al stack)
  (do ((l stack (cdr l)))
      ((null l) nil)
      (if (identify (caar l) (cdar l) p al) (return t))))

(defun findrsloop (ans p al depth stack sl ol next done)
  (backup ol)
  (cond ((not (null ans)) (profail ans next))
        ((null sl) (finddb p al depth stack *theory* next done))
        ((setq ol (unify (maknot (caar sl)) (cdar sl) p al))
         (proexit `(findrsloop ,p ,al ,depth ,stack ,(cdr sl) ,ol ,next ,done)
                  done))
        (t (findrsloop nil p al depth stack (cdr sl) ol next done))))

(defun finddb (p al depth stack th next done)
  (findth p al depth stack th
          `(finddbs ,p ,al ,depth ,stack ,(includees th) ,next ,done) done))

(defun finddbs (ans p al depth stack tl next done)
  (cond ((not (null ans)) (profail ans next))
        ((null tl) (profail nil next))
        (t (finddb p al depth stack (car tl)
                   `(finddbs ,p ,al ,depth ,stack ,(cdr tl) ,next ,done) done))))

(defun findth (p al depth stack th next done)
 (findths nil p al depth stack (envindexps p al th) (environment) nil next done))

;;; ground test in rule case would save work but it is expensive.
;;; note that subset test not good in rule case cause subgoals may bind vars.
;;; we should allow *saves* to work here when caching problem solved.
;;; note that the subolp check is very narrow
;;; note that the subolp action terminates in this theory only!  should cut.

(defun findths (ans p al depth stack fl bl ol next done)
  (backup ol)
  (cond ((not (null ans)) (profail ans next))
        ((null fl) (profail nil next))
        ((and (listp (car fl)) (eq (caar fl) '<=))
         (cond ((setq ol (unify (cadar fl) bl p al))
                (setq next `(findths ,p ,al ,depth ,stack ,(cdr fl) ,bl ,ol ,next ,done))
                (cond (*ancestry* (setq stack (acons p al stack)))
                      (*reduction* ;;; (not (atom p)) (eq 'not (car p)))
                       (setq stack (acons p al stack))))
                (findand (cons 'and (cddar fl)) bl (1+ depth) stack next done))
               (t (findths nil p al depth stack (cdr fl) bl ol next done))))
        ((setq ol (unify (car fl) bl p al))
         (if (not (subolp ol (alist bl)))
             (setq next `(findths ,p ,al ,depth ,stack ,(cdr fl) ,bl ,ol ,next ,done)))
         (proexit next done))
        (t (findths nil p al depth stack (cdr fl) bl nil next done))))

(defun subolp (l1 l2)
  (do ((l l1 (cdr l)))
      ((null (cdr l)) t)
      (if (not (member (car l) l2 :test #'eq)) (return nil))))

(defun proexit (next done)
  (if traceexpressions (setq next `(protraceredo ,next ,done)))
  (if done (apply (car done) next (cdr done)) next))

(defun profail (ans next)
  (if next (apply (car next) ans (cdr next))))

(defun procall (ans next)
  (declare (ignore ans))
  (if next (apply (car next) (cdr next))))

(defun protracecall (p al done)
  (setq p (plugstdexp p al))
  (when (memp p traceexpressions 'instp)
        (tracemessage level '|Call: | p)
        (setq tracecalls (acons done p tracecalls))
        (setq level (1+ level))))

(defun protraceexit (ans done)
  (let (dum)
    (when (setq dum (assoc done tracecalls :test #'eq))
      (setq level (1- level))
      (tracemessage level '|Exit: | (plugstdexp (cdr dum) alist)))
    (proexit ans done)))

(defun protraceredo (ans next done)
  (let (dum)
    (when (setq dum (assoc done tracecalls :test #'eq))
      (tracemessage level '|Redo: | (cdr dum))
      (setq level (1+ level)))
    (profail ans next)))

(defun protracefail (ans next done)
  (let (dum)
    (when (setq dum (assoc done tracecalls :test #'eq))
      (setq tracecalls (cdr tracecalls))
      (setq level (1- level))
      (tracemessage level '|Fail: | (cdr dum)))
    (profail ans next)))


(defparameter *trace-device* t
 "*TRACE-DEVICE* is the device to which inference trace information is
  printed.  The default is T, which directs all inference routines to print
  traces on the terminal.")

(defvar traceexpressions nil)

(defvar tracefacts nil)

(defvar tracecalls nil)

(defmethod trace-expression (&rest l)
 "(TRACE-EXPRESSION &REST l)
  TRACE-EXPRESSION takes any number of expressions as arguments.  It sets up 
  data structures so that various proof procedures print out appropriate
  messages whenever they examine expressions that are instances of the specified
  expressions.  If no arguments are passed to TRACE-EXPRESSION, the result is a 
  list of currently traced expressions.  Otherwise, the value is DONE."
  (cond ((null l) traceexpressions)
	(t (mapc 'traceexp l)
           t)))

(defun traceexp (x)
  (if (not (memp x traceexpressions 'samep))
      (setq traceexpressions (cons x traceexpressions))))

(defmethod untrace-expression (&rest l)
 "(UNTRACE-EXPRESSION &REST l)
  UNTRACE-EXPRESSION takes any number of expressions as arguments.  It eliminates
  the specified expressions from the data structures set up by TRACE-EXPRESSION
  and thus turns off the corresponding tracing.  If no arguments are passed to 
  UNTRACE-EXPRESSION, all traced expressions are deleted, and a list of the those 
  expressions is returned as value.  Otherwise, the value is DONE."
  (cond ((null l) (mapc 'untraceexp traceexpressions))
	(t (mapc 'untraceexp l)))
  t)

(defun untraceexp (x)
  (setq traceexpressions (delone x traceexpressions 'samep)))

(defmethod trace-fact (&rest l)
 "(TRACE-FACT &REST l)
  TRACE-FACT takes any number of expressions as arguments.  It sets up 
  data structures so that various proof procedures print out appropriate
  messages whenever they use database facts that are instances of the specified
  expressions.  If no arguments are passed to TRACE-FACT, the result is a 
  list of currently traced expressions.  Otherwise, the value is DONE."
  (cond ((null l) tracefacts)
	(t (mapc 'tracefactoid l)
           t)))

(defun tracefactoid (x)
  (if (not (memp x tracefacts 'samep))
      (setq tracefacts (cons x tracefacts))))

(defmethod untrace-fact (&rest l)
 "(UNTRACE-FACT &REST l)
  UNTRACE-FACT takes any number of expressions as arguments.  It eliminates
  the specified expressions from the data structures set up by TRACE-FACT
  and thus turns off the corresponding tracing.  If no arguments are passed to 
  UNTRACE-FACT, all traced expressions are deleted, and a list of the those 
  expressions is returned as value.  Otherwise, the value is DONE."
  (cond ((null l) (mapc 'untracefact tracefacts))
	(t (mapc 'untracefact l)))
  t)

(defun untracefact (x)
  (setq tracefacts (delone x tracefacts 'samep)))

(defun tracesave (p al)
  (setq p (plugstdexp p al))
  (when (memp p traceexpressions 'instp)
        (tracemessage level '|Save: | p)
        (setq level (1+ level))))

(defun tracedrop (p al)
  (setq p (plugstdexp p al))
  (when (memp p traceexpressions 'instp)
        (tracemessage level '|Drop: | p)
        (setq level (1+ level))))

(defun tracedone (p al)
  (setq p (plugstdexp p al))
  (when (memp p traceexpressions 'instp)
        (setq level (1- level))
        (tracemessage level '|Done: | p)))

(defun tracefact (x)
  (when (memp x tracefacts 'instp)
        (tracemessage level "Fact: " x)))

(defun tracemessage (n pr p)
  (fresh-line *trace-device*)
  (tracespaces n) (princ pr *trace-device*) (princ p *trace-device*))

(defun tracespaces (n)
  (do ((i 1 (1+ i)))
      ((> i n))
      (princ " | " *trace-device*)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; uniquify
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defparameter *uniquify* 'macintoshway)
(defparameter *uniquify* 'safeway)

(defun uniquificate (ol)
  (uniquify (copy-list ol)))

(defun uniquify (ol)
  (cond ((eq 'safeway *uniquify*) (safeway ol))
        ((eq 'fastway *uniquify*) (fastway ol))
        ((eq 'hash *uniquify*) (hashway ol))
        (*uniquify* (remove-duplicates ol :test #'equalp))
        (t ol)))

(defun safeway (ol)
  (do ((l ol (cdr l)) (i 1 (1+ i)))
      ((null l))
      (rplaca l (cons (car l) i)))
  (setq ol (sort ol #'leqp))
  (do ((l ol))
      ((null (cdr l)) ol)
      (cond ((equalp (caar l) (caadr l)) (rplacd l (cddr l)))
            (t (setq l (cdr l)))))
  (setq ol (sort ol #'< :key #'cdr))
  (do ((l ol (cdr l)))
      ((null l) ol)
      (rplaca l (caar l))))

(defun hashway (ol)
  (cond ((null ol) ol)
        (t (let ((table (make-hash-table :test #'equal :size (length ol))))
             (setf (gethash (car ol) table) t)
             (do ((l ol) )
                 ((null (cdr l)) ol)
                 (cond ((gethash (cadr l) table) (rplacd l (cddr l)))
                       (t (setf (gethash (cadr l) table) t) (setq l (cdr l)))))))))

(defun fastway (ol)
  (setq ol (sort ol #'leqp))
  (do ((l ol))
      ((null (cdr l)) ol)
      (cond ((equalp (car l) (cadr l)) (rplacd l (cddr l)))
            (t (setq l (cdr l))))))

(defun leqp (x y)
  (find (compare x y) '(lt eq)))

(defun geqp (x y)
  (find (compare x y) '(gt eq)))

(defun compare (x y)
  (cond ((atom x)
         (cond ((atom y) (compareatoms x y))
               (t 'lt)))
        ((atom y) 'gt)
        (t (do ((l x (cdr l)) (m y (cdr m)) (dum))
               ((atom l) (if (atom m) (compareatoms l m) 'lt))
               (cond ((null m) (return 'gt))
                     ((eq 'eq (setq dum (compare (car l) (car m)))))
                     (t (return dum)))))))

(defun compareatoms (x y)
  (cond ((equalp x y) 'eq)
        ((realp x) (if (and (realp y) (> x y)) 'gt 'lt))
        ((complexp x)
         (cond ((realp y) 'gt)
               ((complexp y) (compareatoms (realpart x) (realpart y)))
               (t 'lt)))
        ((or (characterp x) (stringp x) (symbolp x))
         (cond ((numberp y) 'gt)
               ((or (characterp y) (stringp y) (symbolp y))
                (if (string-lessp x y) 'lt 'gt))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; reduction
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(eval-when (compile load eval)
  (proclaim '(special *theory* *filter* *test*
                      *start* *increment* *depth* *termination*
                      alist level tracecalls tracefacts traceexpressions)))

(defparameter *consistency* t)

(defmethod reductions (p *theory* &optional (*filter* #'basep) (*test* #'success))
  (let ((alist (environment)) (*reduction*) (level 0) tracecalls)
    (setq *termination* nil *inferences* 0 *unifications* 0)
    (setq p (list p '@l))
    (do ((next (reductiondepth p alist 1 nil '(failure) '(success)) (profail nil next))
         (rule) (nl))
        ((null next) (nreverse nl))
        (setq rule (plugstdexp `(<= ,p . ,(nreverse *reduction*)) alist))
        (if (every #'(lambda (x) (funcall *test* x)) (cddr rule))
            (setq nl (adjoin rule nl :test #'equal))))))

(defmethod nonreductions (p *theory* &optional (*filter* #'basep) (*test* #'success))
  (let ((alist (environment)) (*reduction*) (level 0) tracecalls)
    (setq *termination* nil *inferences* 0 *unifications* 0)
    (setq p `(not ,(list p '@l)))
    (do ((next (reductiondepth p alist 1 nil '(failure) '(success)) (profail nil next))
         (rule) (nl))
        ((null next) (nreverse nl))
        (setq rule (plugstdexp `(<= ,p . ,(nreverse *reduction*)) alist))
        (if (every #'(lambda (x) (funcall *test* x)) (cddr rule))
            (setq nl (adjoin rule nl :test #'equal))))))

(defun reductiondepth (p al depth stack next done)
  (setq *inferences* (1+ *inferences*))
  (cond ((numgreaterp depth *depth*) (setq *termination* t) (profail nil next))
        ((and traceexpressions
              (protracecall p al done)
              (setq next `(protracefail ,next ,done))
              (setq done `(protraceexit ,done)) nil))
        (t (reductionexp p al depth stack next done))))

(defun reductionexp (p al depth stack next done)
  (cond ((atom p) (reductionconst p al depth stack next done))
        ((eq 'not (car p)) (reductionnotexp (cadr p) al depth stack next done))
        ((eq 'and (car p)) (reductionand p al depth stack next done))
        ((eq 'or (car p)) (reductionor p al depth stack next done))
	((eq 'oneof (car p)) (reductionassumption nil p al next done))
	((eq 'same (car p)) (reductionsame p al next done))
	((eq 'distinct (car p)) (reductiondistinct p al next done))
	((eq 'ground (car p)) (reductionground p al next done))
	((eq 'nonground (car p)) (reductionnonground p al next done))
	((eq 'primitive (car p)) (reductionprimitive p al next done))
	((eq 'nonprimitive (car p)) (reductionnonprimitive p al next done))
	((eq '== (car p)) (reductionvalue p al next done))
	((eq 'value (car p)) (reductionvalue p al next done))
	((eq 'execute (car p)) (reductionassumption nil p al next done))
	((eq 'evaluate (car p)) (reductionassumption nil p al next done))
	((eq 'unprovable (car p)) (reductionunprovable p al next done))
	((eq 'bagofall (car p)) (reductionbagofall p al next done))
	((eq 'stringmatch (car p)) (reductionstringmatch p al next done))
	((get (car p) 'basicval) (reductionbasicvalue p al next done))
	((get (car p) 'basic) (reductionbasic p al next done))
	(t (reductioncallfail p al depth stack next done))))

(defun reductionnotexp (p al depth stack next done)
  (cond ((atom p) (reductionnotconst p al depth stack next done))
        ((eq 'not (car p)) (reductiondepth (cadr p) al depth stack next done))
        ((eq 'and (car p)) (reductionornot p al depth stack next done))
        ((eq 'or (car p)) (reductionandnot p al depth stack next done))
	((eq 'oneof (car p)) (profail nil next))
	((eq 'same (car p)) (profail nil next))
	((eq 'distinct (car p)) (profail nil next))
	((eq 'ground (car p)) (profail nil next))
	((eq 'nonground (car p)) (profail nil next))
	((eq 'primitive (car p)) (profail nil next))
	((eq 'nonprimitive (car p)) (profail nil next))
	((eq '== (car p)) (reductionnotvalue p al next done))
	((eq 'value (car p)) (reductionnotvalue p al next done))
	((eq 'execute (car p)) (reductionassumption nil `(not ,p) al next done))
	((eq 'evaluate (car p)) (reductionassumption nil `(not ,p) al next done))
	((eq 'unprovable (car p)) (reductionexp (cadr p) al depth stack next done))
	((eq 'bagofall (car p)) (reductionnotbagofall p al next done))
	((eq 'stringmatch (car p)) (profail nil next))
        ((get (car p) 'basicval) (reductionnotbasicvalue p al next done))
        ((get (car p) 'basic) (reductionnotbasic p al next done))
        (t (reductioncallfail `(not ,p) al depth stack next done))))

(defun reductionconst (p al depth stack next done)
  (cond ((eq 'true p) (proexit next done))
        ((eq 'cut p) (proexit next done))
        ((eq 'false p) (profail nil next))
        (t (reductioncallfail p al depth stack next done))))

(defun reductionnotconst (p al depth stack next done)
  (cond ((eq 'true p) (profail nil next))
        ((eq 'cut p) (profail nil next))
        ((eq 'false p) (proexit next done))
        (t (reductioncallfail `(not ,p) al depth stack next done))))


(defun reductionand (p al depth stack next done)
  (cond ((null (cdr p)) (proexit next done))
        (t (reductionands next (cdr p) al depth stack done))))

(defun reductionands (ans pl al depth stack done)
  (cond ((null pl) (proexit ans done))
        ((eq 'cut (car pl))
         (reductionands `(procut ,depth ,ans) (cdr pl) al depth stack done))
        (t (reductiondepth (car pl) al depth stack
                      ans `(reductionands ,(cdr pl) ,al ,depth ,stack ,done)))))

(defun reductionandnot (p al depth stack next done)
  (cond ((null (cdr p)) (proexit next done))
        (t (reductiondepth `(not ,(cadr p)) al depth stack
                      next `(reductionandnots ,(cddr p) ,al ,depth ,stack ,done)))))

(defun reductionandnots (ans pl al depth stack done)
  (cond ((null pl) (proexit ans done))
        ((eq 'cut (car pl))
         (reductionandnots `(procut ,ans ,done) (cdr pl) al depth stack done))
        (t (reductiondepth `(not ,(car pl)) al depth stack
                      ans `(reductionandnots ,(cdr pl) ,al ,depth ,stack ,done)))))

(defun reductionor (p al depth stack next done)
  (reductionors nil (cdr p) al depth stack next done))

(defun reductionors (ans pl al depth stack next done)
  (cond ((not (null ans)) (profail ans next))
        ((null pl) (profail nil next))
        (t (reductiondepth (car pl) al depth stack
                      `(reductionors ,(cdr pl) ,al ,depth ,stack ,next ,done) done))))

(defun reductionornot (p al depth stack next done)
  (reductionornots nil (cdr p) al depth stack next done))

(defun reductionornots (ans pl al depth stack next done)
  (cond ((not (null ans)) (profail ans next))
        ((null pl) (profail nil next))
        (t (reductiondepth `(not ,(car pl)) al depth stack
                      `(reductionornots ,(cdr pl) ,al ,depth ,stack ,next ,done) done))))

(defun reductionassumption (ans p al next done)
  (cond (ans (profail next done))
        ((setq p (assumablep p al))
         (reductionassume p next done))
        (t (profail nil next))))

(defun reductionassume (p next done)
  (cond ((or (not *consistency*)
             (not (or (rebuttalp p *reduction*) (rebuttheoryp p *theory*)))
             (consistentp p *reduction*))
         (setq *reduction* (cons p *reduction*))
         (proexit `(reductionnext ,next) done))
        (t (profail nil next))))

(defun reductionnext (ans next)
  (setq *reduction* (cdr *reduction*))
  (profail ans next))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Note that both primitive and nonprimitive are true of all objects
;;; but Epilog succeeds only on terms that are primitive or nonprimitive.
;;; This is why (nonprimitive <x>) is not (not (primitive <x>))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun reductionground (p al next done)
  (cond ((chkgroundp (cadr p) al) (proexit next done))
        (t (reductionassumption nil p al next done))))

(defun reductionnonground (p al next done)
  (cond ((chkgroundp (cadr p) al) (profail nil next))
        (t (reductionassumption nil p al next done))))

(defun reductionprimitive (p al next done)
  (cond ((chkprimitivep (cadr p) al)
         (if (chkgroundp (cadr p) al) (proexit next done)
             (reductionassumption nil p al next done)))
        (t (profail nil next))))

(defun reductionnonprimitive (p al next done)
  (cond ((chkprimitivep (cadr p) al) (profail nil next))
        (t (proexit next done))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Same is equals but succeeds only on unifiable terms.
;;; Distinct is true of all pairs of objects
;;; but epilog succeeds only in those cases where the arguments are different
;;; This is why there is no (not (distinct <x> <y>))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun reductionsame (p al next done)
  (let ((ol))
    (cond ((setq ol (unify (caddr p) al (cadr p) al))
           (proexit `(reductionvaluenext ,ol ,next) done))
          (t (profail nil next)))))

(defun reductiondistinct (p al next done)
  (let ((ol))
    (cond ((setq ol (unify (caddr p) al (cadr p) al))
           (backup ol)
           (if (cdr ol) (reductionassumption nil p al next done)
               (profail nil next)))
          (t (proexit next done)))))


(defun reductionvalue (p al next done)
  (let ((x (reductionbasicval (cadr p) al)) (y (caddr p)) (ol))
    (cond ((or (null x) (not (chkprimitivep y al)))
           (reductionassumption nil p al next done))
          ((setq ol (unify y al x al))
           (proexit `(reductionvaluenext ,ol ,next) done))
          (t (profail nil next)))))

(defun reductionvaluenext (ans ol next)
  (backup ol)
  (profail ans next))

(defun reductionnotvalue (p al next done)
  (let ((x (reductionbasicval (cadr p) al)) (y (caddr p)) (ol))
    (cond ((null x) (reductionassumption nil `(not ,p) al next done))
          ((setq ol (unify y al x al)) (backup ol) (profail nil next))
          (t (proexit next done)))))

;;; needless inefficiency here.  Just needs to check atom and car.

(defun reductionbasicval (x al)
  (setq x (plugstdexp x al))
  (cond ((atom x) (if (and (groundp x) (primitivep x)) x))
        ((eq 'execute (car x)) nil)
        ((eq 'evaluate (car x)) nil)
        ((and (get (car x) 'basicval)
              (groundp x)
              (every #'primitivep (cdr x)))
         (funcall (get (car x) 'basicval) x))
        ((and (groundp x) (primitivep x)) x)))

(defun reductionunprovable (p al next done)
  (let (rl)
    (setq p (plugstdexp (cadr p) al))
    (setq rl (residues t p *theory* *filter* *test*))
    (cond ((null rl) (proexit next done))
          ((eq 'true (maksor rl)) (profail nil next))
          (t (reductionassume `(unprovable ,(maksor rl)) next done)))))

(defun reductionbagofall (p al next done)
  (let (rl)
    (setq p (plugstdexp p al))
    (setq rl (residues (cadr p) (caddr p) *theory* *filter* *test*))
    (cond ((null rl) (profail nil next))
          ((eq 'true (maksor rl)) (proexit next done))
          (t  (setq *reduction* (cons `(bagofall ,(cadr p) ,(maksor rl) ,(cadddr p)) *reduction*))
              (proexit `(reductionnext ,next) done)))))

(defun reductionnotbagofall (p al next done)
  (let (rl)
    (setq p (plugstdexp p al))
    (setq rl (residues (cadr p) (caddr p) *theory* *filter* *test*))
    (cond ((null rl) (profail nil next))
          ((eq 'true (maksor rl)) (proexit next done))
          (t (reductionassume `(not (bagofall ,(cadr p) ,(maksor rl) ,(cadddr p))) next done)))))

(defun reductionstringmatch (p al next done)
  (let (pat str dum ol)
    (cond ((and (setq pat (groundplugstdexp (cadr p) al)) (primitivep pat)
                (setq str (groundplugstdexp (caddr p) al)) (primitivep str))
           (if (and (car (setq dum (multiple-value-list (stringmatches pat str))))
                    (setq ol (unify (cdddr p) al (cdr dum) al)))
               (proexit `(findvaluenext ,ol ,next) done)
               (profail nil next)))
          (t (reductionassumption nil p al next done)))))

(defun reductionbasicvalue (p al next done)
  (let ((x (butlast p)) (y (car (last p))) (ol))
    (cond ((and (setq x (groundplugstdexp x al)) (every #'primitivep (cdr x)))
           (setq x (funcall (get (car x) 'basicval) x))
           (if (and (not (null x)) (setq ol (unify y al x al)))
               (proexit `(reductionvaluenext ,ol ,next) done)
               (profail nil next)))
          (t (reductionassumption nil p al next done)))))

(defun reductionbasic (p al next done)
  (let (ans)
    (cond ((and (setq ans (groundplugstdexp p al))
                (every 'primitivep (cdr ans)))
           (if (apply (get (car ans) 'basic) (cdr ans))
               (proexit next done)
               (profail nil next)))
          (t (reductionassumption nil p al next done)))))

(defun reductionnotbasicvalue (p al next done)
  (let ((x (butlast p)) (y (car (last p))) (ol))
    (cond ((and (setq x (groundplugstdexp x al)) (every #'primitivep (cdr x)))
           (setq x (funcall (get (car x) 'basicval) x))
           (cond ((null x) (profail nil next))
                 ((setq ol (unify y al x al)) (backup ol) (profail nil next))
                 (t (proexit next done))))
          (t (reductionassumption nil `(not ,p) al next done)))))

(defun reductionnotbasic (p al next done)
  (let (ans)
    (cond ((and (setq ans (groundplugstdexp p al))
                (every #'primitivep (cdr ans)))
           (if (not (apply (get (car ans) 'basic) (cdr ans)))
               (proexit next done)
               (profail nil next)))
          (t (reductionassumption nil `(not ,p) al next done)))))


(defun reductioncallfail (p al depth stack next done)
  (let (dum)
    (cond ((funcall *filter* (operator p))
           (cond ((and (groundishp (setq dum (plugstdexp p al)))
                       (knownp dum *theory*))
                  (proexit next done))
                 ((funcall *test* (setq dum (plugstdexp p al)))
                  (reductionassume dum next done))
                 (t (profail nil next))))
          (t (reductionrs p al depth stack
                        `(reductioncallfailnext ,depth ,next) done)))))

(defun reductioncallfailnext (ans depth next)
  (cond ((null ans) (profail nil next))
        ((eql ans depth) (profail nil next))
        (t (profail ans next))))

(defun reductionrs (p al depth stack next done)
  (cond ((and *ancestry* (reductionancestor p al stack next done)))
        ((and (numberp *ancestry*) (reductionnumber p al stack 0))
         (reductionassumption nil p al next done))
        (*reduction* ;;; (or (atom p) (not (eq 'not (car p)))))
         (reductionrsloop nil p al depth stack stack nil next done))
        (t (reductiondb p al depth stack *theory* next done))))

(defun reductionnumber (p al stack n)
  (let (ol)
    (cond ((numgeqp n *ancestry*))
          ((null stack) nil)
          ((atom p)
           (reductionnumber p al (cdr stack) (if (eq p (caar stack)) (1+ n) n)))
          ((setq ol (unify p al (caar stack) (cdar stack)))
           (prog1 (reductionnumber p al (cdr stack) (1+ n)) (backup ol)))
          (t (reductionnumber p al (cdr stack) n)))))

(defun reductionancestor (p al stack next done)
  (do ((l stack (cdr l)))
      ((null l) nil)
      (cond ((identify (caar l) (cdar l) p al) (return (profail nil next)))
            ((eq (operator (caar l)) (operator p))
             (setq p (plugstdexp p al))
             (setq *reduction* (cons p *reduction*))
             (return (proexit `(residuenext ,next) done))))))

(defun reductionrsloop (ans p al depth stack sl ol next done)
  (backup ol)
  (cond ((not (null ans)) (profail ans next))
        ((null sl) (reductiondb p al depth stack *theory* next done))
        ((setq ol (unify (maknot (caar sl)) (cdar sl) p al))
         (proexit `(reductionrsloop ,p ,al ,depth ,stack ,(cdr sl) ,ol ,next ,done)
                  done))
        (t (reductionrsloop nil p al depth stack (cdr sl) ol next done))))

(defun reductiondb (p al depth stack th next done)
  (reductionth p al depth stack th
          `(reductiondbs ,p ,al ,depth ,stack ,(includees th) ,next ,done) done))

(defun reductiondbs (ans p al depth stack tl next done)
  (cond ((not (null ans)) (profail ans next))
        ((null tl) (profail nil next))
        (t (reductiondb p al depth stack (car tl)
                   `(reductiondbs ,p ,al ,depth ,stack ,(cdr tl) ,next ,done) done))))

(defun reductionth (p al depth stack th next done)
 (reductionths nil p al depth stack (envindexps p al th) (environment) nil next done))

;;; ground test in rule case would save work but it is expensive.
;;; note that subset test not good in rule case cause subgoals may bind vars.
;;; we should allow *saves* to work here when caching problem solved.

(defun reductionths (ans p al depth stack fl bl ol next done)
  (backup ol)
  (cond ((not (null ans)) (profail ans next))
        ((null fl) (profail nil next))
        ((and (listp (car fl)) (eq (caar fl) '<=))
         (cond ((setq ol (unify (cadar fl) bl p al))
                (setq next `(reductionths ,p ,al ,depth ,stack ,(cdr fl) ,bl ,ol ,next ,done))
                (cond (*ancestry* (setq stack (acons p al stack)))
                      (*reduction* ;;; (not (atom p)) (eq 'not (car p)))
                       (setq stack (acons p al stack))))
                (reductionand (cons 'and (cddar fl)) bl (1+ depth) stack next done))
               (t (reductionths nil p al depth stack (cdr fl) bl ol next done))))
        ((setq ol (unify (car fl) bl p al))
         (if (not (subolp ol (alist bl)))
             (setq next `(reductionths ,p ,al ,depth ,stack ,(cdr fl) ,bl ,ol ,next ,done)))
         (proexit next done))
        (t (reductionths nil p al depth stack (cdr fl) bl nil next done))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; residue.lisp
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defparameter *residue* nil)

(defmethod residue (x p *theory* &optional (*filter* #'basep) (*test* #'success))
 "(RESIDUE P *THEORY* &OPTIONAL (*TEST* #'FAILURE))
  RESIDUE takes as argument a term, sentence, a theory, and two unary
  predicates.  It finds one residue for the specified sentence by applying
  model elimination to the information in the specified theory and its included
  theories.  The search is done in iterative deepening fashion, controlled by
  the variables *START*, *INCREMENT*, and *DEPTH*.  RESIDUE returns as value
  the first residue for the specified sentence that satisfies the two
  predicates.  The first predicate is a test on the operator of each literal in 
  each residue; the second predicate is a test on the entire literal.  If there
  are no residues, the value is NIL."
  (do ((old *depth*) (*depth* (min *start* *depth*) (+ *depth* *increment*))
       (alist (environment)) (*residue* nil) (level 0) (tracecalls) (rl))
      ((> *depth* old) nil)
      (setq *termination* nil *inferences* 0 *unifications* 0)
      (setq x (vars x))
      (cond ((residuedepth p alist 1 nil '(failure) '(success))
             (setq rl (nreverse (plugstdexp *residue* alist)))
             (do ((l (vars p) (cdr l)) (dum))
                 ((null l))
                 (if (not (eq (setq dum (plugstdexp (car l) alist)) (car l)))
                   (setq rl (cons `(same ,(car l) ,dum) rl))))
             (if (every #'(lambda (x) (funcall *test* x)) rl)
                 (return (maksand rl))))
            ((not *termination*) (return nil)))))

(defmethod residues (x p *theory* &optional (*filter* #'basep) (*test* #'success))
 "(RESIDUES P *THEORY* &OPTIONAL (*TEST* #'FAILURE))
  RESIDUES takes as argument a term, sentence, a theory, and two unary
  predicates.  It finds all residues for the specified sentence by applying
  model elimination to the information in the specified theory and its included
  theories.  The search is done in iterative deepening fashion, controlled by
  the variables *START*, *INCREMENT*, and *DEPTH*.  RESIDUES returns as value a
  list of all residues for the specified sentence that satisfy the two
  predicates.  The first predicate is a test on the operator of each literal in 
  each residue; the second predicate is a test on the entire literal.  If there
  are no residues, the value is NIL."
  (let ((alist (environment)) (*residue*) (level 0) tracecalls)
    (setq *termination* nil *inferences* 0 *unifications* 0)
    (setq x (vars x))
    (do ((next (residuedepth p alist 1 nil '(failure) '(success)) (profail nil next))
         (rl) (nl))
        ((null next) (nreverse nl))
        (setq rl (nreverse (plugstdexp *residue* alist)))
        (do ((l (vars p) (cdr l)) (dum))
            ((null l))
            (if (not (eq (setq dum (plugstdexp (car l) alist)) (car l)))
                (setq rl (cons `(same ,(car l) ,dum) rl))))
        (if (every #'(lambda (x) (funcall *test* x)) rl)
            (setq nl (adjoin (maksand rl) nl :test #'equal))))))

(defun residuedepth (p al depth stack next done)
  (setq *inferences* (1+ *inferences*))
  (cond ((numgreaterp depth *depth*) (setq *termination* t) (profail nil next))
        ((and traceexpressions
              (protracecall p al done)
              (setq next `(protracefail ,next ,done))
              (setq done `(protraceexit ,done)) nil))
        (t (residueexp p al depth stack next done))))

(defun residueexp (p al depth stack next done)
  (cond ((atom p) (residueconst p al depth stack next done))
        ((eq 'not (car p)) (residuenotexp (cadr p) al depth stack next done))
        ((eq 'and (car p)) (residueand p al depth stack next done))
        ((eq 'or (car p)) (residueor p al depth stack next done))
	((eq 'oneof (car p)) (residueassumption nil p al next done))
	((eq 'same (car p)) (residuesame p al next done))
	((eq 'distinct (car p)) (residuedistinct p al next done))
	((eq 'ground (car p)) (residueground p al next done))
	((eq 'nonground (car p)) (residuenonground p al next done))
	((eq 'primitive (car p)) (residueprimitive p al next done))
	((eq 'nonprimitive (car p)) (residuenonprimitive p al next done))
	((eq '== (car p)) (residuevalue p al next done))
	((eq 'value (car p)) (residuevalue p al next done))
	((eq 'execute (car p)) (residueassumption nil p al next done))
	((eq 'evaluate (car p)) (residueassumption nil p al next done))
	((eq 'unprovable (car p)) (residueunprovable p al next done))
	((eq 'bagofall (car p)) (residuebagofall p al next done))
	((eq 'stringmatch (car p)) (residuestringmatch p al next done))
	((get (car p) 'basicval) (residuebasicvalue p al next done))
	((get (car p) 'basic) (residuebasic p al next done))
	(t (residuecallfail p al depth stack next done))))

(defun residuenotexp (p al depth stack next done)
  (cond ((atom p) (residuenotconst p al depth stack next done))
        ((eq 'not (car p)) (residuedepth (cadr p) al depth stack next done))
        ((eq 'and (car p)) (residueornot p al depth stack next done))
        ((eq 'or (car p)) (residueandnot p al depth stack next done))
	((eq 'oneof (car p)) (profail nil next))
	((eq 'same (car p)) (profail nil next))
	((eq 'distinct (car p)) (profail nil next))
	((eq 'ground (car p)) (profail nil next))
	((eq 'nonground (car p)) (profail nil next))
	((eq 'primitive (car p)) (profail nil next))
	((eq 'nonprimitive (car p)) (profail nil next))
	((eq '== (car p)) (residuenotvalue p al next done))
	((eq 'value (car p)) (residuenotvalue p al next done))
	((eq 'execute (car p)) (residueassumption nil `(not ,p) al next done))
	((eq 'evaluate (car p)) (residueassumption nil `(not ,p) al next done))
	((eq 'unprovable (car p)) (residueexp (cadr p) al depth stack next done))
	((eq 'bagofall (car p)) (residuenotbagofall p al next done))
	((eq 'stringmatch (car p)) (profail nil next))
        ((get (car p) 'basicval) (residuenotbasicvalue p al next done))
        ((get (car p) 'basic) (residuenotbasic p al next done))
        (t (residuecallfail `(not ,p) al depth stack next done))))

(defun residueconst (p al depth stack next done)
  (cond ((eq 'true p) (proexit next done))
        ((eq 'cut p) (proexit next done))
        ((eq 'false p) (profail nil next))
        (t (residuecallfail p al depth stack next done))))

(defun residuenotconst (p al depth stack next done)
  (cond ((eq 'true p) (profail nil next))
        ((eq 'cut p) (profail nil next))
        ((eq 'false p) (proexit next done))
        (t (residuecallfail `(not ,p) al depth stack next done))))


(defun residueand (p al depth stack next done)
  (cond ((null (cdr p)) (proexit next done))
        (t (residueands next (cdr p) al depth stack done))))

(defun residueands (ans pl al depth stack done)
  (cond ((null pl) (proexit ans done))
        ((eq 'cut (car pl))
         (residueands `(procut ,depth ,ans) (cdr pl) al depth stack done))
        (t (residuedepth (car pl) al depth stack
                      ans `(residueands ,(cdr pl) ,al ,depth ,stack ,done)))))

(defun residueandnot (p al depth stack next done)
  (cond ((null (cdr p)) (proexit next done))
        (t (residuedepth `(not ,(cadr p)) al depth stack
                      next `(residueandnots ,(cddr p) ,al ,depth ,stack ,done)))))

(defun residueandnots (ans pl al depth stack done)
  (cond ((null pl) (proexit ans done))
        ((eq 'cut (car pl))
         (residueandnots `(procut ,ans ,done) (cdr pl) al depth stack done))
        (t (residuedepth `(not ,(car pl)) al depth stack
                      ans `(residueandnots ,(cdr pl) ,al ,depth ,stack ,done)))))

(defun residueor (p al depth stack next done)
  (residueors nil (cdr p) al depth stack next done))

(defun residueors (ans pl al depth stack next done)
  (cond ((not (null ans)) (profail ans next))
        ((null pl) (profail nil next))
        (t (residuedepth (car pl) al depth stack
                      `(residueors ,(cdr pl) ,al ,depth ,stack ,next ,done) done))))

(defun residueornot (p al depth stack next done)
  (residueornots nil (cdr p) al depth stack next done))

(defun residueornots (ans pl al depth stack next done)
  (cond ((not (null ans)) (profail ans next))
        ((null pl) (profail nil next))
        (t (residuedepth `(not ,(car pl)) al depth stack
                      `(residueornots ,(cdr pl) ,al ,depth ,stack ,next ,done) done))))


(defmethod basep (r)
 "BASEP takes a symbol as argument and determines whether it is a predefined
  relation.  If so, it returns T; otherwise, it returns NIL."
  (or (find r '(oneof same distinct ground nonground primitive nonprimitive
                == value execute evaluate unprovable stringmatch))
      (get r 'basic)
      (get r 'basicval)))

(defun assumablep (p al)
  (when (and (funcall *filter* (operator p))
             (funcall *test* (setq p (plugstdexp p al))))
    p))

(defun residueassumption (ans p al next done)
  (cond (ans (profail next done))
        ((setq p (assumablep p al))
         (residueassume p next done))
        (t (profail nil next))))

(defun residueassume (p next done)
  (cond ((or (not *consistency*)
             (not (or (rebuttalp p *residue*) (rebuttheoryp p *theory*)))
             (consistentp p *residue*))
         (setq *residue* (cons p *residue*))
         (proexit `(residuenext ,next) done))
        (t (profail nil next))))

(defun rebuttheoryp (p th)
  (cond ((rebuttalp p (indexees (operator p) th)))
        (t (do ((l (includees th) (cdr l)))
               ((null l) nil)
               (if (rebuttheoryp p (car l)) (return t))))))

(defun rebuttalp (p l)
  (cond ((and (listp p) (eq 'not (car p)))
         (find (operator p) l :test #'arguesp))
        (t (find (operator p) l :test #'rebutsp))))

(defun arguesp (r p)
  (or (eq r p)
      (and (listp p)
           (or (eq r (car p))
               (and (eq '<= (car p))
                    (or (eq r (cadr p))
                        (and (listp (cadr p)) (eq r (caadr p)))))))))

(defun rebutsp (r p)
  (and (listp p)
       (or (and (eq 'not (car p)) 
                (or (eq r (cadr p)) (and (listp (cadr p)) (eq r (caadr p)))))
           (and (eq '<= (car p))
                (listp (setq p (cadr p))) (eq 'not (car p))
                (or (eq r (cadr p)) (and (listp (cadr p)) (eq r (caadr p))))))))

(defun consistentp (p res)
  (let (traceexpressions)
    (cond ((basep (operator p)) t)
          ((null (setq res (remove-if #'(lambda (x) (basep (operator x))) res))) t)
	  ((consisp `(exists ,(vars (maksand (cons p res))) ,(maksand (cons p res))) *theory*) t)
	  ;((consisp `(<= ,(maknot p) . ,res) *theory*) nil)
          ;(t t))))
	  (t nil))))

#|  Mike's version --- don't know what is going on here.
(defun consisp (p th)
  (let ((cs (clausesets (maknot `(forall ,(vars p) ,p)))))
    (decludes 'epitheory)
    (empty 'epitheory)
    (includes 'epitheory th)
    (mapc #'(lambda (x) (save (car x) 'epitheory)) (cdr cs))
    (findp (maknot (caar cs)) 'epitheory)))
|#

; return that p is consistent with th if we fail to prove that TH |= ~Ax.p
(defun consisp (p th)
  (let (head phi vs (*depth* *consistency-depth*))
    (setq vs (freevars p))
    (setq head (gentemp "tlh"))
    (if vs 
	(setq phi `(<=> ,head (not (forall ,vs ,p)))) 
	(setq phi `(<=> ,head (not ,p))))
    (decludes 'epitheory)
    (empty 'epitheory)
    (includes 'epitheory th)
    (mapc #'(lambda (x) (save x 'epitheory)) (contrapositives* phi '(=)))
    (not (fullfindp head 'epitheory))))
    
	   
(defun residuenext (ans next)
  (setq *residue* (cdr *residue*))
  (profail ans next))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Note that both primitive and nonprimitive are true of all objects
;;; but Epilog succeeds only on terms that are primitive or nonprimitive.
;;; This is why (nonprimitive <x>) is not (not (primitive <x>))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun residueground (p al next done)
  (cond ((chkgroundp (cadr p) al) (proexit next done))
        (t (residueassumption nil p al next done))))

(defun residuenonground (p al next done)
  (cond ((chkgroundp (cadr p) al) (profail nil next))
        (t (residueassumption nil p al next done))))

(defun residueprimitive (p al next done)
  (cond ((chkprimitivep (cadr p) al)
         (if (chkgroundp (cadr p) al) (proexit next done)
             (residueassumption nil p al next done)))
        (t (profail nil next))))

(defun residuenonprimitive (p al next done)
  (cond ((chkprimitivep (cadr p) al) (profail nil next))
        (t (proexit next done))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Same is equals but succeeds only on unifiable terms.
;;; Distinct is true of all pairs of objects
;;; but epilog succeeds only in those cases where the arguments are different
;;; This is why there is no (not (distinct <x> <y>))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun residuesame (p al next done)
  (let ((ol))
    (cond ((setq ol (unify (caddr p) al (cadr p) al))
           (proexit `(residuevaluenext ,ol ,next) done))
          (t (profail nil next)))))

(defun residuedistinct (p al next done)
  (let ((ol))
    (cond ((setq ol (unify (caddr p) al (cadr p) al))
           (backup ol)
           (if (cdr ol) (residueassumption nil p al next done)
               (profail nil next)))
          (t (proexit next done)))))


(defun residuevalue (p al next done)
  (let ((x (residuebasicval (cadr p) al)) (y (caddr p)) (ol))
    (cond ((or (null x) (not (chkprimitivep y al)))
           (residueassumption nil p al next done))
          ((setq ol (unify y al x al))
           (proexit `(residuevaluenext ,ol ,next) done))
          (t (profail nil next)))))

(defun residuevaluenext (ans ol next)
  (backup ol)
  (profail ans next))

(defun residuenotvalue (p al next done)
  (let ((x (residuebasicval (cadr p) al)) (y (caddr p)) (ol))
    (cond ((null x) (residueassumption nil `(not ,p) al next done))
          ((setq ol (unify y al x al)) (backup ol) (profail nil next))
          (t (proexit next done)))))

;;; needless inefficiency here.  Just needs to check atom and car.

(defun residuebasicval (x al)
  (setq x (plugstdexp x al))
  (cond ((atom x) (if (and (groundp x) (primitivep x)) x))
        ((eq 'execute (car x)) nil)
        ((eq 'evaluate (car x)) nil)
        ((and (get (car x) 'basicval)
              (groundp x)
              (every #'primitivep (cdr x)))
         (funcall (get (car x) 'basicval) x))
        ((and (groundp x) (primitivep x)) x)))

(defun residueunprovable (p al next done)
  (let (rl)
    (setq p (plugstdexp (cadr p) al))
    (setq rl (residues t p *theory* *filter* *test*))
    (cond ((null rl) (proexit next done))
          ((eq 'true (maksor rl)) (profail nil next))
          (t (residueassume `(unprovable ,(maksor rl)) next done)))))

(defun residuebagofall (p al next done)
  (let (rl)
    (setq p (plugstdexp p al))
    (setq rl (residues (cadr p) (caddr p) *theory* *filter* *test*))
    (cond ((null rl) (profail nil next))
          ((eq 'true (maksor rl)) (proexit next done))
          (t  (setq *residue* (cons `(bagofall ,(cadr p) ,(maksor rl) ,(cadddr p)) *residue*))
              (proexit `(residuenext ,next) done)))))

(defun residuenotbagofall (p al next done)
  (let (rl)
    (setq p (plugstdexp p al))
    (setq rl (residues (cadr p) (caddr p) *theory* *filter* *test*))
    (cond ((null rl) (profail nil next))
          ((eq 'true (maksor rl)) (proexit next done))
          (t (residueassume `(not (bagofall ,(cadr p) ,(maksor rl) ,(cadddr p))) next done)))))

(defun residuestringmatch (p al next done)
  (let (pat str dum ol)
    (cond ((and (setq pat (groundplugstdexp (cadr p) al)) (primitivep pat)
                (setq str (groundplugstdexp (caddr p) al)) (primitivep str))
           (if (and (car (setq dum (multiple-value-list (stringmatches pat str))))
                    (setq ol (unify (cdddr p) al (cdr dum) al)))
               (proexit `(findvaluenext ,ol ,next) done)
               (profail nil next)))
          (t (residueassumption nil p al next done)))))

(defun residuebasicvalue (p al next done)
  (let ((x (butlast p)) (y (car (last p))) (ol))
    (cond ((and (setq x (groundplugstdexp x al)) (every #'primitivep (cdr x)))
           (setq x (funcall (get (car x) 'basicval) x))
           (if (and (not (null x)) (setq ol (unify y al x al)))
               (proexit `(residuevaluenext ,ol ,next) done)
               (profail nil next)))
          (t (residueassumption nil p al next done)))))

(defun residuebasic (p al next done)
  (let (ans)
    (cond ((and (setq ans (groundplugstdexp p al))
                (every 'primitivep (cdr ans)))
           (if (apply (get (car ans) 'basic) (cdr ans))
               (proexit next done)
               (profail nil next)))
          (t (residueassumption nil p al next done)))))

(defun residuenotbasicvalue (p al next done)
  (let ((x (butlast p)) (y (car (last p))) (ol))
    (cond ((and (setq x (groundplugstdexp x al)) (every #'primitivep (cdr x)))
           (setq x (funcall (get (car x) 'basicval) x))
           (cond ((null x) (profail nil next))
                 ((setq ol (unify y al x al)) (backup ol) (profail nil next))
                 (t (proexit next done))))
          (t (residueassumption nil `(not ,p) al next done)))))

(defun residuenotbasic (p al next done)
  (let (ans)
    (cond ((and (setq ans (groundplugstdexp p al))
                (every #'primitivep (cdr ans)))
           (if (not (apply (get (car ans) 'basic) (cdr ans)))
               (proexit next done)
               (profail nil next)))
          (t (residueassumption nil `(not ,p) al next done)))))


(defun residuecallfail (p al depth stack next done)
  (let (dum)
    (cond ((funcall *filter* (operator p))
           (cond ((and (groundishp (setq dum (plugstdexp p al)))
                       (knownp dum *theory*))
                  (proexit next done))
                 ((funcall *test* (setq dum (plugstdexp p al)))
                  (residueassume dum next done))
                 (t (profail nil next))))
          (t (residuers p al depth stack
                        `(residuecallfailnext ,depth ,next) done)))))

(defun groundishp (x)
  (cond ((eq x '?*))
        ((varp x) nil)
        ((atom x))
        ((eq 'quote (car x)))
	(t (every 'groundishp x))))

(defun residuecallfailnext (ans depth next)
  (cond ((null ans) (profail nil next))
        ((eql ans depth) (profail nil next))
        (t (profail ans next))))

(defun residuers (p al depth stack next done)
  (cond ((and *ancestry* (residueancestor p al stack)) (profail nil next))
        ((and (numberp *ancestry*) (residuenumber p al stack 0))
         (residueassumption nil p al next done))
        (*reduction* ;;; (or (atom p) (not (eq 'not (car p)))))
         (residuersloop nil p al depth stack stack nil next done))
        (t (residuedb p al depth stack *theory* next done))))

(defun residuenumber (p al stack n)
  (let (ol)
    (cond ((numgeqp n *ancestry*))
          ((null stack) nil)
          ((atom p)
           (residuenumber p al (cdr stack) (if (eq p (caar stack)) (1+ n) n)))
          ((setq ol (unify p al (caar stack) (cdar stack)))
           (prog1 (residuenumber p al (cdr stack) (1+ n)) (backup ol)))
          (t (residuenumber p al (cdr stack) n)))))

(defun residueancestor (p al stack)
  (do ((l stack (cdr l)))
      ((null l) nil)
      (if (identify (caar l) (cdar l) p al) (return t))))

(defun residuersloop (ans p al depth stack sl ol next done)
  (backup ol)
  (cond ((not (null ans)) (profail ans next))
        ((null sl) (residuedb p al depth stack *theory* next done))
        ((setq ol (unify (maknot (caar sl)) (cdar sl) p al))
         (proexit `(residuersloop ,p ,al ,depth ,stack ,(cdr sl) ,ol ,next ,done)
                  done))
        (t (residuersloop nil p al depth stack (cdr sl) ol next done))))

(defun residuedb (p al depth stack th next done)
  (residueth p al depth stack th
          `(residuedbs ,p ,al ,depth ,stack ,(includees th) ,next ,done) done))

(defun residuedbs (ans p al depth stack tl next done)
  (cond ((not (null ans)) (profail ans next))
        ((null tl) (profail nil next))
        (t (residuedb p al depth stack (car tl)
                   `(residuedbs ,p ,al ,depth ,stack ,(cdr tl) ,next ,done) done))))

(defun residueth (p al depth stack th next done)
 (residueths nil p al depth stack (envindexps p al th) (environment) nil next done))

;;; ground test in rule case would save work but it is expensive.
;;; note that subset test not good in rule case cause subgoals may bind vars.
;;; we should allow *saves* to work here when caching problem solved.

(defun residueths (ans p al depth stack fl bl ol next done)
  (backup ol)
  (cond ((not (null ans)) (profail ans next))
        ((null fl) (profail nil next))
        ((and (listp (car fl)) (eq (caar fl) '<=))
         (cond ((setq ol (unify (cadar fl) bl p al))
                (setq next `(residueths ,p ,al ,depth ,stack ,(cdr fl) ,bl ,ol ,next ,done))
                (cond (*ancestry* (setq stack (acons p al stack)))
                      (*reduction* ;;; (not (atom p)) (eq 'not (car p)))
                       (setq stack (acons p al stack))))
                (residueand (cons 'and (cddar fl)) bl (1+ depth) stack next done))
               (t (residueths nil p al depth stack (cdr fl) bl ol next done))))
        ((setq ol (unify (car fl) bl p al))
         (if (not (subolp ol (alist bl)))
             (setq next `(residueths ,p ,al ,depth ,stack ,(cdr fl) ,bl ,ol ,next ,done)))
         (proexit next done))
        (t (residueths nil p al depth stack (cdr fl) bl nil next done))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; consequences.lisp
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(eval-when (compile load eval)
  (proclaim '(special *theory* *test* *names* *functionals*
                      *start* *increment* *depth* *termination*
                      alist level tracecalls tracefacts traceexpressions)))

(defparameter *consequences* nil)

(defmethod consequences (p *theory* &optional (*test* #'failure))
 "(CONSEQUENCES P *THEORY*)
  CONSEQUENCES takes a sentence and a theory as arguments.  The sentence is
  assumed to be a literal.  CONSEQUENCES uses model elimination to derive
  conclusions from the specified sentence and the sentences in the specified
  theory and its included theories.  The search is bottom-up, depth-first, and
  statically-ordered and uses *DEPTH* as a depth limit.  If it derives a literal
  with relation constant on the list *SAVES*, it saves the literal it derives
  on a list and returns that list as value."
  (let ((alist (environment)) (*consequences*) (level 0) tracecalls)
    (setq *termination* nil)
    (consequencedepth p alist 1)
    (nreverse *consequences*)))

(defun consequencedepth (p al depth)
  (cond ((numgreaterp depth *depth*) (setq *termination* t) nil)
        (t (if traceexpressions (tracesave p al))
           (consequenceexp p al depth)
           (if traceexpressions (tracedone p al)))))

(defun consequenceexp (p al depth)
  (cond ((atom p) (consequenceexpexit p al depth))
        ((eq 'and (car p)) 
         (mapc #'(lambda (x) (consequencedepth x al depth)) (cdr p)))
        (t (consequenceexpexit p al depth))))

(defun consequenceexpexit (p al depth)
  (setq p (plugstdexp p al))
  (cond ((knownp p *theory* 'samep) nil)
;        ((and (listp p) (eq 'execute (car p))) (ignore-errors (eval (cadr p))))
        ((and (consequence p al) nil))
        ((and (savep p) (insert p *theory*) nil))
        (t (consequencedb p al depth *theory*))))

(defun consequence (p al)
  (setq p (plugstdexp p al))
  (when (funcall *test* (operator p))
    (setq *consequences* (cons p *consequences*))))

(defun consequencedb (p al depth th)
  (cond ((consequenceth p al depth th))
        (t (do ((l (includees th) (cdr l)))
               ((null l) nil)
               (consequencedb p al depth (car l))))))

(defun consequenceth (p al depth th)
  (do ((l (envindexps p al th) (cdr l)) (ol) (bl (environment)) (ans))
      ((null l))
      (when (and (listp (car l)) (eq (caar l) '=>) (not (null (cddar l)))
                 (setq ol (unify (cadar l) bl p al)))
        (if tracefacts (tracefact (car l)))
        (setq ans (consequents '(failure) (car l) (butlast (cddar l)) bl depth))
        (backup ol)
        (if ans (return ans)))))

(defun consequents (next rule pl al depth)
  (cond ((null pl) (consequencedepth (car (last rule)) al (1+ depth))
         (profail nil next))
        ((eq 'cut (car pl)) (consequents next rule (cdr pl) al depth) t)
        ((finddepth (car pl) al depth nil
                    next `(consequents ,rule ,(cdr pl) ,al ,depth)))))


(defparameter *newconsequences* nil)

(defmethod newconsequences (p th *filter* *test*)
 "(NEWCONSEQUENCES P *THEORY*)
  NEWCONSEQUENCES takes a sentence and a theory as arguments.  The sentence is
  assumed to be ground literal or a conjunction of ground literals.  NEWCONSEQUENCES
  uses model elimination to derive
  conclusions from the specified sentence and the sentences in the specified
  theory and its included theories.  The search is bottom-up, depth-first, and
  statically-ordered and uses *DEPTH* as a depth limit.  If it derives a literal
  with relation constant on the list *SAVES*, it saves the literal it derives
  on a list and returns that list as value."
  (let ((alist (environment)) (*theory* (make-instance 'theory))
        (*newconsequences*) (level 0) tracecalls)
    (setq *termination* nil)
    (unwind-protect
      (progn (newsave p *theory*)
             (includes *theory* th)
             (newconsequencedepth p alist 'true 1 nil))
      (progn (decludes *theory*)
             (empty *theory*)))
    (nreverse *newconsequences*)))

(defun newsave (p th)
  (cond ((atom p) (save p th))
        ((eq 'and (car p)) (mapc #'(lambda (x) (newsave x th)) (cdr p)))
        (t (save p th))))

(defun newconsequencedepth (p al res depth stack)
  (cond ((numgreaterp depth *depth*) (setq *termination* t) nil)
        ((find (operator p) (cdr stack)) nil)
        (t (if traceexpressions (tracesave p al))
           (newconsequenceexp p al res depth stack)
           (if traceexpressions (tracedone p al)))))

(defun newconsequenceexp (p al res depth stack)
  (cond ((atom p) (newconsequencedb p al res depth stack *theory*))
        ((eq 'and (car p)) 
         (mapc #'(lambda (x) (newconsequencedepth x al res depth stack)) (cdr p)))
        ((funcall *test* (operator p))
         (cond ((eq res 'false))
               ((eq res 'true)
                (setq *newconsequences*
                      (adjoin (plugstdexp p al) *newconsequences* :test #'equalp)))
               (t (setq *newconsequences*
                        (cons (plugstdexp `(=> ,res ,p) al) *newconsequences*))))
         (newconsequencedb p al res depth stack *theory*))
        (t (newconsequencedb p al res depth stack *theory*))))  

(defun newconsequencedb (p al rl depth stack th)
  (cond ((newconsequenceth p al rl depth stack th))
        (t (do ((l (includees th) (cdr l)))
               ((null l) nil)
               (newconsequencedb p al rl depth stack (car l))))))

(defun newconsequenceth (p al rl depth stack th)
  (do ((l (envindexps p al th) (cdr l)) (ol) (bl (environment))
       (antecedent) (consequent))
      ((null l))
      (when (and (listp (car l)) (eq (caar l) '=>) (not (null (cddar l)))
                 (setq ol (unify (cadar l) bl p al)))
        (if tracefacts (tracefact (car l)))
        (setq antecedent (plugstdexp (maksand (butlast (cddar l))) bl))
        (setq consequent (plugstdexp (car (last (car l))) bl))
        (newconsequence antecedent consequent al rl depth stack)
        (backup ol))))

(defun newconsequence (antecedent consequent al rl depth stack)
  (do ((l (residues (vars consequent) antecedent *theory* *filter*) (cdr l)))
      ((null l) rl)
      (newconsequencedepth consequent al (makand rl (car l))
                           (1+ depth) (cons (operator consequent) stack))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
