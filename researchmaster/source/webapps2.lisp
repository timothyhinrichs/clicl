;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; To Do
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Security (parameter tampering, see journal for list)
; Error-handling (allow each guard to have its own error handling; allow errors and updates to be interleaved) 
; Workflows (e.g. show-profile should redirect to login and back to show-profile, unless already logged in)
; Update language (constraints+malleable, views+frameaxioms, etc.)
; Model Checking (testing, access control via implication, see journal for list)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Global variables
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; make sure to end with / 
;(defparameter *fs-path* "/Users/thinrich/Research/code/clicl/researchmaster/")
(defparameter *randlock* (make-lock))
(defparameter *randstate* (make-random-state))
(defparameter *web-path* "/docserver/infoserver/examples/")
(defparameter *component-paths* '((:app . ("researchmaster" "webapps" "webid"))
				  (:html . ("researchmaster" "webapps" "webid"))
				  (:compile . ("researchmaster" "webapps" "webid" "compiled"))
				  (:clicl . ())))
(defun fspath (component path &optional type)
  (let ((res (find component *component-paths* :key #'car)))
    (assert res nil (format nil "Couldn't find component ~A when computing absolute path for ~A" component path))
    (loadfn path :dir (cdr res) :root *localrootdir* :type type)))
 ;   (stringappend *fs-path* (cdr res) path)))

(defun webpath (component path &optional type)
  (let ((res (find component *component-paths* :key #'car)))
    (assert res nil (format nil "Couldn't find component ~A when computing absolute path for ~A" component path))
    (namestring (loadfn path :dir (cdr res) :root *web-path* :type type))))


(defconstant *cookie-session-name* '__session)
(defun find-cookie-session (cookie) (viewfindx '?x `(,*cookie-session-name* ?x) cookie))
(defun drop-cookie-session (cookie) (drop `(,*cookie-session-name* ?x) cookie #'matchp) cookie)
(defun save-cookie-session (id cookie) (save `(,*cookie-session-name* ,id) cookie #'matchp) cookie)

; internal globals
(defvar *sessions* (make-hash-table))
(defvar *output-stream* t)
(defvar *debug-webapps* nil)
(defvar *default-data-type* nil "Whether or not to assume undeclared slots are of type STRING")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Program Parsing
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar *signatures* (make-hash-table))
(define-condition invalid-signature (error) ((comment :initarg :comment :accessor comment)))
(define-condition unknown-signature (error) ((comment :initarg :comment :accessor comment)))
(defun signaturep (x) (nth-value 1 (gethash x *signatures*)))

(defun find-signature (x)
  (multiple-value-bind (y present) (gethash x *signatures*)
    (unless present (error 'unknown-signature :comment x))
    y))

(defmacro defsignature (p &rest l)
  "(DEFSIGNATURE P &REST L) takes a signature name and a list of symbol declarations.
   Each symbol declaration is either a symbol or a list (symbol [:arity n] [:type thing])"
  `(define-signature ',p ',l))

(defun define-signature (p l)
  "(DEFINE-SIGNATURE P L) walks over the symbol declarations in P, creates a list of them, and
   indexes that list in *signatures* by the name in P.  Also constructs a guard
   checking the types.  Returns that list.  Note that "
  (let ((arity nil) (h nil) (type nil) name (sig nil))
    ; walk over the symbol declarations and turn each into a parameter
    (dolist (x l)
      (cond ((atom x) 
	     (setq name x)
	     (setq arity nil)
	     (setq type nil))
	    ((listp x) 
	     (setq name (car x))
	     (setq h (grab-keyvals (cdr x) '(:arity :type) 'invalid-signature))
	     (setq arity (gethash :arity h))
	     (setq type (gethash :type h)))
	    (t (error 'invalid-signature :comment (format nil "Unknown symbol declaration type: ~A" x))))
      (unless (symbolp name) (error 'invalid-signature :comment (format nil "Unknown symbol name type: ~A" x )))
      ; make sure type is a list of things, when it is provided
      (when (and type (atom type)) (setq type (list type)))
      ; make sure reasonable values for arity and type, when provided
      (when (and arity (or (not (integerp arity)) (< arity 0)))
	(error 'invalid-signature :comment (format nil "Non-integral or non-positive arity ~A for symbol ~A" arity x)))
       (when (and type (not (every #'symbolp type)))
	(error 'invalid-signature :comment (format nil "Typename not a symbol in ~A" x)))
      ; infer arity/type if not provided
       (cond ((and (not arity) (not type)) (setq arity 1) (when *default-data-type* (setq type (list 'string))))
	     ((and arity (not type)) (unless *default-data-type* (setq type (n-copies arity 'string))))
	     ((and (not arity) type) (setq arity (length type)))
	     (t (unless (= (length type) arity)
		  (error 'invalid-signature :comment (format nil "Type and arity incompatible in ~A" x)))))
      ; make the parameter
      (push (make-parameter :symbol name :arity arity :type type) sig))
    ; store list of parameters *in the order the programmer declared them*, create guards for types, return list
    (setq sig (nreverse sig))
    (setf (gethash p *signatures*) sig)
    sig))

(defmacro defsignature1 (p &rest l)
  "(DEFSIGNATURE1 P &REST L) takes a signature name and a list of symbol declarations.
   Each symbol declaration is either a symbol or a list (symbol [:type thing]).
   Ensures the arity of each symbol is 1, regardless of declaration."
  `(define-signature1 ',p ',l))

(defun define-signature1 (p l)
  (mapc #'(lambda (x) (setf (parameter-arity x) 1)) (define-signature p l)))

(defmacro defsignature3 (p &rest l)
  "(DEFSIGNATURE3 P &REST L) takes a signature name and a list of symbol declarations.
   Each symbol declaration is either a symbol or a list (symbol [:type thing]).
   Ensures the signature represents a table in triples format, i.e. the first relation is the key
   and has arity 1, and the rest are attributes and have arity 2, where the first argument is the key."
  `(define-signature3 ',p ',l))

(defun define-signature3 (p l)
  (setq p (define-signature p l))
  (setf (parameter-arity (car p)) 1)
  (mapc #'(lambda (x) (setf (parameter-arity x) 2)) (cdr p)))

(defvar *schemas* (make-hash-table))
(defstruct schema name guards signature)
(define-condition invalid-schema (error) ((comment :initarg :comment :accessor comment)))
(define-condition unknown-schema (error) ((comment :initarg :comment :accessor comment)))
(defun schemap (x) (nth-value 1 (gethash x *schemas*)))
(defun find-schema (x)
  (multiple-value-bind (y present) (gethash x *schemas*)
    (unless present (error 'unknown-schema :comment x))
    y))

(defmacro defschema (p &rest l)
  "(DEFSCHEMA P &REST L) "
  `(define-schema ',p ',l))

(defun define-schema (p l)
  (let (h)
    (setq h (grab-keyvals l '(:signature :guards) 'invalid-schema))
    ;(when (gethash p *schemas*) (warn (format nil "Redefining schema ~A" p)))
    (setf (gethash p *schemas*) 
	  (make-schema :name p :signature (gethash :signature h) :guards (gethash :guards h)))))

(defvar *guards* (make-hash-table))
(defconstant *kernel-guards* '(__single-cookie-session __integrity))
(defstruct guard name logic supers datalog)  ; datalog is the version *we* use--keep both around for debugging
(define-condition invalid-guard (error) ((comment :initarg :comment :accessor comment)))
(define-condition unknown-guard (error) ((comment :initarg :comment :accessor comment)))
(defun guardp (x) (nth-value 1 (gethash x *signatures*)))
(defun guard-fol (x) (remove-if #'stringp (guard-logic x)))
(defun find-guard (x)
  (multiple-value-bind (y present) (gethash x *guards*)
    (unless present (error 'unknown-guard :comment x))
    y))   

(defmacro defguard (p &rest l)
  "(DEFGUARD P &REST L)"
   `(define-guard ',p ',l))

(defun define-guard (p l)
  (let (supers)
    (when (eq (first l) :inherits) (setq supers (second l)) (setq l (cddr l)))
    (when (and (member p *kernel-guards*) (gethash p *guards*)) 
      (warn (format nil "Redefining Kernel guard ~A" p)))
    (setf (gethash p *guards*) (make-guard :name p :supers supers :logic l :datalog (guard-to-datalog l)))))

(defun guard-to-datalog (l)
  "(GUARD-TO-DATALOG L) transforms a list of datalog constraints and rules L (each of which may have)
   a text snippet explaining the constraint) into a set of rules
   where each constraint becomes a rule whose head is (__error ?text).
   Rules are either (i) atomic or (ii) begin with <=.   Every other sentence is a constraint."
  (let (rules constraints)
    ; transform = to SAME
    (setq l (mapcar #'(lambda (x) (subrel '((= . same)) x)) l))
    ; pull apart logic and text
    (setq l (extract-kif-plus-text l))   ; now l is a list of (kif . "explanation")
    ;  add errors to the heads of all constraints, convert to rules, and index everything 
    (multiple-value-setq (rules constraints) 
      (split #'(lambda (x) (or (atomicp (car x)) (and (listp (car x)) (eq (caar x) '<=)))) l))
    (setq constraints (mapcan #'(lambda (x) (to-canonical-datalog `(<= (__error ,(cdr x)) ,(maknot (quantify (car x)))))) constraints))
    (setq constraints (mapcar #'order-datalog-rule constraints))
    (define-theory (make-instance 'prologtheory) "" (subrel '((= . same)) (append constraints rules)))))

(defun logic-to-datalog (l)
  (setq l (mapcan #'to-canonical-datalog l))
  (setq l (mapcar #'(lambda (x) (order-datalog-rule x :attach (webapp-builtin-params))) l))
  (setq l (subrel '((= . same)) l))
  l)

(defun extract-kif-plus-text (l)
  (let (th)
    (setq th nil)
    (do ((ls l (cdr ls)) (cnt 0 (1+ cnt)))
	((null ls) (nreverse th))
      (cond ((stringp (second ls)) 
	     (push (cons (first ls) (second ls)) th)
	     (setq ls (cdr ls)))
	    (t
	     (push (cons (first ls) (format nil "constraint ~A" cnt)) th))))))

(defvar *appdb* (make-instance 'prologtheory))
(defmacro defdb (&rest l)
  "(DEFDB &REST L)"
  `(unless (contents *appdb*) (definemore *appdb* ',l)))

(defvar *updates* (make-hash-table))
(defstruct update guards logic datalog)
(define-condition invalid-update (error) ((comment :initarg :comment :accessor comment)))
(define-condition unknown-update (error) ((comment :initarg :comment :accessor comment)))
(defun updatep (x) (nth-value 1 (gethash x *updates*)))
(defun find-update (x)
  (multiple-value-bind (y present) (gethash x *updates*)
    (unless present (error 'unknown-update :comment x))
    y))

(defmacro defupdate (p opts &rest l)
  "(DEFUPDATE P &REST L)"
  `(define-update ',p ',opts ',l))

(defun define-update (p opts l)
  (let (h)
    (setq h (grab-keyvals opts '(:language :guards) 'invalid-update))
    ;(when (gethash p *updates*) (warn (format nil "Redefining update ~A" p)))
    (setf (gethash p *updates*) (make-update :guards (gethash :guards h) :logic l :datalog (update-to-datalog l (gethash :language h))))))

(defun update-to-datalog (l lang)
  "(UPDATE-TO-DATALOG L) turns a list of sentences into an indexed Datalog theory.
   For now, assume l is already datalog."
  (declare (ignore lang))
  ; transform = to SAME
  (setq l (mapcar #'(lambda (x) (subrel '((= . same)) x)) l))
  (define-theory (make-instance 'prologtheory) "" l))

(defvar *forms* (make-hash-table))
(defstruct form schema target guards)
(define-condition invalid-form (error) ((comment :initarg :comment :accessor comment)))
(define-condition unknown-form (error) ((comment :initarg :comment :accessor comment)))
(defun formp (x) (nth-value 1 (gethash x *forms*)))
(defun find-form (x)
  (multiple-value-bind (y present) (gethash x *forms*)
    (unless present (error 'unknown-form :comment x))
    y))
(defun find-form-signaturename (x) (schema-signature (find-schema (form-schema (find-form x)))))

(defmacro defform (p &rest l)
  "(DEFFORM P &REST L)"
  `(define-form ',p ',l))

(defun define-form (p l)
  (let (h)
    (setq h (grab-keyvals l '(:guards :schema :target) 'invalid-form))
    ;(when (gethash p *forms*) (warn (format nil "Redefining form ~A" p)))
    (setf (gethash p *forms*) (make-form :guards (gethash :guards h) :target (gethash :target h) :schema (gethash :schema h)))))

(defvar *tables* (make-hash-table))
(define-condition invalid-table (error) ((comment :initarg :comment :accessor comment)))
(define-condition unknown-table (error) ((comment :initarg :comment :accessor comment)))
(defun tablep (x) (nth-value 1 (gethash x *tables*)))
(defun table-schema (x) x)
(defun find-table (x)
  (multiple-value-bind (y present) (gethash x *tables*)
    (unless present (error 'unknown-table :comment x))
    y))
(defun find-table-signaturename (x) (schema-signature (find-schema (find-table x))))

(defmacro deftable (p &rest l)
  "(DEFFORM P &REST L)"
  `(define-table ',p ',l))

(defun define-table (p l)
  (let (schema)
    (when (eq (first l) :schema) (setq schema (second l)) (setq l (cddr l)))
    (unless schema (error 'invalid-table :comment (format nil "No schema for table ~A" p)))
    ;(when (gethash p *tables*) (warn (format nil "Redefining table ~A" p)))
    (setf (gethash p *tables*) schema)))

(defvar *servlets* (make-hash-table))
(defstruct servlet name page guts entry)
(defstruct guardlist guards)
(defstruct updatelist updates)
(define-condition invalid-servlet (error) ((comment :initarg :comment :accessor comment)))
(define-condition unknown-servlet (error) ((comment :initarg :comment :accessor comment)))
(defun servletp (x) (nth-value 1 (gethash x *servlets*)))
(defun find-servlet (x)
  (multiple-value-bind (y present) (gethash x *servlets*)
    (unless present (error 'unknown-servlet :comment x))
    y))
(defun duplicate-servlet (name newname)
  (let (s new)
    (setq s (find-servlet name))
    (setq new (make-servlet :name newname
			    :page (servlet-page s)
			    :guts (copy-list (servlet-guts s))
			    :entry (servlet-entry s)))
    (setf (gethash newname *servlets*) new)))
(defun servlet-updates (servlet)
  (apply #'append (remove-if-not #'updatelist-p (servlet-guts servlet))))
(defun servlet-guards (servlet)
  (apply #'append (remove-if-not #'guardlist-p (servlet-guts servlet))))

(defmacro defservlet (p &rest l)
  "(DEFSERVLET P &REST L)"
  `(define-servlet ',p ',l))

(defun define-servlet (p l)
  (let (page entry guts)
    (do ((xs l (cddr xs)))
	((null xs))
      (cond ((eq (first xs) :page) (setq page (second xs)))
	    ((eq (first xs) :entry) (setq entry (second xs)))
	    ((eq (first xs) :guards) (push (make-guardlist :guards (second xs)) guts))
	    ((eq (first xs) :updates) (push (make-updatelist :updates (second xs)) guts))
	    (t (error 'invalid-servlet :comment (format nil "Unknown option: ~A" xs)))))
    (setf (gethash p *servlets*) (make-servlet :name p :page page :guts (nreverse guts) :entry entry))))

; HTML keys are lists of (htmlpagenames or filenames or HTMLREPL objs)
(defvar *html* (make-hash-table))
(defstruct htmlrepl target forms tables)
(define-condition invalid-html (error) ((comment :initarg :comment :accessor comment)))
(define-condition unknown-html (error) ((comment :initarg :comment :accessor comment)))
;(defun htmlp (x) (nth-value 1 (gethash x *html*)))
(defun find-html (x)
  (multiple-value-bind (y present) (gethash x *html*)
    (unless present (error 'unknown-html :comment x))
    y))

(defmacro defhtml (p &rest l)
  "(DEFHTML P &REST L)"
  `(define-html ',p ',l))

(defun define-html (p l)
  (let (h (newl nil))
    (dolist (e l)
      (cond ((symbolp e) (push e newl))
	    ((stringp e) (push e newl))
	    ((listp e) 
	     (unless (or (symbolp (car e)) (stringp (car e))) (error 'invalid-html :comment (format nil "~A in ~A" e p)))
	     (setq h (grab-keyvals (cdr e) '(:forms :tables) 'invalid-html))
	     (push (make-htmlrepl :target (car e) :forms (gethash :forms h) :tables (gethash :tables h)) newl))
	    (t (error 'invalid-html :comment p))))
    (setf (gethash p *html*) (nreverse newl))))
    
; Tests
(defparameter *tests* nil)  ; gets reset to NIL every time loaded (defparameter)
(defstruct wtest servlet input cookie error output dbchange sessionchange cookiechange)
(define-condition invalid-test (error) ((comment :initarg :comment :accessor comment)))

(defmacro deftest (&rest l)
  "(DEFTEST &REST L)"
  `(define-test ',l))

(defun define-test (l)
  (let (h)
    (setq h (grab-keyvals l '(:servlet :input :output :cookie :error :dbchange :sessionchange :cookiechange) 'invalid-test))
    (push (make-wtest :servlet (gethash :servlet h) :input (gethash :input h) :output (gethash :output h) 
		      :cookie (gethash :cookie h) :error (gethash :error h) :dbchange (gethash :dbchange h)
		      :sessionchange (gethash :sessionchange h) :cookiechange (gethash :cookiechange h))
	  *tests*)))

; Access control policies
(defparameter *accesscontrol* nil) ; gets reset to NIL every time loaded (defparameter)
(define-condition invalid-accesscontrol (error) ((comment :initarg :comment :accessor comment)))
(defmacro defaccesscontrol (&rest l)
  "(DEFACCESSCONTROL &REST L)"
  `(define-accesscontrol ',l))

(defun define-accesscontrol (l) (setq *accesscontrol* l)) ;(append l *accesscontrol*)))


;;; Helper functions
(defun grab-keyvals (l keys error)
  "(GRAB-KEYVALS L KEYS ERROR) takes a list and a set of keys.  It grabs the key/value pairs
   throwing an error of type ERROR if it finds a key not in the list."
  (do ((ys l (cdr ys))
       (h (make-hash-table)))
      ((null ys) h)
    (cond ((or (not keys) (member (first ys) keys))
	   (setf (gethash (first ys) h) (second ys))
	   (setq ys (cdr ys)))
	  (t (error error  :comment (format nil "Unknown option: ~A" ys))))))


;;;;; Builtins (not set by developers, but by us)

(defvar *weblog-builtins* (make-hash-table))
(defstruct builtin name code args returns internal param)
(defun webapp-builtins () (cons '= (mapcar #'car (hash2bl *weblog-builtins*))))
(defun webapp-builtin-params () 
  (cons (make-parameter :symbol '= :arity 2) 
	(mapcar #'(lambda (x) (builtin-param (cdr x))) (hash2bl *weblog-builtins*))))
(defun is-builtin (x) (if (eq x '=) t (nth-value 1 (gethash x *weblog-builtins*))))

(defmacro defbuiltin (p code args returns &optional internal) `(define-builtin ',p (function ,code) ',args ',returns ',internal))
(defun define-builtin (p func args returns internal)
  "(DEFINE-BUILTIN P FUNC RETURNS INTERNAL) sets up data structures so that the weblog symbol
   P is defined as a builtin that causes code FUNC to run and return RETURNS values."
  ; make sure args and returns are lists
  (unless (listp args) (setq args (list args)))
  (unless (listp returns) (setq returns (list returns)))
  ; register as weblog builtin
  (setf (gethash p *weblog-builtins*) 
	(make-builtin :name p :code func :args args :returns returns :internal internal
		      :param (make-parameter :symbol p :arity (+ (length args) (length returns)) :type (append args returns))))
  ; register builtin with Epilog's viewfind code
  (setf (gethash p *builtins*) (list func (length args) (length returns))))

;;;;; Types (not set by developers, but by us)
; can't use 'type' b/c lisp already does

(defvar *sorts* (make-hash-table))
(defstruct sort name checker caster htmlsanitizer)
; (defsort name builtinchecker builtincaster builtinsanitizer)
(defmacro defsort (p checker caster htmlsanitizer) `(define-sort ',p ',checker ',caster ',htmlsanitizer))
(defun define-sort (p checker caster htmlsanitizer)
  (setf (gethash p *sorts*) (make-sort :name p :checker checker :caster caster :htmlsanitizer htmlsanitizer)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Initialization
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun reset-weblog (&optional (emptydb nil))
  (setq *sessions* (make-hash-table))
  (when emptydb 
    (empty *appdb*)
    (decludes *appdb*))
  (setq *signatures* (make-hash-table))
  (setq *schemas* (make-hash-table))
  (setq *guards* (make-hash-table))
  (setq *updates* (make-hash-table))
  (setq *forms* (make-hash-table))
  (setq *tables* (make-hash-table))
  (setq *servlets* (make-hash-table))
  (setq *html* (make-hash-table))
  (setq *builtins* (make-hash-table))  ; resetting Epilog's builtins
  (setq *weblog-builtins* (make-hash-table))
  (setq *sorts* (make-hash-table))
  (setq *tests* nil)
  (setq *accesscontrol* nil)

  ; internal guard ensuring each cookie has a single session key
  (define-guard '__single-cookie-session `((=> (,*cookie-session-name* ?x) (,*cookie-session-name* ?y) (same ?x ?y))))

  ; internal guard to check against the global state
  (defguard __integrity :inherits (db cookie session __single-cookie-session))
  ; can all be re-defined by the app
  (defguard db)
  (defguard cookie)
  (defguard session)
; (defsort <name> <checker> <caster-from-string> <html-sanitizer>)
(defsort string to-strp to-str htmlify)
(defsort integer str-to-intp str-to-int int-noop)
(defsort boolean str-to-booleanp str-to-bool bool-noop)
(defsort html is-htmlp string-noop html-noop) 

; (defbuiltin <weblog-name> <lisp-function> <list of types of args> <list of types of returns> [<internal>])
;  Note that returning more than 1 value means returning a LIST with that many values
; supported types
;  Should be checking that the INTERNAL builtins are not used by developer
(defbuiltin to-strp to-strp string nil t)
(defbuiltin to-str to-str string string t)
(defbuiltin str-to-intp str-to-intp string nil t)
(defbuiltin str-to-int str-to-int string integer t)
(defbuiltin str-to-boolp str-to-boolp string nil t)
(defbuiltin str-to-bool str-to-bool string boolean t)
(defbuiltin is-htmlp is-htmlp string nil t)
(defbuiltin html-noop identity html html t)
(defbuiltin bool-noop identity boolean boolean t)
(defbuiltin string-noop identity string string t)
(defbuiltin integer-noop identity integer integer t)
(defbuiltin htmlify htmlify string string t)


; don't use - as our relation name since converting "-.0" to a symbol results in -0.0.
; functions
; (defbuiltin = same 2 0)  ; implemented directly
;(defbuiltin symbolize symbolize 1 1)
;(defbuiltin stringify stringify 1 1)
(defbuiltin plus + (integer integer) integer)
(defbuiltin minus - (integer integer) integer)
(defbuiltin times * (integer integer) integer)
(defbuiltin div (lambda (x y) (floor (/ x y))) (integer integer) integer)
(defbuiltin lt numlessp (integer integer) nil)
(defbuiltin lte numleqp (integer integer) nil)
(defbuiltin gte numgeqp (integer integer) nil)
(defbuiltin gt numgreaterp (integer integer) nil)
(defbuiltin currenttime get-universal-time nil integer) 

; Epilog's builtins (from base-new.lisp)
; (defprop word wordp basic)
; (defprop constant constp basic)
; (defprop indvar indvarpp basic)
; (defprop seqvar seqvarpp basic)
; (defprop variable varpp basic)
; (defprop id basicval basicval)
; (defprop stringify basicval basicval)
; (defprop symbolize basicval basicval)
; (defprop convertfromstring basicval basicval)
; (defprop nomination basicval basicval)
; (defprop denotation basicval basicval)
; (defprop integer integerp basic)
; (defprop realnumber realp basic)
; (defprop complexnumber complexp basic)
; (defprop number numberp basic)
; (defprop natural naturalp basic)
; (defprop rationalnumber rationalp basic)
; (defprop positive positivep basic)
; (defprop negative negativep basic)
; (defprop zero zeropp basic)
; (defprop odd oddintegerp basic)
; (defprop even evenintegerp basic)
; (defprop logbit logbitpp basic) 
; (defprop logtest logtest basic) 
; (defprop < numlessp basic)
; (defprop =< numleqp basic)
; (defprop >= numgeqp basic)
; (defprop > numgreaterp basic)
; (defprop less lessp basic)
; (defprop leq leqp basic)
; (defprop * basicvalarith basicval) 
; (defprop + basicvalarith basicval) 
; (defprop - basicvalarith basicval) 
; (defprop / basicvalarith basicval) 
; (defprop 1+ basicvalarith basicval) 
; (defprop 1- basicvalarith basicval) 
; (defprop abs basicvalarith basicval) 
; (defprop acos basicvalarith basicval) 
; (defprop acosh basicvalarith basicval) 
; (defprop ash basicvalarith basicval) 
; (defprop asin basicvalarith basicval) 
; (defprop asinh basicvalarith basicval) 
; (defprop atan basicvalarith basicval) 
; (defprop atanh basicvalarith basicval) 
; (defprop boole basicvalarith basicval) 
; (defprop ceiling basicvalarith basicval) 
; (defprop cis basicvalarith basicval) 
; (defprop complex basicvalarith basicval) 
; (defprop conjugate basicvalarith basicval) 
; (defprop cos basicvalarith basicval) 
; (defprop cosh basicvalarith basicval) 
; (defprop decode-float basicvalarith basicval) 
; (defprop denominator basicvalarith basicval) 
; (defprop exp basicvalarith basicval) 
; (defprop expt basicvalarith basicval) 
; (defprop fceiling basicvalarith basicval) 
; (defprop ffloor basicvalarith basicval) 
; (defprop float basicvalarith basicval) 
; (defprop float-digits basicvalarith basicval) 
; (defprop float-precision basicvalarith basicval) 
; (defprop float-radix basicvalarith basicval) 
; (defprop float-sign basicvalarith basicval) 
; (defprop floor basicvalarith basicval) 
; (defprop fround basicvalarith basicval) 
; (defprop ftruncate basicvalarith basicval) 
; (defprop gcd basicvalarith basicval) 
; (defprop imagpart basicvalarith basicval) 
; (defprop integer-decode-float basicvalarith basicval) 
; (defprop integer-length basicvalarith basicval) 
; (defprop isqrt basicvalarith basicval) 
; (defprop lcm basicvalarith basicval) 
; (defprop log basicvalarith basicval) 
; (defprop logand basicvalarith basicval) 
; (defprop logandc1 basicvalarith basicval) 
; (defprop logandc2 basicvalarith basicval) 
; (defprop logcount basicvalarith basicval) 
; (defprop logeqv basicvalarith basicval) 
; (defprop logior basicvalarith basicval) 
; (defprop lognand basicvalarith basicval) 
; (defprop lognor basicvalarith basicval) 
; (defprop lognot basicvalarith basicval) 
; (defprop logorc1 basicvalarith basicval) 
; (defprop logorc2 basicvalarith basicval) 
; (defprop logxor basicvalarith basicval) 
; (defprop max basicvalarith basicval) 
; (defprop min basicvalarith basicval) 
; (defprop mod basicvalarith basicval) 
; (defprop numerator basicvalarith basicval) 
; (defprop phase basicvalarith basicval) 
; (defprop rational basicvalarith basicval) 
; (defprop rationalize basicvalarith basicval) 
; (defprop realpart basicvalarith basicval) 
; (defprop rem basicvalarith basicval) 
; (defprop round basicvalarith basicval) 
; (defprop scale-float basicvalarith basicval) 
; (defprop signum basicvalarith basicval) 
; (defprop sin basicvalarith basicval) 
; (defprop sinh basicvalarith basicval) 
; (defprop sqrt basicvalarith basicval) 
; (defprop tan basicvalarith basicval) 
; (defprop tanh basicvalarith basicval) 
; (defprop truncate basicvalarith basicval)
; (defprop character characterp basic)
; (defprop alphabetic alphabeticp basic)
; (defprop uppercase uppercasep basic)
; (defprop lowercase lowercasep basic)
; (defprop digit digitp basic)
; (defprop alphanumeric alphanumberp basic)
; (defprop chargreater chargreaterp basic)
; (defprop charless charlessp basic)
; (defprop string stringp basic)
; (defprop substring substringp basic)
; (defprop stringgreater stringgreaterp basic)
; (defprop stringless stringlessp basic)
; (defprop stringmatchall stringmatchallp basic)
; (defprop stringmatchany stringmatchanyp basic)
; (defprop stringmatchphrase stringmatchphrasep basic)
; (defprop stringalphanumeric basicval basicval)
; (defprop stringappend basicval basicval)
; (defprop stringcapitalize basicval basicval)
; (defprop stringcharpos basicval basicval)
; (defprop stringdowncase basicval basicval)
; (defprop stringelement basicval basicval)
; (defprop stringlength basicval basicval)
; (defprop stringposition basicval basicval)
; (defprop stringsubleft basicval basicval)
; (defprop stringsubright basicval basicval)
; (defprop stringsubseq basicval basicval)
; (defprop stringsubstitute basicval basicval)
; (defprop stringupcase basicval basicval)
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Builtins
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun to-str (x) (write-to-string x))
(defun to-strp (x) (declare (ignore x)) t)

(defun str-to-intp (x)  ; tries to parse and if error returns NIL
  (handler-case (progn (parse-integer x) t)
    (condition () nil)))
(defun str-to-int (x)
  (handler-case
      (cond ((symbolp x) x)
	    ((stringp x) (parse-integer x))
	    (t 0))
    (condition () 0)))

(defun str-to-boolp (x)
  (or (equalp x "true") (equalp x "false")))
(defun str-to-bool (x)
  (handler-case
      (cond ((equalp x "true") 'true)
	    ((equalp x "false") 'false)
	    (x 'true)
	    (t 'false))))

(defun is-htmlp (x) 
  (if (htmlpurify-errorsp x) nil t))

(defun noop ())

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; End of Weblog initialization
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; must be called *after* all of the parsing macros and builtins are defined
; could be called after everything is defined, but conceptually the rest 
; is independent of this
(reset-weblog)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Low-level Analysis and Manipulation
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; make sure all the names of html,servlets,signatures,etc. exist.  Below is a start
; Ensure that every concatenated HTML file is XML
; Ensure that every link <a href=... in an HTML file that points to a servlet supplies parameters for a subset of one of the forms targeting that servlet
; Also when rendering, rewrite links to point to infomaster version of deployment
(defun check () (and (check-servlets) (check-html)))
(defun check-servlets (&optional (s nil))
  (let (errors)
    (dolist (x (if s (list (gethash s *servlets*)) (mapcar #'cdr (hash2bl *servlets*))))
      (unless (gethash (servlet-page x) *html*) 
	(push (format nil "In servlet ~A, undefined page ~A" x (servlet-page x)) errors))
      (dolist (y (servlet-guts x))
	(cond ((updatelist-p y)
	       (dolist (z (updatelist-updates y))
		 (unless (gethash z *updates*)
		   (push (format nil "In servlet ~A, undefined update ~A" y z) errors))))
	      (t
	       (dolist (z (guardlist-guards y))
		 (unless (gethash z *guards*)
		   (push (format nil "In servlet ~A, undefined update ~A" y z) errors)))))))
    (dolist (v (nreverse errors))
      (format t ";~A~%" v))
    (not errors)))

(defun check-html (&optional (h nil))
  (let (errors)
    (dolist (x (if h (list h) (mapcar #'cdr (hash2bl *html*))))
      (dolist (y x)
	(cond ((symbolp y) 
	       (unless (gethash y *html*) (push (format nil "In html ~A, undefined html ~A" x y) errors)))
	      ((and (listp y) (symbolp (first y)))
	       ; still ought to check that the form used for replacement is defined, but for now this is good enough.
	       (unless (gethash (first y) *html*) (push (format nil "In html ~A, undefined html ~A" x (first y)) errors))))))
    (dolist (v (nreverse errors))
      (format t ";~A~%" v))
    (not errors)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Mid-level Analysis and Manipulation
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; NOTE: some of this may make less sense now that we have a sequence of updates/guards for a servlet.
;   It was designed for servlets with a single guard and a single update (and in that order).
(defun servlet-guard-logic (servletname)
  "(SERVLET-GUARD-LOGIC SERVLETNAME) returns a list of all the constraints guarding the invocation of SERVLETNAME.
   Ignores guards that follow any update."
  (let (s)
    (setq s (servlet-guts (find-servlet servletname)))
    (apply #'append 
	   (mapcar #'(lambda (x) (contents (guard-fol (find-guard x))))
		   (if (guardlist-p (first s)) (guardlist-guards (first s)) nil)))))

(defun servlet-all-atoms (servletname)
  "(SERVLET-WEBSTATE-ATOMS SERVLETNAME) extractes the webstate predicates used by SERVLET
   and returns several values: 
   (i) db predicates, (ii) cookie predicates, (iii) session predicates, (iv) builtins, (v) remaining predicates."
  (let (updates all (*modals* '(pos neg)))
    (setq updates (mapcar #'(lambda (x) (update-datalog (find-update x))) (servlet-updates (find-servlet servletname))))
    (setq all (delete-duplicates (mapcan #'(lambda (x) (preds (maksand (contents x)))) updates) :test #'param-equal))
    (setq all (mapcar #'(lambda (x) (cons (parameter-symbol x) (nunique (parameter-arity x) "?"))) all))
    (setq all (group-by all #'in-what :key #'relation))
    (values (cdr (assoc :db all)) (cdr (assoc :cookie all)) (cdr (assoc :session all)) 
	    (cdr (assoc :builtin all)) (cdr (assoc :unknown all)))))

(defun servlet-known-atoms (servletname)
  "(SERVLET-KNOWN-ATOMS SERVLETNAME) returns a list of atoms pertinent to SERVLETNAME and 
   known by the user before SERVLETNAME's updates are run.  That is, returns the cookie atoms appearing in the updates,
   together with the servlet's inputs."  
  (nconc (servlet-input-atoms servletname) (nth-value 2 (servlet-all-atoms servletname))))

(defun servlet-empty-tables (servletname)
  "(SERVLET-EMPTY-TABLES SERVLETNAME) returns a list of relations pertinent to SERVLETNAME that are known to always be empty when
   SERVLETNAME begins its updates.  That is, all of the relations appearing in SERVELTNAME's updates except those
   for the DB, SESSION, COOKIE, or relations used as inputs to SERVLETNAME."
  (let (inputs updates all empty (*modals* '(pos neg)))
    (setq inputs (mapcar #'relation (servlet-input-atoms servletname)))
    (setq updates (mapcar #'(lambda (x) (update-datalog (find-update x))) (servlet-updates (find-servlet servletname))))
    (setq all (remove-duplicates (mapcan #'(lambda (x) (relns (maksand (contents x)))) updates)))
    (setq empty (set-difference all inputs))
    (setq empty (remove-if #'in-reserved-namespace empty))
    empty))

(defun servlet-input-signatures (servletname)
  (remove-duplicates (mapcarnot #'extract-signature-name (servlet-input-atoms servletname))))

(defun servlet-input-atoms (servletname)
  "(SERVLET-INPUT-ATOMS SERVLETNAME) returns a list of all atoms that may be given to SERVLETNAME as input,
   though if this servlet is used for multiple forms, it may be that there is no legitimate input that uses
   all the atoms returned.  Each atom has variables for all its arguments.  Each relation name is properly prefixed 
   with its signature.  Only one atom per prefixed relation is returned."
  (remove-duplicates
   (mapcan #'signature-to-atoms 
	   (mapcar #'(lambda (x) (schema-signature (find-schema (form-schema (find-form x))))) 
		   (servlet-formnames servletname)))
   :test #'equal))

(defun servlet-formnames (servletname)
  "(SERVLET-FORMNAMES SERVLETNAME) returns the list of all form names that use the given servletname as a target."
  (mapcarnot #'(lambda (x) (if (eq (form-target (cdr x)) servletname) (car x) nil)) (hash2bl *forms*)))

(defun servlet-output-atoms (servletname)
  "(SERVLET-OUTPUT-ATOMS SERVLETNAME) returns a list of atoms that the given SERVLETNAME may output,
   where each atom has variables for all its arguments.  Each relation name is properly prefixed 
   with its signature.  Only one atom per prefixed relation is returned."
  (remove-duplicates (mapcan #'signature-to-atoms (servlet-output-signatures servletname)) :test #'equal))

(defun signature-to-atoms (signame)
  "(SIGNATURE-TO-ATOMS SIG) takes a signature name and returns a list of atoms, one for each
   relation in that signature, properly prefixed.  All arguments to the atom are distinct variables."
  (mapcar #'(lambda (x) (signature-parameter-to-atom signame x)) (find-signature signame)))

(defun signature-parameter-to-atom (signame param &optional (starting 0))
  (cons (withnamespace (parameter-symbol param) signame) (nunique (parameter-arity param) "?" starting)))

(defun servlet-output-signatures (servletname) 
  "(SERVLET-OUTPUT-SIGNATURES SERVLETNAME) returns the list of signature names that this servlet outputs."
  (html-output-signatures (servlet-page (find-servlet servletname))))

(defun form-all-guards (form)
  "(FORM-ALL-GUARDS FORM) returns the list of all guards for this form."
  (when (symbolp form) (setq form (find-form form)))
  (guard-all-guards (union (form-guards form) (schema-guards (find-schema (form-schema form))))))

(defun table-all-guards (table)
  "(TABLE-ALL-GUARDS TABLE) returns the list of all guards for this table."
  (when (symbolp table) (setq table (find-table table)))
  (guard-all-guards (schema-guards (find-schema (table-schema table)))))

(defun update-all-guards (update)
  "(UPDATE-ALL-GUARDS UPDATE) returns the list of all guards for this update."
  (when (symbolp update) (setq update (find-update update)))
  (guard-all-guards (update-guards update)))

(defun guard-all-guards (guards) (guard-accum guards :key #'(lambda (x) (list (guard-name x)))))
(defun guard-accum-logic (guards) (guard-accum guards :key #'guard-logic))
(defun guard-accum-datalog (guards) (guard-accum guards :key #'guard-datalog))
(defun guard-accum (guards &key (key #'guard-logic))
  "(GUARD-ACCUM GUARD) recursively walks all guards inherited by all the guard names GUARDS
   and accummulates the results of applying KEY to each guard.  Can pass a single GUARD
   in place of a list of GUARDS.  Memoized."
  (unless (listp guards) (setq guards (list guards)))
  (let (h res)
    (setq h (make-hash-table))
    (dolist (g guards)
      (setq res (append (guard-accum-aux g key h) res)))
    (remove-duplicates res :test #'equal)))

(defun guard-accum-aux (guardname key done)
  "(GUARD-ACCUM-AUX GUARDNAME KEY DONE) takes a guard name GUARDNAME, a function KEY,
   and a hash table DONE. Returns the results of applying KEY to each guard inherited
   by GUARDNAME and memoizes results in DONE."
  (let (logic guard)
    (setq logic (gethash guardname done))
    (cond (logic logic)
	  (t (setq guard (find-guard guardname))
	     (setf (gethash guardname done) 
		   (append (apply #'append (mapcar #'(lambda (x) (guard-accum-aux x key done)) (guard-supers guard)))
			   (funcall key guard)))))))

(defun html-output-signatures (thing)
  "(HTML-OUTPUT-SIGNATURES THING) takes an html descriptor (string, symbol, htmlrepl) 
   and returns the list of signature names that are output by that thing.  Assumes no forms/tables are
   replaced more than once and that every form/table occurs in the HTML."
  (cond ((stringp thing) nil)
	((symbolp thing) (remove-duplicates (mapcan #'html-output-signatures (flatten-html thing))))
	((htmlrepl-p thing) 
	 (mapcar #'(lambda (x) (schema-signature (find-schema x)))
		 (union 
		  (mapcar #'(lambda (x) (form-schema (find-form (second x)))) (htmlrepl-forms thing)) 
		  (mapcar #'(lambda (x) (find-table (second x))) (htmlrepl-tables thing)))))	
	(t (error 'invalid-html :comment thing))))    

(defun flatten-html (thing)
  "(FLATTEN-HTML THING) takes an HTML descriptor (string, symbol, htmlrepl) and returns a list
   of strings and htmlrepls representing that descriptor, i.e. it eliminates all symbol references
   and in so doing flattens the description."
  (cond ((stringp thing) (list thing))
	((symbolp thing) (mapcan #'flatten-html (find-html thing)))
	((htmlrepl-p thing) (mapcar #'(lambda (x) (flatten-html-add-repl x thing)) (flatten-html (htmlrepl-target thing))))
	(t (error 'invalid-html :comment thing))))

(defun flatten-html-add-repl (thing repl)
  "(FLATTEN-HTML-ADD-REPL THING REPL) Helper for flatten-html.  Adds the replacements from htmlrepl REPL
   to the thing, always returning an htmlrepl."
  (cond ((or (stringp thing) (symbolp thing)) 
	 (make-htmlrepl :target thing :forms (htmlrepl-forms repl) :tables (htmlrepl-tables repl)))
	((htmlrepl-p thing)
	 (make-htmlrepl :target (htmlrepl-target thing)
			:forms (append (htmlrepl-forms repl) (htmlrepl-forms thing))
			:tables (append (htmlrepl-tables repl) (htmlrepl-tables thing))))
	(t (error 'invalid-html :comment thing))))

(defun updates-to-views (l)
  "(UPDATES-TO-VIEWS L) takes a list of datalog updates and returns a single set of view definitions that are
   logically equivalent to that sequence of updates.  That is, if the updates effect p there is some predicate
   pn such that its contents is the same as p after applying the updates.  The second return value (a hash table from 
   a predicate p to a positive integer n or NIL) allows us to compute a pn that was updated: (tosymbol p (car (gethash p h))).
   If the hash returns NIL, p was not updated and hence p's value after the updates is just p."
  (setq l (append l '(nil)))
  (do ((ls (cdr l) (cdr ls))
       (up (contents (car l)))
       (h (make-hash-table))
       (i 1 (1+ i)))
      ((null ls) (values up h))
    (setq up (updates-to-views-aux up (contents (first ls)) i h))))

(defun updates-to-views-aux (up1 up2 base latest)
  "(UP1 UP2 BASE LATEST) takes 2 lists of datalog updates UP1 and UP2.  UP2 is to be applied after
   UP1.  Also takes BASE, a positive integer dictating where in the sequence of updates UP1 lies.
   Also takes and updates LATEST, a hash table from relations to the greatest BASE wherein
   each relation was last updated via a POS/NEG rule.  Returns a single update equivalent to
   updating UP1 and then UP2 (and modifies LATEST)."
  (let (newth newps h pbase pos neg f p oldp arity bl head th oldhead)
    ; split up1 into pos/neg (hashed on relation) and remainder
    (setq h (make-hash-table))
    (dolist (phi up1)
      (cond ((member (relation phi) '(pos neg))
	     (setq p (relation (second (second phi))))
	     (setf (gethash p h) (cons phi (gethash p h))))
	    (t
	     (push phi newth))))
    ; add pbase definitions for every p with a pos/neg
    (dolist (pposneg (hash2bl h))
      ; figure out what relation to use, and update LATEST as needed
      (setq p (car pposneg))
      (setq oldp (gethash p latest))
      (if oldp (setq oldp (tosymbol p (car oldp))) (setq oldp p))
      (setq pbase (tosymbol p base))
      (push base (gethash p latest))
      (push (cons p pbase) bl)
      ; prepare to combine pos and neg sentences
      (setq arity (length (cdr (second (head (first (cdr pposneg)))))))
      ; ensure combining bodies of pos/neg into a single sentence does not capture variables improperly
      (multiple-value-setq (head th) (heads-same-bodies-diff (cdr pposneg)))
      (setq head (subrel `((,p . ,pbase)) head))
      ; predicate complete bodies of pos and neg sentences
      (multiple-value-setq (pos neg) (split #'posp th))
      (setq f #'(lambda (x) (maksand (body x))))
      (setq pos (maksor (mapcar f pos)))
      (setq neg (maksor (mapcar f neg)))
      ; create sentence
      (setq oldhead (if (> arity 0) (cons oldp (cdr head)) oldp))
      (push `(<= ,head (or ,pos (and ,oldhead (not ,(equantify-except neg (vars head)))))) newps))
    ; convert new sentences to datalog
    ;(setq newps (mapcan #'to-canonical-datalog newps))
    ;;(setq newps (mapcar #'(lambda (x) (if (and (listp x) (eq (car x) '<=)) (cons '<= (delete= (cdr x))) x)) newps))
    ;(setq newps (mapcar #'order-datalog-conjuncts newps))
    ; apply substitutions to up2
    (setq bl (mapcar #'(lambda (x) (cons (car x) (tosymbol (car x) (car (cdr x))))) (hash2list latest)))
    (setq up2 (mapcar #'(lambda (x) (subrel bl x)) up2))
    ; return combination of new up2, newth, newps
    (append newth newps up2)))
      
(defun compose-posneg (l &optional (empty nil))
  "(COMPOSE-POSNEG L) takes a list of datalog updates and composes them.  
   EMPTY is a list of predicates known to be empty before updates are applied.  Returns: 
   (i) a set of view definitions representing the sequence of updates (without pos/neg). 
   (ii) a set of pos/neg definitions that when added to (i) produces a single update equivalent to the original sequence.
   (iii) a set of view definitions that when added to (i) yields definitions for p* for all updated relations p.
   (iv) a binding list from old relations to new relations describing how relations of (iii) relate to original relations.
   (v, vi) 2 hash tables keyed on relations where each value is a list of positive integers at which those relations have update views defined in (i)."
  (let (pos neg i newth vocab args posneg views param modified oldhead newhead poshead neghead bl (*propositions* t))
    (setq pos (make-hash-table))
    (setq neg (make-hash-table))     
    ; turn sequence of (pos p) and (neg p) theories into a single theory without pos/neg
    (setq i 1)
    (setq newth nil)
    (dolist (x l)
      (setq newth (nconc (compose-posneg-aux x i pos neg empty) newth))
      (setq i (1+ i)))
    ; construct additional rules (to create single update and to create view definitions)
    (setq vocab (proppreds (maksand newth)))
    (setq modified (hash2bl (compose-posneg-prior-hash pos neg)))
    ; construct single, top-level pos/neg for each relation
    (dolist (x modified)
      (when (second x)
	(setq param (find (second x) vocab :key #'parameter-symbol))
	(cond ((isproposition param) 
	       (push `(<= (pos ,(first x)) ,(second x)) posneg))
	      (t
	       (setq args (maptimes #'newindvar (parameter-arity param)))
	       (push `(<= (pos ,(cons (first x) args)) ,(cons (second x) args)) posneg))))
      (when (third x)
	(setq param (find (third x) vocab :key #'parameter-symbol))
	(cond ((isproposition param)
	       (push `(<= (neg ,(first x)) ,(third x)) posneg))
	      (t
	       (setq args (maptimes #'newindvar (parameter-arity param)))
	       (push `(<= (neg ,(cons (first x) args)) ,(cons (third x) args)) posneg)))))
    ; construct view definitions
    (dolist (x modified)
      (if (second x)
	  (setq param (find (second x) vocab :key #'parameter-symbol))
	  (setq param (find (third x) vocab :key #'parameter-symbol)))
      (cond ((isproposition param)
	     (setq oldhead (first x))
	     (setq newhead (tosymbol (first x) '*))
	     (setq poshead (second x))
	     (setq neghead (third x)))
	    (t
	     (setq args (maptimes #'newindvar (parameter-arity param)))
	     (setq oldhead (cons (first x) args))
	     (setq newhead (cons (tosymbol (first x) '*) args))
	     (setq poshead (cons (second x) args))
	     (setq neghead (cons (third x) args))))
      (push `(,(relation oldhead) . ,(relation newhead)) bl)
      (when (member (first x) empty) (setq oldhead 'false))
      (unless (second x) (setq poshead 'false))
      (unless (third x) (setq neghead 'false))
      (push `(<= ,newhead ,(makor (makand oldhead (maknot neghead)) poshead)) views))
    (values newth posneg views bl pos neg)))

(defun compose-posneg-aux (up base pos neg empty)
  "(COMPOSE-POSNEG-AUX UP BASE POS NEG) takes a single datalog update UP along with BASE, a positive integer 
   that when appended to relations being updated yields a fresh relation.
   Also takes and updates two hash tables POS and NEG that maps a relation name to
   a list of BASEs where that relation has been updated positively or negatively, resp.
   (the head of the list is assumed to be the latest BASE at which it was updated).
   Returns a set of view definitions where the update to relation R is named RBASE
   (and modifies POS/NEG appropriately).  If no updates occur  "
  (let (newth h rest r newhead posphi negphi posphihead posphir negphihead negphir lastposr lastnegr)
    ; fix rule bodies to account for prior updates 
    (setq up (compose-posneg-priors up pos neg empty))
    (multiple-value-setq (h rest) (posneg-predicate-completion-bl up nil))
    ; create definition for the formulas describing the current predicate-completed updates 
    (dolist (x h)
      (setq r (first x))
      (setq newhead (second x))
      (setq posphi (third x))
      (setq negphi (fourth x))
      (multiple-value-setq (lastposr lastnegr) (compose-posneg-last-relations r pos neg))
      ; add definitions for each pos/neg formula arising from this update (that are non-false)
      ;   and record that there are such updates
      (setq posphihead nil) ; if still NIL after block, know no inserts to R
      (unless (eq posphi 'false)	
	(setq posphir (tosymbol r base "_pos_phi"))
	(setq posphihead (subrel `((,r . ,posphir)) newhead)) 
	(push `(<= ,posphihead ,posphi) newth)
	(push base (gethash r pos)))
      (setq negphihead nil)  ; if still NIL after block, know no deletes from R
      (unless (eq negphi 'false)
	(setq negphir (tosymbol r base "_neg_phi"))
	(setq negphihead (subrel `((,r . ,negphir)) newhead))
        (push `(<= ,negphihead ,negphi) newth)
	(push base (gethash r neg)))
      ; add definitions for pos/neg accounting for this update and previous updates
      (setq newth (nconc (compose-posneg-final r base newhead posphihead negphihead lastposr lastnegr) newth)))
    (nconc newth rest)))


(defun compose-posneg-priors (up pos neg empty)
  "(COMPOSE-POSNEG-PRIORS UP POS NEG) reformulates a set of datalog rules UP so that bodies of rules account for prior updates:
   p(x) if updated previously by pk_pos(x) and pk_neg(x) becomes p(x) ^ -pk_neg(x) | pk_pos(x).  
   If either pk_pos or pk_neg does not exist, simplifies accordingly."
  (let (h newup)
    ; create hash reflecting prior updates
    (setq h (compose-posneg-prior-hash pos neg))
    (dolist (u (contents up) newup)
      (push (mapbody #'(lambda (bod)
			 (mapatoms #'(lambda (x) 
				       (let (pn p n r patom natom)
					 (setq r (relation x))
					 (setq pn (gethash r h))
					 (cond ((not pn) x)
					       (t
						(setq p (first pn))
						(setq n (second pn))
						(setq patom (if p (subrel `((,r . ,p)) x) 'false))
						(setq natom (if n (subrel `((,r . ,n)) x) 'false))
						(setq x (if (member r empty) 'false x))
						(makor (makand x (maknot natom)) patom)))))
				   bod))
		     u) newup))))
      
(defun compose-posneg-final (r base newhead posphihead negphihead lastposr lastnegr)
  "(FLATTEN-UPDATES-BODY BODY LATEST) takes the BODY of a pos/neg rule and any occurrence of a relation that
   has been updated so far is replaced with a complex statement that reflects those updates.  Note: does
   not return canonical datalog even if given canonical datalog."
  (let (lastposhead lastneghead newposhead newneghead th)
    (setq lastposhead (subrel `((,r . ,lastposr)) newhead))
    (setq lastneghead (subrel `((,r . ,lastnegr)) newhead))
    (setq newposhead (subrel `((,r . ,(compose-posneg-pos-rel r base))) newhead))
    (setq newneghead (subrel `((,r . ,(compose-posneg-neg-rel r base))) newhead))
    (when posphihead
      (if lastposr
	  (push `(<= ,newposhead (or ,posphihead (and ,lastposhead (not ,negphihead)))) th)
	  (push `(<= ,newposhead ,posphihead) th)))
    (when negphihead
      (if lastnegr
	  (push `(<= ,newneghead (or (and ,negphihead (not ,posphihead)) (and ,lastneghead (not ,posphihead)))) th)
	  (push `(<= ,newneghead (and ,negphihead (not ,posphihead))) th)))
    th))

(defun compose-posneg-prior-bl (pos neg) (hash2bl (compose-posneg-prior-hash pos neg)))
(defun compose-posneg-prior-hash (pos neg)
  "(COMPOSE-POSNEG-PRIOR-HASH POS NEG) returns a hash table keyed on relations where each value is (<lastposrelation> <lastnegrelation>)." 
  (let (h)
    (setq h (make-hash-table))
    (mapc #'(lambda (x) (multiple-value-bind (p n) (compose-posneg-last-relations x pos neg) (setf (gethash x h) (list p n)))) 
	  (union (hash2keys pos) (hash2keys neg)))
    h))

(defun compose-posneg-last-relations (r pos neg)
  "(COMPOSE-POSNEG-LAST-RELATIONS R POS NEG) takes a relation R and returns two values: the
   last positive update to R and the last negative update to R.  One or both may be NIL, indicating
   there was no last update."
  (setq pos (car (gethash r pos)))
  (setq neg (car (gethash r neg)))
  (when (and pos neg)
    (when (< pos neg) (setq pos nil))
    (when (< neg pos) (setq neg nil)))
  (when pos (setq pos (compose-posneg-pos-rel r pos)))
  (when neg (setq neg (compose-posneg-neg-rel r neg)))
  (values pos neg))

(defun compose-posneg-neg-rel (r base) (tosymbol r base "_neg"))
(defun compose-posneg-pos-rel (r base) (tosymbol r base "_pos"))


;;; types
(define-condition relation-type-error (error) ((comment :initarg :comment :accessor comment)))
(defvar *type-explanations* nil "Data structure constructed during type inference")
(defun explain-type (x) (if (hash-table-p *type-explanations*) (gethash x *type-explanations*) nil))

(defun infer-types ()
  "(INFER-TYPES walks over all the servlets, infers types for all occurring symbols, and writes
   those types back to the *signatures* datastructure or throws an error if a type-mismatch
   occurred."
  (let (types)
    (setq types (type-inference))
    (when (listp types) 
      (print types)
      (error 'relation-type-error :comment types))
    (install-types types)))

(defun install-types (types)
  (let (mytypes signame)
    (dolist (sigbl (hash2bl *signatures*))
      ; walk over parameters
      (setq signame (car sigbl))
      (format t "Fixing signature ~A~%" signame)
      (dolist (param (cdr sigbl))
	; grab the types of all arguments, defaulting to string when there are none
	(format t "Fixing parameter ~A~%" param)
	(setq mytypes nil)
	(dotimes (i (parameter-arity param))
	  (push (or (gethash (withnamespace (relation-arg-to-name (parameter-symbol param) i) signame) types) 'string) mytypes))
	(setf (parameter-type param) (nreverse mytypes))))))

(defun type-inference ()
  "(TYPE-INFERENCE) does type inference over all of the updates being used by some servlet.
   Either returns a List of errors or a hash-table with types for every signature.  It is conceivable
   that we could infer more types by analyzing guards as well---but note that we would want to analyze
   all guards used by a servlet, a form, or a schema used by a form/servlet.  Defaults to STRING."
  (let (equations undeclared alltypes errs declaredtypes)
    ; create equations for each update used by some servlet
    ; don't process the same update more than once
    (setq equations (type-inference-equations))
    ;(print equations)
    (multiple-value-setq (declaredtypes undeclared) (type-inference-declared-types))
    ;(print (hash2bl declaredtypes))
    ; run type-inference and either return errors or fill out unknown types and return type assignments
    (multiple-value-setq (alltypes errs) (type-inference-core declaredtypes equations))
    (cond (errs errs)
	  (t (dolist (u undeclared)   ;(set-difference undeclared (mapcar #'car (hash2bl alltypes))))
	       (unless (gethash u alltypes) (setf (gethash u alltypes) 'string)))
	     alltypes))))

(defun type-inference-equations ()
  "(TYPE-INFERENCE-EQUATIONS) analyze the servlets to extract type-equations from 
   the updates used by the servlets.  Returns a list where each element is of the form (= <rel-arg> <rel-arg> <source>)."
  (let (done equations)
    (setq done (make-hash-table))
    ; walk over servlets and process each update
    (dolist (servlet (hash2bl *servlets*))
      (dolist (gut (servlet-guts (cdr servlet)))
	(when (updatelist-p gut)
	  (dolist (updname (updatelist-updates gut))
	    (unless (gethash updname done)
	      (setq equations (nconc (mapcan #'(lambda (x) (type-inference-extract-equations x updname)) 
					     (update-logic (find-update updname)))
				     equations))
	      (setf (gethash updname done) t))))))
    equations))

(defun type-inference-extract-equations (p name)
  "(TYPE-INFERENCE-EXTRACT-EQUATIONS P NAME) returns a list of (= x y (update NAME P))
   where (i) x and y are relation-arg-names where the same variable appears in P in
   locations x and y (or variables appearing there are deemd =) and (ii)  
   (update NAME P) allows us to reconstruct where this sentence originated from."
  (let (equations)
    (if (and (listp p) (eq (car p) '<=))
	(setq equations (nconc (type-inference-extract-equations-core (maksand (body p)))
			       (type-inference-extract-equations-core (drop-posneg (head p)))))
	(setq equations (type-inference-extract-equations-core p)))
    (setq equations (delete= equations))   ; turns ((= ?x p.1) (= ?x q.2) (= ?y p.3)) into ((= p.1 q.2))
    (mapcar #'(lambda (x) (list '= (second x) (third x) `(:update ,name :rule ,p))) equations)))

(defun type-inference-extract-equations-core (p)
  (cond ((atom p) nil)
	((member (car p) '(=> <= <=> not and or)) (mapcan #'type-inference-extract-equations-core (cdr p)))
	((member (car p) '(forall exists)) (type-inference-extract-equations-core (third p)))
	(t (do ((ps (cdr p) (cdr ps))
		(i 0 (1+ i))
		(eqns))
	       ((null ps) eqns)
	     (when (varp (car ps)) (push `(= ,(relation-arg-to-name (car p) i) ,(car ps)) eqns))))))  

(defun type-inference-declared-types ()
  "(TYPE-INFERENCE-DECLARED-TYPES) looks for all the types of relation-arguments and returns 2 values:
   a hash table from relation-arg-to-name to the type and a list of undeclared relation-arguments.
   Handles both signatures and builtins; does not inspect constraints."
  (let (declaredtypes signame undeclared)
    (setq declaredtypes (make-hash-table))
    ; signatures
    (dolist (sigbl (hash2bl *signatures*))
      ; walk over parameters
      (setq signame (car sigbl))
      (dolist (param (cdr sigbl))
	; either record that the relation's args have no declared types, or add the declarations to our hash
	(if (not (parameter-type param))
	    (dotimes (i (parameter-arity param))
	      (push (relation-arg-to-name (withnamespace (parameter-symbol param) signame) i) undeclared))
	    (do ((types (parameter-type param) (cdr types))
		 (i 0 (1+ i)))
		((null types))
	      (setf (gethash (relation-arg-to-name (withnamespace (parameter-symbol param) signame) i) declaredtypes) (car types))))))
    ; builtins
    (dolist (b (hash2bl *weblog-builtins*))
      (setq b (cdr b))
      ; arguments to builtin
      (do ((types (builtin-args b) (cdr types))
	   (i 0 (1+ i)))
	  ((null types))
	(setf (gethash (relation-arg-to-name (builtin-name b) i) declaredtypes) (car types)))
      ; returns from builtin
      (do ((types (builtin-returns b) (cdr types))
	   (i (length (builtin-args b)) (1+ i)))
	  ((null types))
	(setf (gethash (relation-arg-to-name (builtin-name b) i) declaredtypes) (car types))))
    (values declaredtypes undeclared)))      

(defun type-inference-core (declaredtypes equations)
  "(TYPE-INFERENCE DECLAREDTYPES EQUATIONS) runs type-inference on EQUATIONS, which is a list of
   (= rel1.arg rel2.arg rule) and a hash table DECLAREDTYPES that dictates the declared types
   of the relk.arg that are known.  Infers the types of the remaining relk.arg elements.  If type
   errors occur (because a relk.arg is assigned more than one), records reason for that error.
   Returns new hash with all type assignments, list of errors, list of unassigned relk.args."
  ; equations start: (eq ?x ?y rule)
  (let (errs alltypes history unassigned)
    ;(print equations)
    (setq equations (copy-tree equations))
    ; copy declaredtypes into alltypes
    (setq alltypes (bl2hash (hash2bl declaredtypes)))
    ; record rationale for each variable assignment
    (setq history (make-hash-table))
    (maphash #'(lambda (key val) (declare (ignore val)) (setf (gethash key history) 'declared)) alltypes)
    ; walk over equations; infer new types; record rationale; report conflicts
    (do ((foundnewtype t) (x) (y) (id) (xval) (yval))
	((not foundnewtype))
      (setq foundnewtype nil)
      ;(print equations)
      (dolist (p equations)
	(unless (eq (first p) :done)  ; only analyze if haven't already processed
	  (setq x (second p))   ; the variable
	  (setq y (third p))  ; the variable
	  (setq id (fourth p)) ; the source of this binding
	  (setq xval (gethash x alltypes))  ; value stored for x 
	  (setq yval (gethash y alltypes))  ; value stored for y
	  ;(format t "x: ~A, xval: ~A, y: ~A, yval: ~A~%" x xval y yval)
	  (cond ((and xval yval) 
		 (setf (first p) :done)
		 (unless (eq xval yval)
		   (push `(err (explanation (binds ,id ,x ,y) 
						   (bound ,x ,xval ,(gethash x history)) 
						   (bound ,y ,yval ,(gethash y history))))
			 errs)))
		(xval
		 (setf (gethash y alltypes) xval) 
		 (setf (gethash y history) `(explanation (binds ,id ,x ,y) (bound ,x ,xval ,(gethash x history)) (unbound ,y)))
		 (setq foundnewtype t))
		(yval
		 (setf (gethash x alltypes) yval) 
		 (setf (gethash x history) `(explanation (binds ,id ,x ,y) (bound ,y ,yval ,(gethash x history)) (unbound ,x)))
		 (setq foundnewtype t))))))
    ; compute unassigned relk.args
    (setq *type-explanations* history)
    (dolist (e equations)
      (unless (eq (first e) :done)
	(unless (gethash (second e) alltypes) (push (second e) unassigned))
	(unless (gethash (third e) alltypes) (push (third e) unassigned))))
    ; return type assignments, errors, unassigned
    (values alltypes errs unassigned)))
  
(defun relation-arg-to-name (pred argidx) (read-user-string (format nil "~A.~A" pred argidx)))
(defun name-to-relation-arg (s) 
  (let (idx)
    (setq s (tostring s))
    (setq idx (position #\. s :from-end t))
    (values (tosymbol (subseq s 0 idx)) (tosymbol (subseq s (1+ idx))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Security
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun instrument ()
  "(INSTRUMENT) performs a number of global manipulations of a weblog app."
  (eliminate-parameter-tampering)  ; ensures every servlet services at most 1 form and that every servlet checks all the form's guards
  (eliminate-cross-site-scripting) ; performs type inference (throwing errors if types don't check) and then adds sanitizers to end of servlets
  ;(access-control) ; not stable enough to put here---should be computing new guards to add to each servlet and installing them at the start
  (insert-type-checking-and-coersion) ; only installs declared slots: handles case when servlet services > 1 form
)
; DO A SEARCH FOR ALL HTMLSTRING/STRING/INTEGER/BOOLEAN AND REPLACE WITH THE DEFSORT STUFF

; to eliminate XSS, we (1) throw out any data that doesn't match its type (which is where we catch
;   people trying to write unsafe html in HTMLstring entries) and (2) ensure that any data sent to the
;   browser is of a known type (number, boolean, htmlstring) or is HTML-encoded (for strings)
; For (1), we simply enforce types of inputs before turning control over to user code.
; For (2), we need to sanitize STRING data before sending it to the browser.  Option (a) is to figure out
;   which data needs to be sanitized when it is sent to the browser.  This is easy as long as we know the types
;   of outputs sent to the browser.  Simply allowing the developer to declare input/output/db types is insufficient, however
;   because an input could be declared string but then the db/output could be declared HTML, thereby circumventing the
;   sanitization that ought to have been performed on the string.
;   So at least, we need type checking to ensure that no data ever has its type changed.  Of course, in practice, we need
;   to cast from time to time, so to preserve XSS elimination, we must ensure there is no way to cast from STRING to HTML.
;   Since we are in control of the builtins, this should be no problem.  More generally, we need to ensure that data with an untrusted type is
;   never cast to a trusted type.  That said, it is quite a burden to ask the developer to declare input/output/db types
;   all to be the same, so type inference is a real help.
;   Option (b) is to sanitize the data before it is stored in the DB. The problem with this is that it breaks our security-as-an-afterthought
;   paradigm.  If we sanitize anytime before a string is sent to the browser, it is possible that the user's code will operate differently
;   with and without the XSS defense in place.  We can't sanitize the inputs before the user gets it because while writing to the DB, they may inspect
;   the string; we can't sanitize it just before saving to the DB because the servlet outputing the data may inspect the internals.  That leaves one
;   place: just before being sent to the browser. So this really isn't an option.
; Notice that by eliminating unsafe data on the input and sanitizing string on the output, we do the expensive thing only on input (htmlpurify) 
;   and we do the cheap thing (linear time) on output: html-encode.
; Finally, the reason XSS is different than SQLi is that there are many web apps where we never want user data to contain SQL commands; however,
;   there are many fewer web apps where we never want the user to input HTML.  So while we can eliminate SQL commands from user-data, we cannot
;   in practice eliminate HTML commands from user data.  (We DO actually have the analog of SQL prepare statements: HTML-encoding.)

(defun eliminate-cross-site-scripting ()
  "(ELIMINATE-CROSS-SITE-SCRIPTING) First type checks (with inference) to ensure no implicit casting.  Then checks that 
   there are no explicit type casters (builtins) that move trusted types to untrusted types.  Finally adds a sanitizer
   to the end of every servlet for the untrusted types."
  ; do type checking (as well as inference) and ensure no implicit casting
  (infer-types)
  ; check no builtin that does explicit casting that changes one unsafe type to a different type
  ;  Don't know how to do this.  For single arg-> single arg, easy.  For relations easy.  But for String x Boolean -> Integer, what do we say?
  ;  Maybe just skip this check and rely on the safety of the builtins b/c WE control them.
  ;  (dolist (b (hash2bl *weblog-builtins*))
  ;    (setq b (cdr b))
  ;    (unless (builtin-internal b)
  ;      (when ( ))))
  ; add sanitizer to end of every servlet for untrusted types (and for all data headed to a form).
  ; May be more involved than I thought b/c data can be used in different contexts; hence, we need to create copies of data,
  ;     and sanitize each according to its context.  See paper for a few more details.
)

; If (infer-types) is run, do it *before* this, so we'll check all the right user-data types
(defun insert-type-checking-and-coersion ()
  "(INSERT-TYPE-CHECKING-AND-COERSION) adds guards/updates to each servlet so that
   regardless which form data is submitted to the servlet, the application first checks
   the types of the data (rejecting in the usual way upon violation), and then 
   casts the string representation of that data (as per HTTP) into the proper type.
   It does this by adding guards/updates to the front of the servlet."
  (mapc #'(lambda (x) (insert-type-checking-and-coersion-servlet (car x))) (hash2bl *servlets*)))

(defun insert-type-checking-and-coersion-servlet (servletname)
  (let (inputsigs servlet newguards newupdates)
    (setq servlet (find-servlet servletname))
    (setq inputsigs (servlet-input-signatures servletname))
    (mapc #'create-signature-type-check-and-coersion inputsigs)
    (dolist (sig inputsigs)
      (push (signature-typeguard-name sig) newguards)
      (push (signature-typeupdate-name sig) newupdates))
    ; remember to push the updates before the guards so that we're coersing only *after* we're checking
    (push (make-updatelist :updates newupdates) (servlet-guts servlet))
    (push (make-guardlist :guards newguards) (servlet-guts servlet))))
    
; compute a guard to check that we have been passed the proper type for all inputs
(defun signature-typeguard-name (signame) (tosymbol "__" signame "_types"))
(defun signature-typeupdate-name (signame) (tosymbol "__" signame "_types"))
(defun create-signature-type-check-and-coersion (signame)
  (let (guardth updateth atom atom2 part sort)
    (setq guardth nil)
    (setq updateth nil)
    (dolist (p (find-signature signame))  ; a list of parameters
      (setq atom (signature-parameter-to-atom signame p))
      (setq atom2 (signature-parameter-to-atom signame p (parameter-arity p)))
      (setq part nil)
      ;(format t "parameter: ~A~%" p)
      (do ((vs (cdr atom) (cdr vs))
	   (vs2 (cdr atom2) (cdr vs2))
	   (typenames (parameter-type p) (cdr typenames))
	   (i 1 (1+ i)))
	  ((null typenames))
	;(format t "param name: ~A, typename: ~A~%" (parameter-symbol p) (car typenames))
	; check: push comment and then sentence so they appear in the right order
	(unless (eq (car typenames) 'string)
	  (setq sort (gethash (car typenames) *sorts*))
	  (unless sort (error 'relation-type-error :comment (format nil "unknown type ~A for ~A" (car typenames) p)))
	  (push (format nil "Argument ~A to ~A must be of type ~A" i (car atom) (car typenames)) guardth)
	  (push (list '=> atom (list (sort-checker sort) (car vs))) guardth)
					; conjunction: variable = conjucnts for coersion
	  (push (list (sort-caster sort) (car vs) (car vs2)) part))
      ; coersion: push pos and neg versions so that old data is deleted and new data is saved
      ;   if old=new, then pos wins over neg, so new/old is still saved
	(when part
	  (push (list* '<= `(pos ,atom2) atom part) updateth)
	  (push (list '<= `(neg ,atom) atom) updateth))))
    (when (in-db signame) (setq guardth (list* :inherits '(db) guardth)))
    (when (in-session signame) (setq guardth (list* :inherits '(session) guardth)))
    ; why no in-cookie check?
    (define-guard (signature-typeguard-name signame) guardth)
    (define-update (signature-typeupdate-name signame) nil updateth)))


; need to add negative parameter tampering defense as well: grab the preds the servlet updates and outputs and ensure that the user
;   does not supply any of those except those belonging to the signature.
;   On second thought, we're already defended against negative parameter tampering because we're prepending the signature name
;   for this form to all of the field names; so the user can't insert new keys into the global namespace---only into the
;   form's namespace, which will be ignored by the code anyhow (right??).
; Need to extract range for form fields from constraint dictating it must be in a DB slice
(defun eliminate-parameter-tampering ()
  "(ELIMINATE-PARAMETER-TAMPERING) transforms a web application so that every guard checked on a form is also checked
   by its target servlet."
  (let (s form)
    ; first ensure every servlet is the target of at most 1 form.
    (uniquify-form-servlet-combinations)
    ; walk over forms and add the guards of each form to the guards of its target
    (dolist (f (hash2bl *forms*))
      (setq form (cdr f))
      (setq s (find-servlet (form-target form)))
      (setf (servlet-guts s) (cons (make-guardlist :guards (form-all-guards form)) (servlet-guts s))))))

(defun uniquify-form-servlet-combinations ()
  "(UNIQUIFY-FORM-SERVLET-COMBINATIONS) ensures that each form has a unique servlet target."
  (let (forms counter fs servletname newname)
    (setq forms (group-by (hash2bl *forms*) #'form-target :key #'cdr))
    (dolist (f forms)
      (setq servletname (car f))
      (setq fs (cdr f))  ; a list of (formname . <form-struct>)
      (setq counter 0)
      (dolist (g (cdr fs)) ; walk over all but the first form
	; create a new servlet that is identical to the original but with a different name
	(setq newname (tosymbol "__" servletname counter))
	(duplicate-servlet servletname newname)
	(setq counter (1+ counter))
	; change the form so it points to that servlet
	(setf (form-target (cdr g)) newname)))))


(defvar *session.username* 'session.username)
(defvar *session.username-uniqueness* `(=> (,*session.username* ?x) (,*session.username* ?y) (= ?x ?y)))

; NOTE: can definitely do better if we take the guards into account when reasoning about the updates
;   Currently, we're ignoring all the guards (I think).  We're certainly not reasoning about the fact that guards 
;   can be interspersed with updates.
(defun access-control (servletname &optional (onlycheck nil))
  "(ACCESS-CONTROL SERVLETNAME ONLYCHECK) checks the servlet named SERVLETNAME against the access control policy.
   ONLYCHECK can be set to :allow or :deny to check only those statements."
  (let (ac read write support updates raw posneg views bl input output db cookie session builtins appatoms useratoms)
    ; alter AC so that each allow/deny rule has (i) an object atom with all distinct variable args and (ii) a body with a single atom
    ;    whose arguments are exactly those of the object atom 
    (setq ac (canonicalize-access-control *accesscontrol*))
    ; divide AC by rights; also keep all remaining rules around.
    (setq ac (group-by ac 
		       #'(lambda (x) (if (not (member (relation x) '(allow deny))) 'support (third (head x))))
		       :test #'equal))
    (setq read (cdr (assoc 'read ac)))
    (setq write (cdr (assoc 'write ac)))
    (setq support (cdr (assoc 'support ac))) 
    ; compose update sequence
    (setq updates (mapcar #'(lambda (x) (update-datalog (find-update x))) (servlet-updates (find-servlet servletname))))
    (multiple-value-setq (raw posneg views bl) (compose-posneg updates (servlet-empty-tables servletname)))
    (when *debug-webapps* (format t "~&Composed updates:~%") (pcontents raw))
    ; assemble signatures of interest
    (multiple-value-setq (db cookie session builtins) (servlet-all-atoms servletname))
    (setq input (servlet-input-atoms servletname))
    (setq output (servlet-output-atoms servletname))
    (setq appatoms (append db cookie session input builtins))  ; the atoms the app is given before servlet runs
    (setq useratoms (append output input cookie builtins `((,*session.username* ?0))))  ; the atoms the user knows after servlet runs
    (append (access-control-read servletname (append read support) (append views raw) bl onlycheck appatoms useratoms)
	    (access-control-write servletname (append write support) (append posneg raw) bl onlycheck appatoms))))   

(defun access-control-write (servletname ac updateviews bl onlycheck appatoms)
  (let (support th pn rest ach)
    (setq appatoms (mapcar #'relation appatoms)) 
    (format t ";;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;~%")
    (format t ";;;;; WRITE ACCESS CONTROL ;;;;;~%")
    (format t ";;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;~%")
    ; fix AC bodies to refer to updated preds
    (setq ac (mapbody #'(lambda (x) (subrel bl x)) ac))
    ; turn AC and updates into first-order logic, indexed by relation
    (multiple-value-setq (ach support) (allowdeny-predicate-completion-hash ac))
    (format t "~&AC hash: ~A~%" (hash2bl ach))
    (multiple-value-setq (pn rest) (posneg-predicate-completion-bl updateviews))
    (format t "~&Update hash: ~A~%" (hash2bl pn))
    ; construct background first-order theory 
    (setq th (predicate-completion (append rest support)))
    ;(setq th (append th (mapcar #'quantify (guard-accum-logic '__integrity))))
    (setq th (cons *session.username-uniqueness* (append (servlet-guard-logic servletname) th)))
    (pcontents th)
    ; run tests
    (format t "; Checking that every tuple inserted or deleted is allowed and not denied by the policy.~%")
    (dolist (x pn) ; list of (relation newhead posform negform))
      (format t ";   Checking ~A~%" x)
      (unless (eq onlycheck :deny) 
	(access-control-write-aux (gethash (first x) ach) (cdr x) th appatoms :allow))
      (unless (eq onlycheck :allow)
	(access-control-write-aux (gethash (first x) ach) (cdr x) th appatoms :deny)))))

(defun access-control-write-aux (ac write th webstatepreds mode)
  "(ACCESS-CONTROL-WRITE-AUX R AC WRITE MODE WEBSTATEPREDS) takes AC and WRITE, each of which are lists of the form
   (<atom> <sent1> <sent2>), where the variables of ATOM are the same as in SENT1 and SENT2 (i.e. atom <=> sent1 and
   atom <=> sent2 are predicate completions.  For AC <sent1> represents ALLOW and <sent2> represents DENY; for WRITE
   <sent1> represents POS/INSERT and <sent2> represents NEG/DELETE.  MODE is either :ALLOW or :DENY, indicating which
   one to check.  WEBSTATEPREDS is the list of predicates that are used to represent this servlet's web application state.  
   This function checks if WRITE obeys AC for the prescribed MODE and if not attempts to find a formula defining all those 
   cases for which WRITE disobeys AC in terms of WEBSTATEPREDS.
   Prints results to stdout and returns the formula if found or NIL."
  (let (acquery writequery bl p phi r doc)
    (cond ((and ac write) ; if no ALLOW rules and no DENY rules or no WRITE rules for this relation, don't bother checking
	   (setq ac (stdize ac))  ; ensure no variable conflicts for mgu
	   (setq acquery (if (eq mode :allow) (second ac) (third ac)))
	   (setq writequery (makor (second write) (third write)))
	   (cond ((not (eq acquery 'false))  ; if no ALLOW/DENY rules for this relation, don't bother checking
		  (setq r (relation (first write)))
		  (setq doc (if (eq mode :allow) "all inserted and deleted tuples are ALLOWed" "no inserted or deleted tuple is DENYed"))
		  (setq bl (mgu (first ac) (first write)))  ; should only bind AC vars, but doesn't hurt to plug into both
		  (when (eq mode :deny) (setq acquery (maknot acquery)))
		  (setq p (quantify `(=> ,(plug writequery bl) ,(plug acquery bl))))
		  (cond ((entailment p th (webapp-builtins))
			 (format t ";   SAFE: Proved that for table ~A ~A~%" r doc)
			 nil)
			((setq phi (definable (maknot p) th webstatepreds (webapp-builtins)))
			 (format t ";   VIOLATION: Constructed witness formula for table ~A disproving that ~A~%" r doc)
			 (format t "; ~A~%" phi)
			 phi)
			(t
			 (format t ";   UNKNOWN: Failed to prove for table ~A that ~A; also failed to construct a witnessing formula~%" r doc)
			 nil)))
		 (t
		  (format t ";   N/A: No ~A access control constraints~%" mode))))
	  (t (format t ";   N/A: No writes or no ~A access control constraints~%" mode)))))

(defun access-control-read (servletname ac views bl onlycheck appatoms useratoms)
  ; AC is the READ access control policy, where all bodies of allow/deny are atomic
  ; VIEWS are view definitions for servlet updates;
  ; BL gives the translation between original table names and tables used to define results of updates
  ; ONLYCHECK can be set to either :allow or :deny to ignore the other case
  (let (th builtins userpreds allowrelns support)
    (format t ";;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;~%")
    (format t ";;;;; READ ACCESS CONTROL ;;;;;~%")
    (format t ";;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;~%")
    (setq builtins (webapp-builtins))
    ; fix AC bodies to refer to updated preds
    (setq ac (mapbody #'(lambda (x) (subrel bl x)) ac))
    ; turn AC into FOL, indexed by relation
    (multiple-value-setq (ac support) (allowdeny-predicate-completion-bl ac))
    (format t "~&AC hash: ~A~%" ac)
    ; construct background theory
    (setq th (append support views))
    (setq th (predicate-completion th))
    ;(setq th (append th (mapcar #'quantify (guard-accum-logic '__integrity))))
    (setq th (cons *session.username-uniqueness* (append (servlet-guard-logic servletname) th)))
    (pcontents th)
    ; check if any of the deny relations are definable in terms of the output and built-in relations
    (unless (eq onlycheck :allow)
      (setq userpreds (append (mapcar #'relation useratoms) builtins))
      (dolist (a ac)  ; a list of (relation relationatom allowsent denysent)
	(access-control-read-deny (fourth a) (second a) th userpreds builtins)))
    (unless (eq onlycheck :deny)
      (setq allowrelns (mapcan #'(lambda (x) (relns (third x))) ac))
      (setq allowrelns (remove-duplicates (append builtins allowrelns)))
      (dolist (o (and2list (subrel bl (maksand (servlet-output-atoms servletname)))))
	(access-control-read-allow o allowrelns th appatoms builtins)))))
	
(defun access-control-read-allow (outputatom allowrelns th appatoms builtins)
  (declare (ignore appatoms))
  (let (res)
    (setq res (abduction outputatom th allowrelns builtins))
    (cond ((not res) 
	   (format t ";   VIOLATION: Failed to find definition of output view ~A using allowed views ~A~%" outputatom allowrelns))
	  (t 
	   (format t "; SAFE: Found definition of output view ~A using allowed views ~A:~%~A~%" outputatom allowrelns res)))))

(defun access-control-read-deny (deny doc th userpreds builtins)
  (let (res)
    (setq res (abduction deny th userpreds builtins))
    (cond (res
	   (format t ";   VIOLATION: Inferred denied view ~A from outputs: ~A~%~A~%" doc userpreds res)
	   res)
	  (t
	   (format t ";   UNKNOWN: Failed to infer denied view ~A from outputs: ~A~%~A~%" doc userpreds deny)))))

(defun canonicalize-access-control (th)
  (multiple-value-bind (x y) (mapcaraccum #'canon-access-control (contents th))
    (append x y)))

(defun canon-access-control (p)
  "(CANON-ACCESS-CONTROL P) takes a rule P and if it is an access control rule ensures
   it has the form (<= (allow/deny (q ?x1 ... ?xn) right) (r ?x1 ... ?xn)).
   Returns two values: the new rule and a list of additional rules."
  (cond ((not (member (relation p) '(allow deny))) (values p nil))
	(t
	 (let (orighead head phi vs newhead)
	   (setq orighead (second p))
           ; ensure the head has all unique variables
	   (setq p (car (nth-value 1 (heads-same-bodies-diff (list (list* '<= (second (head p)) (body p)))))))
           ; create symbol for body that has all the same args as the sentence in the head
	   (setq head (list* (first orighead) (head p) (cddr orighead)))
	   (setq phi (maksand (body p)))
	   (setq vs (nreverse (freevars (second head)))) 
	   (setq phi (equantify-except phi vs))
	   (cond ((and (atomicp phi) (equal (vars phi) (vars (second head))))
		  ; check above ensures variable order in phi is same as in head, since vars is deterministic
		  (values `(<= ,head ,phi) nil))
		 (t
		  (setq newhead (cons (gentemp "r") vs))
		  (values `(<= ,head ,newhead) (list `(<= ,newhead ,phi)))))))))
    
(defun posneg-predicate-completion-bl (th &optional (existentialize t))
  (multiple-value-bind (h newth) (posneg-predicate-completion-hash th existentialize)
    (values (hash2bl h) newth)))

(defun posneg-predicate-completion-hash (th &optional (existentialize t)) 
  "(POSNEG-PREDICATE-COMPLETION-HASH TH) takes a theory and returns a hash table
   keyed on relations in TH that appear in the head of a pos/neg rule where the value
   is a list (<newhead> <pred-completion-pos> <pred-completion-neg>).
   Also returns the elements of TH that are not pos/neg rules."
  (let ((*modals* '(pos neg)))
    (modal-predicate-completion-hash th existentialize)))

(defun allowdeny-predicate-completion-bl (th &optional (existentialize t))
  (multiple-value-bind (h newth) (allowdeny-predicate-completion-hash th existentialize)
    (values (hash2bl h) newth)))

(defun allowdeny-predicate-completion-hash (th &optional (existentialize t)) 
  "(ALLOWDENY-PREDICATE-COMPLETION-HASH TH) takes a theory and returns a hash table
   keyed on relations in TH that appear in the head of an allow/deny rule where the value
   is a list (<newhead> <pred-completion-allow> <pred-completion-deny>).
   Also returns the elements of TH that are not allow/deny rules.  Assumes
   the rules are of the form (<= (allow/deny <atom>) ...)."
  (let ((*modals* '(allow deny)))
    (modal-predicate-completion-hash th existentialize)))
    
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Model Checking
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#|
(defun test-all ()
  (let (oldsession newsession oldcookie newcookie possession negsession poscookie negcookie posdb negdb)
    (dolist (e *tests*)
      (let ((olddb *appdb*) bl)
	(setq oldsession (find-cookie-session (wtest-cookie e)))
	(when oldsession 
	  (setq oldsession (gethash oldsession *sessions*)))
	(with-output-to-string (s)
	    (setq newcookie (serve (wtest-input e) (wtest-cookie e) (wtest-servlet e) s)))
	(setq newsession (find-cookie-session newcookie))
	(when newsession
	  (setq newsession (gethash newsession *sessions*)))
	(setq oldcookie (wtest-cookie e))
	
	(setq possession (set-difference newsession oldsession :test #'equal))
	(setq negsession (set-difference oldsession newsession :test #'equal))
	(setq poscookie (set-difference newcookie oldcookie :test #'equal))
	(setq negcookie (set-difference oldcookie newcookie :test #'equal))
	(setq posdb (set-difference (contents *appdb*) (contents olddb) :test #'equal))
	(setq negdb (set-difference (contents olddb) (contents *appdb*) :test #'equal))

#|     ; Don't really know what the output is: the whole bloody database is included currently
	(unless (setequal out (wtest-output e) :test #'equal)
	  (format t "Output error~%")
	  (format t "Actual output: ~%~S~%" out)
	  (format t "Spec: ~%~S~%" (wtest-sessionchange e)))
|#
	(setq bl '((t . t)))
	(unless (test-one possession negsession (wtest-sessionchange e) bl)
	  (format t "Session error~%")
	  (format t "Added session elements: ~%~S~%" (plug possession bl))
	  (format t "Deleted session elements: ~%~S~%" (plug negsession bl))
	  (format t "Spec: ~%~S~%" (plug (wtest-sessionchange e) bl)))

	(unless (test-one poscookie negcookie (wtest-cookiechange e) bl)
	  (format t "Cookie error~%")
	  (format t "Added cookie elements: ~%~S~%" (plug poscookie bl))
	  (format t "Deleted cookie elements: ~%~S~%" (plug negcookie bl))
	  (format t "Spec: ~%~S~%" (plug (wtest-cookiechange e) bl)))

	(unless (test-one posdb negdb (wtest-dbchange e) bl)
	  (format t "DB error~%")
	  (format t "Added DB elements: ~%~S~%" (plug posdb bl))
	  (format t "Deleted DB elements: ~%~S~%" (plug negdb bl))
	  (format t "Spec: ~%~S~%" (plug (wtest-dbchange e) bl)))))))

(defun test-one (pos neg spec bl)
  (multiple-value-bind (specpos specneg) 
      (split #'(lambda (x) (eq (relation x) 'neg)) spec)
    (setq specpos (mapcar #'drop-posneg specpos))
    (setq specneg (mapcar #'drop-posneg specneg))
    (and (setequal pos specpos) (setequal neg specneg))))

;(defstruct wtest servlet input cookie error output dbchange sessionchange cookiechange)
|#	    

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Compiler
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; TODO: figure out uniqueness from constraints; more generally, do something smarter than dropping quantified constraints
(defun compile-form (formname &key (html nil) (js t))
  "(COMPILE-FORM FORMNAME) runs Plato to generate the form described by FORMNAME and saves
  to an HTML/JS file in *compile-dir*."   

  (let (th form target schema guards h)    
    ; extract info
    (setq form (find-form formname))
    (setq target (form-target form))
    (setq schema (find-schema (form-schema form)))
    (setq guards nil)
    (setq guards (union (form-guards form) (schema-guards schema)))
    (setq guards (remove-if-not #'quantifier-free-sentencep (remove-duplicates (mapcan #'guard-accum-logic guards) :test #'equal)))
    (setq guards (and2list (del-namespace (maksand guards) (find-form-signaturename formname))))
    (setq th (prep-plato-theory formname target nil))
    (push `(constraints ',guards) th)
    (setq h (compile-websheet th))
    (setf (htmlform-action h) (format nil "/scwa?"))
    (when html
      (with-open-file (f (fspath :compile (tostring formname ".html")) :direction :output :if-does-not-exist :create :if-exists :supersede)
	(output-htmlform-form f h)))
    (when js
      (with-open-file (f (fspath :compile (tostring formname ".js")) :direction :output :if-does-not-exist :create :if-exists :supersede)
	(princ (htmlform-javascript h) f)
	(format f "function getBigBoxName (x) { return x + 'BIGBOX'; }~%")))
    'done))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Interpreter
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-condition guard-violation (error) ((comment :initarg :comment :accessor comment)))
(define-condition lost-session (error) ((comment :initarg :comment :accessor comment)))
(define-condition internal-error (error) ((comment :initarg :comment :accessor comment)))
; internal servlet

(defmethod process-cookies ((file (eql 'scwa)) postlines)  
  (let (servletname formname possibleforms cookies in signame)
    (handler-case 
	(progn
	  (print *cookies*)
          ; grab target (only necessary b/c deployed with ?scwa in infomaster)
	  (setq servletname (read-user-string (getf-post "servlet" postlines)))
	  (unless (gethash servletname *servlets*)
	    (setq *content* 
		  (with-output-to-string (s)
		    (let ((*output-stream* s))
		      (showerror in (pairs2data-unary *cookies*) servletname (make-instance 'unknown-servlet)))))
	    (return-from process-cookies *cookies*))

          ; grab form source so as to prepend the correct namespace
          ;   first check if serlvet only serves one form and if so, use it; otherwise, trust the submission.
          ; so if we've uniquified form-servlet combinations, we do the secure thing; but still works otherwise.
	  (setq possibleforms (servlet-formnames servletname)) 
	  (if (null (cdr possibleforms))
	      (setq formname (first possibleforms))
	      (setq formname (read-user-string (getf-post "formname" postlines))))
	  (setq in (remove-if #'(lambda (x) (member (car x) '("servlet" "formname") :test #'equal)) postlines)) 
	  (when formname ; prefix keys with signature --- leave as string for pairs2data
	    (setq signame (tostring (schema-signature (find-schema (form-schema (find-form formname))))))
	    (setq in (mapcar #'(lambda (x) (cons (stringappend signame "." (car x)) (cdr x))) in)))

          ; translate the HTTP request data to KIF-data (errors possible with arrays)
	  (handler-case (setq in (pairs2data in))
	    (condition (e)
	      (setq *content*
		    (with-output-to-string (s)
		      (let ((*output-stream* s))
			(showerror in (pairs2data-unary *cookies*) servletname e))))
	      (return-from process-cookies *cookies*)))

          ; run the servlet and return its cookies
	  (setq cookies (pairs2data-unary *cookies*))  ; make sure not to add namespace to cookies so server gets raw data
	  (setq *content*
		(with-output-to-string (stream)
		  (setq cookies (serve in cookies servletname stream))))
          ; turn cookies database back to pairs
	  (data2pairs (del-namespace cookies 'cookie)))
      ; if any uncaught errors occur, probably my fault --- call it an 'internal-error'
      (condition () 
	(setq *content* 
	      (with-output-to-string (s) 
		(let ((*output-stream* s))					       
		  (showerror in (pairs2data-unary *cookies*) servletname (make-instance 'internal-error)))))
	*cookies*))))

(defun withnamespace (pred signame) (tosymbol (stringappend signame "." pred)))

(defun add-namespace (l prefix)
  "(ADD-NAMESPACE L PREFIX) takes a postlines L and a string PREFIX."
  (assert (and (stringp prefix) (every #'(lambda (x) (stringp (car x))) l)) nil 
	  (format nil "ADD-NAMESPACE requires strings as arguments"))
  (mapcar #'(lambda (x) (if (atom x)
			    (withnamespace x prefix)
			    (cons (withnamespace (car x) prefix) (cdr x))))
		    l))

(defun withoutnamespace (pred signame) 
  (let (pre lpre p)
    (setq pre (tostring signame "."))
    (setq lpre (length pre))
    (setq p (tostring pred))
    (if (and (>= (length p) lpre) (string= pre p :end2 lpre))
	(tosymbol (subseq p lpre))
	pred)))

(defun del-namespace (l prefix)
  (let* (pre lpre)
    (setq pre (tostring prefix "."))
    (setq lpre (length pre))
    (mapatoms #'(lambda (x) (let ((p (tostring (relation x))))
			      (if (and (>= (length p) lpre) (string= pre p :end2 lpre))
				  (if (atom x)
				      (tosymbol (subseq p lpre))
				      (cons (tosymbol (subseq p lpre)) (cdr x)))
				  x)))
	      l)))

(defun extract-signature-name (s)
  "(EXTRACT-NAMESPACE S) grabs everything in (relation s) up to the first ."
  (let (idx)
    (setq s (tostring (relation s)))
    (setq idx (position #\. s :from-end t))
    (if idx (tosymbol (subseq s 0 idx)) nil)))

; process is now called *after* process-cookies, which for us does all the work.
; So we just print the result of process-cookies, which was saved in *content*
(defmethod process (s (file (eql 'scwa)) postlines)
  (declare (ignore postlines))
  (princ *content* s))

(define-condition invalid-array-data (error) ((comment :initarg :comment :accessor comment)))
(defun pairs2data-unary (pairs) (mapcar #'(lambda (x) (list (tosymbol-atomic (first x)) (cdr x))) pairs))
(defun pairs2data (pairs &optional (rowcol nil))
  "(PAIRS2DATA PAIRS) takes a list of ('a' . 'b') and returns a list of (a b).
  If a key is of the form 'a[i][j]', creates a list of (a b1 b2 b3 b4).
  Treats i as the column and j as the row; to switch, set optional ROWCOL to T.
  Default of ROWCOL to NIL plays well with JS code that dynamically adds rows by just changing the last index of each input field.
  Returns data or throws an invalid-array-data error."
  (let (arrays reg h rel row val p rows cols atom res)
    ; a list of (key lastidx nexttolastindex val)
    (setq arrays (mapcar #'(lambda (x) (multiple-value-bind (newkey firstidx secondidx) (parse-array (car x))
					 (if rowcol 
					     (list (tosymbol-atomic newkey) firstidx secondidx (cdr x))
					     (list (tosymbol-atomic newkey) secondidx firstidx (cdr x))))) pairs))
    ; handle non-array as usual (to preserve order)
    (multiple-value-setq (arrays reg) (split #'second arrays))
    (setq reg (mapcar #'(lambda (x) (list (tosymbol-atomic (first x)) (fourth x))) reg))
    ; for arrays, hash data into 3-level hash table: keyed on (i) relations, (ii) rows, and (iii) cols
    (setq h (make-hash-table))
    (dolist (a arrays) 
      (setq rel (gethash (first a) h))
      (unless rel  ; initialize hash for rows of this relation
	(setq rel (make-hash-table))
	(setf (gethash (first a) h) rel))
      (setq row (gethash (second a) rel))  ; already know (second a) is NON-NIL
      (cond ((not (third a))  ; only a single index
	     (if row  ; if already a value, throw an error
		 (error 'invalid-array-data
			:comment (format nil "2 values for same index: ~A[~A] is set to ~A and ~A" (first a) (second a) row (fourth a)))
		 (setf (gethash (second a) rel) (fourth a))))
	    (t
	     (unless row ; initialize hash for cols of this row
	       (setq row (make-hash-table))
	       (setf (gethash (second a) rel) row))
	     (setq val (gethash (third a) row))
	     (if val
		 (error 'invalid-array-data 
			:comment (format nil "2 values for same index: ~A[~A][~A] is set to ~A and ~A" (first a) (second a) (third a) val (fourth a)))
		 (setf (gethash (third a) row) (fourth a))))))
    ; walk over the hash-tables to create data, checking for missing values as we go
    (setq res nil)
    ;(print (hash2bl h))
    (dolist (rel.rows (hash2bl h))
      (setq p (car rel.rows))
      (setq rows (cdr rel.rows))
      ;(format t "~&hash for rel ~A:" p)
      ;(print (hash2bl rows))
      (dotimes (i (hash-table-count rows))
	(setq cols (gethash i rows))
	(setq atom (list p))
	(cond ((not cols)
	       (error 'invalid-array-data
		      :comment (format nil "no row index ~A for relation ~A" i p)))
	      ((hash-table-p cols)
	       ;(format t "~& hash for row ~A: " i)
	       ;(print (hash2bl cols))
	       (dotimes (j (hash-table-count cols))
		 (setq val (gethash j cols))
		 (unless val (error 'invalid-array-data
				    :comment (format nil "no col index ~A for row ~A in relation ~A" j i p)))
		 (push (gethash j cols) atom))
	       (push (nreverse atom) res))
	      (t
	       (push (list p cols) res)))))
    (nconc reg (nreverse res))))


(defun parse-array (key)
  (let (lbrack rbrack lbrack2 rbrack2 lastidx lastidx2)
    ; grab last index
    (setq lbrack (search "[" key :from-end t))
    (setq rbrack (search "]" key :from-end t))
    (when (or (not lbrack) (not rbrack)) (return-from parse-array (values key nil nil)))
    (handler-case (setq lastidx (parse-integer (subseq key (1+ lbrack) rbrack)))
      (condition () (return-from parse-array (values key nil nil))))
    ; grab 2nd to last index
    (setq lbrack2 (search "[" key :from-end t :end2 lbrack))
    (setq rbrack2 (search "]" key :from-end t :end2 lbrack))
    (when (or (not lbrack2) (not rbrack2)) (return-from parse-array (values (subseq key 0 lbrack) lastidx nil)))
    (handler-case (setq lastidx2 (parse-integer (subseq key (1+ lbrack2) rbrack2)))
      (condition () (return-from parse-array (values (subseq key 0 lbrack) lastidx nil))))
    (values (subseq key 0 lbrack2) lastidx2 lastidx)))

(Defun unarydata2pairs (data)
  (mapcar #'(lambda (x) (cons (first x) (second x))) data))

(defstruct webapp-timings servestart renderstart renderend serveend)
(defvar *timings* (make-webapp-timings))
(defun showtimings ()
  (let (serve render)
    (setq render (- (webapp-timings-renderend *timings*) (webapp-timings-renderstart *timings*)))
    (setq serve (- (webapp-timings-serveend *timings*) (webapp-timings-servestart *timings*)))
    (format t "Serve: total = ~A sec; w/o render = ~A sec~%" serve (- serve render))
    (format t "Render: total = ~A sec~%" render)))

(defun serve (in cookie servletname s)
  "(SERVE IN COOKIE SERVLETNAME) executes the servlet with name SERVLETNAME on data IN and COOKIE
   and prints the resulting HTML page to stream S and returns new cookie data.  All data is represented
   as lists of KIF atoms."
  (let (sessionid session present (*attachments* t) (*output-stream* s))
    (when *timings* (setq *timings* (make-webapp-timings)) (setf (webapp-timings-servestart *timings*) (get-universal-time)))
    ; look up session
    (setq sessionid (find-cookie-session cookie))
    (setq session nil)
    (when sessionid
      (setq sessionid (read-user-string sessionid))
      (when (listp cookie) (setq cookie (define-theory (make-instance 'prologtheory) "" cookie)))
      ; ensure no cookie session pollution
      (handler-case (mapc #'(lambda (x) (guard-check x cookie))  (guard-all-guards '__single-cookie-session))
	(guard-violation (e) (showerror in cookie servletname e) (return-from serve (contents cookie))))
      ; check if we have the session
      (multiple-value-setq (session present) (gethash sessionid *sessions*))
      (handler-case (unless present (error 'lost-session :comment (list sessionid nil)))
	(lost-session (e) (showerror in cookie servletname e) (return-from serve (contents (drop-cookie-session cookie))))))
    ; serve
    (drop-cookie-session cookie)
    (handler-case (setq cookie (serve-session in cookie session sessionid servletname))
      (condition (e) 
	(showerror in cookie (find-servlet servletname) e) 
	(when sessionid (save-cookie-session sessionid cookie))
	(return-from serve (contents cookie))))
    (when *timings* (setf (webapp-timings-serveend *timings*) (get-universal-time)))
    cookie))

(defun serve-session (in cookie session sessionid servletname)
  "(SERVE-NORMAL IN COOKIE SESSION SERVLETNAME S) takes theories for IN, COOKIE, SESSION, a SERVLETNAME, and a stream S.
   It outputs the result of the servlet to stream S and returns the new cookie data as a list. "
  (let (th servlet)
    ; lookup servlet
    (setq servlet (find-servlet servletname))
    ; create data structure (a theory) representing the server's global state and including in, cookie, and session
    (setq th (get-server-state in cookie session))
    ; process data according to servlet semantics
    (dolist (thing (servlet-guts servlet))
      (cond ((guardlist-p thing) (mapc #'(lambda (x) (guard-check x th)) (guard-all-guards (guardlist-guards thing))))
	    ((updatelist-p thing) (transduce-all-data (updatelist-updates thing) th))))
    ; check integrity constraints for all our data stores
    (mapc #'(lambda (x) (guard-check x th)) (guard-all-guards '__integrity))
    ; set server's state using results of transduction
    (setq cookie (set-server-state th sessionid))
    ; render the page to the user  
    (render2 (servlet-page servlet) th)
    ; free memory by unincluding all theories so that when TH goes out of scope, the memory is freed
    (decludes th)
    ; return the cookie
    cookie))

(defun get-server-state (in cookie session)
  "(GET-SERVER-STATE IN COOKIE SESSION) returns a theory (possibly with includees) that contains 
   all of IN, COOKIE, SESSION, and *appdb*.  IN, COOKIE, SESSION can each be either lists or epilog theories."
  (let (th theories)
    (setq theories (list *appdb*))
    (if (listp in) (setq th (nconc th in)) (push in theories))
    (if (listp cookie) (setq th (nconc th cookie)) (push cookie theories))
    (if (listp session) (setq th (nconc th session)) (push session theories))
    (setq th (define-theory (make-instance 'prologtheory) "" th))
    (mapc #'(lambda (x) (includes th x)) theories)  ; unfortunately, this ensures TH only gets deallocated if we declude it when we're done
    th))

(defun set-server-state (th sessionid)
  "(SET-SERVER-STATE TH SESSIONID) updates the server's state and returns the cookie for the user to update her state."
  (let (cookie session)
    (setq *appdb* (define-theory (make-instance 'prologtheory) "" (find-appdb-data th)))
    (setq session (find-session-data th))
    (setq cookie (drop-cookie-session (find-cookie-data th)))
    ; grab session; ensure that there is a cookie/session link iff session is non-nil
    (cond (session  ; ensure a cookie/session link
	   ; set session
	   (unless sessionid 
	     (with-lock-grabbed (*randlock*) 
	       (setq sessionid (random 1000 *randstate*))))
	   (setf (gethash sessionid *sessions*) session)
	   ; store session id in cookie
	   (push `(,*cookie-session-name* ,sessionid) cookie))
	  (t
	   ; if no session, delete sessionid; cookie session has already been deleted
	   (when sessionid (remhash sessionid *sessions*))))
    cookie))

(defun in-what (r) 
  (let (index sub)
    (cond ((member r (webapp-builtins)) :builtin)
	  (t
	   (setq r (tostring r))
	   (setq index (search "." r))
	   (cond ((not index) :unknown)
		 (t
		  (setq sub (tosymbol (subseq r 0 index)))
		  (cond ((member sub (webapp-builtins)) :builtin)
			(t
			 (case sub
			   (cookie :cookie)
			   (session :session)
			   (db :db)
			   (otherwise :unknown))))))))))
(defun in-reserved-namespace (r) (or (in-cookie r) (in-session r) (in-db r)))
(defun in-cookie (r) (has-prefix (relation r) 'cookie.))
(defun in-session (r) (has-prefix (relation r) 'session.))
(defun in-db (r) (has-prefix (relation r) 'db.))
(defun has-prefix (x pre)
  (let (l)
    (setq x (tostring x))
    (setq pre (tostring pre))
    (setq l (length pre))
    (and (>= (length x) l) (string= pre x :start2 0 :end2 l))))

(defun find-cookie-data (th) (find-data 'cookie th))
(defun find-appdb-data (th) (find-data 'db th))
(defun find-session-data (th) (find-data 'session th))
(defun find-data (namespace th)
  (let (l)
    (setq namespace (tostring namespace "."))
    (setq l (length namespace))
    (remove-if-not #'(lambda (x) 
		       (setq x (tostring (relation x)))
		       (and (>= (length x) l) 
			    (string= namespace x :start2 0 :end2 l)))
		   (sentences '? th))))

(defun transduce-all-data (updates data)
  "(TRANSDUCE-ALL-DATA UPDATES DATA) applies a sequence of database names UPDATES to DATA and
   returns the result.  DATA is assumed to be indexed; returns an indexed DATA."
  (when *debug-webapps* (format t "~&Initial Database: ~%~S~%" (sentences '? data)))
  (dolist (u updates data)
    (when *debug-webapps* (format t "~&Applying update ~A~%" u))
    (setq data (transduce-data u data))
    (when *debug-webapps* (format t "~&Database after applying update ~A: ~%~S~%" u (sentences '? data)))))

(defun transduce-data (up data)
  "(TRANSDUCE-DATA UP DATA) applies the update named UP to DATA (including any guards checking for UP)
   and returns the resulting data.  Assumes DATA and (update-datalog UP) are already indexed.
   Returns an indexed data set."
  (let (pos neg)
    (setq up (gethash up *updates*))
    (unless up (error 'unknown-update :comment up))
    ; check the guards
    (mapc #'(lambda (x) (guard-check x data)) (update-all-guards up))
    ; grab all the intensional relations from the datalog rules
    ;(setq preds (mapcar #'head (contents (update-datalog up))))
    ;(setq preds (remove-duplicates (mapcar #'(lambda (x) (make-parameter :symbol (relation x) :arity (arity x))) preds)))
    ; apply viewfinds to all the intensional relations in UP
    (includes data (update-datalog up))
    (setq pos (viewfinds '?x `(pos ?x) data))
    (setq neg (viewfinds '?x `(neg ?x) data))
    (when *debug-webapps* (format t "~&Pos update: ~%~S~%" pos))
    (when *debug-webapps* (format t "~&Neg update: ~%~S~%" neg))

;    (setq th (mapcan #'(lambda (x) (let ((p (cons (parameter-symbol x) 
;						  (maptimes #'newindvar (parameter-arity x)))))
;				     (viewfinds p p data)))
;	  preds))
    (unincludes data (update-datalog up))
    (mapc #'(lambda (x) (drop* x data)) neg)  ; using drop* to ensure removed from all included theories
    (mapc #'(lambda (x) (save x data)) pos)
    data))

(defun guard-check (guard data) 
  "(GUARD-CHECK GUARD DATA) takes a GUARD name (a set of constraints) and DATA and checks
   that DATA U GUARD is satisfiable.  If not, throws an error and includes an explanation
   for why satisfaction fails.  Should always be able to assume DATA is complete; however,
   there might be datalog view definitions in GUARD."
  (assert (not (listp data)) nil (format nil "guard-check assumes DATA is a real theory, not a list"))
  (let (th preds errs)
    (setq *tmp* th)
    (setq th (guard-datalog (find-guard guard)))
    (includes th data)
    (setq preds (mapcar #'parameter-symbol (get-vocabulary (maksand (contents data)))))
    (setq *tmp2* preds)
    (setq errs (viewsupports '?text '(__error ?text) th #'(lambda (x) (member x preds))))
    (unincludes th data)
    (when errs (error 'guard-violation :comment (list guard errs)))))

;(class-name (class-of err))
(defmethod showerror (in cookie servlet err)
  (let (class)
    (setq class (class-name (class-of err)))
    (format *output-stream* "<html><head><title>~A</title></head><body>" class)
    (format *output-stream* "~&<P>Error in servlet ~A: ~A.~%" servlet class)
    (format *output-stream* "~&<p>~A~%" err)
    (showerror-in in)
    (showerror-cookie cookie)
    (format *output-stream* "</body></html>")))

(defmethod showerror (in cookie servlet (err invalid-array-data))
  (format *output-stream* "<html><head><title>Invalid array data</title></head><body>")
  (format *output-stream* "~&<P>Array data in request to servlet ~A invalid: ~%<br>" servlet)
  (format *output-stream* "<b>~A</b>" (comment err))
  (showerror-in in)
  (showerror-cookie cookie)
  (format *output-stream* "</body></html>"))
   
(defmethod showerror (in cookie servlet (err unknown-servlet))
  (format *output-stream* "<html><head><title>Unknown servlet</title></head><body>")
  (format *output-stream* "~&<P>Unknown servlet: ~A.~%" servlet)
  (showerror-in in)
  (showerror-cookie cookie)
  (format *output-stream* "</body></html>"))
  
(defmethod showerror (in cookie servlet (err lost-session))
  (declare (ignore servlet))
  (format *output-stream* "<html><head><title>Lost session</title></head><body>")
  (format *output-stream* "~&<P>Session invalid.  Please login again.~%")
  (showerror-in in)
  (showerror-cookie cookie)
  (format *output-stream* "</body></html>"))

(defmethod showerror (in cookie servlet (err guard-violation))
  ; servlet name
  (format *output-stream* "<html><head><title>Error caught by ~A</title></head><body>" (servlet-name servlet))
  (format *output-stream* "~&<P>Servlet ~A caught an error~%" (servlet-name servlet))
  ; errors and causes
  (format *output-stream* "~&<p>Found error in guard ~A~%" (first (comment err)))
  (format *output-stream* "<ul>")
  (dolist (e (second (comment err)))
    (format *output-stream* "<li>Error ~S caused by ~S~%" (first e) (htmlify-thing (second e))))
  (format *output-stream* "</ul>~%")
  (showerror-in in)
  (showerror-cookie cookie)
  (format *output-stream* "</body></html>"))

(defun showerror-in (in)
  (format *output-stream* "<p>Data received by server:~%")
  (format *output-stream* "<ul>~%")
  (dolist (i (contents in))
    (format *output-stream* "<li>~S~%" (htmlify-thing i)))
  (format *output-stream* "</ul>~%"))

(defun showerror-cookie (cookie)
  (format *output-stream* "<p>Cookies received by server:~%")
  (format *output-stream* "<ul>~%")
  (dolist (i (contents cookie))
    (format *output-stream* "<li>~S~%" (htmlify-thing i)))
  (format *output-stream* "</ul>~%"))

(defun htmlify-thing (a)
  (cond ((stringp a) (htmlify a))
	((symbolp a) a)
	((listp a) (mapcar #'htmlify-thing a))
	(t a)))

(defstruct htmlbl forms tables)

; old, slow version of renderer
(defun render (htmlname data)
  "(RENDER HTMLNAME DATA) takes an HTMLNAME, DATA to populate that page (overwriting
   any data extracted by PAGENAME as a default), and outputs that page to the default stream.  
   Current implementation only does form/table replacement after html page is fully created;
   to do it properly we need an XML/HTML parser that appropriately deals with partial XML
   files (in a non-lossy way).  For pages with multiple forms/tables that have the same name,
   this implementation is incorrect.  Also, assuming that formname.widgetname gives a unique ID,
   which is again incorrect for pages with multiple forms."
  (when *timings* (setf (webapp-timings-renderstart *timings*) (get-universal-time)))
  (multiple-value-bind (s repls) (render-to-string htmlname)
    (princ (html-subst s (htmlrepls2htmlbl repls) data) *output-stream*))
  (when *timings* (setf (webapp-timings-renderend *timings*) (get-universal-time))))

(defun render-to-string (thing)
  "(RENDER-TO-STRING THING) returns a string representing the html file named HTMLFILE
   as well as the sequence of htmlrepls that ought to be applied.  Note: does not apply
   those HTMLREPLS as it ought to.  Super slow---copying html fragments over and over."
  (let (subhtml out bl)
    (setq out (make-stringbuffer))
    (setq bl nil)
    (cond ((stringp thing) (format out "~A" (read-static-file thing)))
	  ((symbolp thing) 
	   (setq subhtml (gethash thing *html*))
	   (dolist (x subhtml)
	     (multiple-value-bind (s b) (render-to-string x)
	       (format out "~A" s) (setq bl (nconc b bl)))))
	  ((htmlrepl-p thing) 
	   (multiple-value-bind (s b) (render-to-string (htmlrepl-target thing))
	     (setq out s) (setq bl (cons thing b))))
	  (t (error 'invalid-html :comment thing)))
    (values out bl)))

; current version of renderer
(defun render2 (htmlname data)
  (when *timings* (setf (webapp-timings-renderstart *timings*) (get-universal-time)))
  (multiple-value-bind (s repls) (render-to-string2 htmlname)
    (princ (html-subst s (htmlrepls2htmlbl repls) data) *output-stream*))
  (when *timings* (setf (webapp-timings-renderend *timings*) (get-universal-time))))

(defun render-to-string2 (thing)
  (let (repls)
    (values (with-output-to-string (s)
	      (dolist (h (flatten-html thing))
		(cond ((stringp h) (princ (read-static-file h) s))
		      ((and (htmlrepl-p h) (stringp (htmlrepl-target h)))
		       (push h repls)
		       (princ (read-static-file (htmlrepl-target h)) s))
		      (t (error 'invalid-html :comment thing)))))
	    repls)))

(defun htmlrepls2htmlbl (repls) 
  (make-htmlbl :forms (mapcan #'(lambda (x) (copy-list (htmlrepl-forms x))) repls)
	       :tables (mapcan #'(lambda (x) (copy-list (htmlrepl-tables x))) repls)))

(defun read-static-file (name) (read-any-file (fspath :html name)))

(defun html-subst (str htmlrepl data)
  "(PAGE-SUBST STR REPLACEMENTS) takes a valid HTML fragment STR as a string and a list
   of the form :forms ((\"search\" search)) :tables ((\"auctions\" auctions)) and replaces
   each occurrence of a form \"search\" with a rendering of the form SEARCH; similarly for table."
  (xmls:toxml (html-subst-aux (xmls:parse str) htmlrepl data) :shortend nil))

(defun html-subst-aux (html repl data)
  (cond ((atom html) html)
	((not (listp html)) (error 'invalid-html :comment html))
	(t
	 (let (tag)
	   (handler-case (setq tag (tosymbol (car html)))
	     (condition () (setq tag nil)))
	   (case tag
	     (head (html-augment-head html *corejs* *corecss*))
	     (form (html-subst-form html repl data))
	     (table (html-subst-table html repl data))
	     (otherwise (mapcar #'(lambda (x) (html-subst-aux x repl data)) html)))))))

#|
(defun xml2html (x)
  (cond ((atom x) x)
	((not (listp x)) (error 'invalid-html :comment x))
	(t (list* (tosymbol (car x)) 
		  (mapcar #'(lambda (x) (cons (tosymbol (first x)) (rest x))) (second x)) 
		  (mapcar #'xml2html (cddr x))))))
|#
(defun html-augment-head (head jsfiles cssfiles) 
  (append head (mapcar #'js-include-xml jsfiles) (mapcar #'css-include-xml cssfiles))) 
(defun js-include-xml (jsfile) (xmls:parse (js-include jsfile)))
(defun js-include (jsfile) (with-output-to-string (s) (output-htmlform-js jsfile s)))
(defun css-include-xml (cssfile) (xmls:parse (css-include cssfile)))
(defun css-include (cssfile) (with-output-to-string (s) (output-htmlform-style cssfile s)))

(defun html-subst-form (form bls data)
  (let (name bl)
    (setq name (second (find "name" (second form) :key #'first :test #'equalp)))
    (setq bl (find name (htmlbl-forms bls) :key #'first :test #'equal))
    ; if to be substituted for, do so; otherwise, replace with empty form
    (if bl
	(xmls:parse (generate-form (second bl) data))
	(xmls:parse "<form></form>"))))

(defun html-subst-table (table bls data)
  (let (name bl)
    (setq name (second (find "name" (second table) :key #'first :test #'equalp)))
    (setq bl (find name (htmlbl-tables bls) :key #'first :test #'equal))
    ; if to be substituted for, do so; otherwise, recurse
    (if bl
	(xmls:parse (generate-table (second bl) data))
	(list* (first table) (second table) (mapcar #'(lambda (x) (html-subst-aux x bls data)) (cddr table))))))

(defun generate-form (formname data)
  "(GENERATE-FORM FORMNAME DATA) output an HTML form as described by FORMNAME
   where the initial values are drawn from DATA.  Assume JS for error-checking has already been compiled."
  (let (form target th wf html)
    (setq form (find-form formname))
    (setq target (form-target form))
    ;(setq data (del-namespace (contents data) (find-form-signaturename formname)))
    ;(setq signame (find-form-signaturename formname))
    ;(setq data (mapcar #'(lambda (x) (del-namespace x signame)) (contents data)))
    (setq th (prep-plato-theory formname target data))
    (push `(constraints 'nil) th)
    (setq wf (load-formstructure th nil))
    (print (webform-widgets wf))
    (mapc #'(lambda (x) (weblog-adjust-initvals x (webform-univ wf) (webform-eq wf))) (webform-widgets wf))
    (print (webform-widgets wf))
    (setq html (compile-websheet-html (make-htmlform) wf))
    (setf (htmlform-action html) (format nil "/scwa?"))
    (setf (htmlform-submitprep html) "true")
    (with-output-to-string (s)
      (format s "<p>") ; to ensure parsing handles both elements
      (when (probe-file (fspath :compile (tostring formname ".js"))) 
	(princ (js-include (webpath :compile (tostring formname) "js")) s))
      (output-htmlform-form s html)
      (format s "</p>"))))

(defun weblog-adjust-initvals (w univ valuecomp)
  "(WEBLOG-ADJUST-INITVALS W UNIV VALUECOMP) turns the initial values for widget W into objects
   to play nicely with the form gen of Plato.  This is functionality we skip during load-formstructure
   b/c Weblog has already initialized the widget types/displays/initial values properly."
  (setf (widget-init w)
	 (mapcar #'(lambda (x) 
		     (mapcar #'(lambda (y) 
				 (let (elem)
				   (setq elem (find y univ :key #'value-prettyname :test valuecomp))
				   (cond (elem elem)
					 (t (setq elem (make-value :symbol (gentemp "o") :prettyname y))
					    elem)))) 
			     x))
		 (widget-init w))))

; TODO: have plato's output be wrapped in a JS namespace so that it actually works.  
;   Need to call a version of JS init() for each of the forms.
;   Should only have 1 copy of the basic functions (e.g. spreadsheet.js), but then cellvalue needs to be specific to a form.
(defun prep-plato-theory (formname target data)
  "(PREP-PLATO-THEORY FORMNAME DATA) builds a theory for Plato describing FORMNAME, except constraints."
  (let (th signame name fullname args req init desc styles incundef unique types formdata extradata singletuple)
    ;(print (contents data))
    (setq formdata (extract-form-data formname data))
    ;(print formdata)
    ; create basic Plato theory 
    (push `(formname ,formname) th)
    (push `(option completep t) th)
    (push `(option casesensitive t) th)
    (push `(option allowconflicts t) th)
    (push `(option debug nil) th)
    (push `(definitions ',nil) th)
    ; add widgets
    (setq signame (find-form-signaturename formname))
    (dolist (p (find-signature signame))
      (setq name (parameter-symbol p))
      (setq fullname (tosymbol (tostring signame ".") name))
      (setq args (nunique (parameter-arity p) "?"))
      (setq init (viewfinds args (cons fullname args) formdata))
      ;(setq type (or (first (parameter-type p)) 'string))  ; default to string
      (when init (setq init (mapcar #'(lambda (x) (mapcar #'htmlify x)) init)))
      (setq desc (tostring name))
      (setq incundef t)
      (multiple-value-setq (styles unique req types extradata singletuple) (extract-widget-info formname p incundef))
      (when (and unique (not init)) (setq init (list singletuple)))  ; need to know what to fill in if only 1 row
      (dolist (e extradata) (push (list 'type (first e) (cons 'listof (cdr e))) th))  ; add type info to th
      (push `(widget :name ,name :id ,name :req ,req :init ,init :desc ,desc :style ,styles :incundef ,incundef :unique ,unique :typename ,types) th))
    ; providing all the fields for this widget since otherwise load-formstructure does something stupid
    (push `(widget :name servlet :init ((,target)) :style (hidden) :desc "" :req nil :incundef nil :unique t :typename (string)) th)
    (nreverse th)))

(defun extract-widget-info (formname param incundef)
  "(EXTRACT-WIDGET-INFO FORMNAME PARAM) analyzes the guards for FORMNAME to figure out the properties
  for the widget PARAM and returns (i) style, (ii) unique, (iii) required, (iv) typename, (v) list of datalog defining typename.
  Here we're just looking for some simple syntactic patterns; if instead we were to do this at
  compilation time, we could see what statements were entailed.  INCUNDEF only used to generate single default tuple."
  (let (sig unique required paramsymbol dbqueries enumlists atom1 args1 atom2 args2 phiexists phiunique arity vars idx res)
    ; prefix the parameter name by the signature
    (setq sig (schema-signature (find-schema (form-schema (find-form formname)))))
    (setq paramsymbol (tosymbol (tostring sig ".") (parameter-symbol param)))
    (setq arity (parameter-arity param))
    ; analyze logic
    (setq dbqueries nil)
    (setq enumlists nil)
    (setq atom1 (cons paramsymbol (nunique arity "?")))
    (setq args1 (cdr atom1))
    (setq atom2 (cons paramsymbol (nunique arity "?" arity)))
    (setq args2 (cdr atom2))
    (setq phiexists `(exists ,args1 ,atom1))
    (setq phiunique (list* 'or `(not ,atom1) `(not ,atom2) (mapcar #'(lambda (x y) `(= ,x ,y)) args1 args2)))    
    ; look at each FOL guard statement to see if we know what it means
    (dolist (g (form-all-guards formname))
      (dolist (p (guard-logic (find-guard g)))
	(unless (stringp p)
	  ; required
	  (when (samep p phiexists) 
	    (setq required t))
	  ; convert to clausal and check each clause
	  (dolist (q (clauses p))
	    ; uniqueness
	    (when (sentequal q phiunique :test #'samep)
	      (setq unique t))
	    ; enumeration of values: index.list is of the form (index . list), where index is the 0-based arg index
	    (multiple-value-bind (index.list isenum) (enumerated-type q paramsymbol)
	      (when isenum (push index.list enumlists)))
	    ; database query
	    (multiple-value-bind (query isquery safevars) (database-type q paramsymbol arity)
	      (when isquery (push (list query (intersect safevars args1)) dbqueries)))))))
      ; Evaluate DB queries to expand lists of known values
    (dolist (q dbqueries)
      (setq vars (mapcar #'(lambda (x) (parse-integer (symbol-name x) :start 1)) (second q)))
      (setq vars (sort vars #'<))
      (setq res (logical-database-query (mapcar #'makevariable vars) (first q) *appdb*))
      (print q)
      (dolist (v (second q))
	(print v)
	(setq idx (devariable v))
	(push (cons idx (mapcar #'(lambda (x) (nth idx x)) res)) enumlists)))
    ; intersect enumerated lists by arg index
    (setq enumlists (group-by enumlists #'car))  ; yields a list of (index . list1 list2 ...)
    (setq enumlists (mapcar #'(lambda (x) (setf (cdr x) (n-intersection (cdr x) :test #'equal))) enumlists))  ; now (index . list)
    ; for each arg, set type/display (defaulting to string/textbox if no info)
    (let (vals styles typenames typ extradata singletuple)
      (dotimes (i arity)
	(setq vals (cdr (assoc i enumlists)))
	(cond (vals
		(push 'selector styles)
		(setq typ (tosymbol (gentemp "type"))) ; create type name
		(push (cons typ vals) extradata) ; record type's values
		(push typ typenames)
		(if incundef (push (mak-undefined) singletuple) (push (first vals) singletuple)))
	       (t
		(push 'textbox styles)
		(push 'string typenames)
		(push "" singletuple))))
      (setq typenames (nreverse typenames))
      (setq styles (nreverse styles))
      (setq singletuple (nreverse singletuple))
      (values styles unique required typenames extradata singletuple))))

(defun logical-database-query (thing p th)
  (let (r)
    (setq r (list '<= (cons '__tlh thing) p))
    (setq r (logic-to-datalog (brfs r)))
    (setq r (define-theory (make-instance 'prologtheory) "" r))
    (includes r th) 
    (viewfinds thing (cons '__tlh thing) r)))

(defun database-type (p sym arity)
  "(DATABASE-TYPE P ARITY SYM) checks if clause P is equivalent to (or (not (p @x)) (phi @x))
   where phi is a DB query, i.e. have relations in the DB prefix and where all of @x vars
   are distinct and appear in PHI.  Moreover, all vars in non-db literals must appear
   in some positive DB literal (a version of safety).
   Returns 2 values: the phi(@x), after applying @x/nunique(len(@x)),
   and whether or not the sentence is of the desired form (to handle the case (not (p @x))."
  (let (atom safevars bl)
    (setq atom (cons sym (nunique arity "?")))
    (cond ((not (listp p)) (values 'notlistp nil))
	  ((and (atomicp p) (samep p `(not ,atom))) (values 'false t))
	  ((not (eq (car p) 'or)) (values 'notorvalue nil))
	  (t
	   (setq p (cdr p))
           ; check only one occurrence of (not (p @x)); similtaneously ensure all args are vars and are unique
	   (multiple-value-bind (notps rest) (split #'(lambda (x) (samep x `(not ,atom))) p)
	     (when (or (not notps) (cdr notps)) (return-from database-type (values 'toomanyps nil)))
	     (setq notps (first notps))
	     (setq p rest)
             ; check that (i) every var appearing in rest appears in some positive literal that is in the DB namespace and 
	     ;    (ii) the remaining literals are builtins
	     (multiple-value-bind (db others) (split #'in-db p)
	       (setq safevars (vars (remove-if-not #'positive-literalp db)))
	       (cond ((and (subsetp (vars rest) safevars)
			   (every #'(lambda (x) (is-builtin (relation x))) others))
		      (setq bl (mapcar #'cons (cdr (second notps)) (cdr atom)))
		      (values (sublis bl (maksor p)) t (sublis bl safevars)))
		     (t (values 'notjustdb nil)))))))))
	    
(defun enumerated-type (p sym)
  "(ENUMERATED-TYPE P SYM) check if P enumerates a list of possible values for the unary symbol SYM.
   Generalizing this to non-unary possibly requires analyzing several clauses at once (or analyzing the original sentence)."
  (multiple-value-bind (list hasval) (enumerated-type-aux p sym)
    (if hasval
	(values (cons 0 list) hasval)
	(values list hasval))))

(defun enumerated-type-aux (p sym)
  "(ENUMERATED-TYPE P SYM) checks if clause P is equivalent to (or (not (p ?x)) (= ?x a1) (= ?x a2) ...)
   Returns 2 values: the list of (a1 a2 ...) and whether or not the sentence is of the desired form (
  to handle the case (not (p ?x))."
  (cond ((not (listp p)) (values nil nil))
	((and (atomicp p) (samep p `(not (,sym ?x)))) (values nil t))
	((not (eq (car p) 'or)) (values nil nil))
	(t
	 (setq p (cdr p))
	 (let (v vals)
           ; check only one occurrence of (not (p ?x)), grab var.
	   (multiple-value-bind (notps rest) (split #'(lambda (x) (samep x `(not (,sym ?x)))) p)
	     (when (or (not notps) (cdr notps)) (return-from enumerated-type-aux (values nil nil)))
	     (setq notps (first notps))
	     (setq v (first (vars notps)))
	     (setq p rest))
           ; check that remainder are all of the form (= ,var string)
	   (dolist (a p (values (nreverse vals) t))
	     (if (eq (first a) '=)
		 (if (eq (second a) v)
		     (if (atom (third a))
			 (push (third a) vals)
			 (return (values nil nil)))
		     (if (eq (third a) v)
			 (if (atom (second a))
			     (push (second a) vals)
			     (return (values nil nil)))))
		 (return (values nil nil))))))))

(defun extract-form-data (formname data)
  "(EXTRACT-FORM-DATA FORMNAME DATA) finds the portion of DATA appropriate for form FORMNAME
   and returns it (leaving the namespace in tact to avoid hitting Epilog's builtin functions)."
  (extract-signature-data (find-form-signaturename formname) data))

(defun extract-signature-data (signame data)
  (let (newdata sigl p preds)
    (setq preds (find-signature signame))
    (setq signame (tostring signame "."))
    (setq sigl (length signame))
    (dolist (d (contents data))
      (setq p (tostring (relation d)))
      (when (eq (search signame p) 0)
	(setq p (tosymbol (subseq p sigl)))
	(when (member p preds :key #'parameter-symbol)
	  (push d newdata))))
;	  (if (atom d)
;	      (push p newdata)
;	      (push (cons p (cdr d)) newdata)))))
    (nreverse newdata)))	

(defun generate-table (tablename data)
  (let (signame)
    (setq signame (find-table-signaturename tablename))
    (setq data (extract-signature-data signame data))
    (setq data (mapcar #'(lambda (x) (del-namespace x signame)) data))
    (setq data (group-by-hash data #'car))
    (with-output-to-string (s)
      (format s "<div>~%")
      (dolist (p (find-signature signame)) (generate-table-one s p data))
      (format s "</div>~%"))))

(defun generate-table-one (s param data)
  "(GENERATE-TABLE-ONE S PARAM DATA) outputs to the stream S a <table> element
   that represents the PARAM table, filling it with the appropriate elements from DATA.
   DATA is a hash that maps a table names to the list of data for that table."
  (setq data (gethash (parameter-symbol param) data))
  (format s "<table border='1'><caption>~A</caption>~%" (parameter-symbol param))
  (dolist (atom data)
    (princ "<tr>" s)
    (do ((vals (cdr atom) (cdr vals))
	 (typs (parameter-type param) (cdr typs)))
	((null vals))
      (princ "<td>" s)
      (output-untrusted-data s (car typs) (car vals))
      (princ "</td>" s))
    (princ "</tr>" s)
    (crlf s))
  (princ "</table>" s)
  (crlf s))

(defun output-untrusted-data (s type val)
  "(OUTPUT-UNTRUSTED-DATA S TYPE VAL) dumps user-data VAL of type TYPE to html stream S, 
   after eliminating the type-appropriate HTML markup."
  (princ (sanitize-untrusted-data type val) s))

(defun sanitize-untrusted-data (type val)
  (let (sort)
    (setq sort (gethash type *sorts*))
    (cond ((not sort)
	   (htmlify (format nil "~A" val)))
	  (t
	   (viewfindx '?x `(,(sort-htmlsanitizer sort) ,val ?x) 'a)))))

(defun htmlpurify (val)
  "(HTMLPURIFY VAL) returns a string representation of VAL after removing unsafe HTML.
   Utilizes PHP htmlpurifier script in clicl/htmlpurify."
  (write-any-file "/tmp/htmlpurify" val)
  (exec-commandline "php" (fspath :clicl "htmlpurify" "php") "/tmp/htmlpurify"))
  
(defun htmlpurify-errorsp (val)
  "(HTMLPURIFY-ERRORSP VAL) returns a boolean indicating whether or not the purification
   process returns errors."
  (write-any-file "/tmp/htmlpurify" val)
  (setq val (exec-commandline "php" (fspath :clicl "htmlpurifyp" "php") "/tmp/htmlpurify"))
  (if (char= (char val 0) #\E) t nil))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Logical Reasoning ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; Note that definability is a stronger version of abduction (requiring a sentence
;  equivalent to the conclusion, not just one that implies the conclusion)

(defun definable (p th preds &optional (builtins nil)) 
  (definable-epilog p th preds builtins))

(defun abduction (p th preds &optional (builtins nil)) 
  (abduction-epilog p th preds builtins))

(defun entailment (p th &optional (builtins nil)) 
  (entailment-vampire p th builtins))

;;;;;;;;;; Vampire ;;;;;;;;;; 

(defun entailment-vampire (p th &optional (builtins nil))
  (let (value)
    (entailment-vampire-file p th "/tmp/vampire.ent" :builtins builtins)
    (setq value (exec-commandline "vampirewrap /tmp/vampire.ent"))
    (setq value (read-user-string value))
    (if (= value 0) t nil)))


; This doesn't currently work.
; Suppose P is an atom with distinct variables, e.g. (p ?x ?y ?z).
; What we want to do is run vampire in symbol-elimination mode to get
;   all of the consequences of TH whose preds are in PREDS and p.
; Then we want to run vampire on the results in consequence-elimination mode
;   to find an axiomatization of that theory.
; Then we want to use my iff detector algorithm to extract a definition of
;   p from the results.
; I'm currently having trouble getting vampire to run in symbol elimination mode. 
; See http://www.cs.man.ac.uk/~hoderk/ for several slidesets and papers
; See http://www.vprover.org/interpol.cgi for Vampire's page
(defun definable-vampire (p th preds &optional (builtins nil))
  ;(format t "~&Calling DEFINABLE~%")
  ;(format t "Query: ~A~%" p)
  ;(dolist (v th) (print v))
  ;(format t "~&Preds: ~A~%~%" preds)
  (definable-vampire-file p th preds "/tmp/vampire.def" :builtins builtins)
  ;(break)
  nil)

; vampire takes as input a superset of TPTP
(defun definable-vampire-file (p th preds filename &key (time 10) (builtins nil))
  "(DEFINABLE-VAMPIRE-FILE P TH PREDS FILENAME) outputs a vampire input file which 
   computes whether or not P is definable using PREDS modulo theory TH."
  (let (p2 th2 leftpreds rightpreds)
    (multiple-value-setq (p2 th2) (definability-to-interpolation p th preds builtins))
    (setq preds (mapcar #'(lambda (x) (if (symbolp x) (make-parameter :symbol x) x)) preds))
    (setq leftpreds (set-difference (preds (makand p (maksand (contents th)))) preds :key #'parameter-symbol))
    (setq rightpreds (set-difference (preds (makand p2 (maksand (contents th2)))) preds :key #'parameter-symbol))
    (setq leftpreds (delete '= leftpreds :key #'parameter-symbol))
    (setq rightpreds (delete '= rightpreds :key #'parameter-symbol))
    (format t "~&leftpreds: ~A~%" leftpreds)
    (format t "~&rightpreds: ~A~%" rightpreds)
    (with-open-file (f filename :direction :output :if-does-not-exist :create :if-exists :supersede)
      ; options
      (format f "vampire(option,show_interpolant,on).~%")
      (when time (format f "vampire(option,time_limit,~A).~%" time))
      ; right and left symbols
      (dolist (s leftpreds)
	(format f "vampire(symbol,predicate,")
	(kif2tptp-constant (parameter-symbol s) f)
	(format f ",~A,left).~%" (parameter-arity s)))
      (dolist (s rightpreds)
	(format f "vampire(symbol,predicate,")
	(kif2tptp-constant (parameter-symbol s) f)
	(format f ",~A,right).~%" (parameter-arity s)))
      ; right and left formulas
      (format f "vampire(left_formula).~%")
      (kif2tptp p 'axiom f t)
      (format f "vampire(end_formula).~%")
      (format f "vampire(right_formula).~%")
      (kif2tptp p2 'conjecture f t)
      (format f "vampire(end_formula).~%")
      ; theory
      (print (append th th2))
      (dolist (q th) (kif2tptp q 'axiom f t))
      (dolist (q th2) (kif2tptp q 'axiom f t)))))

(defun entailment-vampire-file (p th filename &key (time 10) (builtins nil))
  (declare (ignore builtins))
  (setq th (contents th))
  (with-open-file (f filename :direction :output :if-does-not-exist :create :if-exists :supersede)
      ; options
    (when time (format f "vampire(option,time_limit,~A).~%" time))
    (dolist (q th) (kif2tptp q 'axiom f f))
    (kif2tptp p 'conjecture f)))

(defun consistency-vampire-file (th filename &key (time 10) (builtins nil))
  (declare (ignore builtins))
  (with-open-file (f filename :direction :output :if-does-not-exist :create :if-exists :supersede)
      ; options
    (when time (format f "vampire(option,time_limit,~A).~%" time))
    (dolist (q th) (kif2tptp q 'conjecture f t))))

; This doesn't work. 
;  The construction below is complete but unsound.  IF there is a definition,
;  the construction ensures we will find an interpolant.  But not all interpolants
;  of the construction below are definitions.
(defun definability-to-interpolation (p th preds &optional (builtins nil))
  "(DEFINABILITY-TO-INTERPOLATION P TH PREDS) returns 2 values P' and TH' such that
   TH U TH' implies P <=> P' and the only symbols in the intersection of P and P'
   (including TH and TH') are PREDS.  Does not change any BUILTINS.
   Thus, P is definable in terms of PREDS iff there is an interpolant of P => P'."
  (let (allpreds predstoelim bl th2 p2)
    (setq allpreds (relns (makand p (maksand (contents th)))))
    (setq predstoelim (set-difference (set-difference allpreds preds) builtins))
    (setq bl (mapcar #'(lambda (x) (cons x (gentemp (tostring x)))) predstoelim))
    (setq th2 (and2list (subrel bl (maksand (contents th)))))
    (setq p2 (subrel bl p))
    (values p2 th2)))


;;;;;;;;;; Epilog ;;;;;;;;;; 

(defun abduction-epilog (p th preds &optional (builtins nil))
  (abduction-epilog-aux p th preds builtins #'fullresidue))

; not really definability, but I can't seem to get vampire to work
(defun definable-epilog (p th preds &optional (builtins nil))
  (let (x)
    (setq x (abduction-epilog-aux p th preds builtins #'fullresidues))
    (if x (maksor x) x)))

(defun abduction-epilog-aux (p th preds builtins func)
  (let (res (*depth* 10) (*consistency-depth* 3))
    (setq preds (mapcar #'paramsym preds))
    (setq builtins (mapcar #'paramsym builtins))
    (multiple-value-setq (p th) (make-literal-query p th))
    (setq th (define-theory (make-instance 'metheory) "" (contrapositives* (maksand (contents th)) builtins)))
    (setq res (funcall func p th #'(lambda (x) (member x preds))))
    ;(break)
    res))

(defun make-literal-query (p th)
  (let (head)
    (cond ((atomicp p) (setq head p))
	  (t 
	   (setq head (cons '__tlh (freevars p)))
	   (setq p (list '<= head p))
	   (setq th (cons p (contents th)))))
    (values head th)))

(defun entailment-epilog (p th &optional (builtins nil))
  (let (head newth res (*depth* 10))
    (setq builtins (mapcar #'paramsym builtins))
    (cond ((atomicp p) (setq head p))
	  (t 
	   (setq head (cons '__tlh (freevars p)))
	   (setq p (list '<= head p))
	   (setq th (cons p (contents th)))))
    ;(pcontents th)
    (setq newth (define-theory (make-instance 'metheory) "" (contrapositives* (maksand (contents th)) builtins)))
    ;(when *debug-webapps* (format t "Trying to define ~A from ~A~%" p preds))
    (setq *tmp* newth)
    (setq res (fullfindp head newth))
    ;(break)
    res))
  



