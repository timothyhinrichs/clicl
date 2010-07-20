;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Researchmaster load file
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; assumes infomaster is already loaded

; infomaster files (global vars used in some functions)
(defparameter man (loadfn "manager" :dir "state" :type "kif"))
(defparameter lib (loadfn "library" :dir "state" :type "kif"))
(defparameter rul (loadfn "rules" :dir "state" :type "kif"))

; a more secure version of dualserver
(load (loadfn "secureserver" :dir "source"))
	
; automated reasoning library
(load (loadfn "lisp-enhance" :dir '("source" "logic")))
(load (loadfn "analysis" :dir '("source" "logic")))
(load (loadfn "indexing" :dir '("source" "logic")))
(load (loadfn "normalforms" :dir '("source" "logic")))
(load (loadfn "grounding" :dir '("source" "logic")))
(load (loadfn "interpolation" :dir '("source" "logic")))
(load (loadfn "reformulation" :dir '("source" "logic")))
(load (loadfn "fhlc" :dir '("source" "logic")))
(load (loadfn "solvers" :dir '("source" "logic")))
(load (loadfn "fhlc" :dir '("source" "logic")))
(load (loadfn "external" :dir '("source" "logic")))
(load (loadfn "empirical" :dir '("source" "logic")))
(load (loadfn "esodatalog" :dir '("source" "logic")))

; extensions to AR library -- only loaded to ensure we know when
;    step on the toes of these routines.
(load (loadfn "educational" :dir '("source" "logic")))
(load (loadfn "stringsolver" :dir '("source" "logic")))
(load (loadfn "stringsolvertests" :dir '("source" "logic")))

; still need updating before compatible with current system
;(load (loadfn "gdlfind" :dir '("source" "ggp")))
;(load (loadfn "gamemaster" :dir '("source" "ggp")))
;(load (loadfn "debug" :dir '("source" "ggp")))

; infomaster extensions
(load (loadfn "jack" :dir "source"))
(load (loadfn "examples" :dir "source"))
(load (loadfn "references" :dir "source"))
(load (loadfn "overloads" :dir "source"))
(load (loadfn "spreadsheet" :dir "source"))
(load (loadfn "spreadsheetserver" :dir "source"))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Settings and object initialization
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(setq *log* nil)
(setq *mylog* (namestring (loadfn "log" :type "kif" :dir "state")))
;(defparameter *webformlog* (truename (loadfn "webformlog" :type "kif" :dir "state")))
(defparameter *webformlog* nil)

(resetsystem)

(loaddata man *manager*)
(mapdir #'(lambda (x) (loaddata x *repository*)) 
	(namestring (loadfn "*" :dir '("state" "dumps" "ver6"))))
(loaddata rul *library*)
;(loaddata lib *myviewserver*)
(loadlog *mylog* *repository*)
(includes *repository* *library*)

; reference hierarchy
(setq *displayhier* (init-displayhierarchy (first (read-file (namestring (loadfn "displayhier" :dir "state" :type "lisp"))))))

; topic tag hierarchy
(setq *tagfile* (namestring (loadfn "tagtheory" :dir '("state" "theories") :type "kif")))
(setq *tagtheory* (make-instance 'prologtheory))
(define-prologtheory *tagtheory* "" (construct-datalog-theory (read-file *tagfile*)))
(includes *repository* *tagtheory*)

; tag-based authorization policy
(setq *authfile* (namestring (loadfn "authpolicy" :dir '("state" "theories") :type "kif")))
(setq *auththeory* (make-instance 'prologtheory))
(define-prologtheory *auththeory* "" (mapcar #'morph-tag-queries (read-file *authfile*)))
(includes *repository* *auththeory*)

; initialize webform-database for websheet server
(setq *webform-db* (make-hash-table))
(ws-server-register-form (make-webformdb :name 'plfeature2 :package :plfeature2 :data nil
	:htmlfile "researchmaster/wsgallery/plfeature2.html"
	:lispfile "researchmaster/wsgallery/plfeature2.lisp"
	:widgets '(e m o fs c)))



; enable logging only after loading and logloading
(defmethod insert (p (th (eql *repository*)))
  (when *log* (logmessage `(pos ,p) *log*))
  (call-next-method p th))

(defmethod uninsert (p (th (eql *repository*)))
  (when *log* (logmessage `(neg ,p) *log*))
  (call-next-method p th))

(setq *log* *mylog*)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
