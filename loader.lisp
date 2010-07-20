;;;;;;;; function for loader-file relative paths ;;;;;;;;

(defun loadfn (name &key (dir nil) (type nil))
  "(LOADFN DIR NAME) computes filename relative to load directory."
  (when (not (listp dir)) (setq dir (list dir)))
    (make-pathname :name name :type type
      :directory (append (pathname-directory *load-pathname*) dir)))

(defun compilefn (name &key (dir nil) (type nil))
  "(COMPILEFN DIR NAME) computes filename relative to load directory."
  (when (not (listp dir)) (setq dir (list dir)))
    (make-pathname :name name :type type
      :directory (append (pathname-directory *compile-file-pathname*) dir)))

;;;;;;;; Loading subsystems ;;;;;;;;

; check if snark available
(ignore-errors (when (probe-file (loadfn "snark")) (push 'snark *features*)))

; epilog
(load (loadfn "loader" :dir "epilog"))

; infomaster
(load (loadfn "loader" :dir "infomaster"))

; snark: load if possible
#+snark (progn
	  (load (loadfn "snark-system" :type "lisp" :dir "snark"))
	  (make-snark-system)
	  (in-package :common-lisp-user))
;(defpackage :snark)
;(defpackage :snark-user)
;(defpackage :snark-lisp)

; templates
(load (loadfn "load" :dir "cl-ppcre-2.0.3"))
(load (loadfn "load" :dir "cl-emb-0.4.4"))

; global variables (set before researchmaster, as it loads code using web interface)
(setq *host* (hostname))
(setq *port* 3000)
(setq *home* "/docserver/")
(setq *homedir* (namestring (loadfn nil)))	
(setq *web* (stringappend "http://" *host* ":" (princ-to-string *port*)))

; researchmaster
(load (loadfn "loader" :dir "researchmaster"))
