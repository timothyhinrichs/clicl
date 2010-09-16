;;;;;;;; function for loader-file relative paths ;;;;;;;;

(defun loadfn (name &key (dir nil) (type nil) (root *load-pathname*))
  "(LOADFN DIR NAME) computes filename relative to load directory."
  (when (not (listp dir)) (setq dir (list dir)))
    (make-pathname :name name :type type
      :directory (append (pathname-directory root) dir)))

(defun compilefn (name &key (dir nil) (type nil) (root *compile-file-pathname*))
  "(COMPILEFN DIR NAME) computes filename relative to load directory."
  (when (not (listp dir)) (setq dir (list dir)))
    (make-pathname :name name :type type
      :directory (append (pathname-directory root) dir)))

;;;;;;;; Loading subsystems ;;;;;;;;

; check if snark available
(ignore-errors (when (probe-file (loadfn "snark")) (push :snark *features*)))
#+snark 
(format t "~&Snark is available.~%")
#-snark
(format t "~&Snark is unavailable.~%")

(defvar *localrootdir* *load-pathname*)

; external libraries
(load (loadfn "load" :dir "cl-ppcre-2.0.3"))  ; regular expressions
(load (loadfn "load" :dir "cl-emb-0.4.4"))   ; templates
;(load (loadfn "lex" :dir "cl-lex"))
(load (loadfn "load" :dir '("dso-lex-0.3.2" "dso-util-0.1.2")))  ; lex
(load (loadfn "load" :dir "dso-lex-0.3.2"))  ; lex
(load (loadfn "yacc" :dir "cl-yacc-0.3"))   ; yacc

; epilog
(load (loadfn "loader" :dir "epilog"))

; infomaster
(load (loadfn "loader" :dir "infomaster"))

; snark: load if possible
#+snark (progn
	  (load (loadfn "snark-system" :type "lisp" :dir "snark"))
	  (make-snark-system)
	  (in-package :common-lisp-user))


; global variables (set before researchmaster, as it loads code using web interface)
(setq *host* (hostname))
(setq *port* 3000)
(setq *home* "/docserver/")
(setq *homedir* (namestring (loadfn nil)))	
(setq *web* (stringappend "http://" *host* ":" (princ-to-string *port*)))

; researchmaster
(load (loadfn "loader" :dir "researchmaster"))

