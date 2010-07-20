;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Compile Infomaster
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(compile-file (loadfn "magenta" :dir "source"))
(compile-file (loadfn "factserver" :dir "source"))
(compile-file (loadfn "fastserver" :dir "source"))
(compile-file (loadfn "dataserver" :dir "source"))
(compile-file (loadfn "viewserver" :dir "source"))
(compile-file (loadfn "fullserver" :dir "source"))
(compile-file (loadfn "dualserver" :dir "source"))
(compile-file (loadfn "diffserver" :dir "source"))
(compile-file (loadfn "ruleserver"  :dir "source"))

(compile-file (loadfn "sha1" :dir "source"))
(compile-file (loadfn "logic" :dir "source"))
(compile-file (loadfn "sequel" :dir "source"))
(compile-file (loadfn "formats" :dir "source"))
(compile-file (loadfn "tcp.ccl" :dir "source"))
(compile-file (loadfn "http" :dir "source"))
(compile-file (loadfn "aclhandler" :dir "source"))
(compile-file (loadfn "sqlhandler" :dir "source"))
(compile-file (loadfn "soaphandler-new" :dir "source"))
(compile-file (loadfn "htmlhandler" :dir "source"))
(compile-file (loadfn "docserver" :dir "source"))
(compile-file (loadfn "aclserver" :dir "source"))
(compile-file (loadfn "sqlserver" :dir "source"))
(compile-file (loadfn "soapserver" :dir "source"))
(compile-file (loadfn "protabula" :dir "source"))
(compile-file (loadfn "proforma" :dir "source"))
(compile-file (loadfn "conforma" :dir "source"))
(compile-file (loadfn "prologica" :dir "source"))
(compile-file (loadfn "leftovers" :dir "source"))
(compile-file (loadfn "interface" :dir "source"))
(compile-file (loadfn "remote" :dir "source"))
(compile-file (loadfn "security" :dir "source"))

; all of the below seem out of date, but miscellaneous instantiates some of them
(compile-file (loadfn "translator" :dir "source"))
(compile-file (loadfn "transformer" :dir "source"))
(compile-file (loadfn "integrator" :dir "source"))
(compile-file (loadfn "facilitator" :dir "source"))

(compile-file (loadfn "check" :dir "source"))
(compile-file (loadfn "miscellaneous" :dir "source"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
