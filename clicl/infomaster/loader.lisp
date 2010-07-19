;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; (c) Copyright 1999-2007 by Michael Genesereth.  All rights reserved.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(load (loadfn "magenta" :dir "source"))
(load (loadfn "factserver" :dir "source"))
(load (loadfn "fastserver" :dir "source"))
(load (loadfn "dataserver" :dir "source"))
(load (loadfn "viewserver" :dir "source"))
(load (loadfn "fullserver" :dir "source"))
(load (loadfn "dualserver" :dir "source"))
(load (loadfn "diffserver" :dir "source"))
(load (loadfn "ruleserver"  :dir "source"))

(load (loadfn "sha1" :dir "source"))
(load (loadfn "logic" :dir "source"))
(load (loadfn "sequel" :dir "source"))
(load (loadfn "formats" :dir "source"))
(load (loadfn "tcp.ccl" :dir "source"))
(load (loadfn "http" :dir "source"))
(load (loadfn "aclhandler" :dir "source"))
(load (loadfn "sqlhandler" :dir "source"))
(load (loadfn "soaphandler-new" :dir "source"))
(load (loadfn "htmlhandler" :dir "source"))
(load (loadfn "docserver" :dir "source"))
(load (loadfn "aclserver" :dir "source"))
(load (loadfn "sqlserver" :dir "source"))
(load (loadfn "soapserver" :dir "source"))
(load (loadfn "protabula" :dir "source"))
(load (loadfn "proforma" :dir "source"))
(load (loadfn "conforma" :dir "source"))
(load (loadfn "prologica" :dir "source"))
(load (loadfn "leftovers" :dir "source"))
(load (loadfn "interface" :dir "source"))
(load (loadfn "remote" :dir "source"))
(load (loadfn "security" :dir "source"))

; all of the below seem out of date, but miscellaneous instantiates some of them
(load (loadfn "translator" :dir "source"))
(load (loadfn "transformer" :dir "source"))
(load (loadfn "integrator" :dir "source"))
(load (loadfn "facilitator" :dir "source"))

(load (loadfn "check" :dir "source"))
(load (loadfn "miscellaneous" :dir "source"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Settings
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(setq *host* (hostname))
(setq *port* 3000)
(setq *home* "/docserver/")
(setq *homedir* (namestring *load-pathname*))
(setq *web* (stringappend "http://" *host* ":" (princ-to-string *port*)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Tests
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
#|
(test "gullible:infomaster:tests:factserver.test")
(test "gullible:infomaster:tests:viewserver.test")
(test "gullible:infomaster:tests:fullserver.test")
(test "gullible:infomaster:tests:dataserver.test")
(test "gullible:infomaster:tests:diffserver.test")
(test "gullible:infomaster:tests:ruleserver.test")
(test "gullible:infomaster:tests:fastserver.test")
(test "gullible:infomaster:tests:translator.test")
(test "gullible:infomaster:tests:transformer.test")
(test "gullible:infomaster:tests:integrator.test")
(test "gullible:infomaster:tests:facilitator.test")
(test "gullible:infomaster:tests:interface.test")

(test "gullible:infomaster:tests:album.test")
(test "gullible:infomaster:tests:cookware.test")
(test "gullible:infomaster:tests:housewares.test")
(test "gullible:infomaster:tests:msrp.test")
(test "gullible:infomaster:tests:nhma.test")
(test "gullible:infomaster:tests:once.test")
(test "gullible:infomaster:tests:university.test")
|#
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
