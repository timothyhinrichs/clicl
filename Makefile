
# Makefile for CLICL
LOAD=(load \"$(CURDIR)/loader\")
COMPILE=(load \"$(CURDIR)/compiler\")
TGT=$(CURDIR)/bin/clicl
SAVE=(save-application \"$(TGT)\" :init-file \"$(TGT)-init.lisp\" :prepend-kernel t)
TCP=(starttcp 5000)
QUIT=(quit)

ESODATALOG=(dump-theory (eso-materialize '\$$1 \\\"\$$2\\\") \\\"\$$3\\\")

# writes out an executable encapsulating the logic manipulation functionality
app:
	ccl --eval "(progn $(LOAD) $(SAVE) $(QUIT))"	
	echo "$(TGT) --eval \"(progn $(ESODATALOG) $(QUIT))\"" > $(CURDIR)/bin/esodatalog
	chmod 755 $(CURDIR)/bin/esodatalog
	
# loads the lisp environment and starts the tcp server
lisp:
	ccl --eval "(progn $(LOAD) $(TCP))"

# compiles the individual .lisp files to make loading them more efficient
compile:
	ccl --eval "(progn $(LOAD) $(COMPILE) $(QUIT))"
