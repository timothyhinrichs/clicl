# targets:
#    app: build standalone image and put toplevel apps in ./bin
#    lisp: start lisp, load clicl, and start tcp.  Enter REP loop.
#    compile: compile individual .lisp files

# Makefile for CLICL
LOAD=(load \"$(CURDIR)/loader\")
COMPILE=(load \"$(CURDIR)/compiler\")
TGT=$(CURDIR)/bin/clicl
SAVE=(save-application \"$(TGT)\" :init-file \"$(TGT)-init.lisp\" :prepend-kernel t)
TCP=(starttcp 5000)
QUIT=(quit)

ESODATALOG=(dump-theory (eso-materialize '\$$1 \\\"\$$2\\\") \\\"\$$3\\\")
TEST=(dump-theory (test-all) \\\"$(CURDIR)/testerrors\\\")

# writes out an executable encapsulating the logic manipulation functionality
app:
	ccl --eval "(progn $(LOAD) $(SAVE) $(QUIT))"	
	echo "$(TGT) --eval \"(progn $(ESODATALOG) $(QUIT))\"" > $(CURDIR)/bin/esodatalog
	echo "$(TGT) --eval \"(progn $(TEST) $(QUIT))\"" > $(CURDIR)/bin/test
	echo "cat $(CURDIR)/testerrors" >> $(CURDIR)/bin/test
	chmod 755 $(CURDIR)/bin/esodatalog
	chmod 755 $(CURDIR)/bin/test
	
# loads the lisp environment and starts the tcp server
lisp:
	ccl --eval "(progn $(LOAD) $(TCP))"

# compiles the individual .lisp files to make loading them more efficient
compile:
	ccl --eval "(progn $(LOAD) $(COMPILE) $(QUIT))"
