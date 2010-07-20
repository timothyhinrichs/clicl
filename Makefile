
# Makefile for CLICL
LOAD=(load \"$(CURDIR)/loader\")
COMPILE=(load \"$(CURDIR)/compiler\")
SAVE=(save-application \"$(CURDIR)/clicl\" :init-file \"$(CURDIR)/clicl-init.lisp\" :prepend-kernel t)
TCP=(starttcp 5000)
QUIT=(quit)

# writes out an executable encapsulating the logic manipulaiton functionality
app:
	ccl --eval "(progn $(LOAD) $(SAVE) $(QUIT))"	
	
# loads the lisp environment and starts the tcp server
lisp:
	ccl --eval "(progn $(LOAD) $(TCP))"

# compiles the individual .lisp files to make loading them more efficient
compile:
	ccl --eval "(progn $(LOAD) $(COMPILE) $(QUIT))"
