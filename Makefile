
# Makefile for CLICL

LOAD=(load \"./loader\")
COMPILE=(load \"./compiler\")
SAVE=(save-application \"./clicl\" :prepend-kernel t)
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
