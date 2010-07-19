
# Makefile for CLICL

LOAD=(load \"./loader\")
COMPILE=(load \"./compiler\")
SAVE=(save-application \"./clicl\" :prepend-kernel t)
TCP=(starttcp 5000)

app:
	ccl --eval "(progn $(LOAD) $(SAVE) (quit))"	
	
lisp:
	ccl --eval "(progn $(LOAD) $(TCP))"

compile:
	ccl --eval "(progn $(LOAD) $(COMPILE) (quit))"
