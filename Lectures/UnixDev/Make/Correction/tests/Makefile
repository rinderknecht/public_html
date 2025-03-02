
TOPSRCDIR=..
include ../makefile.conf
INFILES=$(wildcard *.in)
OUTFILES=$(INFILES:.in=.out)
BASEFILES=$(INFILES:.in=)

check: $(PROGRAM) $(OUTFILES)
	@ for i in $(BASEFILES); do \
	  (diff -B $$i.expected $$i.out &> /dev/null \
	  && echo "[OK] $$i") || \
	     echo "[KO] $$i"; \
	done 

$(PROGRAM):
	cd ../src && make

$(OUTFILES): $(PROGRAM)

%.out:%.in
	$(PROGRAM) `cat $<` > $@

clean: 
	rm -f *.out
