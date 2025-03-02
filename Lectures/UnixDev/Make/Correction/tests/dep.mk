INFILES=$(wildcard tests/*.in)
OUTFILES=$(INFILES:.in=.out)
BASEFILES=$(INFILES:.in=)

check: $(PROGRAM) $(OUTFILES)
	@ for i in $(BASEFILES); do \
	  (diff -B $$i.expected $$i.out &> /dev/null \
	  && echo "[OK] $$i") || \
	     echo "[KO] $$i"; \
	done 

$(OUTFILES): $(PROGRAM)

%.out:%.in
	$(PROGRAM) `cat $<` > $@

CLEANFILES+=$(OUTFILES)