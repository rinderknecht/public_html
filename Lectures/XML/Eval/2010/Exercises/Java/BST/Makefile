#-*-makefile-*-

.SILENT:
.SUFFIX:

BASE := BFunObj
TARGETS := ${addsuffix .class,${BASE}}

parts: ${TARGETS}

%.class: %.java
	@printf "Making $@ from $^..."
	@javac -Xlint:unchecked -Werror $^
	@if test $$? -eq 0; then echo " done."; else echo " FAILED."; fi

clean::
	@\rm -f *.class
