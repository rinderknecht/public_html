#-*-makefile-*-

.SUFFIXES:
.SILENT:

BASE := tmerge
TARGETS := ${addsuffix .txt,${BASE}}

parts: ${TARGETS}

%.txt: num.xml %.xsl
	@${saxon}

clean::
	@\rm -f ${TARGETS}

%: ;

define saxon
printf "Making $@ from $^..."; \
java -jar $$HOME/bin/saxon.jar -o:$@ $^; \
if test $$? -eq 0; then echo " done."; else echo " FAILED."; fi
endef
