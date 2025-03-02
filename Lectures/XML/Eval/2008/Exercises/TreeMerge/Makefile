#-*-makefile-*-

HTML    := merge.html
BASE    := tmerge
TARGETS := ${BASE:%=%.txt}
XSL     := ${BASE:%=%.xsl}
XML     := num.xml

# Note: There is no DTD.

include common.mk 

%.txt: %.xml %.xsl
	${call saxon,$@,tmerge.xml,tmerge.xsl}

distclean::

%: ;
