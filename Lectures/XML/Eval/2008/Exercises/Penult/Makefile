#-*-makefile-*-

HTML    := penultimate.html
BASE    := penult
TARGETS := ${BASE:%=%.xml}
XSL     := ${BASE:%=%.xsl}
XML     := toc.xml
DTD     := book.dtd

include common.mk 

.penult.valid: toc.xml book.dtd
	${call xmllint,$<}

penult.xml: .penult.valid penult.xsl
	${call saxon,$@,toc.xml,penult.xsl}

distclean::

%: ;
