#-*-makefile-*-

HTML    := last.html
BASE    := last
TARGETS := ${BASE:%=%.xml}
XSL     := ${BASE:%=%.xsl}
XML     := toc.xml
DTD     := book.dtd

include common.mk 

.last.valid: toc.xml book.dtd
	${call xmllint,$<}

last.xml: .last.valid last.xsl
	${call saxon,$@,toc.xml,last.xsl}

distclean::

%: ;
