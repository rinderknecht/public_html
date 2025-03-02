#-*-makefile-*-

HTML    := squeeze.html
BASE    := squeeze
TARGETS := ${BASE:%=%.xml}
XSL     := squeeze.xsl
XML     := numbers.xml
DTD     := numbers.dtd

include common.mk 

.numbers.valid: numbers.xml numbers.dtd
	${call xmllint,$<}

${TARGETS}: .numbers.valid squeeze.xsl
	${call saxon,$@,numbers.xml,squeeze.xsl}

distclean::

%: ;
