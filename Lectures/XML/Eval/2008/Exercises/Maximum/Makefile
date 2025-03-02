#-*-makefile-*-

HTML    := maximum.html
BASE    := max1 max2 max3
TARGETS := ${BASE:%=%.txt}
XSL     := ${BASE:%=%.xsl}
XML     := numbers.xml
DTD     := numbers.dtd

include common.mk 

.max%.valid: max%.xml max%.dtd
	${call xmllint,$<}

max%.txt: .max%.valid max%.xsl
	${call saxon,$@,max$*.xml,max$*.xsl}

distclean::

%: ;
