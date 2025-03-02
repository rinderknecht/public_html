#-*-makefile-*-

HTML    := summing.html
BASE    := sum1 sum2
TARGETS := ${BASE:%=%.txt}
XSL     := ${BASE:%=%.xsl}
XML     := numbers.xml
DTD     := numbers.dtd

include common.mk 

.sum%.valid: sum%.xml sum%.dtd
	${call xmllint,$<}

sum%.txt: .sum%.valid sum%.xsl
	${call saxon,$@,sum$*.xml,sum$*.xsl}

distclean::

%: ;
