#-*-makefile-*-

HTML    := sum.html
TARGETS := sum.txt
XSL     := sum.xsl
XML     := sum.xml
DTD     := sum.dtd

include common.mk 

.sum.valid: sum.xml sum.dtd
	${call xmllint,$<}

sum.txt: .sum.valid sum.xsl
	${call saxon,$@,sum.xml,sum.xsl}

clean::
	${RM} -f .sum.valid

distclean::

%: ;
