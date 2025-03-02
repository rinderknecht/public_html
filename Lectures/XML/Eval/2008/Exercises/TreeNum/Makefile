#-*- makefile -*-

HTML    := numbering.html
BASE    := num1 num2 num3
TARGETS := ${BASE:%=%.html}
XSL     := ${BASE:%=%.xsl}
XML     := toc.xml
DTD     := book.dtd

include common.mk 

.num%.valid: num%.xml num%.dtd
	${call xmllint,$<}

num%.html: num%.xml num%.xsl
	${call saxon,$@,num$*.xml,num$*.xsl}

distclean::

%: ;
