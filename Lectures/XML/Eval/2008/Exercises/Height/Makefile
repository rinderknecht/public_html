#-*-makefile-*-

HTML    := height.html
BASE    := height1 height2 height3 height4
TARGETS := ${BASE:%=%.txt}
XSL     := ${BASE:%=%.xsl}
XML     := toc.xml
DTD     := book.dtd

include common.mk

.height%.valid: height%.xml height%.dtd
	${call xmllint,$<}

height%.txt: .height%.valid height%.xsl
	${call saxon,$@,height$*.xml,height$*.xsl}

distclean::

%: ;
