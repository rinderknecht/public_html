#-*-makefile-*-

HTML    := merging.html
TARGETS := merge1_out.xml merge2.txt
XSL     := merge1.xsl merge2.xsl
XML     := ordered_lists.xml
DTD     := list.dtd

include common.mk 

.merge%.valid: merge%.xml merge%.dtd
	${call xmllint,$<}

merge%.txt merge%_out.xml: .merge%.valid merge%.xsl
	${call saxon,$@,merge$*.xml,merge$*.xsl}

distclean::

%: ;
