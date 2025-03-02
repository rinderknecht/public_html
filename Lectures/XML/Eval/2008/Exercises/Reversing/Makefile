#-*-makefile-*-

HTML    := reversing.html
BASE    := rev1 rev2
TARGETS := rev1_out.xml rev2_out.html
XSL     := ${BASE:%=%.xsl}
XML     := toc.xml
DTD     := book.dtd

include common.mk

.rev%.valid: rev%.xml rev%.dtd
	${call xmllint,$<}

rev%_out.xml: .rev%.valid
	${call saxon,$@,rev$*.xml,rev$*.xsl}

rev%_out.html: .rev%.valid
	${call saxon,$@,rev$*.xml,rev$*.xsl}

distclean::

%: ;
