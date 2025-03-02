#-*-makefile-*-

HTML    := flipping.html
BASE    := flip1 flip1 flip2 flip3 flip4 flip5
TARGETS := ${BASE:%=%_out.xml}
XSL     := ${BASE:%=%.xsl}
XML     := toc.xml toc_simple.xml
DTD     := book.dtd book_simple.dtd

include common.mk 

.flip%.valid: flip%.xml flip%.dtd
	${call xmllint,$<}

flip%_out.xml: .flip%.valid flip%.xsl
	${call saxon,$@,flip$*.xml,flip$*.xsl}

distclean::

%: ;
