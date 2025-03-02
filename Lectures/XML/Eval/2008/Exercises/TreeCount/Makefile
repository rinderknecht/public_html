#-*-makefile-*-

HTML    := counting.html
BASE    := count1 count2 count3 count4
TARGETS := ${BASE:%=%.txt}
XSL     := ${BASE:%=%.xsl}
XML     := toc.xml
DTD     := book.dtd

include common.mk 

.count%.valid: count%.xml count%.dtd
	${call xmllint,$<}

count%.txt: .count%.valid count%.xsl
	${call saxon,$@,count$*.xml,count$*.xsl}

distclean::

%: ;
