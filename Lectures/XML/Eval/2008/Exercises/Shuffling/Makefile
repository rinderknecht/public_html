#-*-makefile-*-

HTML    := shuffling.html
BASE    := shuffle1 shuffle2 shuffle3
TARGETS := ${BASE:%=%_1.xml} ${BASE:%=%_2.xml} \
           ${BASE:%=%_3.xml} shuffle4_out.xml
XSL     := ${BASE:%=%.xsl}
XML     := persons1.xml persons2.xml persons3.xml persons4.xml
DTD     := persons.dtd

include common.mk 

.shuffle%.valid: shuffle%.xml shuffle%.dtd
	${call xmllint,$<}

shuffle%_1.xml: .shuffle%.valid
	${call saxon,$@,shuffle$*.xml,shuffle$*.xsl}

shuffle%_2.xml: .shuffle%.valid
	${call saxon,$@,shuffle$*.xml,shuffle$*.xsl}

shuffle%_3.xml: .shuffle%.valid
	${call saxon,$@,shuffle$*.xml,shuffle$*.xsl}

shuffle4_out.xml: shuffle4.xml shuffle4.xsl
	${call saxon,$@,shuffle4.xml,shuffle4.xsl}

distclean::

%: ;
