#-*-makefile-*-

HTML     := csv.html
BASE     := csv1 csv2 csv3
TARGETS  := ${BASE:%=%.txt}
XSL      := ${BASE:%=%.xsl}
XML_BASE := csv csv_att
XML      := ${XML_BASE:%=%.xml}
DTD      := ${XML_BASE:%=%.dtd}

include common.mk 

.csv%.valid: csv%.xml csv%.dtd
	${call xmllint,$<}

csv%.txt: .csv%.valid csv%.xsl
	${call saxon,$@,csv$*.xml,csv$*.xsl}

distclean::

%: ;
