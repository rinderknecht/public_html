#-*-makefile-*-

.SECONDARY:

TOUCH   := /usr/bin/touch
ECHO    := /bin/echo
PRINTF  := /usr/bin/printf
RM      := /bin/rm

A2PS    := none
HTML2PS := none
PS2PDF  := /usr/bin/ps2pdf
XMLLINT := /usr/bin/xmllint
JAVA    := /usr/bin/java
GS      := /usr/bin/gs

PDF     := ${HTML:%=%.pdf} ${XSL=%.pdf} Makefile.pdf \
           ${XML:%=%.pdf} ${DTD:%=%.pdf}

include precom.mk

sinclude xmllint.mk saxon.mk
sinclude xml2ps.mk html2ps.mk Makefile2ps.mk a2ps.mk # a2ps.mk last
sinclude ps2pdf.mk gs.mk

distclean::
	@${RM} -f common.mk

clean::
	@${RM} -f .*.valid
