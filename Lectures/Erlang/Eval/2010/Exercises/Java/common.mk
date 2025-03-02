#-*-makefile-*-

ECHO    := /bin/echo
PRINTF  := /usr/bin/printf
RM      := /bin/rm

A2PS    := none
PS2PDF  := /usr/bin/ps2pdf
JAVAC   := /usr/bin/javac
GS      := /usr/bin/gs

PDF     := ${JAVA:%=%.pdf} Makefile.pdf

include precom.mk

sinclude erlc.mk
sinclude javac.mk
sinclude java2ps.mk Makefile2ps.mk a2ps.mk # a2ps.mk last
sinclude ps2pdf.mk gs.mk

${TARGETS}: ${JAVA}
	@${call javac,${MAIN}}

distclean::
	@\rm -f common.mk
