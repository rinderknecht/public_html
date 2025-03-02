#-*-makefile-*-

ECHO    := /bin/echo
PRINTF  := /usr/bin/printf
RM      := /bin/rm

A2PS    := none
PS2PDF  := /usr/bin/ps2pdf
SWIPL   := none
GS      := /usr/bin/gs

MODULES := hw bt gt
PROLOG  := ${MODULES:%=%.P}
TARGETS := ${MODULES:%=%.out}

PDF     := ${PROLOG:%=%.pdf} Makefile.pdf

include precom.mk

sinclude swipl.mk
sinclude prolog2ps.mk Makefile2ps.mk a2ps.mk # a2ps.mk last
sinclude ps2pdf.mk gs.mk

${TARGETS}: %.out: %.pl
	@${call swipl,$@,$^}

distclean::
	@\rm -f common.mk
