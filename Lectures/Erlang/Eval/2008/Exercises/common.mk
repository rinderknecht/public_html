#-*-makefile-*-

ECHO    := /bin/echo
PRINTF  := /usr/bin/printf
RM      := /bin/rm

A2PS    := none
PS2PDF  := /usr/bin/ps2pdf
ERLC    := /usr/bin/erlc
GS      := /usr/bin/gs

MODULES := hw bt bst catalan rbt counter
ERLANG  := ${MODULES:%=%.erl}
TARGETS := ${MODULES:%=%.beam}

PDF     := ${ERLANG:%=%.pdf} Makefile.pdf

include precom.mk

sinclude erlc.mk
sinclude erlang2ps.mk Makefile2ps.mk a2ps.mk # a2ps.mk last
sinclude ps2pdf.mk gs.mk

${TARGETS}: %.beam: %.erl
	@${call erlc,$@,$^}

distclean::
	@\rm -f common.mk
