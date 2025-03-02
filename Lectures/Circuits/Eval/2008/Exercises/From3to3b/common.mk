#-*-makefile-*-

ECHO    := /bin/echo
PRINTF  := /usr/bin/printf
RM      := /bin/rm

A2PS    := none
HTML2PS := none
PS2PDF  := /usr/bin/ps2pdf
GCC     := /usr/bin/gcc
GS      := /usr/bin/gs

PDF     := ${HTML:%=%.pdf} Makefile.pdf ${C:%=%.pdf} 

include precom.mk

sinclude gcc.mk
sinclude gnuc2ps.mk html2ps.mk Makefile2ps.mk a2ps.mk # a2ps.mk last
sinclude ps2pdf.mk gs.mk

${TARGETS}: %: %.c
	@${call gcc,$@,$^}

distclean::
	@\rm -f common.mk
