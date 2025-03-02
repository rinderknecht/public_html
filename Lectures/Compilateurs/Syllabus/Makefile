#-*-makefile-*-

ECHO    := /bin/echo
RM      := /bin/rm
HTML2PS := none
PS2PDF  := /usr/bin/ps2pdf

PDF     := syllabus.html.pdf

.SUFFIX:
.SILENT:
.PHONY: pdf

sinclude html2ps.mk
sinclude ps2pdf.mk
sinclude distclean.mk

pdf: ${PDF}

clean::
	${RM} -f ${PDF}

distclean::
	${RM} -f Makefile

%: ;
