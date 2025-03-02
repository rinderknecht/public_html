#-*-makefile-*-

MODULES := calder_trees cps
ERLANG  := ${MODULES:%=%.erl}
TARGETS := ${MODULES:%=%.beam}

PDF += ${ERLANG:%=%.pdf}

include common.mk 

env:
	${ECHO} ${ARCH_PDF}

distclean::

%: ;
