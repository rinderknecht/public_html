#-*-makefile-*-

MODULES := eval eval_opt
ERLANG  := ${MODULES:%=%.erl}
TARGETS := ${MODULES:%=%.beam}

PDF += ${ERLANG:%=%.pdf}

include common.mk 

env:
	${ECHO} ${ARCH_PDF}

distclean::

%: ;
