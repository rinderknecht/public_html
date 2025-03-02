#-*-makefile-*-

# Neither built-in/suffix rules nor built-in variables
#
.SUFFIXES:
override MAKEFLAGS +=-Rr

MAIN    := EphObj
JAVA    := Stack.java Cell.java ${MAIN}.java
TARGETS := ${MAIN}.class

.DEFAULT_GOAL: ${TARGETS}

${TARGETS}: ${JAVA}
	@javac ${MAIN}.java

clean::
	@\rm -f *.class

%: ;
