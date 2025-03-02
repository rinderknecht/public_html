#-*-makefile-*-

# Neither built-in/suffix rules nor built-in variables
#
.SUFFIXES:
override MAKEFLAGS +=-Rr

MAIN    := FunObj
JAVA    := Stack.java EStack.java NStack.java ${MAIN}.java
TARGETS := ${MAIN}.class

.DEFAULT_GOAL: ${TARGETS}

${TARGETS}: ${JAVA}
	@javac *.java

clean::
	@\rm -f *.class

%: ;
