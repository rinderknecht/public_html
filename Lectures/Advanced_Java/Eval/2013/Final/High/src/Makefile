#-*-makefile-*-

# Neither built-in/suffix rules nor built-in variables
#
.SUFFIXES:
override MAKEFLAGS +=-Rr

MAIN    := FunObj
JAVA    := BST.java Ext.java Int.java \
           Found.java \
           Stack.java EStack.java NStack.java \
           Pair.java \
           LTree.java Leaf.java Node.java \
           ${MAIN}.java
TARGETS := ${MAIN}.class

.DEFAULT_GOAL: ${TARGETS}

${TARGETS}: ${JAVA}
	@javac ${MAIN}.java

clean::
	@\rm -f *.class

%: ;
