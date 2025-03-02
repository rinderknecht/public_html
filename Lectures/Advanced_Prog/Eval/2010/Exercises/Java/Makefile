#-*-makefile-*-

# Neither built-in/suffix rules nor built-in variables
#
.SUFFIXES:
override MAKEFLAGS +=-Rr

MAIN    := FunObj
JAVA    := Stack.java EStack.java NStack.java BST.java Ext.java Int.java \
           ${MAIN}.java
TARGETS := ${MAIN}.class

override PDF := common.mk.in.pdf Makefile.pdf # Before include

include common.mk

distclean::
	@\rm -f common.mk

%: ;
