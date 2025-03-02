#-*-makefile-*-

.SILENT:
.SUFFIX:

BASE := SFunObj
SOURCE := ${BASE}.java
TARGET := ${addsuffix .class,${BASE}}

parts:
	@printf "Making ${TARGET} from ${SOURCE}..."
	@javac -Xlint:unchecked -Werror ${SOURCE}
	@if test $$? -eq 0; then echo " done."; else echo " FAILED."; fi

clean::
	@\rm -f *.class
