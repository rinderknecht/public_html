#-*-makefile-*-

.SILENT:
.SUFFIX:

TARGETS := self

parts: ${TARGETS}

%: %.c
	@printf "Making $@ from $^..."
	@gcc -std=c99 -Werror -pedantic-errors -o $@ $^
	@if test $$? -eq 0; then echo " done."; else " FAILED."; fi

clean::
	\rm -f *.o ${TARGETS}

%: ;
