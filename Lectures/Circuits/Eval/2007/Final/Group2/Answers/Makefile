#-*-makefile-*-

SRC := from10toNeg2

parts:
	@printf "Making ${SRC} from ${SRC}.c..."
	@gcc -std=c99 -Werror -pedantic-errors -o ${SRC} ${SRC}.c
	@if test $$? -eq 0; then echo " done."; else " FAILED."; fi

clean::
	@\rm -f *.o ${SRC}

%: ;
