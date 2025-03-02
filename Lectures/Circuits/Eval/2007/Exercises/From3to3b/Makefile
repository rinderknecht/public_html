#-*-makefile-*-

parts:
	@printf "Making from3to3b from from3to3b.c..."
	@gcc -std=c99 -Werror -pedantic-errors -o from3to3b from3to3b.c
	@if test $$? -eq 0; then echo " done."; else " FAILED."; fi

clean::
	@\rm -f *.o from3to3b

%: ;
