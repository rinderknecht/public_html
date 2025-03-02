#-*-makefile-*-

parts:
	@printf "Making from3bto3 from from3bto3.c..."
	@gcc -std=c99 -Werror -pedantic-errors -o from3bto3 from3bto3.c
	@if test $$? -eq 0; then echo " done."; else " FAILED."; fi

clean::
	@\rm -f *.o from3bto3

%: ;
