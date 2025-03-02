#-*-makefile-*-

parts:
	@printf "Making op2c from op2c.c..."
	@gcc -std=c99 -Werror -pedantic-errors -o op2c op2c.c
	@if test $$? -eq 0; then echo " done."; else " FAILED."; fi

clean::
	@\rm -f *.o op2c

%: ;
