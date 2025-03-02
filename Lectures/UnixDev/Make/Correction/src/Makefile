#-*-makefile-*-

# Project specific variables
include ../makefile.conf
PROGRAM=main
SOURCES=$(wildcard *.c) 

# Generic part of the Makefile
OBJECTS=$(patsubst %.c,%.o, $(SOURCES))

.PHONY: all clean 

all: $(PROGRAM)

%.d: %.c
	$(CC) -MM $(CPPFLAGS) $< > $@.$$$$; \
	sed 's,\($*\)\.o[ :]*,\1.o $@ : ,g' < $@.$$$$ > $@; \
	rm -f $@.$$$$

$(PROGRAM):$(OBJECTS)

clean:
	rm -f main *.o *~ \#* *.d

include $(SOURCES:.c=.d)
