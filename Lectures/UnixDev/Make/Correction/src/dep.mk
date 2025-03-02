PROGRAM=src/main
SOURCES=$(wildcard src/*.c) 

# Generic part of the Makefile
OBJECTS=$(patsubst %.c,%.o, $(SOURCES))
DEPENDS=$(SOURCES=.c=.d)

%.d: %.c
	$(CC) -M $(CPPFLAGS) $< > $@.$$$$; \
	sed 's,\($*\)\.o[ :]*,\1.o $@ : ,g' < $@.$$$$ > $@; \
	rm -f $@.$$$$

$(PROGRAM):$(OBJECTS) $(DEPENDS)

CLEANFILES+=$(OBJECTS) $(PROGRAM) $(DEPENDS)

sinclude $(SOURCES:.c=.d)
