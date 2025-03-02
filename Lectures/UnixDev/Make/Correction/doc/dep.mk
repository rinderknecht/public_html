HEADERS=$(wildcard src/*.h)

.PHONY: doc

doc: doc/functions.txt

doc/functions.txt: $(HEADERS)
	rm -f doc/functions.txt
	grep -A 1 -h '/\*\*' $^ >> doc/functions.txt

CLEANFILES+=functions.txt
