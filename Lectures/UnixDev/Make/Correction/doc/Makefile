HEADERS=$(wildcard ../src/*.h)

.PHONY: doc

doc:functions.txt

functions.txt: $(HEADERS)
	rm -f functions.txt
	grep -A 1 -h '/\*\*' $^ >> functions.txt

clean:
	rm -f functions.txt
