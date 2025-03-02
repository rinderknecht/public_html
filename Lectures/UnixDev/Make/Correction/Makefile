# Makefile

include makefile.conf
SUBDIRS = src doc tests

.PHONY: all doc check clean

ifeq ("${RECURSIVE_MAKE}","yes")
all:
	cd src && $(MAKE)

doc:
	cd doc && $(MAKE) doc

check:
	cd tests && $(MAKE) check

clean:
	for dir in $(SUBDIRS); do \
	  $(MAKE) clean -C $$dir; \
	done
else
include src/dep.mk
include doc/dep.mk
include tests/dep.mk
clean:
	$(RM) $(CLEANFILES)

endif
