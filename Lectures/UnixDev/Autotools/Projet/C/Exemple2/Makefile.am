# Makefile.am
# Utilisation des variables courantes.

AUTOMAKE_OPTIONS = foreign 1.7
bin_PROGRAMS=foo
foo_LDADD= src/libdisplay.a -lncurses

# Permet de rajouter des fichiers dans la tarball de distribution.
EXTRA_DIST=BUGS

# D�fini des sous-r�pertoires pour make.
SUBDIRS= tests src doc

# Certains fichiers doivent �tre d�truits par make clean
# en plus des fichiers standards
CLEANFILES= *~




