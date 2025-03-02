# Makefile.am
# Utilisation des variables courantes.

AUTOMAKE_OPTIONS = foreign 1.7
bin_PROGRAMS=foo
foo_LDADD= src/libdisplay.a -lncurses

# Permet de rajouter des fichiers dans la tarball de distribution.
EXTRA_DIST=BUGS

# Défini des sous-répertoires pour make.
SUBDIRS= tests src doc

# Certains fichiers doivent être détruits par make clean
# en plus des fichiers standards
CLEANFILES= *~




