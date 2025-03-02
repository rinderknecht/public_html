# configure.ac
AC_INIT(configure.ac)
AM_CONFIG_HEADER(config.h)
AM_INIT_AUTOMAKE(fooproject, 0.1)
AC_PROG_CC
AC_PROG_RANLIB

# Testons l'existence de la bibliotheque ncurses sur le systeme.
# On teste en meme temps que les fonctions utilisees sont bien
# presente dans la bibliotheque.
AC_CHECK_LIB(ncurses, initscr)
AC_CHECK_LIB(ncurses, mvprintw)
AC_CHECK_LIB(ncurses, refresh)
AC_CHECK_LIB(ncurses, clear)
AC_CHECK_LIB(ncurses, endwin)

# Que se passe-t-il lorsqu'une bibliotheque n'est pas presente ?
AC_CHECK_LIB(notpresent, omega)

# Maintenant, on vérifie l'existence des headers utilisés.
AC_CHECK_HEADERS([unistd.h])
AC_CHECK_HEADERS([stdlib.h])
AC_CHECK_HEADERS([errno.h])
AC_CHECK_HEADERS([error.h])
AC_CHECK_HEADERS([stdio.h])

AC_CONFIG_FILES([Makefile] [src/Makefile] [tests/Makefile] [doc/Makefile])
AC_OUTPUT