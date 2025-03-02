# -*-autoconf-*-

# ------------------------------------------------------------------------
# General settings
#
AC_INIT(,,rinderknecht@free.fr)
AC_PREREQ(2.53)
AC_CONFIG_SRCDIR

ac_version=$(autoconf -V | grep GNU | sed -n 's|.*) \(.*\)$|\1|p')

# ------------------------------------------------------------------------
# System tools
#
AC_PATH_PROG(GECHO,gecho,none)
if   test "$GECHO" = "none"
then AC_PATH_PROG(ECHO,echo,none)
else ECHO=$GECHO
fi

AC_PATH_PROG(GRM,grm,none)
if   test "$GRM" = "none"
then AC_PATH_PROG(RM,rm,none)
else RM=$GRM
fi

# ------------------------------------------------------------------------
# Application tools
#
AC_PATH_PROG(HTML2PS,html2ps,none)
AC_PATH_PROG(PS2PDF,ps2pdf,none)

AC_SUBST(ECHO)
AC_SUBST(RM)

AC_SUBST(HTML2PS)
AC_SUBST(PS2PDF)

AC_CONFIG_FILES([Makefile])
AC_OUTPUT
chmod a-w Makefile
