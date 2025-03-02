# Autoconfiguration for Objective Caml projects
# (c) 2003 Christian Rinderknecht

# Process this file with `autoconf' to produce a configure script.

# ------------------------------------------------------------------
# General settings
#
AC_INIT(Fun Calc,1.0,Christian.Rinderknecht@devinci.fr)
AC_PREREQ(2.53)
AC_CONFIG_SRCDIR(ast.ml)

ac_version=$(autoconf -V | grep GNU) 

echo "Autoconfiguration of $PACKAGE_STRING"
echo "(c) 2003 Christian Rinderknecht"
echo
echo "This is $ac_version"

# ------------------------------------------------------------------
# Checking for files named core, core.* and *.core. 
#
# These files are deleted by the configure script (feature of
# autoconf), but core.* may be not a system core file. Hence this
# section backs up such files and prompts the user to check them.
#
core_files=$(ls core *.core core.* 2> /dev/null)
maybenot_core=$(ls core.* 2> /dev/null)

if test -n "$core_files";
then
  echo -------------------------------------------------------------
  if test $(echo $core_files | wc -w) = 1;
  then
    echo "File $core_files is going to be deleted!"
  else
    echo "Files $core_files are going to be deleted!"
  fi
  if test -n "$maybenot_core";
  then
    echo n "  => Backup of $maybenot_core in \
            BACKUP-$maybenot_core..."
    cp $maybenot_core BACKUP-$maybenot_core
    echo " done."
    echo "  => Rename or delete it and rerun autoconf \
          and configure."
  fi
fi

echo -----------------------------------------------------------
echo Compilers

# Checking for ocaml bytecode compiler
#
AC_PATH_PROG(OCAMLC,ocamlc,none)
AC_PATH_PROG(OCAMLC_OPT,ocamlc.opt,none)

if test "$OCAMLC_OPT" = "none";
then 
  if test "$OCAMLC" != "none";
  then 
    OCAMLC_VER=`$OCAMLC -version`
    echo "  => ocamlc" v$OCAMLC_VER \
         "is the default compiler to byte-code"
  fi
else
  OCAMLC=$OCAMLC_OPT
  OCAMLC_VER=`$OCAMLC -version`
  echo "  => ocamlc.opt" v$OCAMLC_VER \
       "is the default compiler to byte-code"
fi

# Checking for ocaml native-code compiler
#
AC_PATH_PROG(OCAMLOPT,ocamlopt,none)
AC_PATH_PROG(OCAMLOPT_OPT,ocamlopt.opt,none)

if test "$OCAMLOPT_OPT" = "none"; 
then
  if test "$OCAMLOPT" != "none";
  then
    OCAMLOPT_VER=`$OCAMLOPT -version`
    echo "  => ocamlopt" v$OCAMLOPT_VER "is the default compiler" \
         "to native-code"
  fi
else
  OCAMLOPT=$OCAMLOPT_OPT
  OCAMLOPT_VER=`$OCAMLOPT -version`
  echo "  => ocamlopt.opt" v$OCAMLOPT_VER \
       "is the default compiler to native-code"
fi

# Checking consistency between compilers version numbers
#
if test "$OCAMLC_VER" != "$OCAMLOPT";
then
  echo "  ** Warning: Version numbers of native and byte-code compilers differ."
fi

echo -----------------------------------------------------------------
echo Preprocessors

AC_PATH_PROG(CAMLP4,camlp4,none)

if test "$CAMLP4" != "none";
then
  CAMLP4_VER=`$CAMLP4 -v 2>&1 | sed -n 's|.*version* *\(.*\)$|\1|p' `
  echo "  => camlp4" v$CAMLP4_VER "is the default preprocessor"
  if test "$OCAMLC_VER" != "$CAMLP4_VER";
  then
    echo "  ** Warning: Version numbers of ocamlc and camlp4 differ."
  fi
fi

AC_PATH_PROG(CAMLP4O,camlp4o,none)

echo -----------------------------------------------------------
echo Dependences finders

AC_PATH_PROG(OCAMLDEP,ocamldep,none)
AC_PATH_PROG(OCAMLDEP_OPT,ocamldep.opt,none)

if test "$OCAMLDEP_OPT" = "none";
then
  if test "$OCAMLDEP" != "none";
  then
    echo "  => ocamldep is the default dependences finder"
  fi
else
  OCAMLDEP=$OCAMLDEP_OPT
  echo "  => ocamldep.opt is the default dependences finder"
fi

echo -----------------------------------------------------------
echo Lexer generators

AC_PATH_PROG(OCAMLLEX,ocamllex,none)
AC_PATH_PROG(OCAMLLEX_OPT,ocamllex.opt,none)

if test "$OCAMLLEX_OPT" = "none";
then 
  if test "$OCAMLLEX" != "none";
  then
    echo "  => ocamllex is the default lexer generator"
  fi
else
  OCAMLLEX=$OCAMLLEX_OPT
  echo "  => ocamllex.opt is the default lexer generator"
fi

echo -----------------------------------------------------------
echo Parser generator

AC_PATH_PROG(OCAMLYACC,ocamlyacc,none)

echo -----------------------------------------------------------
echo Directories and paths

if test "$OCAMLC" != "none";
then
  OCAMLLIB=`$OCAMLC -where`
  echo "  => The standard library path is" $OCAMLLIB
fi
echo "  => The source directory is" \'$srcdir\'

# ------------------------------------------------------------------
# Explicit variable substitutions
#
AC_SUBST(OCAMLC_VER)
AC_SUBST(OCAMLC)
AC_SUBST(OCAMLOPT_VER)
AC_SUBST(OCAMLOPT)
AC_SUBST(OCAMLLIB)
AC_SUBST(CAMLP4)
AC_SUBST(OCAMLDEP)
AC_SUBST(OCAMLLEX)
AC_SUBST(srcdir)
AC_SUBST(ac_version)

echo -----------------------------------------------------------
AC_CONFIG_FILES(Makefile)
AC_OUTPUT
chmod a-w Makefile
