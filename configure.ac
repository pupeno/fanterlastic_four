AC_INIT(Example of how to use Serlvers, 0.1.0, pupeno@pupeno.com, fanterlasticfour)
AC_COPYRIGHT(Copyright (C) 2006 Jose Pablo Pupeno Fernandez)

dnl Require autoconf version >=2.59c. first one with erlang macros
AC_PREREQ(2.59c)
dnl Require automake version >=1.9.5.
AM_INIT_AUTOMAKE(1.9.5)

dnl We need the erlang compiler to compile the project.
AC_ERLANG_NEED_ERLC

dnl Define some variables to know where to install everything.
AC_ERLANG_SUBST_ROOT_DIR
AC_ERLANG_SUBST_INSTALL_LIB_SUBDIR(AC_PACKAGE_TARNAME, AC_PACKAGE_VERSION)

dnl Run_erl is needed for the wrapper script.
AC_ARG_VAR([RUN_ERL], [Erlang/OTP interpreter runner [autodetected]])

if test -n "$RUN_ERL"; then
    AC_MSG_CHECKING([for run_erl])
    AC_MSG_RESULT([$RUN_ERL])
else
    AC_PATH_PROG(RUN_ERL, run_erl, [not found], [$PATH:$ERLANG_ROOT_DIR/bin])
fi

if test "$RUN_ERL" = "not found"; then
    AC_MSG_ERROR([Erlang/OTP interpreter runner (run_erl) not found but required])
fi

dnl Erl_call is needed as well.
AC_ERLANG_CHECK_LIB(erl_interface, [], [AC_MSG_ERROR([erl_interface library not found but required])])

AC_ARG_VAR([ERL_CALL], [Erlang/OTP distributed node caller [autodetected]])

if test -n "$ERL_CALL"; then
    AC_MSG_CHECKING([for erl_call])
    AC_MSG_RESULT([$ERL_CALL])
else
    AC_PATH_PROG(ERL_CALL, erl_call, [not found], [$PATH:$ERLANG_LIB_DIR_erl_interface/bin])
fi

if test "$ERL_CALL" = "not found"; then
    AC_MSG_ERROR([Erlang/OTP distributed node caller (erl_call) not found but required])
fi

dnl That's it.
AC_OUTPUT(Makefile src/Makefile script/Makefile)
