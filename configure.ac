AC_PREREQ([2.69])
AC_INIT([Additional Regexp Engines], [0.0.0])
AM_INIT_AUTOMAKE([-Wall -Werror foreign])
AC_PROG_CC
AC_CONFIG_HEADERS([include/config.h])
AC_CONFIG_FILES([Makefile src/Makefile tests/Makefile])
AC_CONFIG_MACRO_DIRS([m4])

m4_ifdef([AM_PROG_AR], [AM_PROG_AR])
AM_PROG_LIBTOOL
AC_ENABLE_SHARED
AC_DISABLE_STATIC
LT_INIT

# Base CFLAGS.
flg="-std=c11 -Wall -Wextra -Weverything -Wpedantic -pedantic"
flg="$flg -Wno-extra-semi -Wno-disabled-macro-expansion -Wno-padded"
flg="$flg -Werror=format-security -Werror=implicit-function-declaration"
flg="$flg -fstack-protector-all -D_FORTIFY_SOURCE=2 -fdiagnostics-color=always"

for f in $flg; do
    saved_cflags="$CFLAGS"
    CFLAGS="$f"
    AC_MSG_CHECKING([whether $f works])
    AC_COMPILE_IFELSE([AC_LANG_PROGRAM([])], [have_f="yes"], [have_f="no"])
    AC_MSG_RESULT([$have_f])
    CFLAGS="$saved_cflags"

    if test "$have_f" = "yes"; then
        CFLAGS="$CFLAGS $f"
    fi
done

# Check for integer overflow check macros.
AC_COMPILE_IFELSE(
    [AC_LANG_PROGRAM([], [[int x;] [__builtin_mul_overflow(0, 1, &x)]])],
    [have_builtin_checks="yes"]
)
if ! test "$have_builtin_checks" = "yes"; then
    AC_MSG_ERROR([Missing builtin overflow checks])
fi

# Check for uthash/utlist.
AC_CHECK_HEADERS([uthash.h utlist.h], [], [AC_MSG_ERROR([Missing uthash-dev])])

# Check for PCRE2.
AC_CHECK_HEADERS([pcre2.h], [enable_pcre2="yes"], [],
    [#define PCRE2_CODE_UNIT_WIDTH 8])
AM_CONDITIONAL([PCRE2], [test "$enable_pcre2" = "yes"])
if test "$enable_pcre2" = "yes"; then
    CFLAGS="$CFLAGS -DPCRE2_CODE_UNIT_WIDTH=8"
    LDFLAGS="$LDFLAGS -lpcre2-8"
fi

# Check for emacs.
AC_PATH_PROG([EMACS], [emacs], [no])
if test "$EMACS" = "no"; then
    AC_MSG_ERROR([Cannot locate emacs])
fi

# Check if Emacs was compiled with --with-modules.
if ! $EMACS -Q --batch --eval "(or (fboundp 'module-load) (kill-emacs 1))"; then
    AC_MSG_ERROR([Emacs was not compiled with --with-modules])
fi

# Add --enable-debug
AC_ARG_ENABLE(debug, AS_HELP_STRING([--enable-debug], [enable debug flags]))
if test "$enable_debug" = "yes"; then
    CFLAGS="$CFLAGS -Werror"
fi

# Add --enable-asan.
AC_ARG_ENABLE(asan, AS_HELP_STRING([--enable-asan], [enable ASan]))
if test "$enable_asan" = "yes"; then
    CFLAGS="$CFLAGS -fsanitize=address"
    CFLAGS="$CFLAGS -fno-omit-frame-pointer"
    CFLAGS="$CFLAGS -fno-optimize-sibling-calls"
fi
AM_CONDITIONAL([ASAN], [test "$enable_asan" = "yes"])

# Add --enable-ubsan.
#
# TODO: enable `implicit-conversion` and `nullability` on newer clang.
AC_ARG_ENABLE(ubsan, AS_HELP_STRING([--enable-ubsan], [enable UBSan]))
if test "$enable_ubsan" = "yes"; then
    CFLAGS="$CFLAGS -fsanitize=undefined"
    CFLAGS="$CFLAGS -fsanitize=float-divide-by-zero"
    CFLAGS="$CFLAGS -fsanitize=unsigned-integer-overflow"
    CFLAGS="$CFLAGS -fno-omit-frame-pointer"

fi
AM_CONDITIONAL([UBSAN], [test "$enable_ubsan" = "yes"])

cat <<EOF

    Build options
    =============
    Enable ASan: ${enable_asan:-no}
    Enable UBSan: ${enable_ubsan:-no}
    Enable PCRE2: ${enable_pcre2:-no}

EOF

AC_OUTPUT
