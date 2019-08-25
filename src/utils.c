/*
 * Copyright (c) 2019, Hans Jerry Illikainen <hji@dyntopia.com>
 *
 * SPDX-License-Identifier: GPL-3.0-or-later
 */

#include <stdio.h>
#include <stdlib.h>

#include "types.h"
#include "utils.h"

/*
 * Apply `fun` with `nargs` of `args`.
 */
emacs_value apply(emacs_env *env, const char *fun, size_t nargs,
                  emacs_value args[])
{
    emacs_value funsym;
    ptrdiff_t pnargs;

    if (!mul_overflow(nargs, 1, &pnargs)) {
        funsym = env->intern(env, fun);
        if (env->is_not_nil(env, funsym)) {
            return env->funcall(env, funsym, pnargs, args);
        }
    }

    return env->intern(env, "nil");
}

/*
 * Invoke `fun` with `nargs` of emacs_value arguments.
 */
emacs_value funcall(emacs_env *env, const char *fun, size_t nargs, ...)
{
    size_t i;
    size_t size;
    va_list ap;
    emacs_value *args = NULL;
    emacs_value rv = env->intern(env, "nil");

    if (nargs > 0) {
        if (mul_overflow(nargs, sizeof(*args), &size)) {
            return rv;
        }

        args = malloc(size);
        if (args == NULL) {
            return rv;
        }

        va_start(ap, nargs);
        for (i = 0; i < nargs; i++) {
            args[i] = va_arg(ap, emacs_value);
        }
        va_end(ap);
    }

    rv = apply(env, fun, nargs, args);

    free(args);
    return rv;
}

/*
 * Retrieve the value for the symbol with `name` if it's bound.
 */
emacs_value value_of(emacs_env *env, const char *name)
{
    emacs_value symbol;

    symbol = env->intern(env, name);
    if (env->is_not_nil(env, funcall(env, "boundp", 1, symbol))) {
        return funcall(env, "symbol-value", 1, symbol);
    }
    return env->intern(env, "nil");
}

/*
 * Expose a function to elisp.
 */
void make_function(emacs_env *env, const char *name, fun *fun,
                   ptrdiff_t min_arity, ptrdiff_t max_arity,
                   const char *docstring, void *data)
{
    emacs_value def;

    def = env->make_function(env, min_arity, max_arity, fun, docstring, data);
    funcall(env, "defalias", 3, env->intern(env, name), def,
            str_make(env, docstring));
}

/*
 * Signal an error.
 */
void non_local_exit_signal(emacs_env *env, const char *symbol, const char *fmt,
                           ...)
{
    emacs_value list;
    emacs_value error;
    emacs_value msg;
    va_list ap;
    char *str = NULL;

    va_start(ap, fmt);
    if (vmsprintf(&str, fmt, ap) > 0) {
        msg = str_make(env, str);
    } else {
        msg = str_make(env, "unknown error");
    }
    va_end(ap);

    error = env->intern(env, symbol);
    list = funcall(env, "list", 1, msg);
    env->non_local_exit_signal(env, error, list);

    free(str);
}

/*
 * Allocate a buffer and populate it with a string according to `fmt`.
 *
 * Similar to the non-standard function `asprintf()`.
 */
int __attribute__((format(printf, 2, 3)))
msprintf(char **strp, const char *fmt, ...)
{
    va_list ap;
    int rv;

    va_start(ap, fmt);
    rv = vmsprintf(strp, fmt, ap);
    va_end(ap);
    return rv;
}

/*
 * Allocate a buffer and populate it with a string according to `fmt`.
 *
 * Similar to the non-standard function `vasprintf()`.
 */
int __attribute__((format(printf, 2, 0)))
vmsprintf(char **strp, const char *fmt, va_list ap)
{
    va_list tmp;
    size_t size;
    int len;
    int rv = -1;

    *strp = NULL;

    /*
     * See:
     * - ISO/IEC 9899:201x 7.21.6.5
     * - ISO/IEC 9899:201x 7.21.6.12
     */
    va_copy(tmp, ap);
    len = vsnprintf(NULL, 0, fmt, tmp);
    va_end(tmp);

    if (len >= 0 && !add_overflow(len, 1, &size)) {
        *strp = malloc(size);
        if (*strp) {
            rv = vsnprintf(*strp, size, fmt, ap);
            if (rv < 0) {
                free(*strp);
                *strp = NULL;
            }
        }
    }

    return rv;
}
