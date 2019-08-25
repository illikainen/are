/*
 * Copyright (c) 2019, Hans Jerry Illikainen <hji@dyntopia.com>
 *
 * SPDX-License-Identifier: GPL-3.0-or-later
 */

#ifndef UTILS_H
#define UTILS_H

#include <stdarg.h>

#include <emacs-module.h>

typedef emacs_value fun(emacs_env *env, ptrdiff_t nargs, emacs_value args[],
                        void *data);

void make_function(emacs_env *env, const char *name, fun *fun,
                   ptrdiff_t min_arity, ptrdiff_t max_arity,
                   const char *docstring, void *data);
emacs_value funcall(emacs_env *env, const char *fun, size_t nargs, ...);
emacs_value apply(emacs_env *env, const char *fun, size_t nargs,
                  emacs_value args[]);
emacs_value value_of(emacs_env *env, const char *name);
void non_local_exit_signal(emacs_env *env, const char *symbol, const char *fmt,
                           ...);
int msprintf(char **strp, const char *fmt, ...);
int vmsprintf(char **strp, const char *fmt, va_list ap);

#endif
