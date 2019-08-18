/*
 * Copyright (c) 2019, Hans Jerry Illikainen <hji@dyntopia.com>
 *
 * SPDX-License-Identifier: GPL-3.0-or-later
 */
#ifndef MODULE_H
#define MODULE_H

#include "utils.h"

struct module_fun {
    fun *fun;
    const char *name;
    ptrdiff_t min_arity;
    ptrdiff_t max_arity;
    const char *docstring;
    void *data;
    struct module_fun *next;
};

#define module_register_fun(mfun, mname, mmin_arity, mmax_arity, mdocstring)   \
    static void __attribute__((constructor)) module_register_##mfun(void)      \
    {                                                                          \
        static struct module_fun m = {                                         \
            .fun = mfun,                                                       \
            .name = mname,                                                     \
            .min_arity = mmin_arity,                                           \
            .max_arity = mmax_arity,                                           \
            .docstring = mdocstring,                                           \
            .data = NULL,                                                      \
        };                                                                     \
        module_add_fun(&m);                                                    \
    }

extern int plugin_is_GPL_compatible;

void module_add_fun(struct module_fun *m);

#endif
