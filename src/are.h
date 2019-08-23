/*
 * Copyright (c) 2019, Hans Jerry Illikainen <hji@dyntopia.com>
 *
 * SPDX-License-Identifier: GPL-3.0-or-later
 */

#ifndef ARE_H
#define ARE_H

#include <stdio.h>

#include <utlist.h>

#include "types.h"

struct are_engine {
    const char *name;
    void *(*compile)(emacs_env *env, struct str *regexp, emacs_value options);
    void (*free)(void *ptr);
    emacs_value (*match)(emacs_env *env, void *ptr, struct str *str,
                         emacs_value options);
    struct are_engine *next;
};

struct are_regexp {
    const struct are_engine *engine;
    void *ptr;
};

#define are_register_engine(engine)                                            \
    static void __attribute__((constructor)) are_register_##engine(void)       \
    {                                                                          \
        engine.name = #engine;                                                 \
        are_add_engine(&engine);                                               \
    }

void are_add_engine(struct are_engine *engine);

#endif
