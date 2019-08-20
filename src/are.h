/*
 * Copyright (c) 2019, Hans Jerry Illikainen <hji@dyntopia.com>
 *
 * SPDX-License-Identifier: GPL-3.0-or-later
 */

#ifndef ARE_H
#define ARE_H

#include <uthash.h>

#include "types.h"

struct are_engine {
    const char *name;
    emacs_value (*string_match)(emacs_env *env, struct string *regexp,
                                struct string *str, size_t start);
    UT_hash_handle hh;
};

#define are_register_engine(engine)                                            \
    static void __attribute__((constructor)) are_register_##engine(void)       \
    {                                                                          \
        engine.name = #engine;                                                 \
        are_add_engine(&engine);                                               \
    }

void are_add_engine(struct are_engine *engine);

#endif
