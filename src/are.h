/*
 * Copyright (c) 2019, Hans Jerry Illikainen <hji@dyntopia.com>
 *
 * SPDX-License-Identifier: GPL-3.0-or-later
 */

#ifndef ARE_H
#define ARE_H

#include <stdio.h>

#include <uthash.h>

#include "types.h"

struct are_engine {
    char name[16];
    UT_hash_handle hh;
};

#define are_register_engine(engine)                                            \
    static void __attribute__((constructor)) are_register_##engine(void)       \
    {                                                                          \
        /*                                                                     \
         * This is kind of ugly, but uthash provides two ways of adding string \
         * keys: HASH_ADD_KEYPTR and HASH_ADD_STR.                             \
         *                                                                     \
         * HASH_ADD_KEYPTR casts away the const qualifier.  While nothing      \
         * seems to modify the memory it points to, it still seems somewhat    \
         * fragile to rely on it never being modified.                         \
         */                                                                    \
        snprintf(engine.name, sizeof(engine.name), "%s", #engine);             \
        are_add_engine(&engine);                                               \
    }

void are_add_engine(struct are_engine *engine);

#endif
