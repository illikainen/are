/*
 * Copyright (c) 2019, Hans Jerry Illikainen <hji@dyntopia.com>
 *
 * SPDX-License-Identifier: GPL-3.0-or-later
 */

#include <stdio.h>

#include <emacs-module.h>

#include "are.h"
#include "log.h"
#include "module.h"

static struct are_engine *engines = NULL;

static struct are_engine *are_find_engine(emacs_env *env)
{
    struct are_engine *engine = NULL;
    struct str *name = NULL;
    emacs_value value;

    value = value_of(env, "are-engine");
    if (!env->is_not_nil(env, value)) {
        debug("are-engine is not set");
        return NULL;
    }

    value = funcall(env, "symbol-name", 1, value);
    if (!env->is_not_nil(env, value)) {
        return NULL;
    }

    name = str_extract(env, value);
    if (name == NULL) {
        return NULL;
    }

    HASH_FIND_STR(engines, name->str, engine);
    str_free(name);

    return engine;
}

/*
 * Add an engine.
 */
void are_add_engine(struct are_engine *engine)
{
    HASH_ADD_STR(engines, name, engine);
}
