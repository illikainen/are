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
    struct string *name = NULL;
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

    name = extract_string(env, value);
    if (name == NULL) {
        return NULL;
    }

    HASH_FIND_STR(engines, name->str, engine);
    free_string(name);

    return engine;
}

static emacs_value are_string_match(emacs_env *env, ptrdiff_t nargs,
                                    emacs_value args[], void *data)
{
    struct string *regexp;

    (void)nargs;
    (void)data;

    regexp = extract_string(env, args[0]);
    printf("regexp: %s\n", regexp->str);
    free_string(regexp);

    return env->intern(env, "string-match");
}
module_register_fun(are_string_match, "are-string-match", 1, 4,
                    "ARE version of `re-search-forward'.");

/*
 * Add an engine.
 */
void are_add_engine(struct are_engine *engine)
{
    HASH_ADD_KEYPTR(hh, engines, engine->name, strlen(engine->name), engine);
}
