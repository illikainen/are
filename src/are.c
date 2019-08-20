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
    struct are_engine *engine;
    struct string *regexp = NULL;
    struct string *str = NULL;
    size_t start = 0;
    emacs_value rv = env->intern(env, "nil");

    (void)data;

    if (nargs < 2) {
        goto out;
    }

    regexp = extract_string(env, args[0]);
    if (regexp == NULL) {
        non_local_exit_signal(env, "Invalid regexp");
        goto out;
    }

    str = extract_string(env, args[1]);
    if (str == NULL) {
        non_local_exit_signal(env, "Invalid string");
        goto out;
    }

    if (nargs > 2 && is_integer(env, args[2])) {
        if (__builtin_mul_overflow(extract_integer(env, args[2]), 1, &start) ||
            start > str->len) {
            non_local_exit_signal(env, "Invalid start position");
            goto out;
        }
    }

    engine = are_find_engine(env);
    if (engine == NULL || engine->string_match == NULL) {
        non_local_exit_signal(env, "Invalid engine");
        goto out;
    }

    debug("regexp: '%s', str: '%s', start: %d", regexp->str, str->str, start);
    rv = engine->string_match(env, regexp, str, start);

out:
    free_string(regexp);
    free_string(str);
    return rv;
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
