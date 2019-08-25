/*
 * Copyright (c) 2019, Hans Jerry Illikainen <hji@dyntopia.com>
 *
 * SPDX-License-Identifier: GPL-3.0-or-later
 */

#include <stdlib.h>
#include <string.h>

#include <emacs-module.h>

#include "are.h"
#include "log.h"
#include "module.h"

static struct are_engine *are_engines_head = NULL;

/*
 * Find an engine based on `value`.
 */
static struct are_engine *are_find_engine(emacs_env *env, emacs_value value)
{
    struct are_engine *engine = NULL;
    struct str *name = NULL;

    value = funcall(env, "symbol-name", 1, value);
    if (!env->is_not_nil(env, value)) {
        return NULL;
    }

    name = str_extract(env, value);
    if (name == NULL) {
        return NULL;
    }

    LL_FOREACH (are_engines_head, engine) {
        if (engine->name && !strcmp(engine->name, name->str)) {
            break;
        }
    }

    str_free(name);
    return engine;
}

/*
 * Allocate and initialize a regexp structure for `engine` and `ptr`.
 */
static struct are_regexp *are_alloc(struct are_engine *engine, void *ptr)
{
    struct are_regexp *r;

    r = malloc(sizeof(*r));
    if (r) {
        r->engine = engine;
        r->ptr = ptr;
    }
    return r;
}

/*
 * Free a regexp struct.
 *
 * The engine-specific free() function (if any) is invoked to deallocate `ptr`.
 */
static void are_free(void *ptr)
{
    struct are_regexp *r = ptr;

    if (r) {
        if (r->engine && r->engine->free && r->ptr) {
            r->engine->free(r->ptr);
        }
        free(r);
    }
}

/*
 * Compile a regular expression.
 */
static emacs_value are_compile(emacs_env *env, ptrdiff_t nargs,
                               emacs_value args[], void *data)
{
    void *ptr;
    struct are_engine *engine;
    struct are_regexp *re;
    struct str *regexp = NULL;
    emacs_value options;
    emacs_value rv = env->intern(env, "nil");

    (void)data;

    if (nargs != 3) {
        non_local_exit_signal(env, "error", "Invalid number of arguments");
    }

    regexp = str_extract(env, args[0]);
    if (regexp == NULL) {
        non_local_exit_signal(env, "error", "Invalid regexp");
        goto out;
    }

    options = args[1];

    engine = are_find_engine(env, args[2]);
    if (engine == NULL || engine->compile == NULL) {
        non_local_exit_signal(env, "error", "Invalid engine");
        goto out;
    }

    ptr = engine->compile(env, regexp, options);
    if (ptr) {
        re = are_alloc(engine, ptr);
        if (re == NULL) {
            non_local_exit_signal(env, "error", "OOM");
            goto out;
        }
        rv = ptr_make(env, re, are_free);
    }

out:
    str_free(regexp);
    return rv;
}
module_register_fun(are_compile, "are--compile", 3, 3, "Compile a regexp.");

static emacs_value are_match(emacs_env *env, ptrdiff_t nargs,
                             emacs_value args[], void *data)
{
    emacs_value options;
    struct are_regexp *re;
    struct str *str = NULL;
    emacs_value rv = env->intern(env, "nil");

    (void)data;

    if (nargs != 3) {
        non_local_exit_signal(env, "error", "Invalid number of arguments");
        goto out;
    }

    re = ptr_extract(env, args[0]);
    if (re == NULL) {
        non_local_exit_signal(env, "error", "Invalid compiled regexp");
        goto out;
    }

    if (re->engine == NULL || re->engine->match == NULL) {
        non_local_exit_signal(env, "error", "Invalid engine");
        goto out;
    }

    str = str_extract(env, args[1]);
    if (str == NULL) {
        non_local_exit_signal(env, "error", "Invalid string");
        goto out;
    }

    options = args[2];

    rv = re->engine->match(env, re->ptr, str, options);

out:
    str_free(str);
    return rv;
}
module_register_fun(are_match, "are--match", 3, 3, "Match a string.");

/*
 * Retrieve the engine for a compiled expression.
 */
static emacs_value are_engine(emacs_env *env, ptrdiff_t nargs,
                              emacs_value args[], void *data)
{
    struct are_regexp *re;
    emacs_value nil = env->intern(env, "nil");

    (void)data;

    if (nargs != 1) {
        return nil;
    }

    re = ptr_extract(env, args[0]);
    if (re == NULL || re->engine == NULL || re->engine->name == NULL) {
        non_local_exit_signal(env, "error", "Invalid compiled regexp");
        return nil;
    }

    return env->intern(env, re->engine->name);
}
module_register_fun(are_engine, "are--engine", 1, 1, "Retrieve engine.");

/*
 * Add an engine.
 */
void are_add_engine(struct are_engine *engine)
{
    LL_APPEND(are_engines_head, engine);
}
