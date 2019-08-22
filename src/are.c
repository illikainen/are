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

static emacs_value are_re_search_forward(emacs_env *env, ptrdiff_t nargs,
                                         emacs_value args[], void *data)
{
    struct are_engine *engine;
    struct str *regexp = NULL;
    struct str *str = NULL;
    enum are_noerror noerror = ARE_NOERROR_NIL;
    intmax_t bound;
    intmax_t count = 1;
    intmax_t point = intmax_extract(env, funcall(env, "point", 0));
    intmax_t point_min = intmax_extract(env, funcall(env, "point-min", 0));
    emacs_value mark;
    emacs_value rv = env->intern(env, "nil");

    (void)data;

    if (nargs < 1) {
        goto out;
    }

    /*
     * This isn't great -- the entire buffer is extracted for the search :(
     */
    str = str_extract(env, funcall(env, "buffer-string", 0));
    if (str == NULL) {
        non_local_exit_signal(env, "Invalid buffer");
        goto out;
    }

    /*
     * 1st arg: `regexp`.
     */
    regexp = str_extract(env, args[0]);
    if (regexp == NULL) {
        non_local_exit_signal(env, "Invalid regexp");
        goto out;
    }

    /*
     * 4th arg: `count` (needed for `bound`).
     */
    if (nargs > 3 && intmax_p(env, args[3])) {
        count = intmax_extract(env, args[3]);
    }

    /*
     * 2nd: `bound.
     */
    if (nargs > 1 && intmax_p(env, args[1])) {
        bound = intmax_extract(env, args[1]);
    } else {
        if (count > 0) {
            if (__builtin_mul_overflow(str->size, 1, &bound)) {
                non_local_exit_signal(env, "Invalid bound");
                goto out;
            }
        } else {
            bound = point_min;
        }
    }

    /*
     * 3rd arg: `noerror`.
     */
    if (nargs > 2) {
        if (env->eq(env, args[2], env->intern(env, "nil"))) {
            noerror = ARE_NOERROR_NIL;
        } else if (env->eq(env, args[2], env->intern(env, "t"))) {
            noerror = ARE_NOERROR_T;
        } else {
            noerror = ARE_NOERROR_OTHER;
        }
    }

    /*
     * If `count` is >0, `re-search-forward` searches forward and `bound`
     * has to be >`point`.
     *
     * If `count` is <0, `re-search-forward` searches backward and `bound`
     * has to be <`point`.
     *
     * If `count` is 0, the bound is checked and a signal is raised if it's
     * >`point`.  However, no search is performed; instead the start and end
     * markers of the match data are simply set to `point` and `point` is
     * returned.  I'm not sure why, but that's how `re-search-forward` seems
     * to handle a count of 0.
     */
    if ((count > 0 && bound < point) || (count <= 0 && bound > point)) {
        non_local_exit_signal(env, "Invalid bound");
        goto out;
    }

    if (count == 0) {
        mark = funcall(env, "make-marker", 0);
        funcall(env, "set-marker", 2, mark, intmax_make(env, point));
        funcall(env, "set-match-data", 1, funcall(env, "list", 2, mark, mark));
        rv = intmax_make(env, point);
        goto out;
    }

    /*
     * At this point, `bound` may be negative if `count` is also negative.
     * This is permissible by `re-search-forward` with a negative `count`,
     * so long as `bound` <= `point`.  Naturally, we can't search before the
     * first position in the buffer so `bound` is adjusted here.
     */
    if (bound < point_min) {
        bound = point_min;
    }

    /*
     * And finally dispatch!
     */
    engine = are_find_engine(env);
    if (engine == NULL || engine->re_search_forward == NULL) {
        non_local_exit_signal(env, "Invalid engine");
        goto out;
    }

    debug("regexp: '%s', str: '%s', bound: %d, noerror: %d, count: %d",
          regexp->str, str->str, bound, noerror, count);
    rv = engine->re_search_forward(env, regexp, str, bound, noerror, count);

out:
    str_free(regexp);
    str_free(str);
    return rv;
}
module_register_fun(are_re_search_forward, "are-re-search-forward", 1, 4,
                    "ARE version of `re-search-forward'.");

static emacs_value are_string_match(emacs_env *env, ptrdiff_t nargs,
                                    emacs_value args[], void *data)
{
    struct are_engine *engine;
    struct str *regexp = NULL;
    struct str *str = NULL;
    size_t start = 0;
    emacs_value rv = env->intern(env, "nil");

    (void)data;

    if (nargs < 2) {
        goto out;
    }

    regexp = str_extract(env, args[0]);
    if (regexp == NULL) {
        non_local_exit_signal(env, "Invalid regexp");
        goto out;
    }

    str = str_extract(env, args[1]);
    if (str == NULL) {
        non_local_exit_signal(env, "Invalid string");
        goto out;
    }

    if (nargs > 2 && intmax_p(env, args[2])) {
        if (__builtin_mul_overflow(intmax_extract(env, args[2]), 1, &start) ||
            start > str_length(str)) {
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
    str_free(regexp);
    str_free(str);
    return rv;
}
module_register_fun(are_string_match, "are-string-match", 1, 4,
                    "ARE version of `re-search-forward'.");

/*
 * Add an engine.
 */
void are_add_engine(struct are_engine *engine)
{
    HASH_ADD_STR(engines, name, engine);
}
