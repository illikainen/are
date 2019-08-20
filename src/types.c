/*
 * Copyright (c) 2019, Hans Jerry Illikainen <hji@dyntopia.com>
 *
 * SPDX-License-Identifier: GPL-3.0-or-later
 */

#include <stdlib.h>
#include <string.h>

#include "types.h"
#include "utils.h"

/**
 * Extract a string from `value`.
 *
 * The string may contain NUL-bytes, so the `len` field in the struct should
 * be used instead of strlen().  The struct must be freed after use.
 */
struct string *extract_string(emacs_env *env, emacs_value value)
{
    struct string *s;
    intmax_t len;
    ptrdiff_t size;

    len = extract_integer(env, funcall(env, "length", 1, value));
    if (len < 0) {
        non_local_exit_signal(env, "invalid string");
        return NULL;
    }

    if (__builtin_add_overflow(len, 1, &size)) {
        non_local_exit_signal(env, "overflow detected");
        return NULL;
    }

    s = calloc(1, sizeof(*s));
    if (s == NULL) {
        non_local_exit_signal(env, "oom");
        return NULL;
    }

    if (__builtin_mul_overflow(len, 1, &s->len) ||
        __builtin_mul_overflow(size, 1, &s->size)) {
        free(s);
        non_local_exit_signal(env, "overflow detected");
        return NULL;
    }

    s->str = malloc(s->size);
    if (s->str == NULL) {
        non_local_exit_signal(env, "oom");
        free_string(s);
        return NULL;
    }

    if (!env->copy_string_contents(env, value, s->str, &size)) {
        non_local_exit_signal(env, "copy_string_contents() failed");
        free_string(s);
        return NULL;
    }

    return s;
}

/**
 * Free memory for string `s`.
 */
void free_string(struct string *s)
{
    if (s) {
        free(s->str);
        free(s);
    }
}

/**
 * Convert a NUL-terminated string to an emacs value.
 */
emacs_value make_string(emacs_env *env, const char *str)
{
    ptrdiff_t len;

    if (str && __builtin_mul_overflow(strlen(str), 1, &len)) {
        return env->make_string(env, str, len);
    }
    return env->intern(env, "nil");
}

/**
 * Check if `value` is a string.
 */
bool is_string(emacs_env *env, emacs_value value)
{
    emacs_value string, type;

    string = env->intern(env, "string");
    type = env->type_of(env, value);
    return env->eq(env, type, string);
}

/**
 * Extract an integer from `value`.
 */
intmax_t extract_integer(emacs_env *env, emacs_value value)
{
    return env->extract_integer(env, value);
}

/**
 * Create an emacs value from integer `value`.
 */
emacs_value make_integer(emacs_env *env, intmax_t value)
{
    return env->make_integer(env, value);
}

/**
 * Check if `value` is an integer.
 */
bool is_integer(emacs_env *env, emacs_value value)
{
    emacs_value integer, type;

    integer = env->intern(env, "integer");
    type = env->type_of(env, value);
    return env->eq(env, type, integer);
}
