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
 * Return true if `c` is a continuation of a UTF-8 multibyte sequence.
 */
static bool is_cont(char c)
{
    return (c & 0xc0) == 0x80;
}

/**
 * Extract a string from `value`.
 *
 * The string may contain multibyte and NUL characters, so `str_length()`
 * should be used instead of `strlen()`.  The struct must be freed with
 * `str_free()` after use.
 */
struct str *str_extract(emacs_env *env, emacs_value value)
{
    struct str *s;
    ptrdiff_t size;

    if (!str_p(env, value)) {
        return NULL;
    }

    /*
     * Determine buffer size.  See:
     * - E.8.3 Conversion Between Lisp and Module Values
     */
    if (!env->copy_string_contents(env, value, NULL, &size)) {
        return NULL;
    }

    s = str_alloc(size);
    if (s == NULL) {
        return NULL;
    }

    if (!env->copy_string_contents(env, value, s->str, &size)) {
        str_free(s);
        return NULL;
    }

    return s;
}

/**
 * Allocate a string large enough for `size`.
 */
struct str *str_alloc(ptrdiff_t size)
{
    struct str *s;
    size_t usize;

    if (mul_overflow(size, 1, &usize)) {
        return NULL;
    }

    s = malloc(sizeof(*s));
    if (s == NULL) {
        return NULL;
    }

    s->str = malloc(usize);
    if (s->str == NULL) {
        free(s);
        return NULL;
    }
    s->size = usize;
    return s;
}

/**
 * Free memory for string `s`.
 */
void str_free(struct str *s)
{
    if (s) {
        free(s->str);
        free(s);
    }
}

/**
 * Retrieve the length for string `s`.
 */
size_t str_length(const struct str *s)
{
    size_t i;
    size_t len = 0;

    if (s && s->str && s->size) {
        for (i = 0; i < s->size - 1; i++) {
            if (!is_cont(s->str[i])) {
                len++;
            }
        }
    }
    return len;
}

/**
 * Convert a NUL-terminated string to an emacs value.
 */
emacs_value str_make(emacs_env *env, const char *str)
{
    ptrdiff_t len;

    if (str && !__builtin_mul_overflow(strlen(str), 1, &len)) {
        return env->make_string(env, str, len);
    }
    return env->intern(env, "nil");
}

/**
 * Check if `value` is a string.
 */
bool str_p(emacs_env *env, emacs_value value)
{
    emacs_value string, type;

    string = env->intern(env, "string");
    type = env->type_of(env, value);
    return env->eq(env, type, string);
}

/**
 * Extract an integer from `value`.
 */
intmax_t intmax_extract(emacs_env *env, emacs_value value)
{
    return env->extract_integer(env, value);
}

/**
 * Create an emacs value from integer `value`.
 */
emacs_value intmax_make(emacs_env *env, intmax_t value)
{
    return env->make_integer(env, value);
}

/**
 * Check if `value` is an integer.
 */
bool intmax_p(emacs_env *env, emacs_value value)
{
    emacs_value integer, type;

    integer = env->intern(env, "integer");
    type = env->type_of(env, value);
    return env->eq(env, type, integer);
}

/*
 * Create an emacs value from size_t `value`.
 */
emacs_value size_make(emacs_env *env, size_t value)
{
    intmax_t i;

    if (__builtin_mul_overflow(value, 1, &i)) {
        return env->intern(env, "nil");
    }
    return intmax_make(env, i);
}
