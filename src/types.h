/*
 * Copyright (c) 2019, Hans Jerry Illikainen <hji@dyntopia.com>
 *
 * SPDX-License-Identifier: GPL-3.0-or-later
 */

#ifndef TYPES_H
#define TYPES_H

#include <emacs-module.h>

#define mul_overflow(a, b, res) __builtin_mul_overflow((a), (b), (res))
#define add_overflow(a, b, res) __builtin_add_overflow((a), (b), (res))

struct str {
    char *str;
    size_t size;
};

struct str *str_extract(emacs_env *env, emacs_value value);
size_t str_length(const struct str *s);
size_t str_position(const struct str *s, size_t idx);
size_t str_position_prev(const struct str *s, size_t cur);
struct str *str_alloc(ptrdiff_t size);
void str_free(struct str *s);
emacs_value str_make(emacs_env *env, const char *str);
bool str_p(emacs_env *env, emacs_value value);

intmax_t intmax_extract(emacs_env *env, emacs_value value);
emacs_value intmax_make(emacs_env *env, intmax_t value);
bool intmax_p(emacs_env *env, emacs_value value);

emacs_value size_make(emacs_env *env, size_t value);

#endif
