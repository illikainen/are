/*
 * Copyright (c) 2019, Hans Jerry Illikainen <hji@dyntopia.com>
 *
 * SPDX-License-Identifier: GPL-3.0-or-later
 */

#ifndef TYPES_H
#define TYPES_H

#include <emacs-module.h>

struct string {
    char *str;
    size_t len;
    size_t size;
};

struct string *extract_string(emacs_env *env, emacs_value value);
void free_string(struct string *s);
emacs_value make_string(emacs_env *env, const char *str);
bool is_string(emacs_env *env, emacs_value value);
intmax_t extract_integer(emacs_env *env, emacs_value value);
emacs_value make_integer(emacs_env *env, intmax_t value);
bool is_integer(emacs_env *env, emacs_value value);

#endif
