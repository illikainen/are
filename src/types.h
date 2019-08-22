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

struct string *str_extract(emacs_env *env, emacs_value value);
void str_free(struct string *s);
emacs_value str_make(emacs_env *env, const char *str);
bool str_p(emacs_env *env, emacs_value value);

intmax_t intmax_extract(emacs_env *env, emacs_value value);
emacs_value intmax_make(emacs_env *env, intmax_t value);
bool intmax_p(emacs_env *env, emacs_value value);

emacs_value size_make(emacs_env *env, size_t value);

#endif
