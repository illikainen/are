/*
 * Copyright (c) 2019, Hans Jerry Illikainen <hji@dyntopia.com>
 *
 * SPDX-License-Identifier: GPL-3.0-or-later
 */

#include <stdio.h>

#include <emacs-module.h>

#include "are.h"
#include "module.h"
#include "types.h"

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
