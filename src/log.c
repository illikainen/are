/*
 * Copyright (c) 2019, Hans Jerry Illikainen <hji@dyntopia.com>
 *
 * SPDX-License-Identifier: GPL-3.0-or-later
 */

#include <stdarg.h>
#include <stdlib.h>

#include "log.h"
#include "types.h"
#include "utils.h"

/*
 * Print a debug message.
 */
void log_debug(emacs_env *env, const char *func, const char *fmt, ...)
{
    va_list ap;
    char *ustr = NULL;
    char *str = NULL;

    if (!env->is_not_nil(env, value_of(env, "are-debug"))) {
        return;
    }

    va_start(ap, fmt);
    if (vmsprintf(&ustr, fmt, ap) < 0) {
        return;
    }
    va_end(ap);

    if (msprintf(&str, "%s: %s", func, ustr) >= 0) {
        funcall(env, "message", 1, str_make(env, str));
    }

    free(ustr);
    free(str);
}
