/*
 * Copyright (c) 2019, Hans Jerry Illikainen <hji@dyntopia.com>
 *
 * SPDX-License-Identifier: GPL-3.0-or-later
 */

#ifndef LOG_H
#define LOG_H

#include <emacs-module.h>

#define debug(...) log_debug(env, __func__, __VA_ARGS__)

void log_debug(emacs_env *env, const char *func, const char *fmt, ...);

#endif
