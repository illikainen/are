/*
 * Copyright (c) 2019, Hans Jerry Illikainen <hji@dyntopia.com>
 *
 * SPDX-License-Identifier: GPL-3.0-or-later
 */

#include <emacs-module.h>
#include <utlist.h>

#include "module.h"
#include "types.h"
#include "utils.h"

int plugin_is_GPL_compatible;

static struct module_fun *module_fun_head = NULL;

/* cppcheck-suppress unusedFunction */
int emacs_module_init(struct emacs_runtime *ert)
{
    struct module_fun *m;
    emacs_env *env = ert->get_environment(ert);

    /* check for compatibility with the running emacs process */
    if ((size_t)ert->size < sizeof(*ert) || (size_t)env->size < sizeof(*env)) {
        return 1;
    }

    /* define functions */
    LL_FOREACH (module_fun_head, m) {
        make_function(env, m->name, m->fun, m->min_arity, m->max_arity,
                      m->docstring, m->data);
    }

    /* allow the module to be require()d */
    funcall(env, "provide", 1, env->intern(env, "libare"));

    return 0;
}

/*
 * Add a function to be defined when the module is loaded.
 */
void module_add_fun(struct module_fun *m)
{
    LL_APPEND(module_fun_head, m);
}
