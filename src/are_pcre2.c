/*
 * Copyright (c) 2019, Hans Jerry Illikainen <hji@dyntopia.com>
 *
 * SPDX-License-Identifier: GPL-3.0-or-later
 */
#include <pcre2.h>

#include "are.h"
#include "types.h"
#include "utils.h"

struct are_pcre2_option {
    const char *name;
    uint32_t flag;
};

static const struct are_pcre2_option are_pcre2_options[] = {
    { "pcre2-allow-empty-class", PCRE2_ALLOW_EMPTY_CLASS },
    { "pcre2-alt-bsux", PCRE2_ALT_BSUX },
    { "pcre2-alt-circumflex", PCRE2_ALT_CIRCUMFLEX },
    { "pcre2-alt-verbnames", PCRE2_ALT_VERBNAMES },
    { "pcre2-anchored", PCRE2_ANCHORED },
    { "pcre2-auto-callout", PCRE2_AUTO_CALLOUT },
    { "pcre2-caseless", PCRE2_CASELESS },
    { "pcre2-dollar-endonly", PCRE2_DOLLAR_ENDONLY },
    { "pcre2-dotall", PCRE2_DOLLAR_ENDONLY },
    { "pcre2-dupnames", PCRE2_DUPNAMES },
    { "pcre2-extended", PCRE2_EXTENDED },
    { "pcre2-firstline", PCRE2_FIRSTLINE },
    { "pcre2-match-unset-backref", PCRE2_MATCH_UNSET_BACKREF },
    { "pcre2-multiline", PCRE2_MULTILINE },
    { "pcre2-never-backslash-c", PCRE2_NEVER_BACKSLASH_C },
    { "pcre2-never-ucp", PCRE2_NEVER_UCP },
    { "pcre2-never-utf", PCRE2_NEVER_UTF },
    { "pcre2-no-auto-capture", PCRE2_NO_AUTO_CAPTURE },
    { "pcre2-no-auto-possess", PCRE2_NO_AUTO_POSSESS },
    { "pcre2-no-dotstar-anchor", PCRE2_NO_DOTSTAR_ANCHOR },
    { "pcre2-no-jit", PCRE2_NO_JIT },
    { "pcre2-no-start-optimize", PCRE2_NO_START_OPTIMIZE },
    { "pcre2-no-utf-check", PCRE2_NO_UTF_CHECK },
    { "pcre2-notbol", PCRE2_NOTBOL },
    { "pcre2-notempty", PCRE2_NOTEMPTY },
    { "pcre2-notempty-atstart", PCRE2_NOTEMPTY_ATSTART },
    { "pcre2-noteol", PCRE2_NOTEOL },
    { "pcre2-partial-hard", PCRE2_PARTIAL_HARD },
    { "pcre2-partial-soft", PCRE2_PARTIAL_SOFT },
    { "pcre2-ucp", PCRE2_UCP },
    { "pcre2-ungreedy", PCRE2_UNGREEDY },
    { "pcre2-use-offset-limit", PCRE2_USE_OFFSET_LIMIT },
    { "pcre2-utf", PCRE2_UTF },
    { NULL, 0 },
};

/*
 * Parse PCRE2 options from `name`.
 */
static uint32_t are_pcre2_parse_options(emacs_env *env, const char *name)
{
    const struct are_pcre2_option *p;
    emacs_value optlist, memq;
    uint32_t opts = 0;

    optlist = value_of(env, name);
    if (env->is_not_nil(env, optlist)) {
        for (p = are_pcre2_options; p->name; p++) {
            memq = funcall(env, "memq", 2, env->intern(env, p->name), optlist);
            if (env->is_not_nil(env, memq)) {
                opts |= p->flag;
            }
        }
    }
    return opts;
}
