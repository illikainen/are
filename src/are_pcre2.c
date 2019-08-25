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
static uint32_t are_pcre2_parse_options(emacs_env *env, emacs_value optlist)
{
    const struct are_pcre2_option *p;
    emacs_value memq;
    uint32_t opts = 0;

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

/*
 * Compile a PCRE2 pattern.
 */
static void *are_pcre2_compile(emacs_env *env, struct str *regexp,
                               emacs_value options)
{
    PCRE2_UCHAR error[512];
    PCRE2_SIZE offset;
    pcre2_code *re;
    uint32_t opts;
    int rc;

    (void)options;

    opts = are_pcre2_parse_options(env, options);
    re = pcre2_compile((PCRE2_SPTR)regexp->str, regexp->size - 1, opts, &rc,
                       &offset, NULL);
    if (re == NULL) {
        pcre2_get_error_message(rc, error, sizeof(error));
        non_local_exit_signal(env, "invalid-regexp", "%s", error);
        return NULL;
    }
    return re;
}

static void are_pcre2_free(void *ptr)
{
    pcre2_code_free(ptr);
}

static emacs_value are_pcre2_match(emacs_env *env, void *ptr, struct str *str,
                                   emacs_value options)
{
    PCRE2_UCHAR error[512];
    PCRE2_SIZE *ovector;
    int rc;
    uint32_t i;
    uint32_t count;
    uint32_t opts = 0;
    pcre2_code *re = ptr;
    pcre2_match_data *match_data = NULL;
    emacs_value *matches = NULL;
    emacs_value rv = env->intern(env, "nil");

    (void)options;

    match_data = pcre2_match_data_create_from_pattern(re, NULL);
    if (match_data == NULL) {
        non_local_exit_signal(env, "error", "Cannot create match data");
        goto out;
    }

    opts = are_pcre2_parse_options(env, options);
    rc = pcre2_match(re, (PCRE2_SPTR)str->str, str->size - 1, 0, opts,
                     match_data, NULL);
    if (rc < 0) {
        if (rc != PCRE2_ERROR_NOMATCH) {
            pcre2_get_error_message(rc, error, sizeof(error));
            non_local_exit_signal(env, "error", "Match error: %s", error);
        }
        goto out;
    }

    ovector = pcre2_get_ovector_pointer(match_data);
    count = pcre2_get_ovector_count(match_data);
    if (ovector == NULL || count == 0 || mul_overflow(count, 2, &count)) {
        non_local_exit_signal(env, "error", "Invalid ovector");
        goto out;
    }

    matches = calloc(count, sizeof(*matches));
    if (matches == NULL) {
        non_local_exit_signal(env, "error", "Too many matches");
        goto out;
    }

    for (i = 0; i < count; i++) {
        matches[i] = size_make(env, str_position(str, ovector[i]));
    }

    rv = apply(env, "list", count, matches);

out:
    free(matches);
    pcre2_match_data_free(match_data);
    return rv;
}

static struct are_engine pcre2 = {
    .compile = are_pcre2_compile,
    .free = are_pcre2_free,
    .match = are_pcre2_match,
};
are_register_engine(pcre2);
