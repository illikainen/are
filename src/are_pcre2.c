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

/*
 * Expose `match-data` to emacs.
 */
static int are_pcre2_set_match_data(emacs_env *env, struct str *str,
                                    PCRE2_SIZE *ovector, uint32_t count,
                                    bool markerp)
{
    size_t i, nmemb, pos;
    emacs_value list;
    emacs_value mark;
    emacs_value *data = NULL;
    int rv = -1;

    if (count == 0 || mul_overflow(count, 2, &nmemb)) {
        goto out;
    }

    data = calloc(nmemb, sizeof(*data));
    if (data == NULL) {
        goto out;
    }

    for (i = 0; i < nmemb; i++) {
        if (markerp) {
            mark = funcall(env, "make-marker", 0);
            if (!env->is_not_nil(env, mark)) {
                goto out;
            }

            /* buffer positions start at 1 */
            pos = str_position(str, ovector[i]);
            if (add_overflow(pos, 1, &pos)) {
                goto out;
            }

            data[i] = funcall(env, "set-marker", 2, mark, size_make(env, pos));
            if (!env->is_not_nil(env, data[i])) {
                goto out;
            }
        } else {
            pos = str_position(str, ovector[i]);
            data[i] = size_make(env, pos);
        }
    }

    list = apply(env, "list", nmemb, data);
    if (!env->is_not_nil(env, list)) {
        goto out;
    }

    funcall(env, "set-match-data", 1, list);
    rv = 0;

out:
    free(data);
    return rv;
}

/*
 * PCRE2 variant of `re-search-forward`.
 */
static emacs_value are_pcre2_re_search_forward(emacs_env *env,
                                               struct str *regexp,
                                               struct str *str, size_t bound,
                                               enum are_noerror noerror,
                                               intmax_t count)
{
    PCRE2_UCHAR error[512];
    PCRE2_SIZE offset;
    PCRE2_SIZE start;
    PCRE2_SIZE last_match;
    PCRE2_SIZE *ovector;
    uint32_t ovector_count;
    int rc;
    uint32_t opts;
    size_t point;
    pcre2_code *re = NULL;
    pcre2_match_data *match_data = NULL;
    emacs_value next_point;
    emacs_value orig_point = funcall(env, "point", 0);
    emacs_value nil = env->intern(env, "nil");
    emacs_value rv = nil;

    if (mul_overflow(intmax_extract(env, orig_point), 1, &point)) {
        non_local_exit_signal(env, "Invalid point");
        goto out;
    }

    opts = are_pcre2_parse_options(env, "are-compile-options");
    re = pcre2_compile((PCRE2_SPTR)regexp->str, regexp->size - 1, opts, &rc,
                       &offset, NULL);
    if (re == NULL) {
        pcre2_get_error_message(rc, error, sizeof(error));
        non_local_exit_signal(env, "Compile: %lu: %s", offset, error);
        goto out;
    }

    match_data = pcre2_match_data_create_from_pattern(re, NULL);
    if (match_data == NULL) {
        non_local_exit_signal(env, "Cannot create match data");
        goto out;
    }

    opts = are_pcre2_parse_options(env, "are-match-options");
    if (count > 0) {
        start = 0;

        while (count--) {
            if (start >= str_length(str) || bound == 0) {
                rv = nil;
                if (noerror == ARE_NOERROR_NIL) {
                    non_local_exit_signal(env, "Match error");
                } else if (noerror == ARE_NOERROR_T) {
                    funcall(env, "goto-char", 1, orig_point);
                } else {
                    funcall(env, "goto-char", 1, size_make(env, bound));
                }
                goto out;
            }

            rc = pcre2_match(re, (PCRE2_SPTR)str->str, bound - 1, start, opts,
                             match_data, NULL);
            if (rc < 0) {
                rv = nil;
                if (rc != PCRE2_ERROR_NOMATCH || noerror == ARE_NOERROR_NIL) {
                    pcre2_get_error_message(rc, error, sizeof(error));
                    non_local_exit_signal(env, "Match error: %s", error);
                } else if (noerror == ARE_NOERROR_T) {
                    funcall(env, "goto-char", 1, orig_point);
                } else if (noerror == ARE_NOERROR_OTHER) {
                    funcall(env, "goto-char", 1, size_make(env, bound));
                }
                goto out;
            }

            ovector = pcre2_get_ovector_pointer(match_data);
            ovector_count = pcre2_get_ovector_count(match_data);
            if (ovector == NULL || ovector_count < 1) {
                non_local_exit_signal(env, "Invalid ovector");
                goto out;
            }

            if (are_pcre2_set_match_data(env, str, ovector, ovector_count,
                                         true) != 0) {
                non_local_exit_signal(env, "Unable to set match data");
                goto out;
            }

            if (add_overflow(ovector[1], start, &start) ||
                add_overflow(ovector[1], 1, &point)) {
                non_local_exit_signal(env, "Invalid position");
                goto out;
            }

            next_point = size_make(env, str_position(str, point));
            rv = funcall(env, "goto-char", 1, next_point);
        }
    } else {
        start = str->size;
        last_match = str->size - 1;

        while (count && (start = str_position_prev(str, start)) &&
               start >= bound) {
            rc = pcre2_match(re, (PCRE2_SPTR)str->str, last_match, start, opts,
                             match_data, NULL);
            if (rc < 0) {
                if (rc == PCRE2_ERROR_NOMATCH) {
                    continue;
                }
                pcre2_get_error_message(rc, error, sizeof(error));
                non_local_exit_signal(env, "Match error: %s", error);
                goto out;
            }
            last_match = start;
            count++;

            ovector = pcre2_get_ovector_pointer(match_data);
            ovector_count = pcre2_get_ovector_count(match_data);
            if (ovector == NULL || ovector_count < 1) {
                non_local_exit_signal(env, "Invalid ovector");
                goto out;
            }

            if (are_pcre2_set_match_data(env, str, ovector, ovector_count,
                                         true) != 0) {
                non_local_exit_signal(env, "Unable to set match data");
                goto out;
            }

            if (mul_overflow(str_position(str, ovector[0]), 1, &point) ||
                add_overflow(point, 1, &point)) {
                non_local_exit_signal(env, "Invalid position");
                goto out;
            }
            rv = funcall(env, "goto-char", 1, size_make(env, point));
        }

        if (count) {
            rv = nil;

            if (noerror == ARE_NOERROR_NIL) {
                non_local_exit_signal(env, "No match");
            } else if (noerror == ARE_NOERROR_T) {
                funcall(env, "goto-char", 1, orig_point);
            } else if (noerror == ARE_NOERROR_OTHER) {
                funcall(env, "goto-char", 1, size_make(env, bound));
            }
            goto out;
        }
    }

out:
    pcre2_match_data_free(match_data);
    pcre2_code_free(re);
    return rv;
}

/*
 * PCRE2 variant of `string-match`.
 */
static emacs_value are_pcre2_string_match(emacs_env *env, struct str *regexp,
                                          struct str *str, size_t start)
{
    PCRE2_UCHAR error[512];
    PCRE2_SIZE *ovector;
    PCRE2_SIZE offset;
    int rc;
    uint32_t count;
    uint32_t opts;
    pcre2_code *re = NULL;
    pcre2_match_data *match_data = NULL;
    emacs_value rv = env->intern(env, "nil");

    opts = are_pcre2_parse_options(env, "are-compile-options");
    re = pcre2_compile((PCRE2_SPTR)regexp->str, str_length(regexp), opts, &rc,
                       &offset, NULL);
    if (re == NULL) {
        pcre2_get_error_message(rc, error, sizeof(error));
        non_local_exit_signal(env, "Compile: %lu: %s", offset, error);
        goto out;
    }

    match_data = pcre2_match_data_create_from_pattern(re, NULL);
    if (match_data == NULL) {
        non_local_exit_signal(env, "Cannot create match data");
        goto out;
    }

    opts = are_pcre2_parse_options(env, "are-match-options");
    rc = pcre2_match(re, (PCRE2_SPTR)str->str, str_length(str), start, opts,
                     match_data, NULL);
    if (rc < 0) {
        if (rc != PCRE2_ERROR_NOMATCH) {
            pcre2_get_error_message(rc, error, sizeof(error));
            non_local_exit_signal(env, "%d: %s", rc, error);
        }
        goto out;
    }

    ovector = pcre2_get_ovector_pointer(match_data);
    count = pcre2_get_ovector_count(match_data);
    if (ovector == NULL || count == 0) {
        non_local_exit_signal(env, "Invalid ovector");
        goto out;
    }

    if (are_pcre2_set_match_data(env, str, ovector, count, false) != 0) {
        non_local_exit_signal(env, "Unable to set match data");
        goto out;
    }

    rv = size_make(env, ovector[0]);

out:
    pcre2_match_data_free(match_data);
    pcre2_code_free(re);
    return rv;
}

static struct are_engine pcre2 = {
    .re_search_forward = are_pcre2_re_search_forward,
    .string_match = are_pcre2_string_match,
};
are_register_engine(pcre2);
