#include "stringutil.h" //do_translate_char
#include "widestring.h" //RwtransChar
#include "regexp.h"

/*
                REGULAR EXPRESSION INITIALIZATION
*/

/* compile regex for pattern, return 0 if valid, 1 if invalid.
 * rgx_empty is a pointer to an empty regex_t struct
 * flags are the regex flags to use (e.g., REG_EXTENDED|REG_NOSUB)
 * specify the ttype_t of the pattern
*/

static int do_init_regex(SEXP ndl, regex_t *rgx_empty, int flags, ttype_t tt, R_xlen_t idx) {
    char errbuff[1001];
    int reti;
    const void *vmax = vmaxget();
    if (tt == use_wchar) {
        int err;
        const wchar_t *ndl_w = RwtransChar(ndl, &err);
        if (err) {
            Riconv_warning_grepvec(err, idx, 0); // 0 for "needle"
            vmaxset(vmax);
            return 1;
        }
        reti = tre_regwcomp(rgx_empty, ndl_w, flags);
        vmaxset(vmax);
        if (reti != 0) {
            tre_regerror(reti, rgx_empty, errbuff, sizeof(errbuff));
            warning("invalid regular expression '%ls': %s", ndl_w, errbuff);
            return 1;
        }
    } else {
        const char *ndl_c = do_translate_char(ndl, tt);
        reti = tre_regcompb(rgx_empty, ndl_c, flags);
        vmaxset(vmax); // rm space potentially allocated by R_alloc
        if (reti != 0) {
            tre_regerror(reti, rgx_empty, errbuff, sizeof(errbuff));
            warning("invalid regular expression '%s': %s", ndl_c, errbuff);
            return 1;
        }
    }
    return 0;
}


int init_regex(SEXP ndl, RegexInfo *rgxo, R_xlen_t idx) {
    return do_init_regex(ndl, &rgxo->rgx, rgxo->flags, rgxo->tt, idx);
}



/*
                REGULAR EXPRESSION MATCHING
*/


/* test if regex finds  match in string */
int str_rgx_match(const char *str, regex_t *rgx) {
    char errbuff[1001];
    int reti = tre_regexecb(rgx, str, 0, NULL, 0);
    if (reti == 0)
        return 1;
    if (reti != REG_NOMATCH) {
        tre_regerror(reti, rgx, errbuff, sizeof(errbuff));
        warning("regex match failed with error: %s", errbuff);
    }
    return 0;
}


/* test if regex finds  match in wide string */
int wstr_rgx_match(const wchar_t *str, regex_t *rgx) {
    char errbuff[1001];
    int reti = tre_regwexec(rgx, str, 0, NULL, 0);
    if (reti == 0)
        return 1;
    if (reti != REG_NOMATCH) {
        tre_regerror(reti, rgx, errbuff, sizeof(errbuff));
        warning("regex match failed with error: %s", errbuff);
    }
    return 0;
}


/*
                RECULAR EXPRESSION CACHE
*/

enum RegexCacheStatus {
    STATUS_BAD = -1,
    STATUS_UNSEEN = 0,
    STATUS_COMPILED = 1
};


int init_rgx_cache(RegexCache *cache, int flags, ttype_t tt, R_xlen_t n) {
    cache->rarr = (regex_t *) R_alloc(n, sizeof(regex_t));
    cache->n = n;
    cache->flags = flags;
    cache->tt = tt;
    cache->seen = (int *) R_Calloc(n, int);
    cache->ini = 1;
    return 0;
}

int update_rgx_cache(SEXP ndl, RegexCache *cache, R_xlen_t idx) {
    // if (TYPEOF(ndl) != CHARSXP) error("needle must be a character vector");
    if (cache->seen[idx] == STATUS_COMPILED)
        return 0;
    if (cache->seen[idx] == STATUS_BAD)
        return 1;
    // else, needle hasn't been seen yet, try to compile to regex
    if ((ndl == NA_STRING) ||
        do_init_regex(ndl, &cache->rarr[idx], cache->flags, cache->tt, idx)
    ) {
        cache->seen[idx] = STATUS_BAD;
        return 1;
    }
    cache->seen[idx] = STATUS_COMPILED;
    return 0;
}

void free_rgx_cache(RegexCache *cache) {
    if (!cache->ini) return;
    for (int j=0; j < cache->n; ++j) {
        if (cache->seen[j] == STATUS_COMPILED)
            tre_regfree(&cache->rarr[j]);
    }
    R_Free(cache->seen); // R_Calloc must be R_Free'd
    cache->n = 0;
    cache->ini = 0;
    cache->rarr = NULL;
    cache->seen = NULL;
}
