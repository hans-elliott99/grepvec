/* grepvec.c
 * search a vector of strings for matches in a vector of fixed strings or regex
 * Author: Hans Elliott
 * Date: 2024-02-16
 * License: MIT 2024 grepvec authors
 *
 * Args:
 *   haystck - character(Nh) of strings to search over
 *   needles - character(Nn) of sub-strings to search with
 *   matchrule - integer(1), determining the output content
 *   fixed - integer(1), if 1 look for exact sub-string match, else use regex
 *   ignorecase - integer(1), if 1 ignore case when regex searching
 */
#include <stdlib.h> // null
#include <string.h> // memset
#include <R.h>
#include <Rinternals.h>
#include <R_ext/Utils.h> // R_CheckUserInterrupt

#include "tre/tre.h" // tre_regcomp, tre_regexec, tre_regfree, tre_regerror


/*
    TODO:
    Can I memset to NA int ?
    Also, should I not be using any Rf_... functions?
*/

/* interval at which to check interrupts */
#define NINTERRUPT 100


enum MatchRule {
    RETURNALL   = 0,
    RETURNFIRST = 1
};

struct StringCache {
    const char **arr;
    int n;
} string_cache = {NULL, 0};


/* use cache so only need to translate chars once */
void init_cache(struct StringCache *cache, int n) {
    cache->arr = (const char **)R_Calloc(n, char*);
    cache->n = n;
}


void update_cache(SEXP ndl, struct StringCache *cache, int idx) {
    // if (idx >= cache->n || idx < 0) error("index out of bounds");
    if (cache->arr[idx] != NULL) return;
    cache->arr[idx] = translateCharUTF8(ndl);
}


/*
    need to free arr since it is R_Calloc'd, but not the individual strings
    since they are just pointers to the original R strings *unless* they are
    R_alloc'd by translateCharUTF8, in which case they are freed by the gc
    when vmax is reset
*/
void free_cache(struct StringCache *cache) {
    if (cache->n == 0) return;
    R_Free(cache->arr); // R_Calloc must be R_Free'd
    cache->n = 0;
}

/*
    called by on.exit in R wrapper function so that resources are freed even if
    interrupted
        - note, if function returns void, I get warning at R level:
        "converting NULL pointer to R NULL"
        (changing the definition to void ... in init.c too doesn't fix this)
*/
SEXP on_exit_grepvec_(void) {
    free_cache(&string_cache);
    return R_NilValue;
}


/*
    ////////////////////////////// REGEX GREPVEC //////////////////////////////
*/

/* compile regex for pattern j if valid regex */
int init_regex(SEXP ndl, regex_t *rgx_empty, int flags) {
    char errbuff[1001];
    const void *vmax = vmaxget();
    const char *ndl_str = translateCharUTF8(ndl);
    int reti = tre_regcompb(rgx_empty, ndl_str, flags);
    vmaxset(vmax); // rm space allocated by translateCharUTF8
    if (reti != 0) {
        tre_regerror(reti, rgx_empty, errbuff, sizeof(errbuff));
        warning("invalid regular expression '%s': %s", ndl_str, errbuff);
        return 1;
    }
    return 0;
}


/* test if regex finds a match in the string */
int strrgx(const char **str, regex_t *rgx) {
    char errbuff[1001];
    int reti = tre_regexecb(rgx, *str, 0, NULL, 0);
    if (reti == 0)
        return 1;
    if (reti != REG_NOMATCH) {
        tre_regerror(reti, rgx, errbuff, sizeof(errbuff));
        warning("regex match failed with error: %s", errbuff);
    }
    return 0;
}



SEXP grepvec_(SEXP needles,
              SEXP haystck,
              SEXP fixed,
              SEXP matchrule,
              SEXP ignorecase,
              SEXP keepdim) {
    int Nn = LENGTH(needles);
    int Nh = LENGTH(haystck);
    int fx = asInteger(fixed);
    int mrule = asInteger(matchrule);
    int keep = asInteger(keepdim);
    /*initial length of match vector for each needle*/
    int Nm = (!keep && mrule != RETURNALL) ? 1 : Nh;

    /*result vector - list of integer vectors*/
    SEXP output = PROTECT(allocVector(VECSXP, Nn));

    int i, j, nmatch, skip;
    int rgx_flags = REG_EXTENDED|REG_NOSUB;
    if (asInteger(ignorecase)) rgx_flags |= REG_ICASE;
    regex_t rgx;
    char *ndl_str;
    void *vmax_outer = NULL;
    init_cache(&string_cache, Nh);
    SEXP indices;
    /*
        iterate and compare compiled regex j with string i
    */
    for (j=0; j < Nn; ++j) {
        if ((j + 1) % NINTERRUPT == 0) R_CheckUserInterrupt();
        skip = (fx) ? 0 : init_regex(STRING_ELT(needles, j), &rgx, rgx_flags);
        if (STRING_ELT(needles, j) == NA_STRING || skip) {
            SET_VECTOR_ELT(output, j, allocVector(INTSXP, 0));
            continue;
        }

        indices = PROTECT(allocVector(INTSXP, Nm));
        if (fx) {
            vmax_outer = vmaxget();
            ndl_str = (char *)translateCharUTF8(STRING_ELT(needles, j));
        }

        nmatch = 0;   // num matches for pattern j
        for (i=0; i < Nh; ++i) {
            if (STRING_ELT(haystck, i) == NA_STRING) {
                continue;
            }
            update_cache(STRING_ELT(haystck, i), &string_cache, i);
            if (
                (fx) ? (strstr(string_cache.arr[i], ndl_str) != NULL) :
                        strrgx(&string_cache.arr[i], &rgx)
            ){
                INTEGER(indices)[nmatch] = i + 1; // R's idx for current hay
                ++nmatch;
                if (mrule == RETURNFIRST) break;
            }
        }
        if (fx)
            vmaxset(vmax_outer); // rm mem allocated for ndl_str
        else
            tre_regfree(&rgx);
        if (keep) {
            for (i=nmatch; i < Nh; ++i) INTEGER(indices)[i] = NA_INTEGER;
            nmatch = Nh;
        }
        // rm extra space allocated to match vec
        SETLENGTH(indices, nmatch);
        SET_VECTOR_ELT(output, j, indices);
        UNPROTECT(1); // indices
    }

    UNPROTECT(1); // output
    return output;
}
