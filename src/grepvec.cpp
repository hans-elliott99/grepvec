/* grepvec.c
 * search a vector of strings for matches in a vector of fixed strings or regex
 * Author: Hans Elliott
 * Date: 2024-02-16
 * License: MIT 2024 grepvec authors
 *
 *
 *
 * Args:
 *   haystck - character(n_hay) of strings to search over
 *   needles - character(n_ndl) of sub-strings to search with
 *   matchrule - integer(1), 0 for all matches, 1 for first, 2 for last
 *   fixed - integer(1), if 1 look for exact sub-string match, else use regex
 *   ignorecase - integer(1), if 1 ignore case when matching
 */
#include <stdlib.h> //null
#include <string.h> //memset

#include <cpp11.hpp>

#include <R.h>
#include <Rinternals.h>
#include <R_ext/Utils.h>

#include <regex.h>

#define tre_regcomp regcomp
#define tre_regexec regexec
#define tre_regfree regfree
#define tre_regerror regerror


enum MatchRule {
    RETURNALL   = 0,
    RETURNFIRST = 1,
    RETURNLAST  = 2,
};

struct RegexCache {
    regex_t *rgxarr;
    int *seen;
    int flags;
    int Nn;
} rgx_cache = {NULL, NULL, 0, 0};


struct FixedCache {
    const char **ndlarr;
    int Nn;
} fixed_cache = {NULL, 0};



/*
    called by on.exit in R wrapper function so that resources are freed even if
    interrupted
        - note, if function returns void, I get warning:
        "converting NULL pointer to R NULL", which I think has to do w init.c
        (but changing the definition to void ... in init.c doesn't fix)
*/
void free_regex_cache(struct RegexCache *cache);
void free_fixed_cache(struct FixedCache *cache);

[[cpp11::register]]
SEXP on_exit_grepvec_(void) {
    free_regex_cache(&rgx_cache);
    free_fixed_cache(&fixed_cache);
    return R_NilValue;
}

/*
    ////////////////////////////// REGEX GREPVEC //////////////////////////////
*/

/* initialize regex cache */
void init_regex_cache(struct RegexCache *cache, int Nn, int ignorecase) {
    // cache->arr = (regex_t *)R_alloc(Nn, sizeof(regex_t));
    cache->rgxarr = (regex_t *)R_Calloc(Nn, regex_t);
    cache->seen   = (int *)R_Calloc(Nn, int);
    cache->flags = REG_EXTENDED|REG_NOSUB;
    if (ignorecase) cache->flags |= REG_ICASE;
    cache->Nn = Nn;
}

/* free resources used by cache */
void free_regex_cache(struct RegexCache *cache) {
    if (cache->Nn == 0) return;
    for (int j=0; j < cache->Nn; j++) {
        if (cache->seen[j] == 1) {
            tre_regfree(&cache->rgxarr[j]);
        }
    }
    // R_Calloc needs to be R_Free'd
    R_Free(cache->rgxarr);
    R_Free(cache->seen);
    cache->Nn = 0;
}


/* compile regex for pattern j if not already done, and if valid regex */
int check_regex_cache(SEXP *ndl, struct RegexCache *cache, int idx) {
    if (cache->seen[idx] == 1)  // regex is already compiled
        return 1;
    if (cache->seen[idx] == -1) // regex should be skipped
        return 0;
    // else, needle hasn't been seen yet
    const void *vmax = vmaxget();
    const char *ndl_str = Rf_translateCharUTF8(*ndl);
    int reti = tre_regcomp(&cache->rgxarr[idx], ndl_str, cache->flags);
    vmaxset(vmax); // rm space allocated for ndl_str
    if (reti != 0) {
        cpp11::warning("could not compile regex for pattern: %s", ndl_str);
        cache->seen[idx] = -1;
        return 0;
    }
    cache->seen[idx] = 1;
    return 1;
}


/* test if regex finds a match in the string */
int matchregex(const char **str, regex_t *rgx) {
    char rgxmsg[1001];
    int reti = tre_regexec(rgx, *str, 0, NULL, 0);
    if (reti == 0)
        return 1;
    if (reti != REG_NOMATCH) {
        tre_regerror(reti, rgx, rgxmsg, sizeof(rgxmsg));
        cpp11::warning("regex match failed: %s", rgxmsg);
    }
    return 0;
}


[[cpp11::register]]
SEXP grepvec_regex_(SEXP needles,
                    SEXP haystck,
                    SEXP matchrule,
                    SEXP ignorecase) {
    int Nh = LENGTH(haystck);
    int Nn = LENGTH(needles);
    int mrule = INTEGER(matchrule)[0];
    // inital length of match vector for each hay
    int Nm = (mrule != RETURNALL) ? 1 : Nn;

    // result vector - list of integer vectors
    SEXP matches = PROTECT(Rf_allocVector(VECSXP, Nh));

    int i, j, nmatch, anymatch;
    init_regex_cache(&rgx_cache, Nn, Rf_asLogical(ignorecase));
    void *vmax_outer = NULL;
    /*
        iterate and compare string i with compiled regex j
    */
    for (i=0; i < Nh; i++) {
        R_CheckUserInterrupt();
        if (STRING_ELT(haystck, i) == NA_STRING) {
            SET_VECTOR_ELT(matches, i, Rf_allocVector(INTSXP, 0));
            continue;
        }
        vmax_outer = vmaxget();
        const char *hay = Rf_translateCharUTF8(STRING_ELT(haystck, i));
        SEXP mtchs_i = PROTECT(Rf_allocVector(INTSXP, Nm));
        memset(INTEGER(mtchs_i), 0, Nm * sizeof(int));

        nmatch = 0;   // num matches for string i
        anymatch = 0; // true if any match for string i
        for (j=0; j < Nn; j++) {
            SEXP ndl = STRING_ELT(needles, j);
            if (ndl == NA_STRING) {
                SET_VECTOR_ELT(matches, i, Rf_allocVector(INTSXP, 0));
                continue;
            }
            if (check_regex_cache(&ndl, &rgx_cache, j)) {
                if (matchregex(&hay, &rgx_cache.rgxarr[j])) {
                    anymatch = 1;
                    INTEGER(mtchs_i)[nmatch] = j + 1; // R's idx for current ndl
                    if (mrule == RETURNFIRST) break;
                    if (mrule == RETURNALL) nmatch++;
                }
            }
        }
        vmaxset(vmax_outer); // rm space allocated for hay
        // rm extra space allocated to match vec
        if (anymatch && mrule != RETURNALL) nmatch = 1;
        SETLENGTH(mtchs_i, nmatch);
        SET_VECTOR_ELT(matches, i, mtchs_i);
        UNPROTECT(1); // mtchs_i
    }

    UNPROTECT(1); // matches
    return matches;
}


/*
    ////////////////////////////// FIXED GREPVEC //////////////////////////////
*/

/* use cache so only need to translate chars one */
void init_fixed_cache(struct FixedCache *cache, int Nn) {
    cache->ndlarr = (const char **)R_Calloc(Nn, char *);
    cache->Nn = Nn;
}

/*
    need to free ndlarr since it is R_Calloc'd, but not the individual strings
    since they are just pointers to the original R strings *unless* they are
    R_alloc'd by Rf_translateCharUTF8, in which case they are freed by the gc
    when the vmax resets
*/
void free_fixed_cache(struct FixedCache *cache) {
    if (cache->Nn == 0) return;
    R_Free(cache->ndlarr);
    cache->Nn = 0;
}

void update_fixed_cache(SEXP *ndl, struct FixedCache *cache, int idx) {
    if (cache->ndlarr[idx] != NULL) return;
    cache->ndlarr[idx] = Rf_translateCharUTF8(*ndl);
}

[[cpp11::register]]
SEXP grepvec_fixed_(SEXP needles,
                    SEXP haystck,
                    SEXP matchrule,
                    SEXP ignorecase) {
    int Nh = LENGTH(haystck);
    int Nn = LENGTH(needles);
    int mrule = INTEGER(matchrule)[0];
    // inital length of match vector for each hay
    int Nm = (mrule != RETURNALL) ? 1 : Nn;

    // result vector - list of integer vectors
    SEXP matches = PROTECT(Rf_allocVector(VECSXP, Nh));
    int i, j, nmatch;
    int anymatch;
    init_fixed_cache(&fixed_cache, Nn);
    /*
        iterate and look for exact sub-string j in string i
    */
    for (i=0; i < Nh; i++) {
        R_CheckUserInterrupt();
        if (STRING_ELT(haystck, i) == NA_STRING) {
            SET_VECTOR_ELT(matches, i, Rf_allocVector(INTSXP, 0));
            continue;
        }
        const char *hay = Rf_translateCharUTF8(STRING_ELT(haystck, i));
        SEXP mtchs_i = PROTECT(Rf_allocVector(INTSXP, Nm));
        memset(INTEGER(mtchs_i), 0, Nm * sizeof(int));

        nmatch = 0;   // num matches for string i
        anymatch = 0; // true if any match for string i
        for (j=0; j < Nn; j++) {
            SEXP ndl = STRING_ELT(needles, j);
            if (ndl == NA_STRING) {
                SET_VECTOR_ELT(matches, i, Rf_allocVector(INTSXP, 0));
                continue;
            }
            update_fixed_cache(&ndl, &fixed_cache, j);
            if (strstr(hay, fixed_cache.ndlarr[j]) != NULL) {
                anymatch = 1;
                INTEGER(mtchs_i)[nmatch] = j + 1; // R's idx for current ndl
                if (mrule == RETURNFIRST) break;
                if (mrule == RETURNALL) nmatch++;
            }
        }
        if (anymatch && mrule != RETURNALL) nmatch = 1;
        SETLENGTH(mtchs_i, nmatch);
        SET_VECTOR_ELT(matches, i, mtchs_i);
        UNPROTECT(1); // mtchs_i
    }

    UNPROTECT(1); // matches
    return matches;
}
