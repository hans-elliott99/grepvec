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
#include <string.h> // strstr
#include <locale.h> // setlocale
#include <R.h>
#include <Rinternals.h>
#include <R_ext/Utils.h> // R_CheckUserInterrupt
// #include <R_ext/Riconv.h>

#include "tre/tre.h" // tre_regcomp, tre_regexec, tre_regfree, tre_regerror


/* interval at which to check interrupts */
#define NINTERRUPT 100


enum MatchRule {
    RETURNALL   = 0,
    RETURNFIRST = 1
};

struct StringCache {
    const wchar_t **arr;
    R_xlen_t n;
} string_cache = {NULL, 0};


#define IS_UTF8(x)  (LEVELS(x) & 8)
#define IS_ASCII(x) (LEVELS(x) & 64)
#define IS_LATIN(x) (LEVELS(x) & 4)

/*
    TODO: using this approach it should be doable to convert to wide char like
    R does when needed...
    Problem - is it going to be a big performance hit to always be translating
    to wide char?
    Also - this actually seems to work... so how to make it portable?

    (else return get_char to translateCharUTF8)
*/
wchar_t *get_char(SEXP x) {
    if (IS_ASCII(x) || x == NA_STRING) {
        Rprintf("ascii or NA\n");
        // return CHAR(x);
    } else if (IS_UTF8(x)) {
        /*need wchar*/
    } else if (IS_LATIN(x)) {
        Rprintf("latin1\n");
        /*need wchar*/
    } else {
        Rprintf("other\n");
        /*need whcar*/
    }
    const char *src = translateCharUTF8(x);
    if (setlocale(LC_ALL, "") == NULL)
        error("setlocale failed.\n");
    wchar_t *wcs;
    size_t length = mbstowcs(NULL, src, 0);
    if (length == (size_t)-1) error("mbstowcs failed1.\n");
    wcs = (wchar_t *)R_alloc(length + 1, sizeof(wchar_t));
    if (wcs == NULL) error("calloc failed.\n");
    if (mbstowcs(wcs, src, length + 1) == (size_t)-1)
        error("mbstowcs failed2.\n");

    Rprintf("Wide character string is: %ls (%zu characters)\n",
            wcs, length);

    return wcs;
}


/* cache so only need to translate chars once */
void init_cache(struct StringCache *cache, R_xlen_t n) {
    cache->arr = (const wchar_t **)R_Calloc(n, wchar_t*);
    cache->n = n;
}


void update_cache(SEXP ndl, struct StringCache *cache, R_xlen_t idx) {
    // if (idx >= cache->n || idx < 0) error("index out of bounds");
    if (cache->arr[idx] != NULL) return;
    cache->arr[idx] = get_char(ndl);
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


/* compile regex for pattern if valid regex */
int init_regex(SEXP ndl, regex_t *rgx_empty, int flags) {
    char errbuff[1001];
    // const void *vmax = vmaxget();
    const wchar_t *ndl_str = get_char(ndl);
    int reti = tre_regwcomp(rgx_empty, ndl_str, flags);
    // vmaxset(vmax); // rm space allocated in translateCharUTF8 via R_alloc
    if (reti != 0) {
        tre_regerror(reti, rgx_empty, errbuff, sizeof(errbuff));
        warning("invalid regular expression '%s': %s", ndl_str, errbuff);
        return 1;
    }
    return 0;
}


/* test if regex finds a match in the string */
int strrgx(const wchar_t **str, regex_t *rgx) {
    char errbuff[1001];
    int reti = tre_regwexec(rgx, *str, 0, NULL, 0);
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
    const R_xlen_t Nn = XLENGTH(needles);
    const R_xlen_t Nh = XLENGTH(haystck);
    const int fx = asInteger(fixed);
    const int mrule = asInteger(matchrule);
    const int keep = asInteger(keepdim);
    int rgx_flags = REG_EXTENDED|REG_NOSUB|TRE_MULTIBYTE;
    if (asInteger(ignorecase)) rgx_flags |= REG_ICASE;
    /*initial length of match vector for each needle*/
    const R_xlen_t Nm = (!keep && mrule != RETURNALL) ? 1 : Nh;

    /*result vector - list of integer vectors*/
    SEXP output = PROTECT(allocVector(VECSXP, Nn));

    R_xlen_t i, j, nmatch;
    int skip;
    regex_t rgx;
    wchar_t *ndl_str;
    SEXP indices;
    init_cache(&string_cache, Nh);
    /*
        iterate and compare compiled regex j with string i
    */
    for (j=0; j < Nn; ++j) {
        if ((j + 1) % NINTERRUPT == 0) R_CheckUserInterrupt();
        skip = (fx) ? 0 : init_regex(STRING_ELT(needles, j), &rgx, rgx_flags);
        if (skip || STRING_ELT(needles, j) == NA_STRING) {
            SET_VECTOR_ELT(output, j, allocVector(INTSXP, 0));
            continue;
        }
        indices = PROTECT(allocVector(INTSXP, Nm));
        if (fx) {
            ndl_str = (wchar_t *)get_char(STRING_ELT(needles, j));
        }

        nmatch = 0;   // num matches for pattern j
        for (i=0; i < Nh; ++i) {
            if (STRING_ELT(haystck, i) == NA_STRING) {
                continue;
            }
            update_cache(STRING_ELT(haystck, i), &string_cache, i);
            // if ((fx) ? (strstr(string_cache.arr[i], ndl_str) != NULL) :
            if (strrgx(&string_cache.arr[i], &rgx)
            ) {
                INTEGER(indices)[nmatch] = i + 1; // R's idx for current hay
                ++nmatch;
                if (mrule == RETURNFIRST) break;
            }
        }
        if (!fx) tre_regfree(&rgx);
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

/*
do_grep, src/main/grep.c

DETERMINE ENCODING INFO
- PATTERN = NA: return vector of NA (integer or string if value = TRUE)
- while useBytes = FALSE:
	- check if PATTERN is only ASCII. If TRUE, check if all TEXTs are also ASCII
		- if TRUE, useBytes = TRUE, next
		- if FALSE, useBytes = FALSE
	- check if PATTERN contains bytes. If FALSE, check if any TEXTs have bytes.
		- if TRUE, useBytes = TRUE, next
		- if FALSE, usebytes = FALSE
	- check if PATTERN is UTF8. If FALSE, check if any TEXTs are UTF8
		- if TRUE, use_UTF8 = TRUE
		- if FALSE, use_UTF8 = FALSE
    - if useUTF8 = TRUE and if not fixed and not PERL (so using TRE to do a regex search):
    	- use_WC = TRUE
    	- use_UTF8 = FALSE

COMPILE REGEX
- if useBytes = TRUE:
	- leave the pattern alone (use CHAR to access it)
- else if use_WC = TRUE
	- use translateChar to access the pattern (translate from current locale)

- if fixed:
	- do nothing
- if PERL:
	- if use_UTF8 and not useBytes, PCRE_UTF8 flag is set
	- compile regex...
- if TRE (default regex engine):
	- if use_WC (because UTF8 string was found), compile with tre_regwcomp
	- else, compile with tre_recompb (because useBytes = TRUE)


ITERATE THROUGH TEXTs:
- if TEXT is NA, skip
- if useBytes:
	- use CHAR to access TEXT (no translation), use tre_regexecb to execute regex
- if use_WC (because use_UTF8) is true


*/