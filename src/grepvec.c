/* grepvec.c
 * search a vector of strings for  vector of sub-strings or regex
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
#include <ctype.h>  //tolower
#include <regex.h>  //regcomp,regexec,regfree
#include <R.h>
#include <Rinternals.h>

char rgxmsg[100];

//
////////// HELPERS //////////
//

// Match Rule
enum {
    RETURNALL = 0,
    RETURNFIRST = 1,
    RETURNLAST = 2,
} matchrule_e;


// set the first element of each match vector to that at the last idx
void uselastmatch(SEXP matches, int Nh) {
    SEXP vec;
    int m;
    for (int i=0; i < Nh; i++) {
        // get the match vector for string i, set the first element to
        // the last idx, remove all other elements
        vec = VECTOR_ELT(matches, i);
        m = length(vec);
        if (m == 0) continue;
        INTEGER(vec)[0] = INTEGER(vec)[m-1];
        SETLENGTH(vec, 1);
    }
}


//
////////// REGEX //////////
//

// test if regex matches string
int matchregex(const char **str, regex_t *rgx) {
    int reti;
    reti = regexec(rgx, *str, 0, NULL, 0);
    if (reti == 0)
        return 1;
    if (reti != REG_NOMATCH) {
        regerror(reti, rgx, rgxmsg, sizeof(rgxmsg));
        warning("regex match failed: %s", rgxmsg);
    }
    return 0;
}


SEXP grepvec_regex(SEXP haystck,
                   SEXP needles,
                   SEXP matchrule,
                   SEXP ignorecase) {
    int Nh = length(haystck);
    int Nn = length(needles);
    int mrule = INTEGER(matchrule)[0];
     // inital length of match vector for each hay
    int Nm = (mrule == RETURNFIRST) ? 1 : Nn;

    // result vector - list of integer vectors
    SEXP matches = PROTECT(allocVector(VECSXP, Nh));

    int i, j, reti, nmatch, mtch_result;
    regex_t *rgxarr;
    int *skipidx; // 1 if the regex at idx j did not compile
    /*
        compile patterns into regexes, store in array
        (heap allocate to avoid stack overflow)
    */
    int regflags = REG_EXTENDED|REG_NOSUB;
    if (INTEGER(ignorecase)[0] == 1) regflags |= REG_ICASE;
    rgxarr  = (regex_t *)R_alloc(Nn, sizeof(regex_t));
    skipidx = (int *)R_Calloc(Nn, int);
    for (j=0; j < Nn; j++) {
        const char *ndl = CHAR(STRING_ELT(needles, j));
        reti = regcomp(&rgxarr[j], ndl, regflags);
        if (reti != 0) {
            warning("could not compile regex for pattern: %s", ndl);
            skipidx[j] = 1;
        }
    }
    /*
        iterate and compare string i with pre-compiled regex j
    */
    for (i=0; i < Nh; i++) {
        SEXP mtchs_i = PROTECT(allocVector(INTSXP, Nm));
        memset(INTEGER(mtchs_i), 0, Nm * sizeof(int));
        const char *hay = CHAR(STRING_ELT(haystck, i));
        nmatch = 0; // num matches for string i

        for (j=0; j < Nn; j++) {
            if (skipidx[j] == 0) {
                mtch_result = matchregex(&hay, &rgxarr[j]);
                if (mtch_result == 1) {
                    INTEGER(mtchs_i)[nmatch] = j + 1; // R's idx for current ndl
                    nmatch++;
                    if (mrule == RETURNFIRST) break;
                }
            }
        }
        // rm extra space allocated to match vec
        SETLENGTH(mtchs_i, (nmatch == 0) ? 0 : nmatch);
        SET_VECTOR_ELT(matches, i, mtchs_i);
        // all elements of a protected list are automatically protected
        // https://cran.r-project.org/doc/manuals/R-exts.html#Garbage-Collection
        UNPROTECT(1);
    }
    /*
        free mem used for regexes allocated in regcomp
    */
    for (j=0; j < Nn; j++) {
        if (skipidx[j] == 0) regfree(&rgxarr[j]);
    }
    R_Free(skipidx); // only R_Calloc needs to be freed

    if (mrule == RETURNLAST)
        uselastmatch(matches, Nh);

    UNPROTECT(1);
    return matches;
}


//
////////// FIXED //////////
//
/*
* case insensitive strstr
* credit:
* https://stackoverflow.com/questions/27303062/strstr-function-like-that-ignores-upper-or-lower-case
*/
char* stristr(const char *haystack, const char *needle) {
    const char *p1 = haystack;
    const char *p2 = needle;
    const char *r = *p2 == 0 ? haystack : 0;

    while(*p1 != 0 && *p2 != 0) {
        if (tolower((unsigned char)*p1) == tolower((unsigned char)*p2)) {
            if (r == 0)
                r = p1;
            p2++;
        } else {
            p2 = needle;
            if (r != 0)
                p1 = r + 1;
            if (tolower((unsigned char)*p1) == tolower((unsigned char)*p2)) {
                r = p1;
                p2++;
            } else {
                r = 0;
            }
        }
        p1++ ;
    }
    return *p2 == 0 ? (char*)r : NULL;
}



/* Search for sub-string exactly as it is.
 * (like using grep with fixed = TRUE / no regular expression)
 *   str - string to search over
 *   sub - sub-string to search for
 *   returns 1 if the sub-string is found
 */
int matchfixed(const char **str, const char **sub, int ignorecase) {
    char *res;
    if (ignorecase == 1) {
        res = stristr(*str, *sub);
    } else {
        res = strstr(*str, *sub);
    }
    if (res == NULL) {
        return 0;
    } else {
        return 1;
    }
}


SEXP grepvec_fixed(SEXP haystck,
                   SEXP needles,
                   SEXP matchrule,
                   SEXP ignorecase) {
    int Nh = length(haystck);
    int Nn = length(needles);
    int mrule = INTEGER(matchrule)[0];
     // inital length of match vector for each hay
    int Nm = (mrule == RETURNFIRST) ? 1 : Nn;

    // result vector - list of integer vectors
    SEXP matches = PROTECT(allocVector(VECSXP, Nh));

    int i, j, nmatch, mtch_result;
    /*
        iterate and look for exact sub-string j in string i
    */
    for (i=0; i < Nh; i++) {
        SEXP mtchs_i = PROTECT(allocVector(INTSXP, Nm));
        memset(INTEGER(mtchs_i), 0, Nm * sizeof(int));
        const char *hay = CHAR(STRING_ELT(haystck, i));
        nmatch = 0; // num matches for string i

        for (j=0; j < Nn; j++) {
            const char *ndl = CHAR(STRING_ELT(needles, j));
            mtch_result = matchfixed(&hay, &ndl, INTEGER(ignorecase)[0]);
            if (mtch_result == 1) {
                INTEGER(mtchs_i)[nmatch] = j + 1; // R's idx for current ndl
                nmatch++;
                if (mrule == RETURNFIRST) break;
            }
        }
        SETLENGTH(mtchs_i, (nmatch == 0) ? 0 : nmatch);
        SET_VECTOR_ELT(matches, i, mtchs_i);
        // all elements of a protected list are automatically protected
        // https://cran.r-project.org/doc/manuals/R-exts.html#Garbage-Collection
        UNPROTECT(1);
    }

    if (mrule == RETURNLAST)
        uselastmatch(matches, Nh);

    UNPROTECT(1);
    return matches;
}
