/* grepvec.c
 * search vector of string with vector of sub-strings
 * Author: Hans Elliott
 * Date: 2023-09-13
 * 
 * NOTES:
 * To re-compile:> R CMD SHLIB grepvec.c
 * 
 * TODO:
 * - Option to return all matched sub-strings/patterns, not just first or last -
 *   would have to have a vector for each string, grow manually as matches found
 */
#include <stdio.h>  //fprintf
#include <stdlib.h> //malloc
#include <string.h> //strcpy
#include <regex.h>  //regcomp,regexec,regfree
#include <R.h>
#include <Rinternals.h>

char rgxmsg[100];

/* grepvec - search for all sub-strings in needles within each string in haystck
 * Args:
 *   haystck - character(n_hay) of strings to search over
 *   needles - character(n_ndl) of sub-strings to search with
 *   fixed - integer(1), if 1 look for exact sub-string match, else use regex
 *   usefirst - integer(1), if 1 use first match, if 0 use last match
 */


////////// HELPERS //////////
// Matchrule
int RETURNALL = 0;
int RETURNFIRST = 1;
int RETURNLAST = 2;


void uselastmatch(SEXP matches, int Nh) {
    SEXP vec;
    int m;
    for (int i=0; i < Nh; i++) {
        // get the match vector for string i, set the first element to
        // the last idx, remove all other elements
        vec = VECTOR_ELT(matches, i);
        m = length(vec);
        INTEGER(vec)[0] = INTEGER(vec)[m-1];
        SETLENGTH(vec, 1);
    }
}


////////// REGEX //////////
// test if regex matches string
int matchregex(const char **str, regex_t *rgx) {
    int reti;
    reti = regexec(rgx, *str, 0, NULL, 0);
    if (reti == 0)
        return 1;
    if (reti != REG_NOMATCH) {
        regerror(reti, rgx, rgxmsg, sizeof(rgxmsg));
        warning("Regex match failed: %s\n", rgxmsg);
    }
    return 0;
}


SEXP grepvec_regex(SEXP haystck,
                   SEXP needles,
                   SEXP matchrule) {
    int Nh = length(haystck);
    int Nn = length(needles);
    int mr = INTEGER(matchrule)[0];
    int Nm = (mr == RETURNFIRST) ? 1 : Nn; // len of match vector for each haystck str
    
    // result vector - list of integer vectors
    SEXP matches = PROTECT(allocVector(VECSXP, Nh));

    int i, j, reti, n, mtch;
    regex_t *rgxarr;
    int *skipidx; // 1 if we regex at idx j did not compile
    // compile patterns into regexes, store in array
    // (heap allocate to avoid stack overflow)
    rgxarr = malloc(sizeof(regex_t) * Nn);
    skipidx = calloc(Nn, sizeof(int));
    for (j=0; j < Nn; j++) {
        const char *ndl = CHAR(STRING_ELT(needles, j));
        reti = regcomp(&rgxarr[j], ndl, REG_EXTENDED|REG_NOSUB);
        if (reti != 0) {
            warning("Could not compile regex for pattern: %s\n", ndl);
            skipidx[j] = 1;
        }
    }

    // iterate and compare string i with pre-compiled regex j
    for (i=0; i < Nh; i++) {
        SEXP mtchs_i = PROTECT(allocVector(INTSXP, Nm));
        memset(INTEGER(mtchs_i), 0, Nm * sizeof(int));
        const char *hay = CHAR(STRING_ELT(haystck, i));
        n = 0; // num matches for string i

        for (j=0; j < Nn; j++) {
            if (skipidx[j] == 0) {
                mtch = matchregex(&hay, &rgxarr[j]);
                if (mtch == 1) {
                    INTEGER(mtchs_i)[n] = j + 1; // R's idx for the current ndl
                    n++;
                    if (mr == RETURNFIRST) break;
                }
            }
        }
        SETLENGTH(mtchs_i, (n == 0) ? 1 : n); // rm extra space allocated to match vec
        SET_VECTOR_ELT(matches, i, mtchs_i);
    }

    // free mem used for regexes allocated in regcomp
    for (j=0; j < Nn; j++) {
        if (skipidx[j] == 0) regfree(&rgxarr[j]);
    }
    free(rgxarr);
    free(skipidx);

    // keeplast
    if (mr == RETURNLAST)
        uselastmatch(matches, Nh);

    UNPROTECT(Nh + 1);
    return matches;
}


////////// FIXED //////////
/* Search for sub-string exactly as it is.
 * (like using grep with fixed = TRUE / no regular expression)
 *   str - string to search over
 *   sub - sub-string to search for
 *   returns 1 if the sub-string is found
 */
int matchfixed(const char **str, const char **sub) {
    char *res = strstr(*str, *sub);
    if (res != NULL) {
        return 1;
    } else {
        return 0;
    }
}


SEXP grepvec_fixed(SEXP haystck,
                   SEXP needles,
                   SEXP matchrule) {
    int Nh = length(haystck);
    int Nn = length(needles);
    int mr = INTEGER(matchrule)[0];
    int Nm = (mr == RETURNFIRST) ? 1 : Nn; // len of match vector for each haystck str
    
    // result vector - list of integer vectors
    SEXP matches = PROTECT(allocVector(VECSXP, Nh));

    int i, j, n, mtch;
    // iterate and look for exact sub-string j in string i
    for (i=0; i < Nh; i++) {
        SEXP mtchs_i = PROTECT(allocVector(INTSXP, Nm));
        memset(INTEGER(mtchs_i), 0, Nm * sizeof(int));
        const char *hay = CHAR(STRING_ELT(haystck, i));
        n = 0; // num matches for string i

        for (j=0; j < Nn; j++) {
            const char *ndl = CHAR(STRING_ELT(needles, j));
            mtch = matchfixed(&hay, &ndl);
            if (mtch == 1) {
                INTEGER(mtchs_i)[n] = j + 1; // R's idx for current ndl
                n++;
                if (mr == RETURNFIRST) break;
            }
        }
        SETLENGTH(mtchs_i, (n == 0) ? 1 : n);
        SET_VECTOR_ELT(matches, i, mtchs_i);
    }

    // keeplast
    if (mr == RETURNLAST)
        uselastmatch(matches, Nh);

    UNPROTECT(Nh + 1);
    return matches;
}
