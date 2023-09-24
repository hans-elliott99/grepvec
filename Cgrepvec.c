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

char rgxmsg[100];

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


////////// MAIN //////////
/* grepvec - search for all sub-strings in needles within each string in haystck
 * Args:
 *   haystck - character(n_hay) of strings to search over
 *   n_hay - integer(1), length of haystck
 *   needles - character(n_ndl) of sub-strings to search with
 *   n_ndl - integer(1), length of needles
 *   matches - integer(n_hay) of to fill with found indices of found matches
 *   fixed - integer(1), if 1 look for exact sub-string match, else use regex
 *   usefirst - integer(1), if 1 use first match, if 0 use last match
 */
void grepvec(const char **haystck, int *n_hay,
             const char **needles, int *n_ndl,
             int *matches,
             int *fixed,
             int *usefirst) {
    int Ni = *n_hay;
    int Nj = *n_ndl;
    int fx = *fixed;
    int uf = *usefirst;
    int mtch;
    int i, j;

    // IF REGEX
    if (fx == 0) {
        // compile patterns into regexes, store in array
        // (heap allocate to avoid stack overflow)
        regex_t *rgxarr = malloc(sizeof(regex_t) * Nj);
        int *skipidx = calloc(Nj, sizeof(int));
        int reti = 1;
        for (j=0; j < Nj; j++) {
            // skipidx[j] = 0;
            reti = regcomp(&rgxarr[j], needles[j], REG_EXTENDED);
            if (reti != 0) {
                warning("Could not compile regex for pattern: %s\n", needles[j]);
                skipidx[j] = 1;
            }
        }
 
        // iterate and compare string i with pre-compiled regex j
        for (i=0; i < Ni; i++) {
            for (j=0; j < Nj; j++) {
                if (skipidx[j] == 0) {
                    mtch = matchregex(&haystck[i], &rgxarr[j]);
                    if (mtch == 1) {
                        // set_string(&matches[i], &needles[j]);
                        matches[i] = j + 1; // R idx
                        if (uf == 1) break;
                    }
                }
            }
        }
        // free mem used for regexes allocated in regcomp
        for (j=0; j < Nj; j++) {
            if (skipidx[j] == 0) regfree(&rgxarr[j]);
        }
        free(rgxarr);
        free(skipidx);
    }
    // IF FIXED
    else {
        // iterate and look for exact sub-string j in string i
        for (i=0; i < Ni; i++) {
            for (j=0; j < Nj; j++) {
                mtch = matchfixed(&haystck[i], &needles[j]);
                if (mtch == 1) {
                    // set_string(&matches[i], &needles[j]);
                    matches[i] = j + 1; // R idx
                    if (uf == 1) break;
                }
            }
        }
    }
    //
}



/* archived
 
void test(const char **str, const char **sub) {
    const char *hay = str[0];
    const char *ndl = sub[0];
    int mtch = matchfixed(&hay, &ndl);
    Rprintf("match = %i\n", mtch);
}

void test(const char **str) {
    int reti = 1;
    regex_t rgx;
    reti = regcomp(&rgx, str[0], REG_NOSUB);
    if (reti != 0) {
        warning("Could not compile regex for pattern: %s\n", str[0]);
        regcomp(&rgx, "", REG_NOSUB);
    }
    
    int m1 = regexec(&rgx, str[0], 0, NULL, 0);
    int m2 = regexec(&rgx, "nope", 0, NULL, 0);
    Rprintf("m1 = %i\nm2 = %i\n", m1 , m2);
}

// copy string into array
//    dest - reference to char array to copy into
//    src  - reference to char array to copy
void set_string(char **dest, const char **src) {
    size_t n = strlen(*src) + 1;
    *dest = (char*) malloc(sizeof(char) * n);
    strcpy(*dest, *src);
    if (*dest[0] == '\0') {
        warning("grepvec.c:set_string:Error copying string %s\n", *src);
    }
}
 
// Fill array with compiled regexes
void regcomp_arr(regex_t *rgxarr, const char **ptrns, int *skipidx, int len) {
    int reti = 1;
    for (int i=0; i < len; i++) {
        reti = regcomp(&rgxarr[i], ptrns[i], REG_NOSUB);
        if (reti != 0) {
            warning("Could not compile regex for pattern: %s\n", ptrns[i]);
            skipidx[i] = 1;
        }
    }
}
 
*/