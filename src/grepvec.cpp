/* grepvec.cpp
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
// #include <regex.h>  //regcomp,regexec,regfree

// #include <regex>
#include <boost/regex.hpp>
#include <iostream>
#include <cpp11.hpp>

#include <R.h>
#include <Rinternals.h>


char rgxmsg[100];

//
////////// HELPERS //////////
//


// Match Rule
enum MatchRule {
    RETURNALL = 0,
    RETURNFIRST = 1,
    RETURNLAST = 2,
};


//
////////// REGEX //////////
//

[[cpp11::register]]
SEXP grepvec_regex_(cpp11::strings needles,
                    cpp11::strings haystck,
                    int matchrule,
                    bool ignorecase) {
    int Nh = haystck.size();
    int Nn = needles.size();
    // inital length of match vector for each hay
    int Nm = (matchrule != RETURNALL) ? 1 : Nn;

    // result vector - list of integer vectors
    SEXP matches = PROTECT(cpp11::safe[Rf_allocVector](VECSXP, Nh));

    int i, j, nmatch;
    bool anymatch;
    std::vector<boost::regex> rgxarr(Nn);
    int *skipidx; // 1 if the regex at idx j did not compile
    /*
        compile patterns into regexes, store in array
    */
    boost::regex_constants::syntax_option_type flags = boost::regex::extended;
    if (ignorecase) flags |= boost::regex::icase;

    skipidx = (int *)R_Calloc(Nn, int);
    for (j=0; j < Nn; j++) {
        const char *ndl = CHAR(STRING_ELT(needles, j));
        try {
            rgxarr[j] = boost::regex(ndl, flags);
        } catch (const boost::regex_error& e) {
            cpp11::warning("%s", e.what());
            skipidx[j] = 1;
        }
    }
    /*
        iterate and compare string i with pre-compiled regex j
    */
    for (i=0; i < Nh; i++) {
        SEXP mtchs_i = PROTECT(cpp11::safe[Rf_allocVector](INTSXP, Nm));
        memset(INTEGER(mtchs_i), 0, Nm * sizeof(int));
        const char *hay = CHAR(STRING_ELT(haystck, i));
        nmatch = 0; // num matches for string i
        anymatch = false;

        for (j=0; j < Nn; j++) {
            if (skipidx[j] == 0) {
                if (boost::regex_search(hay, rgxarr[j])) {
                    INTEGER(mtchs_i)[nmatch] = j + 1; // R's idx for current ndl
                    anymatch = true;
                    if (matchrule == RETURNFIRST) break;
                    if (matchrule == RETURNALL) nmatch++;
                }
            }
        }
        if (anymatch && matchrule != RETURNALL)
            nmatch = 1;
        // rm extra space allocated to match vec
        SETLENGTH(mtchs_i, nmatch);
        SET_VECTOR_ELT(matches, i, mtchs_i);
        UNPROTECT(1); // mtchs_i
    }
    R_Free(skipidx); // only R_Calloc needs to be freed

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
int matchfixed(const char **str, const char **sub, bool ignorecase) {
    const char *res;
    if (ignorecase) {
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



[[cpp11::register]]
SEXP grepvec_fixed_(cpp11::strings needles,
                    cpp11::strings haystck,
                    int matchrule,
                    bool ignorecase) {
    int Nh = haystck.size();
    int Nn = needles.size();
    // inital length of match vector for each hay
    int Nm = (matchrule != RETURNALL) ? 1 : Nn;

    // result vector - list of integer vectors
    SEXP matches = PROTECT(Rf_allocVector(VECSXP, Nh));

    int i, j, nmatch;
    bool anymatch;
    /*
        iterate and look for exact sub-string j in string i
    */
    for (i=0; i < Nh; i++) {
        SEXP mtchs_i = PROTECT(Rf_allocVector(INTSXP, Nm));
        memset(INTEGER(mtchs_i), 0, Nm * sizeof(int));
        const char *hay = CHAR(STRING_ELT(haystck, i));
        nmatch = 0; // num matches for string i
        anymatch = false;

        for (j=0; j < Nn; j++) {
            const char *ndl = CHAR(STRING_ELT(needles, j));
            if (matchfixed(&hay, &ndl, ignorecase)) {
                INTEGER(mtchs_i)[nmatch] = j + 1; // R's idx for current ndl
                anymatch = true;
                if (matchrule == RETURNFIRST) break;
                if (matchrule == RETURNALL) nmatch++;
            }
        }
        if (anymatch && matchrule != RETURNALL)
            nmatch = 1;
        SETLENGTH(mtchs_i, nmatch);
        SET_VECTOR_ELT(matches, i, mtchs_i);
        // all elements of a protected list are automatically protected
        // https://cran.r-project.org/doc/manuals/R-exts.html#Garbage-Collection
        UNPROTECT(1); // mtchs_i
    }

    UNPROTECT(1); // matches
    return matches;
}
