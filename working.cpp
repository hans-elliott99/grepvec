#include <cpp11.hpp>
#include <algorithm>
#include <string>
#include <regex>

#include <R.h>
#include <Rinternals.h>
#include <regex.h>

#include <iostream>

namespace writable = cpp11::writable;

/*
TODO:
cache regexes as we go through loop instead of pre-compiling all

warning messages if bad regex
*/

enum MatchruleEnum {
    RETURNALL = 0,
    RETURNFIRST = 1,
    RETURNLAST = 2,
};


struct regex_cache {
    std::vector<std::regex> rgx;
    std::vector<int> seen;
    std::regex_constants::syntax_option_type flags;
};


bool check_regex_cache(cpp11::r_string *ndl, struct regex_cache *cache, int idx) {
    if (cache->seen[idx] ==  1) return true;
    if (cache->seen[idx] == -1) return false;
    std::string ndl_str = *ndl;
    try {
        cache->rgx[idx] = std::regex(ndl_str, cache->flags);
        cache->seen[idx] = 1;
        return true;
    } catch (const std::regex_error& e) {
        cpp11::warning("error compiling regex %s: %s",
                       ndl_str.c_str(), e.what());
       cache->seen[idx] = -1;
       return false;
    }
}


//
// REGEX
//

[[cpp11::register]]
cpp11::list grepvec_regex_(cpp11::strings needles,
                           cpp11::strings haystck,
                           int matchrule,
                           bool ignorecase) {
    int Nh = haystck.size();
    int Nn = needles.size();
    // inital length of match vector for each haystck
    int Nm = (matchrule != RETURNALL) ? 1 : Nn;
    /*
        setup cache for regex patterns
          want regex style to match R's grep - hence, POSIX extended  
          https://github.com/SurajGupta/r-source/blob/master/src/main/grep.c
    */
    std::regex_constants::syntax_option_type flags = std::regex::extended;
    if (ignorecase) flags |= std::regex::icase;
    struct regex_cache rgx_cache = {
        std::vector<std::regex>(Nn), // rgx
        std::vector<int>(Nn, 0),     // seen
        flags,
    };
    // result vector - a list of integer vectors, length(haystck)
    // SEXP matches = PROTECT(Rf_allocVector(VECSXP, Nh));
    writable::list matches(Nh);

    int i, j, nmatch;
    bool anymatch;
    std::string hay;
    cpp11::r_string ndl;
    /*
        iterate and look for fixed string j in string i
    */
    for (i=0; i < Nh; i++) {
        hay = cpp11::r_string(haystck[i]);
        // SEXP mtchs_i = PROTECT(Rf_allocVector(INTSXP, Nm));
        // memset(INTEGER(mtchs_i), 0, Nm * sizeof(int));
        writable::integers mtchs_i(Nm);
        nmatch = 0;       // num matches for string i
        anymatch = false;
        for (j=0; j < Nn; j++) {
            ndl = needles[j];
            if (check_regex_cache(&ndl, &rgx_cache, j)) {
                if (std::regex_search(hay, rgx_cache.rgx[j])) {
                    // INTEGER(mtchs_i)[nmatch] = j + 1;
                    mtchs_i[nmatch] = j + 1; // R's idx for current ndl
                    anymatch = true;
                    if (matchrule == RETURNFIRST) break;
                    if (matchrule == RETURNALL) nmatch++;
                }
            }
        }
        if (anymatch && matchrule != RETURNALL)
            nmatch = 1;
        // SETLENGTH(mtchs_i, nmatch);
        mtchs_i.resize(nmatch);
        // SET_VECTOR_ELT(matches, i, mtchs_i);
        matches[i] = mtchs_i;
        // UNPROTECT(1); // mtchs_i
    }
    // UNPROTECT(1); // matches
    return matches;
}





//
// FIXED
//

bool matchfixed(std::string hay, std::string ndl, bool ignorecase) {
    // std::string hay = as_cpp<std::string>(haystack);
    // std::string ndl = as_cpp<std::string>(needle);
    if (ignorecase) {
        auto it = std::search(
            hay.begin(), hay.end(),
            ndl.begin(), ndl.end(),
            [](unsigned char chh, unsigned char chn) {
                return std::toupper(chh) == std::toupper(chn);
            }
        );
        return it != hay.end();
    } else {
        return hay.find(ndl) != std::string::npos;
    }
}

[[cpp11::register]]
cpp11::list grepvec_fixed_(cpp11::strings needles,
                           cpp11::strings haystck,
                           int matchrule,
                           bool ignorecase) {
    int Nn = needles.size();
    int Nh = haystck.size();
    // inital length of match vector for each hay
    int Nm = (matchrule == RETURNFIRST) ? 1 : Nn;

    // result vector - a list of integer vectors, length(haystck)
    writable::list matches(Nh);

    int i, j, nmatch, mtch_result, lastmatch;
    /*
        iterate and look for exact sub-string j in string i
    */
    for (i=0; i < Nh; i++) {
        writable::integers mtchs_i(Nm);
        nmatch = 0; // num matches for string i
        for (j=0; j < Nn; j++) {
            mtch_result = matchfixed(cpp11::r_string(haystck[i]),
                                     cpp11::r_string(needles[j]),
                                     ignorecase);
            if (mtch_result) {
                mtchs_i[nmatch] = j + 1; // R's idx for current ndl
                nmatch++;
                if (matchrule == RETURNFIRST) break;
            }
        }
        if (matchrule == RETURNLAST && nmatch > 0) {
            lastmatch = mtchs_i[nmatch - 1];
            mtchs_i[0] = lastmatch;
            mtchs_i.resize(1);
        } else {
            mtchs_i.resize(nmatch);
        }
        matches[i] = mtchs_i;
    }
    return matches;
}
