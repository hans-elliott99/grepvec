/* grepvec.c
 * search a vector of strings for matches in a vector of fixed strings or regex
 * Author: Hans Elliott
 * Date: 2024-03-10
 * License: MIT 2024 grepvec authors
 */
#include "shared.h"     //ttype_t
#include "stringutil.h" //StringCache,StringInfo
#include "widestring.h" //RwtransChar
#include "regexp.h"     //RegexCache,RegexInfo

#include <stdlib.h> // NULL
#include <string.h> // strstr
#include <R.h>
#include <Rinternals.h>
#include <R_ext/Utils.h> // R_CheckUserInterrupt

/* interval at which to check user interrupts */
#define NINTERRUPT 100



StringCache string_cache = {NULL,     //const char **arr
                            NULL,     //const wchar_t **warr
                            0,        //R_xlen_t n
                            use_char, //ttype_t tt
                            0};       //int ini

RegexCache  regex_cache  = {NULL,     //regex_t *rarr
                            0,        //R_xlen_t n
                            0,        //int flags
                            use_char, //ttype_t tt
                            NULL,     //int *seen
                            0};       //int ini


/*
    on_exit_grepvec

    called by on.exit in R wrapper function so that resources are freed even if
    interrupted
        - note, if function is type void, I get warning at R level:
        "converting NULL pointer to R NULL"
        (changing the fn definition to "void ..."" in init.c doesn't fix this)
*/
SEXP C_on_exit_grepvec(void) {
    free_str_cache(&string_cache);
    free_rgx_cache(&regex_cache);
    Riconv_cleanup();
    return R_NilValue;
}



enum MatchRule {
    RETURNALL   = 0,
    RETURNFIRST = 1
};

static int get_match_rule(SEXP x) {
    int mrule;
    const char mrule_ch = CHAR(STRING_ELT(x, 0))[0];
    if (mrule_ch == 'f')
        mrule = RETURNFIRST;
    else if (mrule_ch == 'a')
        mrule = RETURNALL;
    else
        error("match rule must be 'first' or 'all'.");
    return mrule;
}


// get value for argument passed to C function from R
static int Roption(SEXP x, const char *name, int r_type) {
    char type[20];
    int opt;
    switch (r_type) {
        case LGLSXP:
            strcpy(type, "logical");
            opt = asLogical(x);
            break;
        case INTSXP:
            strcpy(type, "integer");
            opt = asInteger(x);
            break;
        default:
            error("Roption: type not implemented.");
    }
    if (TYPEOF(x) != r_type) error("argument '%s' must be %s.", name, type);
    if (LENGTH(x) != 1) error("argument '%s' must be length 1.", name);
    if (opt == NA_INTEGER) return 0;
    return opt;
}


// returns true only if strings x and y are both entirely ascii characters
static int only_ascii(SEXP x, SEXP y, R_xlen_t len_x, R_xlen_t len_y) {
    R_xlen_t n = (len_x > len_y) ? len_x : len_y;
    SEXP *xp = STRING_PTR(x);
    SEXP *yp = STRING_PTR(y);
    for(R_xlen_t i = 0; i < n; ++i) {
        if (i < len_x &&
            !IS_ASCII(xp[i]) && xp[i] != NA_STRING)
            return 0;
        if (i < len_y &&
            !IS_ASCII(yp[i]) && yp[i] != NA_STRING)
            return 0;
    }
    return 1;
}


// returns true if either x or y contains bytes
static int have_bytes(SEXP x, SEXP y, R_xlen_t len_x, R_xlen_t len_y) {
    R_xlen_t n = (len_x > len_y) ? len_x : len_y;
    SEXP *xp = STRING_PTR(x);
    SEXP *yp = STRING_PTR(y);
    for(R_xlen_t i = 0; i < n; ++i) {
        if (i < len_x && IS_BYTES(xp[i]))
            return 1;
        if (i < len_y && IS_BYTES(yp[i]))
            return 1;
    }
    return 0;
}


// returns true if either x or y is utf8
static int have_utf8(SEXP x, SEXP y, R_xlen_t len_x, R_xlen_t len_y) {
    R_xlen_t n = (len_x > len_y) ? len_x : len_y;
    SEXP *xp = STRING_PTR(x);
    SEXP *yp = STRING_PTR(y);
    for(R_xlen_t i = 0; i < n; ++i) {
        if (i < len_x && IS_UTF8(xp[i]))
            return 1;
        if (i < len_y && IS_UTF8(yp[i]))
            return 1;
    }
    return 0;
}


// returns true if either x or y is latin1
static int have_latin1(SEXP x, SEXP y, R_xlen_t len_x, R_xlen_t len_y) {
    R_xlen_t n = (len_x > len_y) ? len_x : len_y;
    SEXP *xp = STRING_PTR(x);
    SEXP *yp = STRING_PTR(y);
    for(R_xlen_t i = 0; i < n; ++i) {
        if (i < len_x && IS_LATIN1(xp[i]))
            return 1;
        if (i < len_y && IS_LATIN1(yp[i]))
            return 1;
    }
    return 0;
}


// returns the "translation type" for the inputs
static ttype_t get_ttype(SEXP ndl, SEXP hay, int fixed, int bytes) {
    int utf8 = 0;
    R_xlen_t Nndl = XLENGTH(ndl), Nhay = XLENGTH(hay);
    if (!bytes)
        bytes = only_ascii(ndl, hay, Nndl, Nhay);
    if (!bytes)
        bytes = have_bytes(ndl, hay, Nndl, Nhay);
    if (!bytes) {
        if (!fixed) utf8 = 1; // return use_wchar
        if (!utf8)
            utf8 = have_utf8(ndl, hay, Nndl, Nhay);
        if (!utf8)
            utf8 = have_latin1(ndl, hay, Nndl, Nhay);
    }
    // if regex, we either use_char or use_wchar
    // (because, if !fixed and !bytes, utf8 = 1)
    if (bytes)
        return use_char;
    if (!fixed && utf8)
        return use_wchar;
    if (fixed && utf8)
        return use_utf8;
    return use_native;
}


/*compare string in cache at idx with the given regular expression*/
static int strrgx(StringCache *cache, RegexInfo *rgxo, R_xlen_t idx) {
    if (rgxo->tt == use_wchar)
        return wstr_rgx_match(cache->warr[idx], &rgxo->rgx);
    return str_rgx_match(cache->arr[idx], &rgxo->rgx);
}


/*compare string with the regular expression at idx of the regex cache*/
static int strrgx2(StringInfo *stro, RegexCache *cache, R_xlen_t idx) {
    if (cache->tt == use_wchar)
        return wstr_rgx_match(stro->wstr, &cache->rarr[idx]);
    return str_rgx_match(stro->str, &cache->rarr[idx]);
}


/*
    for each needle, search for matches in haystacks.
    returns a list, length needles
*/
SEXP C_grepvec(SEXP needles,
               SEXP haystacks,
               SEXP ignore_case,
               SEXP fixed,
               SEXP use_bytes,
               SEXP invert,
               SEXP match,
               SEXP keepdim,
               SEXP return_logical) {
    /*
        setup
    */
    const R_xlen_t Nndl = XLENGTH(needles);
    const R_xlen_t Nhay = XLENGTH(haystacks);
    const int fxd = Roption(fixed, "fixed", LGLSXP);
    const int bytes = Roption(use_bytes, "use_bytes", LGLSXP);
    int icase = Roption(ignore_case, "ignore_case", LGLSXP);
    const int inv = Roption(invert, "invert", LGLSXP);
    const int mrule = get_match_rule(match);
    int keep = Roption(keepdim, "keepdim", LGLSXP);
    const int ret_logical = Roption(return_logical, "return_logical", LGLSXP);
    int rgx_flags = REG_EXTENDED | REG_NOSUB;
    if (icase) rgx_flags |= REG_ICASE;

    if (fxd && icase) {
        warning("'ignore_case' will be ignored since 'fixed = TRUE'.");
        icase = 0;
    }
    if (keep && ret_logical) { // internal only
        warning("'keepdim' has no effect since 'return_logical = TRUE'.");
    }

    /*determine encoding of inputs and get conversion type*/
    ttype_t tt = get_ttype(needles, haystacks, fxd, bytes);

    /*output a list of vectors*/
    SEXP outlist = PROTECT(allocVector(VECSXP, Nndl));
    /*cache for haystacks*/
    init_str_cache(&string_cache, Nhay, tt);
    /*regex info struct*/
    RegexInfo rgxo = {{0},       // regex_t
                      rgx_flags, // flags for tre_regcomp
                      tt};       // ttype_t
    char *ndl_fxd = NULL;

    R_xlen_t i, j, mlen, nmatch, firstidx;
    int skip, res;
    SEXP logicals, indices;
    int *lgl_ptr = NULL, *ind_ptr = NULL;
    SEXP *hay_ptr = STRING_PTR(haystacks);
    SEXP *ndl_ptr = STRING_PTR(needles);
    /*
        iterate over needles
    */
    for (j=0; j < Nndl; ++j) {
        if ((j + 1) % NINTERRUPT == 0) R_CheckUserInterrupt();
        skip = (fxd) ? 0 : init_regex(ndl_ptr[j], &rgxo, j); // skip if bad rgx
        if (skip || ndl_ptr[j] == NA_STRING) {
            SET_VECTOR_ELT(outlist, j,
                           allocVector((ret_logical) ? LGLSXP : INTSXP, 0));
            continue;
        }
        if (fxd)
            ndl_fxd = (char *) do_translate_char(ndl_ptr[j], tt);
        /*
            iterate over haystacks
        */
        mlen = (mrule == RETURNFIRST) ? 1 : Nhay;
        logicals = PROTECT(allocVector(LGLSXP, mlen));
        lgl_ptr = LOGICAL(logicals);
        memset(lgl_ptr, 0, mlen * sizeof(int));
        nmatch = 0;   // num matches for pattern j
        for (i=0; i < Nhay; ++i) {
            if (update_str_cache(hay_ptr[i], &string_cache, i))
                continue; // skip if NA or if error translating encoding
            res = (fxd) ? (strstr(string_cache.arr[i], ndl_fxd) != NULL) :
                           strrgx(&string_cache, &rgxo, i);
            if (res ^ inv) {
                ++nmatch;
                if (mrule == RETURNFIRST) {
                    lgl_ptr[0] = 1;
                    firstidx = i;
                    break;
                } else {
                    lgl_ptr[i] = 1;
                }
            }
        }
        if (!fxd) tre_regfree(&rgxo.rgx);
        /*
            determine output type and shape
                TODO: long vector support
        */
        /*greplvec*/
        if (ret_logical) {
            SET_VECTOR_ELT(outlist, j, logicals);
            UNPROTECT(1); // logicals
        /*grepvec*/
        } else {
            /*special case if no matches - 0 length vector*/
            if (nmatch == 0 && !keep) {
                SET_VECTOR_ELT(outlist, j, allocVector(INTSXP, 0));
                UNPROTECT(1); // logicals
                continue;
            }
            /*special case if match = "first" and found match or keepdim */
            if (mrule == RETURNFIRST) {
                SET_VECTOR_ELT(outlist, j,
                               Rf_ScalarInteger((nmatch) ? firstidx + 1 : NA_INTEGER));
                UNPROTECT(1); // logicals
                continue;
            }
            /*else, fill index vector*/
            indices = PROTECT(allocVector(INTSXP, (keep) ? Nhay : nmatch));
            ind_ptr = INTEGER(indices);
            nmatch = 0;
            for (i=0; i < Nhay; ++i) {
                if (lgl_ptr[i]) {
                    ind_ptr[nmatch++] = i + 1; // R's idx for current hay
                } else if (keep) {
                    ind_ptr[nmatch++] = NA_INTEGER;
                }
            }
            SET_VECTOR_ELT(outlist, j, indices);
            UNPROTECT(2); // logicals, indices
        }
    }

    UNPROTECT(1); // outlist
    return outlist;
}


/*
    for each haystack, search for matches in the vector of needles
    returns a list, length haystacks
*/
SEXP C_vecgrep(SEXP needles,
               SEXP haystacks,
               SEXP ignore_case,
               SEXP fixed,
               SEXP use_bytes,
               SEXP invert,
               SEXP match,
               SEXP keepdim,
               SEXP return_logical) {
    /*
        setup
    */
    const R_xlen_t Nndl = XLENGTH(needles);
    const R_xlen_t Nhay = XLENGTH(haystacks);
    const int fxd = Roption(fixed, "fixed", LGLSXP);
    const int bytes = Roption(use_bytes, "use_bytes", LGLSXP);
    int icase = Roption(ignore_case, "ignore_case", LGLSXP);
    const int inv = Roption(invert, "invert", LGLSXP);
    const int mrule = get_match_rule(match);
    int keep = Roption(keepdim, "keepdim", LGLSXP);
    const int ret_logical = Roption(return_logical, "return_logical", LGLSXP);
    int rgx_flags = REG_EXTENDED | REG_NOSUB;
    if (icase) rgx_flags |= REG_ICASE;

    if (fxd && icase) {
        warning("'ignore_case' will be ignored since 'fixed = TRUE'.");
        icase = 0;
    }
    if (keep && ret_logical) { // internal only
        warning("'keepdim' has no effect since 'return_logical = TRUE'.");
    }

    /*determine encoding of inputs and get conversion type*/
    ttype_t tt = get_ttype(needles, haystacks, fxd, bytes);

    /*output a list of vectors*/
    SEXP outlist = PROTECT(allocVector(VECSXP, Nhay));

    /*cache for needle patterns*/
    if (fxd)
        init_str_cache(&string_cache, Nndl, tt);
    else
        init_rgx_cache(&regex_cache, rgx_flags, tt, Nndl);
    /*string info for current haystack*/
    StringInfo stro = {NULL, // const char *str
                       NULL, // const char *wstr
                       tt};  // ttype_t tt

    R_xlen_t i, j, mlen, nmatch, firstidx;
    int skip, res;
    SEXP logicals, indices;
    int *lgl_ptr = NULL, *ind_ptr = NULL;
    SEXP *hay_ptr = STRING_PTR(haystacks);
    SEXP *ndl_ptr = STRING_PTR(needles);
    /*
        iterate over haystacks
    */
    for (i=0; i < Nhay; ++i) {
        if ((i + 1) % NINTERRUPT == 0) R_CheckUserInterrupt();
        if (init_str_info(&stro, hay_ptr[i], tt)) {
            SET_VECTOR_ELT(outlist, i,
                           allocVector((ret_logical) ? LGLSXP : INTSXP, 0));
            continue; // if NA or if error translating to wide char
        }
        /*
            iterate over needles
        */
        nmatch = 0;   // num matches for haystack i
        mlen = (mrule == RETURNFIRST) ? 1 : Nndl;
        logicals = PROTECT(allocVector(LGLSXP, mlen));
        lgl_ptr = LOGICAL(logicals);
        memset(lgl_ptr, 0, mlen * sizeof(int));
        for (j=0; j < Nndl; ++j) {
            skip = (fxd) ? update_str_cache(ndl_ptr[j], &string_cache, j) :
                           update_rgx_cache(ndl_ptr[j], &regex_cache, j);
            if (skip) {
                SET_VECTOR_ELT(outlist, i, allocVector(LGLSXP, 0));
                continue;
            }
            res = (fxd) ? (strstr(stro.str, string_cache.arr[j]) != NULL) :
                           strrgx2(&stro, &regex_cache, j);
            if (res ^ inv) {
                ++nmatch;
                if (mrule == RETURNFIRST) {
                    lgl_ptr[0] = 1;
                    firstidx = j;
                    break;
                } else {
                    lgl_ptr[j] = 1;
                }
            }
        }
        /*
            determine output type and shape
                TODO: long vector support
        */
        /*greplvec*/
        if (ret_logical) {
            SET_VECTOR_ELT(outlist, i, logicals);
            UNPROTECT(1); // logicals
        /*grepvec*/
        } else {
            /*special case if no matches - 0 length vector*/
            if (nmatch == 0 && !keep) {
                SET_VECTOR_ELT(outlist, i, allocVector(INTSXP, 0));
                UNPROTECT(1); // logicals
                continue;
            }
            /*special case if match = "first" and found match or keepdims */
            if (mrule == RETURNFIRST) {
                SET_VECTOR_ELT(outlist, i,
                               Rf_ScalarInteger((nmatch) ? firstidx + 1 : NA_INTEGER));
                UNPROTECT(1); // logicals
                continue;
            }
            /*else, fill index vector*/
            indices = PROTECT(allocVector(INTSXP, (keep) ? Nndl : nmatch));
            ind_ptr = INTEGER(indices);
            nmatch = 0;
            for (j=0; j < Nndl; ++j) {
                if (lgl_ptr[j]) {
                    ind_ptr[nmatch++] = j + 1; // R's idx for current ndl
                } else if (keep) {
                    ind_ptr[nmatch++] = NA_INTEGER;
                }
            }
            SET_VECTOR_ELT(outlist, i, indices);
            UNPROTECT(2); // logicals, indices
        }
    }

    UNPROTECT(1); // outlist
    return outlist;
}



SEXP C_vecmatch(SEXP needles,
                SEXP haystacks,
                SEXP ignore_case,
                SEXP value,
                SEXP fixed,
                SEXP use_bytes,
                SEXP invert,
                SEXP return_logical,
                SEXP return_counts) {
    /*
        setup
    */
    const R_xlen_t Nndl = XLENGTH(needles);
    const R_xlen_t Nhay = XLENGTH(haystacks);
    int val = Roption(value, "value", LGLSXP);
    int icase = Roption(ignore_case, "ignore_case", LGLSXP);
    const int fxd = Roption(fixed, "fixed", LGLSXP);
    const int bytes = Roption(use_bytes, "use_bytes", LGLSXP);
    const int inv = Roption(invert, "invert", LGLSXP);
    const int ret_logical = Roption(return_logical, "return_logical", LGLSXP);
    const int ret_counts = Roption(return_counts, "return_counts", LGLSXP);

    if (ret_logical && ret_counts)
        error("only one of 'return_logical' and 'return_counts' can be TRUE.");

    const int ret_int = !ret_logical && !ret_counts;
    int rgx_flags = REG_EXTENDED | REG_NOSUB;
    if (icase) rgx_flags |= REG_ICASE;

    if (fxd && icase) {
        warning("'ignore_case' will be ignored since 'fixed = TRUE'.");
        icase = 0;
    }
    if (val && !ret_int) {
        warning("'value' has no effect unless returning indices.");
        val = 0;
    }

    /*determine encoding of inputs and get conversion type*/
    ttype_t tt = get_ttype(needles, haystacks, fxd, bytes);

    /*output a vector with an element for each haystack*/
    SEXP output = PROTECT(allocVector((ret_logical) ? LGLSXP : INTSXP, Nhay));
    int *out_ptr = INTEGER(output);
    memset(out_ptr, 0, Nhay * sizeof(int));

    /*cache for needle patterns*/
    if (fxd)
        init_str_cache(&string_cache, Nndl, tt);
    else
        init_rgx_cache(&regex_cache, rgx_flags, tt, Nndl);
    /*string info for current haystack*/
    StringInfo stro = {NULL, // const char *str
                       NULL, // const char *wstr
                       tt};  // ttype_t tt

    R_xlen_t i, j, nmatch;
    int res;
    SEXP *hay_ptr = STRING_PTR(haystacks);
    SEXP *ndl_ptr = STRING_PTR(needles);
    /*
        iterate over haystacks
    */
    for (i=0; i < Nhay; ++i) {
        if ((i + 1) % NINTERRUPT == 0) R_CheckUserInterrupt();
        if (init_str_info(&stro, hay_ptr[i], tt)) {
            continue; // if NA or if error translating to wide char
        }
        /*
            iterate over needles
        */
        nmatch = 0;   // num matches for haystack i
        for (j=0; j < Nndl; ++j) {
            if ((fxd) ? update_str_cache(ndl_ptr[j], &string_cache, j) :
                        update_rgx_cache(ndl_ptr[j], &regex_cache, j)
            ) {
                continue; // if NA or if error translating str/compiling regex
            }
            res = (fxd) ? (strstr(stro.str, string_cache.arr[j]) != NULL) :
                           strrgx2(&stro, &regex_cache, j);
            if (res ^ inv) {
                ++nmatch;
                if (ret_logical) {
                    out_ptr[i] = 1;
                    break;
                } else if (ret_int) {
                    out_ptr[i] = j + 1; // R's idx for current ndl
                    break;
                }
                // else counting matches, must try all needles
            }
        }
        if (ret_int && nmatch == 0) {
            out_ptr[i] = NA_INTEGER;
        } else if (ret_counts) {
            out_ptr[i] = nmatch;
        }
    }
    /*
        return pattern values if requested
    */
    if (val) {
        SEXP values = PROTECT(allocVector(STRSXP, Nhay));
        for (i=0; i < Nhay; ++i) {
            if (out_ptr[i] == NA_INTEGER) {
                SET_STRING_ELT(values, i, NA_STRING);
            } else {
                SET_STRING_ELT(values, i, ndl_ptr[out_ptr[i] - 1]);
            }
        }
        UNPROTECT(2); // output, values
        return values;
    }

    UNPROTECT(1); // output
    return output;
}