/* grepvec.c
 * search a vector of strings for matches in a vector of fixed strings or regex
 * Author: Hans Elliott
 * Date: 2024-02-16
 * License: MIT 2024 grepvec authors
 */
#include "shared.h" //ttype_t
#include "stringutil.h" //init_cache, update_cache, free_cache
#include "widestring.h" //RwtransChar
#include "regexp.h" //init_regex, strrgx, wstrrgx

#include <stdlib.h> // null
#include <string.h> // strstr
#include <R.h>
#include <Rinternals.h>
#include <R_ext/Utils.h> // R_CheckUserInterrupt

/* interval at which to check user interrupts */
#define NINTERRUPT 100


enum MatchRule {
    RETURNALL   = 0,
    RETURNFIRST = 1
};

StringCache string_cache = {NULL, NULL, 0, use_char};

/*
    called by on.exit in R wrapper function so that resources are freed even if
    interrupted
        - note, if function returns void, I get warning at R level:
        "converting NULL pointer to R NULL"
        (changing the definition to void ... in init.c too doesn't fix this)
*/
SEXP C_on_exit_grepvec(void) {
    free_str_cache(&string_cache);
    Riconv_cleanup();
    return R_NilValue;
}


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
    // (i.e., if !fixed, utf8 = 1, if !fixed && utf8, use_wchar)
    if (bytes)
        return use_char;
    if (!fixed && utf8)
        return use_wchar;
    // if (fixed && utf8)
    //     return use_utf8;
    // return use_native;
    return use_utf8; // default to always returning use_utf8 so not locale dependent?
}


static int strrgx(StringCache *cache, RegexInfo *rgxo, R_xlen_t idx) {
    if (rgxo->tt == use_wchar)
        return wstr_rgx_match(cache->warr[idx], &rgxo->rgx);
    return str_rgx_match(cache->arr[idx], &rgxo->rgx);
}


SEXP C_grepvec(SEXP needles,
               SEXP haystacks,
               SEXP ignore_case,
               SEXP fixed,
               SEXP use_bytes,
               SEXP invert,
               SEXP matchrule,
               SEXP keepdim,
               SEXP return_logical) {
    const R_xlen_t Nndl = XLENGTH(needles);
    const R_xlen_t Nhay = XLENGTH(haystacks);
    const int fxd = asLogical(fixed);
    const int bytes = asLogical(use_bytes);
    const int inv = asLogical(invert);
    const int mrule = asLogical(matchrule);
    const int keep = asLogical(keepdim);
    const int ret_logical = asLogical(return_logical);
    int rgx_flags = REG_EXTENDED | REG_NOSUB;
    if (asLogical(ignore_case)) rgx_flags |= REG_ICASE;

    /*determine encoding of inputs and get conversion type*/
    ttype_t tt = get_ttype(needles, haystacks, fxd, bytes);
    if (tt == use_wchar) {
        Rprintf("using wchar\n");
    } else if (tt == use_utf8) {
        Rprintf("using utf8\n");
    } else if (tt == use_native) {
        Rprintf("using native\n");
    } else {
        Rprintf("using char\n");
    }

    /*output a list of vectors*/
    SEXP outlist = PROTECT(allocVector(VECSXP, Nndl));
    /*cache for haystacks*/
    init_str_cache(&string_cache, Nhay, tt);
    /*regex info struct*/
    RegexInfo rgxo = {{0},       // regex_t
                      rgx_flags, // flags for tre_regcomp
                      tt};       // ttype_t
    char *ndl_str = NULL;

    R_xlen_t i, j, nmatch, indlen;
    int skip, res;
    SEXP logicals, indices;
    int *lgl_ptr = NULL, *ind_ptr = NULL;
    SEXP *hay_ptr = STRING_PTR(haystacks);
    SEXP *ndl_ptr = STRING_PTR(needles);
    /*
        iterate over patterns
    */
    for (j=0; j < Nndl; ++j) {
        if ((j + 1) % NINTERRUPT == 0) R_CheckUserInterrupt();
        skip = (fxd) ? 0 : init_regex(ndl_ptr[j], &rgxo, j); // skip if bad rgx
        if (skip || ndl_ptr[j] == NA_STRING) {
            SET_VECTOR_ELT(outlist, j, allocVector(LGLSXP, 0));
            continue;
        }
        if (fxd)
            ndl_str = (char *) do_translate_char(ndl_ptr[j], tt);
        /*
            iterate over text
        */
        nmatch = 0;   // num matches for pattern j
        logicals = PROTECT(allocVector(LGLSXP, Nhay));
        lgl_ptr = LOGICAL(logicals);
        memset(lgl_ptr, 0, Nhay * sizeof(int));
        for (i=0; i < Nhay; ++i) {
            if (update_str_cache(hay_ptr[i], &string_cache, i))
                continue; // skip if NA or if error translating encoding
            res = (fxd) ? (strstr(string_cache.arr[i], ndl_str) != NULL) :
                           strrgx(&string_cache, &rgxo, i);
            if (res ^ inv) {
                lgl_ptr[i] = res ^ inv;
                ++nmatch;
            }
        }
        if (!fxd) tre_regfree(&rgxo.rgx);
        /*
            determine output type and shape
                TODO: long vector support
        */
        if (ret_logical) {
            /*greplvec*/
            SET_VECTOR_ELT(outlist, j, logicals);
            UNPROTECT(1); // logicals
        } else {
            /*special case if no matches*/
            if (nmatch == 0 && !keep) {
                SET_VECTOR_ELT(outlist, j, allocVector(INTSXP, 0));
                UNPROTECT(1); // logicals
                continue;
            }
            /*length of match vector for each needle*/
            indlen = (nmatch > 0 && mrule == RETURNFIRST) ? 1 : nmatch;
                if (keep) indlen = Nhay;
            indices = PROTECT(allocVector(INTSXP, indlen));
            ind_ptr = INTEGER(indices);
            nmatch = 0;
            for (i=0; i < Nhay; ++i) {
                if (lgl_ptr[i] == 1) {
                    ind_ptr[nmatch++] = i + 1; // R's idx for current hay
                    if (mrule == RETURNFIRST) break;
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
