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

// #include "tre/tre.h" // tre_regcomp, tre_regexec, tre_regfree, tre_regerror

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


static Rboolean only_ascii(SEXP x, R_xlen_t len) {
    R_xlen_t i;

    for(i = 0; i < len; i++) 
	if (!IS_ASCII(STRING_ELT(x, i)) && STRING_ELT(x, i) != NA_STRING)  
	    return FALSE;
    return TRUE;
}


static Rboolean have_bytes(SEXP x, R_xlen_t len) {
    R_xlen_t i;
    for(i = 0; i < len; i++) 
	    if (IS_BYTES(STRING_ELT(x, i))) return TRUE;
    return FALSE;
}

static Rboolean have_utf8(SEXP x, R_xlen_t len)
{
    R_xlen_t i;

    for(i = 0; i < len; i++) 
	    if (IS_UTF8(STRING_ELT(x, i))) return TRUE;
    return FALSE;
}

static Rboolean have_latin1(SEXP x, R_xlen_t len)
{
    R_xlen_t i;

    for(i = 0; i < len; i++) 
	    if (IS_LATIN1(STRING_ELT(x, i))) return TRUE;
    return FALSE;
}


static ttype_t get_ttype(SEXP ndl, SEXP hay, int fixed, int bytes) {
    int utf8 = 0;
    R_xlen_t Nn = XLENGTH(ndl), Nh = XLENGTH(hay);
    if (!bytes)
        bytes = only_ascii(ndl, Nn) && only_ascii(hay, Nh);
    if (!bytes)
        bytes = have_bytes(ndl, Nn) || have_bytes(hay, Nh);
    if (!bytes) {
        if (!fixed) utf8 = 1; // return use_wchar
        if (!utf8)
            utf8 = have_utf8(ndl, Nn) || have_utf8(hay, Nh);
        if (!utf8)
            utf8 = have_latin1(ndl, Nn) || have_latin1(hay, Nh);
    }
    // so if regex, we either use_char or use_wchar
    // (if !fixed, utf8 = 1, if !fixed && utf8, use_wchar)
    if (bytes)
        return use_char;
    if (!fixed && utf8)
        return use_wchar;
    // if (fixed && utf8)
    //     return use_utf8;
    // return use_native;
    return use_utf8; // default to always returning use_utf8 so its not locale dependent
}



/*
 * if all ascii:
 *      - use char to access the strings, use strstr or tre_regexecb
 * if bytes are detected:
 *     - use char to access strings, use strstr or tre_regexecb
 * if utf8 is detected:
 *     - if fixed, translate char to utf8, use strstr
 *     - if regex, translate to wchar, use tre_regwexec
 * if some other encoding (use_utf8 not detected and not all ascii or bytes):
 *     - if fixed, translate char to native,  
 *     - if regex, use wchar to access the strings, use tre_regwexec
 * 
 * only time we use translateCharUTF8 or translateChar is for fixed strings,
 * otherwise we are using CHAR or RwtransChar to access the strings.
 * 
 * TODO: probably don't want to check all of the strings.
 * 
*/
ttype_t old_get_ttype(SEXP ndls, SEXP hays, int fixed) {
    R_xlen_t i, max, Nn, Nh;
    Nn = XLENGTH(ndls);
    Nh = XLENGTH(hays);
    max = (Nn > Nh) ? Nn : Nh;
    // int maxcheck = 10;
    // max = (max > maxcheck) ? maxcheck : max;
    // if (ncheck)
        // max = (max > ncheck) ? ncheck : max;
    int utf8_n = 0, utf8_h = 0;
    int notascii_n = 0, notascii_h = 0;
    for (i=0; i < max; ++i) {
        if (i < Nn && STRING_ELT(ndls, i) != NA_STRING) {
            if (IS_BYTES(STRING_ELT(ndls, i))) return use_char; // if has bytes,no wchar
            if (!IS_ASCII(STRING_ELT(ndls, i))) notascii_n = 1;
            if (IS_UTF8(STRING_ELT(ndls, i))) utf8_n = 1;
        }
        if (i < Nh && STRING_ELT(hays, i) != NA_STRING) {
            if (IS_BYTES(STRING_ELT(hays, i))) return use_char;
            if (!IS_ASCII(STRING_ELT(hays, i))) notascii_h = 1;
            if (IS_UTF8(STRING_ELT(hays, i))) utf8_h = 1;
        }
        if ((utf8_n || utf8_h) || (notascii_n && notascii_h)) {
            break;
        }
    }
    if (utf8_h && utf8_n) {
        if (fixed) return use_utf8;
        else return use_wchar;
    }
    if (notascii_h && notascii_n) {
        if (fixed) return use_native;
        else return use_wchar;
    }
    return use_char;
}



SEXP C_grepvec(SEXP needles,
               SEXP haystck,
               SEXP ignorecase,
               SEXP fixed,
               SEXP usebytes,
               SEXP invert,
               SEXP matchrule,
               SEXP keepdim) {
    const R_xlen_t Nn = XLENGTH(needles);
    const R_xlen_t Nh = XLENGTH(haystck);
    const int fxd = asLogical(fixed);
    const int bytes = asLogical(usebytes);
    const int inv = asLogical(invert);
    const int mrule = asLogical(matchrule);
    const int keep = asLogical(keepdim);
    int rgx_flags = REG_NOSUB|REG_EXTENDED;
    if (asLogical(ignorecase)) rgx_flags |= REG_ICASE;
    /*initial length of match vector for each needle*/
    const R_xlen_t Nm = (!keep && mrule != RETURNALL) ? 1 : Nh;

    /*determine encoding of inputs and get conversion type*/
    ttype_t tt = get_ttype(needles, haystck, fxd, bytes);
    if (tt == use_wchar) {
        Rprintf("using wchar\n");
    } else if (tt == use_utf8) {
        Rprintf("using utf8\n");
    } else if (tt == use_native) {
        Rprintf("using native\n");
    } else {
        Rprintf("using char\n");
    }

    /*output a list of integer vectors*/
    SEXP output = PROTECT(allocVector(VECSXP, Nn));
    /*cache for translated haystacks*/
    init_str_cache(&string_cache, Nh, tt);
    /*regex info struct*/
    struct RegexInfo rgxo = {{0},       // regex_t
                             rgx_flags, // flags for tre_regcomp
                             tt};       // ttype_t

    R_xlen_t i, j, nmatch;
    int skip, res;
    char *ndl_str = NULL;
    SEXP indices;
    int *ind_ptr;
    /*
        iterate and compare compiled regex j with string i
    */
    for (j=0; j < Nn; ++j) {
        Rprintf("needle: %s\n", CHAR(STRING_ELT(needles, j)));
        if ((j + 1) % NINTERRUPT == 0) R_CheckUserInterrupt();
        skip = (fxd) ? 0 : init_regex(STRING_ELT(needles, j), &rgxo, j);
        if (skip || STRING_ELT(needles, j) == NA_STRING) {
            SET_VECTOR_ELT(output, j, allocVector(INTSXP, 0));
            continue;
        }
        if (fxd)
            ndl_str = (char *) do_translate_char(STRING_ELT(needles, j), tt);

        indices = PROTECT(allocVector(INTSXP, Nm));
        ind_ptr = INTEGER(indices);
        nmatch = 0;   // num matches for pattern j
        for (i=0; i < Nh; ++i) {
            if (STRING_ELT(haystck, i) == NA_STRING) {
                continue;
            }
            update_str_cache(STRING_ELT(haystck, i), &string_cache, i);

            if (fxd) {
                res = (strstr(string_cache.arr[i], ndl_str) != NULL);
            } else {
                res = (tt == use_wchar) ? wstrrgx(string_cache.warr[i], &rgxo.rgx)
                                        : strrgx(string_cache.arr[i], &rgxo.rgx);
            }
            if (res ^ inv) {
                ind_ptr[nmatch++] = i + 1; // R's idx for current hay
                if (mrule == RETURNFIRST) break;
            }
        }
        if (!fxd) tre_regfree(&rgxo.rgx);
        if (keep) {
            for (i=nmatch; i < Nh; ++i) ind_ptr[i] = NA_INTEGER;
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


If pattern is only ascii, we still test to make sure all of text is only ascii
    - if all ascii, useBytes
If either pattern or text has bytes, useBytes
If patten is utf8, we still test to make sure all of text is utf8
    - if all utf8, useUTF8



*/