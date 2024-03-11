#include "shared.h" //Riconv_warning
#include "widestring.h" //RwtransChar
#include "stringutil.h"





/*
                STRING ENCODING TRANSLATION
*/

/*
* Translate a string based on its translation type (ttype_t), which is based
*  on the encoding of the string.
*/
const char *do_translate_char(SEXP x, ttype_t tt) {
    // if (TYPEOF(x) != CHARSXP) error("x must be a character vector");
    switch (tt) {
    case (use_utf8):
        return translateCharUTF8(x);
        break;
    case (use_native):
        return translateChar(x);
        break;
    default:
        return CHAR(x);
        break;
    }
}


/*
                STRING INFO OBJECT
*/
/*return 1 if error translating string or if NA*/
int init_str_info(StringInfo *info, SEXP x, ttype_t tt) {
    // if (TYPEOF(x) != CHARSXP) error("x must be a character vector");
    if (x == NA_STRING) return 1; // skip
    info->tt = tt;
    if (tt == use_wchar) {
        int err;
        info->wstr = RwtransChar(x, &err);
        if (err) {
            Riconv_warning_grepvec(err, 0, 1); // 1 for "haystack"
            return 1;
        }
    } else {
        info->str = do_translate_char(x, tt);
    }
    return 0;
}



/*
                STRING CACHE

*/

void init_str_cache(StringCache *cache, R_xlen_t n, ttype_t ttype) {
    if (ttype == use_wchar) {
        cache->warr = (const wchar_t**) R_Calloc(n, wchar_t*);
    } else {
        cache->arr = (const char**) R_Calloc(n, char*);
    }
    cache->n = n;
    cache->tt = ttype;
    cache->ini = 1;
}


/*returns 1 if successful, 0 if string should be skipped due to being NA or
  untranslatable to wide char*/
int update_str_cache(SEXP ndl, StringCache *cache, R_xlen_t idx) {
    // if (TYPEOF(ndl) != CHARSXP) error("x must be a character vector");
    if (ndl == NA_STRING) return 1; // skip
    if (cache->tt == use_wchar) {
        if (cache->warr[idx] != NULL) return 0; // already allocated
        int err;
        cache->warr[idx] = RwtransChar(ndl, &err);
        if (err) {
            Riconv_warning_grepvec(err, idx, 1); // 1 for "haystack"
            return 1; // skip
        }
    } else {
        if (cache->arr[idx] != NULL) return 0; // already allocated
        cache->arr[idx] = do_translate_char(ndl, cache->tt);
    }
    return 0;
}


void free_str_cache(StringCache *cache) {
    // R_Calloc must be R_Free'd
    if (!cache->ini) return;
    if (cache->tt == use_wchar && cache->warr != NULL) {
        R_Free(cache->warr);
    } else {
        if (cache->arr != NULL) R_Free(cache->arr);
    }
    cache->warr = NULL;
    cache->arr = NULL;
    cache->n = 0;
    cache->tt = use_char;
    cache->ini = 0;
}
