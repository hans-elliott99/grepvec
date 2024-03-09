#include "shared.h" //Riconv_warning
#include "widestring.h" //RwtransChar
#include "stringutil.h"


/*          STRING BUFFER          */

/* R_AllocStringBuffer
 * as used here, blen is always set to 0 in RtranslateToWchar, so blen and blen1
 * are always equal to 1 initially.
 * bsize is set to 4 * strlen(CHAR(x)) in RwtransChar, so for example,
 * an input string with one char has strlen = 2 and thus bsize = 4*2 = 8.
 * Then blen = (1/8) * 8 = 0 (since size_t), and 0 < 1 so blen = 0 + 8 = 8,
 * which becomes the size of the buffer.
*/
void *RAllocStringBuffer(size_t blen, RStringBuffer *buf) {
    size_t blen1, bsize = buf->defaultsize;
    if (blen * sizeof(char) < buf->bufsize) return buf->data;
    blen1 = blen = (blen + 1) * sizeof(char);
    blen = (blen / bsize) * bsize;
    if (blen < blen1) blen += bsize;
    /* Result may be accessed as `wchar_t *` and other types; malloc /
    realloc guarantee correct memory alignment for all object types */
    if (buf->data == NULL) {
        buf->data = (char *)malloc(blen);
        if (buf->data)
            buf->data[0] = '\0';
    } else {
        buf->data = (char *)realloc(buf->data, blen);
    }
    buf->bufsize = blen;
    if (!buf->data) {
        buf->bufsize = 0;
        error("could not allocate memory in C function RallocStringBuffer");
    }
    return buf->data;
}

void RFreeStringBuffer(RStringBuffer *buf) {
    if (buf->data != NULL) {
        free(buf->data);
        buf->bufsize = 0;
        buf->data = NULL;
    }
}


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


/*          STRING CACHE            */
void init_str_cache(StringCache *cache, R_xlen_t n, ttype_t ttype) {
    if (ttype == use_wchar) {
        cache->warr = (const wchar_t**)R_Calloc(n, wchar_t*);
    } else {
        cache->arr = (const char**)R_Calloc(n, char*);
    }
    cache->n = n;
    cache->tt = ttype;
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
            Riconv_warning(err, idx, 1); // 1 for haystack
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
    if (cache->tt == use_wchar && cache->warr != NULL) {
        R_Free(cache->warr);
    } else {
        if (cache->arr != NULL) R_Free(cache->arr);
    }
    cache->warr = NULL;
    cache->arr = NULL;
    cache->n = 0;
}
