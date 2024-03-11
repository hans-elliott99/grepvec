
#include "widestring.h"
#include <stdlib.h> //NULL,size_t
#include <wchar.h> //wcslen
#include <errno.h> //errno
#include <R_ext/Riconv.h> // Riconv_open, Riconv_close, Riconv


/*
                STRING BUFFER
    The below functions are adapted from R,
        see source code at src/main/memory.c
*/

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
                WIDE CHAR TRANSLATION
    
    The below functions are adapted from R,
        see source code at src/main/sysutils.c
*/

#ifdef WIN32 // defined on all Windows platforms
static const char TO_WCHAR[] = "UTF-16LE";
#else
#  ifdef WORDS_BIGENDIAN // defined on big-endian systems, not common
static const char TO_WCHAR[] = "UCS-4BE";
#  else
static const char TO_WCHAR[] = "UCS-4LE";
#  endif
#endif


/*needs translation, from type*/
typedef enum {
    NT_FROM_NONE   = 0,
    NT_FROM_UTF8   = 1,
    NT_FROM_LATIN1 = 2,
    NT_FROM_NATIVE = 3,
    NT_FROM_ASCII  = 4,
} nttype_t;


static int RwneedsTranslation(SEXP x) {
    if (IS_ASCII(x)) return NT_FROM_ASCII;
    if (IS_UTF8(x)) return NT_FROM_UTF8;
    if (IS_LATIN1(x)) return NT_FROM_LATIN1;
    return NT_FROM_NATIVE;
}

static const wchar_t *RwcopyAndFreeString(RStringBuffer *cbuff) {
    size_t res = wcslen((wchar_t *) cbuff->data) + 1;
    wchar_t *p = (wchar_t *) R_alloc(res, sizeof(wchar_t));
    memcpy(p, cbuff->data, res * sizeof(wchar_t));
    RFreeStringBuffer(cbuff);
    return p;
}

static const wchar_t *RwfromAscii(const char *src, size_t len) {
    size_t i;
    wchar_t *p = (wchar_t *) R_alloc(len + 1, sizeof(wchar_t));
    for (i=0; i < len; i++)
        p[i] = (wchar_t) src[i];
    p[i] = L'\0';
    return p;
}


static void *latin1_wobj = NULL, *utf8_wobj = NULL;


/*
Note from R source:
Translate from current encoding to wchar_t = UTF-16LE/UCS-4
   NB: that wchar_t is UCS-4 is an assumption, but not easy to avoid.
*/
static int RtranslateToWchar(const char *ans,
                             RStringBuffer *cbuff,
                             nttype_t fromcode) {
    void *obj;
    const char *inbuf, *from;
    char *outbuf;
    size_t inb, outb, res;
    /*Case LATIN1*/
    if (fromcode == NT_FROM_LATIN1) {
        if (!latin1_wobj) {
#ifdef HAVE_ICONV_CP1252
            from = "CP1252";
#else
            from = "latin1";
#endif
            obj = Riconv_open(TO_WCHAR, from);
            if (obj == (void *)-1)
                error("unsupported conversion from '%s' to '%s'",
                      "latin1", TO_WCHAR);
            latin1_wobj = obj;
        } else {
            obj = latin1_wobj;
        }
    /*Case UTF8*/
    } else if (fromcode == NT_FROM_UTF8) {
        if (!utf8_wobj) {
            obj = Riconv_open(TO_WCHAR, "UTF-8");
            if (obj == (void *)-1)
                error("unsupported conversion from '%s' to '%s'",
                      "UTF-8", TO_WCHAR);
            utf8_wobj = obj;
        } else {
            obj = utf8_wobj;
        }
    /*Case Native*/
    } else {
        obj = Riconv_open(TO_WCHAR, "");
        if (obj == (void *)-1)
            error("unsupported conversion from '%s' to '%s'",
                  "native.enc", TO_WCHAR);
    }

    RAllocStringBuffer(0, cbuff);
    inbuf = ans; inb = strlen(ans);
    outbuf = cbuff->data; outb = cbuff->bufsize;
    /* init output */
    Riconv(obj, NULL, NULL, &outbuf, &outb);
    /* convert input */
    res = Riconv(obj, &inbuf, &inb, &outbuf, &outb);
    if (res == (size_t)-1) {
        return errno;
    }
    /* terminate wide string */
    *((wchar_t *)outbuf) = L'\0';
    if (fromcode == NT_FROM_NATIVE) Riconv_close(obj); // dont save for reuse
    return 0;
}


/*translate CHARSXP to wide string*/
const wchar_t *RwtransChar(SEXP x, int *err) {
    // if (TYPEOF(x) != CHARSXP) error("x must be a character vector");
    (*err) = 0;
    nttype_t t = RwneedsTranslation(x);
    if (t == NT_FROM_ASCII)
        return RwfromAscii(CHAR(x), LENGTH(x));
    /*
    Ensure buffer is big enough: wchar_t varies from 16-bit to 32-bit, so max
    is 4 times the number of bytes in the input string
    https://github.com/tidyverse/readr/blob/main/src/Iconv.cpp#L87
    */
    RStringBuffer cbuff = {NULL, 0, 4 * strlen(CHAR(x))};
    (*err) = RtranslateToWchar(CHAR(x), &cbuff, t);
    if (*err) {
        RFreeStringBuffer(&cbuff);
        return L"\0"; // a string that will match to nothing
    }
    return RwcopyAndFreeString(&cbuff);
}


void Riconv_cleanup(void) {
    // Riconv cleanup
    if (latin1_wobj) Riconv_close(latin1_wobj);
    if (utf8_wobj) Riconv_close(utf8_wobj);
    latin1_wobj = NULL;
    utf8_wobj = NULL;
}



void Riconv_warning_grepvec(int errcode, R_xlen_t idx, int is_haystack) {
    char whichvec[9];
    strcpy(whichvec, (is_haystack) ? "haystack" : "needle");
    char msg[] = "Check the encodings of the input vectors.";
    ++idx; // R index
    switch (errcode)
    {
    case EILSEQ:
        warning("invalid multibyte sequence in %s string %d. %s",
                whichvec, idx, msg);
        break; 
    case EINVAL:
        warning("incomplete multibyte sequence in %s string %d. %s",
                whichvec, idx, msg);
        break;
    case E2BIG:
        warning("internal error, iconv output buffer too small.");
        break;
    default:
        warning("iconv failed to convert %s string %d to wide char for unkown reason. %s",
                whichvec, idx, msg);
    }
}