
#include "stringutil.h" //StringBuffer
#include "widestring.h"
#include <stdlib.h> //NULL,size_t
#include <wchar.h> //wcslen
#include <errno.h> //errno
#include <R_ext/Riconv.h> // Riconv_open, Riconv_close, Riconv


/*              WIDE CHAR TRANSLATION                                       */
/*https://github.com/wch/r-source/blob/8857cc7f32d9affd9f519773a43ce4cbde97fe24/src/main/sysutils.c#L1652*/

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

/* don't know if we need to do this because R's stringbuffer is reused and
ours is not... yet, but probably could/should*/
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
        p[i] = (wchar_t)src[i];
    p[i] = L'\0';
    return p;
}


/*
Note from R:
Translate from current encoding to wchar_t = UTF-16LE/UCS-4
   NB: that wchar_t is UCS-4 is an assumption, but not easy to avoid.
*/
static void *latin1_wobj = NULL, *utf8_wobj = NULL;

static int RtranslateToWchar(const char *ans,
                             RStringBuffer *cbuff,
                             nttype_t fromcode) {
    void *obj;
    const char *inbuf, *from;
    char *outbuf;
    size_t inb, outb, res;
    // Rprintf("translateToWchar\n");
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
    /*Case Native - NT_FROM_NATIVE*/
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
        switch (errno)
        {
            // TODO: return ints to use as error codes and make pretty warnings
            // in grepvec
        case EILSEQ:
            error("invalid multibyte sequence.");
            break; 
        case EINVAL:
            error("incomplete multibyte sequence.");
            break;
        case E2BIG:
            error("iconv output buffer too small.");
            break;
        default:
            error("iconv failed to convert to wide char for unkown reason.");
        }
        return 1;
    }
    /* terminate wide string */
    *((wchar_t *)outbuf) = L'\0';
    if (fromcode == NT_FROM_NATIVE) Riconv_close(obj); // dont save for reuse
    return 0;
}

const wchar_t *RwtransChar(SEXP x) {
    if (TYPEOF(x) != CHARSXP) error("x must be a character vector");
    nttype_t t = RwneedsTranslation(x);
    if (t == NT_FROM_ASCII)
        return RwfromAscii(CHAR(x), LENGTH(x));
    /*
    Ensure buffer is big enough: wchar_t varies from 16-bit to 32-bit, so max
    is 4 times the number of bytes in the input string
    https://github.com/tidyverse/readr/blob/main/src/Iconv.cpp#L87
    */
    RStringBuffer cbuff = {NULL, 0, 4 * strlen(CHAR(x))};
    RtranslateToWchar(CHAR(x), &cbuff, t);
    return RwcopyAndFreeString(&cbuff);
}


void Riconv_cleanup(void) {
    if (latin1_wobj) Riconv_close(latin1_wobj);
    if (utf8_wobj) Riconv_close(utf8_wobj);
    latin1_wobj = NULL;
    utf8_wobj = NULL;
}
