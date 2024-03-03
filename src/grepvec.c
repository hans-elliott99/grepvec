/* grepvec.c
 * search a vector of strings for matches in a vector of fixed strings or regex
 * Author: Hans Elliott
 * Date: 2024-02-16
 * License: MIT 2024 grepvec authors
 */
#include <stdlib.h> // null
#include <string.h> // strstr
#include <locale.h> // setlocale
#include <errno.h> // errno
#include <R.h>
#include <Rinternals.h>
#include <R_ext/Utils.h> // R_CheckUserInterrupt
#include <R_ext/Riconv.h> // Riconv_open, Riconv_close, Riconv

#include "tre/tre.h" // tre_regcomp, tre_regexec, tre_regfree, tre_regerror


/* interval at which to check interrupts */
#define NINTERRUPT 100



/*https://github.com/Rdatatable/data.table/blob/1822d352dd129148e67ec0c8b04f8775477fc4fa/src/data.table.h#L30*/
#define IS_UTF8(x)  (LEVELS(x) & 8)
#define IS_ASCII(x) (LEVELS(x) & 64)
#define IS_LATIN(x) (LEVELS(x) & 4)

/*              STRING HELPERS                                              */
///////////////////////////////////////////////////////////////////////////////
typedef struct {
    char *data;
    size_t bufsize;
    size_t defaultsize;
} RStringBuffer;

/*
 * as used here, blen is always set to 0 in RtranslateToWchar, so blen and blen1
 * are always equal to 1 initially.
 * bsize is set to 4 * strlen(CHAR(x)) in RwtransChar, so for example,
 * an input string with one char has strlen = 2 and thus bsize = 4*2 = 8.
 * Then blen = (1/8) * 8 = 0 (since size_t), and 0 < 1 so blen = 0 + 8 = 8,
 * which becomes the size of the buffer.
*/
// R_AllocStringBuffer
void *RallocStringBuffer(size_t blen, RStringBuffer *buf) {
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

void R_FreeStringBuffer(RStringBuffer *buf) {
    if (buf->data != NULL) {
        free(buf->data);
        buf->bufsize = 0;
        buf->data = NULL;
    }
}

/*              WIDE CHAR TRANSLATION                                       */
///////////////////////////////////////////////////////////////////////////////

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

int RwneedsTranslation(SEXP x) {
    if (IS_ASCII(x)) return NT_FROM_ASCII;
    if (IS_UTF8(x)) return NT_FROM_UTF8;
    if (IS_LATIN(x)) return NT_FROM_LATIN1;
    return NT_FROM_NATIVE;
}

/* don't know if we need to do this because R's stringbuffer is reused and
ours is not... yet, but probably could/should*/
const wchar_t *RwcopyAndFreeString(RStringBuffer *cbuff) {
    size_t res = wcslen((wchar_t *) cbuff->data) + 1;
    wchar_t *p = (wchar_t *) R_alloc(res, sizeof(wchar_t));
    memcpy(p, cbuff->data, res * sizeof(wchar_t));
    return p;
}

const wchar_t *RwfromAscii(const char *src, size_t len) {
    // Rprintf("wfromAscii\n");
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

    RallocStringBuffer(0, cbuff);
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

/*
    TODO: using this approach it should be doable to convert to wide char like
    R does when needed...
    Problem - is it going to be a big performance hit to always be translating
    to wide char?
    Also - this actually seems to work... so how to make it portable?

    (else return get_char to translateCharUTF8)
*/
const wchar_t *get_char(SEXP x) {
    // if (IS_ASCII(x) || x == NA_STRING) {
    //     Rprintf("ascii or NA\n");
    // } else if (IS_UTF8(x)) {
    //     Rprintf("utf8\n");
    // } else if (IS_LATIN(x)) {
    //     Rprintf("latin1\n");
    // } else {
    //     Rprintf("native\n");
    // }
    const wchar_t *out = RwtransChar(x);
    // Rprintf("string: %ls\n", out);
    return out;
}

/*                  STRING CACHE                                            */
//////////////////////////////////////////////////////////////////////////////
struct WideStringCache {
    const wchar_t **arr;
    R_xlen_t n;
} stringWCache = {NULL, 0};


/* cache so only need to translate chars once */
void init_cache(struct WideStringCache *cache, R_xlen_t n) {
    cache->arr = (const wchar_t **)R_Calloc(n, wchar_t*);
    cache->n = n;
}


void update_cache(SEXP ndl, struct WideStringCache *cache, R_xlen_t idx) {
    // if (idx >= cache->n || idx < 0) error("index out of bounds");
    if (cache->arr[idx] != NULL) return;
    cache->arr[idx] = get_char(ndl);
}


/*
    need to free arr since it is R_Calloc'd, but not the individual strings
    since they are just pointers to the original R strings *unless* they are
    R_alloc'd by translateCharUTF8, in which case they are freed by the gc
    when vmax is reset
*/
void free_cache(struct WideStringCache *cache) {
    if (cache->n == 0) return;
    R_Free(cache->arr); // R_Calloc must be R_Free'd
    cache->n = 0;
}



/*                      REGEX                                               */
///////////////////////////////////////////////////////////////////////////////


/* compile regex for pattern if valid regex */
int init_regex(SEXP ndl, regex_t *rgx_empty, int flags) {
    char errbuff[1001];
    // const void *vmax = vmaxget();
    const wchar_t *ndl_str = get_char(ndl);

    int reti = tre_regwcomp(rgx_empty, ndl_str, flags);
    // vmaxset(vmax); // rm space allocated in translateCharUTF8 via R_alloc
    if (reti != 0) {
        tre_regerror(reti, rgx_empty, errbuff, sizeof(errbuff));
        warning("invalid regular expression '%s': %s", ndl_str, errbuff);
        return 1;
    }
    return 0;
}


/* test if regex finds a match in the string */
int strrgx(const wchar_t **str, regex_t *rgx) {
    char errbuff[1001];
    int reti = tre_regwexec(rgx, *str, 0, NULL, 0);
    if (reti == 0)
        return 1;
    if (reti != REG_NOMATCH) {
        tre_regerror(reti, rgx, errbuff, sizeof(errbuff));
        warning("regex match failed with error: %s", errbuff);
    }
    return 0;
}


/*                          GREPVEC                                          */
///////////////////////////////////////////////////////////////////////////////
enum MatchRule {
    RETURNALL   = 0,
    RETURNFIRST = 1
};


/*
    called by on.exit in R wrapper function so that resources are freed even if
    interrupted
        - note, if function returns void, I get warning at R level:
        "converting NULL pointer to R NULL"
        (changing the definition to void ... in init.c too doesn't fix this)
*/
SEXP on_exit_grepvec_(void) {
    free_cache(&stringWCache);
    Riconv_cleanup();
    return R_NilValue;
}



SEXP grepvec_(SEXP needles,
              SEXP haystck,
              SEXP fixed,
              SEXP matchrule,
              SEXP ignorecase,
              SEXP keepdim) {
    const R_xlen_t Nn = XLENGTH(needles);
    const R_xlen_t Nh = XLENGTH(haystck);
    const int fx = asInteger(fixed);
    const int mrule = asInteger(matchrule);
    const int keep = asInteger(keepdim);
    int rgx_flags = REG_EXTENDED|REG_NOSUB;
    if (asInteger(ignorecase)) rgx_flags |= REG_ICASE;
    /*initial length of match vector for each needle*/
    const R_xlen_t Nm = (!keep && mrule != RETURNALL) ? 1 : Nh;

    /*result vector - list of integer vectors*/
    SEXP output = PROTECT(allocVector(VECSXP, Nn));

    R_xlen_t i, j, nmatch;
    int skip;
    regex_t rgx;
    wchar_t *ndl_str;
    SEXP indices;
    init_cache(&stringWCache, Nh);
    /*
        iterate and compare compiled regex j with string i
    */
    for (j=0; j < Nn; ++j) {
        if ((j + 1) % NINTERRUPT == 0) R_CheckUserInterrupt();
        skip = (fx) ? 0 : init_regex(STRING_ELT(needles, j), &rgx, rgx_flags);
        if (skip || STRING_ELT(needles, j) == NA_STRING) {
            SET_VECTOR_ELT(output, j, allocVector(INTSXP, 0));
            continue;
        }
        indices = PROTECT(allocVector(INTSXP, Nm));
        if (fx) {
           ndl_str = (wchar_t *)get_char(STRING_ELT(needles, j));
        }

        nmatch = 0;   // num matches for pattern j
        for (i=0; i < Nh; ++i) {
            if (STRING_ELT(haystck, i) == NA_STRING) {
                continue;
            }
            update_cache(STRING_ELT(haystck, i), &stringWCache, i);
            if ((fx) ? (wcsstr(stringWCache.arr[i], ndl_str) != NULL) :
                        strrgx(&stringWCache.arr[i], &rgx)
            ) {
                INTEGER(indices)[nmatch] = i + 1; // R's idx for current hay
                ++nmatch;
                if (mrule == RETURNFIRST) break;
            }
        }
        if (!fx) tre_regfree(&rgx);
        if (keep) {
            for (i=nmatch; i < Nh; ++i) INTEGER(indices)[i] = NA_INTEGER;
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


*/