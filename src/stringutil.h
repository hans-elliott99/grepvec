#ifndef grepvec_STRING_UTIL_H
#define grepvec_STRING_UTIL_H

#include "shared.h" //ttype_t
#include <stdlib.h> //size_t,malloc,realloc,free
#include <R.h>
#include <Rinternals.h>

/*
    STRING BUFFER
*/
typedef struct {
    char *data;
    size_t bufsize;
    size_t defaultsize;
} RStringBuffer;

void *RAllocStringBuffer(size_t blen, RStringBuffer *buf);
void RFreeStringBuffer(RStringBuffer *buf);

const char *do_translate_char(SEXP x, ttype_t tt);

/*
    STRING CACHE
*/
typedef struct {
    const char **arr;
    const wchar_t **warr;
    R_xlen_t n; // length
    ttype_t tt; // flag for type of string translation/if wide char is needed
} StringCache;

void init_cache(StringCache *cache, R_xlen_t n, ttype_t ttype);
void update_cache(SEXP ndl, StringCache *cache, R_xlen_t idx);
void free_cache(StringCache *cache);

/*
    WIDE STRING CACHE
*/
typedef struct {
    const wchar_t **arr;
    R_xlen_t n;
} WideStringCache;

void init_Wcache(WideStringCache *cache, R_xlen_t n);
void update_Wcache(SEXP ndl, WideStringCache *cache, R_xlen_t idx);
void free_Wcache(WideStringCache *cache);

#endif // grepvec_STRING_UTIL_H