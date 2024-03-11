#ifndef grepvec_STRING_UTIL_H
#define grepvec_STRING_UTIL_H

#include "shared.h" //ttype_t
#include <stdlib.h> //size_t,malloc,realloc,free
#include <R.h>
#include <Rinternals.h>


const char *do_translate_char(SEXP x, ttype_t tt);

/*
    STRING INFO
*/
typedef struct {
    const char *str;
    const wchar_t *wstr;
    ttype_t tt;
} StringInfo;

int init_str_info(StringInfo *info, SEXP x, ttype_t tt);


/*
    STRING CACHE
*/
typedef struct {
    const char **arr;
    const wchar_t **warr;
    R_xlen_t n; // length
    ttype_t tt; // flag for type of string translation/if wide char is needed
    int ini;    // whether the cache has been initialized
} StringCache;

void init_str_cache(StringCache *cache, R_xlen_t n, ttype_t ttype);
int update_str_cache(SEXP ndl, StringCache *cache, R_xlen_t idx);
void free_str_cache(StringCache *cache);



#endif // grepvec_STRING_UTIL_H