#ifndef grepvec_REGEXP_H
#define grepvec_REGEXP_H

#include "shared.h"  //ttype_t
#include "tre/tre.h" //regex_t
#include <R.h>
#include <Rinternals.h>

typedef struct {
    regex_t rgx; // the regex struct, will hold a compiled regex, must be freed
    int flags;   // regex compilation flags
    ttype_t tt;  // translation type for the underlying string
} RegexInfo;

int init_regex(SEXP ndl, RegexInfo *rgxo, R_xlen_t idx);


int str_rgx_match(const char *str, regex_t *rgx);
int wstr_rgx_match(const wchar_t *str, regex_t *rgx);


typedef struct {
    regex_t *rarr; // regex_t vector to store compiled regex, each must be freed
    R_xlen_t n;    // number of elements
    int flags;     // regex compilation flags
    ttype_t tt;    // translation type for the underlying string
    int *seen;     // track which patterns have been seen, tell whether to skip 
    int ini;       // whether the cache has been initialized
} RegexCache;

int init_rgx_cache(RegexCache *cache, int flags, ttype_t tt, R_xlen_t n);
int update_rgx_cache(SEXP ndl, RegexCache *cache, R_xlen_t idx);
void free_rgx_cache(RegexCache *cache);

#endif // grepvec_REGEXP_H