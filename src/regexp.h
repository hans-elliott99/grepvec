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

#endif // grepvec_REGEXP_H