#ifndef grepvec_REGEXP_H
#define grepvec_REGEXP_H

#include "shared.h"  //ttype_t
#include "tre/tre.h" //regex_t
#include <R.h>
#include <Rinternals.h>

struct RegexInfo {
    regex_t rgx; // the regex struct, which will hold a compiled regex
    int flags;   // regex compilation flags
    ttype_t tt;  // the translation type for the underlying string
};

int init_regex(SEXP ndl, struct RegexInfo *rgxo, R_xlen_t idx);
int strrgx(const char *str, regex_t *rgx);
int wstrrgx(const wchar_t *str, regex_t *rgx);

#endif // grepvec_REGEXP_H