#ifndef grepvec_REGEXP_H
#define grepvec_REGEXP_H

#include "shared.h"  //ttype_t
#include "tre/tre.h" //regex_t
#include <R.h>
#include <Rinternals.h>

int init_regex(SEXP ndl, regex_t *rgx_empty, int flags, ttype_t tt);
int strrgx(const char *str, regex_t *rgx);
int wstrrgx(const wchar_t *str, regex_t *rgx);

#endif // grepvec_REGEXP_H