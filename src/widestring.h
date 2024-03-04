#ifndef grepvec_WIDESTRING_H
#define grepvec_WIDESTRING_H

#include <R.h>
#include <Rinternals.h>


/*
    From R-internals, section 1.1.2:
    SEXP is a pointer to a SEXPREC which contains a 64-bit sxpinfo header, which
    includes a gp ("general purpose") field, whose bits are accessed by LEVELS.
    Bits 1, 2, 3, 5 and 6 are used for a CHARSXP to denote its encoding.
    Bit 1 indicates that the CHARSXP should be treated as a set of bytes, not
    necessarily representing a character in any known encoding.
    Bits 2, 3 and 6 are used to indicate that it is known to be in Latin-1,
    UTF-8 or ASCII respectively.
*/
#define BYTES_MASK  (1<<1)
#define LATIN1_MASK (1<<2)
#define UTF8_MASK   (1<<3)
#define ASCII_MASK  (1<<6)

#define IS_BYTES(x) (LEVELS(x) & BYTES_MASK)
#define IS_LATIN1(x) (LEVELS(x) & LATIN1_MASK)
#define IS_UTF8(x)  (LEVELS(x) & UTF8_MASK)
#define IS_ASCII(x) (LEVELS(x) & ASCII_MASK)
/*https://github.com/Rdatatable/data.table/blob/1822d352dd129148e67ec0c8b04f8775477fc4fa/src/data.table.h#L30*/
// #define IS_LATIN1(x) (LEVELS(x) & 4)
// #define IS_UTF8(x)  (LEVELS(x) & 8)
// #define IS_ASCII(x) (LEVELS(x) & 64)

const wchar_t *RwtransChar(SEXP x);
void Riconv_cleanup(void);

#endif // grepvec_WIDESTRING_H