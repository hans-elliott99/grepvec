#ifndef grepvec_SHARED_H
#define grepvec_SHARED_H

/*string translation type*/
typedef enum {
    use_char   = 0,
    use_wchar  = 1,
    use_utf8   = 2,
    use_native = 3
} ttype_t;

#endif // grepvec_SHARED_H